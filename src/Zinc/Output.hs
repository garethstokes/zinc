-- | The output foundation (hw6.1): one structured event stream with two
-- renderers, mode-selected. Build code emits 'OutputEvent's into a 'Sink';
-- a single renderer thread (owning the terminal + counters) drains them and
-- renders for the current 'OutputMode' — human (progress/color, hw6.2) or
-- machine (one JSONL line per event, hw6.4). This generalizes the
-- 'Zinc.Diagnostic' error split to /all/ output.
--
-- Concurrency: producers (parallel @produceOne@) only enqueue (thread-safe);
-- the renderer is the sole terminal owner. Shutdown is robust: 'withRenderer'
-- sets a @closed@ flag in @finally@ on any exit (success / 'ZincError' /
-- exception) and the loop drains remaining events via STM before stopping — no
-- sentinel that a dying producer might never write.
module Zinc.Output
  ( OutputEvent (..)
  , Sink (..)
  , emit
  , nullSink
  , OutputMode (..)
  , OutputFlags (..)
  , resolveMode
  , withRenderer
  , eventJson
  , RProg (..)
  , verb
  , progressLine
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
  ( STM
  , TQueue
  , TVar
  , atomically
  , isEmptyTQueue
  , newTQueueIO
  , newTVarIO
  , orElse
  , readTQueue
  , readTVar
  , retry
  , writeTQueue
  , writeTVar
  )
import Control.Exception (finally)
import Data.Maybe (isJust)
import System.Environment (lookupEnv)
import System.IO (hFlush, hIsTerminalDevice, stdout)
import Zinc.Ansi (clearLine, dim, greenBold, red, yellow)
import Zinc.Diagnostic (Diagnostic, Severity (..), diagSeverity, diagTitle, diagnosticJson)
import Zinc.Json (Json (..), renderJson)

-- | A structured output event. Progress through a build flows as these; a
-- 'Diagnostic' also rides the stream (so warnings/errors render in one place).
data OutputEvent
  = ResolveStart
  | Plan Int                       -- ^ number of closure packages to build (for [n/total])
  | FetchStart String              -- ^ package
  | FetchDone String               -- ^ package
  | CompileStart String            -- ^ package
  | CompileDone String Int Bool    -- ^ package, wall-clock ms, cached
  | RegisterDone String            -- ^ package
  | Finished String                -- ^ human-facing one-line summary
  | Note Diagnostic                -- ^ a diagnostic to surface
  deriving (Eq, Show)

-- | A thread-safe event consumer. Producers (any thread) enqueue via 'runSink'.
-- A 'Monoid' so consumers fan out: @renderSink <> recordSink@ feeds both; the
-- 'mempty' 'nullSink' drops everything (quiet / no consumer).
newtype Sink = Sink {runSink :: OutputEvent -> STM ()}

instance Semigroup Sink where
  Sink f <> Sink g = Sink (\e -> f e >> g e)

instance Monoid Sink where
  mempty = Sink (const (pure ()))

-- | The no-op sink (e.g. tests, @--quiet@, headless).
nullSink :: Sink
nullSink = mempty

-- | Emit one event into a sink from 'IO' (any producer thread). The sink's
-- 'STM' action runs atomically; with the renderer's 'TQueue' sink this is a
-- non-blocking enqueue, so concurrent producers never serialize on the terminal.
emit :: Sink -> OutputEvent -> IO ()
emit (Sink f) = atomically . f

-- | The resolved rendering mode for a run.
data OutputMode
  = Human Bool Bool Bool -- ^ color enabled?, stdout is a TTY?, quiet?
  | Machine              -- ^ @--json@: JSONL event stream + a final result envelope
  deriving (Eq, Show)

-- | The parsed output flags (the runtime mode is resolved from these +
-- TTY/@NO_COLOR@ at 'Main' via 'resolveMode').
data OutputFlags = OutputFlags
  { ofJson  :: Bool
  , ofQuiet :: Bool
  }
  deriving (Eq, Show)

-- | Resolve the one 'OutputMode' for a run: @--json@ → 'Machine'; otherwise
-- human, with color iff stdout is a TTY, @NO_COLOR@ is unset, and not @--quiet@.
resolveMode :: OutputFlags -> IO OutputMode
resolveMode (OutputFlags json quiet)
  | json = pure Machine
  | otherwise = do
      tty <- hIsTerminalDevice stdout
      noColor <- isJust <$> lookupEnv "NO_COLOR"
      -- Color and live-rewriting are independent: NO_COLOR disables color but a
      -- TTY can still get a live progress line; a pipe gets neither.
      pure (Human (tty && not noColor) tty quiet)

-- | An event as a JSONL object for machine mode. Stable field order; @event@
-- discriminates from the final result envelope (which has no @event@ key).
eventJson :: OutputEvent -> Json
eventJson ev = case ev of
  ResolveStart       -> tagged "resolve-start" []
  Plan n             -> tagged "plan" [("total", JInt n)]
  FetchStart p       -> tagged "fetch-start" [("package", JString p)]
  FetchDone p        -> tagged "fetch-done" [("package", JString p)]
  CompileStart p     -> tagged "compile-start" [("package", JString p)]
  CompileDone p t c  -> tagged "compile-done" [("package", JString p), ("timeMs", JInt t), ("cached", JBool c)]
  RegisterDone p     -> tagged "register-done" [("package", JString p)]
  Finished s         -> tagged "finished" [("summary", JString s)]
  Note d             -> tagged "diagnostic" [("diagnostic", diagnosticJson d)]
  where
    tagged t fields = JObject (("event", JString t) : fields)

-- | Run an action with a live renderer: forks the renderer thread, hands the
-- action a 'Sink' to emit into, and tears down cleanly. The renderer owns
-- stdout for the event stream; the final result envelope/summary is emitted by
-- the caller after this returns (so it lands last, in order).
withRenderer :: OutputMode -> (Sink -> IO a) -> IO a
withRenderer mode body = do
  q <- newTQueueIO
  closed <- newTVarIO False
  done <- newEmptyMVar
  _ <- forkIO (renderLoop mode q closed `finally` putMVar done ())
  r <- body (Sink (writeTQueue q)) `finally` atomically (writeTVar closed True)
  takeMVar done
  pure r

-- | Progress state the (single) renderer thread folds over the event stream:
-- the closure size (from 'Plan'), how many packages have finished, and the
-- package currently shown on the live line.
data RProg = RProg
  { rTotal :: Int
  , rDone  :: Int
  , rCur   :: String
  }

-- | Drain events until the producer is finished. The STM wait wakes on a new
-- event OR on @closed@ becoming true with an empty queue (then stop) — so a
-- producer that dies mid-build cannot deadlock the renderer.
renderLoop :: OutputMode -> TQueue OutputEvent -> TVar Bool -> IO ()
renderLoop mode q closed = loop (RProg 0 0 "")
  where
    loop st = do
      mev <-
        atomically $
          (Just <$> readTQueue q)
            `orElse` ( do
                         c <- readTVar closed
                         e <- isEmptyTQueue q
                         if c && e then pure Nothing else retry
                     )
      case mev of
        Nothing -> finish
        Just ev -> render st ev >>= loop
    -- On shutdown clear any residual live progress line so the caller's final
    -- summary lands on a clean line.
    finish = case mode of
      Human _ tty _ | tty -> putStr clearLine >> hFlush stdout
      _                   -> pure ()
    render st ev = case mode of
      Machine               -> putStrLn (renderJson (eventJson ev)) >> pure st
      Human color tty quiet -> renderHuman color tty quiet st ev

-- | The human (cargo-style) renderer: committed phase headers, one live
-- progress line for the build (rewritten in place on a TTY, suppressed when
-- piped or @--quiet@), and the final summary left to the caller (it lands after
-- the renderer drains, so it never races this thread).
renderHuman :: Bool -> Bool -> Bool -> RProg -> OutputEvent -> IO RProg
renderHuman color tty quiet st ev = case ev of
  Plan n            -> pure st {rTotal = n}
  ResolveStart      -> commit (verb color "Resolving" ++ " dependencies") >> pure st
  FetchStart p      -> live (verb color "Fetching" ++ " " ++ p) >> pure st
  FetchDone _       -> pure st
  CompileStart p    -> let st' = st {rCur = p} in live (progressLine color st') >> pure st'
  CompileDone _ _ _ -> let st' = st {rDone = rDone st + 1} in live (progressLine color st') >> pure st'
  RegisterDone _    -> pure st
  Finished _        -> pure st
  Note d            -> commit (noteLine color d) >> pure st
  where
    -- A committed scrollback line: clear any live line first (TTY) then print.
    commit s
      | quiet     = pure ()
      | tty       = putStr (clearLine ++ s ++ "\n")
      | otherwise = putStrLn s
    -- The transient live line: only on a TTY (and not quiet); a pipe shows
    -- nothing here (committed headers + the final summary carry the log).
    live s
      | quiet || not tty = pure ()
      | otherwise        = putStr (clearLine ++ s) >> hFlush stdout

-- | A right-aligned cargo-style status verb in a fixed gutter, bold green.
verb :: Bool -> String -> String
verb color v = greenBold color (replicate (max 0 (12 - length v)) ' ' ++ v)

-- | The live build line: @Compiling \<pkg\> [done/total]@ while the closure
-- builds; once the closure is done (members compile after, sequentially) it
-- becomes @Building \<member\>@.
progressLine :: Bool -> RProg -> String
progressLine color st
  | rTotal st > 0 && rDone st < rTotal st =
      verb color "Compiling" ++ " " ++ rCur st ++ " " ++ dim color counter
  | otherwise = verb color "Building" ++ " " ++ rCur st
  where
    counter = "[" ++ show (rDone st) ++ "/" ++ show (rTotal st) ++ "]"

-- | A surfaced 'Note' diagnostic (warnings during a build); a single colored
-- line. Full caret rendering for hard failures lives in 'Zinc.Diagnostic'
-- ('humanError') and is emitted by the command boundary, not the stream.
noteLine :: Bool -> Diagnostic -> String
noteLine color d = sev (diagSeverity d) ++ ": " ++ diagTitle d
  where
    sev SError = red color "error"
    sev _      = yellow color "warning"
