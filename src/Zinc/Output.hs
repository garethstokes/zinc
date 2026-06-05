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
import System.IO (hIsTerminalDevice, stdout)
import Zinc.Diagnostic (Diagnostic, diagnosticJson)
import Zinc.Json (Json (..), renderJson)

-- | A structured output event. Progress through a build flows as these; a
-- 'Diagnostic' also rides the stream (so warnings/errors render in one place).
data OutputEvent
  = ResolveStart
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
  = Human Bool Bool -- ^ color enabled?, quiet?
  | Machine         -- ^ @--json@: JSONL event stream + a final result envelope
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
      pure (Human (tty && not noColor && not quiet) quiet)

-- | An event as a JSONL object for machine mode. Stable field order; @event@
-- discriminates from the final result envelope (which has no @event@ key).
eventJson :: OutputEvent -> Json
eventJson ev = case ev of
  ResolveStart       -> tagged "resolve-start" []
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

-- | Drain events until the producer is finished. The STM wait wakes on a new
-- event OR on @closed@ becoming true with an empty queue (then stop) — so a
-- producer that dies mid-build cannot deadlock the renderer.
renderLoop :: OutputMode -> TQueue OutputEvent -> TVar Bool -> IO ()
renderLoop mode q closed = loop
  where
    loop = do
      mev <-
        atomically $
          (Just <$> readTQueue q)
            `orElse` ( do
                         c <- readTVar closed
                         e <- isEmptyTQueue q
                         if c && e then pure Nothing else retry
                     )
      case mev of
        Nothing -> pure ()
        Just ev -> render ev >> loop
    -- Machine mode: one JSONL line per event. Human progress/color is hw6.2;
    -- for now the human renderer streams nothing (the final summary is printed
    -- by the caller).
    render ev = case mode of
      Machine    -> putStrLn (renderJson (eventJson ev))
      Human _ _  -> pure ()
