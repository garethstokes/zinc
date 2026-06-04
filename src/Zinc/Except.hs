-- | The shared error-handling convention for zinc's internal pipelines.
--
-- Functions that can fail are written as @'Result' a@ (= @ExceptT ZincError IO a@)
-- so do-notation short-circuits on the first error instead of hand-threading
-- @Either@ through nested @case@s. Pipelines fail with a structured 'ZincError'
-- /value/ (rendered once at the boundary by "Zinc.Diagnostic"); 'runResult'
-- unwraps to @IO (Either ZincError a)@ at the CLI/test boundary.
--
-- Migration bridges: 'failWith', 'orFail', and 'liftEither' accept the old
-- @String@ shape and wrap it in 'OtherError', so not-yet-converted call sites
-- keep compiling. New code should prefer the structured variants 'failWithError',
-- 'orFailE', and 'liftEitherE'.
module Zinc.Except
  ( Result
  , runResult
  , orFail
  , orFailE
  , liftEither
  , liftEitherE
  , failWith
  , failWithError
  , liftIO
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), except, runExceptT, throwE)
import Zinc.Diagnostic (ZincError (OtherError))

-- | An effectful pipeline that may fail with a structured 'ZincError'.
type Result a = ExceptT ZincError IO a

-- | Run a pipeline back to the @IO (Either ZincError a)@ shape used at the
-- CLI/test boundary.
runResult :: Result a -> IO (Either ZincError a)
runResult = runExceptT

-- | Lift a not-yet-converted @IO (Either String a)@ action into a pipeline,
-- short-circuiting on its 'Left' (wrapped as 'OtherError').
orFail :: IO (Either String a) -> Result a
orFail io = ExceptT (fmap (either (Left . OtherError) Right) io)

-- | Lift an already-structured @IO (Either ZincError a)@ action into a pipeline.
orFailE :: IO (Either ZincError a) -> Result a
orFailE = ExceptT

-- | Lift a pure @Either String a@ (e.g. a parser result) into a pipeline,
-- wrapping its error as 'OtherError'.
liftEither :: Either String a -> Result a
liftEither = except . either (Left . OtherError) Right

-- | Lift a pure, already-structured @Either ZincError a@ into a pipeline.
liftEitherE :: Either ZincError a -> Result a
liftEitherE = except

-- | Abort the pipeline with a bare message (wrapped as 'OtherError').
failWith :: String -> Result a
failWith = throwE . OtherError

-- | Abort the pipeline with a structured error.
failWithError :: ZincError -> Result a
failWithError = throwE
