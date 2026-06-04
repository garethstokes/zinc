-- | The shared error-handling convention for zinc's internal pipelines.
--
-- Functions that can fail are written as @'Result' a@ (= @ExceptT String IO a@)
-- so do-notation short-circuits on the first error instead of hand-threading
-- @Either@ through nested @case@s. Unwrap with 'runResult' at the CLI/test
-- boundary, where the @IO (Either String a)@ shape is still expected.
--
-- Bridges for incremental migration: 'orFail' lifts a not-yet-converted
-- @IO (Either String a)@ action, and 'liftEither' lifts a pure parser result.
module Zinc.Except
  ( Result
  , runResult
  , orFail
  , liftEither
  , failWith
  , liftIO
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), except, runExceptT, throwE)

-- | An effectful pipeline that may fail with a @String@ error message.
type Result a = ExceptT String IO a

-- | Run a pipeline back to the @IO (Either String a)@ shape used at the
-- CLI/test boundary.
runResult :: Result a -> IO (Either String a)
runResult = runExceptT

-- | Lift an @IO (Either String a)@ action (e.g. a helper not yet converted to
-- 'Result') into a pipeline, short-circuiting on its 'Left'.
orFail :: IO (Either String a) -> Result a
orFail = ExceptT

-- | Lift a pure @Either String a@ (e.g. a parser result) into a pipeline.
liftEither :: Either String a -> Result a
liftEither = except

-- | Abort the pipeline with an error message.
failWith :: String -> Result a
failWith = throwE
