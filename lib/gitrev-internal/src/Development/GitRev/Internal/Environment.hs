-- | Provides utilities for querying environment variables.
--
-- @since 0.1
module Development.GitRev.Internal.Environment
  ( EnvLookupError (..),
    envValQ,
    runInEnvDirQ,
    withEnvValQ,
  )
where

import Control.Exception
  ( Exception (displayException),
  )
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (Lift)
import System.Directory.OsPath qualified as Dir
import System.Environment qualified as Env
import System.OsPath qualified as OsPath

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Development.GitRev.Typed (qToCode)
-- >>> import Language.Haskell.TH (Q, runIO, runQ)
-- >>> import System.Environment (setEnv)

-- | Performs an environment variable lookup in 'Q'.
--
-- ==== __Examples__
--
-- >>> setEnv "SOME_VAR" "val"
-- >>> $$(qToCode $ envValQ "SOME_VAR")
-- Right "val"
--
-- @since 0.1
envValQ ::
  -- | The environment variable @k@.
  String ->
  -- | The result @v@ or an error.
  Q (Either EnvLookupError String)
envValQ var = withEnvValQ var pure

-- | Runs the given 'Q'-action under the directory @d@ pointed to by the
-- given environment variable.
--
-- ==== __Examples__
--
-- >>> import System.Directory (listDirectory)
-- >>> setEnv "SOME_DIR" "./lib/gitrev/src"
-- >>> $$(qToCode $ runInEnvDirQ "SOME_DIR" $ runIO (listDirectory "./"))
-- Right ["Development"]
--
-- @since 0.1
runInEnvDirQ ::
  forall a.
  -- | The environment variable @k@ that should point to some directory
  -- @d@.
  String ->
  -- | The 'Q' action @q@.
  Q a ->
  -- | The result of running @q@ in directory @d@.
  Q (Either EnvLookupError a)
runInEnvDirQ var m = withEnvValQ var $ \repoDirFp -> do
  repoDirOs <- OsPath.encodeUtf repoDirFp
  currDir <- runIO Dir.getCurrentDirectory
  runIO $ Dir.setCurrentDirectory repoDirOs
  r <- m
  runIO $ Dir.setCurrentDirectory currDir
  pure $ r

-- | Environment variable lookup failure. The value is the variable we
-- attempted to look up.
--
-- @since 0.1
newtype EnvLookupError = MkEnvLookupError String
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception EnvLookupError where
  displayException (MkEnvLookupError var) =
    "Failed to lookup environment variable: " ++ var

-- | Runs a 'Q'-action on the result of an environment variable, if it exists.
--
-- ==== __Examples__
--
-- >>> import System.Directory (listDirectory)
-- >>> setEnv "SOME_DIR" "./lib/gitrev/src"
-- >>> $$(qToCode $ withEnvValQ "SOME_DIR" (runIO . listDirectory))
-- Right ["Development"]
--
-- @since 0.1
withEnvValQ ::
  forall a.
  -- | The environment variable @k@ to lookup.
  String ->
  -- | Function to run on @k@'s /value/ if @k@ exists.
  (String -> Q a) ->
  Q (Either EnvLookupError a)
withEnvValQ var onEnv = do
  lookupEnvQ var >>= \case
    Nothing -> pure $ Left $ MkEnvLookupError var
    Just result -> Right <$> onEnv result

lookupEnvQ :: String -> Q (Maybe String)
lookupEnvQ = runIO . Env.lookupEnv
