-- | Provides utilities for querying environment variables.
--
-- @since 0.1
module Development.GitRev.Internal.Environment
  ( EnvError (..),
    envValQ,
    runInEnvDirQ,
    withEnvValQ,
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad (join)
import Development.GitRev.Internal.Git.Common qualified as GitC
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (Lift)
import System.Directory.OsPath qualified as Dir
import System.Environment qualified as Env
import System.OsPath qualified as OsPath

-- $setup
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
  Q (Either EnvError String)
envValQ var = withEnvValQ var pure

-- | Runs the given 'Q'-action under the directory @d@ pointed to by the
-- given environment variable.
--
-- ==== __Examples__
--
-- >>> import System.Directory (listDirectory)
-- >>> setEnv "SOME_DIR" "./src"
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
  Q (Either EnvError a)
runInEnvDirQ var m = fmap join $ withEnvValQ var $ \repoDirFp -> do
  repoDirOs <- OsPath.encodeUtf repoDirFp

  -- Try to change directory
  eCurrDir <- runIO $ GitC.trySync $ do
    currDir <- Dir.getCurrentDirectory
    Dir.setCurrentDirectory repoDirOs
    pure currDir

  let mkErr = Left . MkEnvError var (Just repoDirFp)

  case eCurrDir of
    Left ex -> do
      let rsn = "Could not set directory: " ++ displayException ex
      pure $ mkErr rsn
    Right currDir -> do
      r <- m
      eResult <- runIO $ GitC.trySync $ Dir.setCurrentDirectory currDir
      case eResult of
        Left ex -> do
          let rsn = "Could not restore directory: " ++ displayException ex
          pure $ mkErr rsn
        Right _ -> pure $ Right r

-- | Environment variable lookup failure.
--
-- @since 0.1
data EnvError = MkEnvError
  { -- | The environment variable.
    --
    -- @since 0.1
    var :: String,
    -- | The value of the environment variable, if it exists.
    --
    -- @since 0.1
    value :: Maybe String,
    -- | Text reason for the failure.
    --
    -- @since 0.1
    reason :: String
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception EnvError where
  displayException err =
    mconcat
      [ "Environment error with env variable '",
        var err,
        "', value ",
        valStr,
        ": ",
        reason err
      ]
    where
      valStr = case value err of
        Nothing -> "<none>"
        Just value -> "'" ++ value ++ "'"

-- | Runs a 'Q'-action on the result of an environment variable, if it exists.
--
-- ==== __Examples__
--
-- >>> import System.Directory (listDirectory)
-- >>> setEnv "SOME_DIR" "./src"
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
  Q (Either EnvError a)
withEnvValQ var onEnv = do
  lookupEnvQ var >>= \case
    Nothing -> pure $ Left $ MkEnvError var Nothing "No such var found."
    Just result -> Right <$> onEnv result

lookupEnvQ :: String -> Q (Maybe String)
lookupEnvQ = runIO . Env.lookupEnv
