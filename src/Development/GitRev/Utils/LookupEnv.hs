-- | Provides utilities for querying environment variables.
--
-- @since 1.40.0
module Development.GitRev.Utils.LookupEnv
  ( LookupEnvError (..),
    envValQ,
    runInEnvDirQ,
    withEnvVarQ,
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

-- | Performs an environment variable lookup in 'Q'.
--
-- @since 1.4.0
envValQ ::
  -- | The environment variable @k@.
  String ->
  -- | The result @v@ or an error.
  Q (Either LookupEnvError String)
envValQ var = withEnvVarQ var pure

-- | Runs the given 'Q'-action under the directory @d@ pointed to by the
-- given environment variable.
--
-- @since 1.4.0
runInEnvDirQ ::
  -- | The environment variable @k@ that should point to some directory
  -- @d@.
  String ->
  -- | The 'Q' action @q@.
  Q a ->
  -- | The result of running @q@ in directory @d@.
  Q (Either LookupEnvError a)
runInEnvDirQ var m = withEnvVarQ var $ \repoDirFp -> do
  repoDirOs <- OsPath.encodeUtf repoDirFp
  currDir <- runIO Dir.getCurrentDirectory
  runIO $ Dir.setCurrentDirectory repoDirOs
  r <- m
  runIO $ Dir.setCurrentDirectory currDir
  pure $ r

-- | Environment variable lookup failure. The value is the variable we
-- attempted to look up.
--
-- @since 1.4.0
newtype LookupEnvError = MkLookupEnvError String
  deriving stock (Lift, Show)

-- | @since 1.4.0
instance Exception LookupEnvError where
  displayException (MkLookupEnvError var) =
    "Failed to lookup environment variable: " ++ var

-- | Runs a 'Q'-action on the result of an environment variable, if it exists.
--
-- @since 1.4.0
withEnvVarQ ::
  -- | The environment variable @k@ to lookup.
  String ->
  -- | Function to run on @k@'s /value/ if @k@ exists.
  (String -> Q a) ->
  Q (Either LookupEnvError a)
withEnvVarQ var onEnv = do
  lookupEnvQ var >>= \case
    Nothing -> pure $ Left $ MkLookupEnvError var
    Just result -> Right <$> onEnv result

lookupEnvQ :: String -> Q (Maybe String)
lookupEnvQ s = runIO (Env.lookupEnv s)
