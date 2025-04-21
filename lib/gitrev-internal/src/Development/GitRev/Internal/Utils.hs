{-# LANGUAGE QuantifiedConstraints #-}

-- | Utils module.
--
-- @since 0.1
module Development.GitRev.Internal.Utils
  ( -- * Either projections
    projectStringUnknown,
    Common.projectConst,
    Common.projectFalse,
    Common.projectError,
    Common.projectErrorMap,
    Common.projectLeft,

    -- * Composing errors
    GitOrEnvLookupError (..),

    -- ** Functions
    runGitInEnvDirQ,

    -- ** Mapping utilities
    embedGitError,
    embedEnvLookupError,
    joinEnvLookupGitErrors,
    joinGitEnvLookupErrors,
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (first))
import Development.GitRev.Internal.Environment (EnvLookupError)
import Development.GitRev.Internal.Environment qualified as Env
import Development.GitRev.Internal.Git (GitError)
import Development.GitRev.Internal.Utils.Common qualified as Common
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Development.GitRev.Typed (qToCode)
-- >>> import Development.GitRev.Internal.Git (GitError (..), gitDirtyQ, gitHashQ)
-- >>> import Development.GitRev.Internal.Environment (EnvLookupError (..))
-- >>> import Language.Haskell.TH (Q, runIO, runQ)
-- >>> import System.Environment (setEnv)

-- | Projects 'Left' to the string @UNKNOWN@.
--
-- ==== __Examples__
--
-- >>> :{
--   let gitHashUnknownQ :: Q String
--       gitHashUnknownQ = projectStringUnknown gitHashQ
--   -- inling gitHashUnknownQ here due to stage restriction
--   in $$(qToCode $ projectStringUnknown gitHashQ)
-- :}
-- ...
--
-- >>> $$(qToCode $ projectStringUnknown (pure $ Left ()))
-- "UNKNOWN"
--
-- @since 0.1
projectStringUnknown ::
  forall f e.
  (Functor f) =>
  f (Either e String) ->
  f String
projectStringUnknown = Common.projectConst "UNKNOWN"

-- | Git or env lookup error.
--
-- @since 0.1
data GitOrEnvLookupError
  = -- | @since 0.1
    GitOrEnvLookupGit GitError
  | -- | @since 0.1
    GitOrEnvLookupEnvLookup EnvLookupError
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception GitOrEnvLookupError where
  displayException (GitOrEnvLookupGit ge) = displayException ge
  displayException (GitOrEnvLookupEnvLookup x) = displayException x

-- | @runGitInEnvDirQ var q@ runs @q@ in the directory given by the
-- environment variable.
--
-- ==== __Examples__
--
-- >>> setEnv "SOME_DIR" "./"
-- >>> $$(qToCode $ runGitInEnvDirQ "SOME_DIR" gitHashQ)
-- Right ...
--
-- @since 0.1
runGitInEnvDirQ ::
  forall a.
  -- | Environment variable pointing to a directory path, in which we run
  -- the git process.
  String ->
  -- | Git process to run.
  Q (Either GitError a) ->
  -- | The result.
  Q (Either GitOrEnvLookupError a)
runGitInEnvDirQ var = joinErrors . Env.runInEnvDirQ var
  where
    joinErrors = fmap joinEnvLookupGitErrors

-- | Utility function for joining lookup and git errors.
--
-- ==== __Examples__
--
-- >>> :{
--   let e :: Either EnvLookupError (Either GitError ())
--       e = Right (Left GitNotFound)
--   in joinEnvLookupGitErrors e
-- :}
-- Left (GitOrEnvLookupGit GitNotFound)
--
-- @since 0.1
joinEnvLookupGitErrors ::
  forall p a.
  ( Bifunctor p,
    forall e. Monad (p e)
  ) =>
  -- | .
  p EnvLookupError (p GitError a) ->
  p GitOrEnvLookupError a
joinEnvLookupGitErrors =
  join
    . embedGitError
    . first GitOrEnvLookupEnvLookup

-- | Utility function for joining git and lookup errors.
--
-- ==== __Examples__
--
-- >>> :{
--   let e :: Either GitError (Either EnvLookupError ())
--       e = Right (Left $ MkEnvLookupError "VAR")
--   in joinGitEnvLookupErrors e
-- :}
-- Left (GitOrEnvLookupEnvLookup (MkEnvLookupError "VAR"))
--
-- @since 0.1
joinGitEnvLookupErrors ::
  forall p a.
  ( Bifunctor p,
    forall e. Monad (p e)
  ) =>
  -- | .
  p GitError (p EnvLookupError a) ->
  p GitOrEnvLookupError a
joinGitEnvLookupErrors =
  join
    . embedEnvLookupError
    . first GitOrEnvLookupGit

-- | Embeds a 'GitError' in the larger 'GitOrEnvLookupError'.
--
-- ==== __Examples__
--
-- >>> :{
--   let q :: Q (Either GitError ())
--       q = pure (Left GitNotFound)
--   in runQ $ embedGitError q
-- :}
-- Left (GitOrEnvLookupGit GitNotFound)
--
-- @since 0.1
embedGitError ::
  forall f p a.
  ( Bifunctor p,
    Functor f
  ) =>
  -- | .
  f (p GitError a) ->
  f (p GitOrEnvLookupError a)
embedGitError = fmap (first GitOrEnvLookupGit)

-- | Embeds a 'EnvLookupError' in the larger 'GitOrEnvLookupError'.
--
-- ==== __Examples__
--
-- >>> :{
--   let q :: Q (Either EnvLookupError ())
--       q = pure (Left $ MkEnvLookupError "VAR")
--   in runQ $ embedEnvLookupError q
-- :}
-- Left (GitOrEnvLookupEnvLookup (MkEnvLookupError "VAR"))
--
-- @since 0.1
embedEnvLookupError ::
  forall f p a.
  ( Bifunctor p,
    Functor f
  ) =>
  -- | .
  f (p EnvLookupError a) ->
  f (p GitOrEnvLookupError a)
embedEnvLookupError = fmap (first GitOrEnvLookupEnvLookup)
