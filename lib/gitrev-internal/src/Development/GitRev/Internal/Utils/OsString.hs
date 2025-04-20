{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Development.GitRev.Internal.Utils for 'OsString'.
--
-- @since 0.1
module Development.GitRev.Internal.Utils.OsString
  ( -- * Either projections
    projectStringUnknown,
    projectString,
    projectFalse,
    projectError,
    projectErrorMap,

    -- * Composing errors
    GitOrLookupEnvError (..),

    -- ** Functions
    runGitInEnvDirQ,

    -- ** Mapping utilities
    embedGitError,
    embedLookupEnvError,
    joinLookupEnvGitErrors,
    joinGitLookupEnvErrors,
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (first))
import Development.GitRev.Internal.Environment.OsString (LookupEnvError)
import Development.GitRev.Internal.Environment.OsString qualified as Env
import Development.GitRev.Internal.Git.OsString (GitError)
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Syntax (Lift)
import System.OsString (OsString, osstr)

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> :set -XTemplateHaskell
-- >>> import Development.GitRev.Typed.OsString (qToCode)
-- >>> import Development.GitRev.Internal.Git.OsString (GitError (..), gitDirtyQ, gitHashQ)
-- >>> import Development.GitRev.Internal.Environment.OsString (LookupEnvError (..))
-- >>> import Language.Haskell.TH (Q, runIO, runQ)
-- >>> import System.Environment (setEnv)
-- >>> import System.OsString (OsString, osstr)

-- | Projects 'Left' to the string @UNKNOWN@.
--
-- ==== __Examples__
--
-- >>> :{
--   let gitHashUnknownQ :: Q OsString
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
  f (Either e OsString) ->
  f OsString
projectStringUnknown = projectString [osstr|UNKNOWN|]

-- | Projects 'Left' to the given string.
--
-- ==== __Examples__
--
-- >>> :{
--   let gitHashDefStringQ :: Q OsString
--       gitHashDefStringQ = projectString [osstr|FAILURE|] gitHashQ
--   in $$(qToCode $ projectString [osstr|FAILURE|] gitHashQ)
-- :}
-- ...
--
-- >>> $$(qToCode $ projectString [osstr|FAILURE|] (pure $ Left ()))
-- "FAILURE"
--
-- @since 0.1
projectString ::
  forall f e.
  (Functor f) =>
  OsString ->
  f (Either e OsString) ->
  f OsString
projectString = projectLeft . const

-- | Projects 'Left' to 'False'.
--
-- ==== __Examples__
--
-- >>> :{
--   let gitDirtyDefFalseQ :: Q Bool
--       gitDirtyDefFalseQ = projectFalse gitDirtyQ
--   in $$(qToCode $ projectFalse gitDirtyQ)
-- :}
-- ...
--
-- >>> $$(qToCode $ projectFalse (pure $ Left ()))
-- False
--
-- @since 0.1
projectFalse :: forall f e. (Functor f) => f (Either e Bool) -> f Bool
projectFalse = projectLeft (const False)

-- | Projects 'Left' via 'error', rendering via 'displayException'. Hence
-- an error will cause a compilation failure.
--
-- ==== __Examples__
--
-- >>> :{
--   let gitHashOrDieQ :: Q OsString
--       gitHashOrDieQ = projectError gitHashQ
--   in $$(qToCode $ projectError gitHashQ)
-- :}
-- ...
--
-- @since 0.1
projectError :: forall f e a. (Exception e, Functor f) => f (Either e a) -> f a
projectError = projectErrorMap displayException

-- | Projects 'Left' via 'error', rendering via the given function. Hence
-- an error will cause a compilation failure.
--
-- ==== __Examples__
--
-- >>> :{
--   let gitHashOrDieQ :: Q OsString
--       gitHashOrDieQ = (projectErrorMap show) gitHashQ
--   in $$(qToCode $ (projectErrorMap show) gitHashQ)
-- :}
-- ...
--
-- @since 0.1
projectErrorMap ::
  forall f e a.
  (Functor f) =>
  (e -> String) ->
  f (Either e a) ->
  f a
projectErrorMap onErr = projectLeft (error . onErr)

projectLeft :: forall f e a. (Functor f) => (e -> a) -> f (Either e a) -> f a
projectLeft f = fmap (either f id)

-- | Git or env lookup error.
--
-- @since 0.1
data GitOrLookupEnvError
  = -- | @since 0.1
    GitOrLookupEnvGit GitError
  | -- | @since 0.1
    GitOrLookupEnvLookupEnv LookupEnvError
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Lift,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception GitOrLookupEnvError where
  displayException (GitOrLookupEnvGit ge) = displayException ge
  displayException (GitOrLookupEnvLookupEnv x) = displayException x

-- | @runGitInEnvDirQ var q@ runs @q@ in the directory given by the
-- environment variable.
--
-- ==== __Examples__
--
-- >>> setEnv "SOME_DIR" "./"
-- >>> $$(qToCode $ runGitInEnvDirQ [osstr|SOME_DIR|] gitHashQ)
-- Right ...
--
-- @since 0.1
runGitInEnvDirQ ::
  forall a.
  -- | Environment variable pointing to a directory path, in which we run
  -- the git process.
  OsString ->
  -- | Git process to run.
  Q (Either GitError a) ->
  -- | The result.
  Q (Either GitOrLookupEnvError a)
runGitInEnvDirQ var = joinErrors . Env.runInEnvDirQ var
  where
    joinErrors = fmap joinLookupEnvGitErrors

-- | Utility function for joining lookup and git errors.
--
-- ==== __Examples__
--
-- >>> :{
--   let e :: Either LookupEnvError (Either GitError ())
--       e = Right (Left GitNotFound)
--   in joinLookupEnvGitErrors e
-- :}
-- Left (GitOrLookupEnvGit GitNotFound)
--
-- @since 0.1
joinLookupEnvGitErrors ::
  forall p a.
  ( Bifunctor p,
    forall e. Monad (p e)
  ) =>
  -- | .
  p LookupEnvError (p GitError a) ->
  p GitOrLookupEnvError a
joinLookupEnvGitErrors =
  join
    . embedGitError
    . first GitOrLookupEnvLookupEnv

-- | Utility function for joining git and lookup errors.
--
-- ==== __Examples__
--
-- >>> :{
--   let e :: Either GitError (Either LookupEnvError ())
--       e = Right (Left $ MkLookupEnvError [osstr|VAR|])
--   in joinGitLookupEnvErrors e
-- :}
-- Left (GitOrLookupEnvLookupEnv (MkLookupEnvError "VAR"))
--
-- @since 0.1
joinGitLookupEnvErrors ::
  forall p a.
  ( Bifunctor p,
    forall e. Monad (p e)
  ) =>
  -- | .
  p GitError (p LookupEnvError a) ->
  p GitOrLookupEnvError a
joinGitLookupEnvErrors =
  join
    . embedLookupEnvError
    . first GitOrLookupEnvGit

-- | Embeds a 'GitError' in the larger 'GitOrLookupEnvError'.
--
-- ==== __Examples__
--
-- >>> :{
--   let q :: Q (Either GitError ())
--       q = pure (Left GitNotFound)
--   in runQ $ embedGitError q
-- :}
-- Left (GitOrLookupEnvGit GitNotFound)
--
-- @since 0.1
embedGitError ::
  forall f p a.
  ( Bifunctor p,
    Functor f
  ) =>
  -- | .
  f (p GitError a) ->
  f (p GitOrLookupEnvError a)
embedGitError = fmap (first GitOrLookupEnvGit)

-- | Embeds a 'LookupEnvError' in the larger 'GitOrLookupEnvError'.
--
-- ==== __Examples__
--
-- >>> :{
--   let q :: Q (Either LookupEnvError ())
--       q = pure (Left $ MkLookupEnvError [osstr|VAR|])
--   in runQ $ embedLookupEnvError q
-- :}
-- Left (GitOrLookupEnvLookupEnv (MkLookupEnvError "VAR"))
--
-- @since 0.1
embedLookupEnvError ::
  forall f p a.
  ( Bifunctor p,
    Functor f
  ) =>
  -- | .
  f (p LookupEnvError a) ->
  f (p GitOrLookupEnvError a)
embedLookupEnvError = fmap (first GitOrLookupEnvLookupEnv)
