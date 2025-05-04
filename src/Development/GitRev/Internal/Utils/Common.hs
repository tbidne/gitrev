{-# LANGUAGE QuantifiedConstraints #-}

-- | Common functionality for "Development.GitRev.Internal.Utils".
--
-- @since 0.1
module Development.GitRev.Internal.Utils.Common
  ( -- * Either projections
    projectConst,
    projectFalse,
    projectError,
    projectErrorMap,
    projectLeft,

    -- * Bifunctor utils
    joinFirst,
  )
where

import Control.Exception (Exception (displayException))
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (bimap, first))

-- $setup
-- >>> import Data.Text (Text)
-- >>> import Development.GitRev.Typed.OsString (qToCode)
-- >>> import Development.GitRev.Internal.Environment (EnvError (..))
-- >>> import Development.GitRev.Internal.Git (gitDirtyQ)
-- >>> import Development.GitRev.Internal.Utils (GitRevError (..))
-- >>> import Language.Haskell.TH (Q)

-- | Projects 'Left' to the given value.
--
-- ==== __Examples__
--
-- >>> $$(qToCode $ projectConst "FAILURE" (pure $ Left ()))
-- "FAILURE"
--
-- @since 0.1
projectConst ::
  forall f e a.
  (Functor f) =>
  a ->
  f (Either e a) ->
  f a
projectConst = projectLeft . const

-- | Projects 'Left' to 'False'.
--
-- ==== __Examples__
--
--
-- >>> $$(qToCode $ projectFalse (pure $ Left ()))
-- False
--
-- @since 0.1
projectFalse :: forall f e. (Functor f) => f (Either e Bool) -> f Bool
projectFalse = projectLeft (const False)

-- | Projects 'Left' via 'error', rendering via 'displayException'. Hence
-- an error will cause a compilation failure in 'Language.Haskell.TH.Q'.
--
-- ==== __Examples__
--
-- >>> :{
--   let gitHashOrDieQ :: Q Bool
--       gitHashOrDieQ = projectError gitDirtyQ
-- :}
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
--   let gitHashOrDieQ :: Q Bool
--       gitHashOrDieQ = (projectErrorMap show) gitDirtyQ
-- :}
--
-- @since 0.1
projectErrorMap ::
  forall f e a.
  (Functor f) =>
  (e -> String) ->
  f (Either e a) ->
  f a
projectErrorMap onErr = projectLeft (error . onErr)

-- | Projects 'Left' via the given function.
--
-- @since 0.1
projectLeft :: forall f e a. (Functor f) => (e -> a) -> f (Either e a) -> f a
projectLeft f = fmap (either f id)

-- | Join the 'first' component in a bifunctor, useful for unifying
-- nested errors with @GitRevError@.
--
-- ==== __Examples__
--
-- >>> e = Right @EnvError (Left @Text @() "an error")
-- >>> :type e
-- e :: Either EnvError (Either Text ())
--
-- >>> let joined = joinFirst GitRevErrorEnv GitRevErrorText e
-- >>> joined
-- Left (GitRevErrorText "an error")
--
-- >>> :type joined
-- joined :: Either GitRevError ()
--
-- @since 0.1
joinFirst ::
  forall p a1 a2 b c.
  ( Bifunctor p,
    forall a. Monad (p a)
  ) =>
  -- | Map outer.
  (a1 -> b) ->
  -- | Map inner.
  (a2 -> b) ->
  -- | Nested bifunctor.
  p a1 (p a2 c) ->
  -- | Flattened bifunctor.
  p b c
joinFirst embedE1 embedE2 = join . bimap embedE1 (first embedE2)
