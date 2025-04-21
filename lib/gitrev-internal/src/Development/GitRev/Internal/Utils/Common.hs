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
  )
where

import Control.Exception (Exception (displayException))

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> import Development.GitRev.Typed.OsString (qToCode)
-- >>> import Development.GitRev.Internal.Git (gitDirtyQ)

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
-- an error will cause a compilation failure.
--
-- ==== __Examples__
--
-- >>> :{
--   let gitHashOrDieQ :: Q Bool
--       gitHashOrDieQ = projectError gitDirtyQ
--   in $$(qToCode $ projectError gitDirtyQ)
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
--   let gitHashOrDieQ :: Q Bool
--       gitHashOrDieQ = (projectErrorMap show) gitDirtyQ
--   in $$(qToCode $ (projectErrorMap show) gitDirtyQ)
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

-- | Projects 'Left' via the given function.
--
-- @since 0.1
projectLeft :: forall f e a. (Functor f) => (e -> a) -> f (Either e a) -> f a
projectLeft f = fmap (either f id)
