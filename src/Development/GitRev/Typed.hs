-- |
-- Module      :  $Header$
-- Copyright   :  (c) 2025 Thomas Bidne
-- License     :  BSD3
-- Maintainer  :  tbidne@protonmail.com
--
-- Typed version of "Development.GitRev".
--
-- @since 0.1
module Development.GitRev.Typed
  ( -- * Basic functions
    -- $basic
    gitBranch,
    gitCommitCount,
    gitCommitDate,
    gitDescribe,
    gitDiff,
    gitDirty,
    gitDirtyTracked,
    gitHash,
    gitShortHash,
    gitTree,

    -- * Custom behavior
    -- $custom

    -- ** Out-of-tree builds
    -- $out-of-tree

    -- ** Multiple queries
    -- $multiple

    -- ** Git Primitives
    Git.gitBranchQ,
    Git.gitCommitCountQ,
    Git.gitCommitDateQ,
    Git.gitDescribeQ,
    Git.gitDiffQ,
    Git.gitDirtyQ,
    Git.gitDirtyTrackedQ,
    Git.gitHashQ,
    Git.gitShortHashQ,
    Git.gitTreeQ,

    -- *** Running your own git actions
    Git.runGitQ,
    Git.runGitPostProcessQ,
    IndexUsed (..),

    -- ** Environment lookup
    -- $environment
    Env.envValQ,
    Env.runInEnvDirQ,
    Utils.runGitInEnvDirQ,
    Env.withEnvValQ,

    -- ** Q to Code
    qToCode,

    -- ** Q Combinators

    -- *** Laziness
    -- $laziness
    QFirst,
    QFirst.mkQFirst,
    QFirst.unQFirst,
    QFirst.firstSuccessQ,
    Errors,
    QFirst.mkErrors,
    QFirst.unErrors,

    -- *** Eliminating Either
    Utils.projectStringUnknown,
    Utils.projectConst,
    Utils.projectFalse,
    Utils.projectError,
    Utils.projectErrorMap,
    Utils.projectLeft,

    -- ** Errors
    GitRevError (..),
    GitError (..),
    EnvError (..),

    -- *** Utilities
    Utils.embedGitError,
    Utils.embedEnvError,
    Utils.embedTextError,
    Utils.joinFirst,
  )
where

import Development.GitRev.Internal.Environment
  ( EnvError (MkEnvError),
  )
import Development.GitRev.Internal.Environment qualified as Env
import Development.GitRev.Internal.Git
  ( GitError (MkGitError),
    IndexUsed (IdxNotUsed, IdxUsed),
  )
import Development.GitRev.Internal.Git qualified as Git
import Development.GitRev.Internal.QFirst (Errors, QFirst)
import Development.GitRev.Internal.QFirst qualified as QFirst
import Development.GitRev.Internal.Utils
  ( GitRevError
      ( GitRevErrorEnv,
        GitRevErrorGit
      ),
  )
import Development.GitRev.Internal.Utils qualified as Utils
import Language.Haskell.TH (Code, Q)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift (lift), TExp (TExp))

-- $setup
-- >>> :set -XOverloadedLists
-- >>> import Data.Functor (($>))
-- >>> import Development.GitRev.Typed
-- >>> import Language.Haskell.TH (Code, Q, runIO)

-- $basic
--
-- These functions are simple, merely a typed version of "Development.GitRev"'s
-- API.
--
-- __NOTE:__ These functions do /not/ error if git fails to run, opting
-- instead to return some default value (e.g. string @UNKNOWN@, boolean @False@).

-- $custom
--
-- These functions allow defining custom behavior. For instance, using
-- the primitive 'Development.GitRev.Typed.gitHashQ' and combinator
-- 'Development.GitRev.Typed.projectError', we can define a variant of
-- 'Development.GitRev.Typed.gitHash' that instead fails to compile if there
-- are any problems with git:
--
-- @
-- -- simplified type signatures
-- 'qToCode'      :: 'Q' a -> 'Code' 'Q' a
-- 'Development.GitRev.Typed.projectError' :: 'Q' ('Either' e a) -> 'Q' a
-- 'Development.GitRev.Typed.gitHashQ'     :: 'Q' ('Either' 'GitError' 'String')
-- @
--
-- >>> :{
--   let gitHashOrDie :: Code Q String
--       gitHashOrDie = qToCode $ projectError gitHashQ
-- :}
--
-- We can also define a function that falls back to an environment variable,
-- in case the git command fails. 'Development.GitRev.Typed.firstSuccessQ'
-- takes the first action that returns 'Right'.
--
-- @
-- 'Development.GitRev.Typed.firstSuccessQ' :: 'Data.List.NonEmpty' ('Q' ('Either' e a)) -> 'Q' ('Either' ('Errors' e) a)
--
-- -- unifying errors
-- 'Development.GitRev.Typed.embedGitError' :: 'Q' ('Either' 'GitError' a) -> 'Q' ('Either' 'GitRevError' a)
-- 'Development.GitRev.Typed.embedEnvError' :: 'Q' ('Either' 'EnvError' a) -> 'Q' ('Either' 'GitRevError' a)
--
-- -- look up environment variable
-- 'Development.GitRev.Typed.envValQ' :: 'String' -> 'Q' ('Either' 'EnvError' 'String')
-- @
--
-- >>> :{
--   let gitHashEnv :: String -> Code Q (Either (Errors GitRevError) String)
--       gitHashEnv var =
--         qToCode $
--           firstSuccessQ
--             -- using -XOverloadedLists to make this a little nicer
--             -- syntactically.
--             [ embedGitError gitHashQ,
--               embedEnvError $ envValQ var
--             ]
-- :}
--
-- Naturally, these can be combined:
--
-- >>> :{
--   let gitHashEnvOrDie :: String -> Code Q String
--       gitHashEnvOrDie var =
--         qToCode
--           . projectError
--           $ firstSuccessQ
--             [ embedGitError gitHashQ,
--               embedEnvError $ envValQ var
--             ]
-- :}

-- $out-of-tree
--
-- Custom definitions are particularly useful for \"out-of-tree\" builds,
-- where the build takes place outside of the normal git tree.
--
-- These builds present a problem, as we normally rely on building in the
-- project directory where the @.git@ directory is easy to locate. For
-- example, while 'gitHash' will work for @\'cabal build\'@, it will not work
-- for @\'nix build\'@ or @\'cabal install\'@. Fortunately, there are
-- workarounds, both relying on passing the right data via environment
-- variables.
--
-- 1. Passing the git directory.
--
--     For situations where we can pass the current directory during
--     installation e.g.
--
--     > $ export EXAMPLE_HOME=$(pwd); cabal install example
--
--     We can use
--
--     @
--     'Development.GitRev.Typed.runGitInEnvDirQ' :: 'String' -> 'Q' ('Either' 'GitError' a) -> 'Q' ('Either' 'GitRevError' a)
--     @
--
--     to define
--
--     >>> :{
--       let gitHashSrcDir :: Code Q String
--           gitHashSrcDir =
--             qToCode
--             . projectStringUnknown
--             $ firstSuccessQ
--               [ -- 1. We first try normal gitHashQ.
--                 embedGitError gitHashQ,
--                 -- 2. If that fails, we try again in the directory pointed
--                 --    to by "EXAMPLE_HOME".
--                 runGitInEnvDirQ "EXAMPLE_HOME" gitHashQ
--               ]
--     :}
--
--     If the initial call to 'Development.GitRev.Typed.gitHashQ' fails, then
--     we will try again, running the command from the directory pointed to by
--     @EXAMPLE_HOME@.
--
-- 2. Passing the value itself.
--
--     This approach can work well with nix, as nix flakes provides
--     a variety of revisions via its @self@ interface. For example:
--
--     @
--       # Injecting the git hash via EXAMPLE_HASH where drv is the normal
--       # derivation.
--       drv.overrideAttrs (oldAttrs: {
--         # Also: self.shortRev, self.dirtyShortRev
--         EXAMPLE_HASH = \"${self.rev or self.dirtyRev}\";
--       });
--     @
--
--     Then we can define
--
--     >>> :{
--       let gitHashVal :: Code Q String
--           gitHashVal =
--             qToCode
--             . projectStringUnknown
--             $ firstSuccessQ
--               [ -- 1. We first try normal gitHashQ.
--                 embedGitError gitHashQ,
--                 -- 2. If that fails, get the value directly from
--                 --    "EXAMPLE_HASH".
--                 embedEnvError $ envValQ "EXAMPLE_HASH"
--               ]
--     :}
--
--     Once again, if the first attempt fails, we will run the second action,
--     looking for the value of @EXAMPLE_HASH@.
--
-- Finally, we can compose these together to make a function that works for all
-- three cases:
--
-- >>> :{
--   let gitHashValSrc :: Code Q String
--       gitHashValSrc =
--         qToCode
--           . projectStringUnknown
--           $ firstSuccessQ
--             [ embedGitError gitHashQ,
--               runGitInEnvDirQ "EXAMPLE_HOME" gitHashQ,
--               embedEnvError $ envValQ "EXAMPLE_HASH"
--             ]
-- :}

-- $multiple
--
-- Using the typed interfaced, it is easy to combine multiple queries in
-- a safe way.
--
-- >>> :{
--   import Control.Applicative (liftA3)
--   -- | Returns (date, hash, short hash)
--   gitComplexData :: Code Q (String, String, String)
--   gitComplexData = toCode qs
--     where
--       toCode = qToCode . projectError
--       qs =
--         firstSuccessQ
--           [ embedGitError gitComplexDataFromGitQ,
--             runGitInEnvDirQ "EXAMPLE_HOME" gitComplexDataFromGitQ
--           ]
--   gitComplexDataFromGitQ :: Q (Either GitError (String, String, String))
--   gitComplexDataFromGitQ = do
--     -- custom command for commit YYYY-MM-DD
--     d <- runGitQ ["log", "HEAD", "-1", "--format=%cs"] IdxNotUsed
--     h <- gitHashQ
--     sh <- gitShortHashQ
--     pure $ liftA3 (,,) d h sh
-- :}

-- $laziness
--
-- As alluded to above, 'Q'\'s default semigroup instance is not lazy enough:
--
-- >>> :{
--   $$( qToCode $
--       (runIO (putStrLn "in q1") $> (Right "q1") :: Q (Either () String))
--         <> (runIO (putStrLn "in q2") $> Left ())
--     )
-- :}
-- in q1
-- in q2
-- Right "q1"
--
-- For this reason, we introduce 'QFirst':
--
-- @
-- 'Development.GitRev.Typed.mkQFirst' :: 'Q' ('Either' e a) -> 'QFirst' e a
-- 'Development.GitRev.Typed.unQFirst' :: 'QFirst' e a -> 'Q' ('Either' ('Errors' e) a)
-- @
--
-- >>> :{
--   $$( qToCode $ unQFirst $
--       (mkQFirst $ runIO (putStrLn "in q1") $> (Right "q1") :: QFirst () String)
--         <> (mkQFirst $ runIO (putStrLn "in q2") $> Left ())
--     )
-- :}
-- in q1
-- Right "q1"
--
-- The function
--
-- @
--   'Development.GitRev.Typed.firstSuccessQ' :: 'Data.List.NonEmpty' ('Q' ('Either' e a)) -> 'Q' ('Either' ('Errors' e) a)
-- @
--
-- utilizes 'QFirst' for sequencing a series of 'Q' actions, stopping after the
-- first success.

-- $environment
--
-- This section allows looking up data via environment variables.
--
-- === Warning: caching
--
-- Suppose we install an executable @example-exe@, that depends on
-- @example-lib@, where the latter contains a TH splice for env var @FOO@.
-- We first install via:
--
-- > export FOO=A; cabal install example-exe --installdir=build --overwrite-policy=always
--
-- This will build @example-lib@ with @A@ in the splice.
--
-- Now suppose we run @cabal clean@ (or delete the build directory e.g.
-- @dist-newstyle@) and run:
--
-- > export FOO=B; cabal install example-exe --installdir=build --overwrite-policy=always
--
-- What will the result of the splice be? Probably still @A@! The problem is
-- that cabal does not know that the environment has changed, hence it detects
-- no changes, and @example-lib@ is not re-installed.
--
-- The solution is to manually delete the library @example-lib@, which
-- probably exists in the state directory e.g.
-- @~\/.local\/state\/cabal\/store\/ghc-9.8.4-inplace@.

-- | Return the branch (or tag) name of the current git commit, or @UNKNOWN@
-- if not in a git repository. For detached heads, this will just be
-- "HEAD".
--
-- ==== __Examples__
--
-- > λ. $$gitBranch
-- > "main"
--
-- @since 0.1
gitBranch :: Code Q String
gitBranch = qToCode $ Utils.projectStringUnknown Git.gitBranchQ

-- | Return the number of commits in the current head.
--
-- ==== __Examples__
--
-- > λ. $$gitCommitCount
-- > "47"
--
-- @since 0.1
gitCommitCount :: Code Q String
gitCommitCount = qToCode $ Utils.projectStringUnknown Git.gitCommitCountQ

-- | Return the commit date of the current head.
--
-- ==== __Examples__
--
-- > λ. $$gitCommitDate
-- > "Mon Apr 14 22:14:44 2025 +1200"
--
-- @since 0.1
gitCommitDate :: Code Q String
gitCommitDate = qToCode $ Utils.projectStringUnknown Git.gitCommitDateQ

-- | Return the long git description for the current git commit, or
-- @UNKNOWN@ if not in a git repository.
--
-- ==== __Examples__
--
-- > λ. $$gitDescribe
-- > "1.2.0-14-g40b5d7b"
--
-- @since 0.1
gitDescribe :: Code Q String
gitDescribe = qToCode $ Utils.projectStringUnknown Git.gitDescribeQ

-- | Return the diff of the working copy with HEAD.
--
-- ==== __Examples__
--
-- > λ. $$gitDiff
-- > "diff ..."
--
-- @since 0.1
gitDiff :: Code Q String
gitDiff = qToCode $ Utils.projectStringUnknown Git.gitDiffQ

-- | Return @True@ if there are non-committed files present in the
-- repository.
--
-- ==== __Examples__
--
-- > λ. $$gitDirty
-- > False
--
-- @since 0.1
gitDirty :: Code Q Bool
gitDirty = qToCode $ Utils.projectFalse Git.gitDirtyQ

-- | Return @True@ if there are non-commited changes to tracked files
-- present in the repository.
--
-- ==== __Examples__
--
-- > λ. $$gitDirtyTracked
-- > False
--
-- @since 0.1
gitDirtyTracked :: Code Q Bool
gitDirtyTracked = qToCode $ Utils.projectFalse Git.gitDirtyTrackedQ

-- | Return the hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository.
--
-- ==== __Examples__
--
-- > λ. $$gitHash
-- > "e67e943dd03744d3f93c21f84e127744e6a04543"
--
-- @since 0.1
gitHash :: Code Q String
gitHash = qToCode $ Utils.projectStringUnknown Git.gitHashQ

-- | Return the short hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository.
--
-- ==== __Examples__
--
-- > λ. $$gitShortHash
-- > "e67e943"
--
-- @since 0.1
gitShortHash :: Code Q String
gitShortHash = qToCode $ Utils.projectStringUnknown Git.gitShortHashQ

-- | Return the hash of the current tree.
--
-- ==== __Examples__
--
-- > λ. $$gitTree
-- > "b718a493773568bbf920a4710b5b83bd1762dbb9"
--
-- @since 0.1
gitTree :: Code Q String
gitTree = qToCode $ Utils.projectStringUnknown Git.gitTreeQ

-- | Lifts a 'Q' computation to 'Code', for usage with typed TH.
--
-- @since 0.1
qToCode :: forall a. (Lift a) => Q a -> Code Q a
qToCode = TH.liftCode . fmap TExp . (>>= lift)
