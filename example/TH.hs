{-# LANGUAGE OverloadedLists #-}

module TH
  ( -- * Basic examples for each method
    hashLegacy,
    hashEnvVal,
    hashEnvDir,
    hashEnvValDir,

    -- * Complex example
    gitComplexData,
  )
where

import Control.Applicative (liftA3)
import Data.Bifunctor (Bifunctor (first))
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock.POSIX qualified as PosixTime
import Data.Time.Format qualified as Fmt
import Development.GitRev qualified as GR
import Development.GitRev.Typed
  ( EnvError (MkEnvError, reason, value, var),
    Errors,
    GitError,
    GitRevError,
    IndexUsed (IdxNotUsed),
  )
import Development.GitRev.Typed qualified as GRT
import Language.Haskell.TH (Code, ExpQ, Q)
import Text.Read qualified as TR

-------------------------------------------------------------------------------
--                                    Basic                                  --
-------------------------------------------------------------------------------

-- | Returns the hash or @UNKNOWN@.
hashLegacy :: ExpQ
hashLegacy = GR.gitHash

-- | Returns the hash or the result of an environment variable lookup on
-- @EXAMPLE_HASH@. Falls back to @UNKNOWN@ if both fail.
hashEnvVal :: Code Q String
hashEnvVal =
  GRT.qToCode
    . GRT.projectStringUnknown
    $ GRT.firstSuccessQ
      [ GRT.embedGitError GRT.gitHashQ,
        GRT.embedEnvError $ GRT.envValQ "EXAMPLE_HASH"
      ]

-- | Returns the hash or @UNKNOWN@. If the first attempt fails, we run git
-- again in the directory pointed to by environment variable @EXAMPLE_HOME@.
hashEnvDir :: Code Q String
hashEnvDir =
  GRT.qToCode
    . GRT.projectStringUnknown
    $ GRT.firstSuccessQ
      [ GRT.embedGitError GRT.gitHashQ,
        GRT.runGitInEnvDirQ "EXAMPLE_HOME" GRT.gitHashQ
      ]

-- | Returns the hash or @UNKNOWN@. Tries, in order:
--
-- 1. Normal git hash lookup.
-- 2. Environment variable lookup @EXAMPLE_HASH@.
-- 3. Git hash lookup in directory pointed to by environment variable
--    @EXAMPLE_HOME@.
hashEnvValDir :: Code Q String
hashEnvValDir = toCode gitHash
  where
    toCode :: Q (Either (Errors GitRevError) String) -> Code Q String
    toCode = GRT.qToCode . GRT.projectStringUnknown

    gitHash :: Q (Either (Errors GitRevError) String)
    gitHash =
      GRT.firstSuccessQ
        [ GRT.embedGitError GRT.gitHashQ,
          GRT.runGitInEnvDirQ "EXAMPLE_HOME" GRT.gitHashQ,
          GRT.embedEnvError $ GRT.envValQ "EXAMPLE_HASH"
        ]

-------------------------------------------------------------------------------
--                                   Complex                                 --
-------------------------------------------------------------------------------

-- Complex example, returning commit date (YYYY-MM-DD), (long) hash, and
-- short hash, for creating --version output. Note that because we want parity
-- between nix and cabal, we have to get our hands a bit dirty.

-- | Returns (date, hash, short hash)
gitComplexData :: Code Q (String, String, String)
gitComplexData = toCode qs
  where
    toCode = GRT.qToCode . GRT.projectError

    qs =
      GRT.firstSuccessQ
        [ GRT.embedGitError gitComplexDataFromGitQ,
          GRT.runGitInEnvDirQ "EXAMPLE_HOME" gitComplexDataFromGitQ,
          GRT.embedEnvError gitComplexDataFromEnvQ
        ]

{- HLINT ignore "Use <$>" -}

-- | Normal process, get info from git.
gitComplexDataFromGitQ :: Q (Either GitError (String, String, String))
gitComplexDataFromGitQ = do
  -- We use custom runGitQ rather than normal gitCommitDateQ because the
  -- latter uses --format=%cd e.g.
  --
  --     Thu May 1 14:05:35 2025 +1200
  --
  -- whereas we want --format=%cs i.e.
  --
  --     2025-05-01
  --
  -- We do this because we want consistency with nix, and unfortunately
  -- nix only gives us a local timestamp without any timezone information.
  -- So we throw away the timezone here too. Notice there is still a
  -- possibility of discrepancies because runGitQ implicitly includes zone
  -- info, though we live with it, since it's a minor issue.
  d <- GRT.runGitQ ["log", "HEAD", "-1", "--format=%cs"] IdxNotUsed
  h <- GRT.gitHashQ
  sh <- GRT.gitShortHashQ
  pure $ liftA3 (,,) d h sh

-- | Backup for when we cannot use git e.g. nix. We instead get the data
-- from environment variables:
--
-- - EXAMPLE_MODIFIED: unix time like "1746055623"
-- - EXAMPLE_HASH: long hash
-- - EXAMPLE_SHORT_HASH: short hash
--
-- We have to convert the unix time into the intended format
-- YYYY-MM-DD.
--
-- See packages.example from flake.nix for usage with nix.
gitComplexDataFromEnvQ :: Q (Either EnvError (String, String, String))
gitComplexDataFromEnvQ = do
  let dateVar = "EXAMPLE_MODIFIED"
  d <- fmap (>>= displayUnixTime dateVar) (GRT.envValQ dateVar)
  h <- GRT.envValQ "EXAMPLE_HASH"
  sh <- GRT.envValQ "EXAMPLE_SHORT_HASH"
  pure $ liftA3 (,,) d h sh

-- | Seconds since unix epoch to YYYY-MM-DD format.
displayUnixTime ::
  -- | Env var, for error reporting.
  String ->
  -- | Env var value, unix seconds string.
  String ->
  Either EnvError String
displayUnixTime var unixTimeStr = do
  unixSeconds <-
    first (\s -> mapEnvError $ "Error reading seconds: " ++ s) $
      TR.readEither @Integer unixTimeStr

  let posixTime = fromInteger @POSIXTime unixSeconds
      utcTime = PosixTime.posixSecondsToUTCTime posixTime
      utcFormatted = Fmt.formatTime Fmt.defaultTimeLocale "%Y-%m-%d" utcTime

  pure utcFormatted
  where
    mapEnvError :: String -> EnvError
    mapEnvError reason =
      MkEnvError
        { var,
          value = Just unixTimeStr,
          reason
        }
