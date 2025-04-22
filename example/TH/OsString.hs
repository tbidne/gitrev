{-# LANGUAGE QuasiQuotes #-}

module TH.OsString
  ( hashLegacy,
    hashEnvVal,
    hashEnvDir,
    hashEnvValDir,
  )
where

import Development.GitRev qualified as GR
import Development.GitRev.Typed.OsString (Errors, GitOrEnvLookupError)
import Development.GitRev.Typed.OsString qualified as GRT
import Language.Haskell.TH (Code, ExpQ, Q)
import System.OsString (OsString, osstr)

-- | Returns the hash or @UNKNOWN@.
hashLegacy :: ExpQ
hashLegacy = GR.gitHash

-- | Returns the hash or the result of an environment variable lookup on
-- @EXAMPLE_HASH@. Falls back to @UNKNOWN@ if both fail.
hashEnvVal :: Code Q OsString
hashEnvVal =
  GRT.qToCode
    . GRT.projectStringUnknown
    $ GRT.firstSuccessQ
      (GRT.embedGitError GRT.gitHashQ)
      [GRT.embedEnvLookupError $ GRT.envValQ [osstr|EXAMPLE_HASH|]]

-- | Returns the hash or @UNKNOWN@. If the first attempt fails, we run git
-- again in the directory pointed to by environment variable @EXAMPLE_HOME@.
hashEnvDir :: Code Q OsString
hashEnvDir =
  GRT.qToCode
    . GRT.projectStringUnknown
    $ GRT.firstSuccessQ
      (GRT.embedGitError GRT.gitHashQ)
      [GRT.runGitInEnvDirQ [osstr|EXAMPLE_HOME|] GRT.gitHashQ]

-- | Returns the hash or @UNKNOWN@. Tries, in order:
--
-- 1. Normal git hash lookup.
-- 2. Environment variable lookup @EXAMPLE_HASH@.
-- 3. Git hash lookup in directory pointed to by environment variable
--    @EXAMPLE_HOME@.
hashEnvValDir :: Code Q OsString
hashEnvValDir = toCode gitHash
  where
    toCode :: Q (Either (Errors GitOrEnvLookupError) OsString) -> Code Q OsString
    toCode = GRT.qToCode . GRT.projectStringUnknown

    gitHash :: Q (Either (Errors GitOrEnvLookupError) OsString)
    gitHash =
      GRT.firstSuccessQ
        (GRT.embedGitError GRT.gitHashQ)
        [ GRT.embedEnvLookupError $ GRT.envValQ [osstr|EXAMPLE_HASH|],
          GRT.runGitInEnvDirQ [osstr|EXAMPLE_HOME|] GRT.gitHashQ
        ]
