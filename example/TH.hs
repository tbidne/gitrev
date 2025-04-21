module TH
  ( hashLegacy,
    hashEnvVal,
    hashEnvDir,
    hashEnvValDir,
  )
where

import Development.GitRev qualified as GR
import Development.GitRev.Typed (Errors, GitOrEnvLookupError)
import Development.GitRev.Typed qualified as GRT
import Language.Haskell.TH (Code, ExpQ, Q)

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
      (GRT.embedGitError GRT.gitHashQ)
      [GRT.embedEnvLookupError $ GRT.envValQ "EXAMPLE_HASH"]

-- | Returns the hash or @UNKNOWN@. If the first attempt fails, we run git
-- again in the directory pointed to by environment variable @EXAMPLE_HOME@.
hashEnvDir :: Code Q String
hashEnvDir =
  GRT.qToCode
    . GRT.projectStringUnknown
    $ GRT.firstSuccessQ
      (GRT.embedGitError GRT.gitHashQ)
      [GRT.runGitInEnvDirQ "EXAMPLE_HOME" GRT.gitHashQ]

-- | Returns the hash or @UNKNOWN@. Tries, in order:
--
-- 1. Normal git hash lookup.
-- 2. Environment variable lookup @EXAMPLE_HASH@.
-- 3. Git hash lookup in directory pointed to by environment variable
--    @EXAMPLE_HOME@.
hashEnvValDir :: Code Q String
hashEnvValDir = toCode gitHash
  where
    toCode :: Q (Either (Errors GitOrEnvLookupError) String) -> Code Q String
    toCode = GRT.qToCode . GRT.projectError

    gitHash :: Q (Either (Errors GitOrEnvLookupError) String)
    gitHash =
      GRT.firstSuccessQ
        (GRT.embedGitError GRT.gitHashQ)
        [ GRT.embedEnvLookupError $ GRT.envValQ "EXAMPLE_HASH",
          GRT.runGitInEnvDirQ "EXAMPLE_HOME" GRT.gitHashQ
        ]
