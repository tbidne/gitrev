module TH
  ( hashLegacy,
    hashEnvVal,
    hashEnvDir,
    hashEnvValDir,
  )
where

import Development.GitRev qualified as GR
import Development.GitRev.Typed qualified as GRT
import Development.GitRev.Utils (GitOrLookupEnvError)
import Development.GitRev.Utils qualified as GRU
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
    $ GRU.firstSuccessQ
      (GRT.embedGitError GRT.gitHashQ)
      [GRT.envValQ "EXAMPLE_HASH"]

hashEnvDir :: Code Q String
hashEnvDir =
  GRT.qToCode
    . GRT.projectStringUnknown
    $ GRU.firstSuccessQ
      (GRT.embedGitError GRT.gitHashQ)
      [GRT.runGitInEnvDirQ "EXAMPLE_HOME" GRT.gitHashQ]

hashEnvValDir :: Code Q String
hashEnvValDir = toCode gitHash
  where
    toCode :: Q (Either GitOrLookupEnvError String) -> Code Q String
    toCode = GRT.qToCode . GRT.projectStringUnknown

    gitHash :: Q (Either GitOrLookupEnvError String)
    gitHash =
      GRU.firstSuccessQ
        (GRT.embedGitError GRT.gitHashQ)
        [ GRT.envValQ "EXAMPLE_HASH",
          GRT.runGitInEnvDirQ "EXAMPLE_HOME" GRT.gitHashQ
        ]
