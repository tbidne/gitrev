module TH
  ( hashLegacy,
    hashEnvVal,
    hashEnvDir,
    hashEnvValDir,
  )
where

import Development.GitRev qualified as GR
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
    . GRT.liftDefString
    . GRT.envValFallback "EXAMPLE_HASH"
    $ GRT.gitHashQ

hashEnvDir :: Code Q String
hashEnvDir =
  GRT.qToCode
    . GRT.liftDefString
    . GRT.envSrcFallback "EXAMPLE_HOME"
    $ GRT.gitHashQ

hashEnvValDir :: Code Q String
hashEnvValDir =
  GRT.qToCode
    . GRT.liftDefString
    . GRT.envSrcFallback "EXAMPLE_HOME"
    . GRT.envValFallback "EXAMPLE_HASH"
    $ GRT.gitHashQ
