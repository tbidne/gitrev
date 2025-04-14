module TH
  ( hashLegacy,
    hashTyped,
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
hashTyped :: Code Q String
hashTyped =
  GRT.qToCode
    . GRT.liftDefString
    . GRT.envFallback "EXAMPLE_HASH"
    $ GRT.gitHashQ
