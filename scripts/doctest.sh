set -e

cleanup () {
  rm .ghc.environment.*
}
trap cleanup EXIT

export LANG="C.UTF-8"

cabal build gitrev --write-ghc-environment-files=always

# Run rather than test as 'cabal test' can have problems with doctest,
# apparently.
RUN_DOCTEST=1 cabal run gitrev-doctest:test:doctest
