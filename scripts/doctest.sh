set -e

cleanup () {
  rm .ghc.environment.*
}
trap cleanup EXIT

export LANG="C.UTF-8"

cabal build --enable-tests --write-ghc-environment-files=always

RUN_DOCTEST=1 cabal test doctest
