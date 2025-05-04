set -e

export LANG="C.UTF-8"

cabal haddock gitrev-typed --haddock-hyperlink-source --haddock-quickjump

mkdir -p docs/

# shellcheck disable=SC2038
fd -t f . docs | xargs -I % sh -c "rm -r %"

dir=$(fd -t d html dist-newstyle/build | head -n 1)

cp -r $dir/gitrev-typed/* docs/
