DIR="$(dirname "${BASH_SOURCE[0]}")"
CABAL_ARGS=$(cd $DIR/../; cabal-cargs || exit 1)
OUT_ARGS="-outputdir=$DIR/../out"
