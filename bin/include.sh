CWD=$(realpath "$(dirname "${BASH_SOURCE[0]}")")
(cd $CWD/../; make --silent -f $CWD/envrc.mk .envrc) || exit 1;
source $CWD/../.envrc
FIELDS="-E development --only hs_source_dirs --only ghc_options --only default_extensions --only  default_language --only  cpp_options --only  c_sources --only  cc_options --only  extra_lib_dirs --only  extra_libraries --only  ld_options --only  include_dirs --only  includes --only package_db --only root_dir --only autogen_hs_source_dirs --only autogen_include_dirs --only autogen_includes --only hdevtools_socket"
CABAL_CARGS=$(cd $CWD/../; cabal-cargs $FIELDS || exit 1)
OUTPUT_DIR=$CWD/../dist/build/htmlt/htmlt-tmp/
