# configure.sh --
#
# Run this to configure.

set -xe

prefix=/usr/local

../configure \
    --config-cache                              \
    --cache-file=../config.cache                \
    --enable-maintainer-mode                    \
    --prefix="${prefix}"                        \
    --disable-static --enable-shared            \
    CFLAGS='-O3 -march=i686 -mtune=i686'        \
    FFLAGS='-O3 -march=i686 -mtune=i686'        \
    "$@"

### end of file
