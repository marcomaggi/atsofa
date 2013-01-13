# configure.sh --
#
# Run this to configure.

set -xe

prefix=/usr/local

../configure \
    --config-cache                              \
    --cache-file=../config.cache                \
    --enable-maintainer-mode                    \
    --disable-static --enable-shared            \
    --prefix="${prefix}"                        \
    CFLAGS='-O3 -march=i686 -mtune=i686'        \
    FFLAGS='-O3 -march=i686 -mtune=i686'        \
    "$@"

### end of file
