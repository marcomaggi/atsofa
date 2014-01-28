# configure.sh --
#
# Run this to configure.

set -xe

prefix=/usr/local
if test -d /lib64
then libdir=${prefix}/lib64
else libdir=${prefix}/lib
fi

../configure \
    --config-cache                              \
    --cache-file=../config.cache                \
    --disable-static --enable-shared            \
    --prefix="${prefix}"                        \
    --libdir="${libdir}"                        \
    CFLAGS='-O3'				\
    FFLAGS='-O3'				\
    "$@"

### end of file
