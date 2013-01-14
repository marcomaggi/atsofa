# autogen.sh --
#
# Run this in the top source directory to rebuild the infrastructure.

export PATH=/bin:/usr/local/bin:/usr/bin

set -xe
test -d m4 || mkdir m4
autoreconf --warnings=all --install --verbose "$@"

### end of file
