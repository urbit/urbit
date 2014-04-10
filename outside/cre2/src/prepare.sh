# prepare.sh --
#
# Run this to rebuild the infrastructure and configure.

set -xe

(cd .. && sh autogen.sh)
sh ../configure.sh

### end of file
