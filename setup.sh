# $Id: setup.sh,v 4c09432124cf 2003/02/15 16:17:21 simon $
# Sets up environment variables for development of the Stairwell demo
# (on tiamat).

export ADA_PROJECT_PATH=$HOME/cf:$HOME/timing
export BC=${BC:-bc-devel}
export BUILD=$HOME/cf/.build
export BUILD_DIR=/home/simon/cf/.build
export DEVEL=YES
export TASH=tash832a
export TCL=/usr/lib
export TCL_VERSION=8.3
export TOP=$HOME
