# $Id: setup.sh,v 8590d689a774 2003/09/10 04:54:01 simon $
# Sets up environment variables for development of the Stairwell demo
# (on tiamat).

export ADA_PROJECT_PATH=$HOME/cf:$HOME/timing
export BC=${BC:-bc}
export BUILD=$HOME/cf/.build
export BUILD_DIR=/home/simon/cf/.build
export DEVEL=YES
export TASH=tash832a
export TCL=/usr/lib
export TCL_VERSION=8.3
export TOP=$HOME
