# $Id: setup.sh,v cbc9b02db43f 2003/11/08 16:36:25 simon $
# Sets up environment variables for development of the Stairwell demo
# (on tiamat).

export ADA_PROJECT_PATH=$HOME/cf:$HOME/high_resolution_timing:$HOME/statistics
export BC=${BC:-bc}
export BUILD=$HOME/cf/.build
export BUILD_DIR=/home/simon/cf/.build
export DEVEL=YES
export TASH=tash832a
export TCL=/usr/lib
export TCL_VERSION=8.3
export TOP=$HOME
