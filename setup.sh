# $Id: setup.sh,v 1d847b9ff4bb 2003/11/14 06:13:35 simon $
# Sets up environment variables for development of the Stairwell demo
# (on tiamat).

export ADA_PROJECT_PATH=$HOME/cf:$HOME/high_resolution_timing:$HOME/statistics
export BC=${BC:-bc}
export BUILD=$HOME/cf/.build
export BUILD_DIR=/home/simon/cf/.build
export CASE_EXCEPTIONS=$HOME/cf/emacs_case_exceptions:\
$HOME/.emacs_case_exceptions
export DEVEL=YES
export TASH=tash832a
export TCL=/usr/lib
export TCL_VERSION=8.3
export TOP=$HOME
