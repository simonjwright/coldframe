# $Id: setup.sh,v 09da305d3a40 2003/11/15 05:47:37 simon $
# Sets up environment variables for development of the Stairwell demo
# (on tiamat).

export ADA_PROJECT_PATH=$HOME/cf
export BC=${BC:-bc}
export BUILD=$HOME/cf/.build
export BUILD_DIR=/home/simon/cf/.build
# I think the first writable one is where new entries are added ..
export CASE_EXCEPTIONS=\
$HOME/.emacs_case_exceptions:\
$HOME/cf/emacs_case_exceptions
export DEVEL=YES
export TASH=tash832a
export TCL=/usr/lib
export TCL_VERSION=8.3
export TOP=$HOME
