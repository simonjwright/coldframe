# $Id: setup.sh,v 9ad9c7b1aa24 2004/01/16 07:10:16 simon $
# Sets up environment variables for development of the Stairwell demo
# (on tiamat).

export ADA_PROJECT_PATH=$HOME/cf
export BC=${BC:-bc}
export BUILD=$HOME/cf/.build
export BUILD_BASE=/home/simon/cf/.build
# I think the first writable one is where new entries are added ..
export CASE_EXCEPTIONS=\
$HOME/.emacs_case_exceptions:\
$HOME/cf/emacs_case_exceptions
export DEVEL=YES
export TASH=~/tash832a
export TCL=/usr/lib
export TCL_VERSION=8.3
export TOP=$HOME
