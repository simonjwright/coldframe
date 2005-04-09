# $Id: setup.sh,v 284cc4ed467d 2005/04/09 07:48:55 simon $
# Sets up environment variables for development of the Stairwell demo
# (very local!).

cf=$HOME/local/cf

export ADA_PROJECT_PATH=$cf
export BC=${BC:-bc}
export BUILD=$cf/.build
export BUILD_BASE=$cf/.build
# I think the first writable one is where new entries are added ..
export CASE_EXCEPTIONS=\
$HOME/.emacs_case_exceptions:\
$cf/emacs_case_exceptions
export DEVEL=YES
export TASH=~/tash832a
export TCL=/usr/lib
export TCL_VERSION=8.3
export TOP=$HOME
