# $Id: setup.sh,v 582fe9b01914 2005/05/16 12:50:48 simonjwright $
# Sets up environment variables for development of the Stairwell demo
# (very local!).

cf=$HOME/cf

export ADA_PROJECT_PATH=$cf
export AUNIT=${AUNIT:-AUnit-1.03p}
export BC=bc
export BUILD=$cf/.build
export BUILD_BASE=$cf/.build
# I think the first writable one is where new entries are added ..
export CASE_EXCEPTIONS=\
$HOME/.emacs_case_exceptions:\
$cf/emacs_case_exceptions
case `uname` in
    Linux)
	# hey, I know this is antique, but it works for me ..
	export TASH=~/tash832a
	export TCL=/usr/lib
	export TCL_VERSION=8.3
	;;
    Darwin)
	# The 8.4.9.0 BI distribution; NB you need some fancy symlinks
	# here, see note on the Tash mailing list.
	export TASH=~/tash841a
	export TCL=/usr/local
	export TCL_VERSION=8.4
	;;
    *)
	echo "I don't know how to set up for Tash in `uname`"
	;;
esac
export TOP=$HOME

[ -d $cf/.build ] || (cd $cf; ./create-build-directories)
[ -d $cf/lib ] || (cd $cf; ln -s . lib)
[ -d $cf/project ] || (cd $cf; mkdir project)
