# $Id: setup.sh,v c59ee9194671 2008/04/30 05:00:46 simonjwright $
# Sets up environment variables for ColdFrame development.
# (very specific to SJW config!).

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
export COLDFRAME=cf
case `uname` in
    Darwin | Linux)
	# Use a tcladashell.sf.net install
	export ADA_PROJECT_PATH=$ADA_PROJECT_PATH:~/tcladashell
	;;
    *)
	echo "I don't know how to set up for Tash in `uname`"
	;;
esac
export TOP=$HOME

[ -d $cf/.build ] || (cd $cf; ./create-build-directories)
[ -d $cf/lib ] || (cd $cf; ln -s . lib)
[ -d $cf/project ] || (cd $cf; mkdir project)
