# Sets up environment variables for ColdFrame development.
# (very specific to SJW config!).

cf=$PWD

export ADA_PROJECT_PATH=$cf
export BUILD=$cf/.build
export BUILD_BASE=$cf/.build
# I think the first writable one is where new entries are added ..
export CASE_EXCEPTIONS=\
$HOME/.emacs_case_exceptions:\
$cf/scripts/emacs_case_exceptions
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

[ -d $cf/.build ] || (cd $cf; ./scripts/create-build-directories)
[ -d $cf/coldframeout ] || mkdir $cf/coldframeout
