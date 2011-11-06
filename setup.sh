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

[ -d $cf/.build ] || (cd $cf; ./scripts/create-build-directories)
[ -d $cf/coldframeout ] || mkdir $cf/coldframeout
