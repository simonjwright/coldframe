comp_opt=-gnatq -gnatQ -gnatX -gnatwu -g -O
gnatmake_opt=-g -m
main=${current}
main_unit=${current}
build_dir=/home/simon/ooa/rose/.build
check_cmd=${cross_prefix}gnatgcc -c ${comp_opt} -gnats
make_cmd=cd ${build_dir}
make_cmd=${cross_prefix}gnatmake -o ${main} ${main_unit} ${gnatmake_opt} -cargs ${comp_opt} -bargs ${bind_opt} -largs ${link_opt}
comp_cmd=cd ${build_dir}
comp_cmd=${cross_prefix}gnatgcc -c ${comp_opt}
run_cmd=cd ${build_dir}
run_cmd=${main}
casing=~/.emacs_case_exceptions/
src_dir=../
src_dir=../Library.impl/
src_dir=../Library.gen/
src_dir=~/bc/
obj_dir=./


