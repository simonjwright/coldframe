comp_opt=-gnatq -gnatQ -gnatX -gnatwu -gnatf -gnaty -g -O2
gnatmake_opt=-g -m -j2
main=library-test
main_unit=library-test
build_dir=/home/simon/ooa/rose/.build/
check_cmd=${cross_prefix}gnatgcc -c ${comp_opt} -gnats ${full_current}
make_cmd=cd ${build_dir}
make_cmd=${cross_prefix}gnatmake -o ${main} ${main_unit} ${gnatmake_opt} -cargs ${comp_opt} -bargs ${bind_opt} -largs ${link_opt}
comp_cmd=cd ${build_dir}
comp_cmd=${cross_prefix}gnatgcc -c ${comp_opt} ${full_current}
run_cmd=cd ${build_dir}
run_cmd=${main}
src_dir=../
src_dir=../Library.impl/
src_dir=../Library.gen/
src_dir=/home/simon/bc/
obj_dir=./
debug_pre_cmd=cd ${build_dir}
debug_post_cmd=
