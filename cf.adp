comp_opt=-gnatqQafy -gnatwaL -O2
bind_opt=-E
gnatmake_opt=-g -j2 -m -k
main=${current}
main_unit=${current}
build_dir=/home/simon/cf/.build/
check_cmd=${cross_prefix}gnatgcc -c ${comp_opt} ${full_current} -gnats
make_cmd=cd ${build_dir}
make_cmd=${cross_prefix}gnatmake -o ${main} ${main_unit} ${gnatmake_opt} -cargs ${comp_opt} -bargs ${bind_opt} -largs ${link_opt}
comp_cmd=cd ${build_dir}
comp_cmd=${cross_prefix}gnatgcc -c ${comp_opt} ${full_current}
run_cmd=cd ${build_dir}
run_cmd=${main}
src_dir=../
src_dir=${HOME}/bc/
obj_dir=./
debug_pre_cmd=cd ${build_dir}
debug_post_cmd=