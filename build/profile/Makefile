all: clean gprof.out

include ../common.make

ada_grammar.ads : RUN_ARGS := --profile

run_ada_parser.exe : ada_grammar.ads

gmon.out : run_ada_parser.exe
	./run_ada_parser.exe 10 ../../wisi/test/gnatcoll-xref.adb

gprof.out : gmon.out
	gprof run_ada_parser.exe > gprof.out

clean :: gprof_clean
	rm -f obj_tree/*

gprof_clean :
	rm -f *.out

# Local Variables:
# eval: (load-file "../opentoken.el")
# eval: (ada-parse-prj-file "opentoken.prj")
# eval: (ada-select-prj-file "opentoken.prj")
# end:
# end of file
