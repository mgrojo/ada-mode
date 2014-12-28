all: clean gprof.out

include ../common.make

%.ads : %.wy wisi-generate.exe
	./wisi-generate.exe --profile $< Ada_Emacs
	dos2unix $@ $*.ads

run_ada_parser.exe : ada_grammar.ads

gmon.out : run_ada_parser.exe
	./run_ada_parser.exe ../../wisi/test/gnatcoll-xref.adb

gprof.out : gmon.out
	gprof run_ada_parser.exe > gprof.out

clean :: gprof_clean
	rm -f obj_tree/*

gprof_clean :
	rm *.out

# Local Variables:
# eval: (load-file "../opentoken.el")
# eval: (ada-parse-prj-file "opentoken.prj")
# eval: (ada-select-prj-file "opentoken.prj")
# end:
# end of file
