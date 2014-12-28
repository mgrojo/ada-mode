all: clean gprof.out

include ../common.make

%.ads : %.wy wisi-generate.exe
	./wisi-generate.exe $< Test
	dos2unix $@ $*.ads

%.exe : %.adb force
	gprbuild -p opentoken.gpr $<

run_ada_parser.exe : ada_grammar.ads

gmon.out : run_ada_parser.exe
	./run_ada_parser.exe ../../wisi/test/gnatcoll_xref.adb

gprof.out : gmon.out
	gprof run_ada_parser.exe > gprof.out

clean ::
	rm -f obj_tree/*

# Local Variables:
# eval: (load-file "../opentoken.el")
# eval: (ada-parse-prj-file "opentoken.prj")
# eval: (ada-select-prj-file "opentoken.prj")
# end:
# end of file
