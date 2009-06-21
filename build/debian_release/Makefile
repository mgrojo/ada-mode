all: dirs
	@gnatmake -Popentoken_lib

dirs :: lib

lib:
	@mkdir -p lib

clean ::
	@rm -rf lib/*

distclean:
	@rm -rf obj
	@rm -rf lib

include ../common.make

GNAT_PROJECT := opentoken_test.gpr
GNAT_VERSION := 4.3.2

# Local Variables:
# eval: (ada-parse-prj-file "opentoken.prj")
# eval: (ada-select-prj-file "opentoken.prj")
# end:
# end of file
