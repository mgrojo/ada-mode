all: dirs

lib:
	@mkdir -p lib

clean ::
	@rm -rf lib/*

distclean:
	@rm -rf obj
	@rm -rf lib

include ../common.make

GNAT_PROJECT := opentoken_test.gpr
GNAT_VERSION := 6.2.1

# Local Variables:
# eval: (ada-parse-prj-file "opentoken.prj")
# eval: (ada-select-prj-file "opentoken.prj")
# end:
# end of file
