all: dirs tests examples library

# ../../Test/test_one_harness.adb
one : test_one_harness.run

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

export ADA_PROJECT_PATH := ..;../../../GDS/main/sal/build/x86_gnu_windows_release;../../../GDS/main/makerules

# Local Variables:
# eval: (ada-parse-prj-file "opentoken.prj")
# eval: (ada-select-prj-file "opentoken.prj")
# end:
# end of file
