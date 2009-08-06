all: dirs tests

# ../../Test/test_one_harness.adb
one : test_one_harness.run

trace :
	addr2line -e asu_example_5_10_rd-run.exe 0x411a82 0x411aec 0x416f34 0x401783 0x401235 0x401286 0x7c817075

include ../common.make

GNAT_PROJECT := opentoken_test.gpr
GNAT_VERSION := 6.2.1

export ADA_PROJECT_PATH := ../../../sal/build/x86_gnu_lynx_release:../../../makerules

# Local Variables:
# eval: (ada-parse-prj-file "opentoken.prj")
# eval: (ada-select-prj-file "opentoken.prj")
# end:
# end of file
