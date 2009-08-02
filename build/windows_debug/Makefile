all: dirs tests examples

# ../../Test/test_one_harness.adb
t_one : test_one_harness.run
t_all : test_all_harness.run

#one : asu_example_4_46_rd-run.run
#one : asu_example_5_10-run.run
one : asu_example_5_10_rd-run.run
one : RUN_ARGS := < ../../Examples/asu_example_5_10/Example.txt
#one : asu_example_5_10_rd_no_mixin-run.run

trace :
	addr2line -e asu_example_5_10_rd_no_mixin-run.exe 0x413287 0x415c28 0x40ad2a 0x40ad2a 0x40ca38 0x40ad2a 0x40ad2a 0x40ca38 0x40ad2a 0x40ca38 0x418b39 0x401783 0x401235 0x401286 0x7c817075

include ../common.make

GNAT_PROJECT := opentoken_test.gpr
GNAT_VERSION := 6.2.1

# Local Variables:
# eval: (ada-parse-prj-file "opentoken.prj")
# eval: (ada-select-prj-file "opentoken.prj")
# end:
# end of file
