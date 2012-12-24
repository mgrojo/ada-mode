# (ediff "../windows_release/Makefile" "../linux_release/Makefile")

# no reason to run 'all' in debug
all:

# ../../Test/test_one_harness.adb
#t_one : RUN_ARGS := -v
t_one : test_one_harness.run
t_all : test_all_harness.run

one : RUN_ARGS := ../../wisi/test/ada-subset.wy ada-subset-wy.el
one : wisi-compile.run

trace :
	addr2line -e asu_example_5_10_rd_no_mixin-run.exe 0x413287 0x415c28 0x40ad2a 0x40ad2a 0x40ca38 0x40ad2a 0x40ad2a 0x40ca38 0x40ad2a 0x40ca38 0x418b39 0x401783 0x401235 0x401286 0x7c817075

GPRBUILD_TARGET := x86-windows
include ../common.make

# FIXME: move 'require opentoken' into project file
# Local Variables:
# eval: (gnat-7.1)
# eval: (require 'ada-smie-opentoken)
# eval: (ada-parse-prj-file "opentoken.prj")
# eval: (ada-select-prj-file "opentoken.prj")
# end:
# end of file
