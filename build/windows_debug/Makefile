# (ediff "../windows_release/Makefile" "../linux_release/Makefile")

# no reason to run 'all' in debug
all:

# ../../Test/test_one_harness.adb
#t_one : RUN_ARGS := -v
t_one : test_one_harness.run
t_all : test_all_harness.run

one : ada-subset-wy.el

%-wy.el : wisi-%-generate.exe
	./wisi-$*-generate.exe

wisi-%-generate.adb : %.wy wisi-generate.exe
	./wisi-generate.exe -v $<

vpath %.wy ../../wisi/test

trace :
	addr2line -e wisi-generate.exe 0x4139d8 0x4207aa 0x401aba 0x402641 0x4013f8 0x74ea33a8 0x77689ef0 0x77689ec3

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
