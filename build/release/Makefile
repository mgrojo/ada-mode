# gnu make file for release version of SAL

all : test_all_harness.diff

#../../test/test_all_harness.adb
#../../test/test_one_harness.adb
t_one : test_one_harness.run

t_all : test_all_harness.diff

trace :
	addr2line -e test_all_harness.exe 0x4ec038 0x7c9037bd 0x7c903789 0x7c90eaf8 0x4cedff 0x41a94e 0x41e79a 0x407b5c 0x40aa8e 0x4af793 0x401b66 0x401235 0x401286 0x7c816fd5

GNAT_PROJECT	:= sal_test_agg.gpr
GPRBUILD_TARGET := x86-windows

include ../common.make

#
#Local Variables:
#eval: (ada-parse-prj-file "sal.prj")
#eval: (ada-select-prj-file "sal.prj")
#End:
# end of file
