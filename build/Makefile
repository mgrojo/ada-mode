# gnu make file for SAL
#
# See ../source/sal.html for complete installation instructions

all : t_all

#../test/test_all_harness.adb
#../test/test_one_harness.adb
t_one : export Standard_Common_Build := Debug
t_one : test_one_harness.run

t_all : test_all_harness.diff

trace :
	addr2line -e test_all_harness.exe 0x4ec038 0x7c9037bd 0x7c903789 0x7c90eaf8 0x4cedff 0x41a94e 0x41e79a 0x407b5c 0x40aa8e 0x4af793 0x401b66 0x401235 0x401286 0x7c816fd5

GNAT_PROJECT := sal_test.gpr

include rules.make

one_mem : exec_pro/exercise_min_heap.exe
	$< $(RUN_ARGS)
	gnatmem $<

#
# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# End:
# end of file
