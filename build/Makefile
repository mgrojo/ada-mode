# Gnu Makefile for WisiToken
#
# Top level rules here for visibility; lower level rules in rules.make

ZIP_VERSION := 1.0a
#w-$(shell date +%Y-%m-%d)

all : test-clean tests
#all : library doc zip tag

#export Standard_Common_Build := Debug

# test_one_harness.adb
# test_all_harness.adb
#
t_one :: one-clean
t_one :: wisi-generate.exe
t_one :: ada_lite_re2c.c
t_one :: ada_lite_lr1_run.exe
t_one :: export Standard_Common_Build := Debug
# t_one :: RUN_ARGS ?= Abandon_Pattern 2 3 > test_one_harness.log
t_one :: test_one_harness.run

t_all :: export Standard_Common_Build := Debug
t_all :: wisi-generate.exe gen test-executables
t_all :: test_all_harness.run

one-clean :
	rm -rf ada_lite*

# one :: one-clean
one :: export Standard_Common_Build := Debug
one :: wisi-generate.exe
one :: ada_lite_re2c.c
one :: ada_lite_lr1_run.exe
	./ada_lite_lr1_run.exe -v 2 ../wisi/test/ada_lite.input

#c:/Projects/org.emacs.ada-mode.stephe-4/ada.wy

# rule copied from gprbuild_rules.make; that rule gets overridden by
# %.exe rules in rules.make
exec_pro/wisi-generate.exe : force
	gprbuild -XStandard_Common_Profile=On -p -P wisitoken.gpr wisi-generate

gmon.out : exec_pro/wisi-generate.exe
	exec_pro/wisi-generate.exe --generate LR1 Ada re2c ../wisi/test/ada_lite.wy

generate_ada_lite_lr1.gprof : gmon.out
	gprof exec_pro/wisi-generate.exe > generate_ada_lite_lr1.gprof

two : wisi-generate.exe
	./wisi-generate.exe -v 2 --generate LALR Ada re2c ../wisi/test/ada_lite.wy > ada_lite_lalr_generate.log


.PHONY : t_one_1 t_one_2

doc : ../Docs/wisi-user_guide.info
doc : ../Docs/wisi-user_guide.html

zip : source-clean bz2file zipfile

tag :
	mtn tag h:org.fasttoken fasttoken-$(ZIP_VERSION)

trace :
	addr2line -e test_all_harness.exe 0x7912cf 0x754052 0x73b0bb 0x54f23c 0x609e0d 0x68511f 0x403c80 0x78f197 0x4013b3 0x4014e6 0x7ffc9c3913d0 0x7ffc9c5a54f2

include ../../org.stephe_leake.makerules/texinfo_rules.make
include rules.make

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# End:
# end of file
