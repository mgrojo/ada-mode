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
#t_one :: one-clean
#t_one :: wisi-generate.exe
#t_one :: warth_left_recurse_expr_1_re2c.c
#t_one :: export Standard_Common_Build := Debug
# t_one :: RUN_ARGS ?= Abandon_Pattern 2 3 > test_one_harness.log
t_one :: test_one_harness.run

#t_all :: export Standard_Common_Build := Debug
t_all :: wisi-generate.exe
t_all :: ada_lite_re2c.c
t_all :: character_literal_re2c.c
t_all :: skip_to_grammar_re2c.c
t_all :: dragon_4_43_packrat_re2c.c
t_all :: warth_left_recurse_expr_1_re2c.c
#t_all :: RUN_ARGS ?= 1 test_mckenzie_recover.adb Error_1 1 3 5 > test.log
t_all :: test_all_harness.run

one-clean :
	rm -rf test-clean

# one :: one-clean
# one :: export Standard_Common_Build := Debug
# one :: identifier_list_name_conflict_packrat_re2c.c
# one :: identifier_list_name_conflict_re2c.c
# one :: RUN_ARGS := -v 2 ../wisi/test/identifier_list_name_conflict.input
one :: wisi-generate.exe
	./wisi-generate.exe --time --generate LALR Ada re2c ../wisi/test/ada_lite.wy
	./wisi-generate.exe --time --generate LR1 Ada re2c ../wisi/test/ada_lite.wy
	./wisi-generate.exe --time --generate LALR Ada re2c ../../org.emacs.ada-mode.stephe-4/ada.wy

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
	addr2line -e test_all_harness.exe 0x6eb427 0x7728ff3f 0x7728ff11 0x7729068d

include ../../org.stephe_leake.makerules/texinfo_rules.make
include rules.make

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# End:
# end of file
