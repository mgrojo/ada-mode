# Gnu Makefile for WisiToken
#
# Top level rules here for visibility; lower level rules in rules.make

ZIP_VERSION := 1.0.1
#w-$(shell date +%Y-%m-%d)

all : test-clean tests doc

export Standard_Common_Build := Debug

# test_one_harness.adb
# test_all_harness.adb
#
t_one :: one-clean
t_one :: wisitoken-bnf-generate.exe
t_one :: ada_lite_re2c.c
t_one :: ada_lite_lr1_run.exe
# t_one :: RUN_ARGS ?= Abandon_Pattern 2 3 > test_one_harness.log
t_one :: test_one_harness.run

t_all :: wisitoken-bnf-generate.exe gen test-executables
t_all :: test_all_harness.run

one-clean :
	rm -rf ada_lite*

# one :: one-clean
one :: ../wisitoken_grammar_re2c.c
one :: wisitoken-bnf-generate.exe
	./wisitoken-bnf-generate.exe -v 2 --output_bnf ada_lite_bnf.wy --generate LALR Ada re2c ../Test/bnf/ada_lite_ebnf.wy > debug.log

#c:/Projects/org.emacs.ada-mode.stephe-4/ada.wy

# rule copied from gprbuild_rules.make; that rule gets overridden by
# %.exe rules in rules.make
exec_pro/wisitoken-bnf-generate.exe : force
	gprbuild -XStandard_Common_Profile=On -p -P wisitoken.gpr wisitoken-bnf-generate

gmon.out : exec_pro/wisitoken-bnf-generate.exe
	exec_pro/wisitoken-bnf-generate.exe --generate LR1 Ada re2c ../wisi/test/ada_lite.wy

generate_ada_lite_lr1.gprof : gmon.out
	gprof exec_pro/wisitoken-bnf-generate.exe > generate_ada_lite_lr1.gprof

two : wisitoken-bnf-generate.exe
	./wisitoken-bnf-generate.exe -v 2 --output_bnf debug_bnf.wy --generate LALR Ada re2c ../test/bnf/debug.wy > debug.log


.PHONY : t_one_1 t_one_2

doc :: ../Docs/wisitoken-user_guide.info
doc :: ../Docs/wisitoken-user_guide.html
doc :: create-dir

create-dir : force
	cd ../Docs/; for file in *.info; do install-info $$file dir; done

trace :
	addr2line -e test_all_harness.exe 0x6eb427 0x7728ff3f 0x7728ff11 0x7729068d

include ../../org.stephe_leake.makerules/texinfo_rules.make
include rules.make

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# End:
# end of file
