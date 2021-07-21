# Gnu Makefile for WisiToken
#
# Top level rules here for visibility; lower level rules in rules.make

ZIP_VERSION := 2.1
#w-$(shell date +%Y-%m-%d)

all : test-clean tests recover_stats.exe wisitoken-followed_by.exe doc

#export Standard_Common_Build := Debug
#export GENERATE := BNF_EBNF_Tree_Sitter

# test_one_harness.adb
# test_all_harness.adb
#
#t_one :: one-clean
t_one :: one-clean
t_one :: ada_lite_ebnf_re2c.c
t_one :: ada_lite_ebnf/parser.c
t_one :: test_one_harness.run

t_all :: wisitoken-bnf-generate.exe gen test-executables
t_all :: test_all_harness.run

t_bnf :: RUN_ARGS=1 "" 1 1
t_bnf :: wisitoken-bnf-generate.exe
t_bnf :: gen test-executables
t_bnf :: test_bnf_harness.run

t_mck :: wisitoken-bnf-generate.exe
#t_mck :: GENERATE_ARGS := -v 0 0 4
t_mck :: ada_lite_re2c.c
t_mck :: wisitoken-parse-lr-mckenzie_recover-ada_lite.adb
t_mck :: wisitoken-parse-lr-mckenzie_recover-ada_lite.ads
t_mck :: test_mckenzie_harness.run

one-clean :
	rm -rf ada_lite*

#one :: one-clean
one :: VERBOSITY := debug=1 lexer=1 action=3
one :: GRAMMAR := ada_ebnf
one :: wisitoken-bnf-generate.exe
	./wisitoken-bnf-generate.exe --test_main --output_bnf --generate None --verbosity "$(VERBOSITY)" ../test/bnf/$(GRAMMAR).wy > debug.log


two-clean :
	rm -rf debug*

#two :: VERBOSITY := 1 0 0
#two :: two-clean
two :: wisitoken-bnf-generate.exe
	./wisitoken-bnf-generate.exe -v 0 0 0 --generate LALR Ada re2c ../test/bnf/ada_lite.wy

three :: GENERATE_ARGS := -v 1 0 0
three :: one-clean
three :: ada_lite_ebnf.re2c
three :: ada_lite_ebnf/parser.c

four :: GRAMMAR := dragon_4_43
#four :: gen
four :: test-executables
#	./$(GRAMMAR)_tree_sitter_run.exe -v 1 ../test/bnf/$(GRAMMAR).input
	./$(GRAMMAR)_tree_sitter_run.exe -v 1 foo

# rule copied from gprbuild_rules.make; that rule gets overridden by
# %.exe rules in rules.make
exec_pro/wisitoken-bnf-generate.exe : force
	gprbuild -XStandard_Common_Profile=On -p -P wisitoken.gpr wisitoken-bnf-generate

gmon.out : exec_pro/wisitoken-bnf-generate.exe
	exec_pro/wisitoken-bnf-generate.exe --generate LR1 Ada re2c ../wisi/test/ada_lite.wy

generate_ada_lite_lr1.gprof : gmon.out
	gprof exec_pro/wisitoken-bnf-generate.exe > generate_ada_lite_lr1.gprof

doc :: error_correction_algorithm.dvi
doc :: error_correction_algorithm.bbl
doc :: error_correction_algorithm.ps
doc :: error_correction_algorithm.pdf
doc :: ../Docs/wisitoken-user_guide.info
doc :: ../Docs/wisitoken-user_guide.html
doc :: create-dir

create-dir : force
	cd ../Docs/; for file in *.info; do install-info $$file dir; done

trace :
	addr2line -e test_all_harness.exe 0x6eb427 0x7728ff3f 0x7728ff11 0x7729068d

include ../../org.stephe_leake.makerules/texinfo_rules.make
include ../../org.stephe_leake.makerules/latex_rules.make
include rules.make

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# End:
# end of file
