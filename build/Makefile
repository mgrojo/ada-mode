# Gnu Makefile for WisiToken
#
# Top level rules here for visibility; lower level rules in rules.make

ZIP_VERSION := 2.1
#w-$(shell date +%Y-%m-%d)

all : test-clean tests recover_stats.exe wisitoken-followed_by.exe doc

# export Standard_Common_Build := Debug

# test_one_harness.adb
# test_all_harness.adb
#
#t_one :: one-clean
t_one :: test_one_harness.run

t_all :: wisitoken-bnf-generate.exe gen test-executables
t_all :: test_all_harness.run

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
	touch ../wisitoken-bnf-generate_utils.adb

#one :: one-clean
one :: wisitoken-bnf-generate.exe
	./wisitoken-bnf-generate.exe --time ../../org.emacs.ada-mode/ada.wy

two :: GRAMMAR := ada_lite
two :: wisitoken-bnf-generate.exe
	./wisitoken-bnf-generate.exe --test_main --output_bnf --time --generate LR1 Ada re2c -v 0 2 0 ../test/bnf/$(GRAMMAR).wy > generate-$(GRAMMAR).log

three :: VERBOSITY := -v 1 -m 1
three :: ada_lite_ebnf_lalr_run.exe
	./ada_lite_ebnf_lalr_run.exe -t 1 $(VERBOSITY) ../test/bnf/ada_lite.input > debug.log

# rule copied from gprbuild_rules.make; that rule gets overridden by
# %.exe rules in rules.make
exec_pro/wisitoken-bnf-generate.exe : force
	gprbuild -XStandard_Common_Profile=On -p -P wisitoken.gpr wisitoken-bnf-generate

gmon.out : exec_pro/wisitoken-bnf-generate.exe
	exec_pro/wisitoken-bnf-generate.exe --generate LR1 Ada re2c ../wisi/test/ada_lite.wy

generate_ada_lite_lr1.gprof : gmon.out
	gprof exec_pro/wisitoken-bnf-generate.exe > generate_ada_lite_lr1.gprof

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
