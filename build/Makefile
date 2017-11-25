# Gnu Makefile
#
# Top level rules here for visibility; lower level rules in rules.make

ZIP_VERSION := 6.0b
#w-$(shell date +%Y-%m-%d)

all : test-clean tests library
#all : doc zip tag

# ../Test/test_one_harness.adb
# ../Test/test_all_harness.adb
t_one : export Standard_Common_Build := Debug
t_one : one-clean
t_one : ada_lite_re2c.c
#t_one : RUN_ARGS := 3
t_one : test_one_harness.run

t_all : export Standard_Common_Build := Debug
t_all : ./wisi-generate.exe
t_all : ada_lite_re2c.c
t_all : character_literal_re2c.c
t_all : test_all_harness.run

one-clean :
	rm -rf ada_lite*
# %_process.l : RUN_ARGS := -v 2 --first_state_index 1
one : export Standard_Common_Build := Debug
#one : one-clean
#one : character_literal_re2c.c
#one : RUN_ARGS := -v 4 ../wisi/test/empty_production_6.input
one : subprograms_process.adb.diff

# rule copied from gprbuild_rules.make; that rule gets overridden by
# %.exe rules in rules.make
exec_pro/run_ada_lite_parser.exe : force
	gprbuild -XStandard_Common_Profile=On -p -P wisitoken_test.gpr run_ada_lite_parser -largs -pg -lgmem

some-clean :
	rm -rf $(addsuffix *, $(SOME_CLEAN_FILE))
some : export Standard_Common_Build := Debug
some : some-clean
#some : RUN_ARGS := -v 4
#some : GPRBUILD_ARGS := -v
# some : wisi-generate.exe
# some : SOME_CLEAN_FILE += case_expression
# some : case_expression_re2c.c
# some : case_expression-parse.diff
# some : SOME_CLEAN_FILE += conflict_name
# some : conflict_name_re2c.c
# some : conflict_name-parse.diff
# some : SOME_CLEAN_FILE += empty_production_1
# some : empty_production_1_re2c.c
# some : empty_production_1-parse.diff
# some : SOME_CLEAN_FILE += empty_production_2
# some : empty_production_2_re2c.c
# some : empty_production_2-parse.diff
# some : SOME_CLEAN_FILE += empty_production_3
# some : empty_production_3_re2c.c
# some : empty_production_3-parse.diff
# some : SOME_CLEAN_FILE += empty_production_4
# some : empty_production_4_re2c.c
# some : empty_production_4-parse.diff
# some : SOME_CLEAN_FILE += empty_production_5
# some : empty_production_5_re2c.c
# some : empty_production_5-parse.diff
# some : SOME_CLEAN_FILE += empty_production_6
# some : empty_production_6_re2c.c
# some : empty_production_6-parse.diff
# some : SOME_CLEAN_FILE += empty_production_7
# some : empty_production_7_re2c.c
# some : empty_production_7-parse.diff
# some : SOME_CLEAN_FILE += empty_production_8
# some : empty_production_8_re2c.c
# some : empty_production_8-parse.diff
# some : SOME_CLEAN_FILE += identifier_list_name_conflict
# some : identifier_list_name_conflict_re2c.c
# some : identifier_list_name_conflict-parse.diff
# some : SOME_CLEAN_FILE += subprograms-process
# some : subprograms-process.el.diff
# some : subprograms_process.adb.diff

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
