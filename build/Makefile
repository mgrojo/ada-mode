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
# t_one : aflex.exe
#t_one : one-clean
t_one : ada_lite_yylex.adb
#t_one : RUN_ARGS := 3
# %_process.l : PARSER_ALG := LALR
t_one : test_one_harness.run

t_all : export Standard_Common_Build := Debug
t_all : ./wisi-generate.exe
t_all : ada_lite_yylex.adb
t_all : character_literal_yylex.adb
t_all : test_all_harness.run

one-clean :
	rm -rf character_literal*
# %_process.l : RUN_ARGS := -v 2 --first_state_index 1
one : export Standard_Common_Build := Debug
#one : one-clean
one :
	./wisi-generate.exe -v 2 --output_language Ada_Emacs --lexer Elisp --interface process ../wisi/test/conflict_name.wy

two : RUN_ARGS ?= --verbosity 2
two : ada_lite_yylex.adb run_ada_lite_parser.exe
	./run_ada_lite_parser.exe ../test/ada_mode-recover_indent_3.adb $(RUN_ARGS)

profile: exec_pro/run_ada_lite_parser.exe
	exec_pro/run_ada_lite_parser.exe ../test/ada_mode-recover_indent_3.adb --repeat_count 10
	gprof --demangle=gnat exec_pro/run_ada_lite_parser.exe > ada_lite.profile

# WORKAROUND: -largs -pg works here; the Linker package in .gpr does not
exec_pro/run_ada_lite_parser.exe : force
	gprbuild -XProfile=On -p -P wisitoken_test.gpr run_ada_lite_parser -largs -pg

some-clean :
	rm -rf $(addsuffix *, $(SOME_CLEAN_FILE))
some : export Standard_Common_Build := Debug
some : some-clean
#some : RUN_ARGS := -v 4
# some : aflex.exe
# some : wisi-generate.exe
# some : SOME_CLEAN_FILE += case_expression
# some : case_expression_yylex.adb
# some : case_expression-parse.diff
# some : SOME_CLEAN_FILE += conflict_name
# some : conflict_name-process.el.diff
# some : conflict_name_yylex.adb
# some : conflict_name-parse.diff
# some : SOME_CLEAN_FILE += empty_production_1
# some : empty_production_1_yylex.adb
# some : empty_production_1-parse.diff
# some : SOME_CLEAN_FILE += empty_production_2
# some : empty_production_2_yylex.adb
# some : empty_production_2-parse.diff
# some : SOME_CLEAN_FILE += empty_production_3
# some : empty_production_3_yylex.adb
# some : empty_production_3-parse.diff
# some : SOME_CLEAN_FILE += empty_production_4
# some : empty_production_4_yylex.adb
# some : empty_production_4-parse.diff
# some : SOME_CLEAN_FILE += empty_production_5
# some : empty_production_5_yylex.adb
# some : empty_production_5-parse.diff
# some : SOME_CLEAN_FILE += empty_production_6
# some : empty_production_6_yylex.adb
# some : empty_production_6-parse.diff
# some : SOME_CLEAN_FILE += empty_production_7
# some : empty_production_7_yylex.adb
# some : empty_production_7-parse.diff
# some : SOME_CLEAN_FILE += empty_production_8
# some : empty_production_8_yylex.adb
# some : empty_production_8-parse.diff
# some : SOME_CLEAN_FILE += identifier_list_name_conflict
# some : identifier_list_name_conflict_yylex.adb
# some : identifier_list_name_conflict-parse.diff
# some : SOME_CLEAN_FILE += subprograms-process
# some : subprograms-process.el.diff

doc : ../Docs/wisi-user_guide.info
doc : ../Docs/wisi-user_guide.html

zip : source-clean bz2file zipfile

tag :
	mtn tag h:org.fasttoken fasttoken-$(ZIP_VERSION)

trace :
	addr2line -e test_one_harness.exe 0x58e767 0x76fdff3f 0x76fdff11 0x76fe068d 0x4016e5 0x4028c1 0x4013db 0x76a97c02 0x76ffad2d 0x76ffacf8

include ../../org.stephe_leake.makerules/texinfo_rules.make
include rules.make

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# End:
# end of file
