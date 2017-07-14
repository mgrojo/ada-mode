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

# from ../common.make
# ../../wisi/test/body_instantiation_conflict.wy
one-clean :
	rm -rf character_literal*
# %_process.l : RUN_ARGS := -v 2 --first_state_index 1
#one : one-clean
one :
	./wisi-generate.exe -v 2 --output_language Ada_Emacs --lexer Elisp --interface process ../wisi/test/conflict_name.wy

some-clean :
	rm -rf $(addsuffix *, $(SOME_CLEAN_FILE))
some : export Standard_Common_Build := Debug
some : some-clean
#some : RUN_ARGS := -v 4
# some : aflex.exe
some : wisi-generate.exe
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

doc : ../Docs/wisi-user_guide.info
doc : ../Docs/wisi-user_guide.html

zip : source-clean bz2file zipfile

tag :
	mtn tag h:org.fasttoken fasttoken-$(ZIP_VERSION)

trace :
	addr2line -e test_all_harness.exe 0x5e5247 0x76f5fe6f 0x76f5fe41 0x76f605bd 0x46e15f 0x403271 0x4034f2 0x4013db 0x759e7c02 0x76f7ab8d 0x76f7ab58

include ../../org.stephe_leake.makerules/texinfo_rules.make
include rules.make

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# end:
# end of file
