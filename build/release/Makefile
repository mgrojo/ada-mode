# (ediff "../windows_release/Makefile" "../linux_release/Makefile")
# schroot --chroot=unstable -- make -r all

all: test-clean tests examples library

# from ../common.make
some : wisi-generate.exe
some : test_all_harness.diff
some : empty_production_1_process_yylex.adb
some : empty_production_1-parse.diff
some : empty_production_2_process_yylex.adb
some : empty_production_2-parse.diff
some : empty_production_3_process_yylex.adb
some : empty_production_3-parse.diff
some : empty_production_4_process_yylex.adb
some : empty_production_4-parse.diff
some : empty_production_5_process_yylex.adb
some : empty_production_5-parse.diff
some : empty_production_6_process_yylex.adb
some : empty_production_6-parse.diff
some : empty_production_7_process_yylex.adb
some : empty_production_7-parse.diff
some : empty_production_8_process_yylex.adb
some : empty_production_8-parse.diff
some : identifier_list_name_conflict_process_yylex.adb
some : identifier_list_name_conflict-parse.diff
some : multi_conflict_process_yylex.adb
some : multi_conflict-parse.diff
some : subprograms_process_yylex.adb
some : subprograms-parse.diff

# FIXME: currently broken
# some : asu_example_3_6-run.run
# some : asu_example_4_46-run.run
# some : asu_example_5_10_lr-run.run

# ../../Test/test_one_harness.adb
#t_one : RUN_ARGS := -v
t_one : test_one_harness.run
t_all : test_all_harness.diff

# from ../common.make
#one : test-clean
one : multi_conflict_process_yylex.adb
one : multi_conflict-parse.diff

# two: ~/bin/aflex.exe
two: ada_grammar_process.l ada_grammar_process_yylex.adb run_ada_parser.exe
	bash -c "time ./run_ada_parser 10 ../../time/gnatcoll-xref.adb"

ada_grammar_process.l : ada_grammar.wy wisi-generate.exe
	./wisi-generate.exe --profile ../../time/ada_grammar.wy Ada_Emacs Aflex_Lexer process

~/bin/aflex.exe : force
	make -C /Projects/aflex/build ~/bin/aflex.exe

trace :
	addr2line -e asu_example_5_10_rd-run.exe 0x411a82 0x411aec 0x416f34 0x401783 0x401235 0x401286 0x7c817075

include ../common.make

# (gpr-query-kill-all-sessions)
# Local Variables:
# eval: (load-file "../fasttoken.el")
# eval: (ada-parse-prj-file "fasttoken.prj")
# eval: (ada-select-prj-file "fasttoken.prj")
# end:
# end of file
