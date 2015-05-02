# (ediff "../windows_release/Makefile" "../linux_release/Makefile")
# schroot --chroot=unstable -- make -r all

all: test-clean tests examples library

# from ../common.make
# some : association_token_test-run.diff
# some : enumerated_token_list_test.run
# some : name_token_test-run.diff
# some : production_test-run.run
# some : recognizer_based_integer_test.run
# some : recognizer_integer_test.run
# some : string_test-run.run
# some : test_all_harness.diff
# some : test_html_lexer_safe.diff
# some : test_html_lexer_safe-syntax_error.diff
# some : token_analyzer_ctd-run.run
# some : empty_production_1_yylex.ads
# some : empty_production_1-parse.diff
# some : empty_production_2.ads
# some : empty_production_2-parse.diff
# some : empty_production_3_yylex.ads
# some : empty_production_3-parse.diff
# some : empty_production_4.ads
# some : empty_production_4-parse.diff
# some : empty_production_5_yylex.ads
# some : empty_production_5-parse.diff
# some : empty_production_6.ads
# some : empty_production_6-parse.diff
# some : empty_production_7_yylex.ads
# some : empty_production_7-parse.diff
# some : empty_production_8.ads
# some : empty_production_8-parse.diff
# some : identifier_list_name_conflict_yylex.ads
# some : identifier_list_name_conflict-parse.diff
# some : multi_conflict.ads
# some : multi_conflict-parse.diff
# some : subprograms_yylex.ads
# some : subprograms-parse.diff
# some : asu_example_3_6-run.run
# some : asu_example_4_46-run.run
some : asu_example_5_10_lr-run.run
some : ada_count.run
some : test_ada_lexer.run
some : test_html_lexer_unsafe.run
some : test_java_lexer.run
some : test_m3_lexer.run

# ../../Test/test_one_harness.adb
#t_one : RUN_ARGS := -v
t_one : test_one_harness.run
t_all : test_all_harness.diff

# from ../common.make
#one : test-clean
#one : multi_conflict.ads
#one : multi_conflict-parse.diff

one: ada_grammar_1.ads run_ada_parser_1.exe
	bash -c "time ./run_ada_parser_1 10 ../../wisi/test/gnatcoll-xref.adb"

two: aflex.exe ada_grammar_2.l ada_grammar_2_yylex.ads run_ada_parser_2.exe
	bash -c "time ./run_ada_parser_2 10 ../../wisi/test/gnatcoll-xref.adb"

ada_grammar_1.ads : ada_grammar.wy wisi-generate.exe
	./wisi-generate.exe --profile --suffix "_1" ../../wisi/test/ada_grammar.wy OpenToken_Lexer Ada_Emacs

ada_grammar_2.l : ada_grammar.wy wisi-generate.exe
	./wisi-generate.exe --profile --suffix "_2" ../../wisi/test/ada_grammar.wy Aflex_Lexer Ada_Emacs

aflex.exe : force
	make -C /Projects/aflex/build install

trace :
	addr2line -e asu_example_5_10_rd-run.exe 0x411a82 0x411aec 0x416f34 0x401783 0x401235 0x401286 0x7c817075

# adactl won't use -gnat12 to create the tree files, so we create the
# tree files via opentoken_tree.gpr. '-- -FT' says don't let adactl
# try to create the tree.
check : tree
	../../../adactl-1.11r4/src/adactl.exe -f check_package_vars.aru ../../*.ad? ../../Language_Lexers/*.ad? -- -FT -Tobj_tree

tree : force
	gnatmake -P opentoken_tree.gpr

clean ::
	rm -f obj_tree/*

include ../common.make

# Local Variables:
# eval: (load-file "../opentoken.el")
# eval: (ada-parse-prj-file "opentoken.prj")
# eval: (ada-select-prj-file "opentoken.prj")
# end:
# end of file
