# (ediff "../windows_release/Makefile" "../linux_release/Makefile")
# schroot --chroot=unstable -- make -r all

all: test-clean tests examples library

# ../../Test/test_one_harness.adb
#t_one : RUN_ARGS := -v
t_one : test_one_harness.run
t_all : test_all_harness.diff

# from ../common.make
#one : test-clean
#one : multi_conflict.ads
#one : multi_conflict-parse.diff
#one : ada_syntax_dfa.ads

one: ada_grammar_1.ads run_ada_parser_1.exe
	bash -c "time ./run_ada_parser_1 10 ../../wisi/test/gnatcoll-xref.adb"

two: ada_grammar_2.ads ada_grammar_2_dfa.ads run_ada_parser_2.exe
	bash -c "time ./run_ada_parser_2 10 ../../wisi/test/gnatcoll-xref.adb"

ada_grammar_1.ads : ada_grammar.wy wisi-generate.exe
	./wisi-generate.exe --profile --suffix "_1" ../../wisi/test/ada_grammar.wy OpenToken_Lexer Ada_Emacs

ada_grammar_2.ads : ada_grammar.wy wisi-generate.exe
	./wisi-generate.exe --profile --suffix "_2" ../../wisi/test/ada_grammar.wy Aflex_Lexer Ada_Emacs

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
