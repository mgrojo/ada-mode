# (ediff "../windows_release/Makefile" "../linux_release/Makefile")
# schroot --chroot=unstable -- make -r all

all: test-clean tests examples library

# ../../Test/test_one_harness.adb
#t_one : RUN_ARGS := -v
t_one : wisi-generate.exe
t_one : test_one_harness.run
t_all : test_all_harness.diff

# from ../common.make
#one : test-clean
one : multi_conflict_process_yylex.adb
one : multi_conflict-parse.diff

two: wisi-generate.exe
	./wisi-generate.exe ../../wisi/test/body_instantiation_conflict.wy LR1 Elisp

ada_grammar_process.l : ada_grammar.wy wisi-generate.exe
	./wisi-generate.exe --profile ../../time/ada_grammar.wy Ada_Emacs Aflex_Lexer process

~/bin/aflex.exe : force
	make -C /Projects/aflex/build ~/bin/aflex.exe

trace :
	addr2line -e asu_example_5_10_rd-run.exe 0x411a82 0x411aec 0x416f34 0x401783 0x401235 0x401286 0x7c817075

include ../common.make

# (gpr-query-kill-all-sessions)
# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# end:
# end of file
