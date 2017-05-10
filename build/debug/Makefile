# (ediff "../debug/Makefile" "../release/Makefile")

all: test-clean tests examples library

# ../../Test/test_one_harness.adb
# ../../Test/test_all_harness.adb
t_one : RUN_ARGS := -v
t_one : test_one_harness.run
t_all : ./wisi-generate.exe
t_all : test_all_harness.run

# from ../common.make
# ../../wisi/test/body_instantiation_conflict.wy
one-clean :
	rm -rf identifier_list_name_conflict*
# %_process.l : RUN_ARGS := -v 2 --first_state_index 1
#one : one-clean
#one : identifier_list_name_conflict_process.l
# one : empty_production_1-parse.diff
one: wisi-generate.exe
	./wisi-generate ../../wisi/test/empty_production_8.wy LALR Elisp

some-clean :
	rm -rf subprograms*
some : some-clean
some : wisi-generate.exe
# some : conflict_name_process_yylex.adb
# some : conflict_name-parse.diff
# some : empty_production_1_process_yylex.adb
# some : empty_production_1-parse.diff
# some : empty_production_2_process_yylex.adb
# some : empty_production_2-parse.diff
# some : empty_production_3_process_yylex.adb
# some : empty_production_3-parse.diff
# some : empty_production_4_process_yylex.adb
# some : empty_production_4-parse.diff
# some : empty_production_5_process_yylex.adb
# some : empty_production_5-parse.diff
# some : empty_production_6_process_yylex.adb
# some : empty_production_6-parse.diff
# some : empty_production_7_process_yylex.adb
# some : empty_production_7-parse.diff
# some : empty_production_8_process_yylex.adb
# some : empty_production_8-parse.diff
# some : identifier_list_name_conflict_process_yylex.adb
# some : identifier_list_name_conflict-parse.diff
some : subprograms_process_yylex.adb
some : subprograms-parse.diff

aflex.exe : force
	make -C /Projects/aflex/build install

EMACS := /apps/emacs-24.2.91/bin/emacs.exe
../../wisi/test/%-wy.el : %.wy
	cd ../../wisi/test; $(EMACS) -batch --no-site-file -l generate-known-good.el -f wisi-generate $(<F)

trace :
	addr2line -e test_one_harness.exe 0x46b9e7 0x77e7fe6f 0x77e7fe41 0x77e805bd 0x77e81c68

include ../common.make

# Local Variables:
# eval: (load-file "prj.el")
# end:
# end of file
