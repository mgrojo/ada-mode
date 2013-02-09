# (ediff "../windows_release/Makefile" "../linux_release/Makefile")

# no reason to run 'all' in debug
all:

# ../../Test/test_one_harness.adb
#t_one : RUN_ARGS := -v
t_one : test_one_harness.run
t_all : test_all_harness.run

one : subprograms-wy.diff
one : body_instantiation_conflict-wy.diff
one : identifier_list_name_conflict-wy.diff
one : range_conflict-wy.diff

# run semantic-grammar-create-package, wisent-compile-grammar
two : ../../wisi/test/subprograms-wy.el

EMACS := /apps/emacs-24.2.91/bin/emacs.exe
../../wisi/test/%-wy.el : %.wy
	cd ../../wisi/test; $(EMACS) -batch --no-site-file -l generate-known-good.el -f wisi-generate $(<F)

trace :
	addr2line -e wisi-generate.exe 0x40eb70 0x4214f8 0x401aba 0x402641 0x4013f8 0x74ea33a8 0x77689ef0 0x77689ec3

GPRBUILD_TARGET := x86-windows
include ../common.make

# FIXME: move 'require opentoken' into project file
# Local Variables:
# eval: (load-file "../opentoken.el")
# eval: (ada-parse-prj-file "opentoken.prj")
# eval: (ada-select-prj-file "opentoken.prj")
# end:
# end of file
