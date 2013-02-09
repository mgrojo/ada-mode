# (ediff "../windows_release/Makefile" "../linux_release/Makefile")

# no reason to run 'all' in debug
all:

# ../../Test/test_one_harness.adb
#t_one : RUN_ARGS := -v
t_one : test_one_harness.run
t_all : test_all_harness.run

one : subprograms-wy.el
one : body_instantiation_conflict-wy.el
#one : identifier_list_name_conflict-wy.el
#one : range_conflict-wy.el
#one : ../../wisi/test/subprograms-wy.el
#two : ../../wisi/test/body_instantiation_conflict-wy.el
two : ../../wisi/test/subprograms-wy.el
#two : ../../wisi/test/range_conflict-wy.el
#one : asu_example_5_10_lr-run.run

%-wy.el : RUN_ARGS := -v 1 > wisi.output
%-wy.el : wisi-%-generate.exe
	./wisi-$*-generate.exe $(RUN_ARGS)

EMACS := /apps/emacs-24.2.91/bin/emacs.exe
../../wisi/test/%-wy.el : %.wy
	cd ../../wisi/test; $(EMACS) -batch --no-site-file -l generate-known-good.el -f wisi-generate $(<F)

.PRECIOUS : wisi-%-generate.adb

wisi-%-generate.adb : %.wy wisi-generate.exe
	./wisi-generate.exe $<

wisi-clean :
	rm -f wisi-*-generate.adb *-wy.el

vpath %.wy ../../wisi/test

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
