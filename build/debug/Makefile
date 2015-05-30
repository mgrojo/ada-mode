# (ediff "../debug/Makefile" "../release/Makefile")

all: test-clean tests examples library

# ../../Test/test_one_harness.adb
# ../../Test/test_all_harness.adb
#t_one : RUN_ARGS := -v
t_one : test_one_harness.run
t_all : test_all_harness.run

# from ../common.make
# ../../wisi/test/body_instantiation_conflict.wy
#one : body_instantiation_conflict-wy.el
one:
	./wisi-generate ../../wisi/test/body_instantiation_conflict.wy Elisp

aflex.exe : force
	make -C /Projects/aflex/build install

EMACS := /apps/emacs-24.2.91/bin/emacs.exe
../../wisi/test/%-wy.el : %.wy
	cd ../../wisi/test; $(EMACS) -batch --no-site-file -l generate-known-good.el -f wisi-generate $(<F)

trace :
	addr2line -e wisi-generate.exe 0x40eb70 0x4214f8 0x401aba 0x402641 0x4013f8 0x74ea33a8 0x77689ef0 0x77689ec3

include ../common.make

# Local Variables:
# eval: (load-file "../fasttoken.el")
# eval: (ada-parse-prj-file "fasttoken.prj")
# eval: (ada-select-prj-file "fasttoken.prj")
# end:
# end of file
