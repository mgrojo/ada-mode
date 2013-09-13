# build and test the Emacs wisent Ada indentation and navigation engine

# "compile-ada" is a test; it verifies the test sources are legal Ada
all : byte-compile compile-ada-clean compile-ada test-clean test

# this updates everything after any source change, without running the tests.
byte-compile : ../../ada-grammar-wy.el ../../gpr-grammar-wy.el update

# *-wy.el are in monotone, so this is all we need after a monotone update
update :
	cd ../../; emacs -Q -batch -L . -f batch-byte-compile *.el

test : test-ada test-gpr test-elisp

test-wisi :: body_instantiation_conflict.wisi-test
test-wisi :: case_expression.wisi-test
test-wisi :: identifier_list_name_conflict.wisi-test
test-wisi :: range_conflict.wisi-test
test-wisi :: subprograms.wisi-test
test-wisi :: empty_production_6.wisi-test
test-wisi :: empty_production_8.wisi-test

one : gds_test.gpr.diff
two : debug.adb.diff

vpath %.wy ../../ ../../test/wisi
vpath %.el ../../ .

# these are correct paths for Stephe's development machines; need mechanism to override?
ifeq ($(shell uname),Linux)
WISI_OPENTOKEN := /home/Projects/opentoken/org.opentoken.stephe/build/release
else
# windows
WISI_OPENTOKEN := $(CURDIR)/../../../org.opentoken.stephe/build/release
endif

%.wisi-test : %-wy.el
	emacs -Q -batch -L . -L .. -L ../.. -l run-wisi-test.el --eval '(run-test "$*")'

.PRECIOUS : %-wy.el ../../%-grammar-wy.el

# -v 1 dumps grammar
# -v 2 dumps debug info from parser table generation, continues with unused tokens, unknown conflicts
%-wy.el : %.wy $(WISI_OPENTOKEN)/wisi-generate.exe
	cd ./$(<D); $(WISI_OPENTOKEN)/wisi-generate.exe -v 1 $(<F) Elisp > $(*F).output

# ../../../org.opentoken.stephe/wisi/wisi-generate.adb
$(WISI_OPENTOKEN)/wisi-generate.exe : force
	$(MAKE) -C $(WISI_OPENTOKEN) wisi-generate.exe

.PHONY : force

GPRBUILD_TARGET := x86-windows
%.exe : %.adb force
	gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P wisi_opentoken_agg.gpr $(GPRBUILD_ARGS) $*

byte-compile-clean :
	cd ../..; rm -f *.elc

clean ::
	cd ../..; rm -f *-wy.el *.elc
	rm -f *-wy.el *.elc

test-clean ::
	rm -f *.output *.wisi-test
	cd ../../test/wisi/; rm -f *-wy.el *.output

RUNTEST := run-indent-test-wisi.el
INDENT.EL := ada-wisi.el gpr-wisi.el ../../gpr-grammar-wy.el ../../ada-grammar-wy.el
include ../common.make

# delete non-wisi tests
ADA_TEST_FILES := $(filter-out ada_mode-interactive_smie.adb, $(ADA_TEST_FILES)) # explicit smie functions

test-ada : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

export ADA_PROJECT_PATH=../../test/;../../test/subdir

# eval: (ada-parse-prj-file "wisi_opentoken.prj")
# eval: (ada-select-prj-file "wisi_opentoken.prj")

# Local Variables:
# eval: (ada-parse-prj-file "../ada-mode.prj")
# eval: (ada-select-prj-file "../ada-mode.prj")
# End:
# end of file
