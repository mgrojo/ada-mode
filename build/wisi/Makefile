# build and test the Emacs wisent Ada indentation and navigation engine

# test with mtn controlled source:  (setenv "ADA_MODE_DIR" "-L ../.. -l autoloads.el")
# test with installed elpa package: (setenv "ADA_MODE_DIR" "-f package-initialize")

# "compile-ada" is a test; it verifies the test sources are legal Ada
all : update compile-ada-clean compile-ada autoloads test-clean test

# this updates everything after any source change, without running the tests.
update : ../../ada-grammar-wy.el ../../gpr-grammar-wy.el  byte-compile

# *-wy.el are in monotone, so this is all we need after a monotone update
# doing byte-compile-clean first avoids errors caused by loading new source on old .elc
# Emacs 24.2 requires cl-lib from Gnu ELPA, so we need package-initialize here
byte-compile : byte-compile-clean
	cd ../../; $(EMACS_EXE) -Q -batch -L . --eval '(progn (package-initialize)(batch-byte-compile))' *.el

test : test-wisi test-ada-gnatxref.stamp test-ada-gnatinspect.stamp test-gpr test-elisp gpr-skel.gpr.diff

# test-elisp debug:
# (progn (add-to-list 'load-path (expand-file-name "../../test")) (load "ada-mode-test.el"))

test-wisi :: body_instantiation_conflict.wisi-test
test-wisi :: case_expression.wisi-test
test-wisi :: identifier_list_name_conflict.wisi-test
test-wisi :: range_conflict.wisi-test
test-wisi :: subprograms.wisi-test
test-wisi :: empty_production_6.wisi-test
test-wisi :: empty_production_8.wisi-test

one : RUNTEST := run-indent-test-wisi.el
one : ada_mode-separate_protected_body.adb.diff

vpath %.wy ../../ ../../test/wisi
vpath %.el ../../ .

# These are correct paths for Stephe's development machines;
# WISI_OPENTOKEN can be overridden on the 'make' command line or by an
# external environment variable.
ifeq ($(shell uname),Linux)
WISI_OPENTOKEN ?= /home/Projects/opentoken/org.opentoken/build/release
export ADA_PROJECT_PATH=../../test/:../../test/subdir

else
# windows
WISI_OPENTOKEN ?= $(CURDIR)/../../../org.opentoken/build/release
export ADA_PROJECT_PATH=../../test/;../../test/subdir

endif

%.wisi-test : %-wy.el
	$(EMACS_EXE) -Q -batch -L .. $(ADA_MODE_DIR) -l run-wisi-test.el --eval '(run-test "$*")'

.PRECIOUS : %-wy.el ../../%-grammar-wy.el

# -v 1 dumps grammar
# -v 2 dumps debug info from parser table generation, continues with unused tokens, unknown conflicts
%-wy.el : %.wy $(WISI_OPENTOKEN)/wisi-generate.exe
	cd ./$(<D); $(WISI_OPENTOKEN)/wisi-generate.exe -v 1 $(<F) Elisp > $(*F).output
	cd ./$(<D); dos2unix $(@F)

# ../../../org.opentoken/wisi/wisi-generate.adb
$(WISI_OPENTOKEN)/wisi-generate.exe : force
	$(MAKE) -C $(WISI_OPENTOKEN) wisi-generate.exe

.PHONY : force

byte-compile-clean :
	cd ../..; rm -f *.elc

clean ::
	cd ../..; rm -f *-wy.el *.elc
	rm -f *-wy.el *.elc

test-clean ::
	rm -f *.output *.wisi-test *.stamp
	cd ../../test/wisi/; rm -f *-wy.el *.output

INDENT.EL := ada-wisi.el gpr-wisi.el ../../gpr-grammar-wy.el ../../ada-grammar-wy.el
include ../common.make

.PHONY : force

test-ada-gnatxref : RUNTEST := run-indent-test-wisi.el
test-ada-gnatxref : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-ada-gnatxref.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-gnatxref
	touch $@

test-ada-gnatinspect : RUNTEST := run-indent-test-wisi-gnatinspect.el
test-ada-gnatinspect : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-ada-gnatinspect.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-gnatinspect
	touch $@

# eval: (ada-parse-prj-file "wisi_opentoken.prj")
# eval: (ada-select-prj-file "wisi_opentoken.prj")

# Local Variables:
# eval: (ada-parse-prj-file "../ada-mode.prj")
# eval: (ada-select-prj-file "../ada-mode.prj")
# End:
# end of file
