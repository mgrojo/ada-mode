# build and test the Emacs wisent Ada indentation and navigation engine

# test with mtn controlled source:  (setenv "ADA_MODE_DIR" "-L ../.. -l autoloads.el")
# test with installed elpa package: (setenv "ADA_MODE_DIR" "-f package-initialize")
ADA_MODE_DIR ?= -L ../.. -l autoloads.el

# "compile-ada" is a test; it verifies the test sources are legal Ada
all : byte-compile-clean compile-ada-clean update compile-ada autoloads test-clean test

# this updates everything after any source change, without running the tests.
update : ../../ada-grammar-elisp.el ../../gpr-grammar-elisp.el byte-compile

# *-elisp.el are in monotone, so this is all we need after a monotone update
# doing byte-compile-clean first avoids errors caused by loading new source on old .elc
# Emacs 24.2 requires cl-lib from Gnu ELPA, so we need package-initialize here
byte-compile : byte-compile-clean
	cd ../../; $(EMACS_EXE) -Q -batch -L . --eval '(progn (package-initialize)(batch-byte-compile))' *.el

# IMPROVEME: test-wisi, test-elisp abort on failure; others accumulate failures in test.log
test : test-wisi

# The various gnatxref tests no longer work with any GPL version of GNAT.
# But some customers still manage to use them, so we rely on their
# testing.
#
# GPL 2016: 'gnatlist' does not include system libraries
# GPL 2015: 'gnatfind -pada_mode_parent.gpr' can't find ada_mode-nominal.ali
# GPL 2014: can't compile ada_mode-nominal.ads protected type
# test : test-ada-gnatxref.stamp

test : test-ada-gpr_query.stamp
test : gpr-skel.gpr.diff
test : test-gpr
test : test-elisp
test : summarize

# test-elisp debug:
# (progn (add-to-list 'load-path (expand-file-name "../../test")) (load "ada-mode-test.el"))

# test-wisi debug:
# (progn (load-file "../run-wisi-test.el") (run-test "ada-number-literal"))

test-wisi :: ada-number-literal.wisi-test
test-wisi :: body_instantiation_conflict.wisi-test
test-wisi :: case_expression.wisi-test
test-wisi :: empty_production_6.wisi-test
test-wisi :: empty_production_8.wisi-test
test-wisi :: identifier_list_name_conflict.wisi-test
test-wisi :: number-literal.wisi-test
test-wisi :: range_conflict.wisi-test
test-wisi :: subprograms.wisi-test

ONE_TEST_FILES += ada-number-literal.wisi-test
#ONE_TEST_FILES += ada_mode-nominal.ads
# one : RUNTEST := run-wisi-test.el
one : one-clean $(ONE_TEST_FILES)
one-clean: force
	rm -f $(ONE_TEST_FILES)
	cd ../../test/wisi/; rm -f *-elisp.el *.output

# one : update
# one : one-clean $(addsuffix .diff-run, $(ONE_TEST_FILES))
# one-clean: force
# 	for file in $(ONE_TEST_FILES) ; do rm -f $$file* ; done

vpath %.wy ../../ ../../test/wisi
vpath %.el ../../ .

# These are correct paths for Stephe's development machines;
# WISI_FASTTOKEN can be overridden on the 'make' command line or by an
# external environment variable.
ifeq ($(shell uname),Linux)
WISI_FASTTOKEN ?= /Projects/org.fasttoken/build/release
export ADA_PROJECT_PATH=../../test/:../../test/subdir

else ifeq ($(shell uname),Darwin)
WISI_FASTTOKEN ?= /home/Projects/fasttoken/org.fasttoken/build/release
export ADA_PROJECT_PATH=../../test/:../../test/subdir

else
# windows
# FIXME: change to wisi_generate_dir?
#WISI_FASTTOKEN ?= $(CURDIR)/../../../org.fasttoken/build/release
WISI_FASTTOKEN ?= $(CURDIR)/../../../org.fasttoken/build/release
export ADA_PROJECT_PATH=../../test/;../../test/subdir

endif

%.wisi-test : %-lalr-elisp.el
	$(EMACS_EXE) -Q -batch -L .. $(ADA_MODE_DIR) -l run-wisi-test.el --eval '(run-test "$*")'

.PRECIOUS : %-elisp.el ../../%-grammar-elisp.el

# FIXME: verify wisi-generate version

# -v 1 dumps grammar
# -v 2 dumps debug info from parser table generation, continues with unused tokens, unknown conflicts
%-lalr-elisp.el : %.wy $(WISI_FASTTOKEN)/wisi-generate.exe
	cd ./$(<D); $(WISI_FASTTOKEN)/wisi-generate.exe -v 1 $(<F) > $(*F).output
ifeq ($(shell uname),Linux)
else ifeq ($(shell uname),Darwin)
else
# windows
	cd ./$(<D); dos2unix $(@F)
	cd ./$(<D); dos2unix $*-lr1-elisp.el
endif

%-elisp.el : %.wy $(WISI_FASTTOKEN)/wisi-generate.exe
	cd ./$(<D); $(WISI_FASTTOKEN)/wisi-generate.exe -v 1 $(<F) > $(*F).output
ifeq ($(shell uname),Linux)
else ifeq ($(shell uname),Darwin)
else
# windows
	cd ./$(<D); dos2unix $(@F)
endif

# ../../../org.fasttoken/wisi/wisi-generate.adb
$(WISI_FASTTOKEN)/wisi-generate.exe : force
	$(MAKE) -C $(WISI_FASTTOKEN) wisi-generate.exe

.PHONY : force

byte-compile-clean :
	cd ../..; rm -f *.elc

clean ::
	cd ../..; rm -f *-elisp.el *.elc
	rm -f *-elisp.el *.elc

test-clean ::
	rm -f *.log *.output *.wisi-test *.stamp
	cd ../../test/wisi/; rm -f *-elisp.el *.output

include ../common.make

.PHONY : force

# suitable for copying to ../../notes.text
diffs : force
	cd ../../; find build/wisi -name "*.diff" -not -size 0

test-ada-gnatxref : RUNTEST := run-indent-test-wisi.el
test-ada-gnatxref : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-ada-gnatxref.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-gnatxref
	touch $@
	find . -name "*.diff" -not -size 0 >> test.log

test-ada-gpr_query : RUNTEST := run-indent-test-wisi-gpr_query.el
test-ada-gpr_query : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-ada-gpr_query.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-gpr_query
	touch $@
	find . -name "*.diff" -not -size 0 >> test.log

test-gpr: RUNTEST := run-indent-test-wisi.el
test-gpr: $(addsuffix .diff, $(subst subdir/,,$(GPR_TEST_FILES)))

test-gpr.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-gpr
	touch $@
	find . -name "*.diff" -not -size 0 >> test.log

summarize :
	cat *.log

# ada-gps specific tests
test-ada-gps : RUNTEST := run-indent-test-gps.el
test-ada-gps : $(addsuffix .diff, $(ADA_GPS_TEST_FILES))

test-ada-gps.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-gps
	touch $@
	find . -name "*.diff" -not -size 0 >> test.log

# compare gps indent to ada-wisi indent
compare-ada-gps : RUNTEST := run-indent-test-gps.el
compare-ada-gps : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

compare-ada-gps.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-gps
	touch $@
	find . -name "*.diff" -not -size 0 >> test.log

# we don't use 'expand-file-name' here because that adds c:/ on windows
# eval: (setenv "WISI_FASTTOKEN" "/Projects/org.fasttoken/build/release")
# eval: (ada-parse-prj-file "wisi_fasttoken.prj")
# eval: (ada-select-prj-file "wisi_fasttoken.prj")

# Local Variables:
# eval: (ada-parse-prj-file "../ada-mode.prj")
# eval: (ada-select-prj-file "../ada-mode.prj")
# End:
# end of file
