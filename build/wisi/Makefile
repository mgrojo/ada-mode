# build and test the Emacs wisent Ada indentation and navigation engine

# test with mtn controlled source:  (setenv "ADA_MODE_DIR" "-L ../.. -l autoloads.el")
# test with installed elpa package: (setenv "ADA_MODE_DIR" "-f package-initialize")
ADA_MODE_DIR ?= -L ../.. -l autoloads.el

# "compile-ada" is a test; it verifies the test sources are legal Ada.
# It also generates *.ali files for gnatxref, gpr_query
all : byte-compile-clean compile-ada-clean update compile-ada autoloads test-clean test

# this updates everything after any source change, without running the tests.
update : ../../ada_grammar-elisp.el
update : ../../gpr-grammar-elisp.el
update : ada_mode_wisi_parse.exe
update : install
update : byte-compile

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
test : test-ada-process-gpr_query.stamp
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

#ONE_TEST_FILES += ada_mode-nominal.adb.diff
#one : RUNTEST := run-indent-test-process-gpr_query.el
#one : RUNTEST := run-indent-test-elisp-gpr_query.el
ONE_TEST_FILES += subprograms.wisi-process-test
one : RUNTEST := run-wisi-process-test.el
one : export Standard_Common_Build := Debug
one : one-clean $(ONE_TEST_FILES)
one-clean: force
	rm -f $(ONE_TEST_FILES) $(ONE_TEST_FILES:.diff=.tmp)
	cd ../../test/wisi/; rm -f *-elisp.el *.output

# one : update
# one : one-clean $(addsuffix .diff-run, $(ONE_TEST_FILES))
# one-clean: force
# 	for file in $(ONE_TEST_FILES) ; do rm -f $$file* ; done

vpath %.adb ../../ ../../test/wisi
vpath %.ads ../../ ../../test/wisi
vpath %.el ../../ ../../test/wisi
vpath %.wy ../../ ../../test/wisi

# These are correct paths for Stephe's development machines;
# WISI_FASTTOKEN can be overridden on the 'make' command line or by an
# external environment variable.
ifeq ($(shell uname),Linux)
export WISI_FASTTOKEN ?= /Projects/org.fasttoken/build
export ADA_PROJECT_PATH=../../test/:../../test/subdir

else ifeq ($(shell uname),Darwin)
export WISI_FASTTOKEN ?= /home/Projects/fasttoken/org.fasttoken/build
export ADA_PROJECT_PATH=../../test/:../../test/subdir

else
# windows
export WISI_FASTTOKEN ?= /Projects/org.fasttoken/build
export ADA_PROJECT_PATH=../../test/;../../test/subdir

endif

%.wisi-test : %-elisp.el
	$(EMACS_EXE) -Q -batch -L .. $(ADA_MODE_DIR) -l run-wisi-test.el --eval '(run-test "$*")'

%.wisi-process-test : %_wisi_parse.exe
	$(EMACS_EXE) -Q -batch -L .. $(ADA_MODE_DIR) -l run-wisi-process-test.el --eval '(run-test "$*")'

%_wisi_parse.exe : %_wisi_parse.adb %-process.el force
	gprbuild -p wisi_parse.gpr $<

# we don't copy %_process.ads/b, because we'd also have to copy a lot
# of fasttoken; we add fasttoken_test.gpr to wisi_parse.gpr.
%-process.el : force
	make -C $(WISI_FASTTOKEN) $*-process.el
	cp $(WISI_FASTTOKEN)/$*-process.el ../../test/wisi/$*-process.el

.PRECIOUS : %-elisp.el %-process.el ../../%-grammar-elisp.el  %.ads

# FIXME: verify wisi-generate version

# -v 1 dumps grammar
# -v 2 dumps debug info from parser table generation, continues with unused tokens, unknown conflicts
%-elisp.el : %.wy $(WISI_FASTTOKEN)/wisi-generate.exe
	cd ./$(<D); $(WISI_FASTTOKEN)/wisi-generate.exe -v 1 $(<F) > $(*F).output
ifeq ($(shell uname),Linux)
else ifeq ($(shell uname),Darwin)
else
# windows
	cd ./$(<D); dos2unix $(@F)
endif

../../%_process.ads : ../../%.wy $(WISI_FASTTOKEN)/wisi-generate.exe
	cd ./$(<D); $(WISI_FASTTOKEN)/wisi-generate.exe -v 1 --output_language Ada_Emacs --lexer Elisp --interface process $(<F) > $(*F).ada_output
	cd ./$(<D); dos2unix $(*F)_process.ads $(*F)_process.adb $(*F)-process.el

#ada_mode_wisi_parse.exe : export Standard_Common_Build := Debug
ada_mode_wisi_parse.exe : ../../ada_grammar_process.ads force
	gprbuild -p ada_mode_wisi_parse.gpr

run_ada_parser.exe : ../../ada_grammar_process.ads force
	gprbuild -p ada_mode_wisi_parse.gpr run_ada_parser.adb

install : INSTALL_BIN := $(dir $(shell cygpath -w $(shell bash -c "type -p emacs")))
install : ada_mode_wisi_parse.exe
	strip -o $(INSTALL_BIN)$^ $^

clean ::
	rm -f *.a *.ads *.adb *.exe
	rm -rf obj

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

test-ada-gnatxref : RUNTEST := run-indent-test-elisp-gnatxref.el
test-ada-gnatxref : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-ada-gnatxref.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-gnatxref
	touch $@
	find . -name "*.diff" -not -size 0 >> test.log

test-ada-gpr_query : RUNTEST := run-indent-test-elisp-gpr_query.el
test-ada-gpr_query : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-ada-gpr_query.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-gpr_query
	touch $@
	find . -name "*.diff" -not -size 0 >> test.log

test-gpr: RUNTEST := run-indent-test-elisp.el
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

test-ada-process-gpr_query : RUNTEST := run-indent-test-process-gpr_query.el
test-ada-process-gpr_query : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-ada-process-gpr_query.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-process-gpr_query
	touch $@

# eval: (ada-parse-prj-file "../ada-mode.prj")
# eval: (ada-select-prj-file "../ada-mode.prj")

# Local Variables:
# eval: (setenv "WISI_FASTTOKEN" "/Projects/org.fasttoken/build")
# eval: (load-file "ada_mode_wisi_parse.el")
# eval: (load-file "ada_mode_elisp_prj.el")
# End:
# end of file
