# run tests on Emacs ada-mode indentation engine
#
# In general, we run gnatmake -gnatc to make sure the Ada is
# syntactically correct; in general, we don't care if the indentation
# does the wrong thing on incorrect code, as long as it doesn't hang
# or crash. Exceptions noted below in COMPILE_FILES.
#
# see test-indent.sh to test user directory nicely
# FIXME: add target/script to run tests with GPS; gps -P<your_project> --load=python:reformat_project.py
#     project Default is
#     end Default;
# c:/apps/GNAT-GPS-5.1.2/share/examples/gps/python/reformat_project.py

all : compile test-clean test

# emacs to test with
#
# Something in Cygwin bash declares EMACS=t when running under make
# under Emacs, so we can't use ?= here. This can be overridden with
# 'make EMACS=...'.
EMACS := $(EMACSPATH)/emacs

one : test-clean debug.adb.diff

TEST_FILES := $(shell ls *.ad?)

# this is for debug only
TEST_FILES := $(filter-out debug.adb, $(TEST_FILES))

# FIXME: delete currently broken tests
TEST_FILES := $(filter-out ada_mode-quantified_expressions.adb, $(TEST_FILES)) # 2012 syntax
TEST_FILES := $(filter-out move_to_end.adb, $(TEST_FILES)) # tests ada-move-to-end, which we have not (yet?) implemented
TEST_FILES := $(filter-out move_to_end_if.adb, $(TEST_FILES)) # ada-move-to-end
TEST_FILES := $(filter-out test_goto_decl_start.adb, $(TEST_FILES)) # ada-move-to-end
TEST_FILES := $(filter-out which_test.adb, $(TEST_FILES)) # imenu-default-create-index-function
# end FIXME:

COMPILE_FILES := $(TEST_FILES)

COMPILE_FILES := $(filter-out ada_mode-ada83.ads, $(COMPILE_FILES))# font-lock only

# This has incomplete code deliberately; used for interactive editing test (via EMACSCMD)
COMPILE_FILES := $(filter-out ada_mode-interactive_return.adb, $(COMPILE_FILES))

# This has some illegal code, left that way to test graceful handling in ada-smie
COMPILE_FILES := $(filter-out mats_weber_bugs.adb, $(COMPILE_FILES))

COMPILE_FILES := $(filter-out g-comlin.adb, $(COMPILE_FILES))# copied from gnat runtime; gnat won't compile it!
COMPILE_FILES := $(filter-out gnatprep.adb, $(COMPILE_FILES))# could run thru gnatprep, but it's not worth it.
COMPILE_FILES := $(filter-out highlight.adb, $(COMPILE_FILES))# font-lock only

# These heritage tests don't compile, and they are not worth fixing
COMPILE_FILES := $(filter-out move_to_end.adb, $(COMPILE_FILES))# hard-coded line numbers
COMPILE_FILES := $(filter-out parent.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out prime-volatilities.adb, $(COMPILE_FILES))

.PHONY : all nominal one test test-clean

test : $(addsuffix .diff, $(TEST_FILES)) test-elisp

test-elisp :
	$(EMACS) -Q -batch -L .. -l ada-mode-test.el

%.adb.diff : %.adb %.adb.tmp
	diff -u $^ > $*.adb.diff

%.ads.diff : %.ads %.ads.tmp
	diff -u $^ > $*.ads.diff

.PRECIOUS : %.adb.tmp %.ads.tmp

%.adb.tmp : %.adb ../ada-smie.el
	$(EMACS) -Q -batch -L .. -l runtest.el --eval '(run-test "$<")'

%.ads.tmp : %.ads ../ada-smie.el
	$(EMACS) -Q -batch -L .. -l runtest.el --eval '(run-test "$<")'

COMPILE_FILES := $(COMPILE_FILES:.adb=.ali)
COMPILE_FILES := $(COMPILE_FILES:.ads=.ali)

# remove duplicates
COMPILE_FILES := $(sort $(COMPILE_FILES))

compile : $(COMPILE_FILES)

# we compile with -gnatyN3 to be sure our indentation meets gnat's
# check. We don't check any other style requirements; not needed for
# comparing indentation, and they get in the way.

# bogus warning when compile with -gnatc; bug report [LA23-003]
ada_mode-quantified_expressions.ali : ada_mode-quantified_expressions.adb
	gnatmake -gnat2012  $<

# Some files set Ada mode indentation options that violate gnat's requirement
adacore_d304_005_2.ali : adacore_d304_005_2.adb
	gnatmake -gnat2012 -gnatc  $<

bug_1920.ali : bug_1920.adb
	gnatmake -gnat2012 -gnatc  $<

find_file.ali : find_file.ads
	gnatmake -gnat2012 -gnatc  $<

named_block.ali : named_block.adb
	gnatmake -gnat2012 -gnatc  $<

%.ali : %.adb
	gnatmake -gnat2012 -gnatc -gnatyN3 $<

%.ali : %.ads
	gnatmake -gnat2012 -gnatc -gnatyN3 $<

clean : compile-clean test-clean
	rm -f *~ ../*~

compile-clean :
	rm -f *.ali

test-clean :
	rm -f *.diff *.tmp

zip :
	tar zcf org.emacs.ada-mode.smie-`date +%Y-%m-%d`.tar.gz --exclude _MTN --exclude "*~" --exclude "*.diff" --exclude "*.tmp" --exclude "*.ali" --exclude "*.tar.gz" -C ../.. org.emacs.ada-mode.smie

# end of file
