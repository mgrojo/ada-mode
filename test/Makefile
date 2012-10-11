# run tests on Emacs ada-mode indentation engine

# In general, we run gnatmake -gnatc to make sure the Ada is
# syntactically correct; in general, we don't care if the indentation
# does the wrong thing on incorrect code, as long as it doesn't hang
# or crash. Exceptions noted below in COMPILE_FILES.

all : compile test

# emacs to test with
# EMACS_EXE := /home/Projects/emacs/work_stephe-build/src/emacs
# EMACS_EXE := /home/Projects/emacs/work_stephe-23-build/src/emacs
# EMACS_EXE := /usr/local/bin/emacs
EMACS_EXE ?= $(EMACSPATH)/emacs.exe

#nominal : EMACS_EXE := /usr/bin/emacs
nominal : ada_mode-nominal.ali
nominal : ada_mode-nominal.ads.diff
nominal : ada_mode-nominal.adb.diff
nominal : ada_mode-nominal-child.ali
nominal : ada_mode-nominal-child.ads.diff
nominal : ada_mode-generic_package.ali
nominal : ada_mode-generic_package.ads.diff

user : /Projects/smm.main/source/smm-download.adb.diff

# FIXME: add target to test user directory nicely (no clutter)
# FIXME: add target to run tests with Ada mode 4.01 to compare

one : options.ads.diff

TEST_FILES := $(shell ls *.ad?)

# delete currently broken tests
TEST_FILES := $(filter-out adacore_d304_005_2.adb, $(TEST_FILES)) # ada-indent-label applies to block labels
TEST_FILES := $(filter-out ada-test.adb, $(TEST_FILES)) # multiple compilation units, minor issues
TEST_FILES := $(filter-out bug_1920.adb, $(TEST_FILES)) # ada-indent-label
TEST_FILES := $(filter-out ada_mode-options-indent_return_1.ads, $(TEST_FILES)) # ada-indent-return
TEST_FILES := $(filter-out ada_mode-options-indent_return_2.ads, $(TEST_FILES)) # ada-indent-return

# FIXME: Some library/separate source files don't indent properly if
# there's no leading comment/white space in the file, so don't run the
# tests yet.
TEST_FILES := $(filter-out ada_mode-library_function.adb, $(TEST_FILES))
TEST_FILES := $(filter-out ada_mode-library_procedure.adb, $(TEST_FILES))
TEST_FILES := $(filter-out ada_mode-separate_function.adb, $(TEST_FILES))
TEST_FILES := $(filter-out ada_mode-separate_procedure.adb, $(TEST_FILES))
TEST_FILES := $(filter-out ada_mode-separate_task_body.adb, $(TEST_FILES))

COMPILE_FILES := $(TEST_FILES)

# this is for debug only
COMPILE_FILES := $(filter-out debug.adb, $(COMPILE_FILES))

# These heritage tests don't compile, and they are too hard to fix
# FIXME: review these
COMPILE_FILES := $(filter-out ada-test.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out g-comlin.adb, $(COMPILE_FILES))# copied from gnat runtime; gnat won't compile it!
COMPILE_FILES := $(filter-out gnatprep.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out highlight.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out indent9.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out indent.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out mode.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out move_to_end.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out other_1.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out paramlist2.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out parent.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out prime-volatilities.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out private.adb, $(COMPILE_FILES))

.PHONY : all nominal one test test-clean

test : $(addsuffix .diff, $(TEST_FILES))

%.adb.diff : %.adb %.adb.tmp
	diff -u $^ > $*.adb.diff

%.ads.diff : %.ads %.ads.tmp
	diff -u $^ > $*.ads.diff

.PRECIOUS : %.adb.tmp %.ads.tmp

%.adb.tmp : %.adb ../ada-indent.el
	$(EMACS_EXE) -Q -batch -L .. -l runtest.el --eval '(run-test "$<")'

%.ads.tmp : %.ads ../ada-indent.el
	$(EMACS_EXE) -Q -batch -L .. -l runtest.el --eval '(run-test "$<")'

COMPILE_FILES := $(COMPILE_FILES:.adb=.ali)
COMPILE_FILES := $(COMPILE_FILES:.ads=.ali)

# remove duplicates
COMPILE_FILES := $(sort $(COMPILE_FILES))

compile : $(COMPILE_FILES)

# we compile with -gnatyN3 to be sure our indentation meets gnat's
# check. We don't check any other style requirements; not needed for
# comparing indentation, and they get in the way.

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

compile-clean :
	rm -f *.ali

test-clean :
	rm -f *.diff *.tmp

zip :
	tar zcf org.emacs.ada-mode.smie-`date +%Y-%m-%d`.tar.gz --exclude _MTN --exclude "*~" --exclude "*.diff" --exclude "*.tmp" --exclude "*.ali" --exclude "*.tar.gz" -C ../.. org.emacs.ada-mode.smie

# end of file
