# run tests on Emacs ada-mode indentation engine

# In general, we run gnatmake -gnatc to make sure the Ada is
# syntactically correct; in general, we don't care if the indentation
# does the wrong thing on incorrect code, as long as it doesn't hang
# or crash. Exceptions noted below in COMPILE_FILES.

all : compile test

# emacs to test with
# EMACS := /home/Projects/emacs/work_stephe-build/src/emacs
# EMACS := /home/Projects/emacs/work_stephe-23-build/src/emacs
# EMACS := /usr/local/bin/emacs
EMACS := $(EMACSPATH)/emacs.exe

#nominal : EMACS := /usr/bin/emacs
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
#TEST_FILES := $(filter-out bug_5746.adb, $(TEST_FILES))

COMPILE_FILES := $(TEST_FILES)

# These heritage tests don't compile, and they are too hard to fix
COMPILE_FILES := $(filter-out ada-test.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out beginindent.adb, $(COMPILE_FILES))
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
	$(EMACS) -Q -batch -L .. -l runtest.el --eval '(run-test "$<")'

%.ads.tmp : %.ads ../ada-indent.el
	$(EMACS) -Q -batch -L .. -l runtest.el --eval '(run-test "$<")'

COMPILE_FILES := $(COMPILE_FILES:.adb=.ali)
COMPILE_FILES := $(COMPILE_FILES:.ads=.ali)

# remove duplicates
COMPILE_FILES := $(sort $(COMPILE_FILES))

compile : $(COMPILE_FILES)

%.ali : %.adb %.ads
	gnatmake -gnat2012 -gnatc $<

%.ali : %.ads
	gnatmake -gnat2012 -gnatc $<

clean : compile-clean test-clean

compile-clean :
	rm -f *.ali

test-clean :
	rm -f *.diff *.tmp

zip :
	tar zcf org.emacs.ada-mode.smie-`date +%Y-%m-%d`.tar.gz --exclude _MTN --exclude "*~" --exclude "*.diff" --exclude "*.tmp" --exclude "*.ali" --exclude "*.tar.gz" -C ../.. org.emacs.ada-mode.smie

# end of file
