# common rules for Emacs Ada and gpr mode tests
#
# We run gnatmake -gnatc to make sure the Ada is syntactically
# correct; in general, we don't care if the indentation does the wrong
# thing on incorrect code, as long as it doesn't hang or crash.
# Exceptions noted below in COMPILE_FILES.
#
# Similarly, we run gnatlist on the test gpr files (for testing gpr-mode)
#
# see test-indent.sh to test user directory nicely

ADA_TEST_FILES := $(shell cd ../../test; ls *.ad[sb])
ADA_TEST_FILES := $(ADA_TEST_FILES) $(shell cd ../../test; ls subdir/*.ad[sb])

# this is for debug only
ADA_TEST_FILES := $(filter-out debug.adb, $(ADA_TEST_FILES))

# FIXME: delete currently broken tests
ADA_TEST_FILES := $(filter-out which_test.adb, $(ADA_TEST_FILES)) # imenu-default-create-index-function
# end FIXME:

GPR_TEST_FILES := $(shell cd ../../test/gpr; ls *.gpr)

COMPILE_FILES := $(ADA_TEST_FILES)
COMPILE_FILES := $(subst subdir/,,$(COMPILE_FILES))

COMPILE_FILES := $(filter-out ada_mode-ada83.ads, $(COMPILE_FILES))# font-lock only

# This has incomplete code deliberately; used for interactive editing test (via EMACSCMD)
COMPILE_FILES := $(filter-out ada_mode-interactive.adb, $(COMPILE_FILES))

# This has incomplete code; tests a former bug in syntax-ppss
COMPILE_FILES := $(filter-out ada_mode-long_paren.adb, $(COMPILE_FILES))

# This has incomplete code; used for testing OpenToken special rule
COMPILE_FILES := $(filter-out ada_mode-opentoken.ads, $(COMPILE_FILES))

# This has some illegal code, left that way to test graceful handling in ada-smie
COMPILE_FILES := $(filter-out mats_weber_bugs.adb, $(COMPILE_FILES))

COMPILE_FILES := $(filter-out g-comlin.adb, $(COMPILE_FILES))# copied from gnat runtime; gnat won't compile it!
COMPILE_FILES := $(filter-out gnatprep.adb, $(COMPILE_FILES))# could run thru gnatprep, but it's not worth it.
COMPILE_FILES := $(filter-out highlight.adb, $(COMPILE_FILES))# font-lock only

# These heritage tests don't compile, and they are not worth fixing
COMPILE_FILES := $(filter-out parent.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out prime-volatilities.adb, $(COMPILE_FILES))

# WORKAROUND: gnat 7.0.1 gives bogus warning when compile with -gnatc; bug report [LA23-003]
# without -gnatc, get warnings about missing bodies
COMPILE_FILES := $(filter-out ada_mode-quantified_expressions.adb, $(COMPILE_FILES))

.PHONY : all nominal one test test-clean

vpath %.ads ../../test ../../test/subdir
vpath %.adb ../../test ../../test/subdir
vpath %.gpr ../../test/gpr

test : test-ada test-gpr test-elisp

test-ada : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-gpr : $(addsuffix .diff, $(subst subdir/,,$(GPR_TEST_FILES)))

# emacs to test with
#
# Something in Cygwin bash declares EMACS=t when running under make
# under Emacs, so we can't use ?= here.
#
# emacs 24.2.91 and earlier define "emacs_dir".
#
# This can be overridden with 'make EMACS=...'.
EMACS := $(emacs_dir)/bin/emacs

test-elisp :
	$(EMACS) -Q -batch -L ../../test -L ../.. -l ada-mode-test.el

%.diff : % %.tmp
	diff -u $< $*.tmp > $*.diff

.PRECIOUS : %.tmp

# load path; .. for runtest.el, ../.. for ada-mode.el etc
%.tmp : %
	$(EMACS) -Q -batch -L .. -L ../.. -l $(RUNTEST) --eval '(run-test "$<")'

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
	gnatmake -gnat2012 -P ada_mode_parent.gpr -gnatc -gnatyN3 $(<F)

%.ali : %.ads
	gnatmake -gnat2012 -P ada_mode_parent.gpr -gnatc -gnatyN3 $(<F)

# (grep-find "find .. -type f -print | xargs grep -n FIXME")
clean :: compile-clean test-clean
	find ../../ -name *~ -delete

# .ali files are in source dir, so they are shared between wisi and smie tests
compile-clean :
	rm -f ../../test/*.ali ../../test/subdir/*.ali *.ali

test-clean :
	rm -f *.diff *.tmp
# ada_mode-nominal-child.adb is a temporary, generated by
# ada-make-package-body.
	rm -f ../../test/ada_mode-nominal-child.adb

zip :
	tar zcf org.emacs.ada-mode.smie-`date +%Y-%m-%d`.tar.gz --exclude _MTN --exclude "*~" --exclude "*.diff" --exclude "*.tmp" --exclude "*.ali" --exclude "*.tar.gz" -C ../../../.. org.emacs.ada-mode.smie

# end of file
