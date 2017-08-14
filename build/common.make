# common rules for Emacs Ada and gpr mode tests
#
# We run gprbuild -c to make sure the Ada is syntactically
# correct; in general, we don't care if the indentation does the wrong
# thing on incorrect code, as long as it doesn't hang or crash.
# Exceptions noted below in COMPILE_FILES.

ADA_TEST_FILES := $(shell cd ../test; ls *.ad[sb])
ADA_TEST_FILES := $(ADA_TEST_FILES) $(shell cd ../test; ls subdir/*.ad[sb])

ADA_TEST_FILES := $(filter-out debug.adb, $(ADA_TEST_FILES))# debug only
ADA_TEST_FILES := $(filter-out debug.ads, $(ADA_TEST_FILES))# debug only

GPR_TEST_FILES := $(shell cd ../test/gpr; ls *.gpr)
GPR_TEST_FILES := $(filter-out debug.gpr, $(GPR_TEST_FILES))
GPR_TEST_FILES := $(filter-out gpr-skel.gpr, $(GPR_TEST_FILES))

COMPILE_FILES := $(ADA_TEST_FILES)

COMPILE_FILES := $(subst subdir/,,$(COMPILE_FILES))

# filter out files that are not compiled; in file alphabetical order; comment explains why.

COMPILE_FILES := $(filter-out access_in_record.ads, $(COMPILE_FILES))# incomplete code
COMPILE_FILES := $(filter-out ada_mode-ada83.ads, $(COMPILE_FILES))# font-lock only
COMPILE_FILES := $(filter-out ada_mode-ada95.ads, $(COMPILE_FILES))# font-lock only
COMPILE_FILES := $(filter-out ada_mode-ada2005.ads, $(COMPILE_FILES))# font-lock only
COMPILE_FILES := $(filter-out ada_mode-ada2012.ads, $(COMPILE_FILES))# font-lock only

# parents are not pure
COMPILE_FILES := $(filter-out ada_mode-generic_parent_instantiation.ads, $(COMPILE_FILES))

# These have incomplete code deliberately; used for interactive editing test (via EMACSCMD)
COMPILE_FILES := $(filter-out ada_mode-interactive_common.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out ada_mode-interactive_smie.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out ada_mode-interactive_wisi.adb, $(COMPILE_FILES))
COMPILE_FILES := $(filter-out ada_mode-interactive_gps_fallback.adb, $(COMPILE_FILES))

# This has incomplete code; tests a former bug in syntax-ppss
COMPILE_FILES := $(filter-out ada_mode-long_paren.adb, $(COMPILE_FILES))

# GNAT GPL 2016 complains about compiling these; not clear why
COMPILE_FILES := $(filter-out ada_mode-nominal-separate_package_1-separate_procedure_1.adb, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) ada_mode-nominal-separate_package_1-separate_procedure_1.adb
COMPILE_FILES := $(filter-out ada_mode-nominal-separate_package_1-separate_procedure_2.adb, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) ada_mode-nominal-separate_package_1-separate_procedure_2.adb
COMPILE_FILES := $(filter-out ada_mode-nominal-separate_package_1.adb, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) ada_mode-nominal-separate_package_1.adb

# This has incomplete code; used for testing OpenToken special rule
COMPILE_FILES := $(filter-out ada_mode-opentoken.ads, $(COMPILE_FILES))

# GNAT GPL 2016 complains about compiling these; not clear why
COMPILE_FILES := $(filter-out ada_mode-options-indent_return_1.ads, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) ada_mode-options-indent_return_1.ads
COMPILE_FILES := $(filter-out ada_mode-options-indent_return_2.ads, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) ada_mode-options-indent_return_2.ads
COMPILE_FILES := $(filter-out ada_mode-options-indent_return_3.ads, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) ada_mode-options-indent_return_3.ads
COMPILE_FILES := $(filter-out ada_mode-separate_function.adb, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) ada_mode-separate_function.adb
COMPILE_FILES := $(filter-out ada_mode-separate_procedure.adb, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) ada_mode-separate_procedure.adb
COMPILE_FILES := $(filter-out ada_mode-separate_protected_body.adb, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) ada_mode-separate_protected_body.adb
COMPILE_FILES := $(filter-out ada_mode-separate_task_body.adb, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) ada_mode-separate_task_body.adb
COMPILE_FILES := $(filter-out ada_mode-spec.ads, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) ada_mode-spec.ads

# This has illegal code; used for testing skeleton expansion
COMPILE_FILES := $(filter-out ada_skel.adb, $(COMPILE_FILES))

# This is generated by gnatstub, and raises compile time errors
COMPILE_FILES := $(filter-out ada_mode-spec.adb, $(COMPILE_FILES))

# GNAT GPL 2016 complains about compiling these; not clear why
COMPILE_FILES := $(filter-out adacore_6505_010.ads, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) adacore_6505_010.ads
COMPILE_FILES := $(filter-out adacore_6805_003.ads, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) adacore_6805_003.ads
COMPILE_FILES := $(filter-out adacore_8114_010.ads, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) adacore_8114_010.ads
COMPILE_FILES := $(filter-out adacore_8529_012.ads, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) adacore_8529_012.ads

COMPILE_FILES := $(filter-out aspects.ads, $(COMPILE_FILES))# complicated aspects
SYNTAX_FILES  := $(SYNTAX_FILES) aspects.ads

COMPILE_FILES := $(filter-out bug_2016_11_21_01.adb, $(COMPILE_FILES))# missing declarations
COMPILE_FILES := $(filter-out g-comlin.adb, $(COMPILE_FILES))# copied from gnat runtime; gnat won't compile it!
COMPILE_FILES := $(filter-out gnatprep.adb, $(COMPILE_FILES))# could run thru gnatprep, but it's not worth it.
COMPILE_FILES := $(filter-out gps_indent_options.adb, $(COMPILE_FILES))# gnat style, cases
COMPILE_FILES := $(filter-out highlight.adb, $(COMPILE_FILES))# font-lock only

# GNAT GPL 2016 complains about compiling these; not clear why
COMPILE_FILES := $(filter-out indent6.ads, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) indent6.ads
COMPILE_FILES := $(filter-out indent8.ads, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) indent8.ads

# These heritage tests don't compile, and they are not worth fixing
COMPILE_FILES := $(filter-out parent.adb, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) parent.adb
COMPILE_FILES := $(filter-out prime-volatilities.adb, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) prime-volatilities.adb

# GNAT GPL 2016 complains about compiling these; not clear why
COMPILE_FILES := $(filter-out test_private.ads, $(COMPILE_FILES))
SYNTAX_FILES  := $(SYNTAX_FILES) test_private.ads

ADA_GPS_TEST_FILES := $(shell cd ../test/ada-gps; ls *.ad[sb])

.PRECIOUS : %-wy.el ../%-grammar-wy.el %.tmp

.PHONY : all force nominal one test test-clean

vpath %.adb ../test ../test/subdir ../test/ada-gps
vpath %.ads ../test ../test/subdir ../test/ada-gps
vpath %.el ../ .
vpath %.gpr ../test/gpr
vpath %.wy ../ ../test/wisi

# FIXME: this reports *.diff > 0 from previous tests as well
test-gpr : RUNTEST := run-indent-test-gpr.el
test-gpr : $(addsuffix .diff, $(subst subdir/,,$(GPR_TEST_FILES)))
	find . -name "*.diff" -not -size 0 >> test.log

# emacs to test with
#
# This can be overridden on the 'make' command line or by an external
# environment variable.
EMACS_EXE ?= emacs

test-elisp :
	$(EMACS_EXE) -Q -batch -L ../test -L . $(ADA_MODE_DIR) -l ada-mode-test.el

gpr-skel.gpr.tmp :
	$(EMACS_EXE) -Q -batch -L ../test/gpr -L .. $(ADA_MODE_DIR) -l gpr-skel-test.el --eval '(progn (setq vc-handled-backends nil)(gpr-skel-test))'

%.diff : % %.tmp
	-diff -u $< $*.tmp > $*.diff

%.diff-run : % %.tmp
	-diff -u $< $*.tmp

%.wisi-test : %-wy.el
	$(EMACS_EXE) -Q -batch $(ADA_MODE_DIR) -l run-wisi-test.el --eval '(run-test "$*")'

# -v 1 dumps grammar; useful for debugging parse errors
%-wy.el : %.wy $(WISI_OPENTOKEN)/wisi-generate.exe
	cd ./$(<D); $(WISI_OPENTOKEN)/wisi-generate.exe -v 1 $(<F) Elisp > $(*F).output
ifeq ($(shell uname),Linux)
else ifeq ($(shell uname),Darwin)
else
# windows
	cd ./$(<D); dos2unix $(@F)
endif

autoloads : force
	$(EMACS_EXE) -Q -batch --eval '(progn (setq vc-handled-backends nil)(let ((generated-autoload-file (expand-file-name "../autoloads.el")))(update-directory-autoloads "../")))'

# load path rationale:
#    .. for run-*.el
#    ADA_MODE_DIR = "-L ../.. -l "autoloads.el"" for developing ada-mode
#    ADA_MODE_DIR = "-f package-initialize" for testing installed ELPA package
ADA_MODE_DIR ?= -l define_ADA_MODE_DIR

# All gpr-query functions run "gpr_query" in a background process to
# save startup time. That fails in batch mode; batch mode does not
# support background processes. So we don't run tests in batch mode.
# We can't use -nw here because the standard input is not a tty (at
# least on Windows). We don't include any other dependencies, because
# the complete list is complex, and we sometimes want to ignore it.
%.tmp : %
	$(EMACS_EXE) -Q -L . $(ADA_MODE_DIR) -l $(RUNTEST) --eval '(progn (run-test "$<")(kill-emacs))'

COMPILE_FILES := $(COMPILE_FILES:.adb=.ali)
COMPILE_FILES := $(COMPILE_FILES:.ads=.ali)

SYNTAX_FILES := $(SYNTAX_FILES:.adb=.check)
SYNTAX_FILES := $(SYNTAX_FILES:.ads=.check)

# remove duplicates
COMPILE_FILES := $(sort $(COMPILE_FILES))

compile-ada-test : $(COMPILE_FILES) $(SYNTAX_FILES)

# we compile with -gnatyN3 to be sure our indentation meets gnat's
# check. We don't check any other style requirements; not needed for
# comparing indentation, and they get in the way.

# override on command line for other compiler versions
GPRBUILD := gprbuild

%.ali : %.adb
	$(GPRBUILD) -P ada_mode_compile.gpr -c $(<F)

%.ali : %.ads
	$(GPRBUILD) -P ada_mode_compile.gpr -c $(<F)

%.check : %.adb
	$(GPRBUILD) -P ada_mode_compile.gpr -c -gnats $(<F)

%.check : %.ads
	$(GPRBUILD) -P ada_mode_compile.gpr -c -gnats $(<F)

%.info : %.texi
	makeinfo $< -o ../$@

%.html : %.texi
	makeinfo --html --no-split $< -o ../$@

# (grep-find "find .. -type f -print | xargs grep -n FIXME")
clean :: compile-ada-test-clean test-clean doc-clean
	find ../ -name "*~" -delete

doc-clean ::
	rm -f ../*.info ../*.html ../dir-ada-mode
	cd ..; rm -f *-wy.el *.elc
	rm -f *-wy.el *.elc

# delete the gpr_query database, to be sure it is rebuilt accurately
# for the current compiler version.
compile-ada-test-clean :
	rm -f ../test/*.ali ../test/subdir/*.ali *.ali
	rm -f ../test/*.bexch ../test/subdir/*.bexch *.bexch
	rm -f ../test/gpr_query.db*

test-clean ::
	rm -f *.diff *.tmp
# ada_mode-spec.adb is a temporary, generated by
# ada-make-package-body.
	rm -f ../test/ada_mode-spec.adb
	rm -f *.log *.output *.wisi-test *.stamp
	cd ../test/wisi/; rm -f *-wy.el *.output


source-clean :: test-clean
	-find ../ -name "*~" -print -delete
	-find ../ -name ".#*" -print -delete

# end of file
