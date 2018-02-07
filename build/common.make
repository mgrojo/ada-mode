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

ADA_GPS_TEST_FILES := $(shell cd ../test/ada-gps; ls *.ad[sb])

.PRECIOUS : %-elisp.el %-process.el %.ads %.re2c %.tmp %_process.adb %_re2c.c

.PHONY : all force one test test-clean

vpath %.adb   ../test ../test/ada-gps ../test/subdir ../test/wisi
vpath %.ads   ../test ../test/ada-gps ../test/subdir ../test/wisi
vpath %.re2c  ../test/wisi
vpath %.el    ../ ../test/wisi
vpath %.gpr   ../test/gpr
vpath %.input ../test/wisi
vpath %.wy    ../ ../test/wisi

# emacs to test with
#
# This can be overridden on the 'make' command line or by an external
# environment variable.
EMACS_EXE ?= emacs

test-elisp :
	$(EMACS_EXE) -Q -batch -L ../test -L . $(ADA_MODE_DIR) -l ada-mode-test.el

gpr-skel.gpr.tmp :
	$(EMACS_EXE) -Q -batch -L ../test/gpr -L . $(ADA_MODE_DIR) -l gpr-skel-test.el --eval '(progn (setq vc-handled-backends nil)(gpr-skel-test))'

%.diff : % %.tmp
	-diff -u $< $*.tmp > $*.diff

%.diff-run : % %.tmp
	-diff -u $< $*.tmp

%.wisi-test : %-elisp.el
	$(EMACS_EXE) -Q -batch -L . $(ADA_MODE_DIR) -l run-wisi-test.el --eval '(run-test "$*")'

%_wisi_parse.exe : %_wisi_parse.ads %_process.ads %_re2c.c force
	gprbuild -p wisi_parse.gpr $<

run_%_parse.exe : run_%_parse.ads %_process.ads %_re2c.c force
	gprbuild -p wisi_parse.gpr $<

# -v 1 dumps grammar
%-elisp.el : %.wy $(WISI_WISITOKEN)/wisi-generate.exe
	cd ./$(<D); $(WISI_WISITOKEN)/wisi-generate.exe -v 1 --lexer Elisp --output_language Elisp $(<F) > $(*F).elisp_parse_table
ifeq ($(shell uname),Linux)
else ifeq ($(shell uname),Darwin)
else
# windows
	cd ./$(<D); dos2unix $(@F)
endif

elisp-clean :
	rm -f ../*.output ../autoloads.el
	rm -f ../*-wy.el ../*.elc

# We create the output files in the same directory as the .wy file, so
# they can be saved in CM together.
%_process.ads %.re2c : %.wy $(WISI_WISITOKEN)/wisi-generate.exe
	cd ./$(<D); $(WISI_WISITOKEN)/wisi-generate.exe -v 1 --output_language Ada_Emacs --lexer re2c --interface process --enum $(<F) > $(*F).ada_parse_table
	cd ./$(<D); dos2unix $(*F)_process.ads $(*F)_process.adb $(*F)-process.el $(*F).re2c

%_re2c.c : %.re2c
	$(RE2C_HOME)/bin/re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<

autoloads : force
	$(EMACS_EXE) -Q -batch --eval '(progn (setq vc-handled-backends nil)(let ((generated-autoload-file (expand-file-name "../autoloads.el")))(update-directory-autoloads "../")))'

# load path rationale:
#    .. for run-*.el
#    ADA_MODE_DIR = "-L .. -l "autoloads.el"" for developing ada-mode
#    ADA_MODE_DIR = "-f package-initialize" for testing installed ELPA package
ADA_MODE_DIR ?= -l define_ADA_MODE_DIR

# All gpr-query functions run "gpr_query" in a background process.
# That fails in batch mode; batch mode does not support background
# processes. FIXME: not true in Emacs 25? So we don't run tests in
# batch mode. We can't use -nw here because the standard input is not
# a tty (at least on Windows). We don't include any other
# dependencies, because the complete list is complex, and we sometimes
# want to ignore it.
%.tmp : %
	$(EMACS_EXE) -Q -L . $(ADA_MODE_DIR) -l $(RUNTEST) --eval '(progn (run-test "$<")(kill-emacs))'

COMPILE_FILES := $(COMPILE_FILES:.adb=.ali)
COMPILE_FILES := $(COMPILE_FILES:.ads=.ali)

# remove duplicates
COMPILE_FILES := $(sort $(COMPILE_FILES))

compile-ada-test : force
	gprbuild -v -p ../test/ada_mode_compile.gpr

# we compile with -gnatyN3 to be sure our indentation meets gnat's
# check. We don't check any other style requirements; not needed for
# comparing indentation, and they get in the way.

# override on command line for other compiler versions
GPRBUILD := gprbuild

%.ali : %.adb
	$(GPRBUILD) -P ada_mode_compile.gpr -c $(<F)

%.ali : %.ads
	$(GPRBUILD) -P ada_mode_compile.gpr -c $(<F)

%.info : %.texi
	makeinfo $< -o ../$@

%.html : %.texi
	makeinfo --html --no-split $< -o ../$@

# (grep-find "find .. -type f -print | xargs grep -n FIXME")

clean :: build-ada-exec-clean compile-ada-test-clean doc-clean elisp-clean exe-clean source-clean test-clean profile-clean
	rm -f check_xref.gpr Makefile.conf

doc-clean ::
	rm -f ../*.info ../*.html ../dir-ada-mode

# delete the gpr_query database, to be sure it is rebuilt accurately
# for the current compiler version.
compile-ada-test-clean :
	rm -f ../test/*.ali ../test/subdir/*.ali
	rm -f ../test/*.o ../test/subdir/*.o
	rm -f ../test/*.std* ../test/subdir/*.std*
	rm -f ../test/gpr_query.db*

exe-clean ::
	rm -rf obj
	rm -rf ../obj
	rm -rf ../gpr_query$(EXE_EXT) ../gpr_query.gpr
	rm -rf ../gpr_query-process_refresh.adb
	rm -rf ../ada_mode_gps_indent$(EXE_EXT) ../ada_mode_gps_indent.gpr
	rm -rf ../ada_mode_wisi_parse$(EXE_EXT)
	rm -rf ../gpr_mode_wisi_parse$(EXE_EXT)
	rm -rf ../run_ada_parser$(EXE_EXT)
	rm -rf ../run_gpr_parser$(EXE_EXT)

profile-clean ::
	rm -rf ../exec_pro ../obj_pro

# delete all files created by wisi-generate for main programs
build-ada-exec-clean :
	cd ..; rm -f *.*_parse_table *.re2c *_re2c.c *_re2c_c.ads *-elisp.el *-process.el *_process.ad?

test-clean ::
	rm -f *.diff *.tmp
# ada_mode-spec.adb is a temporary, generated by
# ada-make-package-body.
	rm -f ../test/ada_mode-spec.adb
	rm -f *.log *.output *.wisi-test *.stamp
	cd ../test/wisi/; rm -f *-elisp.el *.*parse_table
	cd ../test/wisi/; rm -f subprograms.ada_parse_table subprograms.re2c subprograms_re2c.c subprograms_re2c_c.ads subprograms-process.el subprograms_process.ad? subprograms_wisi_parse.exe

source-clean :: test-clean
	-find ../ -name "*~" -print -delete
	-find ../ -name ".#*" -print -delete

# end of file
