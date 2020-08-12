# common rules for Emacs Ada and gpr mode tests

ADA_TEST_FILES := $(shell cd ../test; ls *.ad[sb])
ADA_TEST_FILES := $(ADA_TEST_FILES) $(shell cd ../test; ls subdir/*.ad[sb])

GNATXREF_TEST_FILES := $(shell cd ../test; grep -l wisi-prj-select-cache *.ad[sb])

GPR_TEST_FILES := $(shell cd ../test/gpr; ls *.gpr)
GPR_TEST_FILES := $(filter-out debug.gpr, $(GPR_TEST_FILES))
GPR_TEST_FILES := $(filter-out gpr-skel.gpr, $(GPR_TEST_FILES))

RECOVER_TEST_FILES := $(shell cd ../test/correct; ls *.ad?)

.PRECIOUS : %-process.el %.ads %_packrat.re2c %.re2c %.tmp %_process.adb %_re2c.c %_packrat_re2c.c %.diff wisitoken-ada_lite-tokens/%.tokens ada_lite-correct-tokens/%.tokens

.PHONY : all force one test test-clean

vpath %.adb   ../test ../test/subdir
vpath %.ads   ../test ../test/subdir
vpath %.gpr   ../test/gpr
vpath %.wy    ../

test-elisp :
	$(EMACS_EXE) -Q -batch -L ../test -L . $(ADA_MODE_DIR) -l ada-mode-test.el

gpr-skel.gpr.tmp :
	$(EMACS_EXE) -Q -batch -L ../test/gpr -L . $(ADA_MODE_DIR) -l gpr-skel-test.el --eval '(progn (setq vc-handled-backends nil)(gpr-skel-test))'

%.diff : % %.tmp
	-diff -u $< $(*F).tmp > $(*F).diff

%.diff-run : % %.tmp
	-diff -u $< $(*F).tmp

# for building only these
../run_ada_lalr_parse.exe : ../run_ada_lalr_parse.ads ../ada_re2c.c force
	gprbuild -p -j8 ../ada_mode_wisi_parse.gpr $(<F)

../run_ada_lr1_parse.exe : ../run_ada_lr1_parse.ads ../ada_re2c.c force
	gprbuild -p -j8 ../ada_mode_wisi_parse.gpr $(<F)

../run_ada_libadalang_parse.exe : ../run_ada_libadalang_parse.ads force
	gprbuild -p -j8 ../ada_mode_wisi_parse.gpr $(<F)

../run_ada_annex_p_lalr_parse.exe : ../run_ada_annex_p_lalr_parse.ads ../ada_annex_p_re2c.c force
	gprbuild -p -j8 ../ada_mode_wisi_parse.gpr $(<F)

../run_ada_annex_p_lr1_parse.exe : ../run_ada_annex_p_lr1_parse.ads ../ada_annex_p_re2c.c force
	gprbuild -p -j8 ../ada_mode_wisi_parse.gpr $(<F)

elisp-clean :
	rm -f ../*.output ../autoloads.el
	rm -f ../*.elc

ifeq ($(TEST_DIR),source)
$(WISITOKEN_GENERATE) : force
	$(MAKE) -C $(WISITOKEN)/build wisitoken-bnf-generate.exe Standard_Common_Profile=Off Standard_Common_Mem_Check=Off
else
endif

# We create the output files in the same directory as the .wy file, so
# they can be saved in CM together.
%.re2c : %.wy $(WISITOKEN_GENERATE)
	cd ./$(<D); $(WISITOKEN_GENERATE) $(IGNORE_CONFLICTS) --time --output_bnf $(<F)
	cd ./$(<D); dos2unix -q $(*F)-process.el $(*F)_process* $(*F).re2c $(*F)_re2c_c.ads

%_re2c.c : %.re2c
	re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<
	cd ./$(<D); dos2unix -q $(*F)_re2c.c

followed-by : ARGS := ../ada_annex_p.wy term_list REM 1
followed-by : $(WISITOKEN)/build/wisitoken-followed_by.exe
	$(WISITOKEN)/build/wisitoken-followed_by.exe $(ARGS)

$(WISITOKEN)/build/wisitoken-followed_by.exe : force
	cd $(WISITOKEN)/build; make -r wisitoken-followed_by.exe

autoloads : force
	$(EMACS_EXE) -Q -batch --eval "(progn (require 'autoload)(setq generated-autoload-file (expand-file-name \"../autoloads.el\"))(update-directory-autoloads \"../\"))"

# load path rationale:
#    . for run-*.el
#    ADA_MODE_DIR = "-L .. -l "autoloads.el"" for developing ada-mode
#    ADA_MODE_DIR = "-f package-initialize" for testing installed ELPA package
#
# All gpr-query functions run "gpr_query" in a background process.
# That fails in batch mode; batch mode does not support background
# processes. FIXME: not true in Emacs 25? So we don't run tests in
# batch mode. We can't use -nw here because the standard input is not
# a tty (at least on Windows). We don't include any other
# dependencies, because the complete list is complex, and we sometimes
# want to ignore it.
%.tmp : %
	$(EMACS_EXE) -Q -L . $(ADA_MODE_DIR) -l $(RUNTEST) --eval '(progn (run-test "$<")(kill-emacs))'

%.debug : %
	$(EMACS_EXE) -Q -L . $(ADA_MODE_DIR) -l $(RUNTEST) --eval '(progn (package-initialize)(setq debug-on-error t))' $<

benchmark :
	$(EMACS_EXE) -Q -L . $(ADA_MODE_DIR) -l benchmark.el

COMPILE_FILES := $(COMPILE_FILES:.adb=.ali)
COMPILE_FILES := $(COMPILE_FILES:.ads=.ali)

# remove duplicates
COMPILE_FILES := $(sort $(COMPILE_FILES))

compile-ada-test : force
	rm -f ../test/debug.ad?
	rm -f ../test/subdir/ada_mode-spec.adb
	gprbuild -p -j8 ../test/ada_mode_compile.gpr

# we compile with -gnatyN3 to be sure our indentation meets gnat's
# check. We don't check any other style requirements; not needed for
# comparing indentation, and they get in the way.

%.ali : %.adb
	gprbuild -P ada_mode_compile.gpr -c $(<F)

%.ali : %.ads
	gprbuild -P ada_mode_compile.gpr -c $(<F)

%.info : %.texi
	makeinfo $< -o ../$@

%.html : %.texi
	makeinfo --html --no-split $< -o ../$@

# (grep-find "find .. -type f -print | xargs grep -n FIXME")

# for recompiling with release options
recursive-clean : force
	gprclean -r -P ../ada_mode_wisi_parse.gpr

clean :: build-ada-exec-clean compile-ada-test-clean doc-clean elisp-clean exe-clean source-clean test-clean profile-clean
	rm -f check.gpr Makefile.conf

doc-clean ::
	rm -f ../*.info ../*.html ../dir-ada-mode

# delete the gpr_query database, to be sure it is rebuilt accurately
# for the current compiler version.
compile-ada-test-clean :
	rm -f ../test/*.ali ../test/subdir/*.ali
	rm -f ../test/*.o ../test/subdir/*.o
	rm -f ../test/*.std* ../test/subdir/*.std*
	rm -f ../test/gpr_query.db*
	rm -f ../test/debug*

exe-clean ::
	rm -rf ../obj
	rm -rf ../gpr_query$(EXE_EXT) ../gpr_query.gpr
	rm -rf ../gpr_query-process_refresh.adb
	rm -rf ../ada_mode_wisi_*_parse$(EXE_EXT)
	rm -rf ../gpr_mode_wisi_parse$(EXE_EXT)
	rm -f ../ada_mode_wisi_parse.gpr ../wisi.gpr
	rm -rf ../run_ada_*_parse$(EXE_EXT)
	rm -rf ../run_gpr_parse$(EXE_EXT)

profile-clean ::
	rm -rf ../exec_pro ../obj_pro

# delete all files created by wisitoken-bnf-generate for main programs
build-ada-exec-clean :
	cd ..; rm -f *.parse_table *.re2c *_process*.ad? *_re2c_c.ads *_re2c.c *-process.el *_parse_table.txt

test-clean ::
	rm -f *.diff *.tmp
# ada_mode-spec.adb is a temporary, generated by
# ada-make-package-body.
	rm -f ../test/ada_mode-spec.adb
	rm -f *.log *.output *.wisi-test *.stamp

source-clean :: test-clean
	-find ../ -name "*~" -print -delete
	-find ../ -name ".#*" -print -delete

# end of file
