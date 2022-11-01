# common rules for Emacs gpr mode tests

GPR_TEST_FILES := $(shell cd ../test; ls *.gpr)
GPR_TEST_FILES := $(filter-out debug.gpr, $(GPR_TEST_FILES))
GPR_TEST_FILES := $(filter-out gpr-skel.gpr, $(GPR_TEST_FILES))

.PRECIOUS : %-process.el %.ads %.re2c %.tmp %_process.adb %_re2c.c %.diff

.PHONY : all force one test test-clean

vpath %.gpr   ../test
vpath %.wy    ../

gpr-skel.gpr.tmp :
	$(EMACS_EXE) -Q -batch -L ../test -L . $(GPR_MODE_DIR) -l gpr-skel-test.el --eval '(progn $(ELISP)(setq vc-handled-backends nil)(gpr-skel-test))'

%.diff : % %.tmp
	-diff -u $< $(*F).tmp > $(*F).diff

../run_gpr_parse.exe : ../run_gpr_parse.ads ../gpr_re2c.c force
	gprbuild -p -j8 ../gpr_mode_wisi_parse.gpr $(<F)

elisp-clean :
	rm -f ../autoloads.el
	rm -f ../*.elc

ifeq ($(TEST_DIR),source)
$(WISITOKEN_GENERATE) : force
	$(MAKE) -C $(WISITOKEN)/build wisitoken-bnf-generate.exe Standard_Common_Profile=Off Standard_Common_Mem_Check=Off
else
endif

# We create the output files in the same directory as the .wy file, so
# they can be saved in CM together.
%.re2c : %.wy $(WISITOKEN_GENERATE)
	cd ./$(<D); $(WISITOKEN_GENERATE) $(IGNORE_CONFLICTS) --output_bnf $(<F)
	cd ./$(<D); dos2unix -q $(*F)-process.el $(*F)_process* $(*F).re2c $(*F)_re2c_c.ads

%_re2c.c : %.re2c
	re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<
	cd ./$(<D); dos2unix -q $(*F)_re2c.c

autoloads : force
	$(EMACS_EXE) -Q -batch --eval "(progn (require 'autoload)(setq generated-autoload-file (expand-file-name \"../autoloads.el\"))(update-directory-autoloads \"../\"))"

# load path rationale:
#    . for run-*.el
#    GPR_MODE_DIR = "-L .. -L $(WISI) -l "autoloads.el"" for developing ada-mode
#    GPR_MODE_DIR = "" for testing installed ELPA package
%.tmp : %
	$(EMACS_EXE) -Q -L . $(GPR_MODE_DIR) -l $(RUNTEST) --eval '(progn $(ELISP)(run-test "$<")(kill-emacs))' $(RUN_ARGS)

%.info : %.texi
	makeinfo $< -o ../$@

%.html : %.texi
	makeinfo --html --no-split $< -o ../$@

# for recompiling with release options
recursive-clean : force
	gprclean -r -P ../ada_mode_wisi_parse.gpr

clean :: generate-clean compile-ada-test-clean doc-clean elisp-clean exe-clean source-clean test-clean profile-clean
	rm -f check.gpr Makefile.conf

doc-clean ::
	rm -f ../*.info ../*.html ../dir

exe-clean ::
	rm -rf ../obj
	rm -rf ../gpr_mode_wisi_parse$(EXE_EXT)
	rm -rf ../run_gpr_parse$(EXE_EXT)

# delete all files created by wisitoken-bnf-generate for main programs
generate-clean :
	cd ..; rm -f *.parse_table *.re2c *_process*.ad? *_re2c_c.ads *_re2c.c *-process.el

test-clean ::
	rm -f *.diff *.tmp
	rm -f *.log *.stamp

source-clean :: test-clean
	-find ../ -name "*~" -print -delete
	-find ../ -name ".#*" -print -delete

# end of file
