# Build elisp code, Ada executables, manuals; publish to web and ELPA
#
# The gprbuild commands depend on the GPR_PROJECT_PATH environment
# variable, that is set in wisitoken_grammar.prj loaded in the file local
# variables below.

export Standard_Common_Build := Debug
export MMM_MODE ?= c:/Projects/mmm-mode

export WISITOKEN_GRAMMAR_MODE_VERSION := 1.2.0

EMACS_EXE ?= emacs -xrm Emacs.fontBackend:uniscribe

elisp : update test

pub : pub-wisitoken-grammar build-elpa uninstall-elpa

update :: build_executables
update :: autoloads
update :: byte-compile

update-install : update install

test : test-wisitoken_grammar.stamp

ONE_TEST_FILE ?= debug.wy
one :: ELISP ?= (setq-default wisi-parser-verbosity "debug=1" save-parser-log "../debug-1.log" save-edited-text "../debug_edited-1")
one-clean :: force
	for file in $(ONE_TEST_FILE) ; do rm -f $$file.* ; done
one :: one-clean
one :: build_executables
one :: byte-compile
one :: RUNTEST := run-indent-test-grammar.el
one :: $(ONE_TEST_FILE).diff

one-debug :: RUNTEST := run-indent-test-grammar.el
one-debug :: force
	$(EMACS_EXE) -Q -L . -L $(WISI) -l exclude-elpa.el -L $(MMM_MODE) -l $(RUNTEST) --eval '(progn $(ELISP))'

two :: RUN_ARGS ?= command_file debug.cmd > debug.log 2>&1
two :: build_executables
	./run_wisitoken_grammar_parse.exe $(RUN_ARGS)

%.re2c : %.wy $(WISITOKEN)/build/wisitoken-bnf-generate.exe
	$(WISITOKEN)/build/wisitoken-bnf-generate.exe --output_bnf $(<F)
	dos2unix $(*F)_process_actions.ads $(*F)_process_actions.adb $(*F)-process.el
	dos2unix $(*F)_process_main.ads $(*F)_process_main.adb

%_re2c.c : %.re2c
	re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<
	dos2unix $@

# Files generated by wisitoken-bnf-generate are in CM, so this
# is all we need after a CM update. Doing byte-compile-clean
# first avoids errors caused by loading new source on old .elc.
byte-compile : byte-compile-clean
	$(MAKE) -C $(WISI)/build byte-compile
	$(EMACS_EXE) -Q -batch -L . -L $(WISI) -l exclude-elpa.el -L $(MMM_MODE) --eval "(progn (package-initialize)(batch-byte-compile))" *.el

byte-compile-clean :
	rm -f *.elc

autoloads : force
	$(EMACS_EXE) -Q -batch --eval "(progn (require 'autoload)(setq generated-autoload-file (expand-file-name \"autoloads.el\"))(update-directory-autoloads \".\"))"

$(WISITOKEN)/build/wisitoken-bnf-generate.exe : force
	$(MAKE) -C $(WISITOKEN)/build wisitoken-bnf-generate.exe

vpath %.wy test

TEST_FILES := $(shell cd test; ls *.wy)

%.diff : % %.tmp
	-diff -u $< $*.tmp > $*.diff

%.tmp : %
	$(EMACS_EXE) --debug-init -Q -L . -L $(WISI) -l exclude-elpa.el -L $(MMM_MODE) -l $(RUNTEST) --eval '(progn $(ELISP)(run-test "$<")(kill-emacs))'

%.debug : %
	$(EMACS_EXE) -Q -L . -L $(WISI) -l exclude-elpa.el -L $(MMM_MODE) -l $(RUNTEST) --eval '(progn (package-initialize)(setq debug-on-error t))' $<

test-wisitoken_grammar : RUNTEST := run-indent-test-grammar.el
test-wisitoken_grammar : $(addsuffix .diff, $(TEST_FILES))

test-wisitoken_grammar.stamp : test-clean
	$(MAKE) test-wisitoken_grammar
	touch $@
	find . -name "*.diff" -not -size 0 >> test.log

test-clean : force
	rm -f *.diff *.tmp *.log

$(WISI)/wisi.gpr : force
	$(MAKE) -C $(WISI)/build ../wisi.gpr

build_executables : $(WISI)/wisi.gpr wisitoken_grammar_1_re2c.c wisitoken_grammar.gpr force
	gprbuild -p wisitoken_grammar.gpr

install : build_executables
	gprinstall -f -p -P wisitoken_grammar.gpr --install-name=wisitoken_grammar_wisi_parse

wisitoken_grammar.gpr : wisitoken_grammar.gpr.gp
	gnatprep -DELPA="no" $< $@

clean : byte-compile-clean exe-clean generate-clean source-clean test-clean
	rm -f autoloads.el

exe-clean :
	rm -rf obj
	rm -rf bin

# delete all files created by wisitoken-bnf-generate
generate-clean :
	rm -f *.parse_table *.re2c *_re2c.c *_re2c_c.ads *-process.el *_process*.ad?

# delete all files created by Emacs as backups
source-clean :
	-find . -name "*~" -print -delete
	-find . -name ".#*" -print -delete

# for recompiling with release options
recursive-clean : force
	gprclean -r -P wisitoken_grammar.gpr

### ELPA stuff
ELPA_ROOT ?= $(shell cd ../elpa; pwd)
ELPA_WGM := $(ELPA_ROOT)/packages/wisitoken-grammar-mode

pub-wisitoken-grammar : wisitoken_grammar.gpr force
	mkdir -p $(ELPA_WGM)
	rm -f $(ELPA_WGM)/*
	cp wisitoken-grammar-mode.el $(ELPA_WGM)
	cp wisitoken_grammar_1-process.el $(ELPA_WGM)
	cp *wisitoken_grammar*.ad? $(ELPA_WGM)
	cp wisitoken_grammar.gpr.gp wisitoken_grammar_1.wy wisitoken_grammar_1_re2c.c $(ELPA_WGM)
	cp build.sh $(ELPA_WGM)

# assume wisi built, installed (from ada-mode Makefile)
build-elpa : force
	rm -rf $(ELPA_ROOT)/archive
	rm -rf $(ELPA_ROOT)/archive-tmp
	mkdir -p $(ELPA_ROOT)/archive-tmp/packages
	cp -a $(ELPA_WGM) $(ELPA_ROOT)/archive-tmp/packages
	make -C $(ELPA_ROOT)/ process-archive

# For testing ELPA build with ada-mode Makefile
copy-archive : force
	cp -a $(ELPA_WGM) $(ELPA_ROOT)/archive-tmp/packages

uninstall-elpa :
	$(EMACS_EXE) -Q --eval '(progn (load-file "uninstall-elpa.el")(kill-emacs))'

# We don't kill emacs here, so we can check for compilation errors/warnings
install-elpa :
	$(EMACS_EXE) -Q --eval '(load-file "install-elpa.el")'

.PHONY : all force one one-clean
.PRECIOUS : %-process.el %.ads %.diff %.re2c %.tmp %_re2c.c

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# end:
# end of file
