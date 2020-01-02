# Build elisp code, Ada executables, manuals; publish to web and ELPA
#
# The gprbuild commands depend on the GPR_PROJECT_PATH environment
# variable, that is set in wisitoken_grammar.prj loaded in the file local
# variables below.

#export Standard_Common_Build := Debug

export WISITOKEN_GRAMMAR_MODE_VERSION := 1.0.0
export WISI_VERSION                   := 2.1.1

EMACS_EXE ?= emacs -xrm Emacs.fontBackend:uniscribe

elisp : update-elisp test

pub : pub-wisitoken-grammar build-elpa uninstall-elpa

update-elisp :: build_executables
update-elisp :: autoloads
update-elisp :: byte-compile

update-install : update-elisp install

test : test-wisitoken_grammar.stamp

ONE_TEST_FILE := nominal.wy
one-clean :: force
	for file in $(ONE_TEST_FILE) ; do rm -f $$file.* ; done
one :: one-clean
one :: build_executables
one :: RUNTEST := run-indent-test-grammar.el
one :: $(ONE_TEST_FILE).diff

two :: RUN_ARGS ?= --verbosity 0 0 2
#two :: RUN_ARGS ?= --repeat_count 5
#two :: RUN_LOG := > debug.log
two :: build_executables
	./run_wisitoken_grammar_parse.exe test/nominal.wy Indent $(RUN_ARGS) $(RUN_LOG)

%.re2c : %.wy $(WISITOKEN)/build/wisitoken-bnf-generate.exe
	$(WISITOKEN)/build/wisitoken-bnf-generate.exe --output_bnf $(*F)_bnf.wy $(<F)
	dos2unix $(*F)_process_actions.ads $(*F)_process_actions.adb $(*F)-process.el
	dos2unix $(*F)_process_main.ads $(*F)_process_main.adb

%_re2c.c : %.re2c
	$(RE2C_HOME)/re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<
	dos2unix $@

# wisi-grammar-elisp.el is in monotone, so this is all we need after a
# monotone update. Doing byte-compile-clean first avoids errors caused
# by loading new source on old .elc.
byte-compile : byte-compile-clean
	$(EMACS_EXE) -Q -batch -L . -L $(EMACS_WISI) --eval "(progn (package-initialize)(batch-byte-compile))" *.el

byte-compile-clean :
	rm -f *.elc

autoloads : force
	$(EMACS_EXE) -Q -batch --eval "(progn (require 'autoload)(setq generated-autoload-file (expand-file-name \"autoloads.el\"))(update-directory-autoloads \".\"))"

# WISITOKEN is correct for Stephe's development machines;
# it can be overridden on the 'make' command line or by an
# external environment variable.
ifeq ($(shell uname),Linux)
export WISITOKEN ?= /Projects/org.wisitoken
export EMACS_WISI ?= /Projects/org.emacs.ada-mode.stephe-2

else ifeq ($(shell uname),Darwin)
export WISITOKEN ?= /home/Projects/wisitoken/org.wisitoken
export EMACS_WISI ?= /Projects/org.emacs.ada-mode.stephe-2
else
# windows
export WISITOKEN ?= c:/Projects/org.wisitoken
export EMACS_WISI ?= c:/Projects/org.emacs.ada-mode.stephe-2

endif

$(WISITOKEN)/build/wisitoken-bnf-generate.exe : force
	$(MAKE) -C $(WISITOKEN)/build wisitoken-bnf-generate.exe

vpath %.wy test

TEST_FILES := $(shell cd test; ls *.wy)

%.diff : % %.tmp
	-diff -u $< $*.tmp > $*.diff

%.tmp : %
	$(EMACS_EXE) -Q -L . -L $(EMACS_WISI) -l $(RUNTEST) --eval '(progn (run-test "$<")(kill-emacs))'

test-wisitoken_grammar : RUNTEST := run-indent-test-grammar.el
test-wisitoken_grammar : $(addsuffix .diff, $(TEST_FILES))

test-wisitoken_grammar.stamp : test-clean
	$(MAKE) test-wisitoken_grammar
	touch $@
	find . -name "*.diff" -not -size 0 >> test.log

test-clean : force
	rm -f *.diff *.tmp *.log

build_executables : wisitoken_grammar_1_re2c.c wisitoken_grammar.gpr force
	gprbuild -p wisitoken_grammar.gpr

install : build_executables
	gprinstall -f -p -P wisitoken_grammar.gpr --install-name=wisitoken_grammar_wisi_parse

wisitoken_grammar.gpr : wisitoken_grammar.gpr.gp
	gnatprep -DELPA="no" $< $@

clean : byte-compile-clean exe-clean generate-clean source-clean test-clean
	rm -f autoloads.el

exe-clean :
	rm -rf obj
	rm -rf *.exe

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

### tar, gzip stuff

BRANCH := $(notdir $(shell cd ..; pwd))

ifeq ($(BRANCH),org.wisitoken.grammar)
  TAR_FILE := org.wisitoken.grammar-$(WISITOKEN_GRAMMAR_MODE_VERSION).tar.gz
  TAR_DIR := .
  TAR_PAT := org.wisitoken.grammar-$(WISITOKEN_GRAMMAR_MODE_VERSION)
else
  TAR_FILE := $(BRANCH).tar.gz
  TAR_DIR := .
  TAR_PAT := $(BRANCH)
endif

zip :
	tar zcf $(TAR_FILE) --exclude _MTN --exclude "autoloads.el" --exclude "gpr_query.db*" --exclude "*~" --exclude "*.diff" --exclude "*.elc" --exclude "*.exe" --exclude "obj" --exclude "*.stamp" --exclude "*.tar.gz"  --exclude "*.tmp" -C $(TAR_DIR) $(TAR_PAT)

### ELPA stuff
ELPA_ROOT ?= $(shell cd ../elpa; pwd)
ELPA_EXTERNALS ?= $(shell cd ../elpa-externals; pwd)
ELPA_WGM := $(ELPA_EXTERNALS)/wisitoken-grammar-mode

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
