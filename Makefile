# Build elisp code, Ada executables, manuals; publish to web and ELPA
#
# The gprbuild commands depend on the GPR_PROJECT_PATH environment
# variable, that is set in wisitoken_grammar.prj loaded in the file local
# variables below.

#export Standard_Common_Build := Debug

export WISITOKEN_GRAMMAR_VERSION := 0.1

ELPA_ROOT ?= $(shell cd ../elpa; pwd)
EMACS_EXE ?= emacs

elisp : update-elisp test

pub : pub-wisi-grammar build-elpa uninstall-elpa

update-elisp : build_ada_executables
update-elisp : autoloads
update-elisp : byte-compile

update-install : update-elisp install_ada_executables

test : test-wisitoken_grammar.stamp

ONE_TEST_FILE := ada.wy
one-clean : force
	for file in $(ONE_TEST_FILE) ; do rm -f $$file.* ; done
one : one-clean
one : build_ada_executables
one : RUNTEST := run-indent-test-grammar.el
one : $(ONE_TEST_FILE).diff

#two : RUN_ARGS ?= --verbosity 2 --cost_limit 5
#two : RUN_ARGS ?= --repeat_count 5
#two : RUN_LOG := > debug.log
two : build_ada_executables
	./run_wisitoken_grammar_parse.exe wisitoken_grammar_1.wy Indent $(RUN_ARGS) $(RUN_LOG)

%.re2c : %.wy $(WISITOKEN)/wisitoken-bnf-generate.exe
	$(WISITOKEN)/wisitoken-bnf-generate.exe $(<F)
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
	$(EMACS_EXE) -Q -batch --eval '(progn (setq vc-handled-backends nil)(let ((generated-autoload-file (expand-file-name "autoloads.el")))(update-directory-autoloads ".")))'


# WISITOKEN is correct for Stephe's development machines;
# it can be overridden on the 'make' command line or by an
# external environment variable.
ifeq ($(shell uname),Linux)
export WISITOKEN ?= /Projects/org.wisitoken/build
export EMACS_WISI ?= /Projects/org.emacs.ada-mode.stephe-2

else ifeq ($(shell uname),Darwin)
export WISITOKEN ?= /home/Projects/wisitoken/org.wisitoken/build
export EMACS_WISI ?= /Projects/org.emacs.ada-mode.stephe-2
else
# windows
export WISITOKEN ?= c:/Projects/org.wisitoken/build
export EMACS_WISI ?= c:/Projects/org.emacs.ada-mode.stephe-2

endif

$(WISITOKEN)/wisitoken-bnf-generate.exe : force
	$(MAKE) -C $(WISITOKEN) wisitoken-bnf-generate.exe

vpath %.wy ../org.emacs.ada-mode.stephe-2 ../org.wisitoken/wisi/test/ ../org.emacs.java-wisi/source/

TEST_FILES := wisitoken_grammar_1.wy
TEST_FILES += ada.wy
TEST_FILES += java.wy
TEST_FILES += gpr.wy
TEST_FILES += ada_lite.wy
TEST_FILES += character_literal.wy
TEST_FILES += identifier_list_name_conflict.wy

%.diff : % %.tmp
	diff -u $< $*.tmp > $*.diff

%.tmp : %
	$(EMACS_EXE) -Q -L . -L $(EMACS_WISI) -L $(EMACS_WISI)/build -l $(RUNTEST) --eval '(progn (run-test "$<")(kill-emacs))'

test-wisitoken_grammar : RUNTEST := run-indent-test-grammar.el
test-wisitoken_grammar : $(addsuffix .diff, $(TEST_FILES))

test-clean : force
	rm -f *.diff *.tmp *.log

test-wisitoken_grammar.stamp : test-clean
	$(MAKE) test-wisitoken_grammar
	touch $@
	find . -name "*.diff" -not -size 0 >> test.log

build_ada_executables : wisitoken_grammar_1_re2c.c force
	gprbuild -p wisitoken_grammar.gpr

install_ada_executables : build_ada_executables
	gprinstall -f -p -P wisitoken_grammar.gpr --install-name=wisitoken_grammar_wisi_parse

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
  TAR_FILE := org.wisitoken.grammar-$(WISITOKEN_GRAMMAR_VERSION).tar.gz
  TAR_DIR := .
  TAR_PAT := org.wisitoken.grammar-$(WISITOKEN_GRAMMAR_VERSION)
else
  TAR_FILE := $(BRANCH).tar.gz
  TAR_DIR := .
  TAR_PAT := $(BRANCH)
endif

zip :
	tar zcf $(TAR_FILE) --exclude _MTN --exclude "autoloads.el" --exclude "gpr_query.db*" --exclude "*~" --exclude "*.diff" --exclude "*.elc" --exclude "*.exe" --exclude "obj" --exclude "*.stamp" --exclude "*.tar.gz"  --exclude "*.tmp" -C $(TAR_DIR) $(TAR_PAT)

### ELPA stuff
ELPA_ROOT ?= $(shell cd ../../elpa; pwd)

pub-wisi-grammar : force
	mkdir -p $(ELPA_ROOT)/packages/wisitoken-grammar-mode
	rm -rf $(ELPA_ROOT)/packages/wisitoken-grammar-mode/*
	cp wisitoken-grammar*.el $(ELPA_ROOT)/packages/wisitoken-grammar-mode
	cp *wisitoken_grammar*.ad? $(ELPA_ROOT)/packages/wisitoken-grammar-mode
	cp wisitoken_grammar.gpr wisitoken_grammar_1.wy wisitoken_grammar_1_re2c.c $(ELPA_ROOT)/packages/wisitoken-grammar-mode
	cp build.sh $(ELPA_ROOT)/packages/wisitoken-grammar-mode/

build-elpa : force
	rm -rf $(ELPA_ROOT)/archive
	rm -rf $(ELPA_ROOT)/archive-tmp
	mkdir -p $(ELPA_ROOT)/archive-tmp/packages
	cp -a $(ELPA_ROOT)/packages/wisi                   $(ELPA_ROOT)/archive-tmp/packages
	cp -a $(ELPA_ROOT)/packages/wisitoken-grammar-mode $(ELPA_ROOT)/archive-tmp/packages
	make -C $(ELPA_ROOT)/ process-archive


.PHONY : all force one one-clean
.PRECIOUS : %-process.el %.ads %.diff %.re2c %.tmp %_re2c.c

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "wisitoken_grammar.el"))
# end:
# end of file
