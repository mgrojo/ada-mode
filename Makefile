# Build elisp code, Ada executables, manuals; publish to web and ELPA
#
# The gprbuild commands depend on the GPR_PROJECT_PATH environment
# variable, that is set in wisi_grammar.prj loaded in the file local
# variables below.

export WISI_GRAMMAR_VERSION := 0.1

ELPA_ROOT ?= $(shell cd ../elpa; pwd)
EMACS_EXE ?= emacs

elisp : update-elisp test

pub : docs pub-wisi-grammar build-elpa uninstall-elpa

update-elisp : install_ada_executables
update-elisp : autoloads
update-elisp : byte-compile

test : test-wisi_grammar.stamp

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
two : export Standard_Common_Build := Debug
two : build_ada_executables
two : force
	./run_wisi_grammar_1_parse.exe wisi_grammar_1.wy Indent $(RUN_ARGS) $(RUN_LOG)

%_process.ads %.re2c : %.wy $(WISITOKEN)/wisi-generate.exe
	$(WISITOKEN)/wisi-generate.exe -v 1 --output_language Ada_Emacs --lexer re2c --interface process --enum $(<F) > $(*F).ada_parse_table
	dos2unix $(*F)_process.ads $(*F)_process.adb $(*F)-process.el $(*F).re2c

%_re2c.c : %.re2c
	$(RE2C_HOME)/bin/re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<


# wisi-grammar-elisp.el is in monotone, so this is all we need after a
# monotone update. Doing byte-compile-clean first avoids errors caused
# by loading new source on old .elc.
byte-compile : byte-compile-clean
	$(EMACS_EXE) -Q -batch -L . -L $(WISI) --eval '(progn (package-initialize)(batch-byte-compile))' *.el

byte-compile-clean :
	rm -f *.elc

autoloads : force
	$(EMACS_EXE) -Q -batch --eval '(progn (setq vc-handled-backends nil)(let ((generated-autoload-file (expand-file-name "autoloads.el")))(update-directory-autoloads ".")))'


# WISITOKEN is correct for Stephe's development machines;
# it can be overridden on the 'make' command line or by an
# external environment variable.
ifeq ($(shell uname),Linux)
export WISITOKEN ?= /Projects/org.wisitoken/build
export WISI ?= /Projects/org.emacs.ada-mode.stephe-2

else ifeq ($(shell uname),Darwin)
export WISITOKEN ?= /home/Projects/wisitoken/org.wisitoken/build
export WISI ?= /Projects/org.emacs.ada-mode.stephe-2
else
# windows
export WISITOKEN ?= c:/Projects/org.wisitoken/build
export WISI ?= c:/Projects/org.emacs.ada-mode.stephe-2

endif

$(WISITOKEN)/wisi-generate.exe : force
	$(MAKE) -C $(WISITOKEN) wisi-generate.exe

vpath %.wy ../org.emacs.ada-mode.stephe-2 ../org.wisitoken/wisi/test/ ../org.emacs.java-wisi/source/

TEST_FILES := wisi_grammar_1.wy
TEST_FILES += ada.wy
TEST_FILES += java.wy
TEST_FILES += gpr.wy
TEST_FILES += ada_lite.wy
TEST_FILES += character_literal.wy
TEST_FILES += identifier_list_name_conflict.wy

%.diff : % %.tmp
	diff -u $< $*.tmp > $*.diff

%.tmp : %
	$(EMACS_EXE) -batch -L . -L $(WISI) -L $(WISI)/build -l $(RUNTEST) --eval '(progn (run-test "$<")(kill-emacs))'

test-wisi_grammar : RUNTEST := run-indent-test-grammar.el
test-wisi_grammar : $(addsuffix .diff, $(TEST_FILES))

test-wisi_grammar.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-wisi_grammar
	touch $@
	find . -name "*.diff" -not -size 0 >> test.log

build_ada_executables : wisi_grammar_1_re2c.c wisi_grammar_1_process.ads force
	gprbuild -p wisi_grammar.gpr

install_ada_executables : build_ada_executables
	gprinstall -f -p -P wisi_grammar.gpr --install-name=wisi_grammar_wisi_parse

clean : byte-compile-clean exe-clean generate-clean source-clean
	rm -f autoloads.el

exe-clean :
	rm -rf obj
	rm -rf *.exe

# delete all files created by wisi-generate
generate-clean :
	rm -f *.*_parse_table *.re2c *_re2c.c *_re2c_c.ads *-process.el *_process.ad?

# delete all files created by Emacs as backups
source-clean :
	-find . -name "*~" -print -delete
	-find . -name ".#*" -print -delete

### tar, gzip stuff

BRANCH := $(notdir $(shell cd ..; pwd))

ifeq ($(BRANCH),org.wisitoken.grammar)
  TAR_FILE := org.wisitoken.grammar-$(WISI_GRAMMAR_VERSION).tar.gz
  TAR_DIR := .
  TAR_PAT := org.wisitoken.grammar-$(WISI_GRAMMAR_VERSION)
else
  TAR_FILE := $(BRANCH).tar.gz
  TAR_DIR := .
  TAR_PAT := $(BRANCH)
endif

zip :
	tar zcf $(TAR_FILE) --exclude _MTN --exclude "autoloads.el" --exclude "gpr_query.db*" --exclude "*~" --exclude "*.diff" --exclude "*.elc" --exclude "*.exe" --exclude "obj" --exclude "*.stamp" --exclude "*.tar.gz"  --exclude "*.tmp" -C $(TAR_DIR) $(TAR_PAT)

.PHONY : all force one one-clean
.PRECIOUS : %-process.el %.ads %.diff %.re2c %.tmp %_process.adb %_re2c.c

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "wisi_grammar.el"))
# end:
# end of file
