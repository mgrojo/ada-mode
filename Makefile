# Build elisp code, Ada executables, manuals; publish to web and ELPA
#
# The gprbuild commands depend on the GPR_PROJECT_PATH environment
# variable, that is set in wisi_grammar.prj loaded in the file local
# variables below.

export WISI_GRAMMAR_VERSION := 0.1

ELPA_ROOT ?= $(shell cd ../elpa; pwd)
EMACS_EXE ?= emacs

pub : docs pub-wisi-grammar build-elpa uninstall-elpa

update-elisp : autoloads
update-elisp : install_ada_executables
update-elisp : byte-compile

%_process.ads %.re2c : %.wy $(WISITOKEN)/wisi-generate.exe
	$(WISITOKEN)/wisi-generate.exe -v 1 --output_language Ada_Emacs --lexer re2c --interface process --enum $(<F) > $(*F).ada_parse_table
	dos2unix $(*F)_process.ads $(*F)_process.adb $(*F)-process.el $(*F).re2c

%_re2c.c : %.re2c
	$(RE2C_HOME)/bin/re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<


# wisi-grammar-elisp.el is in monotone, so this is all we need after a
# monotone update. Doing byte-compile-clean first avoids errors caused
# by loading new source on old .elc.
byte-compile : byte-compile-clean
	$(EMACS_EXE) -Q -batch -L $(WISI) --eval '(progn (package-initialize)(batch-byte-compile))' *.el

byte-compile-clean :
	rm -f *.elc

autoloads : force
	$(EMACS_EXE) -Q -batch --eval '(progn (setq vc-handled-backends nil)(let ((generated-autoload-file (expand-file-name "autoloads.el")))(update-directory-autoloads ".")))'

two : RUN_ARGS ?= --verbosity 2
#two : RUN_ARGS ?= --repeat_count 5
two : export Standard_Common_Build := Debug
two : build_ada_executables
	run_wisi_grammar_parse.exe wisi_grammar.wy Indent $(RUN_ARGS)

# WISITOKEN is correct for Stephe's development machines;
# it can be overridden on the 'make' command line or by an
# external environment variable.
ifeq ($(shell uname),Linux)
export WISITOKEN ?= /Projects/org.wisitoken/build

else ifeq ($(shell uname),Darwin)
export WISITOKEN ?= /home/Projects/wisitoken/org.wisitoken/build
export WISI ?= c:/Projects/org.emacs.ada-mode.stephe-2
else
# windows
export WISITOKEN ?= /Projects/org.wisitoken/build

endif

$(WISITOKEN)/wisi-generate.exe : force
	$(MAKE) -C $(WISITOKEN) wisi-generate.exe

test-wisi_grammar-elisp : RUNTEST := run-indent-test-elisp.el
test-wisi_grammar-elisp : $(addsuffix .diff, $(TEST_FILES))

test-wisi_grammar-elisp.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-wisi_grammar-elisp
	touch $@
	find . -name "*.diff" -not -size 0 >> test.log

build_ada_executables : wisi_grammar_re2c.c wisi_grammar_process.ads force
	gprbuild -p wisi_grammar.gpr

install_ada_executables : build_ada_executables
	gprinstall -f -p -P wisi_grammar.gpr --install-name=wisi_grammar_wisi_parse

clean : byte-compile-clean exe-clean generate-clean source-clean test-clean
	rm -f autoloads.el

exe-clean :
	rm -rf obj
	rm -rf *.exe

# delete all files created by wisi-generate
generate-clean :
	rm -f *.*_parse_table *.re2c *_re2c.c *_re2c_c.ads *-elisp.el *-process.el *_process.ad?

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

.PHONY : all force
.PRECIOUS : %-process.el %.ads %.re2c %.tmp %_process.adb %_re2c.c

# Local Variables:
# eval: (unless (getenv "WISITOKEN") (setenv "WISITOKEN" "c:/Projects/org.wisitoken/build"))
# eval: (unless (getenv "WISI") (setenv "WISI" "c:/Projects/org.emacs.ada-mode.stephe-2"))
# eval: (unless dvc-doing-ediff-p (load-file "wisi_grammar.el"))
# end:
# end of file
