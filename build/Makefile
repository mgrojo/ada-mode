# Build elisp code, Ada executables, manuals; publish to web and ELPA

GPR_MODE_VERSION = 1.0.0

TEST_DIR ?= source

ELPA_ROOT ?= $(shell cd ../../elpa; pwd)

# emacs to test with
#
# This can be overridden on the 'make' command line or by an external
# environment variable.
ifeq ($(shell uname),Linux)
EMACS_EXE ?= emacs
else ifeq ($(shell uname),Darwin)
EMACS_EXE ?= "/Applications/Emacs.app/Contents/MacOS/Emacs"
else
# windows
# specify uniscribe to workaround weird Windows harfbuzz bug
EMACS_EXE ?= emacs -xrm Emacs.fontBackend:uniscribe
endif

# compile with debug with default Standard_Common_Assertions to get
# assertions for testing new code, or to allow running debugger.
# assertions off for speed
# export Standard_Common_Build := Debug
# export Standard_Common_Assertions := Off

all : byte-compile-clean update test-clean test docs

# 'pub' does not do install, because 'build-elpa' uses the last
# commit, not the current workspace.
pub : pub-gpr
pub-install: build-elpa install-elpa

docs : info html

# This updates elisp and related executables after any source change,
# without running the tests.
update : build_executables
update : autoloads
update : byte-compile

# This updates and installs everything after any source change, without running the
# tests or publishing to elpa
update-install : update
update-install : install
update-install : docs

install : install_executables

# Doing byte-compile-clean first avoids errors caused by
# loading new source on old .elc.
byte-compile : byte-compile-clean
	$(MAKE) -C $(WISI)/build byte-compile
	cd ../; $(EMACS_EXE) -Q -batch -L . -L $(WISI) -l build/exclude-elpa.el --eval '(progn (batch-byte-compile))' *.el
#(setq byte-compile-error-on-warn t)

byte-compile-clean :
	cd ..; rm -f *.elc

ifeq ($(TEST_DIR),source)
test : build_executables
endif
test : gpr-skel.gpr.diff
test : test-gpr-incremental-process.stamp
test : summarize

summarize :
	cat test.log

ONE_TEST_FILE ?= debug.adb
one :: ELISP ?= (setq-default wisi-parser-verbosity "debug=1" save-parser-log "../debug-1.log" save-edited-text "../debug_edited-1")
one :: IGNORE_CONFLICTS := --generate LALR Ada_Emacs Process re2c
one :: build_executables
one :: byte-compile autoloads
one :: one-clean
one-clean :: force
	for file in $(ONE_TEST_FILE) ; do rm -f $$file.* ; done
one :: RUNTEST := run-indent-test.el
one :: $(addsuffix .diff, $(ONE_TEST_FILE))

one-debug :: RUNTEST := run-indent-test.el
one-debug :: force
	$(EMACS_EXE) -Q -L . $(ADA_MODE_DIR) -l $(RUNTEST) --eval '(progn $(ELISP))'

two : RUN_ARGS ?= command_file debug.cmd
two : LOG_ARGS ?= > debug.log 2>&1
#two : RUN_ARGS ?= parse_partial face test/debug.adb --verbosity "debug=1 mckenzie=1 parse=1" --mckenzie_enqueue_limit 60000 --lang_params "3 2 0 1 0 0 2 0 2 0 2 0 0" > debug.log
two : IGNORE_CONFLICTS := --generate LALR Ada_Emacs Process re2c
two : ../run_ada_lalr_parse.exe
	cd ../; ./run_ada_lalr_parse.exe $(RUN_ARGS) $(LOG_ARGS)

two-lr1 : RUN_ARGS ?= parse_partial face test/debug.adb --verbosity "debug=1 mckenzie=1 parse=1" --mckenzie_enqueue_limit 60000 --lang_params "3 2 0 1 0 0 2 0 2 0 2 0 0"
two-lr1 : ../run_ada_lr1_parse.exe
	cd ../; ./run_ada_lr1_parse.exe $(RUN_ARGS)

two-gpr : ../run_gpr_parse.exe
	cd ../; ./run_gpr_parse.exe $(RUN_ARGS)

two_mem : export Standard_Common_Mem_Check := On
two_mem : build_executables
	../exec_mem/run_ada_lalr_parse.exe c:/tmp/regulation-iflight.adb Face
	gnatmem ../exec_mem/run_ada_lalr_parse.exe > regulation_iflight.memory

two_pro : export Standard_Common_Profile := On
two_pro : build_executables
	../exec_pro/run_ada_lalr_parse.exe ../test/ada_mode-recover_partial_02_lr1.adb Indent --enqueue_limit 120_000 --task_count 1
	gprof ../exec_pro/run_ada_lalr_parse.exe > recover_partial_02-lalr.profile

three : RUN_ARGS := c:/Eurocontrol 10
three : run_libadalang.exe
	cd ..; ./run_libadalang.exe $(RUN_ARGS)

# WISITOKEN is correct for Stephe's development machines;
# it can be overridden on the 'make' command line or by an
# external environment variable.
#
# ADA_PROJECT_PATH is used by the ../test/*.ad? files to test Ada mode
# interaction with gpr project files.
ifeq ($(shell uname),Linux)
export WISITOKEN ?= /Projects/org.wisitoken
export WISI      ?= /Projects/org.emacs.wisi
export ADA_PROJECT_PATH=../test/:../test/subdir

else ifeq ($(shell uname),Darwin)
export WISITOKEN ?= /home/Projects/wisitoken/org.wisitoken
export WISI      ?= /home/Projects/org.emacs.wisi
export ADA_PROJECT_PATH=../test/:../test/subdir

else
# windows
export WISITOKEN ?= c:/Projects/org.wisitoken
export WISI      ?= c:/Projects/org.emacs.wisi
export ADA_PROJECT_PATH=../test/;../test/subdir

INSTALL_BIN := d:/Apps/emacs-25.1/libexec/emacs/25.1/x86_64-w64-mingw32
endif

# WISITOKEN_GENERATE must be after WISITOKEN
ifeq ($(TEST_DIR),source)
# test with development source
ADA_MODE_DIR := -L .. -L $(WISI) -l autoloads.el -l build/exclude-elpa.el
WISITOKEN_GENERATE := $(WISITOKEN)/build/wisitoken-bnf-generate.exe

else
# test with installed elpa package; don't rebuild grammars
ADA_MODE_DIR :=
WISITOKEN_GENERATE :=
endif

# 'include' must be after WISITOKEN_GENERATE and other vars are set.
include rules.make

update-test-log : force
	find . -maxdepth 1 -name "*.diff" -not -size 0 >> test.log

test-gpr-incremental-process : RUNTEST := run-indent-test.el
test-gpr-incremental-process : $(addsuffix .diff, $(subst subdir/,,$(GPR_TEST_FILES)))

test-gpr-incremental-process.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-gpr-incremental-process
	touch $@
	echo "test-gpr-incremental-process" >> test.log
	find . -name "*.diff" -not -size 0 >> test.log

ifeq ($(shell uname),Linux)
EXE_EXT :=

GNAT_EXE    := $(shell which gnat)
INSTALL_BIN := $(dir $(GNAT_EXE))

else ifeq ($(shell uname),Darwin)
EXE_EXT :=

GNAT_EXE    := $(shell which gnat)
INSTALL_BIN := $(dir $(GNAT_EXE))

else
# windows
EXE_EXT := .exe

# make can't see 'type', so we use 'which'
GNAT_EXE    := $(shell cygpath --mixed $(shell which gnat))
INSTALL_BIN := $(dir $(GNAT_EXE))

endif

BRANCH := $(notdir $(shell cd ..; pwd))

ifeq ($(BRANCH),org.emacs.ada-mode)
  TAR_FILE := org.emacs.gpr-mode-$(GPR_MODE_VERSION)
else
  TAR_FILE := $(BRANCH)
endif

zip :
	rm -rf ../../$(TAR_FILE)
	mtn checkout --branch $(BRANCH) ../../$(TAR_FILE)
	tar jcf $(TAR_FILE).tar.bz2 --exclude _MTN -C ../.. $(TAR_FILE)

VPATH := ..

### Publish Gnu ELPA package
# First delete all files in Gnu ELPA, so we catch files deleted here.
pub-gpr : force
	mkdir -p $(ELPA_ROOT)/packages/gpr-mode
	rm -rf $(ELPA_ROOT)/packages/gpr-mode/*
	cp ../*.el ../*.texi $(ELPA_ROOT)/packages/gpr-mode
	cp ../build.sh ../install.sh $(ELPA_ROOT)/packages/gpr-mode/
	cp ../Makefile ../NEWS ../README $(ELPA_ROOT)/packages/gpr-mode/
	cp ../*.wy ../gpr*.ad? ../wisi-gpr.ad?  $(ELPA_ROOT)/packages/gpr-mode
	cd $(ELPA_ROOT)/packages/gpr-mode; rm autoloads.el
	cp ../*.c $(ELPA_ROOT)/packages/gpr-mode

# builds $(ELPA_ROOT)/archive-devel/*, from the last commit, _not_ the
# current workspace Also checks copyright; run elpa/GNUMakefile
# check/<pkg> first if added files.
build-elpa : force
	rm -rf $(ELPA_ROOT)/archive-devel
	make -C $(ELPA_ROOT)/ build/gpr-mode build/wisi

uninstall-elpa :
	emacs -Q --eval '(progn (load-file "uninstall-elpa.el")(kill-emacs))'

# We don't kill emacs here, so we can check for compilation errors/warnings
install-elpa :
	emacs -Q --eval '(load-file "install-elpa.el")'

$(WISI)/wisi.gpr : force
	$(MAKE) -C $(WISI)/build ../wisi.gpr

%$(EXE_EXT) : %.adb force
	cd ..; gprbuild -p -j8 -P gpr_mode_wisi_parse.gpr $*.adb

build_executables : $(WISI)/wisi.gpr ../gpr_re2c.c force
	echo $$GPR_PROJECT_PATH
	gprbuild -p -j8 ../gpr_mode_wisi_parse.gpr

install_executables : build_executables
	gprinstall -f -p -P ../gpr_mode_wisi_parse.gpr --install-name=gpr_mode_wisi_parse

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# end:
# end of file
