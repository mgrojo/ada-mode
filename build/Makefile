# Build elisp code, Ada executables, manuals; publish to web and ELPA
#
# The gprbuild commands depend on the GPR_PROJECT_PATH environment
# variable, that is set in the project files loaded in the file local
# variables below.
#
# test ELPA packages from process-archive:
#    (setq package-archives (list (cons "test" "/Projects/elpa/archive/packages")))
# test ELPA packages from all-in-place: (setq package-archives (list (cons "test" "/Projects/elpa/packages")))
#
# Ideally we'd have 5 elpa packages with dependencies:
#
# ada-mode  => gnat-core, wisi, gpr-query
# gpr-mode  => gnat-core, wisi
# gnat-core =>
# gpr_query => gnat-core
# wisi 	    =>
#
# but currently the code is not that clean; gpr-mode and gpr-query
# require ada-mode. And it's not worth making gnat-core a package.

export ADA_MODE_VERSION    := 5.3.1
export ADA_REF_MAN_VERSION := 2012.3
export WISI_VERSION        := 1.1.6

ELPA_ROOT ?= $(shell cd ../../elpa; pwd)

# test with mtn controlled source:  (setenv "ADA_MODE_DIR" "-L .. -l autoloads.el")
# test with installed elpa package: (setenv "ADA_MODE_DIR" "-f package-initialize")
ADA_MODE_DIR ?= -L .. -l autoloads.el

# compile with debug to get assertions for testing new code
# export Standard_Common_Build := Debug

elisp : byte-compile-clean compile-ada-test-clean update-elisp compile-ada-test test-clean test

pub : docs pub-ada pub-wisi pub-ada-ref-man build-elpa uninstall-elpa

docs : info html

# This updates elisp and related executables after any source change,
# without running the tests.
update-elisp : autoloads
update-elisp : build_ada_executables
update-elisp : byte-compile

# This updates and installs everything after any source change, without running the
# tests or publishing to elpa
update-install : update-elisp
update-install : install
update-install : docs

install : install_gpr_query
install : install_ada_executables
ifeq ($(GNAT_VERSION),GPL_2016)
# GPS source code copied from GPS GPL 2016; not compatible with gnatcoll GPL 2017
install : install_ada_mode_gps_indent
endif

# *-elisp.el are in monotone, so this is all we need after a monotone
# update. Doing byte-compile-clean first avoids errors caused by
# loading new source on old .elc.
#
# 'package-initialize' is required for 'queue'.
byte-compile : byte-compile-clean
	cd ../; $(EMACS_EXE) -Q -batch -L . --eval '(progn (package-initialize)(batch-byte-compile))' *.el

byte-compile-clean :
	cd ..; rm -f *.elc

# IMPROVEME: test-wisi, test-elisp abort on failure; others accumulate failures in test.log
test : test-wisi
test : build_ada_executables
test : ada_unit_tests
test : test-ada-elisp-gpr_query.stamp
test : test-ada-lalr-process-gpr_query.stamp
#test : test-ada-packrat-process-gpr_query.stamp
ifeq ($(GNAT_VERSION),GPL_2016)
test : test-ada-gps.stamp
endif
test : gpr-skel.gpr.diff
test : test-gpr-elisp.stamp
test : test-gpr-lalr-process.stamp
test : test-elisp
test : summarize

test-wisi :: ada-number-literal.wisi-test
test-wisi :: body_instantiation_conflict.wisi-test
test-wisi :: case_expression.wisi-test
test-wisi :: empty_production_6.wisi-test
test-wisi :: empty_production_8.wisi-test
test-wisi :: identifier_list_name_conflict.wisi-test
test-wisi :: number-literal.wisi-test
test-wisi :: range_conflict.wisi-test
test-wisi :: force
	echo "test-wisi" >> test.log
	find . -name "*.diff" -not -size 0 >> test.log

summarize :
	cat test.log

ONE_TEST_FILES ?= $(shell cd ../test; ls ada_mode-recover*.ad[sb])
#ONE_TEST_FILES ?= format_paramlist.ads format_paramlist.adb
one : build_ada_executables
#one : byte-compile
one : one-clean
one-clean: force
	for file in $(ONE_TEST_FILES) ; do rm -f $$file.* ; done
one : RUNTEST := run-indent-test-lalr-process-gpr_query.el
#one : RUNTEST := run-indent-test-elisp-gpr_query.el
#one : $(addsuffix .diff-run, $(ONE_TEST_FILES))
one : $(addsuffix .diff, $(ONE_TEST_FILES))

#two : RUN_ARGS ?= --verbosity 2 2 > debug.log
#two : RUN_ARGS ?= --repeat_count 5
two : build_ada_executables
	../run_ada_lalr_parse.exe $(RUN_ARGS)

two_mem : export Standard_Common_Mem_Check := On
two_mem : build_ada_executables
	../exec_mem/run_ada_parse.exe ../test/slow_recover_1.adb Face
	gnatmem ../exec_pro/run_ada_parse.exe > slow_recover_1.memory

two_pro : export Standard_Common_Profile := On
two_pro : build_ada_executables
	../exec_pro/run_ada_parse.exe ../test/ada_mode-recover_bad_char.adb Indent --repeat_count 5
	gprof ../exec_pro/run_ada_parse.exe > ada_mode-recover_bad_char.profile

# WISI_WISITOKEN is correct for Stephe's development machines;
# it can be overridden on the 'make' command line or by an
# external environment variable.
#
# ADA_PROJECT_PATH is used by the ../test/*.ad? files to test Ada mode
# interaction with gpr project files.
ifeq ($(shell uname),Linux)
export WISI_WISITOKEN ?= /Projects/org.wisitoken/build
export ADA_PROJECT_PATH=../test/:../test/subdir

else ifeq ($(shell uname),Darwin)
export WISI_WISITOKEN ?= /home/Projects/wisitoken/org.wisitoken/build
export ADA_PROJECT_PATH=../test/:../test/subdir

else
# windows
export WISI_WISITOKEN ?= c:/Projects/org.wisitoken/build
export ADA_PROJECT_PATH=../test/;../test/subdir

INSTALL_BIN := d:/Apps/emacs-25.1/libexec/emacs/25.1/x86_64-w64-mingw32
endif

include rules.make

$(WISI_WISITOKEN)/wisitoken-bnf-generate.exe : force
	$(MAKE) -C $(WISI_WISITOKEN) wisitoken-bnf-generate.exe

test-ada-elisp-gpr_query : RUNTEST := run-indent-test-elisp-gpr_query.el
test-ada-elisp-gpr_query : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-ada-elisp-gpr_query.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-elisp-gpr_query
	touch $@
	echo "test-ada-elisp-gpr_query" >> test.log
	find . -name "*.diff" -not -size 0 >> test.log

test-ada-lalr-process-gpr_query : RUNTEST := run-indent-test-lalr-process-gpr_query.el
test-ada-lalr-process-gpr_query : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-ada-lalr-process-gpr_query.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-lalr-process-gpr_query
	touch $@
	echo "test-ada-lalr-process-gpr_query" >> test.log
	find . -name "*.diff" -not -size 0 >> test.log

test-ada-packrat-process-gpr_query : RUNTEST := run-indent-test-packrat-process-gpr_query.el
test-ada-packrat-process-gpr_query : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-ada-packrat-process-gpr_query.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-packrat-process-gpr_query
	touch $@
	echo "test-ada-packrat-process-gpr_query" >> test.log
	find . -name "*.diff" -not -size 0 >> test.log

# ada-gps specific tests
test-ada-gps : RUNTEST := run-indent-test-gps.el
test-ada-gps : $(addsuffix .diff, $(ADA_GPS_TEST_FILES))

test-ada-gps.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-gps
	touch $@
	echo "test-ada-gps" >> test.log
	find . -name "*.diff" -not -size 0 >> test.log

test-gpr-elisp : RUNTEST := run-indent-test-elisp-gpr_query.el
test-gpr-elisp : $(addsuffix .diff, $(subst subdir/,,$(GPR_TEST_FILES)))

test-gpr-elisp.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-gpr-elisp
	touch $@
	echo "test-gpr-elisp" >> test.log
	find . -name "*.diff" -not -size 0 >> test.log

test-gpr-lalr-process : RUNTEST := run-indent-test-lalr-process-gpr_query.el
test-gpr-lalr-process : $(addsuffix .diff, $(subst subdir/,,$(GPR_TEST_FILES)))

test-gpr-lalr-process.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-gpr-lalr-process
	touch $@
	echo "test-gpr-lalr-process" >> test.log
	find . -name "*.diff" -not -size 0 >> test.log

# compare gps indent to ada-wisi indent
compare-ada-gps : RUNTEST := run-indent-test-gps.el
compare-ada-gps : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

compare-ada-gps.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-gps
	touch $@
	echo "compare-ada-gps" >> test.log
	find . -name "*.diff" -not -size 0 >> test.log

BRANCH := $(notdir $(shell cd ..; pwd))

ifeq ($(BRANCH),org.emacs.ada-mode)
  TAR_FILE := org.emacs.ada-mode-$(ADA_MODE_VERSION).tar.gz
  TAR_DIR := ../
  TAR_PAT := org.emacs.ada-mode-$(ADA_MODE_VERSION)
else
  TAR_FILE := $(BRANCH).tar.gz
  TAR_DIR := ../
  TAR_PAT := $(BRANCH)
endif

DATE := $(shell date +%Y-%m-%d)
# tarball of installed ELPA packages, for sending to customers without good monotone access
elpa-zip : docs pub-ada pub-wisi build-elpa install-elpa ada-mode-elpa-$(DATE).tar.gz

ada-mode-elpa-$(DATE).tar.gz :
	tar zcf ada-mode-elpa-$(DATE).tar.gz -C ~/.emacs.d/elpa ada-ref-man-$(ADA_REF_MAN_VERSION) ada-mode-$(ADA_MODE_VERSION) wisi-$(WISI_VERSION)

zip :
	tar zcf $(TAR_FILE) --exclude _MTN --exclude "autoloads.el" --exclude "gpr_query.db*" --exclude "*~" --exclude "*.ali" --exclude "*.diff" --exclude "*.elc" --exclude "*.exe" --exclude "obj" --exclude "*.output" --exclude "*.stamp" --exclude "*.tar.gz"  --exclude "*.tmp" --exclude "*.wisi-test" -C $(TAR_DIR) $(TAR_PAT)


MANUALS := ada-mode gpr-mode

INFO_FILES := $(addsuffix .info, $(MANUALS))
info : ../dir-ada-mode
html : $(addsuffix .html, $(MANUALS))

../dir-ada-mode : $(INFO_FILES)
	for file in $(INFO_FILES); do install-info ../$$file ../dir-ada-mode; done

VPATH := ..
# copy files to ELPA ada-mode package
# First delete all files in ELPA, so we catch files deleted here.
pub-ada : force
	rm -rf $(ELPA_ROOT)/archive
	mkdir -p $(ELPA_ROOT)/packages/ada-mode
	rm -rf $(ELPA_ROOT)/packages/ada-mode/*
	cp ../ada*.el ../ada-*.texi ../ada-*.info ../ada_license.text $(ELPA_ROOT)/packages/ada-mode
	rm $(ELPA_ROOT)/packages/ada-mode/ada-ref-man.el
	cp ../build.sh $(ELPA_ROOT)/packages/ada-mode/
	cp ../dir-ada-mode $(ELPA_ROOT)/packages/ada-mode/dir
	cp ../NEWS-ada-mode.text $(ELPA_ROOT)/packages/ada-mode/NEWS
	cp ../README-ada-mode $(ELPA_ROOT)/packages/ada-mode/README
	cp ../gnat*.el $(ELPA_ROOT)/packages/ada-mode
	cp ../gpr*.el ../gpr-*.texi ../gpr-*.info $(ELPA_ROOT)/packages/ada-mode
	cp ../xref-ada.el $(ELPA_ROOT)/packages/ada-mode
	cp ../*.adb ../*.gpr $(ELPA_ROOT)/packages/ada-mode
	cp ../*.gp $(ELPA_ROOT)/packages/ada-mode
	rm -f $(ELPA_ROOT)/packages/ada-mode/gpr_query-process_refresh.adb
	rm -f $(ELPA_ROOT)/packages/ada-mode/gpr_query.gpr
	mkdir -p $(ELPA_ROOT)/packages/ada-mode/gps_source
	cp ../gps_source/*.ad? $(ELPA_ROOT)/packages/ada-mode/gps_source

# copy files to ELPA ada-ref-man package
pub-ada-ref-man: ARM_INFO ?= c:/Projects/org.adaic.arm_form
pub-ada-ref-man : force
	cp ../ada-ref-man.el $(ELPA_ROOT)/packages/ada-ref-man/
	cp ../README-ada-ref-man $(ELPA_ROOT)/packages/ada-ref-man/README
	cp ../dir-ada-ref-man $(ELPA_ROOT)/packages/ada-ref-man/dir
	cp $(ARM_INFO)/build/arm2012.info $(ELPA_ROOT)/packages/ada-ref-man
	cp $(ARM_INFO)/build/aarm2012.info $(ELPA_ROOT)/packages/ada-ref-man
	cp $(ARM_INFO)/source_2012/* $(ELPA_ROOT)/packages/ada-ref-man/source_2012
	cp $(ARM_INFO)/progs/* $(ELPA_ROOT)/packages/ada-ref-man/progs
	cp $(ARM_INFO)/build/Makefile $(ELPA_ROOT)/packages/ada-ref-man/build
	cp $(ARM_INFO)/build/arm_info.gpr $(ELPA_ROOT)/packages/ada-ref-man/build
	cp $(ARM_INFO)/build/arm_info.prj $(ELPA_ROOT)/packages/ada-ref-man/build
	cp $(ARM_INFO)/build/download.py $(ELPA_ROOT)/packages/ada-ref-man/build
	cp $(ARM_INFO)/build/emacs_case_exceptions $(ELPA_ROOT)/packages/ada-ref-man/build

# copy files to ELPA wisi package
pub-wisi : force
	rm -f $(ELPA_ROOT)/packages/wisi/*
	cp ../README-wisi $(ELPA_ROOT)/packages/wisi/README
	cp ../NEWS-wisi.text $(ELPA_ROOT)/packages/wisi/NEWS
	cp ../wisi*.el $(ELPA_ROOT)/packages/wisi
	cp ../parse_table-mode.el $(ELPA_ROOT)/packages/wisi

# 'make -C elpa all-in-place' does not build archive-contents, just *.elc
#
# Other packages are often broken, so just build ours, by only copying
# ours into archive-tmp
build-elpa : force
	rm -rf $(ELPA_ROOT)/archive-tmp
	mkdir -p $(ELPA_ROOT)/archive-tmp/packages
	cp -a $(ELPA_ROOT)/packages/ada-mode $(ELPA_ROOT)/archive-tmp/packages
	cp -a $(ELPA_ROOT)/packages/ada-ref-man $(ELPA_ROOT)/archive-tmp/packages
	cp -a $(ELPA_ROOT)/packages/wisi     $(ELPA_ROOT)/archive-tmp/packages
	make -C $(ELPA_ROOT)/ process-archive

# (setq package-archives (list (cons "test" "/Projects/elpa/archive/packages")))
# (list-packages)

# (dvc-state-one "/Projects/elpa/")
# shell git push

uninstall-elpa :
	emacs -Q --eval '(progn (load-file "uninstall-elpa.el")(kill-emacs))'

# package install from public doesn't work in batch
install-elpa :
	emacs -Q --eval '(progn (load-file "install-elpa.el")(kill-emacs))'

install-elpa-review :
	emacs -Q --eval '(load-file "install-elpa.el")'

## build gpr_query, ada_mode_gps_indent

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

# for debugging:
#export Gnatcoll_Build := Debug
# export Gpr_Query_Build := Debug
#export Ada_Mode_GPS_Indent_Build := Debug
#export BUILD := Debug

# In December 2016, GNATCOLL changed its Xref interface. First, the
# GPR was split out; and second, one of the subprogram
# interfaces.changed.

# Determine whether the split-out gnatcoll_xref.gpr is available.
# We specify $(SHELL) here to work around a Mingw64 'make' problem.
Makefile.conf : create_makefile_conf.sh
	$(SHELL) -c ./create_makefile_conf.sh

include Makefile.conf

../gpr_query-process_refresh.adb : ../gpr_query-process_refresh.adb.gp
	gnatprep -DHAVE_GNATCOLL_XREF=$(HAVE_GNATCOLL_XREF) $< $@

../gpr_query.gpr : ../gpr_query.gpr.gp
	gnatprep -DHAVE_GNATCOLL_XREF=$(HAVE_GNATCOLL_XREF) $< $@

../ada_mode_gps_indent.gpr : ../ada_mode_gps_indent.gpr.gp
	gnatprep -DHAVE_GNATCOLL_XREF=$(HAVE_GNATCOLL_XREF) $< $@

../ada_mode_wisi_parse.gpr : ../ada_mode_wisi_parse.gpr.gp
	gnatprep -DHAVE_LIBADALANG=$(HAVE_LIBADALANG) $< $@

gpr_query$(EXE_EXT) : gpr_query-process_refresh.adb ../gpr_query.gpr force
	gprbuild -p ../gpr_query.gpr

# (setq ada-gps-exec (concat default-directory "ada_mode_gps_indent"))
ada_mode_gps_indent$(EXE_EXT) : ../ada_mode_gps_indent.gpr force
	gprbuild -p ../ada_mode_gps_indent.gpr

debug_gps_indent$(EXE_EXT) : force
	gprbuild -p ../ada_mode_gps_indent.gpr debug_gps_indent

trace :
	addr2line -e ../exec_pro/run_ada_parser.exe 0x79c027 0x7715ff3f 0x7715ff11 0x7716068d 0x758a42 0x402903 0x7afce0 0x4013db 0x74927c02 0x7717ad2d 0x7717acf8

$(INSTALL_BIN)gpr_query$(EXE_EXT) install_gpr_query : gpr_query$(EXE_EXT)
	gprinstall -f -p -P ../gpr_query.gpr --install-name=gpr_query

$(INSTALL_BIN)ada_mode_gps_indent$(EXE_EXT) install_ada_mode_gps_indent : ada_mode_gps_indent$(EXE_EXT)
	gprinstall -f -p -P ../ada_mode_gps_indent.gpr --install-name=ada_mode_gps_indent

build_ada_executables : ../ada_mode_wisi_parse.gpr ../gpr_re2c.c ../ada_re2c.c force
	gprbuild -p ../ada_mode_wisi_parse.gpr

install_ada_executables : build_ada_executables
	gprinstall -f -p -P ../ada_mode_wisi_parse.gpr --install-name=ada_mode_wisi_parse

$(INSTALL_BIN)run_ada_mode_parser$(EXE_EXT) install_run_ada_mode_parser : run_ada_mode_parser$(EXE_EXT)
	gprinstall -f -p -P ../run_ada_mode_parser.gpr --install-name=run_ada_mode_parser

$(INSTALL_BIN)gpr_mode_wisi_parse$(EXE_EXT) install_gpr_mode_wisi_parse : gpr_mode_wisi_parse$(EXE_EXT)
	gprinstall -f -p -P ../ada_mode_wisi_parse.gpr --install-name=gpr_mode_wisi_parse

$(INSTALL_BIN)run_gpr_mode_parser$(EXE_EXT) install_run_gpr_mode_parser : run_gpr_mode_parser$(EXE_EXT)
	gprinstall -f -p -P ../run_ada_mode_parser.gpr --install-name=run_gpr_mode_parser

test_harness.exe : force
	gprbuild -p ../ada_mode_unit_tests.gpr $(GPRBUILD_ARGS)

ada_unit_tests : build_ada_executables test_harness.exe
	../test_harness.exe $(RUN_ARGS)

# only needed for editing, not compiling/running.
# (load-file "ada_mode_unit_tests.el")
# (project-menu-select-by-name "ada_mode_wisi_parse main")
# (project-menu-select-by-name "ada_mode_unit_tests main")

# Load both Ada mode projects, so we can choose one via the menu
# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "ada_mode_wisi_parse.el"))
# end:
# end of file
