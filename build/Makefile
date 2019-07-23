# Build elisp code, Ada executables, manuals; publish to web and ELPA
#
# The gprbuild commands depend on the GPR_PROJECT_PATH environment
# variable, that is set in the project files loaded in the file local
# variables below.
#
# test ELPA packages from process-archive:
#    (setq package-archives (list (cons "test" "/Projects/elpa/archive/packages")))
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

export ADA_MODE_VERSION    := 6.1.1
export ADA_REF_MAN_VERSION := 2012.5
export WISI_VERSION        := 2.1.1

ELPA_ROOT ?= $(shell cd ../../elpa; pwd)

TEST_DIR ?= source
ifeq ($(TEST_DIR),source)
# test with mtn controlled source
ADA_MODE_DIR := -L .. -l autoloads.el
WISITOKEN_GENERATE := $(WISITOKEN)/build/wisitoken-bnf-generate.exe

else
# test with installed elpa package
ADA_MODE_DIR := -f package-initialize
WISITOKEN_GENERATE := ~/.emacs.d/elpa/wisi-2.1.1/wisitoken-bnf-generate.exe
endif

# compile with debug to get assertions for testing new code
# export Standard_Common_Build := Debug

elisp : byte-compile-clean compile-ada-test-clean update-elisp compile-ada-test test-clean test

pub : docs pub-ada pub-wisi pub-ada-ref-man build-elpa uninstall-elpa

docs : info html

# This updates elisp and related executables after any source change,
# without running the tests.
update-elisp : build_executables
update-elisp : autoloads
update-elisp : byte-compile

# This updates and installs everything after any source change, without running the
# tests or publishing to elpa
update-install : update-elisp
update-install : install
update-install : docs

install : install_gpr_query
install : install_executables

# *-elisp.el are in monotone, so this is all we need after a monotone
# update. Doing byte-compile-clean first avoids errors caused by
# loading new source on old .elc.
#
# 'package-initialize' is required for 'queue'.
byte-compile : byte-compile-clean
	cd ../; $(EMACS_EXE) -Q -batch -L . -L $(ELPA_ROOT)/packages/uniquify-files --eval '(progn (package-initialize)(batch-byte-compile))' *.el

byte-compile-clean :
	cd ..; rm -f *.elc

# IMPROVEME: test-wisi, test-elisp abort on failure; others accumulate failures in test.log
test : test-wisi
ifeq ($(TEST_DIR),source)
test : build_executables
endif
test : test-ada-elisp-gpr_query.stamp
test : test-ada-lalr-process-gpr_query.stamp
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

summarize :
	cat test.log

#ONE_TEST_FILES ?= $(shell cd ../test; ls ada_mode-recover*.ad[sb])
#ONE_TEST_FILES := $(filter-out ada_mode-recover_repair_1.adb, $(ONE_TEST_FILES))
ONE_TEST_FILES ?= ada_mode-refactor_object_method_to_method_object.ads
one :: build_executables
#one :: byte-compile
one :: one-clean
one-clean :: force
	for file in $(ONE_TEST_FILES) ; do rm -f $$file.* ; done
	rm -f test.log
one :: RUNTEST := run-indent-test-lalr-process-gpr_query.el
#one :: RUNTEST := run-indent-test-elisp-gpr_query.el
#one :: $(addsuffix .diff-run, $(ONE_TEST_FILES))
one :: $(addsuffix .diff, $(ONE_TEST_FILES))
#	find . -name "*.diff" -not -size 0 >> test.log
#one :: $(addsuffix .debug, $(ONE_TEST_FILES))

# With DOS line endings, use (1- (+ (position-bytes (point)) (line-number-at-pos (point))))
# to get begin, end pos.
# partial indent args: begin-byte send-end parse-end begin-char begin-line end-line begin-indent
two : RUN_ARGS ?= ../test/ada_mode-recover_29.adb Indent --verbosity 1 1 0 --enqueue_limit 120_000
two : build_executables
	../run_ada_lalr_parse.exe $(RUN_ARGS)
	../run_ada_lr1_parse.exe $(RUN_ARGS)

two_mem : export Standard_Common_Mem_Check := On
two_mem : build_executables
	../exec_mem/run_ada_lalr_parse.exe c:/tmp/regulation-iflight.adb Face
	gnatmem ../exec_mem/run_ada_lalr_parse.exe > regulation_iflight.memory

two_pro : export Standard_Common_Profile := On
two_pro : build_executables
	../exec_pro/run_ada_lalr_parse.exe ../test/ada_mode-recover_partial_02_lr1.adb Indent --enqueue_limit 120_000 --task_count 1
	gprof ../exec_pro/run_ada_lalr_parse.exe > recover_partial_02-lalr.profile

# WISITOKEN is correct for Stephe's development machines;
# it can be overridden on the 'make' command line or by an
# external environment variable.
#
# ADA_PROJECT_PATH is used by the ../test/*.ad? files to test Ada mode
# interaction with gpr project files.
ifeq ($(shell uname),Linux)
export WISITOKEN ?= /Projects/org.wisitoken
export ADA_PROJECT_PATH=../test/:../test/subdir

else ifeq ($(shell uname),Darwin)
export WISITOKEN ?= /home/Projects/wisitoken/org.wisitoken
export ADA_PROJECT_PATH=../test/:../test/subdir

else
# windows
export WISITOKEN ?= c:/Projects/org.wisitoken
export ADA_PROJECT_PATH=../test/;../test/subdir

INSTALL_BIN := d:/Apps/emacs-25.1/libexec/emacs/25.1/x86_64-w64-mingw32
endif

include rules.make

$(WISITOKEN)/build/wisitoken-bnf-generate.exe : force
	$(MAKE) -C $(WISITOKEN)/build wisitoken-bnf-generate.exe

update-test-log : force
	find . -name "*.diff" -not -size 0 >> test.log

test-ada-elisp-gpr_query : RUNTEST := run-indent-test-elisp-gpr_query.el
test-ada-elisp-gpr_query : $(addsuffix .diff, $(subst subdir/,,$(ADA_ELISP_TEST_FILES)))

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

BRANCH := $(notdir $(shell cd ..; pwd))

ifeq ($(BRANCH),org.emacs.ada-mode)
  TAR_FILE := org.emacs.ada-mode-$(ADA_MODE_VERSION)
else
  TAR_FILE := $(BRANCH)
endif

zip :
	rm -rf ../../$(TAR_FILE)
	mtn checkout --branch $(BRANCH) ../../$(TAR_FILE)
	tar jcf $(TAR_FILE).tar.bz2 --exclude _MTN -C ../.. $(TAR_FILE)


# tarball of installed ELPA packages, for sending to customers without
# good monotone access
DATE := $(shell date +%Y-%m-%d)
elpa-zip : docs pub-ada pub-wisi ada-mode-elpa-$(DATE).tar.gz

ada-mode-elpa-$(DATE).tar.gz :
	tar zcf ada-mode-elpa-$(DATE).tar.gz -C $(ELPA_ROOT)/packages ada-ref-man ada-mode wisi

tag :
	mtn tag h:org.emacs.ada-mode org.emacs.ada-mode-$(ADA_MODE_VERSION)

MANUALS := ada-mode gpr-mode

INFO_FILES := $(addsuffix .info, $(MANUALS))
info : ../dir-ada-mode
html : $(addsuffix .html, $(MANUALS))

../dir-ada-mode : $(INFO_FILES)
	for file in $(INFO_FILES); do install-info ../$$file ../dir-ada-mode; done

VPATH := ..

### Publish Gnu ELPA packages
# First delete all files in Gnu ELPA, so we catch files deleted here.
#
# Copy files to Gnu ELPA ada-mode package
# ada-project.el depends on not-yet-published elpa packages.
pub-ada : force
	mkdir -p $(ELPA_ROOT)/packages/ada-mode
	rm -rf $(ELPA_ROOT)/packages/ada-mode/*
	cp ../ada*.el ../ada-*.texi ../ada-*.info ../ada_license.text $(ELPA_ROOT)/packages/ada-mode
#	cp ../env-project.el $(ELPA_ROOT)/packages/ada-mode
	rm $(ELPA_ROOT)/packages/ada-mode/ada-project.el
	rm $(ELPA_ROOT)/packages/ada-mode/ada-ref-man.el
	cp ../build.sh $(ELPA_ROOT)/packages/ada-mode/
	cp ../dir-ada-mode $(ELPA_ROOT)/packages/ada-mode/dir
	cp ../NEWS-ada-mode.text $(ELPA_ROOT)/packages/ada-mode/NEWS
	cp ../README-ada-mode $(ELPA_ROOT)/packages/ada-mode/README
	cp ../gnat*.el $(ELPA_ROOT)/packages/ada-mode
	cp ../gpr*.el ../gpr-*.texi ../gpr-*.info $(ELPA_ROOT)/packages/ada-mode
	cp ../xref-ada.el $(ELPA_ROOT)/packages/ada-mode
	cp ../*.wy ../ada*.ad? ../gpr*.ad? ../wisi-ada.ad? ../wisi-gpr.ad? ../*parse.ad? $(ELPA_ROOT)/packages/ada-mode
	cp ../ada_lr1_parse_table.txt $(ELPA_ROOT)/packages/ada-mode
	gzip $(ELPA_ROOT)/packages/ada-mode/ada_lr1_parse_table.txt
	cp ../wisitoken-*-ada.ad? $(ELPA_ROOT)/packages/ada-mode
	rm -f $(ELPA_ROOT)/packages/ada-mode/gpr_query-process_refresh.adb
	cp ../*.c $(ELPA_ROOT)/packages/ada-mode
	cp ../*.gp $(ELPA_ROOT)/packages/ada-mode
	cp ../config.pragmas $(ELPA_ROOT)/packages/ada-mode

# copy files to ELPA ada-ref-man package
pub-ada-ref-man: ARM_INFO ?= c:/Projects/org.adaic.arm_form
pub-ada-ref-man : force
	cp ../ada-ref-man.el $(ELPA_ROOT)/packages/ada-ref-man/
	cp ../README-ada-ref-man $(ELPA_ROOT)/packages/ada-ref-man/README
	cp $(ARM_INFO)/NEWS $(ELPA_ROOT)/packages/ada-ref-man
	cp $(ARM_INFO)/build/dir $(ELPA_ROOT)/packages/ada-ref-man
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
	cp ../wisi.ad? $(ELPA_ROOT)/packages/wisi
	cp ../wisitoken-parse_table-mode.el $(ELPA_ROOT)/packages/wisi
	cp $(WISITOKEN)/Docs/dir $(ELPA_ROOT)/packages/wisi/
	cp $(WISITOKEN)/Docs/*.info $(ELPA_ROOT)/packages/wisi/
	cp $(WISITOKEN)/*.ad? $(ELPA_ROOT)/packages/wisi
	rm $(ELPA_ROOT)/packages/wisi/wisitoken-regexp.ad?
	cp $(WISITOKEN)/wisitoken_grammar_re2c.c  $(ELPA_ROOT)/packages/wisi
	cp $(WISITOKEN)/build/wisitoken-elpa.gpr  $(ELPA_ROOT)/packages/wisi/wisitoken.gpr
	cp ../../org.stephe_leake.makerules/standard_common.gpr $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/long_float_elementary_functions.ads $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-ada_containers* $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_array_image.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_vectors.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_queues.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_vectors-gen_sorted.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_vectors-gen_image*.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_definite_doubly_linked_lists_sorted.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_definite_doubly_linked_lists_sorted-gen_image.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_graphs.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_indefinite_doubly_linked_lists.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_trimmed_image.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_min_heaps_fibonacci.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_queues-gen_image_aux.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_queues.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_red_black_trees.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_stacks.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_stacks-gen_image_aux.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_vectors.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_vectors-gen_image*.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-generic_decimal_image.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal.ad? $(ELPA_ROOT)/packages/wisi
# for wisitoken-bnf-generate:
	cp ../build-wisitoken-bnf-generate.sh $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_vectors-gen_comparable.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_definite_doubly_linked_lists.ad?  $(ELPA_ROOT)/packages/wisi

# 'make -C elpa all-in-place' does not build archive-contents, just *.elc
#
# Other packages are often broken, so just build ours, by only copying
# ours into archive-tmp. Packages at version 0 are deleted by
# process-archive.
build-elpa : force
	rm -rf $(ELPA_ROOT)/archive
	rm -rf $(ELPA_ROOT)/archive-tmp
	mkdir -p $(ELPA_ROOT)/archive-tmp/packages
	cp -a $(ELPA_ROOT)/packages/ada-mode $(ELPA_ROOT)/archive-tmp/packages
	cp -a $(ELPA_ROOT)/packages/ada-ref-man $(ELPA_ROOT)/archive-tmp/packages
#	cp -a $(ELPA_ROOT)/packages/uniqufy-files $(ELPA_ROOT)/archive-tmp/packages
	cp -a $(ELPA_ROOT)/packages/wisi     $(ELPA_ROOT)/archive-tmp/packages
	make -C $(ELPA_ROOT)/ process-archive

# (dvc-state-one "/Projects/elpa/")
# shell git push

uninstall-elpa :
	emacs -Q --eval '(progn (load-file "uninstall-elpa.el")(kill-emacs))'

# We don't kill emacs here, so we can check for compilation errors/warnings
install-elpa :
	emacs -Q --eval '(progn (load-file "install-elpa.el"))'

install-elpa-review :
	emacs -Q --eval '(load-file "install-elpa.el")'

## build gpr_query

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

../ada_mode_wisi_parse.gpr : ../ada_mode_wisi_parse.gpr.gp
	gnatprep -DHAVE_LIBADALANG=$(HAVE_LIBADALANG) $< $@

gpr_query$(EXE_EXT) : gpr_query-process_refresh.adb ../gpr_query.gpr force
	gprbuild -p ../gpr_query.gpr

trace :
	addr2line -e ../exec_pro/run_ada_parser.exe 0x79c027 0x7715ff3f 0x7715ff11 0x7716068d 0x758a42 0x402903 0x7afce0 0x4013db 0x74927c02 0x7717ad2d 0x7717acf8

$(INSTALL_BIN)gpr_query$(EXE_EXT) install_gpr_query : gpr_query$(EXE_EXT)
	gprinstall -f -p -P ../gpr_query.gpr --install-name=gpr_query

build_executables : ../ada_mode_wisi_parse.gpr ../gpr_re2c.c ../ada_re2c.c force
	gprbuild -p ../ada_mode_wisi_parse.gpr

install_executables : build_executables
	gprinstall -f -p -P ../ada_mode_wisi_parse.gpr --install-name=ada_mode_wisi_parse

# Load both Ada mode projects, so we can choose one via the menu
# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "ada_mode_wisi_parse.el"))
# end:
# end of file
