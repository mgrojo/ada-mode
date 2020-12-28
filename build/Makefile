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

export ADA_MODE_VERSION       := 7.1.3
export ADA_REF_MAN_VERSION    := 2012.5
export WISI_VERSION           := 3.1.2
export UNIQUIFY_FILES_VERSION := 1.0.2

ELPA_ROOT ?= $(shell cd ../../elpa; pwd)

TEST_DIR ?= source

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

# compile with debug to get assertions for testing new code
#export Standard_Common_Build := Debug
#export Gnatcoll_Build := Debug

elisp : byte-compile-clean compile-ada-test-clean update compile-ada-test test-clean test

pub : docs pub-ada pub-wisi pub-ada-ref-man clean-elpa build-elpa uninstall-elpa

docs : info html

recover-compare : wisitoken-ada-recover-compare.stamp
recover-compare : libadalang-recover-compare.stamp
recover-compare : wisitoken-ada_lite-recover-compare.stamp
recover-compare : tree_sitter-ada_lite-recover-compare.stamp
recover-compare : compare_lengths.stamp

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

# *-elisp.el are in monotone, so this is all we need after a monotone
# update. Doing byte-compile-clean first avoids errors caused by
# loading new source on old .elc.
#
# 'package-initialize' is required for 'uniquify-files'.
byte-compile : byte-compile-clean
	cd ../; $(EMACS_EXE) -Q -batch -L . --eval '(progn (package-initialize)(setq byte-compile-error-on-warn t)(batch-byte-compile))' *.el

byte-compile-clean :
	cd ..; rm -f *.elc

ifeq ($(TEST_DIR),source)
test : build_executables
endif
test : test-ada-lalr-process-gpr_query.stamp
test : test-ada-lalr-process-gnatxref.stamp
test : gpr-skel.gpr.diff
test : test-gpr-lalr-process.stamp
test : summarize

summarize :
	cat test.log

#ONE_TEST_FILES ?= $(shell cd ../test; ls ada_mode-recover*.ad[sb])
#ONE_TEST_FILES := $(filter-out ada_mode-recover%, $(ADA_TEST_FILES))
ONE_TEST_FILES ?= ada_mode-nominal.adb
#one :: build_executables
#one :: byte-compile autoloads
#one :: compile-ada-test
one :: one-clean
one-clean :: force
	for file in $(ONE_TEST_FILES) ; do rm -f $$file.* ; done
#	rm -f ../test/gpr_query.db*
one :: RUNTEST := run-indent-test-lalr-process-gpr_query.el
#one :: RUNTEST := run-indent-test-lalr-process-gnatxref.el
#one :: $(addsuffix .diff-run, $(ONE_TEST_FILES))
one :: $(addsuffix .diff, $(ONE_TEST_FILES))
#one :: $(addsuffix .debug, $(ONE_TEST_FILES))

# With DOS line endings, use (1- (+ (position-bytes (point)) (line-number-at-pos (point))))
# to get begin, end pos.
# partial indent args: begin-byte send-end parse-end begin-char begin-line end-line begin-indent
two : RUN_ARGS ?= parse indent ../test/ada_mode-recover_41.adb --verbosity "mckenzie=1"
two : IGNORE_CONFLICTS := --generate LALR Ada_Emacs Process re2c
two : ../ada_annex_p_re2c.c ../run_ada_lalr_parse.exe
	../run_ada_lalr_parse.exe $(RUN_ARGS)

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

# WISITOKEN_GENERATE must be after WISITOKEN
ifeq ($(TEST_DIR),source)
# test with mtn controlled source
ADA_MODE_DIR := -L .. -l autoloads.el
WISITOKEN_GENERATE := $(WISITOKEN)/build/wisitoken-bnf-generate.exe

else
# test with installed elpa package; don't rebuild grammars
ADA_MODE_DIR := -f package-initialize
WISITOKEN_GENERATE :=
endif

# 'include' must be after WISITOKEN_GENERATE and other vars are set.
include rules.make

update-test-log : force
	find . -name "*.diff" -not -size 0 >> test.log

test-ada-lalr-process-gnatxref : RUNTEST := run-indent-test-lalr-process-gnatxref.el
test-ada-lalr-process-gnatxref : $(addsuffix .diff, $(subst subdir/,,$(GNATXREF_TEST_FILES)))

test-ada-lalr-process-gnatxref.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-lalr-process-gnatxref
	touch $@
	echo "test-ada-lalr-process-gnatxref" >> test.log
	find . -name "*.diff" -not -size 0 >> test.log

test-ada-lalr-process-gpr_query : RUNTEST := run-indent-test-lalr-process-gpr_query.el
test-ada-lalr-process-gpr_query : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-ada-lalr-process-gpr_query.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-lalr-process-gpr_query
	touch $@
	echo "test-ada-lalr-process-gpr_query" >> test.log
	find . -name "*.diff" -not -size 0 >> test.log

test-gpr-lalr-process : RUNTEST := run-indent-test-lalr-process-gpr_query.el
test-gpr-lalr-process : $(addsuffix .diff, $(subst subdir/,,$(GPR_TEST_FILES)))

test-gpr-lalr-process.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-gpr-lalr-process
	touch $@
	echo "test-gpr-lalr-process" >> test.log
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
  TAR_FILE := org.emacs.ada-mode-$(ADA_MODE_VERSION)
else
  TAR_FILE := $(BRANCH)
endif

wisitoken-ada-recover-compare.stamp : ada-correct-tokens.stamp wisitoken-tokens.stamp
	-for file in $(RECOVER_TEST_FILES) ; do diff -u ada-correct-tokens/$$file.tokens wisitoken-tokens/$$file.tokens > wisitoken-tokens/$$file.diff ; done
	touch $@

wisitoken-ada_lite-recover-compare.stamp : ada_lite-correct-tokens.stamp  wisitoken-ada_lite-tokens.stamp force
	-for file in $(RECOVER_TEST_FILES) ; do if test -f ../test/ada_lite/$$file ; then diff -u ada_lite-correct-tokens/$$file.tokens wisitoken-ada_lite-tokens/$$file.tokens > wisitoken-ada_lite-tokens/$$file.diff ; fi ; done
	touch $@

tree_sitter-ada_lite-recover-compare.stamp : ada_lite-correct-tokens.stamp  tree_sitter-ada_lite-tokens.stamp force
	-for file in $(RECOVER_TEST_FILES) ; do if test -f ../test/ada_lite/$$file ; then diff -u ada_lite-correct-tokens/$$file.tokens tree_sitter-ada_lite-tokens/$$file.tokens > tree_sitter-ada_lite-tokens/$$file.diff ; fi ; done
	touch $@

libadalang-recover-compare.stamp : ada-correct-tokens.stamp  libadalang-tokens force
	-for file in $(RECOVER_TEST_FILES) ; do diff -u ada-correct-tokens/$$file.tokens libadalang-tokens/$$file.tokens > libadalang-tokens/$$file.diff ; done
	touch $@

wisitoken-libadalang-diff : wisitoken-tokens.stamp libadalang-tokens.stamp
	rm -f *.diff
	-for file in $(RECOVER_TEST_FILES) ; do diff -u wisitoken-tokens/$$file.tokens libadalang-tokens/$$file.tokens > $$file.diff ; done

compare_lengths.log : compare_lengths$(EXE_EXT) sum_diff_lengths$(EXE_EXT)
	cd wisitoken-tokens; ../../sum_diff_lengths > ../compare_lengths.log
	cd libadalang-tokens; ../../sum_diff_lengths >> ../compare_lengths.log
	cd wisitoken-ada_lite-tokens; ../../sum_diff_lengths >> ../compare_lengths.log
	cd tree_sitter-ada_lite-tokens; ../../sum_diff_lengths >> ../compare_lengths.log
	../compare_lengths wisitoken-tokens libadalang-tokens >> compare_lengths.log
	../compare_lengths wisitoken-ada_lite-tokens tree_sitter-ada_lite-tokens >> compare_lengths.log

# default to LR1 for error correction
DUMP_ALG ?= LR1

wisitoken-tokens.stamp : ../dump_wisitoken_corrected$(EXE_EXT)
	mkdir -p wisitoken-tokens
	for file in $(RECOVER_TEST_FILES) ; do ../dump_wisitoken_corrected $(DUMP_ALG)  ../test/$$file > wisitoken-tokens/$$file.tokens ; done
	touch $@

wisitoken-ada_lite-tokens.stamp : $(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected.exe
	mkdir -p wisitoken-ada_lite-tokens
	for file in $(RECOVER_TEST_FILES) ; do if test -f ../test/ada_lite/$$file ; then $(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected $(DUMP_ALG) ../test/ada_lite/$$file > wisitoken-ada_lite-tokens/$$file.tokens ; fi ; done
	touch $@

$(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected.exe : force
	$(MAKE) -C $(WISITOKEN)/build dump_wisitoken_ada_lite_corrected.exe

wisitoken-ada_lite-tokens/%.tokens : ../test/ada_lite/% $(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected.exe force
	$(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected.exe LR1 $< $(VERBOSITY) > $@

wisitoken-ada_lite-tokens/%.diff : wisitoken-ada_lite-tokens/%.tokens ada_lite-correct-tokens/%.tokens
	-diff -u ada_lite-correct-tokens/$*.tokens wisitoken-ada_lite-tokens/$*.tokens > $@

tree_sitter-ada_lite-tokens.stamp : $(WISITOKEN)/build/dump_tree_sitter_ada_lite_corrected.exe
	mkdir -p tree_sitter-ada_lite-tokens
	for file in $(RECOVER_TEST_FILES) ; do if test -f ../test/ada_lite/$$file ; then $(WISITOKEN)/build/dump_tree_sitter_ada_lite_corrected.exe ../test/ada_lite/$$file > tree_sitter-ada_lite-tokens/$$file.tokens ; fi ; done
	touch $@

tree_sitter-ada_lite-tokens/%.tokens : ../test/ada_lite/% $(WISITOKEN)/build/dump_tree_sitter_ada_lite_corrected.exe force
	$(WISITOKEN)/build/dump_tree_sitter_ada_lite_corrected.exe LR1 $< $(VERBOSITY) > $@

tree_sitter-ada_lite-tokens/%.diff : tree_sitter-ada_lite-tokens/%.tokens ada_lite-correct-tokens/%.tokens
	-diff -u ada_lite-correct-tokens/$*.tokens tree_sitter-ada_lite-tokens/$*.tokens > $@

$(WISITOKEN)/build/dump_tree_sitter_ada_lite_corrected.exe : force
	$(MAKE) -C $(WISITOKEN)/build dump_tree_sitter_ada_lite_corrected.exe

libadalang-tokens : ../dump_libadalang_corrected$(EXE_EXT)
	mkdir -p libadalang-tokens
	for file in $(RECOVER_TEST_FILES) ; do ../dump_libadalang_corrected  ../test/$$file > libadalang-tokens/$$file.tokens ; done

ada-correct-tokens.stamp : ../dump_wisitoken_corrected$(EXE_EXT)
	mkdir -p ada-correct-tokens
	for file in $(RECOVER_TEST_FILES) ; do ../dump_wisitoken_corrected LALR ../test/correct/$$file > ada-correct-tokens/$$file.tokens ; done
	touch $@

ada_lite-correct-tokens.stamp : $(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected$(EXE_EXT)
	mkdir -p ada_lite-correct-tokens
	for file in $(RECOVER_TEST_FILES) ; do $(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected LALR ../test/ada_lite-correct/$$file > ada_lite-correct-tokens/$$file.tokens ; done
	touch $@

ada_lite-correct-tokens/%.tokens : ../test/ada_lite-correct/% $(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected$(EXE_EXT)
	$(WISITOKEN)/build/dump_wisitoken_ada_lite_corrected$(EXE_EXT) LR1 $< $(VERBOSITY) > $@

zip :
	rm -rf ../../$(TAR_FILE)
	mtn checkout --branch $(BRANCH) ../../$(TAR_FILE)
	tar jcf $(TAR_FILE).tar.bz2 --exclude _MTN -C ../.. $(TAR_FILE)


# tarball of ELPA packages source, for sending to customers with their
# own CM but without good network access
DATE := $(shell date +%Y-%m-%d)
elpa-zip : ada-mode-elpa-$(DATE).tar.gz

ada-mode-elpa-$(DATE).tar.gz :
	tar zcf ada-mode-elpa-$(DATE).tar.gz -C $(ELPA_ROOT)/packages ada-ref-man ada-mode wisi

# tarball to create local ELPA archive
elpa-archive-zip : ada-mode-elpa-archive-$(DATE).tar.bz2

ada-mode-elpa-archive-$(DATE).tar.bz2 : $(ELPA_ROOT)/archive/packages/archive-contents
	tar -c -C $(ELPA_ROOT) archive/packages | bzip2 -9 > ada-mode-elpa-archive-$(DATE).tar.bz2

tag :
	mtn tag h:org.emacs.ada-mode org.emacs.ada-mode-$(ADA_MODE_VERSION)

MANUALS := ada-mode gpr-mode wisi

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
	cp ../ada*.el ../ada-mode.texi ../ada-mode.info  ../ada_license.text $(ELPA_ROOT)/packages/ada-mode
	rm $(ELPA_ROOT)/packages/ada-mode/ada-ref-man.el
	cp ../build.sh ../install.sh $(ELPA_ROOT)/packages/ada-mode/
	cp ../dir-ada-mode $(ELPA_ROOT)/packages/ada-mode/dir
	cp ../NEWS-ada-mode.text $(ELPA_ROOT)/packages/ada-mode/NEWS
	cp ../README-ada-mode $(ELPA_ROOT)/packages/ada-mode/README
	cp ../gnat*.el $(ELPA_ROOT)/packages/ada-mode
	cp ../gpr*.el ../gpr-mode.texi ../gpr-mode.info $(ELPA_ROOT)/packages/ada-mode
	cp ../*.wy ../ada*.ad? ../gpr*.ad? ../run_ada*.ad? ../run_gpr*.ad? ../wisi-ada*.ad? ../wisi-gpr.ad?  $(ELPA_ROOT)/packages/ada-mode
	cp ../ada_lr1_parse_table.txt $(ELPA_ROOT)/packages/ada-mode
	gzip $(ELPA_ROOT)/packages/ada-mode/ada_lr1_parse_table.txt
	cp ../wisitoken-*-ada.ad? $(ELPA_ROOT)/packages/ada-mode
	cp ../*.c $(ELPA_ROOT)/packages/ada-mode
	cp ../*.gp $(ELPA_ROOT)/packages/ada-mode
	cp ../config.pragmas $(ELPA_ROOT)/packages/ada-mode

# copy files to ELPA ada-ref-man package
pub-ada-ref-man: ARM_INFO ?= c:/Projects/org.adaic.arm_form
pub-ada-ref-man : force
	rm -f $(ARM_INFO)/build/*2005.info
	make -C $(ARM_INFO)/build create-dir
	cp ../ada-ref-man.el $(ELPA_ROOT)/packages/ada-ref-man/
	cp ../README-ada-ref-man $(ELPA_ROOT)/packages/ada-ref-man/README
	cp $(ARM_INFO)/NEWS $(ELPA_ROOT)/packages/ada-ref-man
	cp $(ARM_INFO)/build/dir $(ELPA_ROOT)/packages/ada-ref-man
	cp $(ARM_INFO)/build/arm2012.info $(ELPA_ROOT)/packages/ada-ref-man
	cp $(ARM_INFO)/build/aarm2012.info $(ELPA_ROOT)/packages/ada-ref-man
	cp $(ARM_INFO)/source_2012/* $(ELPA_ROOT)/packages/ada-ref-man/source_2012
	cp $(ARM_INFO)/build/arm2020.info $(ELPA_ROOT)/packages/ada-ref-man
	cp $(ARM_INFO)/build/aarm2020.info $(ELPA_ROOT)/packages/ada-ref-man
	cp $(ARM_INFO)/source_2020/* $(ELPA_ROOT)/packages/ada-ref-man/source_2020
	cp $(ARM_INFO)/progs/* $(ELPA_ROOT)/packages/ada-ref-man/progs
	cp $(ARM_INFO)/build/Makefile $(ELPA_ROOT)/packages/ada-ref-man/build
	cp $(ARM_INFO)/build/arm_info.gpr $(ELPA_ROOT)/packages/ada-ref-man/build
	cp $(ARM_INFO)/build/arm_info.prj $(ELPA_ROOT)/packages/ada-ref-man/build
	cp $(ARM_INFO)/build/download.py $(ELPA_ROOT)/packages/ada-ref-man/build
	cp $(ARM_INFO)/build/emacs_case_exceptions $(ELPA_ROOT)/packages/ada-ref-man/build

# copy files to ELPA wisi package
pub-wisi : force
	make -C $(WISITOKEN)/build doc
	rm -f $(ELPA_ROOT)/packages/wisi/*
	cp ../README-wisi $(ELPA_ROOT)/packages/wisi/README
	cp ../NEWS-wisi.text $(ELPA_ROOT)/packages/wisi/NEWS
	cp ../gen_*parse.ad? ../*_wisi_common_parse.ad? $(ELPA_ROOT)/packages/wisi
	cp ../wisi*.el ../wisi.texi ../wisi.info $(ELPA_ROOT)/packages/wisi
	cp ../wisi.ad? $(ELPA_ROOT)/packages/wisi
	cp ../wisi.gpr.gp  $(ELPA_ROOT)/packages/wisi
	cp ../wisitoken-parse_table-mode.el $(ELPA_ROOT)/packages/wisi
	cp $(WISITOKEN)/Docs/dir $(ELPA_ROOT)/packages/wisi/
	cp $(WISITOKEN)/Docs/*.info $(ELPA_ROOT)/packages/wisi/
	cp $(WISITOKEN)/*.ad? $(ELPA_ROOT)/packages/wisi
	rm $(ELPA_ROOT)/packages/wisi/wisitoken-regexp.ad?
	cp $(WISITOKEN)/wisitoken_grammar_re2c.c  $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.makerules/standard_common.gpr $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/long_float_elementary_functions.ads $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-ada_containers* $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_array_image.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_queues.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_stacks*.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_vectors.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_vectors-gen_refs.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_vectors-gen_image*.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_vectors_sorted*.ad? $(ELPA_ROOT)/packages/wisi
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
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_vectors_sorted.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unconstrained_array_image.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unconstrained_array_image_aux.ad? $(ELPA_ROOT)/packages/wisi
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
clean-elpa :
	rm -rf $(ELPA_ROOT)/archive
	rm -rf $(ELPA_ROOT)/archive-tmp
	mkdir -p $(ELPA_ROOT)/archive-tmp/packages

build-elpa : force
	cp -a $(ELPA_ROOT)/packages/ada-mode $(ELPA_ROOT)/archive-tmp/packages
	cp -a $(ELPA_ROOT)/packages/ada-ref-man $(ELPA_ROOT)/archive-tmp/packages
	cp -a $(ELPA_ROOT)/packages/uniquify-files $(ELPA_ROOT)/archive-tmp/packages
	cp -a $(ELPA_ROOT)/packages/wisi     $(ELPA_ROOT)/archive-tmp/packages
	make -C $(ELPA_ROOT)/ process-archive

# (dvc-state-one "/Projects/elpa/")
# shell git push

uninstall-elpa :
	emacs -Q --eval '(progn (load-file "uninstall-elpa.el")(kill-emacs))'

# We don't kill emacs here, so we can check for compilation errors/warnings
install-elpa :
	emacs -Q --eval '(load-file "install-elpa.el")'

# We specify $(SHELL) here to work around a Mingw64 'make' problem.
Makefile.conf : create_makefile_conf.sh
	$(SHELL) -c ./create_makefile_conf.sh

include Makefile.conf

../ada_mode_wisi_parse.gpr : ../ada_mode_wisi_parse.gpr.gp
	gnatprep -DHAVE_GNAT_UTIL=$(HAVE_GNAT_UTIL) -DHAVE_LIBADALANG=$(HAVE_LIBADALANG) -DELPA="no" $< $@

../wisi.gpr : ../wisi.gpr.gp
	gnatprep -DELPA="no" $< $@

trace :
	addr2line -e ../exec_pro/run_ada_parser.exe 0x79c027 0x7715ff3f 0x7715ff11 0x7716068d 0x758a42 0x402903 0x7afce0 0x4013db 0x74927c02 0x7717ad2d 0x7717acf8

$(INSTALL_BIN)gpr_query$(EXE_EXT) install_gpr_query : gpr_query$(EXE_EXT)
	gprinstall -f -p -P ../gpr_query.gpr --install-name=gpr_query

%$(EXE_EXT) : %.adb force
	gprbuild -p -j8 ../ada_mode_wisi_parse.gpr $*.adb

build_executables :  ../ada_mode_wisi_parse.gpr ../wisi.gpr ../gpr_re2c.c ../ada_annex_p_re2c.c force
	gprbuild -p -j8 ../ada_mode_wisi_parse.gpr

install_executables : build_executables
	gprinstall -f -p -P ../ada_mode_wisi_parse.gpr --install-name=ada_mode_wisi_parse

../ada_annex_p_txt.wy : ../ada_annex_p.txt annex_p_to_wy.exe
	../annex_p_to_wy.exe ../ada_annex_p.txt ../ada_annex_p_txt.wy
	dos2unix -q ../ada_annex_p_txt.wy

compare_annex_p.stamp : ../ada_annex_p_txt.wy ../ada_annex_p.wy compare_annex_p.exe
	../compare_annex_p.exe ../ada_annex_p_txt.wy ../ada_annex_p.wy
	touch $@

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# end:
# end of file
