# Build elisp code, Ada executables, manuals

# compile with debug to get assertions for testing new code
#export Standard_Common_Build := Debug

export WISI_VERSION := 3.1.3

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
EMACS_EXE ?= emacs
endif

all : byte-compile-clean update test-clean test

docs : info html

update : autoloads
update : byte-compile

# Doing byte-compile-clean first avoids errors caused by
# loading new source on old .elc.
#
# 'package-initialize' is required for 'uniquify-files'.
byte-compile : byte-compile-clean
	cd ../; $(EMACS_EXE) -Q -batch -L . -l build/exclude-elpa.el --eval '(progn (package-initialize)(setq byte-compile-error-on-warn t)(batch-byte-compile))' *.el

byte-compile-clean :
	cd ..; rm -f *.elc

# Most of wisi is tested via ada-mode; see the tests there.
test : test_all_harness.run

# WISITOKEN is correct for Stephe's development machines;
# it can be overridden on the 'make' command line or by an
# external environment variable.
#
# ADA_PROJECT_PATH is used by the ../test/*.ad? files to test Ada mode
# interaction with gpr project files.
ifeq ($(shell uname),Linux)
export WISITOKEN ?= /Projects/org.wisitoken
EXE_EXT :=

else ifeq ($(shell uname),Darwin)
export WISITOKEN ?= /home/Projects/wisitoken/org.wisitoken
EXE_EXT :=

else
# windows
export WISITOKEN ?= c:/Projects/org.wisitoken
EXE_EXT := .exe

endif

# 'include' must be after env vars are set.
include rules.make

test_all_harness.run : ../test_all_harness$(EXE_EXT) force
	../test_all_harness$(EXE_EXT) $(RUN_ARGS)

BRANCH := $(notdir $(shell cd ..; pwd))

ifeq ($(BRANCH),org.emacs.wisi)
  TAR_FILE := org.emacs.wisi-$(WISI_VERSION)
else
  TAR_FILE := $(BRANCH)
endif

zip :
	rm -rf ../../$(TAR_FILE)
	mtn checkout --branch $(BRANCH) ../../$(TAR_FILE)
	tar jcf $(TAR_FILE).tar.bz2 --exclude .git -C ../.. $(TAR_FILE)


# tarball of ELPA package source, for sending to customers with their
# own CM but without good network access
DATE := $(shell date +%Y-%m-%d)
elpa-zip : wisi-elpa-$(DATE).tar.gz

wisi-elpa-$(DATE).tar.gz :
	tar zcf wisi-elpa-$(DATE).tar.gz -C $(ELPA_ROOT)/packages wisi

# tarball to create local ELPA archive
elpa-archive-zip : wisi-elpa-archive-$(DATE).tar.bz2

MANUALS := wisi

INFO_FILES := $(addsuffix .info, $(MANUALS))
info : $(INFO_FILES)
html : $(addsuffix .html, $(MANUALS))

VPATH := ..

ELPA_ROOT ?= $(shell cd ../../elpa; pwd)

# ELPA builds .info from .texinfo; if add new here, add to ../../elpa/elpa-packages
pub : force
	rm -rf $(ELPA_ROOT)/packages/wisi/*
	cp ../NEWS ../README ../*.make $(ELPA_ROOT)/packages/wisi
	cp ../*.ad? ../*.el ../*.sh ../*.texi ../wisi.gpr.gp $(ELPA_ROOT)/packages/wisi
	cd $(ELPA_ROOT)/packages/wisi; rm -f autoloads.el *libadalang* test*.ad?
	cp $(WISITOKEN)/Docs/wisitoken-user_guide.texinfo $(ELPA_ROOT)/packages/wisi
	cp $(WISITOKEN)/*.ad? $(ELPA_ROOT)/packages/wisi
	cp $(WISITOKEN)/main/*.ad? $(ELPA_ROOT)/packages/wisi
	cp $(WISITOKEN)/wisitoken_grammar_re2c.c  $(ELPA_ROOT)/packages/wisi
	rm $(ELPA_ROOT)/packages/wisi/wisitoken-lexer-regexp.ad?
	rm $(ELPA_ROOT)/packages/wisi/wisitoken-regexp.ad?
	rm $(ELPA_ROOT)/packages/wisi/recover_stats.adb
	rm $(ELPA_ROOT)/packages/wisi/wisitoken-followed_by.adb
	rm $(ELPA_ROOT)/packages/wisi/wisitoken-gen_token_enum.ad?
	rm $(ELPA_ROOT)/packages/wisi/wisitoken-parse-tree_sitter.ad?
	rm $(ELPA_ROOT)/packages/wisi/wisitoken-utf_8.ad?
	cp ../../org.stephe_leake.makerules/standard_common.gpr $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/build/alire_rules.make $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-unix_text_io.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/long_float_elementary_functions.ads $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-ada_containers* $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_array_image.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_doubly_linked_lists.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_doubly_linked_lists-gen_image_aux.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_queues.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_stacks*.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_vectors-gen_image_aux.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_vectors-gen_refs.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_vectors.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_bounded_definite_vectors_sorted*.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_definite_doubly_linked_lists-gen_image.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_definite_doubly_linked_lists-gen_image_aux.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_definite_doubly_linked_lists.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_definite_doubly_linked_lists_ref_count.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_definite_doubly_linked_lists_sorted.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_graphs.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_indefinite_doubly_linked_lists-gen_image_aux.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_indefinite_doubly_linked_lists.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_trimmed_image.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_hash_tables.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_min_heaps_fibonacci.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_queues.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_red_black_trees.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_stacks.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_vectors-gen_comparable.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_vectors-gen_image*.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_vectors.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_definite_vectors_sorted.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unbounded_sparse_ordered_sets.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-gen_unconstrained_array_image_aux.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal-generic_decimal_image.ad? $(ELPA_ROOT)/packages/wisi
	cp ../../org.stephe_leake.sal/source/sal.ad? $(ELPA_ROOT)/packages/wisi

../wisi.gpr : ../wisi.gpr.gp
	gnatprep -DELPA="no" $< $@

trace :
	addr2line -e ../exec_pro/run_ada_parser.exe 0x79c027 0x7715ff3f 0x7715ff11 0x7716068d 0x758a42 0x402903 0x7afce0 0x4013db 0x74927c02 0x7717ad2d 0x7717acf8

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# end:
# end of file
