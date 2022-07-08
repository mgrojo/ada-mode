# Build elisp code, Ada executables, manuals

# compile with debug to get assertions for testing new code
#export Standard_Common_Build := Debug

export WISI_VERSION := 3.1.3

all : byte-compile-clean update test-clean test

pub : docs

docs : info html

# This updates generated elisp and byte-compiled files after any
# source change, without running tests.
update : autoloads
update : byte-compile

# Doing byte-compile-clean first avoids errors caused by
# loading new source on old .elc.
#
# 'package-initialize' is required for 'uniquify-files'.
byte-compile : byte-compile-clean
	cd ../; emacs -Q -batch -L . -l build/exclude-elpa.el --eval '(progn (package-initialize)(setq byte-compile-error-on-warn t)(batch-byte-compile))' *.el

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

# FIXME; update to git
# tag :
# 	git tag h:org.emacs.wisi org.emacs.wisi-$(WISI_VERSION)

MANUALS := wisi

INFO_FILES := $(addsuffix .info, $(MANUALS))
html : $(addsuffix .html, $(MANUALS))

VPATH := ..

pub-wisi : force
# FIXME: add git push o.e.w -> elpa git
	make -C $(WISITOKEN)/build doc
	cp $(WISITOKEN)/*.ad? $(ELPA_ROOT)/packages/wisi
	cp $(WISITOKEN)/wisitoken_grammar_re2c.c  $(ELPA_ROOT)/packages/wisi
	rm $(ELPA_ROOT)/packages/wisi/wisitoken-regexp.ad?
# FIXME: delete more files not used by wisi
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

../wisi.gpr : ../wisi.gpr.gp
	gnatprep -DELPA="no" $< $@

trace :
	addr2line -e ../exec_pro/run_ada_parser.exe 0x79c027 0x7715ff3f 0x7715ff11 0x7716068d 0x758a42 0x402903 0x7afce0 0x4013db 0x74927c02 0x7717ad2d 0x7717acf8

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# end:
# end of file
