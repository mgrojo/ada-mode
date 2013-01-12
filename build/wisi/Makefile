# build and test the Emacs wisent Ada indentation and navigation engine

all : ada-grammar-wy.elc compile-clean compile test-clean test

one : standard_common.gpr.diff

vpath %.wy ../../

# For Emacs 24.2, we use downloaded cedet-1.1 (on advice from cedet semantic mailing list)
ada-grammar-wy.elc : ada-grammar.wy
	cd ../../; $(EMACS) -Q -batch -L build -l load-paths-24.3.el -f semantic-grammar-batch-build-packages $(F<)

RUNTEST := runtest-wisi.el

include ../common.make

clean ::
	rm -f *-wy.el *.elc

export ADA_PROJECT_PATH=../../test/;../../test/subdir

# Local Variables:
# eval: (ada-parse-prj-file "../../test/subdir/ada_mode.adp")
# eval: (ada-select-prj-file "../../test/subdir/ada_mode.adp")
# End:
# end of file
