# build and test the Emacs wisent Ada indentation and navigation engine

all : ../../ada-grammar-wy.el ../../gpr-grammar-wy.el compile-clean compile test-clean test

test :: body_instantiation_conflict.wisi-test
test :: case_expression.wisi-test
test :: identifier_list_name_conflict.wisi-test
test :: range_conflict.wisi-test
test :: subprograms.wisi-test

one : case_expression.wisi-test
two : gds.gpr.diff

vpath %.wy ../../ ../../test/wisi
vpath %.el ../../ .

%.wisi-test : %-wy.el
	$(EMACS) -Q -batch -L . -L .. -L ../.. -l run-wisi-test.el --eval '(run-test "$*")'

.PRECIOUS : %-wy.el ../../%-grammar-wy.el

WISI_OPENTOKEN := ../../../org.opentoken.stephe/build/windows_release

# -v dumps grammar, continues with unused tokens
%-wy.el : %.wy $(WISI_OPENTOKEN)/wisi-generate.exe
	cd ./$(<D); $(CURDIR)/$(WISI_OPENTOKEN)/wisi-generate.exe -v 1 $(<F) Elisp > $(*F).output

# ../../../org.opentoken.stephe/wisi/wisi-generate.adb
$(WISI_OPENTOKEN)/wisi-generate.exe : force
	$(MAKE) -C $(WISI_OPENTOKEN) wisi-generate.exe

.PHONY : force

GPRBUILD_TARGET := x86-windows
%.exe : %.adb force
	gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P wisi_opentoken_agg.gpr $(GPRBUILD_ARGS) $*

clean ::
	cd ../..; rm -f *-wy.el *.elc
	rm -f *-wy.el *.elc

test-clean ::
	rm -f *.output *.wisi-test
	cd ../../test/wisi/; rm -f *-wy.el *.output

RUNTEST := run-indent-test-wisi.el
INDENT.EL := ada-wisi.el gpr-wisi.el ../../gpr-grammar-wy.el ../../ada-grammar-wy.el
include ../common.make

test-ada : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

export ADA_PROJECT_PATH=../../test/;../../test/subdir

# eval: (ada-parse-prj-file "../ada-mode.prj")
# eval: (ada-select-prj-file "../ada-mode.prj")

# Local Variables:
# eval: (ada-parse-prj-file "wisi_opentoken.prj")
# eval: (ada-select-prj-file "wisi_opentoken.prj")
# End:
# end of file
