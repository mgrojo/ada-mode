# build and test the Emacs wisent Ada indentation and navigation engine

all : ada-grammar-wy.el ../../gpr-grammar-wy.el compile-clean compile test-clean test

test :: range_conflict.wisi-test
test :: subprograms.wisi-test
test :: body_instantiation_conflict.wisi-test
test :: identifier_list_name_conflict.wisi-test

one : subprograms.wisi-test
two : gds.gpr.diff

vpath %.wy ../../ ../../test/wisi
vpath %.el ../../ .

%.wisi-test : %-wy.el
	$(EMACS) -Q -batch -L . -L .. -L ../.. -l run-wisi-test.el --eval '(run-test "$*")'

.PRECIOUS : %-wy.el

WISI_OPENTOKEN := ../../../org.opentoken.stephe/build/windows_release

../../gpr-grammar-wy.el : gpr-grammar-wy.el
	cp $< $@

%-wy.el : %.wy $(WISI_OPENTOKEN)/wisi-generate.exe
	$(WISI_OPENTOKEN)/wisi-generate.exe -v 1 $< Elisp > $*.output

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
	rm -f *.wisi-test

RUNTEST := run-indent-test-wisi.el
INDENT.EL := ada-wisi.el gpr-wisi.el gpr-grammar-wy.el
# INDENT.EL += ada-grammar-wy.el FIXME: currently broken
include ../common.make

export ADA_PROJECT_PATH=../../test/;../../test/subdir

# eval: (ada-parse-prj-file "../ada-mode.prj")
# eval: (ada-select-prj-file "../ada-mode.prj")

# Local Variables:
# End:
# eval: (ada-parse-prj-file "wisi_opentoken.prj")
# eval: (ada-select-prj-file "wisi_opentoken.prj")
# end of file
