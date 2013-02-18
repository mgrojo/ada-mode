# build and test the Emacs wisent Ada indentation and navigation engine

all : ada-grammar-wy.el gpr-grammar-wy.el compile-clean compile test-clean test

test :: test-wisi-parse

#one : range_conflict-wy.el
one : range_conflict.wisi-test
two : test-gpr

vpath %.wy ../../ ../../test/wisi
vpath %.el ../../ .

%.wisi-test : %-wy.el
	$(EMACS) -Q -batch -L . -L .. -L ../.. -l run-wisi-test.el --eval '(run-test "$*")'

# WORKAROUND: semantic-grammar-batch-build-packages can't handle `, in rules; patch here
# WORKAROUND: parser does not execute actions when grammar is byte-compiled (may need to byte-compile everything?)
# See notes in comp.el about byte-compile-handler
gpr-grammar-wy.el : gpr-grammar.wy
	rm -f ../../wisent.output
	cd ../../; $(EMACS) -Q -batch -L build -L . -l load-paths-24.3.el -f semantic-grammar-batch-build-packages $(<F)
	sed -i "s/(wisi/,(wisi/" ../../$@
	rm -f ../../$*-wy.elc

.PRECIOUS : %-wy.el

WISI_OPENTOKEN := ../../../org.opentoken.stephe/build/windows_release

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

RUNTEST := runtest-wisi.el
INDENT.EL := ada-wisi.el gpr-wisi.el ada-grammar-wy.el gpr-grammar-wy.el
include ../common.make

export ADA_PROJECT_PATH=../../test/;../../test/subdir

# eval: (ada-parse-prj-file "../ada-mode.prj")
# eval: (ada-select-prj-file "../ada-mode.prj")

# Local Variables:
# End:
# eval: (ada-parse-prj-file "wisi_opentoken.prj")
# eval: (ada-select-prj-file "wisi_opentoken.prj")
# end of file
