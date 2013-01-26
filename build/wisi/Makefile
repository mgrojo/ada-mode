# build and test the Emacs wisent Ada indentation and navigation engine

all : ada-grammar-wy.el gpr-grammar-wy.el compile-clean compile test-clean test

one : ada_mode-generic_package.ads.diff
two : test-gpr

# use OpenToken to compile same grammar, to see how broken OpenToken is :(
debug-grammar : wisi-gpr-grammar-generate.run

vpath %.wy ../../
vpath %.el ../../

# WORKAROUND: semantic-grammar-batch-build-packages can't handle `, in rules; patch here
%-wy.el : %.wy
	rm -f ../../wisent.output
	cd ../../; $(EMACS) -Q -batch -L build -L . -l load-paths-24.3.el -f semantic-grammar-batch-build-packages $(<F)
	sed -i "s/(wisi/,(wisi/" ../../$@
	rm -f ../../$*-wy.elc

wisi-%-generate.run : RUN_ARGS := -v
wisi-%-generate.run : wisi-%-generate.exe
	./wisi-$*-generate.exe $(RUN_ARGS)

.PRECIOUS : wisi-%-generate.adb
.PRECIOUS : wisi-gpr-grammar-generate.exe

WISI_OPENTOKEN := ../../../org.opentoken.stephe/build/windows_debug

wisi-%-generate.adb : %.wy $(WISI_OPENTOKEN)/wisi-generate.exe
	$(WISI_OPENTOKEN)/wisi-generate.exe $<

# ../../../org.opentoken.stephe/wisi/wisi-generate.adb
$(WISI_OPENTOKEN)/wisi-generate.exe : force
	$(MAKE) -C ../../../org.opentoken.stephe/build/windows_debug wisi-generate.exe

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

# (ada-parse-prj-file "wisi_opentoken_agg.gpr")
# (ada-select-prj-file "wisi_opentoken_agg.gpr")

# Local Variables:
# eval: (ada-parse-prj-file "../ada-mode.prj")
# eval: (ada-select-prj-file "../ada-mode.prj")
# End:
# end of file
