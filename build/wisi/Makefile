# build and test the Emacs wisent Ada indentation and navigation engine

all : ../../ada-grammar-wy.el ../../gpr-grammar-wy.el compile-clean compile test-clean test

test-wisi :: body_instantiation_conflict.wisi-test
test-wisi :: case_expression.wisi-test
test-wisi :: identifier_list_name_conflict.wisi-test
test-wisi :: range_conflict.wisi-test
test-wisi :: subprograms.wisi-test

one : ada_mode-nominal.adb.diff
two : debug.adb.diff

vpath %.wy ../../ ../../test/wisi
vpath %.el ../../ .

# There doesn't seem to be a reliable way to detect the OS?
# not clear what sets LIBRARY_PATH on windows (sigh)
ifeq ($(LIBRARY_PATH),/usr/lib/i386-linux-gnu/)
#linux
WISI_OPENTOKEN := /home/Projects/opentoken/org.opentoken.stephe/build/linux_release

# this is apparently overridden to /bin/emacs! must specify on make command line (sigh).
EMACS := /usr/bin/emacs
else
# windows
WISI_OPENTOKEN := $(CURDIR)/../../../org.opentoken.stephe/build/windows_release
endif

foo :
	echo $(LIBRARY_PATH)
	echo $(WISI_OPENTOKEN)
	echo $(EMACS)

%.wisi-test : %-wy.el
	$(EMACS) -Q -batch -L . -L .. -L ../.. -l run-wisi-test.el --eval '(run-test "$*")'

.PRECIOUS : %-wy.el ../../%-grammar-wy.el

# -v 1 dumps grammar, continues with unused tokens
# -v 2 dumps debug info from parser table generation
%-wy.el : %.wy $(WISI_OPENTOKEN)/wisi-generate.exe
	cd ./$(<D); $(WISI_OPENTOKEN)/wisi-generate.exe -v 1 $(<F) Elisp > $(*F).output

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

# FIXME: delete currently broken tests
ADA_TEST_FILES := $(filter-out ada_mode-interactive.adb, $(ADA_TEST_FILES)) # explicit smie functions
# end FIXME:

test-ada : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

export ADA_PROJECT_PATH=../../test/;../../test/subdir

# eval: (ada-parse-prj-file "wisi_opentoken.prj")
# eval: (ada-select-prj-file "wisi_opentoken.prj")

# Local Variables:
# eval: (ada-parse-prj-file "../ada-mode.prj")
# eval: (ada-select-prj-file "../ada-mode.prj")
# End:
# end of file
