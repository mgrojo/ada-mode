# build and test the Emacs wisent Ada indentation and navigation engine

# test with mtn controlled source:  (setenv "ADA_MODE_DIR" "-L ../.. -l autoloads.el")
# test with installed elpa package: (setenv "ADA_MODE_DIR" "-f package-initialize")
ADA_MODE_DIR ?= -L ../.. -l autoloads.el

# "compile-ada" is a test; it verifies the test sources are legal Ada
# it also generates *.ali files for gnatxref, gpr_query
all : update compile-ada-clean compile-ada autoloads test-clean test

# things that can be done on debian stable (opentoken doesn't build, no gpr_query, no gnatstub)
debian-all : compile-ada-clean compile-ada autoloads test-clean test-debian

# this updates everything after any source change, without running the tests.
update : ../../ada_grammar-wy.el
update : ../../gpr-grammar-wy.el
# update : ../../ada_syntax.ads
update : ../../ada_grammar.ads
update : byte-compile
update : ada_mode_wisi_parse.exe

# *-wy.el are in monotone, so this is all we need after a monotone update
# doing byte-compile-clean first avoids errors caused by loading new source on old .elc
# Emacs 24.2 requires cl-lib from Gnu ELPA, so we need package-initialize here
byte-compile : byte-compile-clean
	cd ../../; $(EMACS_EXE) -Q -batch -L . --eval '(progn (package-initialize)(batch-byte-compile))' *.el

test : test-wisi
test : test-ada-gnatxref.stamp
test : test-ada-gpr_query.stamp
test : test-ada-ext-gpr_query.stamp
test : test-gpr
test : test-elisp
test : gpr-skel.gpr.diff

test-debian : test-ada-gnatxref.stamp test-gpr test-elisp gpr-skel.gpr.diff

# test-elisp debug:
# (progn (add-to-list 'load-path (expand-file-name "../../test")) (load "ada-mode-test.el"))

# test-wisi debug:
# (progn (load-file "../run-wisi-test.el") (run-test "ada-number-literal"))

test-wisi :: ada-number-literal.wisi-test
test-wisi :: body_instantiation_conflict.wisi-test
test-wisi :: case_expression.wisi-test
test-wisi :: empty_production_6.wisi-test
test-wisi :: empty_production_8.wisi-test
test-wisi :: identifier_list_name_conflict.wisi-test
test-wisi :: number-literal.wisi-test
test-wisi :: range_conflict.wisi-test
test-wisi :: subprograms.wisi-test

#one : install
one : RUNTEST := run-indent-test-wisi-ext-gpr_query.el
#one : ada_mode-parens.adb.diff
one :
	emacs -Q -L .. -L ../.. -l autoloads.el -l run-indent-test-wisi-ext-gpr_query.el --eval '(progn (run-test "../../test/ada_mode-parens.ads"))'

two : export Ada_Mode_Wisi_Parse_Build := Debug
two : export WISI_OPENTOKEN := /Projects/org.opentoken.stephe/build/debug
two : run_ada_parser.exe
	./run_ada_parser.exe -v 1 ../../test/debug.adb > debug.parse

vpath %.wy ../../ ../../test/wisi
vpath %.el ../../ .
vpath %.l ../../ .

INDENT.EL := ada-wisi.el gpr-wisi.el ../../gpr-grammar-wy.el ../../ada_grammar-wy.el ada_mode_wisi_parse.exe

# These are correct paths for Stephe's development machines;
# WISI_OPENTOKEN can be overridden on the 'make' command line or by an
# external environment variable.
ifeq ($(shell uname),Linux)
WISI_OPENTOKEN ?= /home/Projects/opentoken/org.opentoken.stephe/build/release
export ADA_PROJECT_PATH=../../test/:../../test/subdir

else
# windows
# (setenv "WISI_OPENTOKEN" (concat (substring default-directory 2) "../../../org.opentoken.stephe/build/release"))
WISI_OPENTOKEN ?= $(CURDIR)/../../../org.opentoken.stephe/build/release
export ADA_PROJECT_PATH=../../test/;../../test/subdir

endif

%.wisi-test : %-wy.el
	$(EMACS_EXE) -Q -batch -L .. $(ADA_MODE_DIR) -l run-wisi-test.el --eval '(run-test "$*")'

.PRECIOUS : %-wy.el ../../%-wy.el %.a %.ads

# -v 1 dumps grammar
# -v 2 dumps debug info from parser table generation, continues with unused tokens, unknown conflicts
%-wy.el : %.wy $(WISI_OPENTOKEN)/wisi-generate.exe
	cd ./$(<D); $(WISI_OPENTOKEN)/wisi-generate.exe -v 1 $(<F) Elisp > $(*F).elisp_output
	cd ./$(<D); dos2unix $(@F)

%.ads : %.wy $(WISI_OPENTOKEN)/wisi-generate.exe
	cd ./$(<D); $(WISI_OPENTOKEN)/wisi-generate.exe -v 1 $(<F) Ada_Emacs > $(*F).ada_output
	cd ./$(<D); dos2unix $(@F) $(*F).el

%.a : %.l
	aflex -i $<

%.ads : %.a
	gnatchop -w $*.a $*_dfa.a $*_io.a

%.exe : ../../%.adb force
	gprbuild -p ada_mode_wisi_parse_agg.gpr $<

ada_mode_wisi_parse.exe : ../../ada_grammar.ads force
	gprbuild -p ada_mode_wisi_parse_agg.gpr

run_ada_parser.exe : ../../ada_grammar.ads force
	gprbuild -p ada_mode_wisi_parse_agg.gpr run_ada_parser.adb

install : INSTALL_BIN := $(dir $(shell bash -c "type -p emacs"))
install : ada_mode_wisi_parse.exe
	strip -o $(INSTALL_BIN)$^ $^

clean ::
	rm -f *.a *.ads *.adb *.exe
	rm -rf obj

# ../../../org.opentoken/wisi/wisi-generate.adb
$(WISI_OPENTOKEN)/wisi-generate.exe : force
	$(MAKE) -C $(WISI_OPENTOKEN) wisi-generate.exe

.PHONY : force

byte-compile-clean :
	cd ../..; rm -f *.elc

clean ::
	cd ../..; rm -f *-wy.el *.elc
	rm -f *-wy.el *.elc

test-clean ::
	rm -f *.output *.wisi-test *.stamp
	cd ../../test/wisi/; rm -f *-wy.el *.output

include ../common.make

.PHONY : force

test-ada-gnatxref : RUNTEST := run-indent-test-wisi.el
test-ada-gnatxref : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-ada-gnatxref.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-gnatxref
	touch $@

test-ada-gpr_query : RUNTEST := run-indent-test-wisi-gpr_query.el
test-ada-gpr_query : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-ada-gpr_query.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-gpr_query
	touch $@

test-ada-ext-gpr_query : RUNTEST := run-indent-test-wisi-ext-gpr_query.el
test-ada-ext-gpr_query : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))

test-ada-ext-gpr_query.stamp : force
	rm -f *.diff *.tmp
	$(MAKE) test-ada-ext-gpr_query
	touch $@

# we don't use 'expand-file-name' here because that adds c:/ on windows
# eval: (setenv "WISI_OPENTOKEN" "/Projects/org.opentoken.stephe/build/release")
# eval: (ada-parse-prj-file "wisi_opentoken.prj")
# eval: (ada-select-prj-file "wisi_opentoken.prj")

# eval: (ada-parse-prj-file "../ada-mode.prj")
# eval: (ada-select-prj-file "../ada-mode.prj")

# Local Variables:
# eval: (setenv "WISI_OPENTOKEN" "/Projects/org.opentoken.stephe/build/debug")
# eval: (ada-parse-prj-file "ada_mode_wisi_parse.prj")
# eval: (ada-select-prj-file "ada_mode_wisi_parse.prj")
# End:
# end of file
