# common parts of makefiles for OpenToken

# note that we use .exe for test executables even on non-windows, to
# keep the makerules simpler.

.PRECIOUS : %.exe %.out

.PHONY : force

VPATH := ../..
VPATH += ../../Test
VPATH += ../../Examples/ASU_Example_3_6
VPATH += ../../Examples/ASU_Example_4_46
VPATH += ../../Examples/ASU_Example_5_10
VPATH += ../../Examples/Language_Lexer_Examples
VPATH += ../../Language_Lexers
VPATH += ../../wisi
VPATH += ../../wisi/test

# Variables for library creation
export GPRBUILD_TARGET := $(shell gcc -dumpmachine)
ifneq (,$(findstring mingw,$(GPRBUILD_TARGET)))
   export DYN_LIB_EXTENSION := dll
else ifneq (,$(findstring darwin,$(GPRBUILD_TARGET)))
   export DYN_LIB_EXTENSION := dylib
else ifneq (,$(findstring linux,$(GPRBUILD_TARGET)))
   export DYN_LIB_EXTENSION := so
else
   $(error "Don't know dynamic lib file extension for $(GPRBUILD_TARGET)")
endif

tests : wisi-generate.exe
tests : association_token_test-run.diff
tests : enumerated_token_list_test.run
tests : name_token_test-run.diff
tests : production_test-run.run
tests : recognizer_based_integer_test.run
tests : recognizer_integer_test.run
tests : string_test-run.run
tests : test_all_harness.diff
tests : test_html_lexer_safe.diff
tests : test_html_lexer_safe-syntax_error.diff
tests : token_analyzer_ctd-run.run
tests : token_list_test-run.run
tests : token_selection_test-run.run
tests : token_sequence_test-run.run

# wisi parse tests
# ../../wisi/test

# to parse .wy, build .ads, run parser, we'd like to do:
# %_run.exe : %_run.adb %.ads; gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P opentoken_test.gpr $(GPRBUILD_ARGS) $*_run
# but that gets overridden by the simpler .exe rule for other things. So we must list %.ads explicitly in tests:

# from ../wisi/test
# some also or only run from ../wisi/test/test_wisi_suite.adb
tests : empty_production_1.ads
tests : empty_production_1-parse.diff
tests : empty_production_2.ads
tests : empty_production_2-parse.diff
tests : empty_production_3.ads
tests : empty_production_3-parse.diff
tests : empty_production_4.ads
tests : empty_production_4-parse.diff
tests : empty_production_5.ads
tests : empty_production_5-parse.diff
tests : empty_production_6.ads
tests : empty_production_6-parse.diff
tests : empty_production_7.ads
tests : empty_production_7-parse.diff
tests : empty_production_8.ads
tests : empty_production_8-parse.diff
tests : identifier_list_name_conflict.ads
tests : identifier_list_name_conflict-parse.diff
tests : multi_conflict.ads
tests : multi_conflict-parse.diff
tests : subprograms.ads
tests : subprograms-parse.diff

examples : asu_example_3_6-run.run
examples : asu_example_4_46-run.run
examples : asu_example_4_46_rd-run.run
examples : asu_example_5_10_lr-run.run
examples : asu_example_5_10_rd_commute-run.run
examples : asu_example_5_10_rd_list-run.run
examples : ada_count.run
examples : test_ada_lexer.run
examples : test_html_lexer_unsafe.run
examples : test_java_lexer.run
examples : test_m3_lexer.run

asu_example_3_6-run.run : asu_example_3_6-run.exe
	cd ../../Examples/ASU_Example_3_6; $(CURDIR)/asu_example_3_6-run.exe

asu_example_4_46-run.run : asu_example_4_46-run.exe
	cd ../../Examples/ASU_Example_4_46; $(CURDIR)/asu_example_4_46-run.exe

asu_example_4_46_rd-run.run : asu_example_4_46_rd-run.exe
	cd ../../Examples/ASU_Example_4_46; $(CURDIR)/asu_example_4_46_rd-run.exe

asu_example_5_10_lr-run.run : asu_example_5_10_lr-run.exe
	cd ../../Examples/ASU_Example_5_10; $(CURDIR)/asu_example_5_10_lr-run.exe Example.txt

asu_example_5_10_rd_commute-run.run : asu_example_5_10_rd_commute-run.exe
	cd ../../Examples/ASU_Example_5_10; $(CURDIR)/asu_example_5_10_rd_commute-run.exe Example.txt

asu_example_5_10_rd_list-run.run : asu_example_5_10_rd_list-run.exe
	cd ../../Examples/ASU_Example_5_10; $(CURDIR)/asu_example_5_10_rd_list-run.exe Example.txt

ada_count.run : ada_count.exe
	./ada_count.exe ../../Examples/Language_Lexer_Examples/ada_count.adb ../../Examples/Language_Lexer_Examples/test_ada_lexer.adb

test_all_harness.out : test_all_harness.exe wisi-generate.exe

test_ada_lexer.run : test_ada_lexer.exe
	./test_ada_lexer.exe ../../Examples/Language_Lexer_Examples/test_ada_lexer.adb

test_html_lexer_safe.out : test_html_lexer_safe.exe test_html_scan.html
	./$^ $(RUN_ARGS) > $@

test_html_lexer_safe-syntax_error.out : test_html_lexer_safe.exe test_html_scan-syntax_error.html
	./$^ $(RUN_ARGS) > $@

test_html_lexer_unsafe.run : test_html_lexer_unsafe.exe
	./test_html_lexer_unsafe.exe ../../Docs/opentoken.html

test_java_lexer.run : test_java_lexer.exe
	./test_java_lexer.exe ../../Examples/Language_Lexer_Examples/something.java

# yes, we use the java source as a test for the m3 lexer. Close enough!
test_m3_lexer.run : test_m3_lexer.exe
	./test_m3_lexer.exe ../../Examples/Language_Lexer_Examples/something.java

install: library
	make -f Install.make install

library:
	gprbuild -p -Popentoken_lib

clean :: test-clean
	rm -rf obj/* lib-obj/* *.exe
	rm -rf lib/*

distclean :: clean
	rm -rf obj obj_tree

test-clean :
	rm -f *.diff *_run.exe *-run.exe *test.exe *.parse_table *.out *.parse *.txt *-wy.el
	rm -f *.ads *.adb

source-clean ::
	-find $(SOURCE_ROOT) -name "*~" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name ".#*" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name "*,t" -print | xargs rm -v

# the test executables are only in the test project file, which requires AUnit
# Override the project file for wisi-generate.exe, for use with Emacs Ada mode without AUnit
wisi-generate.exe : force
	gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P opentoken.gpr $(GPRBUILD_ARGS) wisi-generate

%.check : %.adb force; gnatmake -p -k -gnatc -Popentoken_test.gpr $(GNATMAKE_ARGS) $*

%.out : %.exe ;	./$*.exe > $*.out 2>&1

DIFF_OPT := -u -w
%.diff : %.good_out %.out ; diff $(DIFF_OPT) $^ > $@

%.diff : %.good_el %.el ; diff $(DIFF_OPT) $^ > $@

# the parse_table and the state trace of the parse is the known good output
%-parse.diff : %.good_parse %.parse
	diff $(DIFF_OPT) $(^:parse=parse_table) > $@
	diff $(DIFF_OPT) $^ >> $@

%.run : %.exe ;	./$(*F).exe $(RUN_ARGS)

# %-wy.el : RUN_ARGS := -v 1
%-wy.el : %.wy wisi-generate.exe
	./wisi-generate.exe $(RUN_ARGS) $< Elisp > $*.output

# wisi-generate Ada_Emacs runs lalr_parser.generate, and we always want the parse_table for tests
%.ads : RUN_ARGS ?= -v 1
%.ads : %.wy wisi-generate.exe
	./wisi-generate.exe $(RUN_ARGS) $< Ada_Emacs > $*.parse_table
	dos2unix $*.parse_table

%.parse : %.input %_run.exe
	./$*_run.exe -v $< > $*.parse
	dos2unix $*.parse

%.exe : force; gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P opentoken_test.gpr $(GPRBUILD_ARGS) $*

.PRECIOUS : %-wy.el %.ads %_run.exe %.parse

vpath %.wy ../../wisi/test
vpath %-wy.good_el  ../../wisi/test
vpath %.good_parse  ../../wisi/test
vpath %.input  ../../wisi/test

# end of file
