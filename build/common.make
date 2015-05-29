# common parts of makefiles for FastToken

# note that we use .exe for test executables even on non-windows, to
# keep the makerules simpler.

.PRECIOUS : %.exe %.out

.PHONY : force

VPATH := ../..
VPATH += ../../Test
VPATH += ../../Examples/ASU_Example_3_6
VPATH += ../../Examples/ASU_Example_4_46
VPATH += ../../Examples/ASU_Example_5_10
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
tests : production_test-run.run
tests : recognizer_based_integer_test.run
tests : recognizer_integer_test.run
tests : string_test-run.run
tests : test_all_harness.diff
tests : test_html_lexer_safe.diff
tests : test_html_lexer_safe-syntax_error.diff
tests : token_analyzer_ctd-run.run

# from ../wisi/test
#
# to parse .wy, build .ads, run parser, we'd like to do:
#
# %_run.exe : %_run.adb %.ads; gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P fasttoken_test.gpr $(GPRBUILD_ARGS) $*_run
#
# but that gets overridden by the simpler .exe rule for other things.
# So we must list %.ads or %.l explicitly in tests. We do half of
# these tests with the Aflex lexer, to get some testing with that.
# Testing with an Emacs module calling the elisp wisi lexer and wisi
# actions is done from the ada-mode development tree, not here.
#
# some also or only run from ../wisi/test/test_wisi_suite.adb
tests : empty_production_1_yylex.ads
tests : empty_production_1-parse.diff
tests : empty_production_2.ads
tests : empty_production_2-parse.diff
tests : empty_production_3_yylex.ads
tests : empty_production_3-parse.diff
tests : empty_production_4.ads
tests : empty_production_4-parse.diff
tests : empty_production_5_yylex.ads
tests : empty_production_5-parse.diff
tests : empty_production_6.ads
tests : empty_production_6-parse.diff
tests : empty_production_7_yylex.ads
tests : empty_production_7-parse.diff
tests : empty_production_8.ads
tests : empty_production_8-parse.diff
tests : identifier_list_name_conflict_yylex.ads
tests : identifier_list_name_conflict-parse.diff
tests : multi_conflict.ads
tests : multi_conflict-parse.diff
tests : subprograms_yylex.ads
tests : subprograms-parse.diff

examples : asu_example_3_6-run.run
examples : asu_example_4_46-run.run
examples : asu_example_5_10_lr-run.run
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
	./test_html_lexer_unsafe.exe ../../Docs/fasttoken.html

test_java_lexer.run : test_java_lexer.exe
	./test_java_lexer.exe ../../Examples/Language_Lexer_Examples/something.java

# yes, we use the java source as a test for the m3 lexer. Close enough!
test_m3_lexer.run : test_m3_lexer.exe
	./test_m3_lexer.exe ../../Examples/Language_Lexer_Examples/something.java

install: library
	make -f Install.make install

uninstall:
	make -f Install.make install-clean

library:
	gprbuild -p --RTS=$(ADA_RUN_TIME) -Pfasttoken_lib

clean :: test-clean
	rm -rf obj *.exe
	rm -rf libzcx libsjlj libobjzcx libobjsjlj

distclean :: clean
	rm -rf obj obj_tree

test-clean :
	rm -f *.diff *.in *_run.exe *-run.exe *test.exe *.parse_table *.out *.parse *.txt *-wy.el
	rm -f *.ads *.adb

source-clean ::
	-find $(SOURCE_ROOT) -name "*~" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name ".#*" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name "*,t" -print | xargs rm -v

# the test executables are only in the test project file, which requires AUnit
# Override the project file for wisi-generate.exe, for use with Emacs Ada mode without AUnit
wisi-generate.exe : force
	gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P fasttoken.gpr $(GPRBUILD_ARGS) wisi-generate

%.check : %.adb force; gnatmake -p -k -gnatc -Pfasttoken_test_agg.gpr $(GNATMAKE_ARGS) $*

%.out : %.exe
	./$*.exe > $*.out 2>&1
	dos2unix $*.out

DIFF_OPT := -u -w
%.diff : %.good_out %.out ; diff $(DIFF_OPT) $^ > $@

%.diff : %.good_el %.el ; diff $(DIFF_OPT) $^ > $@

# the parse_table and the state trace of the parse is the known good output
%-parse.diff : %.good_parse %.parse
	diff $(DIFF_OPT) $(^:parse=parse_table) > $@
	diff $(DIFF_OPT) $(^:parse=el) >> $@
	diff $(DIFF_OPT) $^ >> $@

%.run : %.exe ;	./$(*F).exe $(RUN_ARGS)

# %-wy.el : RUN_ARGS := -v 1
%-wy.el : %.wy wisi-generate.exe
	./wisi-generate.exe $(RUN_ARGS) $< Elisp > $*.output

# wisi-generate Ada_Emacs runs lalr_parser.generate, and we always want the parse_table for tests
# match historical first_state_index, first_parser_label
%.ads : RUN_ARGS ?= -v 1 --first_state_index 1 --first_parser_label 1
%.ads : %.wy wisi-generate.exe
	./wisi-generate.exe $(RUN_ARGS) $< Ada_Emacs FastToken_Lexer > $*.parse_table
	dos2unix $*.parse_table
	dos2unix -q $*.el

%.l : RUN_ARGS ?= -v 1 --first_state_index 1 --first_parser_label 1
%.l : %.wy wisi-generate.exe
	./wisi-generate.exe $(RUN_ARGS) $< Ada_Emacs Aflex_Lexer > $*.parse_table
	dos2unix $*.parse_table
	dos2unix -q $*.el

clean :: wisi-clean

# delete files created by wisi-generate
wisi-clean :
	rm -f *.parse_table *.ads *.adb *.el *.l

# for a wisi test
ada_grammar.ads : RUN_ARGS ?= --profile
ada_grammar.ads : LEXER ?= Aflex_Lexer

%.parse : %.input %_run.exe
	./$*_run.exe -v 2 $< > $*.parse
	dos2unix $*.parse

%.exe : force; gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P fasttoken_test_agg.gpr $(GPRBUILD_ARGS) $*

%.ada : %.l
	aflex -i -s -E -D../../wisi/fasttoken_aflex_dfa.adb.template -O../../wisi/fasttoken_aflex_io.adb.template $(AFLEX_ARGS) $<

%_yylex.ads : %.ada
	gnatchop -w $*_yylex.ada $*_dfa.ada $*_io.ada

clean :: aflex-clean

# delete files created by aflex
aflex-clean :
	rm -f *.a *_dfa.ad? *_io.ad? *_yylex.adb

.PRECIOUS : %.ada %.ads %_run.exe %.l %.parse %-wy.el

vpath %.wy ../../wisi/test
vpath %-wy.good_el  ../../wisi/test
vpath %.good_parse  ../../wisi/test
vpath %.input  ../../wisi/test

# end of file
