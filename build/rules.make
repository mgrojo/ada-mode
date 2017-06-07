# makefile rules for FastToken

# note that we use .exe for test executables even on non-windows, to
# keep the makerules simpler.

.PRECIOUS : %.exe %.out

.PHONY : zip force

VPATH := ../..
VPATH += ../Test
VPATH += ../Examples/ASU_Example_3_6
VPATH += ../Examples/ASU_Example_4_46
VPATH += ../Examples/ASU_Example_5_10
VPATH += ../wisi
VPATH += ../wisi/test

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
tests : ada_lite_yylex.adb
tests : character_literal_yylex.adb
tests : test_all_harness.diff

# from ../wisi/test
#
# to parse .wy, build %_yylex.adb, and run the parser, we'd like to do:
#
# %_run.exe : %_run.adb %_yylex.adb; gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P fasttoken_test.gpr $(GPRBUILD_ARGS) $*_run
#
# but that gets overridden by the simpler .exe rule for other things.
# So we must list %_yylex.adb explicitly in tests.
#
# Testing with an Emacs module calling the elisp wisi lexer and wisi
# actions is done from the ada-mode development tree, not here.
#
# some also or only run from ../wisi/test/test_wisi_suite.adb We only
# diff %-process.el on a couple tests, because it doesn't depend on
# the grammar much
#tests : case_expression-elisp.el.diff done in wisi_wy_test.adb
tests : case_expression_yylex.adb
tests : case_expression-parse.diff
tests : conflict_name-process.el.diff
tests : conflict_name_yylex.adb
tests : conflict_name-parse.diff
tests : empty_production_1_yylex.adb
tests : empty_production_1-parse.diff
tests : empty_production_2_yylex.adb
tests : empty_production_2-parse.diff
tests : empty_production_3_yylex.adb
tests : empty_production_3-parse.diff
tests : empty_production_4_yylex.adb
tests : empty_production_4-parse.diff
tests : empty_production_5_yylex.adb
tests : empty_production_5-parse.diff
tests : empty_production_6_yylex.adb
tests : empty_production_6-parse.diff
tests : empty_production_7_yylex.adb
tests : empty_production_7-parse.diff
tests : empty_production_8_yylex.adb
tests : empty_production_8-parse.diff
tests : identifier_list_name_conflict_yylex.adb
tests : identifier_list_name_conflict-parse.diff
tests : subprograms-process.el.diff
tests : subprograms_yylex.adb
tests : subprograms-parse.diff

examples : asu_example_3_6-run.run
examples : asu_example_4_46-run.run
examples : asu_example_5_10_lr-run.run

asu_example_3_6-run.run : asu_example_3_6-run.exe
	cd ../Examples/ASU_Example_3_6; $(CURDIR)/asu_example_3_6-run.exe

asu_example_4_46-run.run : asu_example_4_46-run.exe
	cd ../Examples/ASU_Example_4_46; $(CURDIR)/asu_example_4_46-run.exe

asu_example_4_46_rd-run.run : asu_example_4_46_rd-run.exe
	cd ../Examples/ASU_Example_4_46; $(CURDIR)/asu_example_4_46_rd-run.exe

asu_example_5_10_lr-run.run : asu_example_5_10_lr-run.exe
	cd ../Examples/ASU_Example_5_10; $(CURDIR)/asu_example_5_10_lr-run.exe Example.txt

asu_example_5_10_rd_commute-run.run : asu_example_5_10_rd_commute-run.exe
	cd ../Examples/ASU_Example_5_10; $(CURDIR)/asu_example_5_10_rd_commute-run.exe Example.txt

asu_example_5_10_rd_list-run.run : asu_example_5_10_rd_list-run.exe
	cd ../Examples/ASU_Example_5_10; $(CURDIR)/asu_example_5_10_rd_list-run.exe Example.txt

ada_count.run : ada_count.exe
	./ada_count.exe ../Examples/Language_Lexer_Examples/ada_count.adb ../Examples/Language_Lexer_Examples/test_ada_lexer.adb

test_all_harness.out : test_all_harness.exe wisi-generate.exe

test_ada_lexer.run : test_ada_lexer.exe
	./test_ada_lexer.exe ../Examples/Language_Lexer_Examples/test_ada_lexer.adb

test_html_lexer_safe.out : test_html_lexer_safe.exe test_html_scan.html
	./$^ $(RUN_ARGS) > $@

test_html_lexer_safe-syntax_error.out : test_html_lexer_safe.exe test_html_scan-syntax_error.html
	./$^ $(RUN_ARGS) > $@

test_html_lexer_unsafe.run : test_html_lexer_unsafe.exe
	./test_html_lexer_unsafe.exe ../Docs/fasttoken.html

test_java_lexer.run : test_java_lexer.exe
	./test_java_lexer.exe ../Examples/Language_Lexer_Examples/something.java

# yes, we use the java source as a test for the m3 lexer. Close enough!
test_m3_lexer.run : test_m3_lexer.exe
	./test_m3_lexer.exe ../Examples/Language_Lexer_Examples/something.java

install: library wisi-generate.exe
	make -f Install.make install

uninstall:
	make -f Install.make install-clean

library:
	gprbuild -p --RTS=$(ADA_RUN_TIME) -Pfasttoken_lib

clean :: test-clean
	rm -rf obj *.exe
	rm -rf libzcx libsjlj libobjzcx libobjsjlj

test-clean : wisi-clean aflex-clean
	rm -f *.diff *.in *_run.exe *-run.exe *test.exe *.out *.parse *.txt *-wy.el

source-clean ::
	-find $(SOURCE_ROOT) -name "*~" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name ".#*" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name "*,t" -print | xargs rm -v

# the test executables are only in the test project file, which requires AUnit
# Override the project file for wisi-generate.exe, for use with Emacs Ada mode without AUnit
wisi-generate.exe : force
	gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P fasttoken.gpr $(GPRBUILD_ARGS) wisi-generate

%.check : %.adb force; gnatmake -p -k -gnatc -Pfasttoken_test.gpr $(GNATMAKE_ARGS) $*

%.out : %.exe
	./$*.exe $(RUN_ARGS) > $*.out 2>&1
	dos2unix $*.out

DIFF_OPT := -u -w
%.diff : %.good_out %.out ; diff $(DIFF_OPT) $^ > $@

%.diff : %.good_el %.el ; diff $(DIFF_OPT) $^ > $@

# the parse_table and the state trace of the parse is the known good output
%-parse.diff : %.good_parse %.parse
	diff $(DIFF_OPT) $(^:parse=parse_table) > $@
	diff $(DIFF_OPT) $^ >> $@

# This runs wisi-generate with lexer Elisp; -process.l uses lexer Aflex
%-process.el : %.wy wisi-generate.exe
	./wisi-generate.exe -v 1 --output_language Ada_Emacs --lexer Elisp --interface process $< > $*.parse_table
	dos2unix $*.parse_table
	dos2unix $*-process.el

%-process.el.diff : %-process.good_el %-process.el
	diff $(DIFF_OPT) $< $*-process.el > $@

%.run : %.exe ;	./$(*F).exe $(RUN_ARGS)

%.l : %.wy wisi-generate.exe
	./wisi-generate.exe -v 1 $< > $*.parse_table
	dos2unix $*.parse_table

~/bin/aflex.exe : force
	make -C /Projects/edu.uci.aflex/build install

# delete files created by wisi-generate
wisi-clean :
	rm -f *.parse_table *.ads *.adb *.el *.l

%.parse : %.input %_run.exe
	./$*_run.exe -v 4 $< > $*.parse
	dos2unix $*.parse

%.exe : force; gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P fasttoken_test.gpr $(GPRBUILD_ARGS) $*

%_yylex.ada : %.l
	aflex -i -s -E -D../wisi/fasttoken_aflex_dfa.adb.template -O../wisi/fasttoken_aflex_io.adb.template $(AFLEX_ARGS) $<

%_yylex.adb : %_yylex.ada
	gnatchop -w $*_yylex.ada $*_dfa.ada $*_io.ada

# delete files created by aflex
aflex-clean :
	rm -f *.a *_dfa.ad? *_io.ad? *_yylex.adb

source-clean ::
	-find ../ -name "*~" -delete
	-find ../ -name ".#*" -delete

BRANCH := $(notdir $(shell cd ..; pwd))

bz2file : force
	rm -rf ../../$(BRANCH)-$(ZIP_VERSION)
	mtn checkout --branch $(BRANCH) ../../$(BRANCH)-$(ZIP_VERSION)
	tar -c -O  -C ../.. --exclude=_MTN --exclude "build/x86*" --exclude=.mtn-ignore --exclude=.dvc-exclude --exclude debug_parser.adb --no-anchor $(BRANCH)-$(ZIP_VERSION) | bzip2 -9 > fasttoken-$(ZIP_VERSION).tar.bz2

zipfile : ROOT := $(shell cd ..; basename `pwd`)
zipfile : force
	cd ../..; zip -q -r $(CURDIR)/fasttoken-$(ZIP_VERSION).zip $(BRANCH)-$(ZIP_VERSION) -x "$(ROOT)-$(ZIP_VERSION)/_MTN/*" -x "$(ROOT)-$(ZIP_VERSION)/build/x86_*" -x "$(ROOT)-$(ZIP_VERSION)/.mtn-ignore" -x "$(ROOT)-$(ZIP_VERSION)/.dvc-exclude" -x "$(ROOT)-$(ZIP_VERSION)/debug_parser.adb"

.PRECIOUS : %.ada %.ads %_run.exe %.l %.parse %-process.el %-wy.el

vpath %-wy.good_el  ../wisi/test
vpath %.good_parse  ../wisi/test
vpath %.input  ../wisi/test
vpath %.texinfo ../Docs
vpath %.wy ../wisi/test ../time

# end of file
