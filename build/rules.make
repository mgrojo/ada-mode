# makefile rules for WisiToken

# note that we use .exe for test executables even on non-windows, to
# keep the makerules simpler.

.PRECIOUS : %.exe %.out

.PHONY : zip force

VPATH := ../..
VPATH += ../Test
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
tests : ada_lite_re2c.c
tests : character_literal_re2c.c
tests : test_all_harness.diff

# from ../wisi/test
#
# to parse .wy, build %_lexer.c, and run the parser, we'd like to do:
#
# %_run.exe : %_run.adb %_lexer.c; gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P wisitoken_test.gpr $(GPRBUILD_ARGS) $*_run
#
# but that gets overridden by the simpler .exe rule for other things.
# So we must list %_lexer.c explicitly in tests.
#
# Testing with an Emacs module calling the elisp wisi lexer and wisi
# actions is done from the ada-mode development tree, not here.
#
# some also or only run from ../wisi/test/test_wisi_suite.adb.
#
# We only diff %-process.el on a couple tests, because it doesn't
# depend on the grammar much
#
# FIXME: keep at least one aflex test, or delete aflex support

#tests : case_expression-elisp.el.diff done in wisi_wy_test.adb
tests : case_expression_re2c.c
tests : case_expression-parse.diff
tests : conflict_name-process.el.diff
tests : conflict_name_yylex.adb
tests : conflict_name-parse.diff
tests : empty_production_1_re2c.c
tests : empty_production_1-parse.diff
tests : empty_production_2_re2c.c
tests : empty_production_2-parse.diff
tests : empty_production_3_re2c.c
tests : empty_production_3-parse.diff
tests : empty_production_4_re2c.c
tests : empty_production_4-parse.diff
tests : empty_production_5_re2c.c
tests : empty_production_5-parse.diff
tests : empty_production_6_re2c.c
tests : empty_production_6-parse.diff
tests : empty_production_7_re2c.c
tests : empty_production_7-parse.diff
tests : empty_production_8_re2c.c
tests : empty_production_8-parse.diff
tests : identifier_list_name_conflict_re2c.c
tests : identifier_list_name_conflict-parse.diff
tests : subprograms-process.el.diff

# we don't run subprograms-parse because subprograms is used in a real
# Emacs Ada mode test, so it has real elisp syntax.

test_all_harness.out : test_all_harness.exe wisi-generate.exe

install: library wisi-generate.exe
	make -f Install.make install

uninstall:
	make -f Install.make install-clean

library:
	gprbuild -p --RTS=$(ADA_RUN_TIME) -Pwisitoken_lib

clean :: test-clean
	rm -rf obj *.exe
	rm -rf obj_pro exec_pro
	rm -rf libzcx libsjlj libobjzcx libobjsjlj

test-clean : wisi-clean aflex-clean quex-clean re2c-clean
	rm -f *.diff *.in *_run.exe *-run.exe *test.exe *.out *.parse *.txt *.wy

source-clean ::
	-find $(SOURCE_ROOT) -name "*~" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name ".#*" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name "*,t" -print | xargs rm -v

# the test executables are only in the test project file, which requires AUnit
# Override the project file for wisi-generate.exe, for use with Emacs Ada mode without AUnit
wisi-generate.exe : force
	gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P wisitoken.gpr $(GPRBUILD_ARGS) wisi-generate

%.check : %.adb force; gnatmake -p -k -gnatc -Pwisitoken_test.gpr $(GNATMAKE_ARGS) $*

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

%-process.el : %.wy wisi-generate.exe
	./wisi-generate.exe -v 1 --output_language Ada_Emacs --lexer Elisp --interface process $< > $*.parse_table
	dos2unix $*.parse_table
	dos2unix $*-process.el

%-process.el.diff : %-process.good_el %-process.el
	diff $(DIFF_OPT) $< $*-process.el > $@

%.run : %.exe ;	./$(*F).exe $(RUN_ARGS)

# We assume lexer is specified in the .wy file. wisi-generate also generates other files.
%.re2c %.qx %.l : %.wy wisi-generate.exe
	./wisi-generate.exe -v 1 $< > $*.parse_table
	dos2unix $*.parse_table

~/bin/aflex.exe : force
	make -C /Projects/edu.uci.aflex/build install

# delete files created by wisi-generate
# don't delete prj.el
wisi-clean :
	rm -f *-elisp.el *-process.el *.parse_table *.ads *.adb  *.l *.qx

# -v 2 gives stack trace
%.parse : %.input %_run.exe
	./$*_run.exe -v 2 $< > $*.parse
	dos2unix $*.parse

%.exe : force; gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P wisitoken_test.gpr $(GPRBUILD_ARGS) $*

# Aflex rules; wisi-generate outputs %.l
%_yylex.ada : %.l
	aflex -i -s -E -D../wisi/wisitoken_aflex_dfa.adb.template -O../wisi/wisitoken_aflex_io.adb.template $(AFLEX_ARGS) $<

%_yylex.adb : %_yylex.ada
	gnatchop -w $*_yylex.ada $*_dfa.ada $*_io.ada

aflex-clean :
	rm -f *.ada *_dfa.ad? *_io.ad? *_yylex.adb

# Quex rules; wisi-generate outputs %.qx
%_lexer.c : %.qx
	$(QUEX_PATH)/quex-exe.py --language C --encoding utf8 -i $< -o $*_lexer

quex-clean :
	rm -f *.h *.c

# Re2c rules; wisi-generate outputs %.qx
%_re2c.c : %.re2c
	$(RE2C_HOME)/bin/re2c --debug-output --input custom -W -Werror --utf-8 -o $@ $<

re2c-clean :
	rm -f *.c *.re2c

# clean rules
source-clean ::
	-find ../ -name "*~" -delete
	-find ../ -name ".#*" -delete

recursive-clean ::
	gnatclean -F -r -P wisitoken_test.gpr

BRANCH := $(notdir $(shell cd ..; pwd))

bz2file : force
	rm -rf ../../$(BRANCH)-$(ZIP_VERSION)
	mtn checkout --branch $(BRANCH) ../../$(BRANCH)-$(ZIP_VERSION)
	tar -c -O  -C ../.. --exclude=_MTN --exclude "build/x86*" --exclude=.mtn-ignore --exclude=.dvc-exclude --exclude debug_parser.adb --no-anchor $(BRANCH)-$(ZIP_VERSION) | bzip2 -9 > wisitoken-$(ZIP_VERSION).tar.bz2

zipfile : ROOT := $(shell cd ..; basename `pwd`)
zipfile : force
	cd ../..; zip -q -r $(CURDIR)/wisitoken-$(ZIP_VERSION).zip $(BRANCH)-$(ZIP_VERSION) -x "$(ROOT)-$(ZIP_VERSION)/_MTN/*" -x "$(ROOT)-$(ZIP_VERSION)/build/x86_*" -x "$(ROOT)-$(ZIP_VERSION)/.mtn-ignore" -x "$(ROOT)-$(ZIP_VERSION)/.dvc-exclude" -x "$(ROOT)-$(ZIP_VERSION)/debug_parser.adb"

.PRECIOUS : %.ada %.ads %_run.exe %.l %.parse %-process.el %.qx %.re2c %-wy.el

vpath %-wy.good_el  ../wisi/test
vpath %.good_parse  ../wisi/test
vpath %.input  ../wisi/test
vpath %.texinfo ../Docs
vpath %.wy ../wisi/test ../time

# end of file
