# makefile rules for WisiToken

# note that we use .exe for test executables even on non-windows, to
# keep the makerules simpler.

.PRECIOUS : %.exe %.out

.PHONY : zip force

VPATH := ../..
VPATH += ../Test
VPATH += ../wisi
VPATH += ../wisi/test

vpath %-wy.good_el  ../wisi/test
vpath %.good_parse  ../wisi/test
vpath %.input  ../wisi/test
vpath %.texinfo ../Docs
vpath %.wy ../wisi/test ../time

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
tests : skip_to_grammar_re2c.c
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

#tests : case_expression-elisp.el.diff done in wisi_wy_test.adb
tests : case_expression_re2c.c
tests : case_expression-parse.diff
tests : character_literal_re2c.c
tests : character_literal-parse.diff
tests : conflict_name_re2c.c
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
tests : subprograms_process_actions.adb.diff

# we don't run subprograms-parse because subprograms is used in a real
# Emacs Ada mode test, so it relies on the wisi Ada Emacs runtime,
# which is in Ada mode, not here.

test_all_harness.out : test_all_harness.exe wisi-generate.exe

install: library wisi-generate.exe
	make -f Install.make install

uninstall:
	make -f Install.make install-clean

library:
	gprbuild -p --RTS=$(ADA_RUN_TIME) -Pwisitoken_lib

# We do not include wisi-clean here, since that would prevent bootstrapping.
clean :: test-clean
	rm -rf obj *.exe
	rm -rf obj_pro exec_pro
	rm -rf libzcx libsjlj libobjzcx libobjsjlj

test-clean : wisi-clean re2c-clean
	rm -f *.diff *.in *_run.exe *-run.exe *test.exe *.out *.parse *.txt *.wy

source-clean ::
	-find $(SOURCE_ROOT) -name "*~" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name ".#*" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name "*,t" -print | xargs rm -v

# We want the files generated for wisi_grammar in ../wisi.
# We don't include wisi-generate.exe in the dependencies here, to allow bootstrapping.
../wisi/wisi_grammar.re2c : wisi_grammar.wy
	cd ../wisi; $(CURDIR)/wisi-generate.exe -v 1 wisi_grammar.wy > wisi_grammar.parse_table

../wisi/wisi_grammar_re2c.c : ../wisi/wisi_grammar.re2c
	$(RE2C_HOME)/bin/re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<

wisi_grammar-clean :
	rm -rf wisi_grammar*
	cd ../wisi/; rm -rf wisi_grammar*.ad? wisi_grammar.parse_table wisi_grammar.re2c wisi_grammar_re2c.c wisi_grammar_re2c_c.ads

update-wisi_grammar : wisi_grammar-clean ../wisi/wisi_grammar_re2c.c

# the test executables are only in the test project file, which requires AUnit
# Override the project file for wisi-generate.exe, for use with Emacs Ada mode without AUnit
wisi-generate.exe : force
	gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P wisitoken.gpr $(GPRBUILD_ARGS) wisi-generate $(GPRBUILD_LINK_ARGS)

%.check : %.adb force; gnatmake -p -k -gnatc -Pwisitoken_test.gpr $(GNATMAKE_ARGS) $*

%.out : %.exe
	./$*.exe $(RUN_ARGS) > $*.out 2>&1
	dos2unix $*.out

DIFF_OPT := -u -w
%.diff : %.good_out %.out ; diff $(DIFF_OPT) $^ > $@

%.diff : %.good_el %.el ; diff $(DIFF_OPT) $^ > $@

%_process_actions.adb.diff : %_process_actions.adb.good %_process_actions.adb
	diff $(DIFF_OPT) $^ > $@
	diff $(DIFF_OPT) ../wisi/test/$*_process_main.adb.good $*_process_main.adb >> $@
	diff $(DIFF_OPT) ../wisi/test/$*.good_re2c $*.re2c >> $@
	diff $(DIFF_OPT) ../wisi/test/$*-process.good_el $*-process.el >> $@

# the parse_table and the state trace of the parse is the known good output
%-parse.diff : %.good_parse %.parse
	diff $(DIFF_OPT) $(^:parse=parse_table) > $@
	diff $(DIFF_OPT) $^ >> $@

%-process.el %_process_actions.adb : %.wy wisi-generate.exe
	./wisi-generate.exe -v 1 --output_language Ada_Emacs --lexer re2c --interface process $< > $*.parse_table
	dos2unix $*.parse_table
	dos2unix $*_process_actions.adb
	dos2unix $*_process_main.adb
	dos2unix $*.re2c
	dos2unix $*-process.el

%-process.el.diff : %-process.good_el %-process.el
	diff $(DIFF_OPT) $< $*-process.el > $@

%.run : %.exe ;	./$(*F).exe $(RUN_ARGS)

# We assume lexer is specified in the .wy file. wisi-generate also generates other files.
%.re2c %.qx %.l : %.wy wisi-generate.exe
	./wisi-generate.exe -v 1 $< > $*.parse_table
	dos2unix $*.parse_table

# delete files created by wisi-generate
# don't delete prj.el
wisi-clean :
	rm -f *-elisp.el *-process.el *.parse_table *.ads *.adb  *.l *.qx

# -v 2 gives stack trace
%.parse : %.input %_run.exe
	./$*_run.exe -v 2 $< > $*.parse
	dos2unix $*.parse

%.exe : force; gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P wisitoken_test.gpr $(GPRBUILD_ARGS) $*

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

.PRECIOUS : %.ada %.ads %_run.exe %.l %.parse %-process.el %_process.adb %.qx %.re2c %-wy.el

# end of file
