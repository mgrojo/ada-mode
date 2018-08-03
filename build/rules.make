# makefile rules for WisiToken

# note that we use .exe for test executables even on non-windows, to
# keep the makerules simpler.

.PRECIOUS : %.exe %.out %.re2c %_packrat.re2c

.PHONY : zip force

VPATH := ../..
VPATH += ../Test
VPATH += ../Test/bnf

vpath %.texinfo ../Docs
vpath %.wy ../wisi/test

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

tests :: wisitoken-bnf-generate.exe
tests :: gen
tests :: test_all_harness.diff

# generated code used by test_wisi_suite.adb and others.
gen :: ada_lite_re2c.c
gen :: body_instantiation_conflict_re2c.c
gen :: case_expression_re2c.c
gen :: character_literal_re2c.c
gen :: conflict_name_re2c.c
gen :: dragon_4_43_re2c.c
gen :: empty_production_1_re2c.c
gen :: empty_production_2_re2c.c
gen :: empty_production_3_re2c.c
gen :: empty_production_4_re2c.c
gen :: empty_production_5_re2c.c
gen :: empty_production_6_re2c.c
gen :: empty_production_7_re2c.c
gen :: empty_production_8_re2c.c
gen :: identifier_list_name_conflict_re2c.c
gen :: range_conflict_re2c.c
gen :: skip_to_grammar_re2c.c
gen :: subprograms_re2c.c
gen :: warth_left_recurse_expr_1_re2c.c

test_all_harness.out : test_all_harness.exe wisitoken-bnf-generate.exe gen test-executables

install: library wisitoken-bnf-generate.exe
	make -f Install.make install

uninstall:
	make -f Install.make install-clean

library:
	gprbuild -p --RTS=$(ADA_RUN_TIME) -Pwisitoken_lib

clean :: test-clean
	rm -rf obj *.exe
	rm -rf obj_pro exec_pro
	rm -rf libzcx libsjlj libobjzcx libobjsjlj

# don't delete prj.el
test-clean :
	rm -f *.ad? *.c *.diff *-elisp.el *-process.el *.in *.re2c *.exe *.out *.parse* *.txt *.wy

source-clean ::
	-find $(SOURCE_ROOT) -name "*~" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name ".#*" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name "*,t" -print | xargs rm -v

# We want the files generated for wisi_grammar in ../wisi, for CM, and to avoid deleting them in clean.
# We don't include wisitoken-bnf-generate.exe in the dependencies here, to allow bootstrapping.
../wisi/wisi_grammar.re2c : wisi_grammar.wy
	cd ../wisi; $(CURDIR)/wisitoken-bnf-generate.exe wisi_grammar.wy
	dos2unix ../wisi/wisi_grammar*

../wisi/wisi_grammar_re2c.c : ../wisi/wisi_grammar.re2c
	re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<
	dos2unix ../wisi/wisi_grammar_re2c.c

wisi_grammar-clean :
	rm -rf wisi_grammar*
	cd ../wisi/; rm -rf wisi_grammar*.ad? wisi_grammar.parse_table wisi_grammar.re2c wisi_grammar_re2c.c wisi_grammar_re2c_c.ads

update-wisi_grammar : wisi_grammar-clean ../wisi/wisi_grammar_re2c.c

# Executables are normally compiled by the test project file, which requires AUnit
# Override the project file for wisitoken-bnf-generate.exe, for use with Emacs Ada mode without AUnit
wisitoken-bnf-generate.exe : force
	gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P wisitoken.gpr $(GPRBUILD_ARGS) wisitoken-bnf-generate $(GPRBUILD_LINK_ARGS)

test-executables : force
	gprbuild -p --autoconf=obj/auto.cpgr -P wisitoken_test.gpr

%.out : %.exe
	./$*.exe $(RUN_ARGS) > $*.out
	dos2unix -q $*.out

DIFF_OPT := -u -w
%.diff : %.good_out %.out ; diff $(DIFF_OPT) $^ > $@

%.run : %.exe ;	./$(*F).exe $(RUN_ARGS)

%.re2c : %.wy wisitoken-bnf-generate.exe
	./wisitoken-bnf-generate.exe --test_main $<
	dos2unix -q $**

%.exe : force; gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P wisitoken_test.gpr $(GPRBUILD_ARGS) $*

%_re2c.c : %.re2c
	$(RE2C_HOME)/re2c --debug-output --input custom -W -Werror --utf-8 -o $@ $<
	dos2unix $*_re2c.c

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

# end of file
