# makefile rules for WisiToken

# note that we use .exe for test executables even on non-windows, to
# keep the makerules simpler.

.PRECIOUS : %.exe %.out %.re2c %_packrat.re2c

.PHONY : zip force

VPATH := ..
VPATH += ../test

vpath %.wy ../test/bnf ../Docs/error_correction_examples

vpath %.texinfo ../Docs
vpath %.tex ../Docs

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

# generated code used by test_bnf_suite.adb and others.
# If add to this, add to wisitoken_test.gpr
gen_BNF :: wisitoken-parse-lr-mckenzie_recover-ada_lite.adb
gen_BNF :: wisitoken-parse-lr-mckenzie_recover-ada_lite.ads
gen_BNF :: ada_lite_re2c.c
gen_BNF :: body_instantiation_conflict_re2c.c
gen_BNF :: case_expression_re2c.c
gen_BNF :: character_literal_re2c.c
gen_BNF :: conflict_name_re2c.c
gen_BNF :: dragon_4_43_re2c.c
gen_BNF :: empty_production_1_re2c.c
gen_BNF :: empty_production_2_re2c.c
gen_BNF :: empty_production_3_re2c.c
gen_BNF :: empty_production_4_re2c.c
gen_BNF :: empty_production_5_re2c.c
gen_BNF :: empty_production_6_re2c.c
gen_BNF :: empty_production_7_re2c.c
gen_BNF :: empty_production_8_re2c.c
gen_BNF :: range_conflict_re2c.c
gen_BNF :: skip_to_grammar_re2c.c
gen_BNF :: warth_left_recurse_expr_1_re2c.c

gen_EBNF :: ada_ebnf_bnf.wy # not a valid grammar
gen_EBNF :: ada_lite_ebnf_re2c.c
gen_EBNF :: identifier_list_name_conflict_re2c.c
gen_EBNF :: java_ebnf_bnf.wy # not a valid grammar
gen_EBNF :: java_enum_ch19_re2c.c
gen_EBNF :: java_expressions_antlr_re2c.c
gen_EBNF :: java_expressions_ch19_re2c.c
gen_EBNF :: java_types_ch19_re2c.c
gen_EBNF :: lalr_generator_bug_01_re2c.c
gen_EBNF :: nested_ebnf_optional_re2c.c
gen_EBNF :: python_ebnf_bnf.wy # not a valid grammar
gen_EBNF :: subprograms_re2c.c
gen_EBNF :: three_action_conflict_re2c.c
gen_EBNF :: wisitoken-parse-lr-mckenzie_recover-ada_lite_ebnf.adb
gen_EBNF :: wisitoken-parse-lr-mckenzie_recover-ada_lite_ebnf.ads

gen_Tree_Sitter :: ada_lite_ebnf_tree_sitter.c

GENERATE ?= all
ifeq ($(GENERATE),all)
gen : gen_BNF gen_EBNF gen_Tree_Sitter
else ifeq ($(GENERATE),BNF)
gen : gen_BNF
else ifeq ($(GENERATE),EBNF)
gen : gen_EBNF
else ifeq ($(GENERATE),Tree_Sitter)
gen : gen_Tree_Sitter
endif

test_all_harness.out : test_all_harness.exe wisitoken-bnf-generate.exe gen test-executables

install: library wisitoken-bnf-generate.exe
	make -f Install.make install

uninstall:
	make -f Install.make install-clean

library:
	gprbuild -p -j8 --RTS=$(ADA_RUN_TIME) -Pwisitoken_lib

clean :: test-clean
	rm -rf obj *.exe
	rm -rf obj_pro exec_pro
	rm -rf libzcx libsjlj libobjzcx libobjsjlj

# don't delete prj.el
test-clean :
	rm -f *.ad? *.c *.diff *-process.el *.in *.js *.re2c *.exe *.out *.parse* *.txt *.wy

# We want the files generated for wisitoken_grammar.wy in ../, for CM,
# and to avoid deleting them in clean. We don't include
# wisitoken-bnf-generate.exe in the dependencies here, to allow
# bootstrapping.
../wisitoken_grammar.re2c : ../wisitoken_grammar.wy
	cd ../; $(CURDIR)/wisitoken-bnf-generate.exe wisitoken_grammar.wy
	dos2unix ../wisitoken_grammar*

../wisitoken_grammar_re2c.c : ../wisitoken_grammar.re2c
	re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<
	dos2unix ../wisitoken_grammar_re2c.c

wisitoken_grammar-clean :
	cd ../; rm -rf wisitoken_grammar_actions.ad? wisitoken_grammar_main.ad? wisitoken_grammar*.parse_table wisitoken_grammar.re2c wisitoken_grammar_re2c.c wisitoken_grammar_re2c_c.ads

update-wisitoken_grammar : wisitoken_grammar-clean ../wisitoken_grammar_re2c.c

# Executables are normally compiled by the test project file, which requires AUnit
# Override the project file for wisitoken-bnf-generate.exe, for use with Emacs Ada mode without AUnit
wisitoken-bnf-generate.exe : force
	gprbuild -p -j8 -P wisitoken.gpr wisitoken-bnf-generate

wisitoken-to_tree_sitter.exe : force
	gprbuild -p -j8 -P wisitoken.gpr wisitoken-to_tree_sitter

wisitoken-followed_by.exe : force
	gprbuild -p -j8 -P wisitoken.gpr wisitoken-followed_by

test-executables : force
	gprbuild -p -j8 --autoconf=obj/auto.cpgr -P wisitoken_test.gpr

wisitoken-parse-lr-mckenzie_recover-ada_lite.% : wisitoken-parse-lr-mckenzie_recover-ada_lite.%.gp
	gnatprep -b -r -T -DADA_LITE=Ada_Lite $^ $@

wisitoken-parse-lr-mckenzie_recover-ada_lite_ebnf.% : wisitoken-parse-lr-mckenzie_recover-ada_lite.%.gp
	gnatprep -b -r -T -DADA_LITE="Ada_Lite_Ebnf" $^ $@

%.out : %.exe
	./$*.exe $(RUN_ARGS) > $*.out
	dos2unix -q $*.out

DIFF_OPT := -u -w
%.diff : %.good_out %.out ; diff $(DIFF_OPT) $^ > $@

%.run : %.exe ;	./$(*F).exe $(RUN_ARGS)

%.re2c : %.wy wisitoken-bnf-generate.exe
	./wisitoken-bnf-generate.exe --output_bnf --test_main $(GENERATE_ARGS) $<
	dos2unix -q $**

%.exe : force; gprbuild -p -j8 -P wisitoken_test.gpr $(GPRBUILD_ARGS) $*

%_re2c.c : %.re2c
	re2c --debug-output --input custom -W -Werror --utf-8 -o $@ $<
	dos2unix $*_re2c.c

%_tree_sitter.c : %.wy wisitoken-bnf-generate.exe
	tree-sitter generate ./$*.js
	mv src/parser.c $*_tree_sitter.c

%_bnf.wy : %.wy wisitoken-bnf-generate.exe
	./wisitoken-bnf-generate.exe --output_bnf --generate None $<
	dos2unix -q $@

source-clean ::
	-find ../ -name "*~" -delete
	-find ../ -name ".#*" -delete

recursive-clean ::
	gnatclean -F -r -P wisitoken_test.gpr

BRANCH := $(notdir $(shell cd ..; pwd))

zip : force
	rm -rf ../../$(BRANCH)-$(ZIP_VERSION)
	mkdir ../../$(BRANCH)-$(ZIP_VERSION)
	cd ..; git archive $(BRANCH) | tar -x -C ../$(BRANCH)-$(ZIP_VERSION)
	tar -c -C ../.. $(BRANCH)-$(ZIP_VERSION) | bzip2 -9 > wisitoken-$(ZIP_VERSION).tar.bz2

tag :
	git tag $(BRANCH)-$(ZIP_VERSION) $(BRANCH)

# end of file
