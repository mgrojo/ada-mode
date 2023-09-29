# makefile rules for WisiToken

# note that we use .exe for test executables even on non-windows, to
# keep the makerules simpler.

.PRECIOUS : %.exe %.out %.re2c %_packrat.re2c

.PHONY : zip force

VPATH := ..
VPATH += ../test

vpath %.bib ../Docs
vpath %.js .
vpath %.tex ../Docs
vpath %.texinfo ../Docs
vpath %.wy ../test/bnf ../Docs/error_correction_examples

tests :: wisitoken_test.gpr
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
gen_BNF :: empty_production_2_optimized_list_re2c.c
gen_BNF :: empty_production_3_re2c.c
gen_BNF :: empty_production_4_re2c.c
gen_BNF :: empty_production_5_re2c.c
gen_BNF :: empty_production_6_re2c.c
gen_BNF :: empty_production_7_re2c.c
gen_BNF :: empty_production_8_re2c.c
gen_BNF :: optimized_list_re2c.c
gen_BNF :: prec_assoc_re2c.c
gen_BNF :: range_conflict_re2c.c
gen_BNF :: skip_to_grammar_re2c.c
gen_BNF :: warth_left_recurse_expr_1_re2c.c

gen_EBNF :: ada_ebnf_re2c.c
gen_EBNF :: ada_lite_ebnf_re2c.c
gen_EBNF :: grammar_grammar_01_re2c.c
gen_EBNF :: identifier_list_name_conflict_re2c.c
gen_EBNF :: java_ebnf_bnf.wy # not a valid grammar
gen_EBNF :: java_enum_ch19_re2c.c
gen_EBNF :: java_expressions_antlr_re2c.c
gen_EBNF :: java_expressions_ch19_re2c.c
gen_EBNF :: java_types_ch19_re2c.c
gen_EBNF :: lalr_generator_bug_01_re2c.c
gen_EBNF :: nested_ebnf_optional_re2c.c
gen_EBNF :: optimized_conflict_01_re2c.c
gen_EBNF :: optimized_conflict_02_re2c.c
gen_EBNF :: optimized_conflict_03_re2c.c
gen_EBNF :: optimized_conflict_04_re2c.c
gen_EBNF :: optimized_list_ebnf_re2c.c
gen_EBNF :: prec_assoc_ebnf_re2c.c
gen_EBNF :: python_ebnf_bnf.wy # not a valid grammar
gen_EBNF :: subprograms_re2c.c
gen_EBNF :: three_action_conflict_re2c.c
gen_EBNF :: wisitoken-parse-lr-mckenzie_recover-ada_lite_ebnf.adb
gen_EBNF :: wisitoken-parse-lr-mckenzie_recover-ada_lite_ebnf.ads

# Not included in above so we can skip if tree-sitter is not
# installed.
gen_Tree_Sitter :: ada_lite_ebnf_tree_sitter.c
gen_Tree_Sitter :: ada_lite_tree_sitter.c
gen_Tree_Sitter :: character_literal_tree_sitter.c
gen_Tree_Sitter :: dragon_4_43_tree_sitter.c
gen_Tree_Sitter :: prec_assoc_tree_sitter.c
gen_Tree_Sitter :: prec_assoc_ebnf_tree_sitter.c

# GENERATE is used by wisitoken_test.gpr; see there for valid values.
# We assume tree-sitter is not installed; can be overridden by user.
GENERATE ?= BNF_EBNF_Tree_Sitter
ifeq ($(GENERATE),BNF_EBNF_Tree_Sitter)
gen :: gen_BNF gen_EBNF gen_Tree_Sitter
else ifeq ($(GENERATE),BNF_EBNF)
gen :: gen_BNF gen_EBNF
else ifeq ($(GENERATE),BNF)
gen :: gen_BNF
else ifeq ($(GENERATE),EBNF)
gen :: gen_EBNF
else ifeq ($(GENERATE),Tree_Sitter)
gen :: gen_Tree_Sitter
endif

test_all_harness.out : test_all_harness.exe wisitoken-bnf-generate.exe gen test-executables

clean :: test-clean
	rm -f Makefile.conf
	rm -rf obj bin devel_obj
	rm -rf obj_pro exec_pro
	rm -f error_correction_algorithm.ps
	rm -rf bindings src Cargo.toml package.json

# don't delete prj.el, release_process.text
test-clean :
	rm -f binding.gyp *.ad? *.c *.diff *-process.el *.in *.js *.re2c *.exe *.out *.parse* *.set_table *.txt *.wy
	rm -f *.wy.errors *.tree_text

# We want the files generated for wisitoken_grammar.wy in ../, for CM,
# and to avoid deleting them in clean. We don't include
# wisitoken-bnf-generate.exe in the dependencies here, to allow
# bootstrapping.
../wisitoken_grammar.re2c : ../wisitoken_grammar.wy
	cd ../; $(CURDIR)/wisitoken-bnf-generate.exe wisitoken_grammar.wy
	dos2unix -q ../wisitoken_grammar*

../wisitoken_grammar_re2c.c : ../wisitoken_grammar.re2c
	re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<
	dos2unix -q ../wisitoken_grammar_re2c.c

wisitoken_grammar-clean :
	cd ../; rm -rf wisitoken_grammar_actions.ad? wisitoken_grammar_main.ad? wisitoken_grammar*.parse_table wisitoken_grammar.re2c wisitoken_grammar_re2c.c wisitoken_grammar_re2c_c.ads

update-wisitoken_grammar : wisitoken_grammar-clean ../wisitoken_grammar_re2c.c

# Executables are normally compiled by the test project file, which requires AUnit
# Override the project file for wisitoken-bnf-generate.exe, for use with Emacs Ada mode without AUnit
wisitoken-bnf-generate.exe : obj/s-memory.ali force
	gprbuild -p -j8 $(GPRBUILD_ARGS) -P wisitoken.gpr wisitoken-bnf-generate

# We must compile s-memory.adb specially, because it overrides a system package
obj/s-memory.ali :
	gprbuild -c -P wisitoken.gpr s-memory.adb

wisitoken-to_tree_sitter.exe : force
	gprbuild -p -j8 -P wisitoken.gpr wisitoken-to_tree_sitter

wisitoken-followed_by.exe : force
	gprbuild -p -j8 -P wisitoken.gpr wisitoken-followed_by

test-executables : force wisitoken_test.gpr
	gprbuild -p -j8 -P wisitoken_test.gpr $(GPRBUILD_ARGS)

# gprbuild can run gnatprep as part of the compiler, but that requires
# putting the gnatprep symbols in the .ad? file, which means we have
# no way to distinguish between ada_lite and ada_lite_ebnf.
wisitoken-parse-lr-mckenzie_recover-ada_lite.% : wisitoken-parse-lr-mckenzie_recover-ada_lite.%.gp
	gnatprep -b -r -T -DADA_LITE=Ada_Lite $^ $@

wisitoken-parse-lr-mckenzie_recover-ada_lite_ebnf.% : wisitoken-parse-lr-mckenzie_recover-ada_lite.%.gp
	gnatprep -b -r -T -DADA_LITE="Ada_Lite_Ebnf" $^ $@

%.out : %.exe
	./$(*F).exe $(RUN_ARGS) > $*.out
	dos2unix -q $*.out

DIFF_OPT := -u -w
%.diff : %.good_out %.out ; diff $(DIFF_OPT) $^ > $@

%.run : %.exe ;	./$(*F).exe $(RUN_ARGS)

%.re2c %.js : %.wy wisitoken-bnf-generate.exe
	./wisitoken-bnf-generate.exe --output_bnf --test_main $(GENERATE_ARGS) $<
	dos2unix -q $**_actions.adb $**_actions.ads $*.js $*_bnf.wy $**_main.adb $**.parse_table

%.exe : force; gprbuild -p -j8 -P wisitoken_test.gpr $(GPRBUILD_ARGS) $*

%_re2c.c : %.re2c
	re2c --location-format gnu --debug-output --input custom -W -Werror --utf-8 -o $@ $<
	dos2unix $*_re2c.c

%_tree_sitter.c : %.js
	tree-sitter generate $^
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
