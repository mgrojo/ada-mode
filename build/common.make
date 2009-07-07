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

dirs :: obj

obj:
	mkdir -p obj

tests : dirs

tests : association_token_test-run.diff
tests : bracketed_comment_test-run.run
tests : enumerated_token_list_test.run
tests : lookahead_test-run.run
tests : name_token_test-run.diff
tests : production_test-run.run
tests : recognizer_based_integer_test.run
tests : recognizer_integer_test.run
tests : string_test-run.run
tests : string_token_test-run.diff
tests : test_all_harness.diff
tests : token_analyzer_ctd-run.run
tests : token_list_test-run.run
tests : token_selection_test-run.run
tests : token_sequence_test-run.run

examples : asu_example_3_6-run.run
examples : asu_example_4_46-run.run
examples : asu_example_5_10-run.run
examples : ada_count.run

asu_example_3_6-run.run : asu_example_3_6-run.exe
	cd ../../Examples/ASU_Example_3_6; $(CURDIR)/asu_example_3_6-run.exe

asu_example_4_46-run.run : asu_example_4_46-run.exe
	cd ../../Examples/ASU_Example_4_46; $(CURDIR)/asu_example_4_46-run.exe

asu_example_5_10-run.run : asu_example_5_10-run.exe
	cd ../../Examples/ASU_Example_5_10; $(CURDIR)/asu_example_5_10-run.exe < Example.txt

ada_count.run : ada_count.exe
	./ada_count.exe ../../Examples/Language_Lexer_Examples/ada_count.adb ../../Examples/Language_Lexer_Examples/test_ada_lexer.adb

clean :: test-clean
	rm -f *.exe
	rm -f obj/*

test-clean :
	rm -f *.diff *.out *.txt

source-clean ::
	-find $(SOURCE_ROOT) -name "*~" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name ".#*" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name "*,t" -print | xargs rm -v

%.exe : %.adb force; gnatmake -k -C -P$(GNAT_PROJECT) $(GNATMAKE_ARGS) $* $(GNATMAKE_POST_ARGS)

%.out : %.exe ;	./$*.exe > $*.out 2>&1

DIFF_OPT := -u -w
%.diff : %.good_out %.out ; diff $(DIFF_OPT) $^ > $@

%.run : %.exe ;	./$(*F).exe $(RUN_ARGS)

# end of file
