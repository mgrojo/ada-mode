# makefile rules for WisiToken

# note that we use .exe for test executables even on non-windows, to
# keep the makerules simpler.

.PRECIOUS : %.exe %.out %.re2c %_packrat.re2c

.PHONY : zip force

VPATH := ../..
VPATH += ../Test
VPATH += ../wisi
VPATH += ../wisi/test

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

tests :: wisi-generate.exe
tests :: ada_lite_re2c.c
tests :: character_literal_re2c.c
tests :: skip_to_grammar_re2c.c
tests :: dragon_4_43_packrat_re2c.c
tests :: warth_left_recurse_expr_1_re2c.c

tests :: test_all_harness.diff

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

test-clean : wisi-clean re2c-clean
	rm -f *.diff *.in *_run.exe *-run.exe *test.exe *.out *.parse *.txt *.wy
	rm -f wisi/*.el wisi/*.re2c wisi/*.ad? wisi/*.exe wisi/*.parse* wisi/*.c

source-clean ::
	-find $(SOURCE_ROOT) -name "*~" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name ".#*" -print | xargs rm -v
	-find $(SOURCE_ROOT) -name "*,t" -print | xargs rm -v

# We want the files generated for wisi_grammar in ../wisi, for CM, and to avoid deleting them in wisi-clean.
# We don't include wisi-generate.exe in the dependencies here, to allow bootstrapping.
../wisi/wisi_grammar.re2c : wisi_grammar.wy
	cd ../wisi; $(CURDIR)/wisi-generate.exe wisi_grammar.wy
	dos2unix ../wisi/wisi_grammar*

../wisi/wisi_grammar_re2c.c : ../wisi/wisi_grammar.re2c
	re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<
	dos2unix ../wisi/wisi_grammar_re2c.c

wisi_grammar-clean :
	rm -rf wisi_grammar*
	cd ../wisi/; rm -rf wisi_grammar*.ad? wisi_grammar.parse_table wisi_grammar.re2c wisi_grammar_re2c.c wisi_grammar_re2c_c.ads

update-wisi_grammar : wisi_grammar-clean ../wisi/wisi_grammar_re2c.c

# Executables are normally compiled by the test project file, which requires AUnit
# Override the project file for wisi-generate.exe, for use with Emacs Ada mode without AUnit
wisi-generate.exe : force
	gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P wisitoken.gpr $(GPRBUILD_ARGS) wisi-generate $(GPRBUILD_LINK_ARGS)

%.out : %.exe
	./$*.exe $(RUN_ARGS) > $*.out
	dos2unix -q $*.out

DIFF_OPT := -u -w
%.diff : %.good_out %.out ; diff $(DIFF_OPT) $^ > $@

%.run : %.exe ;	./$(*F).exe $(RUN_ARGS)

%.re2c : %.wy wisi-generate.exe
	./wisi-generate.exe $<
	dos2unix -q $**

%_packrat.re2c : %.wy wisi-generate.exe
	./wisi-generate.exe --suffix "_packrat" --generate Packrat_Gen Ada $<
	dos2unix -q $**

# delete files created by wisi-generate in tests
# don't delete prj.el
wisi-clean :
	rm -f *-elisp.el *-process.el *.*parse_table *.ads *.adb

%.exe : force; gprbuild -p --autoconf=obj/auto.cgpr --target=$(GPRBUILD_TARGET) -P wisitoken_test.gpr $(GPRBUILD_ARGS) $*

%_re2c.c : %.re2c
	$(RE2C_HOME)/re2c --debug-output --input custom -W -Werror --utf-8 -o $@ $<
	dos2unix $*_re2c.c

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

# end of file
