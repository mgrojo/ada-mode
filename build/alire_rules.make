# gnu make rules for building with Alire.

# alr must be started in the directory holding alire.toml
alire-build : force
	GPR_PROJECT_PATH= ; alr --no-tty --no-color $(ALIRE_ARGS) build $(ALIRE_BUILD_ARGS)

alire-env :
	GPR_PROJECT_PATH= ;  alr $(ALIRE_ARGS) printenv

alire-clean :
	alr clean
	rm -rf alire build/obj/release build/obj/development

ifeq ($(shell uname),Linux)
ALIRE_PREFIX := $(WISITOKEN_ALIRE_PREFIX)
else ifeq ($(shell uname),Darwin)
ALIRE_PREFIX := $(WISITOKEN_ALIRE_PREFIX)
else
# Windows

# Use of anything from alire env requires special treatment on
# Windows; they use backslash directory separator, which mingw shell
# treats as an escape before it translates the filename to msys
# convention. So we use quotes and explicit translation via cygpath.
ALIRE_PREFIX := $(shell cygpath --unix "$(WISITOKEN_ALIRE_PREFIX)")
endif

WISITOKEN_GENERATE := $(ALIRE_PREFIX)/build/bin/wisitoken-bnf-generate.exe

# The wisitoken crate builds wisitoken-bnf-generate.exe, so we don't
# need a rule to build it here.
%.re2c : %.wy
	$(WISITOKEN_GENERATE) $(IGNORE_CONFLICTS) --verbosity "time=1" --output_bnf $(<F)
	for file in $(*F)_*parse_table.txt ; do mkdir -p bin; mv $file bin; done

%_re2c.c : %.re2c
	re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<

.PHONY : force

# end of file
