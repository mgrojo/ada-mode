# gnu make rules for using wisitoken-bnf-generate with Alire.

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

# This is run before gprbuild, so the bin dir is not yet created.
%.re2c : %.wy
	$(WISITOKEN_GENERATE) $(IGNORE_CONFLICTS) --output_bnf $(<F)
	if [ -f $(*F)_*parse_table.txt ]; then mkdir -p bin; for file in $(*F)_*parse_table.txt; do mv $$file bin; done fi

%_re2c.c : %.re2c
	re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<

# end of file
