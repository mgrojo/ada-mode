# gnu make rules for building with Alire.

# alr must be started in the directory holding alire.toml
alire-build : force
	GPR_PROJECT_PATH= ; alr --no-tty --no-color $(ALIRE_ARGS) build $(ALIRE_BUILD_ARGS)

alire-env :
	GPR_PROJECT_PATH= ;  alr $(ALIRE_ARGS) printenv

WISITOKEN_GENERATE := $(WISITOKEN_ALIRE_PREFIX)/build/bin/wisitoken-bnf-generate.exe

$(WISITOKEN_GENERATE) :
	$(MAKE) -C $(WISITOKEN_ALIRE_PREFIX) build/bin/wisitoken-bnf-generate.exe

%.re2c : %.wy $(WISITOKEN_GENERATE)
	$(WISITOKEN_GENERATE) $(IGNORE_CONFLICTS) --verbosity "time=1" --output_bnf $(<F)

%_re2c.c : %.re2c
	re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<

.PHONY : force

# end of file
