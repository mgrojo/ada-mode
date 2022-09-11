# For compiling ada-mode Ada with Alire

include ../org.stephe_leake.makerules/alire_rules.make

WISITOKEN_GENERATE := $(WISITOKEN_ALIRE_PREFIX)/build/wisitoken-bnf-generate.exe

$(WISITOKEN_GENERATE) :
	$(MAKE) -C $(WISITOKEN_ALIRE_PREFIX) build/wisitoken-bnf-generate.exe

%.re2c : %.wy $(WISITOKEN_GENERATE)
	$(WISITOKEN_GENERATE) $(IGNORE_CONFLICTS) --verbosity "time=1" --output_bnf $(<F)

%_re2c.c : %.re2c
	re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<

# Local Variables:
# eval: (load-file "prj-eglot.el")
# End:
