# gnu make rules for building with Alire.

# alr must be started in the directory holding alire.toml
alire-build : force
	GPR_PROJECT_PATH= ; alr --no-tty --no-color $(ALIRE_ARGS) build $(ALIRE_BUILD_ARGS)

alire-env :
	GPR_PROJECT_PATH= ;  alr $(ALIRE_ARGS) printenv

# there's no point in doing 'alr clean' here; 'rm' is faster, better, less error-prone.
alire-clean :
	rm -rf alire config build/obj/release build/obj/development

.PHONY : force

# end of file
