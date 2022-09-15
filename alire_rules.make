# gnu make rules for building with Alire.

# alr must be started in the directory holding alire.toml
alire-build : force
	GPR_PROJECT_PATH= ; alr --no-tty --no-color $(ALIRE_ARGS) build $(ALIRE_BUILD_ARGS)

alire-env :
	GPR_PROJECT_PATH= ;  alr $(ALIRE_ARGS) printenv

.PHONY : force

# end of file
