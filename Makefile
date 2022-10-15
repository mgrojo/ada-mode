# Build WisiToken with Alire; see build/Makefile for non-Alire build

STEPHES_ADA_LIBRARY_ALIRE_PREFIX ?= $(CURDIR)/../org.stephe_leake.sal

include $(STEPHES_ADA_LIBRARY_ALIRE_PREFIX)/build/alire_rules.make

# This rule is used by client crate actions to build process their grammar file
# Therefore wisitoken_alire_mains.gpr does _not_ need to be in alire.toml project-files
build/bin/wisitoken-bnf-generate.exe :
	cd build; gprbuild -P wisitoken_alire_mains.gpr wisitoken-bnf-generate

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj-eglot.el"))
# End:
# end of file
