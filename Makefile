# Build WisiToken with Alire; see build/Makefile for non-Alire build

include ../org.stephe_leake.makerules/alire_rules.make

build/wisitoken-bnf-generate.exe :
	cd build; gprbuild -P wisitoken_alire_mains.gpr wisitoken-bnf-generate

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj-eglot.el"))
# End:
# end of file
