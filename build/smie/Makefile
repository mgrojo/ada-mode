# run tests on Emacs smie Ada mode indentation engine

# no gpr-smie.el
all : compile-clean compile test-clean test-ada test-elisp

one : ada_mode.ads.diff

RUNTEST := runtest-smie.el

include ../common.make

export ADA_PROJECT_PATH=../../test/;../../test/subdir

# Local Variables:
# eval: (ada-parse-prj-file "../../test/subdir/ada_mode.adp")
# eval: (ada-select-prj-file "../../test/subdir/ada_mode.adp")
# End:
# end of file
