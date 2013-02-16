# run tests on Emacs smie Ada mode indentation engine

# no gpr-smie.el
all : compile-clean compile test-clean test-ada test-elisp

one : ada_mode.ads.diff

RUNTEST := run-indent-test-smie.el

include ../common.make

# FIXME: delete currently broken tests
ADA_TEST_FILES := $(filter-out ada_mode-conditional_expressions.adb, $(ADA_TEST_FILES)) # indentation
ADA_TEST_FILES := $(filter-out which_test.adb, $(ADA_TEST_FILES)) # imenu-default-create-index-function
# end FIXME:

test-ada : $(addsuffix .diff, $(subst subdir/,,$(ADA_TEST_FILES)))
export ADA_PROJECT_PATH=../../test/;../../test/subdir

# Local Variables:
# eval: (ada-parse-prj-file "../../test/subdir/ada_mode.adp")
# eval: (ada-select-prj-file "../../test/subdir/ada_mode.adp")
# End:
# end of file
