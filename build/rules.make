# common parts of makefiles for SAL

include ../../org.stephe_leake.makerules/gprbuild_rules.make
include ../../org.stephe_leake.makerules/common_rules.make

# ignore whitespace in diff, so we can ignore Unix vs DOS line endings
# in test output files. Note that this has to come _after_
# common_rules.make.

DIFF_OPT = -u -w

vpath %.adb      ../source
vpath %.adb      ../test
vpath %.good_out ../test

test-clean ::
	rm -f *.config

# end of file
