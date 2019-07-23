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

SPARK_FILES += prove_bounded_definite_queues.ads
SPARK_FILES += prove_bounded_definite_stacks.ads
SPARK_FILES += prove_bounded_definite_vectors.ads

clean :: test-clean
clean :: profile-clean

test-clean ::
	rm -f *.config
	rm -f *.csv

profile-clean ::
	rm -rf exec_pro
	rm -rf obj_pro
	rm -rf exec_mem
	rm -rf obj_mem
# end of file
