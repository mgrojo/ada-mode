all: dirs tests examples

# ../../Test/test_one_harness.adb
t_one : test_one_harness.run
t_two : test_debug_harness.run

t_all : test_all_harness.run

one : RUN_ARGS=t
one : association_token_test-run.run

trace :
	addr2line -e token_selection_test-run.exe 0x805b2e7 0x805cfd3 0x8067ed9 0x804d36b 0x40345453

dirs :: lib

lib:
	@mkdir -p lib

clean ::
	@rm -rf lib/*

distclean ::
	@rm -rf lib

include ../common.make

GNAT_PROJECT := opentoken_test.gpr

# Local Variables:
# eval: (ada-parse-prj-file "opentoken.prj")
# eval: (ada-select-prj-file "opentoken.prj")
# end:
# end of file
