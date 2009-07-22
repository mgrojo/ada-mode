all: dirs tests examples

# ../../Test/test_one_harness.adb
#one : test_one_harness.run
one : asu_example_4_46_rd-run.run
#one : asu_example_5_10-run.run
#one : asu_example_5_10_rd-run.run
#one : RUN_ARGS := < ../../Examples/asu_example_5_10/Example.txt
#one : asu_example_5_10_rd-run_bad.run

trace :
	addr2line -e asu_example_5_10_rd-run.exe 0x411a82 0x411aec 0x416f34 0x401783 0x401235 0x401286 0x7c817075

# adactl won't use -gnat05 to create the tree files, so we create the
# tree files via opentoken_tree.gpr. '-- -FT' says don't let adactl
# try.
check : tree
	../../../adactl-1.11r4/src/adactl.exe -f check_package_vars.aru ../../*.ad? ../../Language_Lexers/*.ad? -- -FT -Tobj_tree

tree : force
	gnatmake -P opentoken_tree.gpr

clean ::
	rm -f obj_tree/*

include ../common.make

GNAT_PROJECT := opentoken_test.gpr
GNAT_VERSION := 6.2.1

# Local Variables:
# eval: (ada-parse-prj-file "opentoken.prj")
# eval: (ada-select-prj-file "opentoken.prj")
# end:
# end of file
