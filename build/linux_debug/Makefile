# (ediff "../windows_debug/Makefile" "../linux_debug/Makefile")
# schroot --chroot=unstable -- make -r all

all: test-clean tests examples library

# ../../Test/test_one_harness.adb
t_one : test_one_harness.run
t_all : test_all_harness.diff

one : RUN_ARGS := ../../wisi/test/body_instantiation_conflict.wy Elisp
one : wisi-generate.run

GPRBUILD_TARGET := x86-linux
include ../common.make

# Local Variables:
# eval: (load-file "../opentoken.el")
# eval: (ada-parse-prj-file "opentoken.prj")
# eval: (ada-select-prj-file "opentoken.prj")
# end:
# end of file
