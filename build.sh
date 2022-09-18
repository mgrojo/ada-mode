#!/bin/sh
# Build executables for Ada mode.
#    build.sh <other gprbuild options>
#    e.g. 'build.sh -j0' : use all available processors to compile
#         'build.sh -wn' : treat warnings as warnings.
#         'build.sh -vh' : Verbose output (high verbosity)
#
# See install.sh for install

# As of gnat pro 21, gnat_util is no longer provided or required
echo 'with "gnat_util"; abstract project check is end check;' > check.gpr
gprbuild -P check.gpr > /dev/null 2>&1
if test $? -eq 0 ; then
    HAVE_GNAT_UTIL=yes
else
    HAVE_GNAT_UTIL=no
fi

echo "gnatprep " $args
gnatprep $args

# FIXME: check for alr, use it

echo "FIXME: not using Alire; create config/emacs_ada_mode_config.gpr"

# Allow running build.sh again, since it often fails the first time.
#  - Run gprclean, to allow changing compilers and other drastic things

gprclean -r -P emacs_gpr_query.gpr

gprbuild -p -j8 -P emacs_gpr_query.gpr "$@"

# end of file
