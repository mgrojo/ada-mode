#!/bin/sh
# Build executables for gpr mode.
#    build.sh <other gprbuild options>
#    e.g. 'build.sh -j0' : use all available processors to compile
#         'build.sh -wn' : treat warnings as warnings.
#         'build.sh -vh' : Verbose output (high verbosity)
#
# See install.sh for install

if type alr; then
    echo "building gpr-mode executables via Alire"
    alr get emacs_gpr_mode~1.0.1
    cd emacs_gpr_mode_*; alr build --release

elif type gprbuild; then
    echo "building gpr-mode executables via gnat compiler"
    
    if [ -d ../wisi-4.1.? ]; then
        WISI_DIR=`ls -d ../wisi-4.1.?`
    fi

    args=`echo -DELPA="yes" $WISI_DIR/wisi.gpr.gp $WISI_DIR/wisi.gpr`
    echo "gnatprep " $args
    gnatprep $args

    # We don't add WISI_DIR to GPR_PROJECT_PATH because the user may have
    # already set GPR_PROJECT_PATH.

    # Allow running build.sh again, since it often fails the first time.
    #  - Run gprclean, to allow changing compilers and other drastic things

    gprclean -r -P gpr_mode_wisi_parse.gpr -aP$WISI_DIR

    gprbuild -p -j8 -P gpr_mode_wisi_parse.gpr -aP $WISI_DIR "$@"

else
    echo "neither Alire nor gnat compiler found"
    return 1
fi

# end of file
