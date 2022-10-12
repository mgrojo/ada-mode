#!/bin/sh
# Build executables for Ada mode.
#    build.sh <other gprbuild options>
#    e.g. 'build.sh -j0' : use all available processors to compile
#         'build.sh -wn' : treat warnings as warnings.
#         'build.sh -vh' : Verbose output (high verbosity)
#
# See install.sh for install

if type alr; then
    echo "building ada-mode executables via Alire"
    alr get emacs_ada_mode

elif type gprbuild; then
    echo "building ada-mode executables via gnat compiler"
    
    if [ -d ../wisi-4.1.? ]; then
        WISI_DIR=`ls -d ../wisi-4.1.?`
    else
        # try devel version
        WISI_DIR=`find .. -type d -name "wisi-4.1.beta*"`
    fi

    args=`echo -DELPA="yes" $WISI_DIR/wisi.gpr.gp $WISI_DIR/wisi.gpr`
    echo "gnatprep " $args
    gnatprep $args

    # We don't add WISI_DIR to GPR_PROJECT_PATH because the user may have
    # already set GPR_PROJECT_PATH.

    # Allow running build.sh again, since it often fails the first time.
    #  - Run gprclean, to allow changing compilers and other drastic things

    gprclean -r -P ada_mode_wisi_parse.gpr -aP$WISI_DIR

    gprbuild -p -j8 -P $WISI_DIR/wisi.gpr wisitoken-bnf-generate

    # We generate the Ada LR1 parse table .txt file here, because it is too
    # large to keep in ELPA. The code generated by wisitoken-bnf-generate
    # is in ELPA, because it requires the re2c tool, which we don't expect
    # users to have installed. The LR1 parse table for gpr is in the Ada
    # code, so we don't need to generate that here.
    echo "generate Ada LR1 parse table"
    $WISI_DIR/wisitoken-bnf-generate --task_count 1 ada_annex_p.wy

    gprbuild -p -j8 -P ada_mode_wisi_parse.gpr -aP $WISI_DIR "$@"

else
    echo "neither Alire nor gnat compiler found"
    return 1
fi

# end of file
