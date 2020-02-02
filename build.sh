#!/bin/sh
# Build executables for Ada mode.
#
# See install.sh for install

# support for libadalang is still experimental
gnatprep  -DHAVE_LIBADALANG="no" -DELPA="yes" ada_mode_wisi_parse.gpr.gp ada_mode_wisi_parse.gpr

export GPR_PROJECT_PATH="../wisi-3.0.1"

gnatprep -DELPA="yes" $GPR_PROJECT_PATH/wisi.gpr.gp $GPR_PROJECT_PATH/wisi.gpr

gprbuild -p -j8 -P ada_mode_wisi_parse.gpr
gzip -d -q ada_lr1_parse_table.txt.gz

# end of file
