#!/bin/sh
# Install executables for gpr-query
#
# See build.sh for build (must be run before install).

# $1 : optional --prefix=<dir>
#
# If you don't have write permission in the GNAT installation
# directory, you need to use --prefix=<dir>, or run with root priviledges.

# FIXME: check for 'alr', use it. devel version has 'install'.

gprinstall -f -p -P emacs_gpr_query.gpr --install-name=emacs_gpr_query $1 

# end of file.
