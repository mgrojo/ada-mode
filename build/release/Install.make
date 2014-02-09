# install OpenToken in GNAT tree
# (ediff "../windows_release/Makefile.install" "../linux_release/Makefile.install")

# Where opentoken library should be installed
# Default is into current GNAT directory
INSTALL_DIR ?= $(dir $(shell which gnatls))..
prefix=$(INSTALL_DIR)

all: install

# On Windows, *.dll is installed in bin directory for runtime, _and_ in lib directory for link time.
# FIXME: one of those should be named *.a?
# FIXME: on Linux?
I_BIN = $(prefix)/bin
I_INC = $(prefix)/include/opentoken
I_LIB = $(prefix)/lib/opentoken
I_GPR = $(prefix)/lib/gnat

# run this from the main Makefile to install the library
#
# we use "tar c | tar x" instead of "cp -p" because the latter is not
# available on some platforms
install: install-clean
	mkdir -p $(I_INC)
	mkdir -p $(I_LIB)
	mkdir -p $(I_GPR)
	(cd lib; tar cf - *.ali) | tar xf - -C $(I_LIB)
	chmod a-w $(I_LIB)/*.ali
	cp lib/libopentoken* $(I_BIN)
	chmod a-w $(I_BIN)/libopentoken*
	cp lib/libopentoken* $(I_LIB)
	chmod a-w $(I_LIB)/libopentoken*
	(cd ../../; tar cf - *.ad[bs]) | tar xf - -C $(I_INC)
	(cd ../../Language_Lexers; tar cf - *.ad[bs]) | tar xf - -C $(I_INC)
	(cd ../; tar cf - opentoken.gpr) | tar xf - -C $(I_GPR)

install-clean :
	rm -rf $(I_INC)
	rm -rf $(I_LIB)
	rm -f  $(I_GPR)/opentoken.gpr

# end of file
