# gnu make rules for GNAT, using gprbuild
# standard settings are in standard_common.gpr.

.PHONY : force

# Main compile and link rule. Note that there is no way to specify the
# project search path on the gprbuild command line; it must be in the
# environment variable GPR_PROJECT_PATH
# We don't include -k here; that would run script tests even if building the .exe fails.
%.exe : %.adb force; gprbuild -p --autoconf=objects/auto.cgpr -P$(GNAT_PROJECT) $(GPRBUILD_ARGS) $*.adb

exec_pro/%.exe : %.adb force
	gprbuild -p --autoconf=objects/auto.cgpr -XStandard_Common_Profile=On -P$(GNAT_PROJECT) $(GPRBUILD_ARGS) $*

exec_mem/%.exe : %.adb force
	gprbuild -p --autoconf=objects/auto.cgpr -XStandard_Common_Memory=On -P$(GNAT_PROJECT) $(GPRBUILD_ARGS) $*

# Ada syntax check only; much faster when things are changing.
%.check : %.adb force; gnatmake -p -k -gnatc -P$(GNAT_PROJECT) $(GNATMAKE_ARGS) $*

# We don't use gnatclean because it only removes compiler-generated
# files that the project file knows about; that usually does not
# include most test executables. We don't clean inherited projects,
# because that's almost always a bad idea. We use several commands, so
# the command line doesn't get too long on large directories.
clean ::
	rm -f *.TMP *.exe *.bexch
	rm -rf objects

# this can be useful when experimenting with different compiler versions
recursive-clean ::
	gnatclean -F -r -P$(GNAT_PROJECT)

# end of file
