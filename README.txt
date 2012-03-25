When scheme or Ada source changes, check/update the following:

build/Makefile ARM_VERSION          four digit year

build/Makefile ARM_VERSION_OPTION   command line option to arm_form.exe to choose current version

build/Makefile DEBIAN_VERSION       reset to .1, or bump if .1 for current ARM_VERSION was published

progs/arm_texinfo.adb Index_Clause  section number corresponding to index
    to find the new one:
        generate arm<version>.texinfo
        move to end of file
        search backward for "@section ... Index"; '...' is the index number

progs/arm_texinfo.adb * Z::         no entries for Z?
    look near end of arm<version>.texinfo
