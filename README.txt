To do a release:

build/Makefile
    update-upstream

(dvc-status "../org.adaic.arm_form.upstream")
    commit message "update from upstream"

(xmtn-propagate-one "../org.adaic.arm_form.upstream" ".")

Verify the following in build/Makefile:
    ARM_VERSION          four digit year
    ARM_VERSION_OPTION   command line option to arm_form.exe to choose current version
    DEBIAN_VERSION       reset to .1, or bump if .1 for current ARM_VERSION was published

build/Makefile
    all

verify the following in progs/arm_texinfo.adb:

    Index_Clause  section number corresponding to index
        to find the new one:
            generate arm<version>.texinfo
            move to end of file
            search backward for "@section ... Index"; '...' is the index number

    Z::         no entries for Z?
        look near end of arm<version>.texinfo
