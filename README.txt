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

    ZIP_DATE             date of build

build/Makefile
    all

verify the following in progs/arm_texi.adb:

    Index_Clause  section number corresponding to index
        not clear how to find the new one

    Z::         no entries for Z?
        look near end of arm<version>.texinfo

Update web page
    /Projects/Web/stephe-leake/ada/arm.html
    ~/Web/Makefile
        edit ARM_INFO_ZIP_VERSION
        arm_info sync
