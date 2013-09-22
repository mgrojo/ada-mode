Project to serve as a convenient packaging of the Ada Reference Manual
formatter and sources.

There are two monotone branches:

org.adaic.arm_form.upstream

    Verbatim copy of the sources in the AdaIC CVS web server
    http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/
    except that:

        the file names are converted to lowercase.

        the line endings in the source directory are converted to unix.

org.adaic.arm_form

    Local branch with minor changes and a Makefile that builds
    everything.

To do a release:

build/download.py
    near the end of the file, change the default value of "tag" if
    necessary (for a new Ada version)

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
