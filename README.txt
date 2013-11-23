Project to serve as a convenient packaging of the Ada Reference Manual
formatter and sources.

What is the Ada Reference Manual?
=================================

The Ada Reference Manual is the free (libre) version of the
international standard ISO/IEC 8652:2012(E). It describes a
programming language called "Ada".

The Ada Reference Manual was last revised in 2012, and approved by ISO
in 2013; it is called "Ada 2012". Previous versions are labeled Ada 95
and Ada 2005.

If you want versions of the manual with change markup, see
http://www.adaic.com/standards/

For each year version, there are two different versions of manual, one
that contains just the text of the official standard, and one which
contains additional annotations for compiler writers, language lawyers
etc. The latter version is called the "Annotated Ada Reference Manual"
(or AARM for short). Both versions are provided by this package.

Why aren't these files the same as upstream?
================================================

The text and HTML files are the same as upstream (processed by the
same tool from the same Scheme sources).

The upstream release does not include an info version; this package
uses the upstream tool to produces texinfo format from the upstream
Scheme sources, and then standard tools to produce info format.

The upstream PDF files are produced from the Scheme sources using
Microsoft word as an intermediate step. The PDF file included here is
produced from the texinfo intermediate; it is intended for paper
printing only, since it has no hyperlinks.

Why don't these PDF files have hyperlinks?
==========================================

The upstream PDF files don't have hyperlinks either. The problem is
the intermediate processors; they don't generate proper hyperlinks for
PDF (they do for HTML and info). To get hyperlinks in PDF, we would
need to adapt the Ada code to produce PDF directly from the original
source.

Where do these files come from?
===============================

Source files for the Ada Reference Manual are released as a ZIP
archive at http://www.ada-auth.org/arm-files/2012-SRC.zip. This
archive has many defaults.

- Previous versions of the standards are not provided.

- The Scheme-like tool used to generate readable formats is provided
  separately via an unconvenient CVSWeb HTML interface on
  http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ARM/.

- The file name does not contain any version apart from the Ada
  standard (a four digit year). In other terms, its contents may
  change while the name does not. This makes quite hard tracking new
  releases. For example, on 2013/09/02, the diff with revision tagged
  as Ada2012_Final in the VCS counts 600 lines, not only comments.

- Names in the ZIP archive are cased randomly (Progs/ARM_SUB.ADb is a
  good example :-), probably because authors use a case-insensitive
  OS. The case is not consistent between the ZIP archive and the CVS
  web interface. Both cause compilation errors due to case mismatch
  with hardcoded paths in *.MSM.

We hope that this archive will be more convenient. Its content is also
available in the org.adaic.arm_form.upstream branch of the
www.ada-france.org monotone server.

Usage
=====

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
