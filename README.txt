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

The official version of the Ada Reference Manual is available at
http://www.adaic.com/standards/. It provides versions of the manual
with change markup.

For each year version, there are two different versions of manual, one
that contains just the text of the official standard, and one which
contains additional annotations for compiler writers, language lawyers
etc. The latter version is called the "Annotated Ada Reference Manual"
(or AARM for short). Both versions are provided by this package.

Why aren't these files the same as upstream?
================================================

The text and HTML files are the same as upstream (processed by the
same tool from the same Scribe sources).

The upstream release does not include an info version; this package
uses the upstream tool to produce texinfo format from the upstream
Scribe sources, and then standard tools to produce info format.

The upstream PDF files are produced from the Scribe sources using
Microsoft word as an intermediate step. The PDF file built by this
package is produced from the texinfo intermediate; it is intended for
paper printing only, since it has no hyperlinks.

Why don't these PDF files have hyperlinks?
==========================================

The upstream PDF files don't have hyperlinks either. The problem is
the intermediate processors; they don't generate proper hyperlinks for
PDF (they do for HTML and info). To get hyperlinks in PDF, we would
need to adapt the Ada code to produce PDF directly from the original
source.

Where do these files come from?
===============================

The "upstream" for this distribution is a CVSWeb HTML interface at
http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ARM/.

The file build/download.py provides simple tools to download the
upstream source; that is run from build/Makefile.

Usage
=====

There are two branches in the Gnu savannah server
(git.sv.gnu.org:/srv/git/ada-mode.git):

org.adaic.arm_form.upstream

    Verbatim copy of the sources in the AdaIC CVS web server
    http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/
    except that:

        the file names are converted to lowercase.

        the line endings in the source directory are converted to unix.

org.adaic.arm_form

    Local branch with minor changes, the texinfo generator, and a
    Makefile that builds everything.

To do a release:

NEWS
    document changes

build/Makefile
    if necessary, update AVAILABLE_YEARS for latest version
    if no new version, bump trailing digit in ZIP_VERSION
        otherwise, reset digit to 1

    update_upstream

        If download.py reports non-zero failed downloads, run it in a
        shell repeatedly (for one year), until it reports no failed
        downloads.

        If download.py reports non-zero no such tag (should only
        happen with draft versions), run 'download.py HEAD', after all
        tagged files are succesfully downloaded.

        Then make:
            mark_%_downloaded
            source_scribe_%.stamp

        repeat for each year, and 'progs'.

(dvc-status "../org.adaic.arm_form.upstream")
    commit message "update from upstream"

(dvc-propagate-one "../org.adaic.arm_form.upstream" ".")

build/Makefile
    all publish

verify the following in progs/arm_texi.adb:

    Z::         no entries for Z?
        look near end of arm<version>.texinfo

Update web page
    /Projects/Web/stephe-leake/ada/arm.html
    ~/Web/Makefile
        edit ARM_INFO_ZIP_VERSION
        arm_info sync

(dvc-status ".")
