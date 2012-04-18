# Build info version of Ada RM, orig.tar.gz for Debian package, and tar.gz for my web page

# see ../README.txt for list of things to change when scheme or Ada source changes.

ARM_VERSION := 2012

# 1 = 1995
# 2 = 2005
# 3 = 2012
ARM_VERSION_OPTION := 3

DEBIAN_VERSION := $(ARM_VERSION).1
ZIP_DATE       := 20100608

all : html info pdf TXT publish debian-free

html : html/arm$(ARM_VERSION)/RM-TOC.html
html : html/aarm$(ARM_VERSION)/AA-TOC.html
info : arm$(ARM_VERSION).info
info : aarm$(ARM_VERSION).info
pdf  : arm$(ARM_VERSION).pdf
pdf  : aarm$(ARM_VERSION).pdf
TXT  : txt/arm$(ARM_VERSION)/RM-TOC.TXT
TXT  : txt/aarm$(ARM_VERSION)/AA-TOC.TXT

# We have been unable to get rid of all the section ref violations, so we
# specify --no-validate. Delete that to see the errors.
TEXI_INFO_OPTS := --no-split --no-number-sections --no-validate
TEXI_DVI_OPTS := --quiet

arm$(ARM_VERSION).texinfo : arm_form.exe
	cd ../source; ../build/arm_form.exe RM info New-Only $(ARM_VERSION_OPTION)
	mv ../source/RM.texinfo ./arm$(ARM_VERSION).texinfo

aarm$(ARM_VERSION).texinfo : arm_form.exe
	cd ../source; ../build/arm_form.exe AARM info New-Only $(ARM_VERSION_OPTION)
	mv ../source/AA.texinfo ./aarm$(ARM_VERSION).texinfo

html/arm$(ARM_VERSION)/RM-TOC.html : arm_form.exe | html/arm$(ARM_VERSION) ../source/Output
	cd ../source; ../build/arm_form.exe RM HTML New-Only $(ARM_VERSION_OPTION)
	rm -rf html/arm$(ARM_VERSION)
	mv ../source/Output html/arm$(ARM_VERSION)
	mkdir ../source/Output

html/aarm$(ARM_VERSION)/AA-TOC.html : arm_form.exe | html/aarm$(ARM_VERSION) ../source/Output
	cd ../source; ../build/arm_form.exe AARM HTML New-Only $(ARM_VERSION_OPTION)
	rm -rf html/aarm$(ARM_VERSION)
	mv ../source/Output html/aarm$(ARM_VERSION)
	mkdir ../source/Output

txt/arm$(ARM_VERSION)/RM-TOC.TXT : arm_form.exe | txt/arm$(ARM_VERSION) ../source/Output
	cd ../source; ../build/arm_form.exe RM Text New-Only $(ARM_VERSION_OPTION)
	rm -rf txt/arm$(ARM_VERSION)
	mv ../source/Output txt/arm$(ARM_VERSION)
	mkdir ../source/Output

txt/aarm$(ARM_VERSION)/AA-TOC.TXT : arm_form.exe | txt/aarm$(ARM_VERSION) ../source/Output
	cd ../source; ../build/arm_form.exe AARM Text New-Only $(ARM_VERSION_OPTION)
	rm -rf txt/aarm$(ARM_VERSION)
	mv ../source/Output txt/aarm$(ARM_VERSION)
	mkdir ../source/Output

arm_form.exe : force
	gnatmake -p -k -P arm_info.gpr

html/arm$(ARM_VERSION) html/aarm$(ARM_VERSION) txt/arm$(ARM_VERSION) txt/aarm$(ARM_VERSION) :
	mkdir -p $@

../source/Output :
	mkdir ../source/Output

publish : publish-info publish-zip

publish-info : aarm$(ARM_VERSION).info arm$(ARM_VERSION).info
	tar zcf arm$(ARM_VERSION)-$(ZIP_DATE).tar.gz aarm$(ARM_VERSION).info arm$(ARM_VERSION).info

publish-zip : arm_info-$(ZIP_DATE)-src.tar.gz

arm_info-$(ZIP_DATE)-src.tar.gz :
	tar zcf arm_info-$(ZIP_DATE)-src.tar.gz -C ../.. --exclude "*~" --exclude "*.gz" --exclude "*.info" --exclude objects --exclude arm_form.exe --exclude _MTN --exclude .mtn-ignore arm_info

# starting from a mtn checkout of arm_info, generate the Debian source tarball, set up for building Debian package
debian-free : | ../../ada-reference-manual-$(DEBIAN_VERSION)-current
debian-free : arm_info-$(ZIP_DATE)-src.tar.gz
	cp $^ ../../ada-reference-manual-$(DEBIAN_VERSION)-current/ada-reference-manual_$(DEBIAN_VERSION).orig.tar.gz
	mtn -d ~/monotone-dbs/ada-france.db checkout --branch org.debian.ada-reference-manual ../../ada-reference-manual-$(DEBIAN_VERSION)-current/ada-reference-manual-$(DEBIAN_VERSION)
	cd ../../ada-reference-manual-$(DEBIAN_VERSION)-current; tar zxf ada-reference-manual_$(DEBIAN_VERSION).orig.tar.gz

../../ada-reference-manual-$(DEBIAN_VERSION)-current :
	mkdir -p $@

# This retrieves CVS HEAD, for both Ada and Scheme source, for the
# next Ada version (dvc-status "../../arm_info.upstream")
#
update-upstream : source_ada.clean source_scheme.clean source_ada.stamp source_scheme.stamp

upstream-diff : ../ada-reference-manual-$(DEBIAN_VERSION)/arm_info.diff

UPSTREAM_ADA := arm_cont.adb
UPSTREAM_ADA += arm_cont.ads
UPSTREAM_ADA += arm_corr.adb
UPSTREAM_ADA += arm_corr.ads
UPSTREAM_ADA += arm_db.adb
UPSTREAM_ADA += arm_db.ads
UPSTREAM_ADA += arm_file.adb
UPSTREAM_ADA += arm_file.ads
UPSTREAM_ADA += arm_form.ada
UPSTREAM_ADA += arm_frm.adb
UPSTREAM_ADA += arm_frm.ads
UPSTREAM_ADA += arm_frmd.adb
UPSTREAM_ADA += arm_frmd.ads
UPSTREAM_ADA += arm_frms.adb
UPSTREAM_ADA += arm_html.adb
UPSTREAM_ADA += arm_html.ads
UPSTREAM_ADA += arm_indx.adb
UPSTREAM_ADA += arm_indx.ads
UPSTREAM_ADA += arm_inp.adb
UPSTREAM_ADA += arm_inp.ads
UPSTREAM_ADA += arm_mast.adb
UPSTREAM_ADA += arm_mast.ads
UPSTREAM_ADA += arm_out.ads
UPSTREAM_ADA += arm_rtf.adb
UPSTREAM_ADA += arm_rtf.ads
UPSTREAM_ADA += arm_str.adb
UPSTREAM_ADA += arm_str.ads
UPSTREAM_ADA += arm_sub.adb
UPSTREAM_ADA += arm_sub.ads
UPSTREAM_ADA += arm_syn.adb
UPSTREAM_ADA += arm_syn.ads
UPSTREAM_ADA += arm_texi.adb
UPSTREAM_ADA += arm_texi.ads
UPSTREAM_ADA += arm_text.adb
UPSTREAM_ADA += arm_text.ads

# We do not convert to unix line endings here to minimize the diff
# with upstream; the Ada compiler on Debian can handle DOS line
# endings.
source_ada.stamp :
	for file in $(UPSTREAM_ADA); do wget -nv -O ../../arm_info.upstream/progs/$$file "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/$$file?rev=HEAD"; done
	touch source_ada.stamp

source_ada.clean :
	rm -f source_ada.stamp

# CVS web interface doesn't support retrieve by tag. These revs are
# for tag Amend_Final, which is the Ada 2005 version.
source_ada_2005.stamp :
	wget -O ../../arm_info.upstream/progs/arm_cont.adb "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_cont.adb?rev=1.8"
	wget -O ../../arm_info.upstream/progs/arm_cont.ads "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_cont.ads?rev=1.3"
	wget -O ../../arm_info.upstream/progs/arm_corr.adb "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_corr.adb?rev=1.8"
	wget -O ../../arm_info.upstream/progs/arm_corr.ads "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_corr.ads?rev=1.6"
	wget -O ../../arm_info.upstream/progs/arm_db.adb "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_db.adb?rev=1.11"
	wget -O ../../arm_info.upstream/progs/arm_db.ads "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_db.ads?rev=1.7"
	wget -O ../../arm_info.upstream/progs/arm_file.adb "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_file.adb?rev=1.1"
	wget -O ../../arm_info.upstream/progs/arm_file.ads "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_file.ads?rev=1.1"
	wget -O ../../arm_info.upstream/progs/arm_form.ada "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_form.ada?rev=1.12"
	wget -O ../../arm_info.upstream/progs/arm_frm.adb "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_frm.adb?rev=1.50"
	wget -O ../../arm_info.upstream/progs/arm_frm.ads "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_frm.ads?rev=1.21"
	wget -O ../../arm_info.upstream/progs/arm_frms.adb "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_frms.adb?rev=1.4"
	wget -O ../../arm_info.upstream/progs/arm_html.adb "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_html.adb?rev=1.39"
	wget -O ../../arm_info.upstream/progs/arm_html.ads "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_html.ads?rev=1.20"
	wget -O ../../arm_info.upstream/progs/arm_indx.adb "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_indx.adb?rev=1.9"
	wget -O ../../arm_info.upstream/progs/arm_indx.ads "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_indx.ads?rev=1.4"
	wget -O ../../arm_info.upstream/progs/arm_inp.adb "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_inp.adb?rev=1.6"
	wget -O ../../arm_info.upstream/progs/arm_inp.ads "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_inp.ads?rev=1.3"
	wget -O ../../arm_info.upstream/progs/arm_mast.adb "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_mast.adb?rev=1.10"
	wget -O ../../arm_info.upstream/progs/arm_mast.ads "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_mast.ads?rev=1.2"
	wget -O ../../arm_info.upstream/progs/arm_out.ads "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_out.ads?rev=1.17"
	wget -O ../../arm_info.upstream/progs/arm_rtf.adb "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_rtf.adb?rev=1.26"
	wget -O ../../arm_info.upstream/progs/arm_rtf.ads "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_rtf.ads?rev=1.12"
	wget -O ../../arm_info.upstream/progs/arm_str.adb "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_str.adb?rev=1.1"
	wget -O ../../arm_info.upstream/progs/arm_str.ads "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_str.ads?rev=1.1"
	wget -O ../../arm_info.upstream/progs/arm_sub.adb "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_sub.adb?rev=1.4"
	wget -O ../../arm_info.upstream/progs/arm_sub.ads "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_sub.ads?rev=1.2"
	wget -O ../../arm_info.upstream/progs/arm_syn.adb "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_syn.adb?rev=1.7"
	wget -O ../../arm_info.upstream/progs/arm_syn.ads "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_syn.ads?rev=1.3"
	wget -O ../../arm_info.upstream/progs/arm_text.adb "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_text.adb?rev=1.17"
	wget -O ../../arm_info.upstream/progs/arm_text.ads "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/arm_text.ads?rev=1.10"
	touch source_ada_2005.stamp

UPSTREAM_SCHEME := 01.MSS
UPSTREAM_SCHEME += 02.MSS
UPSTREAM_SCHEME += 03A.MSS
UPSTREAM_SCHEME += 03B.MSS
UPSTREAM_SCHEME += 03C.MSS
UPSTREAM_SCHEME += 04A.MSS
UPSTREAM_SCHEME += 04B.MSS
UPSTREAM_SCHEME += 05.MSS
UPSTREAM_SCHEME += 06.MSS
UPSTREAM_SCHEME += 07.MSS
UPSTREAM_SCHEME += 08.MSS
UPSTREAM_SCHEME += 09.MSS
UPSTREAM_SCHEME += 10.MSS
UPSTREAM_SCHEME += 11.MSS
UPSTREAM_SCHEME += 12.MSS
UPSTREAM_SCHEME += 13A.MSS
UPSTREAM_SCHEME += 13B.MSS
UPSTREAM_SCHEME += AARM.MSM
UPSTREAM_SCHEME += ATTRIBS.MSS
UPSTREAM_SCHEME += DS.MSS
UPSTREAM_SCHEME += GLOSSARY.MSS
UPSTREAM_SCHEME += IMPLDEF.MSS
UPSTREAM_SCHEME += INDEX.MSS
UPSTREAM_SCHEME += INFOSYS.MSS
UPSTREAM_SCHEME += LANGDEF.MSS
UPSTREAM_SCHEME += LIBRARY.MSS
UPSTREAM_SCHEME += NALL.BAT
UPSTREAM_SCHEME += NUMERICS.MSS
UPSTREAM_SCHEME += PRAGMAS.MSS
UPSTREAM_SCHEME += PRE.MSS
UPSTREAM_SCHEME += PRE_ADA.MSS
UPSTREAM_SCHEME += PRE_DIRS.MSS
UPSTREAM_SCHEME += PRE_IO.MSS
UPSTREAM_SCHEME += PRE_Locales.MSS
UPSTREAM_SCHEME += PRE_MATH.MSS
UPSTREAM_SCHEME += Pre_Containers.MSS
UPSTREAM_SCHEME += Pre_Con2.MSS
UPSTREAM_SCHEME += Pre_Environ.MSS
UPSTREAM_SCHEME += Pre_Con2.MSS
UPSTREAM_SCHEME += RM.MSM
UPSTREAM_SCHEME += RT.MSS
UPSTREAM_SCHEME += Real_attribs.mss
UPSTREAM_SCHEME += SAFETY.MSS
UPSTREAM_SCHEME += SP.MSS
UPSTREAM_SCHEME += SYNTAX.MSS
UPSTREAM_SCHEME += TITLE.MSS
UPSTREAM_SCHEME += ansi_cover.mss
UPSTREAM_SCHEME += ansi_title.mss
UPSTREAM_SCHEME += front_matter.mss
UPSTREAM_SCHEME += interface.mss
UPSTREAM_SCHEME += iso-rm.msm
UPSTREAM_SCHEME += obsolescent.mss
UPSTREAM_SCHEME += pre_chars.mss
UPSTREAM_SCHEME += pre_cmdln.mss
UPSTREAM_SCHEME += pre_standard.mss
UPSTREAM_SCHEME += pre_strings.mss

# This retrieves CVS HEAD, for the next Ada version (commit to mtn branch arm_info.upstream)
#
# We convert to unix line endings because the scheme processor insists
# on uniform formatting; this works for both Windows and Debian.
source_scheme.stamp :
	for file in $(UPSTREAM_SCHEME); do wget -nv -O ../../arm_info.upstream/source/$$file "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/source/$$file?rev=HEAD"; done
	cd ../../arm_info.upstream/source; $(CURDIR)/convert_to_unix.sh
	touch source_scheme.stamp

source_scheme.clean :
	rm source_scheme.stamp

2005-SRC.zip :
	wget http://www.ada-auth.org/arm-files/2005-SRC.zip

source_scheme_2005.stamp : 2005-SRC.zip
	cd ../../arm_info.upstream/source; unzip ../build/2005-SRC.zip; rm arm_form.exe; $(CURDIR)/convert_to_unix.sh
	touch source_scheme.stamp

# delete everything back to mtn checkout
clean ::
	rm -rf objects html txt
	rm -f *.info* *.texinfo
	rm -rf ../source/Output

info-clean :
	rm -f *.info*

include ../../../GDS/main/makerules/common_rules.make
include ../../../GDS/main/makerules/gnat_project_rules.make

# This produces a PDF, but it has no hyperlinks, so it is suitable
# only for printing.
%.dvi : %.texinfo
	texi2dvi $(TEXI_DVI_OPTS) $<

%.ps : %.dvi
	dvips $<

%.pdf : %.ps
	ps2pdf $<

# we don't want all the rules in texinfo_rules.make; they collide with latex_rules.make.
%.info : %.texinfo
	makeinfo $(TEXI_INFO_OPTS) $<

VPATH = ../source ../progs

#Local Variables:
#eval: (ada-parse-prj-file "arm_info.prj")
#eval: (ada-select-prj-file "arm_info.prj")
#eval: (makefile-mode)
#End:
# end of file
