# Build info version of Ada RM, orig.tar.gz for Debian package, and tar.gz for my web page

# see ../README.txt for list of things to change when scribe or Ada source changes.
#
# Cygwin texi2dvi (GNU Texinfo 4.13) 1.135 fails silently!

DEBIAN_VERSION := 2012.1
ZIP_DATE       := 20120428

# debian-free is _not_ in all
all : source_scribe_2005.stamp html info pdf TXT publish

html : html/arm2012/RM-TOC.html
html : html/aarm2012/AA-TOC.html
html : html/arm2005/RM-TOC.html
html : html/aarm2005/AA-TOC.html
info : arm2012.info
info : aarm2012.info
info : arm2005.info
info : aarm2005.info
pdf  : arm2012.pdf
pdf  : aarm2012.pdf
pdf  : arm2005.pdf
pdf  : aarm2005.pdf
TXT  : txt/arm2012/RM-TOC.TXT
TXT  : txt/aarm2012/AA-TOC.TXT
TXT  : txt/arm2005/RM-TOC.TXT
TXT  : txt/aarm2005/AA-TOC.TXT

# We have been unable to get rid of all the section ref violations, so we
# specify --no-validate. Delete that to see the errors.
TEXI_INFO_OPTS := --no-split --no-number-sections --no-validate
TEXI_DVI_OPTS := --quiet

arm2012.texinfo : arm_form.exe
	cd ../source; ../build/arm_form.exe RM info New-Only 3
	mv ../source/RM.texinfo ./arm2012.texinfo

arm2005.texinfo : arm_form.exe
	cd ../source_2005; ../build/arm_form.exe RM info New-Only 2
	mv ../source_2005/RM.texinfo ./arm2005.texinfo

aarm2012.texinfo : arm_form.exe
	cd ../source; ../build/arm_form.exe AARM info New-Only 3
	mv ../source/AA.texinfo ./aarm2012.texinfo

aarm2005.texinfo : arm_form.exe
	cd ../source_2005; ../build/arm_form.exe AARM info New-Only 2
	mv ../source_2005/AA.texinfo ./aarm2005.texinfo

html/arm2012/RM-TOC.html : arm_form.exe | html/arm2012 ../source/Output
	cd ../source; ../build/arm_form.exe RM HTML New-Only 3
	rm -rf html/arm2012/*
	mv ../source/Output/* html/arm2012

html/arm2005/RM-TOC.html : arm_form.exe | html/arm2005 ../source_2005/Output
	cd ../source_2005; ../build/arm_form.exe RM HTML New-Only 2
	rm -rf html/arm2005/*
	mv ../source_2005/Output/* html/arm2005

html/aarm2012/AA-TOC.html : arm_form.exe | html/aarm2012 ../source/Output
	cd ../source; ../build/arm_form.exe AARM HTML New-Only 3
	rm -rf html/aarm2012/*
	mv ../source/Output/* html/aarm2012

html/aarm2005/AA-TOC.html : arm_form.exe | html/aarm2005 ../source_2005/Output
	cd ../source_2005; ../build/arm_form.exe AARM HTML New-Only 2
	rm -rf html/aarm2005/*
	mv ../source_2005/Output/* html/aarm2005

txt/arm2012/RM-TOC.TXT : arm_form.exe | txt/arm2012 ../source/Output
	cd ../source; ../build/arm_form.exe RM Text New-Only 3
	rm -rf txt/arm2012/*
	mv ../source/Output/* txt/arm2012

txt/arm2005/RM-TOC.TXT : arm_form.exe | txt/arm2005 ../source_2005/Output
	cd ../source_2005; ../build/arm_form.exe RM Text New-Only 2
	rm -rf txt/arm2005/*
	mv ../source_2005/Output/* txt/arm2005

txt/aarm2012/AA-TOC.TXT : arm_form.exe | txt/aarm2012 ../source/Output
	cd ../source; ../build/arm_form.exe AARM Text New-Only 3
	rm -rf txt/aarm2012/*
	mv ../source/Output/* txt/aarm2012

txt/aarm2005/AA-TOC.TXT : arm_form.exe | txt/aarm2005 ../source_2005/Output
	cd ../source_2005; ../build/arm_form.exe AARM Text New-Only 2
	rm -rf txt/aarm2005/*
	mv ../source_2005/Output/* txt/aarm2005

arm_form.exe : force
	gnatmake -p -k -P arm_info.gpr

trace :
	addr2line -e arm_form.exe 0x5c12e7 0x5c598e 0x43e05b 0x446c5b 0x448369 0x545205 0x4e9221 0x4e3b1a 0x5831a1 0x5ad295 0x5adee9 0x4cb3f5 0x401850 0x401235 0x401286 0x7c817075

html/arm2005 html/arm2012 html/aarm2005 html/aarm2012 txt/arm2005 txt/arm2012 txt/aarm2005 txt/aarm2012 ../source/Output ../source_2005 ../source_2005/Output :
	mkdir -p $@

publish : publish-info publish-zip

publish-info : aarm2012.info arm2012.info aarm2005.info arm2005.info
	tar zcf arm2012-$(ZIP_DATE).tar.gz aarm*.info arm*.info

publish-zip : arm_info-$(ZIP_DATE)-src.tar.gz

arm_info-$(ZIP_DATE)-src.tar.gz :
	tar zcf arm_info-$(ZIP_DATE)-src.tar.gz -C ../.. --exclude "*~" --exclude "*.dvi" --exclude "*.gz" --exclude "*.info" --exclude "*.log" --exclude "*.pdf" --exclude "*.ps" --exclude "*.texinfo" --exclude "*.toc" --exclude "*.stamp" --exclude "*.zip" --exclude html --exclude objects --exclude txt --exclude arm_form.exe --exclude _MTN --exclude .mtn-ignore --exclude=.dvc-exclude org.adaic.arm_form

# starting from a mtn checkout of org.adaic.arm_form, generate the Debian source tarball, set up for building Debian package
# ../../ada-reference-manual-2012.1-current/ada-reference-manual-2012.1/debian/rules
debian-free : debian-clean debian-update
	mtn checkout --branch org.debian.ada-reference-manual ../../ada-reference-manual-$(DEBIAN_VERSION)-current/ada-reference-manual-$(DEBIAN_VERSION)
	cd ../../ada-reference-manual-$(DEBIAN_VERSION)-current; tar jxf ada-reference-manual_$(DEBIAN_VERSION).orig.tar.bz2

debian-zip : ada-reference-manual_$(DEBIAN_VERSION).orig.tar.bz2

# we include the 2005 scribe source in the Debian tarball; simpler than including 2005-SRC.zip
ada-reference-manual_$(DEBIAN_VERSION).orig.tar.bz2 : source_scribe_2005.stamp
	rm -rf ../../ada-reference-manual-$(DEBIAN_VERSION)
	mtn checkout --branch org.adaic.arm_form ../../ada-reference-manual-$(DEBIAN_VERSION)
	cp -r ../source_2005 ../../ada-reference-manual-$(DEBIAN_VERSION)
	tar -c -O  -C ../.. --exclude _MTN --exclude .mtn-ignore --no-anchor ada-reference-manual-$(DEBIAN_VERSION) | bzip2 -9 > ada-reference-manual_$(DEBIAN_VERSION).orig.tar.bz2

debian-update : ada-reference-manual_$(DEBIAN_VERSION).orig.tar.bz2
debian-update : | ../../ada-reference-manual-$(DEBIAN_VERSION)-current
	cp $^ ../../ada-reference-manual-$(DEBIAN_VERSION)-current/

debian-clean :
	rm ada-reference-manual_$(DEBIAN_VERSION).orig.tar.bz2
	rm -rf ../../ada-reference-manual-$(DEBIAN_VERSION)-current

../../ada-reference-manual-$(DEBIAN_VERSION)-current :
	mkdir -p $@

# This retrieves CVS HEAD, for both Ada and Scribe source, for the
# next Ada version (dvc-status "../../org.adaic.arm_form.upstream")
#
update-upstream : source_ada.clean source_scribe.clean source_ada.stamp source_scribe.stamp

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
	for file in $(UPSTREAM_ADA); do wget -nv -O ../../org.adaic.arm_form.upstream/progs/$$file "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/$$file?rev=HEAD"; done
	touch source_ada.stamp

source_ada.clean :
	rm -f source_ada.stamp

UPSTREAM_SCRIBE := 01.MSS
UPSTREAM_SCRIBE += 02.MSS
UPSTREAM_SCRIBE += 03A.MSS
UPSTREAM_SCRIBE += 03B.MSS
UPSTREAM_SCRIBE += 03C.MSS
UPSTREAM_SCRIBE += 04A.MSS
UPSTREAM_SCRIBE += 04B.MSS
UPSTREAM_SCRIBE += 05.MSS
UPSTREAM_SCRIBE += 06.MSS
UPSTREAM_SCRIBE += 07.MSS
UPSTREAM_SCRIBE += 08.MSS
UPSTREAM_SCRIBE += 09.MSS
UPSTREAM_SCRIBE += 10.MSS
UPSTREAM_SCRIBE += 11.MSS
UPSTREAM_SCRIBE += 12.MSS
UPSTREAM_SCRIBE += 13A.MSS
UPSTREAM_SCRIBE += 13B.MSS
UPSTREAM_SCRIBE += AARM.MSM
UPSTREAM_SCRIBE += ATTRIBS.MSS
UPSTREAM_SCRIBE += DS.MSS
UPSTREAM_SCRIBE += GLOSSARY.MSS
UPSTREAM_SCRIBE += IMPLDEF.MSS
UPSTREAM_SCRIBE += INDEX.MSS
UPSTREAM_SCRIBE += INFOSYS.MSS
UPSTREAM_SCRIBE += LANGDEF.MSS
UPSTREAM_SCRIBE += LIBRARY.MSS
UPSTREAM_SCRIBE += NUMERICS.MSS
UPSTREAM_SCRIBE += PRAGMAS.MSS
UPSTREAM_SCRIBE += PRE.MSS
UPSTREAM_SCRIBE += PRE_ADA.MSS
UPSTREAM_SCRIBE += PRE_DIRS.MSS
UPSTREAM_SCRIBE += PRE_IO.MSS
UPSTREAM_SCRIBE += PRE_Locales.MSS
UPSTREAM_SCRIBE += PRE_MATH.MSS
UPSTREAM_SCRIBE += Pre_Containers.MSS
UPSTREAM_SCRIBE += Pre_Con2.MSS
UPSTREAM_SCRIBE += Pre_Environ.MSS
UPSTREAM_SCRIBE += Pre_Con2.MSS
UPSTREAM_SCRIBE += RM.MSM
UPSTREAM_SCRIBE += RT.MSS
UPSTREAM_SCRIBE += Real_attribs.mss
UPSTREAM_SCRIBE += SAFETY.MSS
UPSTREAM_SCRIBE += SP.MSS
UPSTREAM_SCRIBE += SYNTAX.MSS
UPSTREAM_SCRIBE += TITLE.MSS
UPSTREAM_SCRIBE += ansi_cover.mss
UPSTREAM_SCRIBE += ansi_title.mss
UPSTREAM_SCRIBE += front_matter.mss
UPSTREAM_SCRIBE += interface.mss
UPSTREAM_SCRIBE += iso-rm.msm
UPSTREAM_SCRIBE += obsolescent.mss
UPSTREAM_SCRIBE += pre_chars.mss
UPSTREAM_SCRIBE += pre_cmdln.mss
UPSTREAM_SCRIBE += pre_standard.mss
UPSTREAM_SCRIBE += pre_strings.mss

# This retrieves CVS HEAD, for the next Ada version (commit to mtn branch org.adaic.arm_form.upstream)
#
# We convert to unix line endings because the scribe processor insists
# on uniform formatting; this works for both Windows and Debian.
source_scribe.stamp :
	for file in $(UPSTREAM_SCRIBE); do wget -nv -O ../../org.adaic.arm_form.upstream/source/$$file "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/source/$$file?rev=HEAD"; done
	cd ../../org.adaic.arm_form.upstream/source; $(CURDIR)/convert_to_unix.sh
	touch source_scribe.stamp

source_scribe.clean :
	rm -f source_scribe.stamp

2005-SRC.zip :
	wget http://www.ada-auth.org/arm-files/2005-SRC.zip

source_scribe_2005.stamp : 2005-SRC.zip | ../source_2005
	cd ../source_2005; unzip ../build/2005-SRC.zip; rm arm_form.exe
	cd ../source_2005; dos2unix -f *.MSS *.mss *.MSM
	cd ../source_2005; patch -p 1 < ../build/source_2005.diff
	rm 2005-SRC.zip
	touch source_scribe_2005.stamp

source_2005.diff : source_2005_orig.stamp
	-cd ../; diff -u source_2005_orig source_2005 > build/source_2005.diff

source_2005_orig.stamp : | ../source_2005_orig
	cd ../source_2005_orig; unzip ../build/2005-SRC.zip; rm arm_form.exe
	cd ../source_2005_orig; dos2unix -f *.MSS *.mss *.MSM
	touch source_2005_orig.stamp

# delete everything back to mtn checkout plus source_2005.
clean ::
	rm 2005-SRC.zip
	rm -rf objects html txt
	rm -f *.dvi *.info* *.texinfo *.log *.pdf *.stamp *.tar.gz *.toc *.zip
	rm -rf ../source/Output ../source_2005/Output

info-clean :
	rm -f *.info*

.PHONY : force
#include ../../../GDS/main/makerules/common_rules.make
#include ../../../GDS/main/makerules/gnat_project_rules.make

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
#eval: (delete '("\\.mss\\'" . scribe-mode) auto-mode-alist)
#eval: (ada-parse-prj-file "arm_info.prj")
#eval: (ada-select-prj-file "arm_info.prj")
#eval: (makefile-mode)
#End:
# end of file
