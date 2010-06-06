# Build info version of Ada RM, orig.tar.gz for Debian package, and tar.gz for my web page

VERSION := 2005.2
ZIP_DATE := 20100521

all : debian-free info publish

html : arm2005.html
html : aarm2005.html
info : arm2005.info
info : aarm2005.info

# We have been unable to get rid of all the section ref violations. We
# specify --force to output the info even with some violations. We
# don't specify --no-validate so we catch new ones and are inspired to
# work harder on the old ones.
TEXI_INFO_OPTS := --no-split --no-number-sections --force

../ada-reference-manual-$(VERSION)/arm2005.texinfo : arm_form.exe | ../ada-reference-manual-$(VERSION)
	cd ../source_2005; ../build/arm_form.exe RM info New-Only 2
	mv ../source_2005/RM.texinfo ../ada-reference-manual-$(VERSION)/arm2005.texinfo

../ada-reference-manual-$(VERSION)/aarm2005.texinfo : arm_form.exe | ../ada-reference-manual-$(VERSION)
	cd ../source_2005; ../build/arm_form.exe AARM info New-Only 2
	mv ../source_2005/AA.texinfo ../ada-reference-manual-$(VERSION)/aarm2005.texinfo

../ada-reference-manual-$(VERSION)/html/arm2005/RM-TOC.html : arm_form.exe | ../ada-reference-manual-$(VERSION)/html ../source_2005/Output
	cd ../source_2005; ../build/arm_form.exe RM HTML New-Only 2
	rm -rf ../ada-reference-manual-$(VERSION)/html/arm2005
	mv ../source_2005/Output ../ada-reference-manual-$(VERSION)/html/arm2005
	mkdir ../source_2005/Output

../ada-reference-manual-$(VERSION)/html/aarm2005/AA-TOC.html : arm_form.exe | ../ada-reference-manual-$(VERSION)/html ../source_2005/Output
	cd ../source_2005; ../build/arm_form.exe AARM HTML New-Only 2
	rm -rf ../ada-reference-manual-$(VERSION)/html/aarm2005
	mv ../source_2005/Output ../ada-reference-manual-$(VERSION)/html/aarm2005
	mkdir ../source_2005/Output

../ada-reference-manual-$(VERSION)/txt/arm2005/RM-TOC.TXT : arm_form.exe | ../ada-reference-manual-$(VERSION)/txt ../source_2005/Output
	cd ../source_2005; ../build/arm_form.exe RM Text New-Only 2
	rm -rf ../ada-reference-manual-$(VERSION)/txt/arm2005
	mv ../source_2005/Output ../ada-reference-manual-$(VERSION)/txt/arm2005
	mkdir ../source_2005/Output

../ada-reference-manual-$(VERSION)/txt/aarm2005/AA-TOC.TXT : arm_form.exe | ../ada-reference-manual-$(VERSION)/txt ../source_2005/Output
	cd ../source_2005; ../build/arm_form.exe AARM Text New-Only 2
	rm -rf ../ada-reference-manual-$(VERSION)/txt/aarm2005
	mv ../source_2005/Output ../ada-reference-manual-$(VERSION)/txt/aarm2005
	mkdir ../source_2005/Output

arm_form.exe : force
	gnatmake -p -k -P arm_info.gpr

../ada-reference-manual-$(VERSION) :
	mkdir -p ../ada-reference-manual-$(VERSION)

../ada-reference-manual-$(VERSION)/html :
	mkdir -p ../ada-reference-manual-$(VERSION)/html

../ada-reference-manual-$(VERSION)/txt :
	mkdir -p ../ada-reference-manual-$(VERSION)/txt

../source_2005/Output :
	mkdir ../source_2005/Output

../source_2005 :
	mkdir ../source_2005

../progs :
	mkdir ../progs

publish : publish-info publish-zip

publish-info : aarm2005.info arm2005.info
	tar zcf arm2005-$(ZIP_DATE).tar.gz aarm2005.info arm2005.info

publish-zip : ../ada-reference-manual-$(VERSION)/arm_info.diff
publish-zip : ../ada-reference-manual-$(VERSION)/arm2005.texinfo
publish-zip : ../ada-reference-manual-$(VERSION)/aarm2005.texinfo
	tar zcf arm_info-$(ZIP_DATE)-src.tar.gz -C .. --exclude "*~" --exclude "*.gz" --exclude "*.info" --exclude objects --exclude arm_form.exe build ada-reference-manual-$(VERSION)

# starting from a mtn checkout, generate the Debian source tarball
debian-free : | ../ada-reference-manual-$(VERSION)
debian-free : ../ada-reference-manual-$(VERSION)/arm2005.texinfo
debian-free : ../ada-reference-manual-$(VERSION)/aarm2005.texinfo
	tar zcf ada-reference-manual-$(VERSION).orig.tar.gz -C .. ada-reference-manual-$(VERSION)
	mv ada-reference-manual-$(VERSION).orig.tar.gz ../../ada-reference-manual-2005.2-current

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
UPSTREAM_ADA += arm_text.adb
UPSTREAM_ADA += arm_text.ads

# This retrieves CVS HEAD, for the next Ada version
source_ada.stamp :
	for file in $(UPSTREAM_ADA); do wget -O ../../arm_info.upstream/progs/$$file "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/$$file?rev=HEAD"; done
	touch source_ada.stamp

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

2005-SRC.zip :
	wget http://www.ada-auth.org/arm-files/2005-SRC.zip

source_scheme.stamp : 2005-SRC.zip
	cd ../source_2005; unzip ../build/2005-SRC.zip; rm arm_form.exe; ../build/convert_to_unix.sh
	touch source_scheme.stamp

patch.stamp : source_ada_2005.stamp source_scheme.stamp
	cd ..; patch -0 < ../arm_info.diff

../ada-reference-manual-$(VERSION)/arm_info.diff :
	mtn diff --exclude "." --exclude "../.mtn-ignore" -r "h:arm_info.upstream" -r "h:arm_info" > ../ada-reference-manual-$(VERSION)/arm_info.diff

# delete everything back to mtn checkout
clean ::
	rm -rf objects
	rm -f *.info* *.texinfo
	rm -rf ../source_2005/Output
	rm -rf ../ada-reference-manual-$(VERSION)

info-clean :
	rm -f *.info*

export GNAT_VERSION := $(shell gcc -dumpversion)
include ../../../GDS/main/makerules/common_rules.make
include ../../../GDS/main/makerules/gnat_project_rules.make

# This produces a PDF, but it has no hyperlinks, so it is suitable
# only for printing.
%.dvi : %.texinfo
	texi2dvi --quiet $<

%.ps : %.dvi
	dvips $<

%.pdf : %.ps
	ps2pdf $<

# we don't want all the rules in texinfo_rules.make; they collide with latex_rules.make.
%.info : ../ada-reference-manual-$(VERSION)/%.texinfo
	makeinfo $(TEXI_INFO_OPTS) $<


export ADA_PROJECT_PATH := ../../../GDS/main/makerules

VPATH = ../source ../progs


#Local Variables:
#eval: (ada-parse-prj-file "arm_info.prj")
#eval: (ada-select-prj-file "arm_info.prj")
#eval: (makefile-mode)
#End:
# end of file
