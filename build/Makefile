# Build info version of Ada RM

VERSION := 2005.2
ZIP_DATE := 20100521

all : dirs info html

# ../source_2005/RM.tex
# arm2005.texinfo
# arm2005.info
one : arm2005.info

two : foo.info

html : arm2005.html
html : aarm2005.html
info : arm2005.info
info : aarm2005.info

TEXI_INFO_OPTS := --no-validate --no-split --no-number-sections

# file://c:/Gnu/GNAT-5.01a/doc/arm2005.html
# file://c:/Stephe/Ada/ARM_Info/source_2005/Output/RM-TOC.html
# c:/Stephe/Ada/ARM_Info/build/arm2005.info
# c:/Stephe/Ada/ARM_Info/build/aarm2005.info
# c:/Gnu/GNAT-5.01a/doc/info/arm2005.info
dirs :
	for dir in ../progs ../source_2005/Output ../ada-reference-manual-$(VERSION); do \
		if [ ! -d $$dir ]; then mkdir -p $$dir; fi; done

arm2005.texinfo : arm_form.exe | ../ada-reference-manual-$(VERSION)
	cd ../source_2005; ../build/arm_form.exe RM info New-Only 2
	mv ../source_2005/RM.texinfo ../ada-reference-manual-$(VERSION)/arm2005.texinfo

aarm2005.texinfo : arm_form.exe | ../ada-reference-manual-$(VERSION)
	cd ../source_2005; ../build/arm_form.exe AARM info New-Only 2
	mv ../source_2005/AA.texinfo ../ada-reference-manual-$(VERSION)/aarm2005.texinfo

arm2005.html : arm_form.exe | ../ada-reference-manual-$(VERSION)/html ../source_2005/Output
	cd ../source_2005; ../build/arm_form.exe RM HTML New-Only 2
	rm -rf ../ada-reference-manual-$(VERSION)/html/arm2005
	mv ../source_2005/Output ../ada-reference-manual-$(VERSION)/html/arm2005
	mkdir ../source_2005/Output

aarm2005.html : arm_form.exe | ../ada-reference-manual-$(VERSION)/html ../source_2005/Output
	cd ../source_2005; ../build/arm_form.exe AARM HTML New-Only 2
	rm -rf ../ada-reference-manual-$(VERSION)/html/aarm2005
	mv ../source_2005/Output ../ada-reference-manual-$(VERSION)/html/aarm2005
	mkdir ../source_2005/Output

arm2005.txt : arm_form.exe | ../ada-reference-manual-$(VERSION)/txt ../source_2005/Output
	cd ../source_2005; ../build/arm_form.exe RM Text New-Only 2
	rm -rf ../ada-reference-manual-$(VERSION)/txt/arm2005
	mv ../source_2005/Output ../ada-reference-manual-$(VERSION)/txt/arm2005
	mkdir ../source_2005/Output

aarm2005.txt : arm_form.exe | ../ada-reference-manual-$(VERSION)/txt ../source_2005/Output
	cd ../source_2005; ../build/arm_form.exe AARM Text New-Only 2
	rm -rf ../ada-reference-manual-$(VERSION)/txt/aarm2005
	mv ../source_2005/Output ../ada-reference-manual-$(VERSION)/txt/aarm2005
	mkdir ../source_2005/Output

arm_form.exe : force
	gnatmake -p -k -Parm_info.gpr

../ada-reference-manual-$(VERSION) :
	mkdir -p ../ada-reference-manual-$(VERSION)

../ada-reference-manual-$(VERSION)/html :
	mkdir -p ../ada-reference-manual-$(VERSION)/html

../ada-reference-manual-$(VERSION)/txt :
	mkdir -p ../ada-reference-manual-$(VERSION)/txt

../source_2005/Output :
	mkdir ../source_2005/Output

trace :
	addr2line -e arm_form.exe 0x804a167 0x80a95b6 0x80a875c 0x80a8c61 0x80a8c61 0x80a8c61 0x80a8c16 0x80a8c16 0x80a8c16 0x80a8c16 0x80a8c61 0x80a8c16 0x80a8c61 0x80a3b4b 0x81278fb 0x811187b 0x810b718 0x81a1297 0x81cadf4 0x81cbf64 0x80f2e81 0x804a0e5 0x400477a3

publish : publish-info publish-zip

publish-info : aarm2005.info arm2005.info
	tar zcf arm2005-$(ZIP_DATE).tar.gz aarm2005.info arm2005.info

publish-zip :
	tar zcf arm_form-$(ZIP_DATE)-src.tar.gz -C .. --exclude="*~" --exclude=source_2005/Output --exclude=CVS progs build/Makefile build/arm_info.prj

VPATH : ../ada-reference-manual-$(VERSION)

debian-free : dirs
debian-free : source.stamp patch.stamp
debian-free : aarm2005.texinfo arm2005.texinfo
debian-free : html/aarm2005/AA-TOC.html html/arm2005/RM-TOC.html
debian-free : txt/aarm2005/AA-TOC.txt txt/arm2005/RM-TOC.txt
debian-free : AA-Final.pdf RM-Final.pdf
	tar zcf ada-reference-manual-$(VERSION)_orig.tar.gz -C .. ada-reference-manual-$(VERSION)
	mv ada-reference-manual-$(VERSION)_orig.tar.gz ../../ada-reference-manual-2005.2-current

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

source_ada.stamp :
	for file in $(UPSTREAM_ADA); do wget -O ../../arm_info.upstream/progs/$$file "http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/progs/$$file?rev=HEAD"; done
	touch source_ada.stamp

# CVS web interface doesn't support retrieve by tag. These revs are
# for tag Amend_Final, which is the Ada 2005 version.
source_ada_2005 :
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

2005-SRC.zip :
	wget http://www.ada-auth.org/arm-files/2005-SRC.zip

source_scheme.stamp : 2005-SRC.zip
	cd ../source_2005; unzip ../build/2005-SRC.zip; rm arm_form.exe
	touch source_scheme.stamp

patch.stamp : source_ada.stamp source_scheme.stamp
	cd ..; patch -0 < ../arm_info.diff

AA-Final.pdf :
	wget http://www.adaic.com/standards/05aarm/AA-Final.pdf

RM-final.pdf :
	wget http://www.adaic.com/standards/05rm/RM-Final.pdf

arm_info.diff.gz :
	mtn diff -r "h:arm_info.upstream" -r "h:arm_info" > arm_info.diff
	tar zcf arm_info.diff.gz arm_info.diff

clean ::
	rm -f *.info* *.texinfo
	rm -f ../source_2005/Output/*

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
