# Build info version of Ada RM, and tar.gz for my web page

# Copyright (c) 2010, 2013 Stephen Leake <stephen_leake@stephe-leake.org>
# Copyright (c) 2013       Nicolas Boulenguez <nicolas@debian.org>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


# see ../README.txt for list of things to change when scribe or Ada source changes.
#
# Cygwin texi2dvi (GNU Texinfo 4.13) 1.135 fails silently!

# Compilation of the Scribe formatter written for the Ada Reference
# Manual, translating .ms[ms] files into various formats.
PROJECT         := arm_info.gpr
BUILDER_OPTIONS := -k
ADAFLAGS        :=
LDFLAGS         :=
# Note that the project adds its own flags to these.

ZIP_DATE       := 20130312

# General targets are:
.PHONY: all clean publish

# Default target is:
#all : source_scribe_2005.stamp
all : html info TXT
# pdf not in 'all' because may not have tools installed

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
	mv ../source/output/rm.texinfo ./arm2012.texinfo

arm2005.texinfo : arm_form.exe
	cd ../source_2005; ../build/arm_form.exe RM info New-Only 2
	mv ../source_2005/output/rm.texinfo ./arm2005.texinfo

aarm2012.texinfo : arm_form.exe
	cd ../source; ../build/arm_form.exe AARM info New-Only 3
	mv ../source/output/aa.texinfo ./aarm2012.texinfo

aarm2005.texinfo : arm_form.exe
	cd ../source_2005; ../build/arm_form.exe AARM info New-Only 2
	mv ../source_2005/output/aa.texinfo ./aarm2005.texinfo

html/arm2012/RM-TOC.html : arm_form.exe | html/arm2012 ../source/output
	cd ../source; ../build/arm_form.exe RM HTML New-Only 3
	rm -rf html/arm2012/*
	mv ../source/output/* html/arm2012

html/arm2005/RM-TOC.html : arm_form.exe | html/arm2005 ../source_2005/output
	cd ../source_2005; ../build/arm_form.exe RM HTML New-Only 2
	rm -rf html/arm2005/*
	mv ../source_2005/output/* html/arm2005

html/aarm2012/AA-TOC.html : arm_form.exe | html/aarm2012 ../source/output
	cd ../source; ../build/arm_form.exe AARM HTML New-Only 3
	rm -rf html/aarm2012/*
	mv ../source/output/* html/aarm2012

html/aarm2005/AA-TOC.html : arm_form.exe | html/aarm2005 ../source_2005/output
	cd ../source_2005; ../build/arm_form.exe AARM HTML New-Only 2
	rm -rf html/aarm2005/*
	mv ../source_2005/output/* html/aarm2005

txt/arm2012/RM-TOC.TXT : arm_form.exe | txt/arm2012 ../source/output
	cd ../source; ../build/arm_form.exe RM Text New-Only 3
	rm -rf txt/arm2012/*
	mv ../source/output/* txt/arm2012

txt/arm2005/RM-TOC.TXT : arm_form.exe | txt/arm2005 ../source_2005/output
	cd ../source_2005; ../build/arm_form.exe RM Text New-Only 2
	rm -rf txt/arm2005/*
	mv ../source_2005/output/* txt/arm2005

txt/aarm2012/AA-TOC.TXT : arm_form.exe | txt/aarm2012 ../source/output
	cd ../source; ../build/arm_form.exe AARM Text New-Only 3
	rm -rf txt/aarm2012/*
	mv ../source/output/* txt/aarm2012

txt/aarm2005/AA-TOC.TXT : arm_form.exe | txt/aarm2005 ../source_2005/output
	cd ../source_2005; ../build/arm_form.exe AARM Text New-Only 2
	rm -rf txt/aarm2005/*
	mv ../source_2005/output/* txt/aarm2005

arm_form.exe : force
	gnatmake -p $(BUILDER_OPTIONS) -P $(PROJECT) $(foreach var,ADAFLAGS LDFLAGS,"-X$(var)=$($(var))")

trace :
	addr2line -e arm_form.exe 0x5c12e7 0x5c598e 0x43e05b 0x446c5b 0x448369 0x545205 0x4e9221 0x4e3b1a 0x5831a1 0x5ad295 0x5adee9 0x4cb3f5 0x401850 0x401235 0x401286 0x7c817075

html/arm2005 html/arm2012 html/aarm2005 html/aarm2012 txt/arm2005 txt/arm2012 txt/aarm2005 txt/aarm2012 ../source/output ../source_2005 ../source_2005/output :
	mkdir -p $@

INFO_ARCHIVE := arm2012-$(ZIP_DATE).tar.gz
publish: $(INFO_ARCHIVE)
# TODO: Add 2005 versions.
$(INFO_ARCHIVE): aarm2012.info arm2012.info
	tar cf $@ $^
clean::
	rm --force $(INFO_ARCHIVE)

ZIP_ARCHIVE := arm_info-$(ZIP_DATE)-src.tar.gz
ZIP_EXCLUDES := *~ *.dvi *.gz *.info *.log *.pdf *.ps *.texinfo *.toc \
  *.stamp *.zip arm_form.exe _MTN .mtn-ignore .dvc-exclude
publish: $(ZIP_ARCHIVE)
$(ZIP_ARCHIVE):
	tar cf $@ -C ../.. $(foreach e,$(ZIP_EXCLUDES),--exclude="$(e)") org.adaic.arm_form
clean::
	rm --force $(ZIP_ARCHIVE)

# This retrieves CVS HEAD, for both Ada and Scribe source, for the
# next Ada version (dvc-status "../../org.adaic.arm_form.upstream")
#
update-upstream : source_ada.clean source_scribe.clean source_ada.stamp source_scribe.stamp

# We do not convert to unix line endings here to minimize the diff
# with upstream; the Ada compiler can handle DOS line endings.
source_ada.stamp :
	rm -rf ../../org.adaic.arm_form.upstream/progs
	cd ../../org.adaic.arm_form.upstream; python3 ../org.adaic.arm_form/build/download.py progs
	touch source_ada.stamp

source_ada.clean :
	rm -f source_ada.stamp

# This retrieves the CVS tag given in download.py "tag" (near the end of the file)
# after updating, commit to mtn branch org.adaic.arm_form.upstream
#
# We convert to unix line endings in download.py because the scribe
# processor insists on uniform formatting; this works for both Windows
# and Debian.
source_scribe.stamp :
	rm -rf ../../org.adaic.arm_form.upstream/source
	cd ../../org.adaic.arm_form.upstream; python3 ../org.adaic.arm_form/build/download.py source 2012
	touch source_scribe.stamp

source_scribe.clean :
	rm -f source_scribe.stamp

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
	rm -f *.dvi *.info* *.texinfo *.log *.pdf *.stamp *.toc *.zip
	rm -rf ../source/output ../source_2005/output

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
