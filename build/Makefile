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


# Specific targets generate every $document$year.$format combination.
# Ex: make arm2012.pdf
AVAILABLE_DOCUMENTS := aarm arm
AVAILABLE_YEARS     := 2005 2012
AVAILABLE_FORMATS   := html info texinfo txt pdf

# These variables are intended to be set on the command line when
# selecting a group of targets.
# Ex: make "FORMATS=html pdf"
DOCUMENTS := $(AVAILABLE_DOCUMENTS)
YEARS     := $(AVAILABLE_YEARS)
FORMATS   := $(filter-out pdf texinfo, $(AVAILABLE_FORMATS))
# Texinfo is of little interest for the end user.
# Pdf requires installation of many huge formatting tools.

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
all: $(foreach d,$(DOCUMENTS),\
     $(foreach y,$(YEARS),\
     $(foreach f,$(FORMATS),\
       $(d)$(y).$(f))))

# We have been unable to get rid of all the section ref violations, so we
# specify --no-validate. Delete that to see the errors.
TEXI_INFO_OPTS := --no-split --no-number-sections --no-validate
TEXI_PDF_OPTS := --quiet

SOURCE_DIR_2005 := source_2005
SOURCE_DIR_2012 := source

MASTER_arm  := rm.msm
MASTER_aarm := aarm.msm

# Rough dependency, but Scheme files should not change often.
SOURCES_2005 := $(wildcard ../$(SOURCE_DIR_2005)/*.ms[ms])
SOURCES_2012 := $(wildcard ../$(SOURCE_DIR_2012)/*.ms[ms])

# See progs/command.txt.
CHANGES      := New-Only
# See inside SOURCE_DIR_YEAR/DOCUMENT.msm for possible values.
VERSION_2005 := 2
VERSION_2012 := 3

define for_d_y_in_AVAILABLE_DOCUMENTS_YEARS_loop

  $(d)$(y).texinfo: arm_form.exe $(SOURCES_$(y))
	cd ../$(SOURCE_DIR_$(y)); \
        ../build/$$< $(MASTER_$(d)) info $(CHANGES) $(VERSION_$(y)) ../build/
	mv `sed -ne 's/^@FilePrefix{\(.*\)}/\L\1/p' ../$(SOURCE_DIR_$(y))/$(MASTER_$(d))`.texinfo $$@
  clean::
	rm --force $(d)$(y).texinfo

  $(d)$(y).html: arm_form.exe $(SOURCES_$(y))
	rm --force --recursive $$@
	mkdir --parents $$@
	cd ../$(SOURCE_DIR_$(y)); \
         ../build/$$< $(MASTER_$(d)) HTML $(CHANGES) $(VERSION_$(y)) ../build/$$@/
  clean::
	rm --force --recursive $(d)$(y).html

  $(d)$(y).txt: arm_form.exe $(SOURCES_$(y))
	rm --force --recursive $$@
	mkdir --parents $$@
	cd ../$(SOURCE_DIR_$(y)); \
        ../build/$$< $(MASTER_$(d)) Text $(CHANGES) $(VERSION_$(y)) ../build/$$@/
  clean::
	rm --force --recursive $(d)$(y).txt

  $(d)$(y).info: $(d)$(y).texinfo
	texi2any --info $(TEXI_INFO_OPTS) $$< -o $$@
  clean::
	rm --force $(d)$(y).info

  $(d)$(y).pdf: $(d)$(y).texinfo
	texi2any --pdf $(TEXI_PDF_OPTS) $$< -o $$@
  clean::
	rm --force $(d)$(y).log $(d)$(y).pdf $(d)$(y).toc

endef
$(foreach d,$(AVAILABLE_DOCUMENTS),\
$(foreach y,$(AVAILABLE_YEARS),\
$(eval $(for_d_y_in_AVAILABLE_DOCUMENTS_YEARS_loop))))

arm_form.exe : force
	gnatmake -p $(BUILDER_OPTIONS) -P $(PROJECT) $(foreach var,ADAFLAGS LDFLAGS,"-X$(var)=$($(var))")
clean::
	rm --force arm_form.exe
	rm --force --recursive objects

trace :
	addr2line -e arm_form.exe 0x5c12e7 0x5c598e 0x43e05b 0x446c5b 0x448369 0x545205 0x4e9221 0x4e3b1a 0x5831a1 0x5ad295 0x5adee9 0x4cb3f5 0x401850 0x401235 0x401286 0x7c817075

../source_2005 :
	mkdir -p $@

INFO_ARCHIVE := arm2012-$(ZIP_DATE).tar.gz
publish: $(INFO_ARCHIVE)
$(INFO_ARCHIVE): $(foreach d,$(DOCUMENTS),\
                 $(foreach y,$(YEARS),\
                 $(d)$(y).info))
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
	cd ../../org.adaic.arm_form.upstream; python3 ../org.adaic.arm_form/build/download.py 2012
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
	rm -f *.stamp *.zip

info-clean :
	rm -f *.info*

.PHONY : force
#include ../../../GDS/main/makerules/common_rules.make
#include ../../../GDS/main/makerules/gnat_project_rules.make

VPATH = ../source ../progs

#Local Variables:
#eval: (delete '("\\.mss\\'" . scribe-mode) auto-mode-alist)
#eval: (ada-parse-prj-file "arm_info.prj")
#eval: (ada-select-prj-file "arm_info.prj")
#eval: (makefile-mode)
#End:
# end of file
