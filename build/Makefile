# common tasks; see subdirectories for Ada builds

ZIP_VERSION := 6.0a
#w-$(shell date +%Y-%m-%d)

.PHONY : zip force

doc : ../Docs/wisi-user_guide.info
doc : ../Docs/wisi-user_guide.html

vpath %.texinfo ../Docs

%.info : %.texinfo
	makeinfo $< -o $@

%.html : %.texinfo
	makeinfo --html --no-split $< -o $@

source-clean ::
	-find ../ -name "*~" -delete
	-find ../ -name ".#*" -delete
	-find ../ -name "*,t" -delete

linux-clean :
	make -C linux_release distclean
	make -C linux_debug distclean

zip : source-clean bz2file zipfile

BRANCH := $(notdir $(shell cd ..; pwd))

bz2file : force
	rm -rf ../../$(BRANCH)-$(ZIP_VERSION)
	mtn checkout --branch $(BRANCH) ../../$(BRANCH)-$(ZIP_VERSION)
	tar -c -O  -C ../.. --exclude=_MTN --exclude "build/x86*" --exclude=.mtn-ignore --exclude=.dvc-exclude --exclude debug_parser.adb --no-anchor $(BRANCH)-$(ZIP_VERSION) | bzip2 -9 > opentoken-$(ZIP_VERSION).tar.bz2

zipfile : ROOT := $(shell cd ..; basename `pwd`)
zipfile : force
	cd ../..; zip -q -r $(CURDIR)/$(ROOT)-$(ZIP_VERSION).zip opentoken-$(ZIP_VERSION) -x "$(ROOT)-$(ZIP_VERSION)/_MTN/*" -x "$(ROOT)-$(ZIP_VERSION)/build/x86_*" -x "$(ROOT)-$(ZIP_VERSION)/.mtn-ignore" -x "$(ROOT)-$(ZIP_VERSION)/.dvc-exclude" -x "$(ROOT)-$(ZIP_VERSION)/debug_parser.adb"

tag :
	mtn tag h:org.opentoken opentoken-$(ZIP_VERSION)

# end of file
