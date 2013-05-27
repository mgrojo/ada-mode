# common tasks; see subdirectories for Ada builds

ZIP_VERSION := 4.0w-$(shell date +%Y-%m-%d)

.PHONY : zip force

source-clean ::
	-find ../ -name "*~" -print | xargs rm -v
	-find ../ -name ".#*" -print | xargs rm -v
	-find ../ -name "*,t" -print | xargs rm -v

linux-clean :
	make -C linux_release distclean
	make -C linux_debug distclean

zip : bz2file zipfile

BRANCH := $(notdir $(shell cd ..; pwd))

bz2file : force
	rm -rf ../../$(BRANCH)-$(ZIP_VERSION)
	mtn checkout --branch $(BRANCH) ../../$(BRANCH)-$(ZIP_VERSION)
	tar -c -O  -C ../.. --exclude=_MTN --exclude=.mtn-ignore --exclude=.dvc-exclude --no-anchor $(BRANCH)-$(ZIP_VERSION) | bzip2 -9 > $(BRANCH)-$(ZIP_VERSION).tar.bz2

zipfile : ROOT := $(shell cd ..; basename `pwd`)
zipfile : force
	cd ../..; zip -q -r $(CURDIR)/$(ROOT)-$(ZIP_VERSION).zip $(ROOT)-$(ZIP_VERSION) -x "$(ROOT)-$(ZIP_VERSION)/_MTN/*" -x "$(ROOT)-$(ZIP_VERSION)/.mtn-ignore" -x "$(ROOT)-$(ZIP_VERSION)/.dvc-ignore"

tag :
	mtn tag h:org.opentoken opentoken-$(ZIP_VERSION)

# end of file
