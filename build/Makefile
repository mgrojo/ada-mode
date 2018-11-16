# no local test; only tested by use in other projects

BRANCH := $(notdir $(shell cd ..; pwd))

ZIP_VERSION := 1.0a

zip :
	rm -rf ../../$(BRANCH)-$(ZIP_VERSION)
	mtn checkout --branch $(BRANCH) ../../$(BRANCH)-$(ZIP_VERSION)
	tar -c -C ../.. --exclude=_MTN --exclude=.mtn-ignore $(BRANCH)-$(ZIP_VERSION) | bzip2 -9 > org.stephe_leake.aunit_ext-$(ZIP_VERSION).tar.bz2

.PHONY : zip

# end of file
