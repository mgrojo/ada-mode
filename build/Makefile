# common tasks; see subdirectories for Ada builds

ZIP_VERSION := 4.0w

.PHONY : zip force

source-clean ::
	-find ../ -name "*~" -print | xargs rm -v
	-find ../ -name ".#*" -print | xargs rm -v
	-find ../ -name "*,t" -print | xargs rm -v

linux-clean :
	make -C linux_release distclean
	make -C linux_debug distclean

zip : bz2file zipfile

bz2file : force
	rm -rf ../../opentoken-$(ZIP_VERSION)
	mtn checkout --branch org.opentoken ../../opentoken-$(ZIP_VERSION)
	tar -c -O  -C ../.. --exclude=_MTN --exclude=.mtn-ignore --exclude=.dvc-exclude --no-anchor opentoken-$(ZIP_VERSION) | bzip2 -9 > opentoken-$(ZIP_VERSION).tar.bz2

zipfile : force
	cd ../..; zip -q -r opentoken.main/Build/opentoken-$(ZIP_VERSION).zip opentoken-$(ZIP_VERSION) -x "opentoken-$(ZIP_VERSION)/_MTN/*" -x "opentoken-$(ZIP_VERSION)/.mtn-ignore" -x "opentoken-$(ZIP_VERSION)/.dvc-ignore"

debian-update : | ../../opentoken-$(ZIP_VERSION)-current
	cp opentoken-$(ZIP_VERSION).tar.bz2 ../../opentoken-$(ZIP_VERSION)-current/opentoken_$(ZIP_VERSION).orig.tar.bz2

debian : debian-update
	rm -rf ../../opentoken-$(ZIP_VERSION)-current/opentoken-$(ZIP_VERSION)
	mtn checkout --branch org.debian.libopentoken ../../opentoken-$(ZIP_VERSION)-current/opentoken-$(ZIP_VERSION)
	cd ../../opentoken-$(ZIP_VERSION)-current; tar jxf opentoken_$(ZIP_VERSION).orig.tar.bz2

../../opentoken-$(ZIP_VERSION)-current :
	mkdir ../../opentoken-$(ZIP_VERSION)-current

tag :
	mtn tag h:org.opentoken opentoken-$(ZIP_VERSION)

# end of file
