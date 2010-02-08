# common tasks; see subdirectories for Ada builds

ZIP_VERSION := 4.0a

.PHONY : zip force

source-clean ::
	-find ../ -name "*~" -print | xargs rm -v
	-find ../ -name ".#*" -print | xargs rm -v
	-find ../ -name "*,t" -print | xargs rm -v

linux-clean :
	make -C linux_release distclean
	make -C linux_debug distclean

zip : linux-clean source-clean gzipfile zipfile

gzipfile : force
	rm -rf ../../opentoken-$(ZIP_VERSION)
	mtn checkout --branch org.opentoken ../../opentoken-$(ZIP_VERSION)
	tar -zcf opentoken-$(ZIP_VERSION).tar.gz -C ../.. --exclude=_MTN --exclude=.mtn-ignore --exclude=.dvc-exclude --no-anchor opentoken-$(ZIP_VERSION)

zipfile : force
	cd ../..; zip -q -r opentoken.main/Build/opentoken-$(ZIP_VERSION).zip opentoken-$(ZIP_VERSION) -x "opentoken-$(ZIP_VERSION)/_MTN/*" -x "opentoken-$(ZIP_VERSION)/.mtn-ignore"

debian :
	cp opentoken-$(ZIP_VERSION).tar.gz ../../opentoken-$(ZIP_VERSION)-current/opentoken_$(ZIP_VERSION).orig.tar.gz
	sudo rm -rf ../../opentoken-$(ZIP_VERSION)-current/opentoken-$(ZIP_VERSION)
	mtn checkout --branch org.debian.libopentoken ../../opentoken-$(ZIP_VERSION)-current/opentoken-$(ZIP_VERSION)
	cd ../../opentoken-$(ZIP_VERSION)-current; tar zxf opentoken_$(ZIP_VERSION).orig.tar.gz

tag :
	mtn tag h:org.opentoken opentoken-$(ZIP_VERSION)

# end of file
