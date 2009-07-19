# common tasks; see subdirectories for Ada builds

ZIP_VERSION := 3.1w

.PHONY : zip force

source-clean ::
	-find ../ -name "*~" -print | xargs rm -v
	-find ../ -name ".#*" -print | xargs rm -v
	-find ../ -name "*,t" -print | xargs rm -v

linux-clean :
	make -C linux_release distclean
	make -C linux_debug distclean

ZIPFILE := opentoken-$(ZIP_VERSION).tar.gz
zip : linux-clean source-clean $(ZIPFILE)

zipfile : $(ZIPFILE)

$(ZIPFILE) : force
	tar -zcf $(ZIPFILE) -C ../.. --exclude=_MTN --exclude=*.tar.gz --exclude=*.tar --no-anchor opentoken

# end of file
