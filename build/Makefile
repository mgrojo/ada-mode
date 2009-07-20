# common tasks; see subdirectories for Ada builds

source-clean ::
	-find ../ -name "*~" -print | xargs rm -v
	-find ../ -name ".#*" -print | xargs rm -v
	-find ../ -name "*,t" -print | xargs rm -v

# GDS targets
lynx-all :
	make -C lynx_release all

windows-all :
	make -C windows_release all

# end of file
