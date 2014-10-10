# Build gpr_query, manuals; publish to web and ELPA; see subdirs for
# building elisp code and running tests (each subdir supports a
# different indentation engine)
#
# test packages from process-archive: (setq package-archives (list (cons "test" "/Projects/elpa/archive/packages")))
# test packages from all-in-place: (setq package-archives (list (cons "test" "/Projects/elpa/packages")))
#
# Ideally we'd have 5 packages with dependencies:
#
# ada-mode     => gnat-core, wisi, gpr-query
# gpr-mode     => gnat-core, wisi
# gnat-core    =>
# gpr_query => gnat-core
# wisi         =>
#
# but currently the code is not that clean; gpr-mode and gpr-query
# require ada-mode. And it's not worth making gnat-core a package.

VERSION := 5.1.6

all : docs pub-ada pub-ada-ref-man pub-wisi build-elpa install-elpa

docs : info html

BRANCH := $(notdir $(shell cd ..; pwd))
zip :
	tar zcf $(BRANCH)-$(VERSION).tar.gz --exclude _MTN --exclude "autoloads.el" --exclude "gpr_query.db*" --exclude "*~" --exclude "*.ali" --exclude "*.diff" --exclude "*.elc" --exclude "*.exe" --exclude "obj" --exclude "*.output" --exclude "*.stamp" --exclude "*.tar.gz"  --exclude "*.tmp" --exclude "*.wisi-test" -C ../../ $(BRANCH)


MANUALS := ada-mode gpr-mode

INFO_FILES := $(addsuffix .info, $(MANUALS))
info : ../dir-ada-mode
html : $(addsuffix .html, $(MANUALS))

../dir-ada-mode : $(INFO_FILES)
	for file in $(INFO_FILES); do install-info ../$$file ../dir-ada-mode; done

VPATH := ..

%.info : %.texi
	makeinfo $< -o ../$@

%.html : %.texi
	makeinfo --html --no-split $< -o ../$@

clean ::
	rm -f ../*.info ../*.html ../dir-ada-mode

source-clean :
	-find .. -name "*~" -print -delete
	-find .. -name ".#*" -print -delete

.PHONY : force

# copy files to ELPA ada-mode package
pub-ada : force
	rm -rf ../../elpa/archive
	mkdir -p ../../elpa/packages/ada-mode
	rm -f ../../elpa/packages/ada-mode/ada-mode-pkg.el
	cp ../ada*.el ../ada-*.texi ../ada-*.info ../ada_license.text ../../elpa/packages/ada-mode
	cp ../dir-ada-mode ../../elpa/packages/ada-mode/dir
	cp ../NEWS-ada-mode.text ../../elpa/packages/ada-mode/NEWS
	cp ../README-ada-mode ../../elpa/packages/ada-mode/README
	cp ../gnat*.el ../../elpa/packages/ada-mode
	cp ../gpr*.el ../gpr-*.texi ../gpr-*.info ../../elpa/packages/ada-mode

# copy files to ELPA ada-ref-man package
pub-ada-ref-man: ARM_INFO ?= c:/Projects/arm_info/org.adaic.arm_form
pub-ada-ref-man : force
	cp ../ada-ref-man.el ../../elpa/packages/ada-ref-man/
	cp ../README-ada-ref-man ../../elpa/packages/ada-ref-man/README
	cp ../dir-ada-ref-man ../../elpa/packages/ada-ref-man/dir
	cp $(ARM_INFO)/build/arm2012.info ../../elpa/packages/ada-ref-man
	cp $(ARM_INFO)/build/aarm2012.info ../../elpa/packages/ada-ref-man

# copy files to ELPA wisi package
pub-wisi : force
	rm -f ../../elpa/packages/wisi/wisi-pkg.el
	cp ../README-wisi ../../elpa/packages/wisi/README
	cp ../NEWS-wisi.text ../../elpa/packages/wisi/NEWS
	cp ../wisi*.el ../../elpa/packages/wisi

# 'make -C elpa all-in-place' does not build archive-contents, just *.elc
#
# Other packages are often broken, so just build ours, by only copying
# ours into archive-tmp
build-elpa : force
	rm -rf ../../elpa/archive-tmp
	mkdir -p ../../elpa/archive-tmp/packages
	cp -a ../../elpa/packages/ada-mode ../../elpa/archive-tmp/packages
	cp -a ../../elpa/packages/ada-ref-man ../../elpa/archive-tmp/packages
	cp -a ../../elpa/packages/wisi     ../../elpa/archive-tmp/packages
	make -C ../../elpa/ process-archive

# (setq package-archives (list (cons "test" "/Projects/elpa/archive/packages")))
# (list-packages)

# (dvc-state-one "/Projects/elpa/")
# shell git push

# package install from public doesn't work in batch
install-elpa :
	emacs -Q --eval '(progn (load-file "install-elpa.el")(kill-emacs))'

install-clean :
	rm -r ~/.emacs.d/elpa/ada-mode*
	rm -r ~/.emacs.d/elpa/ada-ref-man*
	rm -r ~/.emacs.d/elpa/wisi*
	rm -r ~/.emacs.d/elpa/cl*

## build gpr_query

.PHONY : force

GNAT_EXE    := $(shell which gnat)
INSTALL_BIN := $(dir $(GNAT_EXE))

ifeq ($(shell uname),Linux)
EXE_EXT :=

else
# windows
EXE_EXT := .exe

endif

# '-' means no std .gpr; use gnat_utils from ~/Projects (source debug)
# set in Emacs for gud-gdb
# (setenv "GPR_PROJECT_PATH" "-:/home/gds/Projects/gnat_util-7.2.1-src:/home/gds/Projects/gnatcoll-1.7w-src/src")
# (setenv "GPR_PROJECT_PATH" nil)
# export GPR_PROJECT_PATH := -:/home/gds/Projects/gnat_util-7.2.1-src:/home/gds/Projects/gnatcoll-1.7w-src/src
# export Gpr_Query_Build := Debug
# export Gnatcoll_Build := Debug
# export BUILD := Debug
gpr_query$(EXE_EXT) : force
	gprbuild -p gpr_query.gpr

one : $(INSTALL_BIN)/gpr_query$(EXE_EXT)
#	gpr_query -P gpr_query.gpr -c project_path
#	gpr_query -P ~/GDS/work_stephe_3/dscovr/build/x86_64_gnu_linux_release/gds_dscovr_test_agg.gpr -c source_dirs
#	cd ~/GDS/work_stephe_3/dscovr/build/x86_64_gnu_linux_release/; gpr_query -P gds_dscovr_test_agg.gpr -c source_dirs
	cd ~/GDS/work_stephe_3/dscovr/build/; gpr_query --force_refresh -P itcsb_time_sync_simics_agg.gpr -c "refs delta_time:itcsb_time_sync.cpp"
# --autoconf=x86_64_gnu_linux_release/obj_dscovr_tcc_link/auto.cgpr
#--tracefile .gnatdebug

trace :
	addr2line -e gpr_query 0x925505 0x4166d3 0x40cb34 0x391ae1ed1b 0x40cb77

$(INSTALL_BIN)/gpr_query$(EXE_EXT) install : gpr_query$(EXE_EXT)
	cp gpr_query$(EXE_EXT) $(INSTALL_BIN)

clean ::
	rm -rf obj
	rm -rf gpr_query$(EXE_EXT)

# (getenv "GPR_PROJECT_PATH")

# Local Variables:
# eval: (ada-parse-prj-file "gpr_query.prj")
# eval: (ada-select-prj-file "gpr_query.prj")
# end:
# end of file
