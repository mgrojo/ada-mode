# Build gpr_query, manuals; publish to web and ELPA; see subdirs for
# building elisp code and running tests (each subdir supports a
# different indentation engine)
#
# test packages from process-archive: (setq package-archives (list (cons "test" "/Projects/elpa/archive/packages")))
# test packages from all-in-place: (setq package-archives (list (cons "test" "/Projects/elpa/packages")))
#
# Ideally we'd have 5 packages with dependencies:
#
# ada-mode     => gnat-core, wisi, gnat-inspect
# gpr-mode     => gnat-core, wisi
# gnat-core    =>
# gnat-inspect => gnat-core
# wisi         =>
#
# but currently the code is not that clean; gpr-mode and gnat-inspect
# require ada-mode. And it's not worth making gnat-core a package.

VERSION := 5.1.5

all : docs pub-ada pub-wisi build-elpa install-elpa

docs : info html

BRANCH := $(notdir $(shell cd ..; pwd))
zip :
	tar zcf $(BRANCH)-$(VERSION).tar.gz --exclude _MTN --exclude "autoloads.el" --exclude "gnatinspect.db*" --exclude "*~" --exclude "*.ali" --exclude "*.diff" --exclude "*.elc" --exclude "*.output" --exclude "*.stamp" --exclude "*.tar.gz"  --exclude "*.tmp" --exclude "*.wisi-test" -C ../../ $(BRANCH)


MANUALS := ada-mode gpr-mode

INFO_FILES := $(addsuffix .info, $(MANUALS))
info : ../dir
html : $(addsuffix .html, $(MANUALS))

../dir : $(INFO_FILES)
	for file in $(INFO_FILES); do install-info ../$$file ../dir; done

VPATH := ..

%.info : %.texi
	makeinfo $< -o ../$@

%.html : %.texi
	makeinfo --html --no-split $< -o ../$@

clean ::
	rm -f ../*.info ../*.html ../dir

source-clean :
	-find .. -name "*~" -print -delete
	-find .. -name ".#*" -print -delete

.PHONY : force

# copy files to ELPA ada-mode package
pub-ada : force
	rm -rf ../../elpa/archive
	mkdir -p ../../elpa/packages/ada-mode
	rm -f ../../elpa/packages/ada-mode/ada-mode-pkg.el
	cp ../README ../ada*.el ../ada-*.texi ../ada-*.info ../dir ../ada_license.text ../../elpa/packages/ada-mode
	cp ../NEWS-ada-mode.text ../../elpa/packages/ada-mode/NEWS
	cp ../gnat*.el ../../elpa/packages/ada-mode
	cp ../gpr*.el ../gpr-*.texi ../gpr-*.info ../../elpa/packages/ada-mode

# copy files to ELPA wisi package
pub-wisi : force
	rm -f ../../elpa/packages/wisi/wisi-pkg.el
	cp ../NEWS-wisi.text ../../elpa/packages/wisi/NEWS
	cp ../wisi*.el ../../elpa/packages/wisi

# all-in-place does not build archive-contents, just *.elc
#
# Other packages are often broken, so just build ours, by only copying
# ours into archive-tmp
build-elpa : force
	rm -rf ../../elpa/archive-tmp
	mkdir -p ../../elpa/archive-tmp/packages
	cp -a ../../elpa/packages/ada-mode ../../elpa/archive-tmp/packages
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
	rm -r ~/.emacs.d/elpa/wisi*
	rm -r ~/.emacs.d/elpa/cl*

## build gpr_query, ada_mode_gps_indent

.PHONY : force

GNAT_EXE    := $(shell which gnat)
INSTALL_BIN := $(dir $(GNAT_EXE))

ifeq ($(shell uname),Linux)
EXE_EXT :=

else
# windows
EXE_EXT := .exe

ada_mode_gps_indent$(EXE_EXT) : export GPS_ROOT ?= /Projects/gps-release-ide-6.0.1-src/

endif

# for debugging:
export Gnatcoll_Build := Debug
# export Gpr_Query_Build := Debug
export BUILD := Debug

# '-' means no std .gpr; use gnat_utils from ~/Projects (source debug)
# set in Emacs for gud-gdb
# (setenv "GPR_PROJECT_PATH" "-:/home/gds/Projects/gnat_util-7.2.1-src:/home/gds/Projects/gnatcoll-1.7w-src/src")
# (setenv "GPR_PROJECT_PATH" nil)
# export GPR_PROJECT_PATH := -:/home/gds/Projects/gnat_util-7.2.1-src:/home/gds/Projects/gnatcoll-1.7w-src/src
gpr_query$(EXE_EXT) : force
	gprbuild -p gpr_query.gpr

# (setq ada-gps-exec (concat default-directory "ada_mode_gps_indent"))
ada_mode_gps_indent$(EXE_EXT) : force
	gprbuild -p ada_mode_gps_indent.gpr

one : ada_mode_gps_indent$(EXE_EXT)
	./ada_mode_gps_indent$(EXE_EXT) < commands.txt

trace :
	addr2line -e gpr_query 0x925505 0x4166d3 0x40cb34 0x391ae1ed1b 0x40cb77

$(INSTALL_BIN)/gpr_query$(EXE_EXT) install_gpr_query : gpr_query$(EXE_EXT)
	cp gpr_query$(EXE_EXT) $(INSTALL_BIN)

$(INSTALL_BIN)/ada_mode_gps_indent$(EXE_EXT) install_ada_mode_gps_indent : ada_mode_gps_indent$(EXE_EXT)
	cp ada_mode_gps_indent$(EXE_EXT) $(INSTALL_BIN)

install : install_gpr_query install_ada_mode_gps_indent

clean ::
	rm -rf obj
	rm -rf gpr_query$(EXE_EXT)
	rm -rf ada_mode_gps_indent$(EXE_EXT)

# (getenv "GPR_PROJECT_PATH")

# Local Variables:
# eval: (gnat-2014)
# eval: (setenv "GPS_ROOT" "/Projects/gps-release-ide-6.0.1-src")
# eval: (ada-parse-prj-file "ada_mode_gps_indent.prj")
# eval: (ada-select-prj-file "ada_mode_gps_indent.prj")
# end:
# end of file
