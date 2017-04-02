# Build gpr_query, manuals; publish to web and ELPA; see subdirs for
# building elisp code and running tests (each subdir supports a
# different indentation engine)
#
# test packages from process-archive: (setq package-archives (list (cons "test" "/Projects/elpa/archive/packages")))
# test packages from all-in-place: (setq package-archives (list (cons "test" "/Projects/elpa/packages")))
#
# Ideally we'd have 5 elpa packages with dependencies:
#
# ada-mode  => gnat-core, wisi, gpr-query
# gpr-mode  => gnat-core, wisi
# gnat-core =>
# gpr_query => gnat-core
# wisi 	    =>
#
# but currently the code is not that clean; gpr-mode and gpr-query
# require ada-mode. And it's not worth making gnat-core a package.

export ADA_MODE_VERSION    := 5.2.1
export ADA_REF_MAN_VERSION := 2012.3
export WISI_VERSION        := 1.1.4

ELPA_ROOT ?= $(shell cd ../../elpa; pwd)

all : docs pub-ada pub-wisi build-elpa uninstall-elpa
all : pub-ada-ref-man

docs : info html

BRANCH := $(notdir $(shell cd ..; pwd))

ifeq ($(BRANCH),org.emacs.ada-mode)
  TAR_FILE := org.emacs.ada-mode-$(ADA_MODE_VERSION).tar.gz
  TAR_DIR := ../../
  TAR_PAT := org.emacs.ada-mode-$(ADA_MODE_VERSION)
else
  TAR_FILE := $(BRANCH).tar.gz
  TAR_DIR := ../
  TAR_PAT := $(BRANCH)
endif

DATE := $(shell date +%Y-%m-%d)
# tarball of installed ELPA packages, for sending to customers without good monotone access
elpa-zip : docs pub-ada pub-wisi build-elpa install-elpa ada-mode-elpa-$(DATE).tar.gz

ada-mode-elpa-$(DATE).tar.gz :
	tar zcf ada-mode-elpa-$(DATE).tar.gz -C ~/.emacs.d/elpa ada-ref-man-$(ADA_REF_MAN_VERSION) ada-mode-$(ADA_MODE_VERSION) wisi-$(WISI_VERSION)

zip :
	tar zcf $(TAR_FILE) --exclude _MTN --exclude "autoloads.el" --exclude "gpr_query.db*" --exclude "*~" --exclude "*.ali" --exclude "*.diff" --exclude "*.elc" --exclude "*.exe" --exclude "obj" --exclude "*.output" --exclude "*.stamp" --exclude "*.tar.gz"  --exclude "*.tmp" --exclude "*.wisi-test" -C $(TAR_DIR) $(TAR_PAT)


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
# First delete all files in ELPA, so we catch files deleted here.
pub-ada : force
	rm -rf $(ELPA_ROOT)/archive
	mkdir -p $(ELPA_ROOT)/packages/ada-mode
	rm -rf $(ELPA_ROOT)/packages/ada-mode/*
	cp ../ada*.el ../ada-*.texi ../ada-*.info ../ada_license.text $(ELPA_ROOT)/packages/ada-mode
	rm $(ELPA_ROOT)/packages/ada-mode/ada-ref-man.el
	cp ../dir-ada-mode $(ELPA_ROOT)/packages/ada-mode/dir
	cp ../NEWS-ada-mode.text $(ELPA_ROOT)/packages/ada-mode/NEWS
	cp ../README-ada-mode $(ELPA_ROOT)/packages/ada-mode/README
	cp ../gnat*.el $(ELPA_ROOT)/packages/ada-mode
	cp ../gpr*.el ../gpr-*.texi ../gpr-*.info $(ELPA_ROOT)/packages/ada-mode
	cp ../*.adb ../*.gpr $(ELPA_ROOT)/packages/ada-mode
	mkdir -p $(ELPA_ROOT)/packages/ada-mode/gps_source
	cp ../gps_source/*.ad? $(ELPA_ROOT)/packages/ada-mode/gps_source

# copy files to ELPA ada-ref-man package
pub-ada-ref-man: ARM_INFO ?= c:/Projects/org.adaic.arm_form
pub-ada-ref-man : force
	cp ../ada-ref-man.el $(ELPA_ROOT)/packages/ada-ref-man/
	cp ../README-ada-ref-man $(ELPA_ROOT)/packages/ada-ref-man/README
	cp ../dir-ada-ref-man $(ELPA_ROOT)/packages/ada-ref-man/dir
	cp $(ARM_INFO)/build/arm2012.info $(ELPA_ROOT)/packages/ada-ref-man
	cp $(ARM_INFO)/build/aarm2012.info $(ELPA_ROOT)/packages/ada-ref-man

# copy files to ELPA wisi package
pub-wisi : force
	rm -f $(ELPA_ROOT)/packages/wisi/*
	cp ../README-wisi $(ELPA_ROOT)/packages/wisi/README
	cp ../NEWS-wisi.text $(ELPA_ROOT)/packages/wisi/NEWS
	cp ../wisi*.el $(ELPA_ROOT)/packages/wisi

# 'make -C elpa all-in-place' does not build archive-contents, just *.elc
#
# Other packages are often broken, so just build ours, by only copying
# ours into archive-tmp
build-elpa : force
	rm -rf $(ELPA_ROOT)/archive-tmp
	mkdir -p $(ELPA_ROOT)/archive-tmp/packages
	cp -a $(ELPA_ROOT)/packages/ada-mode $(ELPA_ROOT)/archive-tmp/packages
	cp -a $(ELPA_ROOT)/packages/ada-ref-man $(ELPA_ROOT)/archive-tmp/packages
	cp -a $(ELPA_ROOT)/packages/wisi     $(ELPA_ROOT)/archive-tmp/packages
	make -C $(ELPA_ROOT)/ process-archive

# (setq package-archives (list (cons "test" "/Projects/elpa/archive/packages")))
# (list-packages)

# (dvc-state-one "/Projects/elpa/")
# shell git push

uninstall-elpa :
	emacs -Q --eval '(progn (load-file "uninstall-elpa.el")(kill-emacs))'

# package install from public doesn't work in batch
install-elpa :
	emacs -Q --eval '(progn (load-file "install-elpa.el")(kill-emacs))'

install-elpa-review :
	emacs -Q --eval '(load-file "install-elpa.el")'

## build gpr_query, ada_mode_gps_indent

.PHONY : force

GNAT_EXE    := $(shell which gnat)
INSTALL_BIN := $(dir $(GNAT_EXE))

ifeq ($(shell uname),Linux)
EXE_EXT :=

# export GPS_ROOT ?= /Projects/gps-gpl-2016-src/

else ifeq ($(shell uname),Darwin)
EXE_EXT :=

else
# windows
EXE_EXT := .exe

# export GPS_ROOT ?= d:/Apps/GNAT-gpl_2016/gps-gpl-2016-src

endif

# for debugging:
#export Gnatcoll_Build := Debug
# export Gpr_Query_Build := Debug
#export Ada_Mode_GPS_Indent_Build := Debug
#export BUILD := Debug

# '-' means no std .gpr; use gnat_utils from ~/Projects (source debug)
# set in Emacs for gud-gdb
# (setenv "GPR_PROJECT_PATH" "-:/home/gds/Projects/gnat_util-7.2.1-src:/home/gds/Projects/gnatcoll-1.7w-src/src")
# (setenv "GPR_PROJECT_PATH" nil)
# export GPR_PROJECT_PATH := -:/home/gds/Projects/gnat_util-7.2.1-src:/home/gds/Projects/gnatcoll-1.7w-src/src
gpr_query$(EXE_EXT) : force
	gprbuild -p ../gpr_query.gpr

# (setq ada-gps-exec (concat default-directory "ada_mode_gps_indent"))
ada_mode_gps_indent$(EXE_EXT) : force
	gprbuild -p ../ada_mode_gps_indent.gpr

debug_gps_indent$(EXE_EXT) : force
	gprbuild -p ../ada_mode_gps_indent.gpr debug_gps_indent

one : ada_mode_gps_indent$(EXE_EXT)
	./ada_mode_gps_indent$(EXE_EXT) < commands.txt

trace :
	addr2line -e ../ada_mode_gps_indent.exe 0x96c9bd 0x96b1f6 0x96b24a 0x401cb6 0x989036 0x4013db 0x759e7c02 0x7708ab8d 0x7708ab58

$(INSTALL_BIN)/gpr_query$(EXE_EXT) install_gpr_query : gpr_query$(EXE_EXT)
	gprinstall -f -P ../gpr_query.gpr

$(INSTALL_BIN)/ada_mode_gps_indent$(EXE_EXT) install_ada_mode_gps_indent : ada_mode_gps_indent$(EXE_EXT)
	gprinstall -f -p -P ../ada_mode_gps_indent.gpr --install-name=ada_mode_gps_indent

install : install_gpr_query install_ada_mode_gps_indent

clean ::
	rm -rf ../obj
	rm -rf ../gpr_query$(EXE_EXT)
	rm -rf ../ada_mode_gps_indent$(EXE_EXT)

# (getenv "GPR_PROJECT_PATH")
# eval: (setenv "GPS_ROOT" "d:/Apps/GNAT-gpl_2016/gps-gpl-2016-src")

# Local Variables:
# eval: (ada-parse-prj-file "ada_mode_gps_indent.prj")
# eval: (ada-select-prj-file "ada_mode_gps_indent.prj")
# end:
# end of file
