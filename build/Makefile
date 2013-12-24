# Build manuals, publish to web and ELPA; see subdirs for building
# code and running tests (each subdir supports a different indentation
# engine)
#
# test packages from process-archive: (setq package-archives (list (cons "test" "/Projects/elpa/archive/packages")))
# test packages from all-in-place: (setq package-archives (list (cons "test" "/Projects/elpa/packages")))
#
# Ideally we'd have 5 packages with dependencies:
#
# ada-mode     => gnat wisi, gnat-inspect
# gpr-mode     => gnat wisi
# gnat         =>
# gnat-inspect => gnat
# wisi         =>
#
# but currently the code is not that clean; gpr-mode and gnat-inspect
# require ada-mode. And it's not worth making gnat-core a package.

all : info html

publish : info pub-ada pub-wisi build-elpa

BRANCH := $(notdir $(shell cd ..; pwd))
zip :
	tar zcf $(BRANCH)-`date +%Y-%m-%d`.tar.gz --exclude _MTN --exclude "*~" --exclude "*.ali" --exclude "*.diff" --exclude "*.elc" --exclude "*.output" --exclude "*.tar.gz"  --exclude "*.tmp" -C ../../ $(BRANCH)


MANUALS := ada-mode gpr-mode

info : $(addsuffix .info, $(MANUALS))
html : $(addsuffix .html, $(MANUALS))

VPATH := ..

%.info : %.texi
	makeinfo $< -o ../$@

%.html : %.texi
	makeinfo --html --no-split $< -o ../$@

.PHONY : force

# FIXME: use install-info to create dir?
pub-ada : force
	mkdir -p ../../elpa/packages/ada-mode
	rm -f ../../elpa/packages/ada-mode/ada-mode-pkg.el
	cp ../ada*.el ../ada-*.info ../ada_license.text ../../elpa/packages/ada-mode
	cp ../gnat*.el ../../elpa/packages/ada-mode
	cp ../gpr*.el ../gpr-*.info ../../elpa/packages/ada-mode

pub-wisi : force
	rm -f ../../elpa/packages/wisi/wisi-pkg.el
	cp ../wisi*.el ../../elpa/packages/wisi

# WORKAROUND: all-in-place does not build archive-contents
build-elpa : force
	rm -rf ../../elpa/archive-tmp
	make -C ../../elpa/ archive-tmp process-archive EMACS="emacs -Q --batch"

# (dvc-diff "/Projects/elpa/")
# shell git push

# end of file
