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

VERSION := 5.0.1

publish : docs pub-ada pub-wisi build-elpa install-elpa

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

clean :
	rm -f ../*.info ../*.html ../dir

.PHONY : force

pub-ada : force
	mkdir -p ../../elpa/packages/ada-mode
	rm -f ../../elpa/packages/ada-mode/ada-mode-pkg.el
	cp README ../ada*.el ../ada-*.info ../dir ../ada_license.text ../../elpa/packages/ada-mode
	cp ../gnat*.el ../../elpa/packages/ada-mode
	cp ../gpr*.el ../gpr-*.info ../../elpa/packages/ada-mode

pub-wisi : force
	rm -f ../../elpa/packages/wisi/wisi-pkg.el
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

# (dvc-state "/Projects/elpa/")
# shell git push

install-elpa :
	emacs -Q --batch -l install-elpa.el

# end of file
