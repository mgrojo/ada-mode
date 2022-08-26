# Build elisp code; publish to ELPA

all : byte-compile autoloads

# Doing byte-compile-clean first avoids errors caused by
# loading new source on old .elc.
byte-compile : byte-compile-clean
	cd ../; $(EMACS_EXE) -Q -batch -L . -L $(WISI) -l build/exclude-elpa.el --eval '(progn (batch-byte-compile))' *.el
#(setq byte-compile-error-on-warn t)

byte-compile-clean :
	cd ..; rm -f *.elc

autoloads : force
	$(EMACS_EXE) -Q -batch --eval "(progn (require 'autoload)(setq generated-autoload-file (expand-file-name \"autoloads.el\"))(update-directory-autoloads \".\"))"

ifeq ($(shell uname),Linux)
EMACS_EXE ?= emacs
else ifeq ($(shell uname),Darwin)
EMACS_EXE ?= "/Applications/Emacs.app/Contents/MacOS/Emacs"
else
# windows
# specify uniscribe to workaround weird Windows harfbuzz bug
EMACS_EXE ?= emacs -xrm Emacs.fontBackend:uniscribe
endif

export WISI ?= $(shell cd ../../org.emacs.wisi; pwd)

ELPA_ROOT ?= $(shell cd ../../elpa; pwd)

BRANCH := $(notdir $(shell cd ..; pwd))

ifeq ($(BRANCH),org.emacs.gpr-compiler)
  TAR_FILE := org.emacs.gpr-mode-$(GNAT_COMPILER_VERSION)
else
  TAR_FILE := $(BRANCH)
endif

zip :
	rm -rf ../../$(TAR_FILE)
	mtn checkout --branch $(BRANCH) ../../$(TAR_FILE)
	tar jcf $(TAR_FILE).tar.bz2 -C ../.. $(TAR_FILE)

VPATH := ..

### Publish Gnu ELPA package
pub : force
# FIXME: just git push

pub-install: build-elpa install-elpa

# builds $(ELPA_ROOT)/archive-devel/*, from the last commit, _not_ the
# current workspace Also checks copyright; run elpa/GNUMakefile
# check/<pkg> first if added files.
build-elpa : force
	rm -rf $(ELPA_ROOT)/archive-devel
	make -C $(ELPA_ROOT)/ build/gnat-compiler build/wisi

uninstall-elpa :
	emacs -Q --eval '(progn (load-file "uninstall-elpa.el")(kill-emacs))'

# We don't kill emacs here, so we can check for compilation errors/warnings
install-elpa :
	emacs -Q --eval '(load-file "install-elpa.el")'

.PHONY : force

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# end:
# end of file
