all : byte-compile autoloads

ifeq ($(shell uname),Linux)
EMACS_EXE ?= emacs
else ifeq ($(shell uname),Darwin)
EMACS_EXE ?= "/Applications/Emacs.app/Contents/MacOS/Emacs"
else
# windows
# specify uniscribe to workaround weird Windows harfbuzz bug
EMACS_EXE ?= emacs -xrm Emacs.fontBackend:uniscribe
endif

export WISI ?= $(system cd ../org.emacs.wisi; pwd)

byte-compile : byte-compile-clean force
	$(MAKE) -C $(WISI)/build byte-compile
	$(EMACS_EXE) -Q -batch -L . -L $(WISI) -l build/exclude-elpa.el --eval '(progn (batch-byte-compile))' *.el
#(setq byte-compile-error-on-warn t)

byte-compile-clean :
	cd ..; rm -f *.elc

autoloads : force
	$(EMACS_EXE) -Q -batch --eval "(progn (require 'autoload)(setq generated-autoload-file (expand-file-name \"../autoloads.el\"))(update-directory-autoloads \"../\"))"

.PHONY : force

# Local Variables:
# eval: (unless dvc-doing-ediff-p (load-file "prj.el"))
# end:
# end of file
