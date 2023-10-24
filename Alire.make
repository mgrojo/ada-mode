# For compiling ada-mode Ada with Alire

STEPHES_ADA_LIBRARY_ALIRE_PREFIX ?= $(CURDIR)/../org.stephe_leake.sal
WISITOKEN_ALIRE_PREFIX ?= $(CURDIR)/../org.wisitoken

include $(STEPHES_ADA_LIBRARY_ALIRE_PREFIX)/build/alire_rules.make
include $(WISITOKEN_ALIRE_PREFIX)/build/wisitoken_alire_rules.make

# normally run 'alire-build' target; set ALIRE_BUILD_ARGS to --release


# debug/release determined by */config/*.gpr; run alr build [--release] to change
# or save/copy that .gpr file
bin/% : alr.env force
	. ./alr.env; gprbuild -P emacs_ada_mode.gpr


TEST_FILE ?= ada_mode-function_2.adb
one :: ELISP ?= (setq-default save-parser-log "../debug-1.log" save-edited-text "../debug_edited-1")
#one :: bin/ada_mode_wisi_lalr_parse
#one :: byte-compile autoloads
one :: one-clean
one-clean :: force
	rm -f $(TEST_FILE).*
one :: RUNTEST := run-indent-test-lalr-partial-process-gpr_query.el
#one :: RUNTEST := run-indent-test-lalr-incremental-process-gpr_query.el
one :: $(addsuffix .diff, $(TEST_FILE))

.PRECIOUS : %.diff

vpath %.adb   test test/subdir
vpath %.ads   test test/subdir

%.diff : % %.tmp
	-diff -u $< $(*F).tmp > $(*F).diff

export WISITOKEN     ?= $(shell cd ../org.wisitoken; pwd)
export WISI          ?= $(shell cd ../org.emacs.wisi; pwd)
export GNAT_COMPILER ?= $(shell cd ../elpa/packages/gnat-compiler; pwd)
export GPR_QUERY     ?= $(shell cd ../elpa/packages/gpr-query; pwd)

ADA_MODE_DIR := -L build -l autoloads.el -L $(WISI) -l $(WISI)/autoloads.el -L $(GNAT_COMPILER) -l $(GNAT_COMPILER)/autoloads.el -L $(GPR_QUERY) -l $(GPR_QUERY)/autoloads.el -l exclude-elpa.el

%.tmp : %
	emacs -Q -L . $(ADA_MODE_DIR) -l $(RUNTEST) --eval '(progn $(ELISP)(run-test "$<")(kill-emacs))' $(RUN_ARGS)

# Local Variables:
# eval: (load-file "prj-alire.el")
# End:
