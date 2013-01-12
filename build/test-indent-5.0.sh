# run test-indent.sh with Emacs Ada mode 5.0
#
# $1: absolute path to a directory containing Ada files
# $2: output directory

export EMACS_ADA_MODE=/Projects/org.emacs.ada-mode.smie
export EMACS_SMIE_TEST=/Projects/org.emacs.ada-mode.smie/test
export EMACS_RUNTEST=$EMACS_SMIE_TEST/runtest.el
export EMACS=/apps/emacs-24.2/bin/emacs.exe

time $EMACS_SMIE_TEST/test-indent.sh $1 $2 $EMACS_SMIE_TEST/gds-options-5.0.el

# end of file
