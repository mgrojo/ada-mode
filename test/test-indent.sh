# Test indentation all Ada files in $1 using Emacs Ada mode 5.0
#
# $1: absolute path to a directory containing Ada files
# $2: output directory
# $3: elisp file setting user Ada mode options
#
# $EMACS_ADA_MODE: directory containing ada-mode.el (4.01 or 5.0)
# $EMACS_RUNTEST : path to runtest.el
# $EMACS         : emacs executable
#
# Each file matching *.ad[bs] is read into Emacs, reindented, and
# saved to $2 (with a flat directory structure, so file names must be
# unique). It is then diff'ed against the original.

if [ "x$3" = "x" ]; then
    echo "usage: test-indent.sh <source-dir> <output-dir> <options file>"
    exit
fi

if [ ! -d $1 ]; then
    echo "$1 not a directory"
    exit
fi

if [ ! -d $2 ]; then
    echo "$2 not a directory"
    exit
fi

if [ ! -r $3 ]; then
    echo "$3 not readable file"
    exit
fi

if [ "x$EMACS_ADA_MODE" = "x" ]; then
    echo "define EMACS_ADA_MODE"
    exit
fi

if [ "x$EMACS_RUNTEST" = "x" ]; then
    echo "define EMACS_RUNTEST"
    exit
fi

cd $2

for file in $1/*.ad[bs] ; do
    $EMACS -Q -batch -L $EMACS_ADA_MODE -l $3 -l $EMACS_RUNTEST --eval "(run-test \"$file\")"
    filename=`basename $file`
    diff -u $filename.tmp $file > $filename.diff
done

# end of file
