# Test indentation all Ada files in $1 using Emacs Ada mode 5.0
#
# $1: absolute path to a directory containing Ada files
# $2: output directory
#
# $EMACS_ADA_MODE_5: directory containing ada-indent.el
# $EMACS : emacs executable
#
# Each file matching *.ad[bs] is read into Emacs, reindented, and
# saved to $2 (with a flat directory structure, so file names must be
# unique). It is then diff'ed against the original.

if [ "x$2" = "x" ]; then
    echo "usage: test-indent.sh <source-dir> <output-dir>"
    exit
fi

if [ "x$EMACS_ADA_MODE_5" = "x" ]; then
    echo "define EMACS_ADA_MODE_5"
    exit
fi

cd $2

for file in $1/*.ad[bs] ; do
    $EMACS -Q -batch -L $EMACS_ADA_MODE_5 -l test/runtest.el --eval "(run-test \"$file\")"
    filename=`basename $file`
    diff -u $file $filename.tmp > $filename.diff
done

# end of file
