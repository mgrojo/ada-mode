# Create Makefile.conf, which defines HAVE_TREE_SITTER

tree-sitter --help > /dev/null 2>&1
if test $? -eq 0 ; then
    echo 'HAVE_TREE_SITTER := "yes"' >> Makefile.conf
else
    echo 'HAVE_TREE_SITTER := "no"' >> Makefile.conf
fi

# end of file
