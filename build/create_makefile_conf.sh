# Create Makefile.conf, which defines HAVE_GNAT_XREF

echo 'with "gnatcoll_xref"; abstract project check_xref is end check_xref;' > check_xref.gpr
gprbuild -P check_xref.gpr > /dev/null 2>&1
if test $? -eq 0 ; then
    echo 'HAVE_GNAT_XREF := "yes"' > Makefile.conf
else
    echo 'HAVE_GNAT_XREF := "no"' > Makefile.conf
fi

# end of file
