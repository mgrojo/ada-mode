# Create Makefile.conf, which defines HAVE_GNAT_XREF, HAVE_LIBADALANG

echo 'with "gnatcoll_xref"; abstract project check is end check;' > check.gpr
gprbuild -P check.gpr > /dev/null 2>&1
if test $? -eq 0 ; then
    echo 'HAVE_GNATCOLL_XREF := "yes"' > Makefile.conf
else
    echo 'HAVE_GNATCOLL_XREF := "no"' > Makefile.conf
fi

echo 'with "libadalang"; abstract project check is end check;' > check.gpr
gprbuild -P check.gpr > /dev/null 2>&1
if test $? -eq 0 ; then
    echo 'HAVE_LIBADALANG := "yes"' >> Makefile.conf
else
    echo 'HAVE_LIBADALANG := "no"' >> Makefile.conf
fi

# end of file
