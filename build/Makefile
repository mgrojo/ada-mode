# Build manuals; see subdirs for building code and running tests (each
# subdir supports a different indentation engine)

all : info html

MANUALS := ada-mode gpr-mode

info : $(addsuffix .info, $(MANUALS))
html : $(addsuffix .html, $(MANUALS))

VPATH := ..

%.info : %.texi
	makeinfo $< -o ../$@

%.html : %.texi
	makeinfo --html --no-split $< -o ../$@

# end of file
