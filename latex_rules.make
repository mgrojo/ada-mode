# gnu make rules for processing TeX files, using TexLive distribution

.PRECIOUS : %.dvi %.aux %.out

export TEX_OPTIONS :=

# The include search path for latex is specified in TEX_INPUTS, not on the command line
%.dvi : %.tex %.aux
	latex $() -interaction nonstopmode -file-line-error $(TEX_OPTIONS) $<

# putting & here does not leave yap running; start it separately
%.view : %.dvi
	yap $<

%.print : %.dvi
	cygstart -p $<

# We use dvips and ps2pdf rather than pdflatex, because pdflatex
# doesn't support eepic, dvips does a better job with references, and
# it just works better :). Run latex one more time to get the
# references right.
%.ps : %.tex %.dvi
	latex $(LATEX_INCLUDES) -interaction nonstopmode -file-line-error $(TEX_OPTIONS) $<
	dvips -q -o $*.ps $*.dvi

%.pdf : %.ps
	ps2pdf $*.ps

%.out : %.tex %.dvi
	latex $(LATEX_INCLUDES) -interaction nonstopmode -file-line-error $(TEX_OPTIONS) $<

%.aux : %.tex
	latex $(LATEX_INCLUDES) -interaction nonstopmode -file-line-error $(TEX_OPTIONS) $<

%.bbl : %.aux %.bib
	bibtex $*.aux

clean ::
	rm -f *log *.aux *.bbl *.blg *.dvi *.pdf *.out *.toc *.xcp
	if [ -d auto ]; then rm -rf auto; fi

maintainer-clean :: clean

release-clean ::
	rm -f *.dvi *.log *.aux *.out

# end of file
