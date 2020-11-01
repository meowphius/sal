# gnu make rules for processing texinfo files

%.dvi : %.texinfo
	latex $(TEXI_INCLUDE) --c-style-errors $(TEXI_DVI_OPTS) '\nonstopmode\input{$<}'

%.view : %.dvi
	cmd /c start $<

%.pdf : %.texinfo %.out %.aux
	pdflatex --c-style-errors $(TEXI_INCLUDE) $(TEXI_PDF_OPTS) '\nonstopmode\input{$<}'

%.out : %.texinfo
	pdflatex --c-style-errors $(TEXI_INCLUDE) $(TEXI_PDF_OPTS) '\nonstopmode\input{$<}'

%.aux : %.texinfo
	pdflatex --c-style-errors $(TEXI_INCLUDE) $(TEXI_PDF_OPTS) '\nonstopmode\input{$<}'

%.info : %.texinfo
	makeinfo $(TEXI_INFO_OPTS) $<

%.html : %.texinfo
	makeinfo --html --no-split $(TEXI_HTML_OPTS) -o $*.html $<

clean ::
	rm -f *.aux *.dvi *.html *.info *.log *.out *.pdf

release-clean ::
	rm -f *.aux *.dvi *.log *.out

maintainer-clean :: clean

# end of file
