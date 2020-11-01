# gnu make rules for processing TeX files, using MiKTeX distribution

.PRECIOUS : %.dvi %.aux %.out

TEX_OPTIONS := -quiet

%.dvi : %.tex
	latex $(LATEX_INCLUDES) -interaction nonstopmode --c-style-errors $(TEX_OPTIONS) $<

%.view : %.dvi
	cygstart $<

# %.out holds the bookmarks, %.aux the references
%.pdf : %.tex %.out %.aux
	pdflatex $(LATEX_INCLUDES) -interaction nonstopmode --c-style-errors $(TEX_OPTIONS) $<

%.out : %.tex
	pdflatex $(LATEX_INCLUDES) -interaction nonstopmode --c-style-errors $(TEX_OPTIONS) $<

%.aux : %.tex
	pdflatex $(LATEX_INCLUDES) -interaction nonstopmode --c-style-errors $(TEX_OPTIONS) $<

clean ::
	rm -f *log *.aux *.dvi *.pdf *.out
	if [ -d auto ]; then rm -rf auto; fi

maintainer-clean :: clean

release-clean ::
	rm -f *.dvi *.log *.aux *.out

# end of file
