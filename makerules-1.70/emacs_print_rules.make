# make rules for producing PDF listing files.
#
# Environment variables:
# GNU_EMACS_CUSTOM : location of stephe_emacs and stephe_emacs_site_lisp

%.ada.ps : %.ada
	emacs -q --load=$(MAKERULES)/ps-ada.el --file=$< --eval="(ada-ps-buffer)" --kill

# for printing several files together, with consecutive line numbers
%.ada : $(ADA_SOURCES)
	cat $(ADA_SOURCES) > $@

%.pdf : %.ps
	ps2pdf -dCompatibility=1.4 -dPDFSETTINGS=/screen $< $@

clean ::
	rm -f *.ps *.pdf

# end of file
