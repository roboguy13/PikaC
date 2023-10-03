MAIN=paper

all: $(MAIN).pdf 

$(MAIN).pdf: $(MAIN).tex $(wildcard *.tex) *.bib
	pdflatex $(MAIN)
	bibtex $(MAIN)
	pdflatex $(MAIN)
	pdflatex $(MAIN)

.PHONY: clean

clean:
	rm -f $(MAIN).aux
	rm -f $(MAIN).bbl
	rm -f $(MAIN).blg
	rm -f $(MAIN).out
	rm -f $(MAIN).log
	rm -f $(MAIN).pdf

