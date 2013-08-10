all:
	pdflatex thesis.tex
	bibtex thesis
	pdflatex thesis.tex
	pdflatex thesis.tex


clean:
	rm *.aux
	rm *.bbl
	rm *.blg
	rm *.log
	rm *.lol
	rm *.lot
	rm *.lof
	rm *.toc
