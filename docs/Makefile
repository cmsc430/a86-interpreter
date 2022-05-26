DOC := Semantics.pdf

LATEX := latexmk

build: $(DOC)

%.pdf: %.tex
	$(LATEX) -pdf $^

clean:
	$(RM) *.aux *.log *.fls *.fdb_latexmk

clean-all: clean
	$(RM) *.pdf
