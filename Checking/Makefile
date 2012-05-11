LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Abstract
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
CC=ocamlc
XMLLIB=xml-light.cma
LANGLIB=language.cma


all: pack

pack: check
	$(CC) -I $(LANGDIR) $(LANGLIB) -a *.cmo -o static-analysis.cma
check: 
	$(CC) -I $(LANGDIR) $(LANGLIB) -c syntax_check.ml
	$(CC) -I $(LANGDIR) $(LANGLIB) -c type_inference.ml

clean:
	rm -rf *.cm*

