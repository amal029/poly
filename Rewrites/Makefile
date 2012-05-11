LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Abstract
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
CC=ocamlc
XMLLIB=xml-light.cma
LANGLIB=language.cma

all: fcfg


fcfg: fcheck
	$(CC) -I $(LANGDIR) $(LANGLIB) -a *.cmo -o rewrites.cma
fcheck: 
	$(CC) -I $(LANGDIR) -i fcfg.ml
	$(CC) -I $(LANGDIR) -i cfg.ml
	$(CC) -I $(LANGDIR) $(LANGLIB) -c fcfg.ml
	$(CC) -I $(LANGDIR) $(LANGLIB) -c cfg.ml

clean:
	rm -rf *.cm*
