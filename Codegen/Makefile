COMP_HOME=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
LANGDIR=$(COMP_HOME)/Abstract
XMLLIB=xml-light.cma
LANGLIB=language.cma
OCMLOPTS=-a
OCMALDEBUG=-g
SRC=andrewxml.ml
CC=ocamlc

all: backend.cma

backend.cma: 
	$(CC) -I $(LANGDIR) -I $(XMLDIR) $(XMLLIB) $(LANGLIB) $(OCAMLDEBUG) $(OCMLOPTS) $(SRC) -o $@

clean:
	rm -rf *cm* *o *a *mli
