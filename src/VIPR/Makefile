LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Abstract
ERRORDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Error
BATTERIES=$(HOME)/batteries-included/_build/src
LANGLIB=language.cmxa
ERRORLIB=error.cmxa
BATLIBBOPT=batteries.cmxa
CC=ocamlopt
YACC=ocamlyacc
LEX=ocamllex


all: vipr.cmxa


vipr.cmxa: check
	$(CC) -a *.cmx -o $@


check:
	$(CC) -pp "camlp4o pa_macro.cmo -DDEBUG" -I $(LANGDIR) -I $(ERRORDIR) -c vipr.ml
	$(YACC) -v parser.mly
	$(LEX) lexer.mll
	$(CC) -c parser.mli
	$(CC) -c lexer.ml
	$(CC) -c parser.ml
	$(CC) -pp "camlp4o pa_macro.cmo -DDEBUG" -I $(BATTERIES) -I $(LANGDIR) -I $(ERRORDIR) -c vipr2poly.ml


clean:
	rm -rf *.cm* *.o *.a
