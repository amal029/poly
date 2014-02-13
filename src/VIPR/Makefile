LANGDIR=../language
ERRORDIR=../error
BATTERIES=`ocamlfind query batteries`
DOTDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/backend
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
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(LANGDIR) -I $(ERRORDIR) -c vipr.ml
	$(YACC) -v viprParser.mly
	$(LEX) viprLexer.mll
	$(CC) -c viprParser.mli
	$(CC) -c viprLexer.ml
	$(CC) -c viprParser.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(DOTDIR) -I $(BATTERIES) -I $(LANGDIR) -I $(ERRORDIR) -c vipr2poly.ml


clean:
	rm -rf *.cm* *.o *.a viprParser.ml viprLexer.ml viprParser.mli *.output
