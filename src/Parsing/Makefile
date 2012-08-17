ERRORDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Error
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
OCMLOPTS=-a
OCMALDEBUG=-g
XMLLIB=xml-light.cma
ERRORLIB=error.cma
CC=ocamlc
YACC=ocamlyacc
LEX=ocamllex


all: parser

parser:
	$(CC) -c language.mli
	$(CC) -c error.mli
	$(YACC) parser.mly
	$(LEX) lexer.mll
	$(CC) -c parser.mli
	$(CC) -c lexer.ml
	$(CC) -I $(ERRORDIR) -c parser.ml
	$(CC) -I $(ERRORDIR) $(ERRORLIB) -a *.cmo -o parser.cma

clean:
	rm -rf *o *cm* parser.mli lexer.mli *output lexer.ml parser.ml language.mli error.mli
