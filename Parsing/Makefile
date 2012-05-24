LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Abstract
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
OCMLOPTS=-a
OCMALDEBUG=-g
XMLLIB=xml-light.cma
LANGLIB=language.cmxa
CC=ocamlc
YACC=ocamlyacc
LEX=ocamllex


all: parser

parser:
	$(CC) -c language.mli
	$(YACC) parser.mly
	$(LEX) lexer.mll
	$(CC) -c parser.mli
	$(CC) -c lexer.ml
	$(CC) -c parser.ml
	$(CC) -a *.cmo -o parser.cma

clean:
	rm -rf *o *cm* parser.mli lexer.mli *output lexer.ml parser.ml language.mli
