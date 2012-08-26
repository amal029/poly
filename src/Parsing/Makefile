ERRORDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Error
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
OCMLOPTS=-a
OCMXALDEBUG=-g
XMLLIB=xml-light.cmxa
ERRORLIB=error.cmxa
CC=ocamlopt
YACC=ocamlyacc
LEX=ocamllex


all: parser

parser:
	# $(CC) -c language.mli
	# $(CC) -c error.mli
	$(YACC) parser.mly
	$(LEX) lexer.mll
	$(CC) -c parser.mli
	$(CC) -c lexer.ml
	$(CC) -I $(ERRORDIR) -c parser.ml
	$(CC) -a *.cmx -o parser.cmxa

clean:
	rm -rf *o *cm* parser.mli lexer.mli *output lexer.ml parser.ml language.mli error.mli *.a
