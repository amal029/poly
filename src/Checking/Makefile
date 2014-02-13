LANGDIR=../Abstract
BACKDIR=../backend
ERRORDIR=../Error
PARSEDIR=../Parsing
REWRITEDIR=../Optimization
XMLDIR=`ocamlfind query xml-light`
CC=ocamlopt
XMLLIB=xml-light.cmxa
LANGLIB=language.cmxa
CODEGENLIB=codegen.cmxa
REWRITELIB=optimizations.cmxa
PARSERLIB=parser.cmxa 
ERRORLIB=error.cmxa


all: pack

pack: check
	$(CC) -a *.cmx -o static-analysis.cmxa
check: 
	$(CC) -I $(LANGDIR) -I $(PARSEDIR) $(PARSELIB) $(LANGLIB) -c syntax_check.ml
	$(CC) -pp "camlp4o pa_macro.cmo -UDEBUG" -I $(LANGDIR) -I	\
	$(BACKDIR) -I $(REWRITEDIR) -I $(PARSEDIR) $(PARSELIB)		\
	$(LANGLIB) -I $(ERRORDIR) $(ERRORLIB) -c type_inference.ml

clean:
	rm -rf *.cm* *mli *.a *.o

