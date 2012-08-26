XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
XMLLIB=xml-light.cmxa
OCMLOPTS=-a
OCMALDEBUG=-g
CC=ocamlopt
SRC=language.ml
SRC2=andrewLang.ml

all: language.cmxa

language.cmxa: check
	$(CC) $(OCMLOPTS) language.cmx andrewLang.cmx -o $@

check:
	$(CC) -I $(XMLDIR) $(XMLLIB) -c $(SRC)
	$(CC) -I $(XMLDIR) $(XMLLIB) -c $(SRC2)
	cp language.cmi ../Parsing/language.cmi
	# $(CC) -i $(SRC) > language.mli
	# $(CC) -i $(SRC) > ../Parsing/language.mli

clean:
	rm -rf *cm* *o *a *mli
