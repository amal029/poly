XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
XMLLIB=xml-light.cma
OCMLOPTS=-a
OCMALDEBUG=-g
CC=ocamlc
SRC=language.ml
SRC2=andrewLang.ml

all: language.cma

language.cma: check
	$(CC) -I $(XMLDIR) $(XMLLIB) $(OCAMLDEBUG) $(OCMLOPTS) $(SRC) $(SRC2) -o $@

check:
	$(CC) -i $(SRC) > language.mli
	# $(CC) -I $(XMLDIR) -i $(SRC) $(SRC2)
	$(CC) -i $(SRC) > ../Parsing/language.mli
	$(CC) -c language.mli

clean:
	rm -rf *cm* *o *a *mli
