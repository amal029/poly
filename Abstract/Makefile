OCMLOPTS=-a
OCMALDEBUG=-g
CC=ocamlc
SRC=language.ml

all: language.cma

language.cma: check
	$(CC) $(OCAMLDEBUG) $(OCMLOPTS) $(SRC) -o $@

check:
	$(CC) -i $(SRC) > language.mli
	$(CC) -i $(SRC) > ../Parsing/language.mli
	$(CC) -c language.mli

clean:
	rm -rf *cm* *o *a *mli
