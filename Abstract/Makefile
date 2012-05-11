OCMLOPTS=-a
OCMALDEBUG=-g
CC=ocamlc
SRC=language.ml

all: language.cma

language.cma: check
	$(CC) $(OCAMLDEBUG) $(OCMLOPTS) $(SRC) -o $@

check:
	$(CC) -i $(SRC) > ../Parsing/language.mli

clean:
	rm -rf *cm* *o *a
