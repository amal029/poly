LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Abstract
LANGLIB=language.cma
CC=ocamlc
SRC=reporting.ml

all: error
	$(CC) -I $(LANGDIR) $(LANGLIB) -a *.cmo -o error.cma

error:
	$(CC) -I $(LANGDIR) -i $(SRC) > ../Parsing/error.mli
	$(CC) -I $(LANGDIR) $(LANGLIB) -c $(SRC)

clean:
	rm -rf *.cm*
