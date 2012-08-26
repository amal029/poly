LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Abstract
LANGLIB=language.cmxa
CC=ocamlopt
SRC=reporting.ml

all: error
	$(CC) -a *.cmx -o error.cmxa

error:
	$(CC) -I $(LANGDIR) $(LANGLIB) -c $(SRC)
	cp reporting.cmi ../Parsing/error.cmi
	# $(CC) -I $(LANGDIR) -i $(SRC) > ../Parsing/error.mli

clean:
	rm -rf *.cm* *.a *.o
