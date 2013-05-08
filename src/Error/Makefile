LANGDIR=$(ROOTDIR)/Abstract
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
