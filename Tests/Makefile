LANGDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Abstract
PARSEDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml/Parsing
PARSELIB=parser.cma
LANGLIB=language.cma
CC=ocamlc
SRC=polyc.ml
TARGET=polyc

all: compile

run:
	mkdir -p ../output
	./polyc $(FILE)

compile:
	$(CC) -I $(PARSEDIR) -I $(LANGDIR) $(PARSELIB) $(LANGLIB) $(SRC) -o $(TARGET)

clean:
	rm -rf ../output polyc *.cm* *mli

