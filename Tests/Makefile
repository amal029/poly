COMP_HOME=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml
LANGDIR=$(COMP_HOME)/Abstract
PARSEDIR=$(COMP_HOME)/Parsing
ANALYSISDIR=$(COMP_HOME)/Checking
REWRITEDIR=$(COMP_HOME)/Rewrites
OPTDIR=$(COMP_HOME)/Optimization
PARSELIB=parser.cma
LANGLIB=language.cma
REWRITES=rewrites.cma
OPTIMIZATIONS=optimizations.cma
ANALYSIS=static-analysis.cma
CC=ocamlc.opt
SRC=polyc.ml
TARGET=polyc

all: compile

run:
	mkdir -p ../output
	./polyc $(FILE)

compile:
	$(CC) -I $(PARSEDIR) -I $(LANGDIR) -I $(REWRITEDIR) -I $(OPTDIR) \
	-I $(ANALYSISDIR) $(PARSELIB) $(LANGLIB) \
	$(REWRITES) $(OPTIMIZATIONS) $(ANALYSIS) $(SRC) -o $(TARGET)

clean:
	rm -rf ../output polyc *.cm* *mli

