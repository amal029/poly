COMP_HOME=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml
BATTERIES=/macports-1.9.1/lib/ocaml/site-lib/batteries/
CAMOMILEDIR=/macports-1.9.1/lib/ocaml/site-lib/camomile
BIGARRAYDIR=/macports-1.9.1/lib/ocaml/
LANGDIR=$(COMP_HOME)/Abstract
PARSEDIR=$(COMP_HOME)/Parsing
ANALYSISDIR=$(COMP_HOME)/Checking
REWRITEDIR=$(COMP_HOME)/Rewrites
OPTDIR=$(COMP_HOME)/Optimization
BACKEND=$(COMP_HOME)/backend
PARSELIB=parser.cma
LANGLIB=language.cma
REWRITES=rewrites.cma
OPTIMIZATIONS=optimizations.cma
ANALYSIS=static-analysis.cma
CODEGEN=codegen.cma
BATLIB=batteries_uni.cma
BATLIBB=batteries.cma
CAMOMILELIB=camomile.cma
CAMOMILELIBB=camomileLibrary.cma
BIGARRAYLIB=bigarray.cma
NUMSLIB=nums.cma
UNIXLIB=unix.cma
STRLIB=str.cma
CC=ocamlc
SRC=polyc.ml
TARGET=polyc
FIND=ocamlfind

all: compile

run:
	mkdir -p ../output
	./polyc $(FILE)

compile:
	$(CC) -I $(PARSEDIR) -I $(LANGDIR) -I $(REWRITEDIR) -I $(ANALYSISDIR) -I $(BACKEND) -I $(OPTDIR) $(PARSELIB) $(LANGLIB) $(CODEGEN) $(REWRITES) $(ANALYSIS) $(OPTIMIZATIONS) $(SRC) -o $(TARGET)

clean:
	rm -rf ../output ../output1 polyc *.cm* *mli

