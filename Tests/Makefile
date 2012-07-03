COMP_HOME=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/std-lang/Ocaml
BATTERIES=/macports-1.9.1/lib/ocaml/site-lib/batteries/
CAMOMILEDIR=/macports-1.9.1/lib/ocaml/site-lib/camomile
BIGARRAYDIR=/macports-1.9.1/lib/ocaml/
LLVMDIR=/macports-1.9.1/libexec/llvm-3.2/lib/ocaml/
ERRORDIR=$(COMP_HOME)/Error
LIBDIR=$(COMP_HOME)/lib
LANGDIR=$(COMP_HOME)/Abstract
PARSEDIR=$(COMP_HOME)/Parsing
ANALYSISDIR=$(COMP_HOME)/Checking
REWRITEDIR=$(COMP_HOME)/Rewrites
CODEGENDIR=$(COMP_HOME)/Codegen
OPTDIR=$(COMP_HOME)/Optimization
BACKEND=$(COMP_HOME)/backend
CODEGENLIB=backend.cma
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
ERRORLIB=error.cma
FILEUTILS=fileutils.cma
LLVMLIB=llvm_scalar_opts.cma llvm_bitwriter.cma llvm_analysis.cma llvm_executionengine.cma llvm_target.cma
CC=ocamlc
SRC=polyc.ml
TARGETC=polyc
FIND=ocamlfind

all: compiler

run:
	mkdir -p ../output
	./polyc $(FILE)

compiler:
	$(CC) -I $(LLVMDIR) -I $(PARSEDIR) -I $(LANGDIR) -I		\
	   $(REWRITEDIR) -I $(ANALYSISDIR) -I $(BACKEND) -I $(OPTDIR)	\
	  -I $(ERRORDIR) $(ERRORLIB) $(PARSELIB) $(LANGLIB) $(CODEGEN)	\
	   $(REWRITES) $(ANALYSIS) $(OPTIMIZATIONS) -I $(CODEGENDIR)	\
	   $(CODEGENLIB) -I $(LIBDIR) $(FILEUTILS) $(LLVMLIB) -cc g++	\
	  -cclib -lstdc++ -ccopt -rdynamic -custom $(SRC) -o $(TARGETC)

clean:
	rm -rf ../output ../output1 polyc polyi *.cm* *mli

