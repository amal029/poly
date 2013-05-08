COMP_HOME=$(ROOTDIR)
XMLLIB=xml-light.cmxa
ERRORDIR=$(COMP_HOME)/Error
LIBDIR=$(COMP_HOME)/lib
LANGDIR=$(COMP_HOME)/Abstract
VIPRDIR=$(COMP_HOME)/VIPR
VECDIR=$(COMP_HOME)/Vectorization
PARSEDIR=$(COMP_HOME)/Parsing
ANALYSISDIR=$(COMP_HOME)/Checking
REWRITEDIR=$(COMP_HOME)/Rewrites
CODEGENDIR=$(COMP_HOME)/Codegen
OPTDIR=$(COMP_HOME)/Optimization
BACKEND=$(COMP_HOME)/backend
CODEGENLIB=backend.cmxa
PARSELIB=parser.cmxa
LANGLIB=language.cmxa
REWRITES=rewrites.cmxa
OPTIMIZATIONS=optimizations.cmxa
ANALYSIS=static-analysis.cmxa
CODEGEN=codegen.cmxa
BATLIBB=batteries.cmxa
CAMOMILELIB=camomile.cmxa
CAMOMILELIBB=camomileLibrary.cmxa
BIGARRAYLIB=bigarray.cmxa
STRLIB=str.cmxa
NUMSLIB=nums.cmxa
UNIXLIB=unix.cmxa
STRLIB=str.cmxa
ERRORLIB=error.cmxa
# FILEUTILS=fileutils.cmxa
LLVMLIB=llvm_bitreader.cmxa llvm.cmxa llvm_scalar_opts.cmxa llvm_bitwriter.cmxa llvm_analysis.cmxa llvm_executionengine.cmxa llvm_target.cmxa llvm_ipo.cmxa
CC=ocamlopt
SRC=polyc.ml
TARGETC=polyc
FIND=ocamlfind
METISLIB=lib-metis.cmxa
DOTLIB=odot.cmxa
VECLIB=veclang.cmxa
VIPRLIB=vipr.cmxa

all: compiler

run:
	mkdir -p ../output
	./polyc $(FILE)

compiler:
	$(CC) -I $(VECDIR) -I $(ODOTDIR) -I $(LLVMDIR) -I $(PARSEDIR) -I $(LANGDIR)	\
	 -I $(VIPRDIR) -I $(REWRITEDIR) -I $(ANALYSISDIR) -I $(BACKEND) -I $(OPTDIR)	\
	-I $(METISDIR) -I $(BATTERIES) $(LLVMLIB) $(STRLIB) $(BIGARRAYLIB)	\
	$(UNIXLIB) $(NUMSLIB) $(BATLIBB) -I $(ERRORDIR) $(DOTLIB)	\
	 -I $(XMLDIR) $(XMLLIB) $(ERRORLIB) $(PARSELIB) $(LANGLIB) $(CODEGEN) $(VIPRLIB) $(REWRITES)	\
	$(ANALYSIS) $(OPTIMIZATIONS) $(VECLIB) -I $(CODEGENDIR) $(CODEGENLIB) -I	\
	$(METISDIR) -I $(LIBDIR) $(METISLIB) -cc clang -cclib -lstdc++	\
	 -ccopt -rdynamic $(SRC) -o $(TARGETC)

clean:
	rm -rf ../output ../output1 polyc polyi *.cm* *mli *.a *.o

