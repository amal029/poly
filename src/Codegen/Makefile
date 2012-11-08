BATTERIES=$(HOME)/batteries-included/_build/src
COMP_HOME=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src
XMLDIR=$(HOME)/Dropbox/Personal/OCAML_WORK/xml-light
LLVMDIR=/opt/llvm/lib/ocaml
#LLVMDIR=/macports-1.9.1/libexec/llvm-3.2/lib/ocaml/
DOTDIR=$(COMP_HOME)/backend
ERRORDIR=$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/poly/src/Error
LANGDIR=$(COMP_HOME)/Abstract
VECDIR=$(COMP_HOME)/Vectorization
XMLLIB=xml-light.cmxa
LANGLIB=language.cmxa
LLVMLIB=llvm.cmxa llvm_executionengine.cmxa llvm_bitreader.cmxa
ERRORLIB=error.cmxa
DOTLIB=codegen.cmxa
BATLIBBOPT=batteries.cmxa
OCMLOPTS=-a
OCMXALDEBUG=-g
SRC=andrewxml.ml myLlvm_cuda.ml myLlvm.ml
CC=ocamlopt

all: backend.cmxa


backend.cmxa: check
	$(CC) -a andrewxml.cmx myLlvm_cuda.cmx myLlvm.cmx -o $@

check: 
	$(CC) -pp "camlp4o pa_macro.cmo -DDEBUG" -I $(LANGDIR) -I	\
	   $(XMLDIR) -I $(VECDIR) -I $(DOTDIR) -I $(LLVMDIR) -I $(ERRORDIR)		\
	   -I $(BATTERIES) $(DOTLIB) $(ERRORLIB) $(XMLLIB) $(LANGLIB) $(LLVMLIB)	\
	  $(BATLIBBOPT) $(OCAMLDEBUG) -c $(SRC)

clean:
	rm -rf *cm* *o *a *mli
