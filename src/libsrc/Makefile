CC=clang

INCLUDES=/usr/local/cuda/include

INCLUDES +=-I$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/NVIDIA_NVVM/LibNVVM/lib/universal_Darwin_release/
INCLUDES +=-I.

CFLAGS=-S -emit-llvm -DDEBUG -I$(INCLUDES)

all: print.bc nvvm.bc

print.bc:
	$(CC) $(CFLAGS) -o $@ print.c

nvvm.bc:
	$(CC) $(CFLAGS) -o $@ nvvm.c

clean:
	rm -rf gmon.out *.ll *.bc
