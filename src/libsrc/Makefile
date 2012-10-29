CC=clang

INCLUDES=/usr/local/cuda/include

INCLUDES +=-I$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/NVIDIA_NVVM/LibNVVM/lib/universal_Darwin_release/
INCLUDES +=-I.

CFLAGS=-DDEBUG -O3 -S -emit-llvm -DDEBUG -I$(INCLUDES)

all: print.ll nvvm.ll

print.ll:
	$(CC) $(CFLAGS) -o $@ print.c

nvvm.ll:
	$(CC) $(CFLAGS) -o $@ nvvm.c

clean:
	rm -rf gmon.out *.ll *.bc
