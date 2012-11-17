CC=clang

INCLUDES=/usr/local/cuda/include

SYS=$(shell uname -s)

ifeq ($(SYS),Darwin)
AS=llvm-as
else
AS=llvm-as-3.0
endif

INCLUDES +=-I$(HOME)/Dropbox/IBM_Work/START_WORK_IRELAND/NVIDIA_NVVM/LibNVVM/lib/universal_Darwin_release/
INCLUDES +=-I.

CFLAGS=-g -O0 -S -emit-llvm -I$(INCLUDES)

all: print.ll nvvm.ll

print.ll:
	$(CC) $(CFLAGS) -o $@ print.c
	$(AS) print.ll -o print.bc

nvvm.ll:
	$(CC) $(CFLAGS) -o $@ nvvm.c
	$(AS) nvvm.ll -o nvvm.bc

clean:
	rm -rf gmon.out *.ll *.bc
