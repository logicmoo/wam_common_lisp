
CMN     = -m32 -g -O0
#CMN     = -m32 -O2 -fomit-frame-pointer

CFLAGS  = $(CMN) -pedantic -Wall
CC      = gcc
LFLAGS  = $(CMN) -lm -ldl
LINKER  = gcc


## Rules

build/lisp800: build/lisp800.o
	$(LINKER) -o build/lisp800 build/lisp800.o $(LFLAGS)

build/lisp800.o: build c/lisp800.c
	$(CC) $(CFLAGS) -c c/lisp800.c -o build/lisp800.o

build:
	mkdir build

clean:
	rm -rf build
