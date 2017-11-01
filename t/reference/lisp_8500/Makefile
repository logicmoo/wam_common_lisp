#2012-12-05 20:59:45 +0100	<pjb>	dim: gcc -m32 -o lisp500 lisp500.c -ldl -lm && ./lisp500
#2012-12-05 21:00:04 +0100	<pjb>	It must be compiled in 32-bit.
# dmailes says dont forget to
#   apt-get install libc6-dev-i386 linux-libc-dev:i386 linux-libc-dev

CMN     = -m32 -g -O0
#CMN     = -m32 -O2 -fomit-frame-pointer

CFLAGS  = $(CMN) -pedantic -Wall
CC      = gcc
LFLAGS  = $(CMN) -lm -ldl
LINKER  = gcc


## Rules

./lisp800: ./lisp800.o
	$(LINKER) -o ./lisp800 ./lisp800.o $(LFLAGS)

./lisp800.o: build ./lisp800.c
	$(CC) $(CFLAGS) -c ./lisp800.c -o ./lisp800.o

build:
	mkdir build

clean:
	rm -rf build




