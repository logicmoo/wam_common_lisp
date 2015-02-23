lisp500 : lisp500.o
	cc -o lisp500 lisp500.o

lisp500.o : lisp500.c
	cc -c lisp500.c

clean :
	rm lisp500 lisp500.o
