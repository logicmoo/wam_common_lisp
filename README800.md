lisp800
=======

lisp800 is based on Teemu Kalvas's lisp500. Lisp800 is an attempt to clean up the original Lisp500 source code to make it more useful for embedding and hacking.
The main goal is to make interpreter embeddable and multiple context-oriented.

## How to start
```bash
  cd src
  make
  rlwrap ./build/lisp800 lisp/init800.lisp
```
Note, that ``rlwrap`` is not mandatory, i.e. you can run this as ``./build/lisp800 lisp/init800.lisp`` but the latter one lacks convenient readline wrapper's features you may want to have.

## How to run smoke test
```bash
  cd src
  ./script/retest.sh
```
The execution output of the last command should contain PASSED at the last line, e.g.:
```text
OK: (LIST 4 3 2 1 0) is EQUAL to (ACCUM 4)
OK: 1 is EQ to (FOO)
OK: (MACROEXPAND-1 (QUOTE (DEFWRAP FOO))) is EQUAL to (QUOTE (DEFUN FOO NIL 1))
PASSED
```
