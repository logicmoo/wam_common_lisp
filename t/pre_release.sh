#!/bin/bash -x

true running sanity-test.lisp
./wam_repl.pl --load sanity-test.lisp --markdown --quit >  sanity-test.lisp.md

true you should see '("hello" "from" "lisp")' fairly soon
./wam_repl.pl hello.lisp from lisp

true compiling hello.lisp
rm -f  ./hello.lisp.pro
./wam_repl.pl --compile hello.lisp
chmod +x ./hello.lisp.pro

true you should see '("hello" "from" "prolog")' fairly soon
./hello.lisp.pro from prolog

true compiling hello.lisp to executable
./wam_repl.pl --compile hello.lisp --exe ./hello

true you should see ("hello" "from" "exe") immediately
./hello from exe

