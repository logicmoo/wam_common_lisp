# !/bin/bash

set -e
make clean && make
./build/lisp800 "lisp/core800.lisp" "test/smoke.lisp"
