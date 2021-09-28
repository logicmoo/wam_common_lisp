#!/bin/sh
BREAK_CHARS="(){}[],^%$#@\"\";''|\\"
RLWRAP=
if [ $TERM == "dumb" ]; then  # slime
  RLWRAP=
else
  RLWRAP="rlwrap --remember --history-filename=$HOME/.sbcl_history --histsize=1000000 -c -b $BREAK_CHARS -f $HOME/.sbcl_completions"
fi
if [ $# -eq 0 ]; then
  exec $RLWRAP /opt/local/bin/sbcl
else # permits #!/usr/bin/env sbcl , but breaks sbcl --help, etc.
  exec /opt/local/bin/sbcl --script $*
fi

