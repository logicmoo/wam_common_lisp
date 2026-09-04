#!/bin/bash

DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

(
cd $DIR0

if [ ! -f /.dockerenv ]; then
   lmoo $DIR0/$(basename "${BASH_SOURCE[0]}") $*
   return 0 2>/dev/null
   exit 0
fi

export GLOB="$*"

CMD_TIMEOUT=5m 

[ -n "${MAX_TEST_SUITE_TIME}" ] && [ $MAX_TEST_SUITE_TIME -lt 5 ] && (
  export CMD_TIMEOUT=10s
  GLOB="*1*.*"
  echo "Warn: (MAX_TEST_SUITE_TIME < 5m) we want our tests to be short:  CMD_TIMEOUT=${CMD_TIMEOUT}"
)


[ -z "$GLOB" ] && GLOB="*0*.*"
lmoo-junit "$GLOB"

)
