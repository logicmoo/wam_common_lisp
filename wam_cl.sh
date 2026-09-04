#!/usr/bin/env bash
# WAM-CL launcher (Unix). Starts the Common Lisp REPL, or runs args.
# Works from any directory; resolves paths relative to this script.
set -e
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec swipl -p "library=$HERE/prolog" -g "set_prolog_flag(lisp_interactive,true),lisp" "$HERE/prolog/wamcl.pl" "$@"
