@echo off
rem WAM-CL launcher (Windows). Starts the Common Lisp REPL, or runs args.
rem Works from any directory; resolves paths relative to this script.
setlocal
set "HERE=%~dp0"
set "HERE=%HERE:\=/%"
swipl -p "library=%HERE%prolog" -g lisp "%HERE%prolog/wamcl.pl" %*
endlocal
