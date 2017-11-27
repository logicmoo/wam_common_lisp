name(wam_common_lisp).
version('1.1.118').
title('ANSI Common Lisp implemented in Prolog').
keywords([interpreters,lisp,sexpr,logicmoo]).
author( 'Douglas Miles', 'http://www.linkedin.com/in/logicmoo' ).
packager( 'TeamSPoon/LogicMoo', 'https://github.com/TeamSPoon/' ).
maintainer( 'TeamSPoon', 'https://github.com/TeamSPoon/' ).
home( 'https://github.com/TeamSPoon/wam_common_lisp' ).
download( 'https://github.com/TeamSPoon/wam_common_lisp/release/*.zip' ).
requires(loop_check).
requires(predicate_streams).
requires(no_repeats).
requires(dictoo).
requires(must_trace).
requires(logicmoo_utils).
autoload(false).

