name(wam_common_lisp).
version('1.1.118').
title('ANSI Common Lisp implemented in Prolog').
keywords([interpreters,lisp,sexpr,logicmoo]).
author( 'Douglas Miles', 'http://www.linkedin.com/in/logicmoo' ).
packager( 'TeamSPoon/LogicMoo', 'https://github.com/TeamSPoon/' ).
maintainer( 'TeamSPoon', 'https://github.com/TeamSPoon/' ).
home( 'https://github.com/TeamSPoon/wam_common_lisp' ).
download( 'https://github.com/TeamSPoon/wam_common_lisp/release/*.zip' ).
requires(dictoo).
requires(logicmoo_utils).
requires(loop_check).
requires(must_trace).
requires(no_repeats).
requires(predicate_streams).
requires(with_open_options).
requires(with_thread_local).
requires(xlisting).
autoload(false).

