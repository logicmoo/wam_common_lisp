/*******************************************************************
 *
 * A Lisp compiler, written in Prolog
 *
 * (lisp_library.pl)
 *
 * (c) Neil Smith, 2001
 *
 * This program provides some built-in functionality for the 
 * Lisp compiler.  It requires that the file lisp_compiler.pl has 
 * already been successfully compiled.
 *
 * Definitions in this file are given in the Lisp-like syntax 
 * read by this compiler.
 *
 *******************************************************************/

second(l) <<== 
	first(rest(l)).

third(l) <<==
	first(rest(rest(l))).


% We don't support &rest parameters yet, so we need a different
% definition of list for every different number of arguments

list_1(a) <<== 
	cons(a, nil).

list_2(a, b) <<== 
	cons(a, list_1(b)).

list_3(a, b, c) <<== 
	cons(a, list_2(b,c)).


lisp_append(l1, l2) <<==
	if( null(l1), 
	    l2, 
	    cons( first(l1),
	          lisp_append(rest(l1),
	                      l2))).
 

mapcar(func, l) <<==
	if( null(l), 
		nil,
		cons( lisp_apply(func, list_1(first(l))),
			mapcar(func, rest(l)))).


