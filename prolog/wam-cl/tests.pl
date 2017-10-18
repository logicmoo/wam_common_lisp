/*******************************************************************
 *
 * A Lisp compiler, written in Prolog
 *
 * (tests.pl)
 *
 * (c) Neil Smith, 2001
 *
 * A few sample function definitions, mainly used by me as simple 
 * test cases for the compiler.  I'm sure you can come up with 
 * something better...
 *
 *******************************************************************/



simple(x) <<== x.


lisp_append_2(l1, l2) <<==
	cond(  [[null(l1), l2], 
		[t,	cons( first(l1),
			      lisp_append_2(rest(l1),
			                    l2))]]).
 

lisp_error(x) <<== setq(y, 5).


lisp_let() <<==
	let([bind(x, 3), bind(y, 5)], 
		[	x, 		% implicit progn here
			y]).


% maps 'first' over a list of lists
mapfirst(l) <<==
	mapcar(function(first), l).


<<== defvar(fred, 13).

<<== defvar(george).


reset_george(val) <<==
	setq(george, val).


make_adder(x) <<==
	function(lambda([y], plus(x, y))).


scale_list(xs, scale) <<==
	let([bind(fred, function(lambda([num], times(scale, num))))],
		mapcar(fred, xs)).


make_summer(total) <<== 
	function(lambda([n],
		setq(total, plus(total, n)))).


sum_with_map(xs) <<==
	let([bind(running_total, 0)],
		let([bind(summer, function(lambda([n], setq(running_total, 
							plus(running_total, n)))))],
		[ mapcar(summer, xs),
		  running_total ])).


