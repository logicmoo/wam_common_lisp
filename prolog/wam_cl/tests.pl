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


:- dynamic(tst:is_local_test/1).
:- multifile(tst:is_local_test/1).
:- discontiguous(tst:is_local_test/1).
:- dynamic(tst:is_local_test/2).
:- multifile(tst:is_local_test/2).
:- discontiguous(tst:is_local_test/2).
:- dynamic(tst:is_local_test/3).
:- multifile(tst:is_local_test/3).
:- discontiguous(tst:is_local_test/3).


tst:is_local_test(H):- tst:is_local_test(H,_V).
   
tst:is_local_test(H,V):-
  clause(tst:is_local_test(_,H,V),true,_).
   
tst:is_local_test(_,H,_):-
  clause(tst:is_local_test(H),true,_).
tst:is_local_test(_,H,V):-
  clause(tst:is_local_test(H,V),true,_).

call_test_compiled(Name,Value):- 
  must_or_rtrace(compile_test(Name,Code,Return,Expected)),
  debug_var('Return',Return),
  debug_var('Expected',Expected),
  debug_var('OutValue',OutValue),
  ignore(catch((Code,Return=Value),goto(_,OutValue,_),Value=OutValue)),
  dmsg(Expected=Value).

  

compile_test(Name,Code,Return,Expected):-
   tst:is_local_test(Name,SExpression,Expected),
   as_sexp(SExpression,Expression),
   dbmsg(lisp_compiled_eval(Expression)),
   must_or_rtrace(writeExpression(Expression)),
   lisp_compile(Return,Expression,Code),
   
   term_attvars(Code,AttVars),maplist(del_attr_rev2(vn),AttVars).



simple(x) <<== x.


lisp_append_2(l1, l2) <<==
	cond(  [[null(l1), l2], 
		[t,	cons( first(l1),
			      lisp_append_2(rest(l1),
			                    l2))]]).
 

lisp_error(x) <<== setq(y, 5).

%:- rtrace.
lisp_let() <<==
	let([bind(x, 3), bind(y, 5)], 
		      progn(x,y)).

lisp_let1() <<==
	let([bind(x, 3), bind(y, 5)], 
			x, 		% implicit progn here
			y).


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
	let([bind(fred, function(lambda([num], times(scale, num))))], mapcar(fred, xs)).


make_summer(total) <<== 
	function(lambda([n],
		setq(total, plus(total, n)))).


sum_with_map(xs) <<==
	let([bind(running_total, 0)],
		let([bind(summer, function(lambda([n], setq(running_total, 
							plus(running_total, n)))))],
		 mapcar(summer, xs),
		  running_total )).


