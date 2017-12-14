/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * This program provides some built-in functionality for the 
 * Lisp compiler.  It requires that the file lisp_compiler.pl has 
 * already been successfully compiled.
 *
 * Definitions in this file are given in the Lisp-like syntax 
 * read by this compiler.
*  ..............................
 *
 *
 * Neil''s Notes:
 *
 * (c) Neil Smith, 2001
 *
 * This program, and its associated support files, forms a compiler
 * for a subset of the language LISP.  It supports a few simple
 * built-in procedures, listed below.  It also supports both special
 * and lexical variables, and higher-order functions and lexical
 * closures.
 *
 * This compiler was written in LPA Prolog v3.6 under MS Windows.
 * It should run under other Prologs without too much conversion needed,
 * but note the required library modules.
 *
 *
 * Special forms
 *
 * [] and nil are treated as special forms, evaluating to [], and treated as 'false'
 * t is a special form, evaluating to t, and treated as 'true'
 * if, cond
 * progn (and implicit progn in defun and let bodies)
 * quote
 * let
 * setq
 * function
 * lambda
 * defvar, defparameter (both with and without initial values)
 *
 * Built-in procedures (defined in builtin_lisp_functions.pl)
 *
 * cons, first, rest, null
 * eq, equalp
 * plus, minus, times, divide
 * lisp_not, or, and
 * cl_apply
 *
 * Other procedures are defined in lisp_library.pl
 *
 *******************************************************************/


:- op(1200, xfx, <<== ).	% function definition
:- op(1200,  fx, <<== ).	% functional imperative definition

% :- ensure_loaded(builtin_lisp_functions). % Lisp primitives: this directives is at the end of the file
% :- ensure_loaded(lisp_library).	% Functions defined in lisp: this directive is at the end of the file
					% allowing them to be compiled correctly


% The hook into the compiler

lisp_compiler_term_expansion( (FunctionHeadP <<== FunctionBodyP),PrologCode):-                 
        must_det_l((expand_pterm_to_sterm(FunctionHeadP,FunctionHead),
        expand_pterm_to_sterm(FunctionBodyP,FunctionBody),        
        FunctionHead=[Name|FormalParams],
        lisp_compile([defun,Head,FormalParams,FunctionBody],ResultCode),
        asserts_to_prolog_code(ResultCode,PrologCode).

asserts_to_prolog_code((A,B),PrologCode):-!,
        asserts_to_prolog_code(A,AA),
        asserts_to_prolog_code(B,BB),
        append(AA,BB,PrologCode).
asserts_to_prolog_code(:-asserta_tracked(_T,A),[A]).
asserts_to_prolog_code(:-assert(A),[A]).
asserts_to_prolog_code(:-asserta(A),[A]).
asserts_to_prolog_code(:-assert_if_new(A),[A]).
asserts_to_prolog_code(:-assertz(A),[A]).
asserts_to_prolog_code(:-A, AA):-!,asserts_to_prolog_code(A,AA).
asserts_to_prolog_code(A, [:-A]).

lisp_compiler_term_expansion( ( <<== FunctionBodyP), ( :-   (Code, writeExpression(Result)) ):-
        must_det_l((expand_pterm_to_sterm(FunctionBodyP,FunctionBody),
        lisp_compile(Result,FunctionBody,Body),
        body_cleanup_keep_debug_vars(Ctx,Body,Code))).


ssip_compiler_term_expansion(Symbol,lambda(Args,Body),[OOUT]):- atom(Symbol),is_list(Args),
  length(Args,A1),
  A is A1+1,
  cfunctor(P,Symbol,A),
 (predicate_property(P,defined)->gensym(Symbol,SymbolR);Symbol=SymbolR),
  Head=..[SymbolR|Args],
  subst(Body,Symbol,SymbolR,BodyM),
  OUT= ((Head <<== BodyM)),
  always(lisp_compiler_term_expansion(OUT,OOUT)),!.

ssip_compiler_term_expansion(Symbol,Symbol2,ssip_define(Symbol,Symbol2)):-!.

% The hook into the compiler
term_expansion(Symbol==Function,O) :- I= (Symbol==Function),ssip_compiler_term_expansion(Symbol,Function,O),nl,nl,
  flatten([I,O],L),
  maplist(dbmsg,L),!.
  % in_cmt(maplist(portray_clause,L)),!.
term_expansion(I,O) :- lisp_compiler_term_expansion(I,O),I\==O,nl,nl,
  flatten([I,O],L),
  maplist(dbmsg,L),!.


% Now Prolog can understand them, compile the additional library files



%% [tim.prolog]siptest.
%% Tim Finin, University of Pennsylvania, Mon Oct 27 10:39:27 1986
%% this file contains samples for SIP.

fact == lambda([n], if(=(n,0),1,n*fact(sub1(n)))).


add1 == lambda([n], n+1).

sub1 == lambda([n], n-1).

% higher order functions

mapcar ==
  lambda([fun,l],
         if(null(l),
            nil,
            cons(fun(car(l)),mapcar(fun,cdr(l))))).

% simple list manipulation functions.

length == lambda([l], if(null(l),0,add1(length(cdr(l))))).

append == lambda([l1,l2],if(null(l1),l2,cons(car(l1),append(cdr(l1),l2)))).


% stuff for streams.

filter ==
  lambda([fun,s],
         if('emptyStream?'(s),
            s,
            if(fun(head(s)),
               consStream(head(s),filter(fun,tail(s))),
               filter(fun,tail(s))))).

from(n) <<== consStream(n,from(n+1)).
% from == lambda([n],consStream(n,from(n+1))).

nthStream == lambda([s,n],if(n=1,head(s),nthStream(tail(s),n-1))).

integers == from(1).

% environments

makeCounter ==
  lambda([],
         begin(counter == 0,
               lambda([],setq(counter,1+counter)))).

caaaar == lambda([x],car(car(car(car(x))))).

caar == lambda([x],car(car(x))).

reverse ==
  lambda([l],
     if(null(l),
        l,
        append(reverse(cdr(l)),(cons(car(l),nil))))).


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
		cons( cl_apply(func, list_1(first(l))),
			mapcar(func, rest(l)))).


/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
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



:- fixup_exports.




