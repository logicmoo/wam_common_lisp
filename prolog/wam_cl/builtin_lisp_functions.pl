/*******************************************************************
 *
 * A Lisp compiler, written in Prolog
 *
 * (builtin_lisp_functions.pl)
 *
 * (c) Neil Smith, 2001
 *
 * This program provides some built-in functionality for the 
 * Lisp compiler.  It requires that the file lisp_compiler.pl has 
 * already been successfully compiled.
 *
 *******************************************************************/

:- multifile(special_var/2).
:- dynamic(special_var/2).

%module(_,_).

%:- ensure_loaded((writef)).
%:- ensure_loaded(library(lists)).

first(List, Result):- List==[]->Result=[];
	once( (	List = [Result|_]
	    ;	error(first_not_cons, ErrNo, _),
		throw(ErrNo)	)).


rest(List, Result):- List==[]->Result=[];
	once( (	List = [_|Result]
	    ;	error(rest_not_cons, ErrNo, _),
		throw(ErrNo)	)).

cdr(List, Result):- rest(List, Result).
car(List, Result):- first(List, Result).


cons(Item, List, Result):-
	Result = [Item|List].

null(Item, Result):-
		Item = []
	->	Result = t
	;	Result = [].

eq(Item1, Item2, Result):-
		Item1 == Item2
	->	Result = t
	;	Result = [].

equalp(Item1, Item2, Result):-
		Item1 = Item2
	->	Result = t
	;	Result = [].


plus(Num1, Num2, Result):-
	Result is Num1 + Num2.
minus(Num1, Num2, Result):-
	Result is Num1 - Num2.
times(Num1, Num2, Result):-
	Result is Num1 * Num2.
divide(Num1, Num2, Result):-
	Result is Num1 / Num2.


lisp_not(Boolean, Result):-
		Boolean = []
	->	Result = t
	;	Result = [].

or(Bool1, Bool2, Result):-
		once( (Bool1 \= [] ; Bool2 \= []))
	->	Result = t
	;	Result = [].

and(Bool1, Bool2, Result):-
		(Bool1 \= [] , Bool2 \= [])
	->	Result = t
	;	Result = [].


lisp_apply(FunctionObject, Arguments, Result):-
		FunctionObject = closure(FormalArgs, Body, Environment)
	->	zip_with(FormalArgs, Arguments, [Arg, Val, bv(Arg, [Val|_])]^true, Bindings),
		apply(Body, [[Bindings|Environment], Result])
	;	FunctionObject = function(FunctionName), 
		append(Arguments, [Result], ArgumentsResult),
		Function =.. [FunctionName|ArgumentsResult],
		call(Function).


extract_variable_value([Val|Vals], FoundVal, Hole):-
		var(Vals)
	->	FoundVal = Val,
		Hole = Vals
	;	extract_variable_value(Vals, FoundVal, Hole).

% set_variable_value([Val|Vals], LetValue, Hole):- extract_variable_value([Val|Vals], FoundVal, Hole).



lisp_call(Function, Result):-
	apply(Function, [Result]).



show_special:-
		setof(sv(Var, Value), special_var(Var, Value), SVs)
	->	writef('Variable \tValue\n\n'),
		every(SVs, [sv(Var2, Value2)]^(writef('%t :\t%t\n',[Var2, Value2])))
	;	writef('No special variables\n').



