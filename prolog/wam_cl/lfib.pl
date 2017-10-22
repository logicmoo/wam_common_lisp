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

:- ensure_loaded((writef)).
:- ensure_loaded(library(lists)).

first(List, Result):- List==[]->Result=[];
	once( (	List = [Result|_]
	    ;	error(first_not_cons, ErrNo, _),
		throw(ErrNo)	)).

rest(List, Result):- List==[]->Result=[];
	once( (	List = [_|Result]
	    ;	error(rest_not_cons, ErrNo, _),
		throw(ErrNo)	)).

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


% plus(Num1, Num2, Result):-Result is Num1 + Num2.

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


lisp_call(Function, Result):-
	apply(Function, [Result]).



show_special:-
		setof(sv(Var, Value), special_var(Var, Value), SVs)
	->	writef('Variable \tValue\n\n'),
		every(SVs, [sv(Var2, Value2)]^(writef('%t :\t%t\n',[Var2, Value2])))
	;	writef('No special variables\n').


:- use_module(library(tabling)).
:- table fibt/2.
fibt(0, 1) :- !.
fibt(1, 1) :- !.
fibt(N, F) :-
        N > 1,
        N1 is N-1,
        N2 is N-2,
        fibt(N1, F1),
        fibt(N2, F2),
        F is F1+F2.

fibp(0, 1) :- !.
fibp(1, 1) :- .
fibp(N, F) :-
        N > 1,
        N1 is N-1,
        N2 is N-2,
        fibp(N1, F1),
        fibp(N2, F2),
        F is F1+F2.
% SBCL 
% * (time (fib 38))
% 1.264000 seconds of total run time (1.264000 user, 0.000000 system)
% YAP
% ?- time(fibp(38,O)).
% 4.924 CPU in 4.953 seconds ( 99% CPU)
% SWI
% ?- timel(fibp(38,O)).
% 252,983,942 inferences, 19.712 CPU in 19.949 seconds (99% CPU, 12833899 Lips)
% CLISP
% (time (fib 38))
% Run time: 53.0 sec.
% BProlog
% ?- time(fibp(38,O)).
% CPU time 75.764 seconds.

fibp2(N, F) :-
        N =< 1 
        -> F = 1 
        ;
        N1 is N-1,
        N2 is N-2,
        fibp2(N1, F1),
        fibp2(N2, F2),
        F is F1+F2.
% SBCL 
% * (time (fib 38))
% 1.264000 seconds of total run time (1.264000 user, 0.000000 system)
% YAP
% ?- time(fibp2(38,O)).
% 3.124 CPU in 3.148 seconds ( 99% CPU)
% SWI
% ?- timel(fibp2(38,O)).
% 442,721,899 inferences, 24.558 CPU in 24.826 seconds (99% CPU, 18027611 Lips)
% CLISP
% (time (fib 38))
% 53.0 sec.




-(A, B, R):- R is A - B.
-(A, R):- R is -A.
+(A, B, R):- R is A + B.
*(A, B, R):- R is A * B.
'/'(A, B, R):- R is A / B.

>(A, B, R):- A > B-> R=t ; R=[].
<(A, B, R):- A < B-> R=t ; R=[].
=(A, B, R):- A \= B-> R=[] ; R=t.

is_special_var_c(_,_):-!,fail.
sym_arg_val_envc(N,A,B,_) :- is_special_var_c(N,B) -> true ; A = B.


/*
 
NOTES:
 
* WAM-CL currently produces code 6 times slower than the handwritten code
 
* Handwritten Prolog is 2-3 slower than SBCL
 
* If WAM-CL becomes fast as handwritten code,
** it will be 17 times faster than CLISP
** it will be 6 times faster than ECL
 
 
 
 
(defun fib (n)
  (if (> n 1)
    (+ (fib (- n 1))
       (fib (- n 2)))
    1))
 
*/

% WAM-CL 
fibc(A, K) :- !,
        B=[[bv(n, [A|_])]],
        sym_arg_val_envc(n, A, C, B),
        >(C, 1, D),
        (   D\=[]
        ->  sym_arg_val_envc(n, A, E, B),
            -(E, 1, F),
            fibc(F, I),
            sym_arg_val_envc(n, A, G, B),
            -(G, 2, H),
            fibc(H, J),
            +(I, J, L),
            K=L
        ;   K=1
        ).
fibc(_, _) :- '<<=='(fibc(n),if(n>1, fibc(n-1)+fibc(n-2), 1)).


% HANDWRITTEN

fibp2(N, F) :-
        N =< 1 
        -> F = 1 
        ;
        N1 is N-1,
        N2 is N-2,
        fibp2(N1, F1),
        fibp2(N2, F2),
        F is F1+F2.





% SBCL 1.3.1
% * (time (fib 38))
% 1.264000 seconds of total run time (1.264000 user, 0.000000 system)

% YAP-Prolog (Hand written)
% ?- time(fibp2(38,O)).
% 3.124 CPU in 3.148 seconds ( 99% CPU)

% YAP-Lisp (WAM-CL)
% ?- time(fibc(38,O)).
% 20.184 CPU in 20.340 seconds ( 99% CPU)

% SWI-Prolog (Hand written)
% ?- timel(fibp2(38,O)).
% 24.558 CPU in 24.826 seconds (99% CPU, 18027611 Lips)

% ECL 15.3.7
% > (time (fib 38))
% run time  : 25.516 secs (real time : 26.290 secs)

% CLISP 2.49
% (time (fib 38))
% 53.0 sec.

% SWI-Lisp (WAM-CL)
% ?- time(fibc(38,O)).
% 113.043 CPU in 114.324 seconds (99% CPU, 15665558 Lips)


sym_arg_val_envd(Atom,_InValue,Value,Environment):- 
  (once((	(member(Bindings, Environment),
			member(bv(Atom, Value0), Bindings),
			extract_variable_value(Value0, Value, _))
		    ;	special_var(Atom, Value)
		    ;	(lisp_error_description(unbound_atom, ErrNo, _),throw(ErrNo, Atom))))).

fibd(A, K) :- !,
        B=[[bv(n, [A|_])]],
        sym_arg_val_envd(n, A, C, B),
        >(C, 1, D),
        (   D\=[]
        ->  sym_arg_val_envd(n, A, E, B),
            -(E, 1, F),
            fibd(F, I),
            sym_arg_val_envd(n, A, G, B),
            -(G, 2, H),
            fibd(H, J),
            +(I, J, L),
            K=L
        ;   K=1
        ).
fibd(_, _) :- '<<=='(fibd(n),if(n>1, fibd(n-1)+fibd(n-2), 1)).
% YAP
% ?- time(fibd(38,O)).
% 41.608 CPU in 42.418 seconds ( 98% CPU)
        

