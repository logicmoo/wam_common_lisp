/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (builtin_lisp_functions.pl)
 *
 * (c) Neil Smith, 2001
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * This program provides some built-in functionality for the 
 * Lisp compiler.  It requires that the file lisp_compiler.pl has 
 * already been successfully compiled.
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(prims,[]).

:- set_module(class(library)).

:- include('header.pro').

prims:cl_exact.

%module(_,_).

:- ensure_loaded((utils_writef)).
:- ensure_loaded(library(lists)).

:- use_module(library('dialect/sicstus/arrays')).
% :- use_module(library('dialect/sicstus')).

% Numbers, pathnames, and arrays are examples of self-evaluating objects.
is_self_evaluationing_object(X):- var(X),!.
is_self_evaluationing_object(X):- atomic(X),!,is_self_evaluationing_const(X).
is_self_evaluationing_object('$OBJ'(_,_)):-!.
is_self_evaluationing_object('$CHAR'(_)):-!.

is_self_evaluationing_object(X):- (is_dict(X);is_array(X);is_rbtree(X)),!.

is_self_evaluationing_const(X):- atomic(X),is_self_evaluationing_const0(X),!.
is_self_evaluationing_const0(X):- (X==t;X==[];number(X);is_keywordp(X);string(X);(blob(X,T),T\==text)),!.
is_self_evaluationing_const0(X):- is_functionp(X).

is_functionp(X):- \+ atom(X),!,fail.
is_functionp(X):- atom_concat_or_rtrace('f_',_,X),!.
is_functionp(X):- atom_concat_or_rtrace('cl_',_,X),!.

%:- dynamic(op_replacement/2).
user:op_replacement(first,cl_car).
cl_car(List, Result):- 
  (List = [Result|_] -> true;
  (List==[] -> Result=[];
  (	error(first_not_cons, ErrNo, _),
		throw(ErrNo)))).

user:op_replacement(rest,cl_cdr).
cl_cdr(List, Result):- List==[]->Result=[];
	once( (	List = [_|Result]
	    ;	error(rest_not_cons, ErrNo, _),
		throw(ErrNo)	)).


user:op_replacement(setcar,cl_rplaca).
cl_rplaca(Cons,Obj,Cons):- nb_setarg(1,Cons,Obj).

user:op_replacement(setcdr,cl_rplacd).
cl_rplacd(Cons,Obj,Cons):- nb_setarg(2,Cons,Obj).

cl_cons(Item,
 List, Result):-
	Result = [Item|List].

cl_append(A,B,R):- append(A,B,R),!.

cl_list(List,List).

cl_plus(Num1, Num2, Result):-Result is Num1 + Num2.

cl_minus(Num1, Num2, Result):-
	Result is Num1 - Num2.
cl_times(Num1, Num2, Result):-
	Result is Num1 * Num2.
cl_divide(Num1, Num2, Result):-
	Result is Num1 / Num2.


cl_lisp_not(Boolean, Result):-
		Boolean = []
	->	Result = t
	;	Result = [].


/*
Wrongness
cl_or(Bool1, Bool2, Result):-
		once( (Bool1 \= [] ; Bool2 \= []))
	->	Result = t
	;	Result = [].

cl_and(Bool1, Bool2, Result):-
		(Bool1 \= [] , Bool2 \= [])
	->	Result = t
	;	Result = [].

*/

lisp_apply(FunctionObject, Arguments, Result):-
		FunctionObject = [closure,FormalArgs, Body, Environment]
	->	zip_with(FormalArgs, Arguments, [Arg, Val, bv(Arg,Val)]^true, Bindings),
		apply(Body, [[Bindings|Environment], Result])
	;	FunctionObject = [function,FunctionName], 
		append(Arguments, [Result], ArgumentsResult),
		Function =.. [FunctionName|ArgumentsResult],
		call(Function).


lisp_call(Function, Result):-
	apply(Function, [Result]).




t_or_nil(G,Ret):- G->Ret=t;Ret=[].

cl_not(Obj,Ret):- t_or_nil(Obj == [] , Ret).
cl_null(Obj,Ret):- t_or_nil(Obj == [] , Ret).

=(N1,N2,Ret):- t_or_nil( (N1=N2),Ret). 
cl_eq(A,B,Ret):- t_or_nil( is_eq(A,B) , Ret).
cl_eql(A,B,Ret):- t_or_nil( is_eql(A,B) , Ret).
cl_equal(A,B,Ret):- t_or_nil( is_equal(A,B) , Ret).
cl_equalp(A,B,Ret):- t_or_nil( is_equalp(A,B) , Ret).

is_eql(X,Y):- X=:=Y.
is_eq(X,Y):- X==Y.
is_equal(X,Y):- X=@=Y.
is_equalp(X,Y):- f_u_to_pvs(X,XX),f_u_to_pvs(Y,YY),XX=@=YY.

f_u_to_pvs(X,[float|XX]):- notrace(catch(XX is (1.0 * X),_,fail)),!.
f_u_to_pvs(X,XX):- findall([P|V],(get_opv(X,P,V);get_struct_opv(X,P,V)),List),List\==[],sort(List,XX).
f_u_to_pvs(X,[str|XX]):-format(string(S),'~w',[X]),string_upper(S,XX).



cl_sqrt(X,Y):- \+ integer(X)-> Y is sqrt(X);
   (Y is sqrt(X)).

f_u_c43(N1,N2,Ret):- Ret is (N1 + N2).
+(N1,N2,Ret):- Ret is (N1 + N2).

f_u_c45(N1,N2,Ret):- Ret is (N1 + N2).
cl_c45(N1,N2,Ret):- Ret is (N1 - N2).
-(N1,N2,Ret):- Ret is (N1 - N2).
f_u_(N1,N2,Ret):- Ret is (N1 - N2).


f_u_c42(N1,N2,Ret):- Ret is (N1 + N2).
*(N1,N2,Ret):- Ret is (N1 * N2).

f_u_c47(N1,N2,Ret):- Ret is (N1 + N2).
'/'(N1,N2,Ret):- Ret is (N1 / N2).

/*
op_replacement(+,plus).
op_replacement(-,minus).
op_replacement(*,mult).
op_replacement(<,lessThan).
op_replacement(>,greaterThan).
*/

<(N1,N2,Ret):- t_or_nil(<(N1,N2),Ret). 
>(N1,N2,Ret):- t_or_nil(>(N1,N2),Ret). 

'1+'(N,Ret):- Ret is N + 1.
'1-'(N,Ret):- Ret is N - 1.


% =(A, B, R):- A \= B-> R=[] ; R=t.

is_special_var_c(_,_):-!,fail.
sym_arg_val_envc(N,A,B,_) :- is_special_var_c(N,B) -> true ; A = B.




show_special:-
		setof(Package:Var=Type:Value, symp:symbol_info(Var, Package, Type, Value), SVs)
	->	writef('Variable \tValue\n\n'),
		every(SVs, [(Var2 = Value2)]^(writef('%t :\t%t\n',[Var2, Value2])))
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
fibp(1, 1) :- !.
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
        ->  sym_arg_val_envc(n, A, Obj, B),
            -(Obj, 1, F),
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

fibp3(N, F) :-
        N =< 1 
        -> F = 1 
        ;
        N1 is N-1,
        N2 is N-2,
        fibp3(N1, F1),
        fibp3(N2, F2),
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
% ?- timel(fibp3(38,O)).
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


sym_arg_val_envd(Var,_InValue,Value,Environment):- 
  (once((	(member(Bindings, Environment),
			member(bv(Var, Value0), Bindings),
			extract_variable_value(Value0, Value, _))
		    ;	symbol_value(Var, Value)
		    ;	(lisp_error_description(unbound_atom, ErrNo, _),throw(ErrNo, Var))))).

fibd(A, K) :- !,
        B=[[bv(n, [A|_])]],
        sym_arg_val_envd(n, A, C, B),
        >(C, 1, D),
        (   D\=[]
        ->  sym_arg_val_envd(n, A, Obj, B),
            -(Obj, 1, F),
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


make_accessor(cadr).

make_accessor(cdar).

make_accessor(cddr).

make_accessor(caaar).

make_accessor(caadr).

make_accessor(cadar).

make_accessor(caddr).

make_accessor(cdaar).

make_accessor(cdadr).

make_accessor(cddar).

make_accessor(cdddr).

make_accessor(caaaar).

make_accessor(caaadr).

make_accessor(caadar).

make_accessor(caaddr).

make_accessor(cadaar).

make_accessor(cadadr).

make_accessor(caddar).

make_accessor(cadddr).

make_accessor(cdaaar).

make_accessor(cdaadr).

make_accessor(cdadar).

make_accessor(cdaddr).

make_accessor(cddaar).

make_accessor(cddadr).

make_accessor(cdddar).

make_accessor(cddddr).

/*
(caar x)        (car (car x))                    
(cadr x)        (car (cdr x))                    
(cdar x)        (cdr (car x))                    
(cddr x)        (cdr (cdr x))                    
(caaar x)       (car (car (car x)))              
(caadr x)       (car (car (cdr x)))              
(cadar x)       (car (cdr (car x)))              
(caddr x)       (car (cdr (cdr x)))              
(cdaar x)       (cdr (car (car x)))              
(cdadr x)       (cdr (car (cdr x)))              
(cddar x)       (cdr (cdr (car x)))              
(cdddr x)       (cdr (cdr (cdr x)))              
(caaaar x)      (car (car (car (car x))))        
(caaadr x)      (car (car (car (cdr x))))        
(caadar x)      (car (car (cdr (car x))))        
(caaddr x)      (car (car (cdr (cdr x))))        
(cadaar x)      (car (cdr (car (car x))))        
(cadadr x)      (car (cdr (car (cdr x))))        
(caddar x)      (car (cdr (cdr (car x))))        
(cadddr x)      (car (cdr (cdr (cdr x))))        
(cdaaar x)      (cdr (car (car (car x))))        
(cdaadr x)      (cdr (car (car (cdr x))))        
(cdadar x)      (cdr (car (cdr (car x))))        
(cdaddr x)      (cdr (car (cdr (cdr x))))        
(cddaar x)      (cdr (cdr (car (car x))))        
(cddadr x)      (cdr (cdr (car (cdr x))))        
(cdddar x)      (cdr (cdr (cdr (car x))))        
(cddddr x)      (cdr (cdr (cdr (cdr x))))  


symbol_info(Sym,P,function,O),symbol_info(Sym,P,function_type,FT),symbol_info(Sym,P,name,Name),
  format('~N% ~w (~w ~w)~n~q(A,Result):- ...\n\n',[Name,FT,P, O]),nl,fail.

*/      



:- fixup_exports.

