/*******************************************************************
 *
 * C1 Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(mth, []).
:- set_module(class(library)).
:- include('header.pro').


wl:type_checked(P):- current_predicate(_,mth:P), \+ predicate_property(mth:P,imported_from(_)),
   P=..[_|List],maplist( =(number),List).

wl:coercion(In, number, Out):- is_numberp(In),to_prolog_number(In,Out).

to_prolog_number('$NUMBER'(_,Y),Z):- !, to_prolog_number(Y,Z).
to_prolog_number('$RATIO'(X,Y),Z):- !, to_prolog_number(X,XX),to_prolog_number(Y,YY),Z is XX/YY.
to_prolog_number('$COMPLEX'(X,Y),Z):- !, to_prolog_number(Y,YY), 0 is YY,to_prolog_number(X,Z).
to_prolog_number('$EXP'(I,_,E),N):- !, notrace(catch(N is (I * 10^E),_,fail)),!.
to_prolog_number(X,Y):- Y is X,!.

is_numberp('$NUMBER'(_,_)).
is_numberp('$RATIO'(_,_)).
is_numberp('$COMPLEX'(_,_)).
is_numberp('$EXP'(_,_,_)).
is_numberp(P):- number(P).

cl_sqrt(X,Y):- \+ integer(X)-> (Y is sqrt(X)) ; (IY is sqrt(X), RY is floor(IY),(RY=:=IY -> Y=RY ; Y=IY)).

%cl_floor(X,Y):- Y is floor(X).
%cl_log(X,Y):- Y is log(X).

define_cl_math(_,0).
% define_cl_math(F,0):- atom_concat('cl_',F,CLN), P=..[CLN,X],FP=..[F], assertz(P:- X is FP).
% define_cl_math(F,1):- atom_concat('cl_',F,CLN), P=..[CLN,X,R],FP=..[F,X], show_call_trace(user:assertz(P:- R is FP)).
define_cl_math(F,2):- atom_concat('cl_',F,CLN), P=..[CLN,X,Y,R],FP=..[F,X,Y], show_call_trace(user:assertz(P:- R is FP)).
define_cl_math(_,_).

f_u_c43(N1,N2,Ret):- Ret is (N1 + N2).
+(N1,N2,Ret):- Ret is (N1 + N2).

f_u_c45(N1,N2,Ret):- Ret is (N1 + N2).
-(N1,N2,Ret):- Ret is (N1 - N2).

f_u_c42(N1,N2,Ret):- Ret is (N1 + N2).
*(N1,N2,Ret):- Ret is (N1 * N2).

f_u_c47(N1,N2,Ret):- Ret is (N1 + N2).
'/'(N1,N2,Ret):- Ret is (N1 / N2).

<(N1,N2,Ret):- t_or_nil(<(N1,N2),Ret). 
>(N1,N2,Ret):- t_or_nil(>(N1,N2),Ret). 

'1+'(N,Ret):- Ret is N + 1.
'1-'(N,Ret):- Ret is N - 1.

f_c61(N1,N2,Ret):- t_or_nil( (N1=:=N2),Ret). 
=(N1,N2,Ret):- t_or_nil( (N1=:=N2),Ret).

cl_plus(Num1, Num2, Result):-Result is Num1 + Num2.

cl_minus(Num1, Num2, Result):-
	Result is Num1 - Num2.
cl_times(Num1, Num2, Result):-
	Result is Num1 * Num2.
cl_divide(Num1, Num2, Result):-
	Result is Num1 / Num2.


:- fixup_exports.


