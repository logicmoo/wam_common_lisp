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


cl_sqrt(X,Y):- \+ integer(X)-> (Y is sqrt(X)) ; (IY is sqrt(X), RY is floor(IY),(RY=:=IY -> Y=RY ; Y=IY)).

%cl_floor(X,Y):- Y is floor(X).
%cl_log(X,Y):- Y is log(X).

define_cl_math(_,0).
% define_cl_math(F,0):- atom_concat('cl_',F,CLN), P=..[CLN,X],FP=..[F], assertz(P:- X is FP).
define_cl_math(F,1):- atom_concat('cl_',F,CLN), P=..[CLN,X,R],FP=..[F,X], show_call_trace(user:assertz(P:- R is FP)).
define_cl_math(F,2):- atom_concat('cl_',F,CLN), P=..[CLN,X,Y,R],FP=..[F,X,Y], show_call_trace(user:assertz(P:- R is FP)).

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


