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
:- include('header').


grovel_math:-
  ignore((((((clause(arithmetic:eval(P,_,_),_),nonvar(P)),(functor(P,F,A),always(define_cl_math(F,A)))))),fail)).

wl:interned_eval(call(grovel_math)).

% define_cl_math(_,0).
% define_cl_math(F,0):- atom_concat_or_rtrace('cl_',F,CLN), P=..[CLN,X],FP=..[F], assertz(P:- X is FP).
% define_cl_math(F,1):- atom_concat_or_rtrace('cl_',F,CLN), P=..[CLN,X,R],FP=..[F,X], show_call_trace(user:assertz(P:- R is FP)).
define_cl_math(F,1):- atom_concat_or_rtrace('cl_',F,CLN), P=..[CLN,X,R],FP=..[F,X],
  (is_defined(CLN,2)-> true ; always(system:assertz(P:- R is FP))).
define_cl_math(F,2):- atom_concat_or_rtrace('cl_',F,CLN), P=..[CLN,X,Y,R],FP=..[F,X,Y],
  (is_defined(CLN,3)-> true ; always(system:assertz(P:- R is FP))).
define_cl_math(_,_).

wl:type_checked(P):- current_predicate(_,mth:P), \+ predicate_property(mth:P,imported_from(_)),
   P=..[_|List],maplist( =(number),List).

% Lisp COERCE
wl:coercion(In, number, Out):- is_numberp(In),to_prolog_number(In,Out).

to_prolog_number('$NUMBER'(_,Y),Z):- !, to_prolog_number(Y,Z).
to_prolog_number('$RATIO'(X,Y),Z):- !, to_prolog_number(X,XX),to_prolog_number(Y,YY),Z is XX/YY.
to_prolog_number('$COMPLEX'(X,Y),Z):- !, to_prolog_number(Y,YY), 0 is YY,to_prolog_number(X,Z).
to_prolog_number('$EXP'(I,_,E),N):- !, notrace(catch(N is (I * 10^E),_,fail)),!.
to_prolog_number(X,Y):- Y is X,!.

% Lisp Type Predicates

is_numberp('$NUMBER'(_,_)).
is_numberp('$RATIO'(_,_)).
is_numberp('$COMPLEX'(_,_)).
is_numberp('$EXP'(_,_,_)).
is_numberp(P):- number(P).

is_oddp(N):- 1 is N div 2.
is_evenp(N):- 0 is N div 2.

cl_oddp(N,R):- t_or_nil(is_oddp(N),R).
cl_evenp(N,R):- t_or_nil(is_evenp(N),R).

cl_minusp(N,R):- t_or_nil(N<0,R).
cl_plusp(N,R):- t_or_nil(N>0,R).
cl_zerop(N,R):- t_or_nil(N=:=0,R).


% Lisp Comparison Predicates

cl_c61(N1,N2,Ret):- t_or_nil( (N1=:=N2),Ret). 
'='(N1,N2,Ret):- t_or_nil( (N1=:=N2),Ret).

cl_c60_c61(N1,N2,Ret):- t_or_nil('=<'(N1,N2),Ret).
'<='(N1,N2,Ret):- t_or_nil('=<'(N1,N2),Ret).

cl_c62_c61(N1,N2,Ret):- t_or_nil('>='(N1,N2),Ret).
'>='(N1,N2,Ret):- t_or_nil('>='(N1,N2),Ret).

cl_c60(N1,N2,Ret):- t_or_nil(<(N1,N2),Ret). 
'<'(N1,N2,Ret):- t_or_nil(<(N1,N2),Ret). 

cl_c62(N1,N2,Ret):- t_or_nil(<(N1,N2),Ret). 
'>'(N1,N2,Ret):- t_or_nil(>(N1,N2),Ret). 

% Lisp Operators/Functions
cl_sqrt(X,Y):-
    X < 0 
       -> (NX is -X , cl_sqrt(NX,NY), Y = '$COMPLEX'(0, NY))
     ;
    (\+ integer(X)
      -> (Y is sqrt(X)) 
      ;
      (IY is sqrt(X), RY is floor(IY),(RY=:=IY -> Y=RY ; Y=IY))).

cl_exp(N,Ret):- Ret is exp(N).

cl_expt(N1,N2,Ret):- Ret is (N1 ^ N2).


%cl_floor(X,Y):- Y is floor(X).
%cl_log(X,Y):- Y is log(X).


'1+'(N,Ret):- Ret is N + 1.
'1-'(N,Ret):- Ret is N - 1.

cl_c43(N1,N2,Ret):- Ret is (N1 + N2).
'+'(N1,N2,Ret):- Ret is (N1 + N2).

cl_c45(N1,N2,Ret):- Ret is (N1 + N2).
'-'(N1,N2,Ret):- Ret is (N1 - N2).

cl_c42(N1,N2,Ret):- Ret is (N1 + N2).
'*'(N1,N2,Ret):- Ret is (N1 * N2).

cl_c47(N1,N2,Ret):- Ret is (N1 + N2).
'/'(N1,N2,Ret):- Ret is (N1 / N2).

cl_plus(Num1, Num2, Result):-
        Result is Num1 + Num2.
cl_minus(Num1, Num2, Result):-
	Result is Num1 - Num2.
cl_times(Num1, Num2, Result):-
	Result is Num1 * Num2.
cl_divide(Num1, Num2, Result):-
	Result is Num1 / Num2.


:- fixup_exports.


% tests

end_of_file.

 (exp 0) =>  1.0
 (exp 1) =>  2.718282
 (exp (log 5)) =>  5.0 
 (expt 2 8) =>  256
 (expt 4 .5) =>  2.0
 (expt #c(0 1) 2) =>  -1
 (expt #c(2 2) 3) =>  #C(-16 16)
 (expt #c(2 2) 4) =>  -64 

  (expt -8 1/3) => #C(1.0 1.7320508)


