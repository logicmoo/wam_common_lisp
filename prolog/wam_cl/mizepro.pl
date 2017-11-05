/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(mizepro, []).
:- set_module(class(library)).
:- include('header.pro').

body_cleanup(Body,Code):-
   term_attvars(Body,AttVars),
   maplist(del_attr_rev2(freeze),AttVars),
   mize_body(',',Body,Code).

body_cleanup_keep_debug_vars(Body,Code):-
   term_attvars(Body,AttVars),
   maplist(del_attr_rev2(freeze),AttVars),
   mize_body(',',Body,Code).


del_attr_rev2(Name,Var):- del_attr(Var,Name).

conjoin_0(A,B,A):- B==true,!.
conjoin_0(A,B,B):- A==true,!.
conjoin_0((A,AA),B,(A,AAB)):-!, conjoin(AA,B,AAB).
conjoin_0(A,B,(A,B)).

mize_body(_,A,A):- \+ compound(A),!.
mize_body(F,(A,B),AB):-!,mize_body(F,A,AA),mize_body(F,B,BB),conjoin_0(AA,BB,AB).
mize_body(_,(A -> B ; C),(AA -> BB ; CC)):-!,mize_body1(->,A,AA),mize_body1(';',B,BB),mize_body1(';',C,CC).
mize_body(_,(B ; C),( BB ; CC)):-!,mize_body1(';',B,BB),mize_body1(';',C,CC).
mize_body(F,A,C):- mize_body1(F,A,B),mize_body2(F,B,C),!.
mize_body(_F,A,B):- compound_name_arguments(A,F,AA),must_maplist(mize_body(F),AA,BB),B=..[F|BB].
mize_body(_,A,A):-!.

mize_body1(_,A,A):- var(A),del_attr(A,rwstate).
mize_body1(_,A,A):- \+ compound(A),!.
mize_body1(F,(A,B),AB):-!,mize_body1(F,A,AA),mize_body1(F,B,BB),conjoin_0(AA,BB,AB).
mize_body1(F,A,B):- is_list(A),must_maplist(mize_body1(F),A,B).
mize_body1(_,A,L=[R]):- A =@= (L=[R, []]).
mize_body1(_F,A,B):- compound_name_arguments(A,F,AA),must_maplist(mize_body1(F),AA,BB),B=..[F|BB].
mize_body1(_,A,A):-!.

mize_body2(_,A,A):- \+ compound(A),!.
%mize_body2(_,A=B,pass2(A=B)):- var(A),var(B),A=B,!.
mize_body2(_F,A,B):- compound_name_arguments(A,F,AA),must_maplist(mize_body2(F),AA,BB),B=..[F|BB].
mize_body2(_,A,A):-!.

:- fixup_exports.

