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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(typcheck, []).
:- set_module(class(library)).
:- include('header').

do_correctly(P,AT):- compound(P),functor(P,F,A),functor(AT,F,A),wl:type_checked(AT).


%correctly(P,P):- is_self_evaluating_object(P),!.
correctly(P,AT):- P=..[_|L],make_correctly(P,AT,1,L,Agenda),!,call(Agenda).

make_correctly(P,_,_,_,P):- \+ compound(P).
make_correctly(P,AT,N,[R],Out):- !, 
 (arg(N,AT,Type) ->
  (setarg(N,P,Ret),Out=(P,coerce_to(Ret,Type,R)));
  (Out=P)).
make_correctly(P,AT,From,[H|T],TTT):- arg(From,AT,Type), setarg(From,P,NewArg), From2 is From+1,
    Coerce = coerce_to(H,Type,NewArg),
   (nonvar(H) -> (Coerce,TTT=TT); (TTT= (Coerce,TT))),
   make_correctly(P,AT,From2,T,TT).
/*
make_correctly(P,AT,F,From,[H|T],TTT):- From2 is From+1,
   (nonvar(H) -> (setarg(P,From,NewArg),coerce_to(H,From,NewArg), TTT= TT); (TTT= (coerce_to(F,From,H),TT))),
   make_correctly(P,AT,F,From2,T,TT).
*/
:- multifile(wl:coercion/3).
coerce_to(In, Type, Out):- wl:coercion(In, Type, Out),!.
coerce_to(H,_,H).


compound_starts_with(More,_):- \+ compound(More),!,fail.
compound_starts_with(A,A):-!.
compound_starts_with((A,_),B):- compound(A),A=B.

skip_type_checks(asserta(_)).
skip_type_checks(assert_if_new(_)).
skip_type_checks(asserta_tracked(_,_)).
skip_type_checks(assertz(_)).
skip_type_checks(assert(_)).

add_type_checks(_Ctx,Some,Some):- \+ compound(Some),!.
add_type_checks(_Ctx,Some,Some):- skip_type_checks(Some),!.
add_type_checks(Ctx,(A:B),(A:BB)):-!, add_type_checks(Ctx,B,BB).
add_type_checks(_,(P,More),Agenda):- \+ compound_starts_with(More,coerce_to(_,_,_)),wl:do_correctly(P,AT),P=..[_|L],make_correctly(P,AT,1,L,Agenda),!.
add_type_checks(_,P,Agenda):- wl:do_correctly(P,AT),P=..[_|L],make_correctly(P,AT,1,L,Agenda),!.
%add_type_checks(_,P,correctly(P,AT)):- wl:do_correctly(P,AT).
add_type_checks(_Ctx,Some,Some):- is_self_evaluating_object(Some),!.

add_type_checks(Ctx,(A,B),(AA,BB)):-!, add_type_checks(Ctx,A,AA),add_type_checks(Ctx,B,BB).

add_type_checks(Ctx,[S|Some],[SR|SomeR]):- add_type_checks(Ctx,S,SR),add_type_checks(Ctx,Some,SomeR).

add_type_checks(_Ctx,C1,C2):- 
  compound_name_arguments(C1,F,C1O),
  must_maplist(add_type_checks(F),C1O,C2O),C2=..[F|C2O].
add_type_checks(_Ctx,Some,Some).



:- fixup_exports.


