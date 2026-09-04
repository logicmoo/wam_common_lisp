/* Part of LogicMOO Base Logicmoo Path Setups
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_body_textstr.pl
:- module(logicmoo_util_body_textstr,
          [ enable_body_textstr/0,
            do_body_textstr/4,
            disable_body_textstr/0]).

:- style_check(+singleton).
:- style_check(+discontiguous).
% :- style_check(-atom).
% :- style_check(-string).

/*
delete_eq([],Item,[]):-!,dmsg(warn(delete_eq([],Item,[]))).
delete_eq([L|List],Item,List):-Item==L,!.
delete_eq([L|List],Item,[L|ListO]):-delete_eq(List,Item,ListO),!.
% user:term_expansion(I,O):- current_predicate(logicmoo_bugger_loaded/0),not(t_l:into_form_code),e2c_term_expansion(I,O).
*/

% :- ensure_loaded(logicmoo_util_body_file_scope).
:-meta_predicate(do_body_textstr(+,+,+,-)).



do_body_textstr(_,_ ,A,A):- ( \+ compound(A) ; is_list(A)),!.
do_body_textstr(Head,Vars ,(DECL,BodI),BodO):- compound(DECL),DECL=cycstring(StrVar), !,
   do_body_textstr(Head,Vars,stringArg(StrVar,BodI),BodO). 
do_body_textstr(_,_ ,call(H),call(H)):-!.
do_body_textstr(Head,Vars,stringArg(StrVar,BodI),stringArgUC(StrVar,NewVar,OOO)):- !, vsubst(BodI,StrVar,NewVar,BodO),
   do_body_textstr(Head,Vars,BodO,OOO).
do_body_textstr(Head,[StrVar|Vars],BodI,BodO):- !, do_body_textstr(Head,Vars,stringArg(StrVar,BodI),BodO). 
do_body_textstr(Head,Vars,(A;B),(AA;BB)):- !,do_body_textstr(Head,Vars,A,AA),do_body_textstr(Head,Vars,B,BB).
do_body_textstr(Head,Vars,V^(H),V^(HO)):- !,do_body_textstr(Head,Vars,H,HO).
do_body_textstr(Head,Vars,(A,B),(AA,BB)):- !,do_body_textstr(Head,Vars,A,AA),do_body_textstr(Head,Vars,B,BB).
do_body_textstr(Head,Vars,BodI,BodO):- compound(BodI),
   \+ predicate_property(BodI,static),
   functor(BodI,F, _),
   \+ arg(_,v(!,var,call_body_reorder_compare,call_body_reorder,words_append),F),
   ereq(isT(isa(F,'Predicate'))),
   ereq(t(argIsa,F,N,StringType)),ereq(ttStringType(StringType)),arg(N,BodI,StrVar),!,
   do_body_textstr(Head,Vars,stringArg(StrVar,BodI),BodO). 
do_body_textstr(Head,Vars,H,HO):- H=..HL, must_maplist(do_body_textstr(Head,Vars),HL,HOL),HO=..HOL.

stringArg(User,CallWithUser):- vsubst(no_repeats(CallWithUser),User,Cyc,CallWithCyc),!, 
 (ground(User) -> (ssz(User,Cyc),CallWithCyc)  ; (CallWithCyc, ssz(User,Cyc))).

ssz(User,Cyc):-cycStringToString(Cyc,User).

stringArgUCTest:-stringArgUCTest('"I"',_).
stringArgUCTest:-stringArgUCTest(['"I"','"am"'],_).
stringArgUCTest(User,Cyc):- stringArgUC(User,Cyc,dmsg(User->Cyc)),!.

stringArgUC(User,Cyc,CallWithCyc):- must_det(var(Cyc)),!,stringArgUC2(User,Cyc,CallWithCyc).

stringArgUC2(User,Cyc,CallWithCyc):- var(User),!,CallWithCyc,cycStringToString(Cyc,User).
stringArgUC2([User,U2|MORE],Cyc,CallWithCyc):- Cyc=[User,U2|MORE],!,CallWithCyc.
stringArgUC2([User],Cyc,CallWithCyc):- Cyc=User,!,CallWithCyc,atom(Cyc).
stringArgUC2(User,Cyc,CallWithCyc):- Cyc=User,!,CallWithCyc,atom(Cyc).

cycStringToString(Cyc,User):- (atom(Cyc)->User=[Cyc];User=Cyc),!.
      



enable_body_textstr:- enable_in_file(do_body_textstr).
disable_body_textstr:- disable_in_file(do_body_textstr).

:- disable_body_textstr.


:- if(true).
% some tests

:- endif.
