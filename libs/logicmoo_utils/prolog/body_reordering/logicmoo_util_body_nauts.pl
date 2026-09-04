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
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_body_nauts.pl
:- module(logicmoo_util_body_nauts,
          [ enable_nautifiers/0,
            do_nautifier/4,
            disable_nautifiers/0]).

:- style_check(+singleton).
:- style_check(+discontiguous).
% :- style_check(-atom).
% :- style_check(-naut).

/*
delete_eq([],Item,[]):-!,dmsg(warn(delete_eq([],Item,[]))).
delete_eq([L|List],Item,List):-Item==L,!.
delete_eq([L|List],Item,[L|ListO]):-delete_eq(List,Item,ListO),!.
% user:term_expansion(I,O):- current_predicate(logicmoo_bugger_loaded/0),not(t_l:into_form_code),e2c_term_expansion(I,O).
*/

:- ensure_loaded(logicmoo_util_body_file_scope).
:-meta_predicate(do_nautifier(+,+,+,-)).



do_nautifier(_,_ ,A,A):- ( \+ compound(A) ; is_list(A)),!.
do_nautifier(Head,Vars ,(DECL,BodI),BodO):- compound(DECL),DECL=cycnaut(StrVar), !,
   do_nautifier(Head,Vars,nautArg(StrVar,BodI),BodO). 
do_nautifier(_,_ ,call(H),call(H)):-!.
do_nautifier(Head,Vars,nautArg(StrVar,BodI),nautArgUC(StrVar,NewVar,OOO)):- !, vsubst(BodI,StrVar,NewVar,BodO),
   do_nautifier(Head,Vars,BodO,OOO).
do_nautifier(Head,[StrVar|Vars],BodI,BodO):- !, do_nautifier(Head,Vars,nautArg(StrVar,BodI),BodO). 
do_nautifier(Head,Vars,V^(H),V^(HO)):- !,do_nautifier(Head,Vars,H,HO).

:- enable_body_reorder.

do_nautifier(Head,Vars,BodI,BodO):- compound(BodI),functor(BodI,F, _),
   reorderBody(cyckb_t_e2c(argIsa,F,N,NAUTType),fail,ttNAUTType(NAUTType)),arg(N,BodI,StrVar),!,
   do_nautifier(Head,Vars,nautArg(StrVar,BodI),BodO). 
/*
do_nautifier(Head,Vars,(H,L),(HO,LO)):-!, do_nautifier(Head,Vars,H,HO),do_nautifier(Head,Vars,L,LO).
do_nautifier(Head,Vars,(C->H;L),(CO->HO;LO)):-!, do_nautifier(Head,Vars,C,CO),do_nautifier(Head,Vars,H,HO),do_nautifier(Head,Vars,L,LO).
do_nautifier(Head,Vars,(H->L),(HO->LO)):-!, do_nautifier(Head,Vars,H,HO),do_nautifier(Head,Vars,L,LO).
do_nautifier(Head,Vars,(H;L),(HO;LO)):-!, do_nautifier(Head,Vars,H,HO),do_nautifier(Head,Vars,L,LO).
do_nautifier(Head,Vars,'{}'(H),'{}'(HO)):- !,do_nautifier(Head,Vars,H,HO).
*/
do_nautifier(Head,Vars,H,HO):- H=..HL, must_maplist(do_nautifier(Head,Vars),HL,HOL),HO=..HOL.


ttNAUTType('CharacterNAUT').
ttNAUTType('SubLNAUT').
ttNAUTType('SubLListOfNAUTs').
% ttNAUTType(['ListOfTypeFn', X]):-atom(X),ttNAUTType(X).


nautArg(User,CallWithUser):- dtrace,vsubst(no_repeats(CallWithUser),User,Cyc,CallWithCyc),!, (ground(User) -> (ssz(User,Cyc),CallWithCyc)  ; (CallWithCyc, ssz(User,Cyc))).


nautArgUC(User,Cyc,CallWithCyc):- must_det(var(Cyc)),!,nautArgUC2(User,Cyc,CallWithCyc).
nautArgUC2(User,Cyc,CallWithCyc):- var(User),!,CallWithCyc,cycNAUTToNAUT(Cyc,User).
nautArgUC2([User,U2|MORE],Cyc,CallWithCyc):- Cyc=[User,U2|MORE],!,CallWithCyc.
nautArgUC2([User],Cyc,CallWithCyc):- Cyc=User,!,CallWithCyc,atom(Cyc).

cycNAUTToNAUT(Cyc,User):- (atom(Cyc)->User=[Cyc];User=Cyc),!.
      


:- set_prolog_flag(do_nautifier,false).

enable_nautifiers:- enable_in_file(do_nautifier).
disable_nautifiers:- disable_in_file(do_nautifier).


:- if(true).
% some tests

:- endif.
