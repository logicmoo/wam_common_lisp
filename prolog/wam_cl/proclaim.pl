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
:- module(evil, []).
:- set_module(class(library)).
:- include('header').

sf_declare(_,_).
sf_proclaim(Assert,t):- assert(is_proclaimed(Assert)).

f_sxhash(O,H):- term_hash(O,H).

:- f_intern("PSXHASH",pkg_sys,_).
f_sys_psxhash(O,H):- f_sys_to_pvs(O,HT),term_hash(HT,H).
%f_u_psxhash(O,H):-f_sys_psxhash(O,H).

/*
;;;     (PROCLAMATION function-name ([arg-type]*) return-type
;;;             &rest {:no-sp-change|:pure|:reader|:no-side-effects})
*/
wl:init_args(3,sys_proclamation).
wl:interned_eval("(sys:set-opv `sys:proclamation :compile-as :operator)").
sf_sys_proclamation(Name,ArgumentTypes,ResultType,List,NameO):- 
  store_meta_info(set_proclamation,Name,ArgumentTypes,ResultType,[List],NameO).
set_proclamation(O,P,V):- atom_concat(P,'_proclaimed',PP), set_opv(O,PP,V).

wl:init_args(3,deftype).
wl:interned_eval("(sys:set-opv `cl:deftype :compile-as :operator)").
sf_deftype(Name,Lambda,Doc,Test,NameO):- set_opv(Name,typedoc,Doc),sf_deftype(Name,Lambda,Test,NameO).
sf_deftype(Name,Lambda,Test,NameO):- store_meta_info(set_deftype,Name,Lambda,Test,[],NameO).
set_deftype(O,P,V):- atom_concat(P,'_deftype',PP), set_opv(O,PP,V).


sf_defsetf(Get,Set,[],Res):- trace,
   f_defsetf(Get,Set,Res),!.

wl:init_args(3,sys_defknown).
wl:interned_eval("(sys:set-opv `SYS:defknown :compile-as :operator)").
sf_sys_defknown(Name,ArgumentTypes,ResultType,List,NameO):- store_meta_info(set_defknown,Name,ArgumentTypes,ResultType,List,NameO).
set_defknown(O,P,V):- set_opv(O,P,V).
 
store_meta_info(_With,In,_ArgumentTypes,_ResultType,_List,[]):- In==[],!.
store_meta_info(With,[Name|Names],ArgumentTypes,ResultType,List,[NameO|NameOL]):-!,
   store_meta_info(With,Name,ArgumentTypes,ResultType,List,NameO),
   store_meta_info(With,Names,ArgumentTypes,ResultType,List,NameOL).
store_meta_info(With,Name,ArgumentTypes,ResultType,List,Name):-   
   call(With,Name,lambda_list,ArgumentTypes),
   call(With,Name,result_type,ResultType),
   ignore((append([ExtraInfo],PList,List),
     (List\==[]->call(With,Name,extra_info,ExtraInfo);true),
     plist_to_names_values(PList,PNames,PValue),
     maplist(call(With,Name),PNames,PValue))).


:- fixup_exports.


