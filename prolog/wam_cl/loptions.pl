/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * 8ball.pl 
 *
 * Douglas'' Notes:
 *
 * 8BALL is used to predict when failure and errors may occur
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module('0pts', []).
:- set_module(class(library)).

:- include('./header').

is_pl_atom_key(N):- wl:wam_cl_option_local(N,_).
is_pl_atom_key(N):- \+ atom(N),!,fail.
is_pl_atom_key(N):- current_prolog_flag(N,_).
is_pl_atom_key(N):- \+ is_symbolp(N),\+ atomic_list_concat([_,_|_],'-',N),downcase_atom(N,N).

to_pl_atom_key(N,K):- var(N),!,K=N. 
to_pl_atom_key(N,K):- is_pl_atom_key(N),!,N=K.
to_pl_atom_key(N,K):- to_prolog_string(N,S),!,downcase_atom(S,DC),atomic_list_concat(HC,'-',DC),!,atomic_list_concat(HC,'_',K).
to_pl_atom_key(N,N).

to_pl_atom_value(N,K):- var(N),!,K=N. 
to_pl_atom_value(N,K):- number(N),!,K=N. 
to_pl_atom_value(N,K):- current_prolog_flag(_,N),N=K.
to_pl_atom_value(N,K):- is_pl_atom_key(N),!,N=K.
to_pl_atom_value(kw_missing,kw_missing).
to_pl_atom_value(N,K):- to_prolog_string(N,S),!,downcase_atom(S,DC),atomic_list_concat(HC,'-',DC),!,atomic_list_concat(HC,'_',K).
to_pl_atom_value(N,N).

:- dynamic(wam_cl_option/2).
:- thread_local(wl:wam_cl_option_local/2).

f_sys_get_wam_cl_option(N,V):- to_pl_atom_key(N,K),to_pl_atom_value(V,VV),wam_cl_option(K,VV).

wam_cl_option(N,V):- V==true,!,wam_cl_option(N,t).
wam_cl_option(N,V):- nonvar(N), wl:wam_cl_option_local(N,VV),!,V=VV.
wam_cl_option(N,V):- var(N), wl:wam_cl_option_local(N,VV),V=VV.
wam_cl_option(speed,V):- !, (current_prolog_flag(runtime_speed,V)->true;V=1).
wam_cl_option(safety,V):- !, (current_prolog_flag(runtime_safety,V)->true;V=1).
wam_cl_option(debug,V):- !, (current_prolog_flag(runtime_debug,V)->true;V=1).
wam_cl_option(safe(_),t):- !, (wam_cl_option(safety,V),V>0).
wam_cl_option(_,TF):- wam_cl_option(safety,N),(N<1-> TF=t; TF=kw_missing).
%wam_cl_option(N,kw_missing).



set_wam_cl_option_h(speed,V):- number(V),set_prolog_flag(runtime_speed,V).
set_wam_cl_option_h(safety,V):- number(V),set_prolog_flag(runtime_safety,V).
set_wam_cl_option_h(debug,V):- number(V),set_prolog_flag(runtime_debug,V),fail.
set_wam_cl_option_h(Flag,V):- atom(Flag),current_prolog_flag(Flag,_),
   to_prolog_flag_value(V,TF),!,set_prolog_flag(Flag,TF).

to_prolog_flag_value([],false).
to_prolog_flag_value(t,true).
to_prolog_flag_value(O,O).


f_sys_set_wam_cl_option(N,V):- to_pl_atom_key(N,K),to_pl_atom_value(V,VV),set_wam_cl_option(K,VV).

set_wam_cl_option(N,V):- 
   assertion(nonvar(N)),assertion(nonvar(V)),
   ignore(set_wam_cl_option_h(N,V)),
   retractall(wl:wam_cl_option_local(N,_)),!,
   (V\==kw_missing->asserta(wl:wam_cl_option_local(N,V));true).

:- fixup_exports.

