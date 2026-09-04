/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File 'with_thread_local.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'with_thread_local.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_with_assertions.pl
:- module(locally_each,
          [ locally/2,
            locally_each/2,
            locally_tl/2,
            locally_hide/2,
            locally_hide_each/2,
            local_override/2,
            w_o_c/1,
            w_o_c/2
          ]).

:- meta_predicate
        locally((:),(:)),
        locally_each((:),(:)),
        locally_hide((:),(:)),        
        locally_hide_each((:),(:)),
        wtl(+,*,0,3),
        wtl_how(3,0,0,0,0).        

/** <module> Utility LOGICMOO_REDO_LOCALLY
This module allows drastic changes to prolog data to happen very temporarily. (to be reset or temporarily changed.)
@author Douglas R. Miles
@license LGPL
*/

% :- system:use_module(library(logicmoo_startup)).
% % % OFF :- system:use_module(library(must_sanity)).

        
:- module_transparent
        check_thread_local_1m/1,
        each_call_cleanup_local/3,
        to_thread_local_1m/3,
        key_asserta/2,
        locally_tl/2,
        key_erase/1,
        module_effect/3,
        module_effect_ue/3.


:- set_module(class(library)).
% WAS OFF  :- system:use_module(library(no_repeats)).
% % % OFF :- system:use_module(library(logicmoo/each_call)).
:- use_module('./each_call').


%% locally_hide_each( :Fact, :Call) is nondet.
%
%  Temporally Disable Fact with `Fact :- !,fail.`
%
%  use locally_hide_each/3 if respecting Non-determism is important 
% (slightly slower?)
 
locally_hide(Fact,Cm:Call):-
  quietly(module_effect((Fact :- !,fail),M,BareEffect)) ->
    wtl(M,BareEffect,Cm:Call,Cm:setup_call_cleanup).

%% locally_hide_each( :Fact, :Call) is nondet.
%
%  Temporally Disable Fact with `Fact :- !,fail.`
%
%  But Ensure Non-determism is respected (slightly slower?)
%
%  uses each_call_cleanup/3 instead of setup_call_cleanup/3
 
locally_hide_each(Fact,Cm:Call):-  
  quietly(module_effect((Fact :- !,fail),M,BareEffect)) ->
    wtl(M,BareEffect,Cm:Call,Cm:each_call_cleanup).

:- meta_predicate(w_o_c(:)).

w_o_c(G):- tracing,!,call(G).
w_o_c(G):- catch(w_o_c(error, G),E,
  (wdmsg(w_o_c(error=E, G)),dumpST,wdmsg(w_o_c(error=E, G)),break,trace,G)).

:- meta_predicate(w_o_c(+,:)).
w_o_c(How, G):- 
   locally(set_prolog_flag(occurs_check,How),G).


%% locally_each( :Effect, :Call) is nondet.
%
%  Temporally have :Effect (see locally/2)
%
%  But Ensure Non-determism is respected (effect is undone between Redos)
%
%  uses each_call_cleanup/3 instead of setup_call_cleanup/3 (slightly slower?)
%
% for example,
%
%  locally_each/2 works (Does not throw)
% ===
% ?- current_prolog_flag(xref,Was), 
%     locally_each(set_prolog_flag(xref,true),
%     assertion(current_prolog_flag(xref,true));assertion(current_prolog_flag(xref,true))),
%     assertion(current_prolog_flag(xref,Was)),fail.
% ===
%
%  locally/2 does not work (it throws instead)
% ===
% ?- current_prolog_flag(xref,Was), 
%     locally(set_prolog_flag(xref,true),
%     assertion(current_prolog_flag(xref,true));assertion(current_prolog_flag(xref,true))),
%     assertion(current_prolog_flag(xref,Was)),fail.
% ===
locally_each(Em:Effect,Cm:Call):- wtl(Em,Effect,Cm:Call,Cm:each_call_cleanup).


%% locally( :Effect, :Call) is nondet.
%
% Effect may be of type:
%
%  set_prolog_flag -
%     Temporarily change prolog flag
%
%  op/3 - 
%     change op
%
%  $gvar=Value -
%     set a global variable
%
%  Temporally (thread_local) Assert some :Effect 
%
%  use locally_each/3 if respecting Non-determism is important 
% (slightly slower?)
%
% ===
% ?- current_prolog_flag(xref,Was), 
%     locally(set_prolog_flag(xref,true),
%     assertion(current_prolog_flag(xref,true))),
%     assertion(current_prolog_flag(xref,Was)). 
% ===


% locally(Em:Effect,Cm:Call):- ground(Call),!,wtl(Em,Effect,Cm:Call,Cm:setup_call_cleanup).
locally(Em:Effect,Cm:Call):- wtl(Em,Effect,Cm:Call,Cm:each_call_cleanup).

locally_tl(Effect,Call):- locally(t_l:Effect,Call).

local_override(N,V):- nb_current(N,V0),!,V0=V.

wtl(_,[],Call,_):- !,Call.
wtl(M,+With,Call,How):- !,wtl(M,With,Call,How).
wtl(M,-[With|MORE],Call,How):- !,wtl(M,-With,wtl(M,-MORE,Call,How),How).
wtl(M,[With|MORE],Call,How):- !,wtl(M,With,wtl(M,MORE,Call,How),How).
wtl(M,(With,MORE,How),Call,How):- !,wtl(M,With,wtl(M,MORE,Call,How),How).
wtl(M,(With;MORE,How),Call,How):- !,wtl(M,With,Call,How);wtl(M,MORE,Call,How).
wtl(M,not(With),Call,How):- !,wtl(M,- With,Call,How).
wtl(M,-With,Call,setup_call_cleanup):- !,locally_hide(M:With,Call).
wtl(M,-With,Call,_How):- !,locally_hide_each(M:With,Call).


wtl(M,op(New,XFY,OP),Call,_How):- 
  (M:current_op(PrevN,XFY,OP);PrevN=0),!,
   wtl_how(trusted_redo_call_cleanup, PrevN==New , op(New,XFY,OP), Call, op(PrevN,XFY,OP)).

wtl(M,set_prolog_flag(N,VALUE),Call,_How):- !,
  (M:current_prolog_flag(N,WAS);WAS=unknown_flag_error(M:set_prolog_flag(N,VALUE))),!,
   wtl_how(trusted_redo_call_cleanup, VALUE==WAS, M:set_prolog_flag(N,VALUE),Call,M:set_prolog_flag(N,WAS)).

wtl(M,current_prolog_flag(N,VALUE),Call,_How):- !,
  (M:current_prolog_flag(N,WAS);WAS=unknown_flag_error(M:set_prolog_flag(N,VALUE))),!,
   wtl_how(trusted_redo_call_cleanup, VALUE==WAS, M:set_prolog_flag(N,VALUE),Call,M:set_prolog_flag(N,WAS)).

wtl(M,local_override(N,VALUE),Call,_How):- !,  
   M:(nb_current(N,WAS) -> 
    call_cleanup((b_setval(N,VALUE),Call,b_setval(N,WAS)),b_setval(N,WAS));
    call_cleanup((b_setval(N,VALUE),Call,nb_delete(N)),nb_delete(N))).

wtl(M,nb_setval(N,VALUE),Call,_How):- !,  
   M:(nb_current(N,WAS) -> 
    call_cleanup((nb_setval(N,VALUE),Call,nb_setval(N,WAS)),nb_setval(N,WAS));
    call_cleanup((nb_setval(N,VALUE),Call,nb_delete(N)),nb_delete(N))).

wtl(M,$(N)=VALUE,Call,How):- !,
    wtl(M,local_override(N,VALUE),Call,How).

wtl(M,b_setval(N,VALUE),Call,How):- !,
    wtl(M,local_override(N,VALUE),Call,How).

% undocumented
wtl(M,before_after(Before,After,How),Call,How):- !,
     (M:Before -> call(How,true,Call,M:After); Call).

wtl(_,M:With,Call,How):- quietly(module_effect_ue(M:With,N,O))-> (O\==M:With),!,wtl(N,O,Call,How).
wtl(M,With,Call,How):- quietly(module_effect_ue(M:With,N,O))-> (O\==With),!,wtl(N,O,Call,How).

wtl(M,Assert,Call,setup_call_cleanup):- !,
   wtl_how(setup_call_cleanup,clause_true(M,Assert),M:asserta(Assert,Ref),Call,M:erase(Ref)).

wtl(M,Assert,Call,How):- 
   wtl_how(How,clause_true(M,Assert),
      key_asserta(M,Assert),Call,key_erase(M)).

clause_true(M,(H:-B)):- !, functor(H,F,A),functor(HH,F,A),M:nth_clause(HH,1,Ref),M:clause(HH,BB,Ref),!,(H:-B)=@=(HH:-BB).
clause_true(M, H    ):- copy_term(H,HH),M:clause(H,true),!,H=@=HH.

% wtl_how(How, Test , Pre , Call, Post)

%wtl_how(setup_call_cleanup, Test , Pre , Call, Post):- !, (Test -> Call ; setup_call_cleanup(Pre , Call, Post)).
%wtl_how(setup_call_cleanup, Test , Pre , Call, Post):- !, (Test -> Call ; setup_call_cleanup(Pre , Call, Post)).
wtl_how(setup_call_cleanup, _Test , Pre , Call, Post):- !, each_call_cleanup_local(Pre , Call, Post).
wtl_how(each_call_cleanup, _Test , Pre , Call, Post):- each_call_cleanup(Pre , Call, Post).
wtl_how(How, Test , Pre , Call, Post):-  Test -> Call ; call(How, Pre , Call, Post).

each_call_cleanup_local(Pre,Call,Post):- 
  redo_call_cleanup(Pre,Call,Post).

:- thread_initialization(nb_setval('$w_tl_e',[])).
:- initialization(nb_setval('$w_tl_e',[]),restore).

key_asserta(M,Assert):- M:asserta(Assert,Ref),
 (nb_current('$w_tl_e',Was)->nb_linkval('$w_tl_e',[(Ref)|Was]);nb_setval('$w_tl_e',[(Ref)])).

key_erase(M):- once(ignore(((nb_current('$w_tl_e',[(Ref)|Was])->(nb_linkval('$w_tl_e',Was)->catch(M:erase(Ref),E,dmsg(E))))))).



un_user(user:P,P):-!.
un_user(system:P,P):-!.
un_user(user:CMDI,CMDI):-!.
un_user(logicmoo_webbot:CMDI,CMDI):-!.
un_user(eggdrop:CMDI,CMDI):-!.
un_user(with_thread_local:P,P):-!.
un_user(P,P).

module_effect_e(M,H,HH):-
  quietly(module_effect(H,MNew,LH)),
  (MNew == M -> HH=LH ; HH= MNew:LH).

module_effect_ue(MP,M,P):-module_effect(MP,M,PU), un_user(PU,P).

module_effect(+M:Call,M,+Call).
module_effect(M: +Call,M,+Call).
module_effect(-M:Call,M,-Call).
module_effect(M: -Call,M,-Call).
module_effect(_:op(N,XFY,M:OP),M,op(N,XFY,OP)).
module_effect(M:set_prolog_flag(FM:Flag,Value),M,set_prolog_flag(FM:Flag,Value)).
module_effect(M:set_prolog_flag(Flag,Value),M,set_prolog_flag(M:Flag,Value)).
%module_effect(FM:set_prolog_flag(Flag,Value),FM,set_prolog_flag(FM:Flag,Value)).
module_effect($(M):N=V,M,$(N)=V).
module_effect(M:[H|T],M,[HH|TT]):-
  maplist(module_effect_e(M),[H|T],[HH|TT]).
  
module_effect(Assert,Module,ThreadLocal):-
   module_effect_striped(Assert,Module,Stripped),
   to_thread_local_1m(Stripped,Module,ThreadLocal).

module_effect(Call,Module,UnQCall):- strip_module(Call,Module,UnQCall).


module_effect_striped(_:((M:H):-B), M,(H:-B)).
module_effect_striped(M:(H:-B), M,(H:-B)).
module_effect_striped(((M:H):-B), M,(H:-B)).
module_effect_striped(Call,Module,UnQCall):- strip_module(Call,Module,UnQCall).


%% to_thread_local_1m( ?Call, ?Module, ?ThreadLocal) is det.
%
% Converted To Thread Local Head 
%
to_thread_local_1m(MM:HEAD,I,O):- MM = (M:M), !,to_thread_local_1m(M:HEAD,I,O).
to_thread_local_1m((TL:Head :- BODY),_,(TL:Head :- BODY)):- nonvar(TL),check_thread_local_1m(TL:Head).
to_thread_local_1m((H:-B),TL,(HH:-B)):-!,to_thread_local_1m(H,TL,HH).
to_thread_local_1m(Head,baseKB,t_l:Head).
to_thread_local_1m(Head,t_l,t_l:Head).
to_thread_local_1m(Head,tlbugger,tlbugger:Head).
to_thread_local_1m(Head,TL,TL:Head):-check_thread_local_1m(TL:Head).


%% check_thread_local_1m( ?TLHead) is nondet.
%
% Check Thread Local 1m.
%
check_thread_local_1m(_):- \+ current_prolog_flag(runtime_safety,3), \+ current_prolog_flag(runtime_speed,0).
check_thread_local_1m(t_l:_):-!.
check_thread_local_1m((H:-_)):-!,check_thread_local_1m(H).
check_thread_local_1m(tlbugger:_):-!.
check_thread_local_1m(lmcache:_):-!.
check_thread_local_1m(TLHead):- predicate_property(TLHead,(thread_local)).


:- if(current_predicate(fixup_exports/0)).
:- fixup_exports.
:- endif.

