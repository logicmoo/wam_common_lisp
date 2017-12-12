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
:- module(env, []).
:- set_module(class(library)).
:- include('header').


% el_new(el(X,X)):-X=[].
el_new(X):-X=[tl].


set_el(O,Env,Name,Value):- HH=bv(Name,Value), H=[HH],ct(O,1,Env,H),ct(O,2,Env,H).

/** PUSH-PREPEND-IF-NEW **/
update_or_prepend(O,Env,Name,Value):- 
  Env=el(List,_) 
   -> (List==[]-> set_el(O,Env,Name,Value);update_lst_or_prepend(O,List,Name,Value))
  ; update_lst_or_prepend(O,Env,Name,Value).

update_lst_or_prepend(O,Env,Name,Value):- Env=[H|T],H=bv(Name,_),
  (ct(O,2,H,Value)->true;(T\==[],update_lst_or_prepend(O,T,Name,Value))),!.
update_lst_or_prepend(O,Env,Name,Value):- Env=[H|T],!,ct(O,2,Env,[H|T]),ct(O,1,Env,bv(Name,Value)).

/** PUSH-APPEND-IF-NEW **/
update_or_append(O,Env,Name,Value):- 
    Env=el(_,_) -> update_el_or_append(O,Env,Env,Name,Value); update_lst_or_append(O,Env,Name,Value).

update_el_or_append(O,Env,el([H|List],_),Name,Value):- !, 
   update_el_tail_or_append(O,[H|List],Name,Value,TO),
   (nonvar(TO)->true;ct(O,2,Env,TO)).
update_el_or_append(O,Env,_,Name,Value):- !, set_el(O,Env,Name,Value).
update_lst_or_append(O,Env,Name,Value):- Env=[HH|T],HH=H,
  (H=bv(Name,_)->ct(O,2,HH,Value);
  (T==[]->(ct(O,2,Env,[bv(Name,Value)]));update_lst_or_append(O,T,Name,Value))).
update_el_tail_or_append(O,Env,Name,Value,TO):- Env=[H|T],
  (((H=bv(Name,_),ct(O,2,H,Value)) -> TO =_;
    (T==[]-> TO=[bv(Name,Value)],
       (ct(O,2,Env,TO));update_el_tail_or_append(O,T,Name,Value,TO)))).

ct(O,N,P,E):- var(E) -> true ; call(O,N,P,E).
  
/** PUSH-APPEND **/
push_append(O,Env,Name,Value):- Env=el(_,_)->push_el_append(O,Env,Env,Name,Value);push_lst_append(O,Env,Name,Value).
push_el_append(O,Env,el(_,[]),Name,Value):- set_el(O,Env,Name,Value).
push_el_append(O,Env,el(_,Tail),Name,Value):- T=[bv(Name,Value)],ct(O,2,Tail,T),ct(O,2,Env,T).
push_lst_append(O,Env,Name,Value):- Env=[_|T],(T==[]->(ct(O,2,Env,[bv(Name,Value)]));push_lst_append(O,T,Name,Value)).

/** PUSH-PREPEND **/
push_prepend(O,Env,Name,Value):- Env=el(List,_)->push_le_prepend(O,Env,List,Name,Value);push_list_prepend(O,Env,Name,Value).

push_le_prepend(O,Env,[],Name,Value):- !, set_el(O,Env,Name,Value).
push_le_prepend(O,Env,_,Name,Value):- push_list_prepend(O,Env,Name,Value).

push_list_prepend(O,Env,Name,Value):- Env=[H|T],ct(O,2,Env,[H|T]),ct(O,1,Env,bv(Name,Value)).












add_to_env(Env,Name,Value):- update_or_append(nb_setarg,Env,Name,Value).
%add_to_env(Name,Value):- env_toplevel(Env),update_or_prepend(nb_setarg,Env,Name,Value).

ensure_env(Env):- notrace(( var(Env)->env_current(Env);true)).

env_toplevel(Env):- b_getval('$env_toplevel',Env),!.
env_current(Env):-env_toplevel(Env).

:- el_new(ENV),nb_setval('$env_toplevel',ENV).

% env_current(Env):- nb_current('$env_current',Env),!.
%env_current(Env):- env_toplevel(Env),nb_linkval('$env_current',Env),!.

reenter_lisp(Ctx,Env):- notrace(( env_current(Env),new_compile_ctx(Ctx))). 
% GlobalBindings


:- fixup_exports.

