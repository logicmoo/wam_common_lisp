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
%el_new(X):-X=[tl].


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


user:portray(environment{name:N, tracker:_}):-!,writeq(N).
user:portray(X):- is_rbtree(X),!,writeq(is_rbtree).
user:portray(env(RB,_)):- get_env_attribute(RB,name,Value),!, writeq(Value).
user:portray(env(_,_)):- writeq(env/2).



add_to_env(ENV,Name,Value):- update_or_append(nb_setarg,ENV,Name,Value).
%add_to_env(Name,Value):- global_env(ENV),update_or_prepend(nb_setarg,ENV,Name,Value).

ensure_env(ENV):- notrace(nonvar(ENV)->true;( \+ is_env(ENV)->current_env(ENV);true)).


%new_compile_ctx(ENV):- new_assoc(ENV)put_attr(ENV,type,ctx).
new_compile_ctx(env(ENV,[])):- gensym('iENV_',N), list_to_rbtree([type-ctx(N)],ENV).


global_env(ENV):- b_getval('$env_global',ENV),!.
toplevel_env(ENV):- b_getval('$env_toplevel',ENV),!.
current_env(ENV):- b_getval('$env_current',ENV),!.

extend_env(ENV):-
  current_env(TL),
  new_compile_ctx(ENV),
  set_parent_child(TL,ENV).

unextend_env(Parent):-
  current_env(Current),
  toplevel_env(TL),
  (TL==Current -> Parent=Current;
   (get_env_attribute(Current,parent,Parent),
    get_env_attribute(Parent,parent,NewParent),
    set_parent_child(NewParent,Parent))),!.
unextend_env(ENV):- current_env(ENV).

set_parent_child(TL,ENV):-   
  ignore(set_env_attribute(ENV,parent,TL)),
  nb_setval('$env_current',ENV).


get_tracker(ENV,Ctx):- var(ENV),!,get_attr(ENV,tracker,Ctx).
get_tracker(ENV,_):- \+ compound(ENV),!,fail.
get_tracker(ENV,Ctx):- is_rbtree(ENV),!,ENV=Ctx.
get_tracker(ENV,Ctx):- arg(_,ENV,Ctx),is_rbtree(Ctx),!.
%get_tracker(ENV,Ctx):- arg(_,ENV,ENV2),get_tracker(ENV2,Ctx),!.

set_tracker(ENV,Ctx):- var(ENV),!,put_attr(ENV,tracker,Ctx).
set_tracker(ENV,Ctx):- compound(ENV),!,
  (((arg(N,ENV,Ctx),is_rbtree(Ctx)))->nb_setarg(N,ENV,Ctx);nb_setarg(1,ENV,Ctx)).
  
is_env(ENV):- get_tracker(ENV,Ctx),!, rb_in(type,ctx(_),Ctx).

get_env_attribute(ENV,Name,Value):-
  get_tracker(ENV,Ctx), rb_in(Name,Value,Ctx).

set_env_attribute(ENV,Name,Value):-
   get_tracker(ENV,Ctx),!,    
   nb_rb_insert(Ctx,Name,Value),!,
   %rb_insert(Ctx,Name,Value,Ctx1),
   %set_tracker(ENV,Ctx1).
   !.

remove_env_attribute(ENV,Name):-
  get_tracker(ENV,Ctx), rb_delete(Ctx,Name,Ctx1),
  set_tracker(ENV,Ctx1).


% current_env(ENV):- nb_current('$env_current',ENV),!.
%current_env(ENV):- global_env(ENV),nb_linkval('$env_current',ENV),!.

get_local_env(Locals,ENV):- get_var(Locals,'$env',ENV).
set_local_env(Locals,ENV):- set_var(Locals,'$env',ENV).

reenter_lisp(ENV,ENV):- notrace(( current_env(ENV),current_env(ENV))).


% GlobalBindings

ensure_env :-
  (nb_current('$env_current',_)->true;reset_env).

reset_env:- 
  always((
  nb_delete('$env_current'),
  nb_delete('$env_global'),
  nb_delete('$env_toplevel'),
  new_compile_ctx(GLOBAL),
  set_env_attribute(GLOBAL,name,'GLOBAL'),
  debug_var('ToplevelEnv',TL),debug_var('GLOBAL',GLOBAL),
  new_compile_ctx(TL),
  set_env_attribute(TL,name,'TOPLEVEL'),
  nb_setval('$env_global',GLOBAL),
  nb_setval('$env_topevel',TL),
  nb_setval('$env_current',TL),
  set_parent_child(GLOBAL,TL))).

:- fixup_exports.

