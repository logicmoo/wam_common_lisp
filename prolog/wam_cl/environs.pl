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
% user:portray(X):- is_rbtree(X),!,writeq(is_rbtree).
user:portray(env(RB,_)):- get_env_attribute(RB,name,Value),!, writeq(Value).
user:portray(env(_,_)):- writeq(env/2).



add_to_env(ENV,Name,Value):- update_or_append(nb_setarg,ENV,Name,Value).
%add_to_env(Name,Value):- global_env(ENV),update_or_prepend(nb_setarg,ENV,Name,Value).

global_env(ENV):- ignore(b_getval('$env_global',ENV)),!.
parent_env(ENV):- ignore(b_getval('$env_current',ENV)),!.
toplevel_env(ENV):- b_getval('$env_toplevel',ENV),!.


%new_compile_ctx(ENV):- new_assoc(ENV)put_attr(ENV,type,ctx).
new_compile_ctx([environ=Sym]):-gensym(env_,Sym).
%new_compile_ctx(env(ENV,[])):- gensym('iENV_',N), list_to_rbtree([type-ctx(N)],ENV).

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

set_parent_child(TL,ENV):- ==(TL,ENV),!.
set_parent_child(_TL,ENV):-   
  %ignore(set_env_attribute0(ENV,parent,TL)),
  nb_setval('$env_current',ENV).

get_tracker(ENV,ENV).
/*
get_tracker(ENV,Ctx):- is_rbtree(ENV),!,ENV=Ctx.
get_tracker(ENV,Ctx):- var(ENV),get_attr(ENV,tracker,Ctx),!.
get_tracker(ENV,_):- \+ compound(ENV),!,fail.
get_tracker(ENV,Ctx):- arg(_,ENV,Ctx),is_rbtree(Ctx),!.
%get_tracker(ENV,Ctx):- arg(_,ENV,ENV2),get_tracker(ENV2,Ctx),!.
*/
set_tracker(ENV,ENV):-!.
/*set_tracker(ENV,Ctx):- var(ENV),!,put_attr(ENV,tracker,Ctx).
set_tracker(ENV,Ctx):- compound(ENV),!,
  (((arg(N,ENV,Ctx),is_rbtree(Ctx)))->nb_setarg(N,ENV,Ctx);nb_setarg(1,ENV,Ctx)).
*/ 
%is_env(ENV):- get_tracker(ENV,Ctx),!, rb_in(type,ctx(_),Ctx).
is_env(ENV):- nonvar(ENV),!.
is_env(ENV):- notrace((sub_term(Sub,ENV),is_list(Sub))).

get_ctx_env_attribute(Ctx,Env,Name,Value):- get_env_attribute(Env,Name,Value)->true;get_env_attribute(Ctx,Name,Value).
   
get_env_attribute(Env,Name,Value):- notrace(get_env_attribute0(Env,Name,Value)).
get_env_attribute0(ENV,Name,Value):-fail, get_tracker(ENV,Ctx),rb_in(Name,Value,Ctx).
get_env_attribute0(Env,Name,Value):-  sub_term(Sub,Env),compound(Sub),Sub= (PName=VValue),Name==PName,Value=VValue,!.


set_env_attribute(Env,Name,Value):- quietly(always(set_env_attribute0(Env,Name,Value))).
set_env_attribute0(ENV,_Name,_Value):- var(ENV),!.
set_env_attribute0(ENV,Name,Value):-fail,
   get_tracker(ENV,Ctx),!,   nb_rb_insert(Ctx,Name,Value),
   %rb_insert(Ctx,Name,Value,Ctx1),%set_tracker(ENV,Ctx1),
   !.
set_env_attribute0(Env,Name,Value):- 
   sub_term(Sub,Env),compound(Sub),Sub=(PName=_),
   nonvar(PName),Name=PName,
   nb_setarg(2,Sub,Value),!.
set_env_attribute0(Env,Name,Value):- 
  sub_term(Sub,Env),compound(Sub),Sub=[H|T],
  nb_setarg(2,Sub,[H|T]),
  nb_setarg(1,Sub,Name=Value),!.
set_env_attribute0(Env,Name,Value):- 
  sub_term(Sub,Env),compound(Sub),
  Sub=..[F,H,T],
  SetT=..[F,H,T],
  nb_setarg(2,Sub,SetT),
  nb_setarg(1,Sub,Name=Value).



sub_term_index(Sub,Term,N,T) :-
        compound(Term),
        arg(N0, T0, Sub0),
        ((Sub0=Sub,T0=T,N0=N);sub_n_subterm(Sub,Sub0,N,T)).
        

remove_env_attribute(ENV,Name):-fail,
  get_tracker(ENV,Ctx),!,rb_delete(Ctx,Name,Ctx1),
  set_tracker(ENV,Ctx1).

remove_env_attribute(Env,Name):- 
  sub_term(Sub,Env),compound(Sub),
   arg(N,Sub,Nil),Nil=(PName=_),
   nonvar(PName),Name=PName,
   nb_setarg(N,Sub,[]),!.


% current_env(ENV):- nb_current('$env_current',ENV),!.
%current_env(ENV):- global_env(ENV),nb_linkval('$env_current',ENV),!.

get_local_env(Locals,ENV):- get_var(Locals,'$env',ENV).
set_local_env(Locals,ENV):- set_var(Locals,'$env',ENV).

reenter_lisp(CTX,ENV):- notrace(( ensure_ctx(CTX),ensure_env(ENV))).
/*
make_env_append(_Ctx,_Env,HeadEnv,[A|More],HeadEnv=More):-A==[],!.
make_env_append(_Ctx,_Env,HeadEnv,[A|List],HeadEnv=[A|More]):- List==[], var(More),!. % ,never_bind(More),!.
make_env_append(_Ctx,_Env,HeadEnv,[[A|List]|More],HeadEnv=ALL):- is_list(List),append([A|List],More,ALL).
make_env_append(_Ctx,_Env,HeadEnv,ZippedArgEnv,HeadEnv=ZippedArgEnv):-!.
*/
make_env_append(Ctx,Env,HeadEnv,More,HeadEnv=ALL):- 
   env_append(Ctx,Env,More,MALL),
   env_append(Ctx,Env,MALL,ALL),!.

env_append(_Ctx,_Env,More,ALL):-var(More),!,ALL=More.
env_append(_Ctx,_Env,[VAR|Rest],Rest):-VAR==Rest,!.
env_append(Ctx,Env,[[A|List]|More],Next):- is_list(A),List==[],append(A,Right,Next),env_append(Ctx,Env,More,ALL),ALL=Right,!.
env_append(Ctx,Env,[A|More],ALL):-A==[],!,env_append(Ctx,Env,More,ALL).
env_append(Ctx,Env,[[A|List]|More],[A|ALL]):- nonvar(A),env_append(Ctx,Env,[List|More],ALL).
env_append(Ctx,Env,[NONVAR|Rest],[A|ALL]):-nonvar(NONVAR),NONVAR=[A|List],!,env_append(Ctx,Env,[List|Rest],ALL).
%env_append(_Ctx,_Env,[A|List],[A|More]):- List==[], var(More),!. % ,never_bind(More),!.
env_append(_Ctx,_Env,[[A|List]|More],[[A|List]|More]):- List==More,!.
env_append(_Ctx,_Env,ZippedArgEnv,ZippedArgEnv):-!.

% GlobalBindings

ensure_env(ENV):- (nonvar(ENV)->true;(is_env(ENV)->true;current_env(ENV))).
ensure_ctx(ENV):- (nonvar(ENV)->true;(is_env(ENV)->true;b_getval('$env_global',ENV))).
current_env(ENV):- ensure_env,nb_current('$env_current',WASENV),!,(is_env(ENV)->WASENV==ENV;WASENV=ENV).

ensure_env :-
  (nb_current('$env_current',_)->true;reset_env).

reset_env:- 
  always((
  nb_delete('$env_current'),
  nb_delete('$env_global'),
  nb_delete('$env_toplevel'),
  new_compile_ctx(GLOBAL),
  set_env_attribute0(GLOBAL,name,'GLOBAL'),
  debug_var('ToplevelEnv',TL),debug_var('GLOBAL',GLOBAL),
  new_compile_ctx(TL),
  set_env_attribute0(TL,name,'TOPLEVEL'),
  nb_setval('$env_global',GLOBAL),
  nb_setval('$env_topevel',TL),
  nb_setval('$env_current',TL),
  set_parent_child(GLOBAL,TL))).



get_value_or_default(Ctx,Name,Value,IfMissing):- oo_get_attr(Ctx,Name,Value)->true;Value=IfMissing.


get_alphas(Ctx,Alphas):- get_tracker(Ctx,Ctx0),get_alphas0(Ctx0,Alphas).
get_alphas0(Ctx,Alphas):- get_value_or_default(Ctx,alphas,Alphas,[]).

add_alphas(_,_):-!.
add_alphas(Ctx,Alphas):- always((get_tracker(Ctx,Ctx0),add_alphas0(Ctx0,Alphas))).
add_alphas0(Ctx,Alpha):- atom(Alpha),!,get_value_or_default(Ctx,alphas,Alphas,[]),oo_put_attr(Ctx,alphas,[Alpha|Alphas]).
add_alphas0(_Ctx,Alphas):- \+ compound(Alphas),!.
add_alphas0(Ctx,Alphas):- Alphas=..[_|ARGS],maplist(add_alphas0(Ctx),ARGS).


:- fixup_exports.

