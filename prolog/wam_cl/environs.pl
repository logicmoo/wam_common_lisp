/* ******************************************************************
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

:- include('./header').

:- meta_predicate ct(3,?,?,?).
:- meta_predicate push_append(3,?,?,?).
:- meta_predicate push_el_append(3,?,*,*,*).
:- meta_predicate push_le_prepend(3,?,*,?,?).
:- meta_predicate push_list_prepend(3,?,*,*).
:- meta_predicate push_lst_append(3,?,*,*).
:- meta_predicate push_prepend(3,?,?,?).
:- meta_predicate set_el(3,?,*,*).
:- meta_predicate update_el_or_append(3,?,*,*,?).
:- meta_predicate update_el_tail_or_append(3,?,*,?,?).
:- meta_predicate update_lst_or_append(3,?,*,?).
:- meta_predicate update_lst_or_prepend(3,*,*,*).
:- meta_predicate update_or_append(3,?,?,?).
:- meta_predicate update_or_prepend(3,?,?,?).
:- meta_predicate within_labels_context(*,*,0).

:- use_module(library(rbtrees)).

load_and_call(G):- current_predicate(_,G) -> call(G) ; throw(load_and_call(G)).

:- thread_initialization(nb_setval('$labels_suffix','')).
suffix_by_context(_Ctx,Atom,SuffixAtom):- nb_current('$labels_suffix',Suffix),atom_concat_suffix(Atom,Suffix,SuffixAtom).
suffixed_atom_concat(Ctx,L,R,LRS):- atom_concat_or_rtrace(L,R,LR),suffix_by_context(Ctx,LR,LRS).
push_labels_context(Ctx,Atom):- suffix_by_context(Ctx,Atom,SuffixAtom),b_setval('$labels_suffix',SuffixAtom).
within_labels_context(_Ctx,Label,G):- nb_current('$labels_suffix',Suffix),
   setup_call_cleanup(b_setval('$labels_suffix',Label),G,b_setval('$labels_suffix',Suffix)).
gensym_in_labels(Ctx,Stem,GenSym):- suffix_by_context(Ctx,Stem,SuffixStem),gensym(SuffixStem,GenSym).

get_label_suffix(_Ctx,Suffix):-nb_current('$labels_suffix',Suffix).


show_ctx_info(Ctx):- term_attvars(Ctx,CtxVars),maplist(del_attr_rev2(freeze),CtxVars),show_ctx_info2(Ctx).
show_ctx_info2(Ctx):- ignore((get_tracker(Ctx,Ctx0),in_comment(show_ctx_info3(Ctx0)))).
show_ctx_info3(Ctx):- is_rbtree(Ctx),!,forall(rb_in(Key, Value, Ctx),fmt9(Key=Value)).
show_ctx_info3(Ctx):- fmt9(ctx=Ctx).
     

% el_new(el(X,X)):-X=[].
%el_new(X):-X=[tl].


set_el(O,Env,Name,Value):- HH=bv(Name,Value), H=[HH],ct(O,1,Env,H),ct(O,2,Env,H).

/* * PUSH-PREPEND-IF-NEW **/
update_or_prepend(O,Env,Name,Value):- 
  Env=el(List,_) 
   -> (List==[]-> set_el(O,Env,Name,Value);update_lst_or_prepend(O,List,Name,Value))
  ; update_lst_or_prepend(O,Env,Name,Value).

update_lst_or_prepend(O,Env,Name,Value):- Env=[H|T],H=bv(Name,_),
  (ct(O,2,H,Value)->true;(T\==[],update_lst_or_prepend(O,T,Name,Value))),!.
update_lst_or_prepend(O,Env,Name,Value):- Env=[H|T],!,ct(O,2,Env,[H|T]),ct(O,1,Env,bv(Name,Value)).

/* * PUSH-APPEND-IF-NEW **/
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
  
/* PUSH-APPEND */
push_append(O,Env,Name,Value):- Env=el(_,_)->push_el_append(O,Env,Env,Name,Value);push_lst_append(O,Env,Name,Value).
push_el_append(O,Env,el(_,[]),Name,Value):- set_el(O,Env,Name,Value).
push_el_append(O,Env,el(_,Tail),Name,Value):- T=[bv(Name,Value)],ct(O,2,Tail,T),ct(O,2,Env,T).
push_lst_append(O,Env,Name,Value):- Env=[_|T],(T==[]->(ct(O,2,Env,[bv(Name,Value)]));push_lst_append(O,T,Name,Value)).

/* * PUSH-PREPEND **/
push_prepend(O,Env,Name,Value):- Env=el(List,_)->push_le_prepend(O,Env,List,Name,Value);push_list_prepend(O,Env,Name,Value).

push_le_prepend(O,Env,[],Name,Value):- !, set_el(O,Env,Name,Value).
push_le_prepend(O,Env,_,Name,Value):- push_list_prepend(O,Env,Name,Value).

push_list_prepend(O,Env,Name,Value):- Env=[H|T],ct(O,2,Env,[H|T]),ct(O,1,Env,bv(Name,Value)).



:- multifile(user:portray/1).
:- dynamic(user:portray/1).
:- discontiguous(user:portray/1).

my_portray_list(Var):- var(Var),!,writeq(Var),!.
my_portray_list([]):- writeq('}').
my_portray_list([H|List]):- hide_portray(H),!,my_portray_list(List).
my_portray_list([H|List]):- my_portray_list(H),!,write(','),my_portray_list(List).
my_portray_list(_).

hide_portray(C):- ground(C),hide_portray_g(C).

hide_portray_g(var_tracker(_)=_Dict).

% user:portray(List):- notrace((nonvar(List),List=[_|_],sub_term(E,List),ground(E),E = ((environ=W)),write(environment(W)))).

user:portray(X):- is_rbtree(X),!,writeq(is_rbtree).
%user:portray(List):- nonvar(List),List=[_|_],member(E,List),hide_portray(E),!,write('[{'),ignore(my_portray_list(List)),write('}]'),!.
%user:portray(Hide):- hide_portray(Hide),!,write('.').
user:portray(environment{name:N, tracker:_}):-!,writeq(e(N)).
%user:portray(env(RB,_)):- get_env_attribute(RB,name,Value),!, writeq(Value).
%user:portray(env(_,_)):- writeq(env/2).



add_to_env(ENV,Name,Value):- update_or_append(nb_setarg,ENV,Name,Value).
%add_to_env(Name,Value):- global_env(ENV),update_or_prepend(nb_setarg,ENV,Name,Value).

global_env(ENV):- ignore(nb_current('$env_global',ENV)),!.
parent_env(ENV):- ignore(nb_current('$env_current',ENV)),!.
toplevel_env(ENV):- nb_current('$env_toplevel',ENV),!.


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
get_env_attribute0(Env,Name,Value):-  sub_term(Sub,Env),compound(Sub),Sub=(PName=VValue),(Name=PName),Value=VValue,!.


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

get_lambda_def(Ctx,Env,defmacro,ProcedureName,FormalParms,LambdaExpression):- get_symbol_fbounds(Ctx,Env,ProcedureName,kw_macro,[lambda,FormalParms|LambdaExpression]).
get_lambda_def(Ctx,Env,defun,ProcedureName,FormalParms,LambdaExpression):- get_symbol_fbounds(Ctx,Env,ProcedureName,kw_function,[lambda,FormalParms|LambdaExpression]).
get_lambda_def(_Ctx,_Env,DefType,ProcedureName,FormalParms,LambdaExpression):- wl:lambda_def(DefType,ProcedureName,_,FormalParms,LambdaExpression).
get_lambda_def(_Ctx,_Env,DefType,ProcedureName,FormalParms,LambdaExpression):-  wl:lambda_def(DefType,_,ProcedureName,FormalParms,LambdaExpression).

get_symbol(Sym,Symbol):- \+ compound(Sym),!,Sym=Symbol.
get_symbol(Sym,Symbol):- arg(1,Sym,Mid),!,get_symbol(Mid,Symbol).
get_symbol(Sym,Sym).

get_symbol_fbounds(Ctx,Env,Sym,BindTypeReq,FBOUND):- get_symbol(Sym,Symbol),  
  %(Symbol==u_babbit->trace;true),
   BindTypeReq=BindType,
  get_symbol_fbounds0(Ctx,Env,Symbol,BindType,FBOUND),
  BindTypeReq=BindType.

get_symbol_fbounds0(Ctx,Env,Symbol,BindType,FBOUND):- 
  ((get_env_attribute(Env,fbound(Symbol,BindType),FBOUND0));
   get_env_attribute(Ctx,fbound(Symbol,BindType),FBOUND0)),!,
  normalized_fbound(FBOUND0,FBOUND).

normalized_fbound(FBOUND0,FBOUND):- \+ compound(FBOUND0),FBOUND0=FBOUND.
normalized_fbound(function(FBOUND0),FBOUND):- !, normalized_fbound(FBOUND0,FBOUND).
normalized_fbound(FBOUND0,FBOUND):- FBOUND0=FBOUND.
  



%get_symbol_fbounds0(_Ctx,_Env,Symbol,BindType,ProposedName):- get_opv(Symbol,symbol_function,ProposedName),
%  (atom(ProposedName)->bind_type_naming(BindType,_,ProposedName);bind_type_naming_of(BindType,Symbol,ProposedName)).

     
add_symbol_fbounds(Ctx,Env,Name=Value):- 
  always((set_env_attribute(Ctx,Name,Value),
  set_env_attribute(Env,Name,Value))).

remove_symbol_fbounds(Ctx,Env,Name=Value):- 
  always((remove_env_attribute(Ctx,Name,Value),
  remove_env_attribute(Env,Name,Value))).


sub_term_index(Sub,Term,N,T) :-
        compound(Term),
        arg(N0, T0, Sub0),
        ((Sub0=Sub,T0=T,N0=N);sub_term_index(Sub,Sub0,N,T)).
        

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
  always(( env_append(Ctx,Env,More,MALL),
   env_append(Ctx,Env,MALL,ALL))),!.

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

ensure_ctx(ENV):- (nonvar(ENV)->true;(is_env(ENV)->true;ignore((notrace((nb_current('$env_global',ENV))))))).
ensure_env(ENV):- (nonvar(ENV)->true;(is_env(ENV)->true;current_env(ENV))).
current_env(ENV):- ignore((notrace((ensure_env,nb_current('$env_current',WASENV),!,(is_env(ENV)->WASENV==ENV;WASENV=ENV))))).

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
:- thread_initialization(reset_env).

