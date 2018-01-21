/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (arglists.pl)
 *
 *
 * Douglas'' Notes 2017:
 *
 *
 *

[10:59] <dmiles> So in my param list parser I have: (item list &optional opt1 &rest rst &key (test (function eql) testp) (test-not () notp))
[10:59] <dmiles> I am parsing my keys and what is left over gets sent to rst but what i am wondering is if i should also delay parsing of opt1 as well
[11:00] <Bike> optional and rest together is kind of confusing.
[11:01] <dmiles> yeah
[11:02] <dmiles> though delete my &rest and then i suppose the question makes sense
[11:03] <dmiles> i just not thought of delaying &optional before
[11:04] <dmiles> i worked on a lisp impl for work we didnt have &key so everyone emulated them using &rest when they needed it
[11:05] <dmiles> and i always grabed &optionals first 
[11:06] <dmiles> (users/programmers knew to plug up their optional holes first)
[11:08] <dmiles> (so that is why i think of people using &optional and &rest together)
[11:09] <dmiles> but now i am implementing a common lisp .. oops i actually planned on compiling work code to .. but firstly i should obey common lisp (and have that code get fixed if it needs to be used)
[11:10] <dmiles> but at least that code wont ever use &key if it is doing that confussing weirdness
[11:11] <pjb> dmiles: not left over!  Everything!  &rest and &key take the same arguments.
[11:11] <dmiles> so should i do   requireds, keys, optionals, rest  or  requireds, optionals, keys, rest 
[11:12] <pjb> &key is a subparse of &rest if you will.
[11:12] <pjb> mandatory, optional, rest
[11:12] <dmiles> oh! still give them to &rest?
[11:12] <pjb> then if you have key you take them from rest.
[11:13] <dmiles> ok so i do hide them from rest
[11:13] <pjb> (defun foo (m &optional o &rest r &key k)  (list m o r k)) #| --> foo |# (foo 1 2 :k 3 :allow-other-keys t :foo 42) #| --> (1 2 (:k 3 :allow-other-keys t :foo 42) 3) |#
[11:14] <pjb> Note in particular, that once you have &key, this implies that (evenp (length rest))
[11:14] <pjb> and that every even numbered rest argument is a symbol.
[11:17] <dmiles> so ...  (foo 1 2 :k 3 :allow-other-keys t :foo 42)   my rest would bge.. ?
[11:17] <dmiles> () ?
[11:17] <pjb> it is (:k 3 :allow-other-keys t :foo 42) as show above.
[11:18] <dmiles> ok..  same with &body ?
[11:18] <pjb> same.
[11:20] <dmiles> i belive what your saing is correct but doesnt that make stuff like  (defmacro foo (&body b &key use-gentemp) ... )   harder?
[11:22] <dmiles> i guess actually that would be done like  (defmacro foo (&body b &key use-gentemp) ... )  

[11:22] <dmiles> oops
[11:22] <dmiles> i guess actually that would be done like  (defmacro foo ( bodus  &key use-gentemp) ... )  
[11:22] * Trystam is now known as Tristam
[11:23] <pjb> In the case of macros, you can use multiple level lambda lists.  (defmacro moo ((&body body) &key use-gentemp) ...)  (moo ((print 1) (print 2)) :use-gemtemp t)
[11:23] <pjb> of course, in general we keep &body at the end of the toplevel, and wrap the options in parentheses.
[11:23] <pjb> (defmacro moo ((&key use-gentemp) &body body) ...)
[11:23] <pjb> (moo (:use-gentemp t) (print 1) (print 2))
[11:24] <pjb> But you could use (&body body1) (&body body2) if you have several bodies.
[11:24] * dmiles has a ahah moment or two

[11:25] <sjl> dmiles: also remember you can give keyargs multiple times, and the leftmost one wins, but they're all included in the rest list http://paste.stevelosh.com/59fe04ce08977900084430d3
[11:25] * dmiles need sa list of hairy arglists :P
[11:25] <sjl> and don't forget &aux :)
[11:26] <pjb> https://framagit.org/com-informatimago/com-informatimago/blob/master/common-lisp/lisp-sexp/lambda-list-syntax.txt
[11:26] <dmiles> aux i got covered i think
[11:26] <pjb> lambda-list parser: https://framagit.org/com-informatimago/com-informatimago/blob/master/common-lisp/lisp-sexp/source-form.lisp#L816
[11:28] <sjl> non-GPL lambda-list parser: https://gitlab.common-lisp.net/alexandria/alexandria/blob/master/macros.lisp#L90
[11:29] <pjb> you can use the syntax to generate random lambda lists.
[11:29] <pjb> But you also want invalid lambda-lists and check your error detection.
[11:30] <dmiles> aux is allowed anwaywhere i was thinking
[11:31] <sjl> I'm not sure that's correct
[11:31] <dmiles> but not allowed anywhere?
[11:31] <sjl> clhs 3.4.1.5
[11:31] <specbot> Specifiers for &aux variables: http://www.lispworks.com/reference/HyperSpec/Body/03_dae.htm
[11:31] <sjl> If the lambda list keyword &aux is present, all specifiers after it are auxiliary variable specifiers.
[11:31] <sjl> "all specifiers after it"


from:  https://framagit.org/com-informatimago/com-informatimago/blob/master/common-lisp/lisp-sexp/lambda-list-syntax.txt

boa-lambda-list
ordinary-lambda-list::= (var*
                [&optional {var | (var [init-form [supplied-p-parameter]])}*]
                [&rest var]
                [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]]
                [&aux {var | (var [init-form])}*])


generic-lambda-list::= (var*
                [&optional {var | (var)}*]
                [&rest var]
                [&key {var | ({var | (keyword-name var)})}* [&allow-other-keys]])

specialized-lambda-list::= ({var | (var [specializer])}*
                [&optional {var | (var [init-form [supplied-p-parameter]])}*]
                [&rest var]
                [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]]
                [&aux {var | (var [init-form])}*])



reqvars::= var*

optvars::= [&optional {var | (var [init-form [supplied-p-parameter]])}*]

restvar::= [{&rest | &body} var]

keyvars::= [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}*
            [&allow-other-keys]]

auxvars::= [&aux {var | (var [init-form])}*]

envvar::= [&environment var]

wholevar::= [&whole var]

macro-lambda-list::= (wholevar envvar  reqvars envvar  optvars envvar
                restvar envvar  keyvars envvar  auxvars envvar) |
               (wholevar envvar  reqvars envvar  optvars envvar .  var)

pattern::= destructuring-lambda-list


destructuring-lambda-list::= (wholevar reqvars optvars restvar keyvars auxvars)
                           | (wholevar reqvars optvars . var)

setf-lambda-list::= (var*
                [&optional {var | (var [init-form [supplied-p-parameter]])}*]
                [&rest var]
                [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]]
                [&environment var]


type-lambda-list ::= macro-lambda-list

modify-macro-lambda-list::= (var*
                [&optional {var | (var [init-form [supplied-p-parameter]])}*]
                [&rest var]


method-combination-lambda-list::= (wholevar var*
                wholevar
                [&optional {var | (var [init-form [supplied-p-parameter]])}*]
                wholevar
                [&rest var]
                wholevar
                [&key {var | ({var | (keyword-name var)} [init-form [supplied-p-parameter]])}* [&allow-other-keys]]
                wholevar
                [&aux {var | (var [init-form])}*]
                wholevar)

*******************************************************************/
:- module(arglists, []).

:- set_module(class(library)).
:- include('header').


reserved_symbols(_Names,_PVars).
as_rest(_,R,R).
as_body(_,B,B).

get_env(Local,N,Env):- (get_var(Local,N,Env)->nonvar(Env))->true;
   (get_local_env(Local,Env)-> true ; (reenter_lisp(_Ctx,Env),set_local_env(Local,Env))).


/*kw_is_present(RestNKeys,F,KWP,KWPV):- 
   ((append(_Left,[F,KWPV|More],RestNKeys),length(More,Nth),is_evenp(Nth)) ->
     KWP=t ; KWP=[]).
*/
   
get_kw(_Env,RestNKeys,F,KW,Value,ElseInit,PresentP):- 
   ((append(_Left,[F,Value|More],RestNKeys),length(More,Nth),is_evenp(Nth)) -> PresentP=t ;
     ((F \== KW ,append(_Left,[F,Value|More],RestNKeys),length(More,Nth),is_evenp(Nth)) -> PresentP=t;
      (PresentP=[],ElseInit))).
     
      


enforce_atomic(F):- (simple_atom_var(F)->true;(lisp_dump_break)).
arginfo_incr(Prop,ArgInfo):- get_dict(Prop,ArgInfo,Old),New is Old +1, b_set_dict(Prop,ArgInfo,New).

arginfo_append(NewItem,Prop,ArgInfo):- get_dict(Prop,ArgInfo,Old),
  (Old==0 -> b_set_dict(Prop,ArgInfo,[NewItem]);
  (is_list(Old) -> (append(Old,[NewItem],New),b_set_dict(Prop,ArgInfo,New));
  (b_set_dict(Prop,ArgInfo,[Old,NewItem])))).
arginfo_set(Prop,ArgInfo,New):- nb_set_dict(Prop,ArgInfo,New).

  

enter_ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,Mode,FormalParms0,RequiredArgs,Names,PVars,Code):-
  correct_formal_params(FormalParms0,FormalParms),
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,Mode,FormalParms,RequiredArgs,Names,PVars,Code),
  maplist(add_param_var(Ctx),Names,PVars).

add_param_var(Ctx,Name,PVar):- 
   add_tracked_var(Ctx,Name,PVar),
   rw_add(Ctx,Name,p),
   debug_var([Name,'_In'],PVar).


ordinary_args(_Ctx,_Env,_ArgInfo,_RestNKeys,_Whole,_, [],[],[],[],true):-!.
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,_,['&allow-other-keys'|FormalParms],Params,Names,PVars,Code):- !,
  arginfo_incr(allow_other_keys,ArgInfo),
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,aux,FormalParms,Params,Names,PVars,Code).
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,_,['&allow_other_keys'|FormalParms],Params,Names,PVars,Code):- !,
  arginfo_incr(allow_other_keys,ArgInfo),
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,aux,FormalParms,Params,Names,PVars,Code).
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,_,['&aux'|FormalParms],Params,Names,PVars,Code):- !,
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,aux,FormalParms,Params,Names,PVars,Code).
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,_,['&optional'|FormalParms],Params,Names,PVars,Code):- !, 
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,optional,FormalParms,Params,Names,PVars,Code).

ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,_,['&key'|FormalParms],Params,Names,PVars,Code):- !,
  always(ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,key,FormalParms,Params,Names,PVars,Code)).

% &rest
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,_,['&rest',F|FormalParms],Params,[F|Names],[RestNKeys|PVars],PCode):- !, 
  arginfo_append(F,rest,ArgInfo),  
  arginfo_append(rest,complex,ArgInfo),
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,rest,FormalParms,Params,Names,PVars,PCode).
  
% &body
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,_,['&body',F|FormalParms],Params,[F|Names],[V|PVars],PCode):- !, 
  arginfo_append(F,rest,ArgInfo),
  arginfo_append(F,body,ArgInfo),
  arginfo_append(body,complex,ArgInfo),
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,rest,FormalParms,Params,Names,PVars,Code),
  PCode = (Code,as_body(F,V,RestNKeys)).

% &whole 
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,MODE,['&whole',F|FormalParms],Params,[F|Names],[Whole|PVars],PCode):- !, 
  arginfo_append(F,whole,ArgInfo), 
  arginfo_append(whole,complex,ArgInfo),
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,MODE,FormalParms,Params,Names,PVars,PCode).
  

 
% &environment
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,Mode,['&environment',F|FormalParms],Params,[F|Names],[V|PVars],
  (parent_env(V),Code)):-  !,
  arginfo_append(F,env,ArgInfo),
  arginfo_append(environment,complex,ArgInfo),
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,Mode,FormalParms,Params,Names,PVars,Code).

ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,Mode,['&environment'],Params,Names,PVars,Code):-!,
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,Mode,['&environment','$env'],Params,Names,PVars,Code).




% Parsing required(s)  (defmacro dolist ((var listform &optional resultform) &body body) ... )
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,required,[F|FormalParms],[List|Params],Names12,PVars12,
  (Code2,Code)):-  
  %debug_var('Destructure',List),
  %List = [H|T],List = [H|T],
  %debug_var('H',List),
 Whole2 = List,
  is_list(F),!,
  SubFormal = F,

  %HeadParms=Whole,
  debug_var('SubEnv',Env),
  ArgInfo2 = arginfo{req:0,all:0,sublists:0,opt:0,rest:0,whole:0,body:0,key:0,aux:0,env:0,allow_other_keys:0,names:Names,complex:0},
  %function_head_params(Ctx,Env,_,SubFormal,_ZippedArgEnv,_RestNKeys2,Whole2,_RequiredArgs2,ArgInfo2,Names2,PVars2,Code2),  
  enter_ordinary_args(Ctx,Env,ArgInfo2,RestNKeys2,Whole2,required,SubFormal,SubReqParams,Names2,PVars2,Code2),  
  %debug_var('SubRestNKeys',RestNKeys2),
  %expand_function_head(Ctx,Env,subSymbol,SubFormal,Whole2,_Head2,_ZippedArgEnv,_Result,_HeadDefCode,Code2),Names=Names12,PVars=PVars12,
  %make_compiled(Ctx,Env,_MResult,_Symbol,SubFormal,Whole2,_HeadParms,_HeadDefCode,Code2),Names=Names12,PVars=PVars12,
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,required,FormalParms,Params,Names,PVars,Code),!,
  append(Names,Names2,Names12),append(PVars,PVars2,PVars12),
  append(SubReqParams,RestNKeys2,List).
  


% Parsing required(s)
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,required,[F|FormalParms],[V|Params],[F|Names],[V|PVars],Code):- !,
  enforce_atomic(F),
  arginfo_append(F,all,ArgInfo),arginfo_append(F,req,ArgInfo),
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,required,FormalParms,Params,Names,PVars,Code).


% Parsing &optional(s)
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,optional,[ [F,InitForm,Present]|FormalParms],Params,[F,Present|Names],[V,PresentP|PVars],PCode):- !,
   enforce_atomic(F),
   arginfo_append(F,all,ArgInfo),arginfo_append(F,opt,ArgInfo),   
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,optional,FormalParms,Params,Names,PVars,Code),
   compile_init_opt(Env,ArgInfo,RestNKeys,F,V,[InitForm],InitCode),
   arg_info_count(ArgInfo,opt,Count),
   PCode = (arg_is_present(Env,RestNKeys,Present,PresentP,Count),InitCode,Code).
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,optional,[[F,InitForm]|FormalParms],Params,[F|Names],[V|PVars],(InitCode,Code)):- !,
   enforce_atomic(F),
   arginfo_append(F,all,ArgInfo),arginfo_append(F,opt,ArgInfo),   
   compile_init_opt(Env,ArgInfo,RestNKeys,F,V,[InitForm],InitCode),
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,optional,FormalParms,Params,Names,PVars,Code).
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,optional,[F|FormalParms],Params,Names,PVars,Code):- !,
   enforce_atomic(F),   
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,optional,[[F,[]]|FormalParms],Params,Names,PVars,Code).

% Parsing &aux(s)
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,aux,[[F,InitForm]|FormalParms],Params,[F|Names],[V|PVars],PCode):- !,
   enforce_atomic(F),   
   arginfo_append(F,aux,ArgInfo),    
   compile_aux_or_key(Env,F,V,[InitForm],InitCode),   
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,aux,FormalParms,Params,Names,PVars,Code),
   PCode = (InitCode,Code).
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,aux,[F|FormalParms],Params,Names,PVars,Code):-!, 
   enforce_atomic(F),   
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,aux,[[F,[]]|FormalParms],Params,Names,PVars,Code).

% Parsing &key(s)
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,key,[ [F,InitForm,Present]|FormalParms],Params,[F,Present|Names],[V,PresentP|PVars],PCode):- !,
   enforce_atomic(F),         
   to_kw(F,KW),
   arginfo_append(KW,key,ArgInfo),
   lisp_compile(Env,Else,InitForm,InitCode),
   body_cleanup_keep_debug_vars(Ctx,(InitCode,Else=V),InitElse),
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,key,FormalParms,Params,Names,PVars,MoreCode),
   debug_var([F,'_Present'],PresentP),   
   PCode = (get_kw(Env,RestNKeys,KW,F,V,InitElse,PresentP),MoreCode).
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,key,[ [[KW,F],InitForm]|FormalParms],Params,[F|Names],[V|PVars],PCode):- !,
   enforce_atomic(F),   
   arginfo_append(KW,key,ArgInfo),
   lisp_compile(Env,Else,InitForm,InitCode),
   body_cleanup_keep_debug_vars(Ctx,(InitCode,Else=V),InitElse),
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,key,FormalParms,Params,Names,PVars,Code),
   debug_var([F,'_P'],PresentP),
   PCode =(get_kw(Env,RestNKeys,KW,F,V,InitElse,PresentP),Code).

ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,key,[[F,InitForm]|FormalParms],Params,Names,PVars,Code):- !, 
   enforce_atomic(F), % loops  back
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,key,[[[F,F],InitForm]|FormalParms],Params,Names,PVars,Code).

ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,key,[F|FormalParms],Params,Names,PVars,Code):- !, 
   enforce_atomic(F),  % loops  back
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,key,[[[F,F],[]]|FormalParms],Params,Names,PVars,Code).

compile_init_opt(Env,ArgInfo,RestNKeys,Var,FinalResult,[InitForm],
  opt_var(Env,Var,FinalResult,Code,Result,Count,RestNKeys)):- 
    arg_info_count(ArgInfo,opt,Count),
    debug_var("Optionals",RestNKeys),
    lisp_compile(Result,InitForm,Code),!.

% compile_aux_or_key(Env,F,P,InitForm,InitCode)
compile_aux_or_key(Env,Var,FinalResult,[InitForm],
  (aux_var(Env,Var,FinalResult,Code,Result))):- 
    lisp_compile(Result,InitForm,Code),!.
compile_aux_or_key(Env,Var,FinalResult,[InitForm|_More],
  (aux_var(Env,Var,FinalResult,Code,Result))):- 
    lisp_dump_break,
    lisp_compile(Result,InitForm,Code).
   
% [name, 'package-designator', '&optional', [error, t]]


aux_var(Env, Var, In, G, Thru):- var(In),!,G,set_var(Env,Var,Thru).
aux_var(Env, Var, In, _, Thru):- set_var(Env,Var,In),!,In=Thru.

simple_atom_var(Atom):- atom(Atom), Atom\=nil,Atom\=[], correct_formal_params_c38(Atom,CAtom),
  \+ arg(_, v('&optional', '&key', '&aux', '&rest', '&body', '&environment'),CAtom).

% opt_var(Env,Var,FinalResult,Code,Result,Count,RestNKeys).
opt_var(Env, Var, FinalResult, _G, _Default, Nth, Optionals):- nth1(Nth,Optionals,Value),nonvar(Value),FinalResult=Value,set_var(Env,Var,Value).
opt_var(Env, Var, FinalResult, G, Default, _Nth, _Optionals):-  G, FinalResult=Value,FinalResult=Default,set_var(Env,Var,Value).

align_args_local(FN,RequiredArgs,RestNKeys,WholeMinusSymbol,LB,_ArgInfo,PARAMS,wl:init_args(x,FN)):- 
  get_init_args(FN,x),!,
  LB = true,
  RestNKeys = _,
  PARAMS = RequiredArgs,
  RequiredArgs =WholeMinusSymbol.

/*
% invoke([fn,r1,r2,r3],RET).
align_args_local(FN,RequiredArgs,RestNKeys,WholeMinusSymbol,LB,_ArgInfo,PARAMS,wl:init_args(whole,FN)):-
  eval_bind_parameters(FN),!,
  LB = append([FN|RequiredArgs],RestNKeys,WholeMinusSymbol),
  PARAMS = [WholeMinusSymbol].

% invoke([r1,r2,r3],RET).
align_args_local(FN,RequiredArgs,RestNKeys,WholeMinusSymbol,LB,_ArgInfo,PARAMS,wl:init_args(bind_parameters,FN)):-
  eval_uses_whole(FN),!,
  LB = append([FN|RequiredArgs],RestNKeys,WholeMinusSymbol),
  PARAMS = [WholeMinusSymbol].
 */

% invoke(r1,r2,[o3,key1,value1],RET).
align_args_local(FN,RequiredArgs,RestNKeys,WholeMinusSymbol,LB,ArgInfo,ArgsPlus,wl:init_args(N,FN)):- 
  get_init_args(FN,N),number(N),!,
  RestNKeys = _,
  append(RequiredArgs,[RestNKeys],BetterArgs),
  append(RequiredArgs,RestNKeys,WholeMinusSymbol),
   (ArgInfo.whole == 0 -> LB = true ; LB = append(RequiredArgs,RestNKeys,WholeMinusSymbol)),
  BetterArgs = ArgsPlus.

align_args_local(FN,RequiredArgs,RestNKeys,WholeMinusSymbol,LB,ArgInfo,GoodHeadParms,wl:init_args(Reqs,FN)):-
 always(is_list(RequiredArgs)),length(RequiredArgs,Reqs),
 append(RequiredArgs,[RestNKeys],RARGS), 
 append(RequiredArgs,RestNKeys,WholeMinusSymbol),
   (ArgInfo.whole == 0 -> LB = true ; LB = append(RequiredArgs,RestNKeys,WholeMinusSymbol)),
 align_args(FN,FN,RARGS,kILLiTT,HeadParms),!,
 append(GoodHeadParms,[kILLiTT],HeadParms).

align_args_local(FN,RequiredArgs,RestNKeys,WholeMinusSymbol,LB,ArgInfo,HeadParms,wl:init_args(Reqs,FN)):-
 always(is_list(RequiredArgs)),length(RequiredArgs,Reqs),
 append(RequiredArgs,RestNKeys,WholeMinusSymbol),
   (ArgInfo.whole == 0 -> LB = true ; LB = append(RequiredArgs,RestNKeys,WholeMinusSymbol)),
 append(RequiredArgs,[RestNKeys],HeadParms),!.

expand_function_head_macro(Ctx,Env,Symbol,FN,FormalParms,Whole,HeadParms,ZippedArgEnv,ArgInfo,HeadDefCode,HeadCode) :- 
 expand_function_head(Ctx,Env,Symbol,FN,FormalParms,Whole,HeadParms,ZippedArgEnv,ArgInfo,HeadDefCode,HeadCode).
/*
expand_function_head_macro(Ctx,Env,Symbol,Macro,FormalParms,Whole, HeadParms,ZippedArgEnv,ArgInfo, HeadDefCode,HeadCode):-
  expand_function_head(Ctx,Env,Symbol,Macro,FormalParms,Whole, HeadParms0,ZippedArgEnv,ArgInfo, HeadDefCode0,HeadCode0),!,
  (ArgInfo.env==0 -> 
    ((HeadCode=(global_env(Env),HeadCode0),HeadDefCode=HeadDefCode0,HeadParms=HeadParms0)) 
    ; 
    ((HeadCode=(global_env(Env),HeadCode0),HeadDefCode=(assert_lsp(Symbol,wl:declared(Macro,env_arg1)),HeadDefCode0), 
       HeadParms=HeadParms0))).
    */

% Dotted HeadParms
expand_function_head(Ctx,Env,Symbol,FN,FormalParms,Whole,HeadParms,ZippedArgEnv,ArgInfo,HeadDefCode,HeadCode):- 
   append(Req,Rest,FormalParms),Rest\==[],\+ Rest=[_|_], 
   append(Req,['&rest',Rest],NewFormalParms),!,
   expand_function_head(Ctx,Env,Symbol,FN,NewFormalParms,Whole,HeadParms,ZippedArgEnv,ArgInfo,HeadDefCode,HeadCode).


/*
% eval_uses_exact
expand_function_head(Ctx,Env,Symbol,FN,FormalParms,Whole,HeadParms,ZippedArgEnv,ArgInfo, HeadDefCode,(HeadCode)):-
      Whole = [_|WholeMinusSymbol],
   (eval_uses_exact(FN) ; \+ (member(Mode,FormalParms), \+ simple_atom_var(Mode))),!,
   %debug_var('NilRestNKeys',RestNKeys), 
       function_head_params(Ctx,Env,FN,FormalParms,ZippedArgEnv,_RestNKeys,Whole,RequiredArgs,ArgInfo,_Names,_PVars,HeadCode),
               HeadDefCode = (assert_lsp(Symbol,wl:arglist_info(Symbol,FN,FormalParms,ArgInfo)),!,
                 assert_lsp(Symbol,wl:init_args(x,FN))),
               always(HeadDefCode),
     WholeMinusSymbol= RequiredArgs,            
       HeadParms =  RequiredArgs.
       

% eval_uses_bind_parameters
expand_function_head(Ctx,Env,Symbol,FN,FormalParms,Whole,HeadParms,ZippedArgEnv,ArgInfo, HeadDefCode,HeadCodeOut):-
   Whole = [_|WholeMinusSymbol],
   eval_uses_bind_parameters(FN),!,
   debug_var('PBRestNKeys',RestNKeys),
   always((function_head_params(Ctx,Env,FN,FormalParms,ZippedArgEnv,RestNKeys,Whole,RequiredArgs,ArgInfo,_Names,_PVars,
     ZippedArgEnv,HeadCodeIgnored), % slow_trace,
               HeadDefCode = (assert_lsp(Symbol,wl:arglist_info(Symbol,FN,FormalParms,ArgInfo)),
                assert_lsp(Symbol,wl:init_args(bind_parameters,FN))),
               always(HeadDefCode),   
   debug_var('BinderCode',BindCode),
   debug_var('WholeMinusSymbol',WholeMinusSymbol), 
   HeadCodeOut = (
     append(RequiredArgs,RestNKeys,WholeMinusSymbol),
     ignore(HeadCodeIgnored),
     must_bind_parameters(Env,Whole,RestNKeys,FormalParms,WholeMinusSymbol,Env,BindCode),
     always(BindCode)),
   HeadParms = [WholeMinusSymbol])).
*/
% align_args_local
expand_function_head(Ctx,Env,Symbol,FN,FormalParms,Whole,HeadParms,ZippedArgEnv,ArgInfo, (HeadDefCode,assert_lsp(Symbol,Used)),
  HeadCodeOut):-
   Whole = [_|WholeMinusSymbol],
   debug_var('RestNKeys',RestNKeys),
   always((function_head_params(Ctx,Env,FN,FormalParms,ZippedArgEnv,RestNKeys,Whole,RequiredArgs,ArgInfo,_Names,_PVars,HeadCode),
   HeadDefCode = (assert_lsp(Symbol,wl:arglist_info(Symbol,FN,FormalParms,ArgInfo))),
   always(HeadDefCode),
   align_args_local(FN,RequiredArgs,RestNKeys,WholeMinusSymbol,LB,ArgInfo,HeadParms,Used),!,  
   assert_lsp(Symbol,Used:-true),
   HeadCodeOut = (LB,HeadCode))).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function_head_params(Ctx,Env,_Symbol,FormalParms,ZippedArgEnv,RestNKeys,Whole,RequiredArgs,ArgInfo,Names,PVars,Code):-!,
   ArgInfo = arginfo{req:0,all:0,sublists:0,opt:0,rest:0,whole:0,body:0,key:0,aux:0,env:0,allow_other_keys:0,names:Names,complex:0},
   enter_ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,required,FormalParms,RequiredArgs,Names,PVars,Code),
	zip_with(Names, PVars, [Var, Val, bv(Var,Val)]^true, ZippedArgEnv),!.
   

bind_formal_arginfo(ArgInfo, Arguments, OldEnv, NewEnv,BindCode):-
      ( ArgInfo.rest==0 -> bind_formal_arginfo_no_rest(ArgInfo, Arguments, OldEnv, NewEnv,BindCode);
      bind_formal_arginfo_with_rest(ArgInfo, Arguments, OldEnv, NewEnv,BindCode)).

bind_formal_arginfo_with_rest(ArgInfo, Arguments, OldEnv, NewEnv, _BindCode):- 
      arg_info_count(ArgInfo,req,N),length(Left,N),
      always(append(Left,RestNKeys,Arguments)), % Checks arg underflow
      ArgInfo.rest=[RestArg|_],
      append(OldEnv, bv(RestArg,RestNKeys),EnvMid),
      bind_formal_old(ArgInfo.names,Left,EnvMid,NewEnv).

bind_formal_arginfo_no_rest(ArgInfo, Arguments, OldEnv, NewEnv, _BindCode):- 
      bind_formal_old(ArgInfo.names,Arguments,OldEnv,NewEnv),!.
   


      
% Expands the arguments 
bind_formal_old(_LeftOver, [], Env, Env, true).
bind_formal_old([], _, Env, Env, true).
bind_formal_old([FormalParam|FormalParms], [ActualParam|ActualParams],
		Bindings0, Bindings,BindCode):- 
	bind_formal_old(FormalParms, ActualParams, 
		[bv(FormalParam,ActualParam)|Bindings0], Bindings,BindCode).



make_bind_value_missing( Var,Env,true):-simple_atom_var(Var),!,make_bind_value_required(Var,[],Env),!.
make_bind_value_missing([Var,InitForm],Env,Code):-!,simple_atom_var(Var),lisp_compile(Env,Value,InitForm,Code),make_bind_value_required(Var,Value,Env),!.
make_bind_value_missing([Var,InitForm,IfPresent],Env,Code):-simple_atom_var(Var),make_bind_value_required(IfPresent,[],Env),make_bind_value_missing([Var,InitForm],Env,Code).

make_bind_value_required(Var,Value,Env):-simple_atom_var(Var),!,debug_var([Var,'_In'],Value),append_open_list(Env,bv(Var,Value)).
make_bind_value_required([Var,_InitForm],Value,Env):-simple_atom_var(Var),make_bind_value_required(Var,Value,Env).
make_bind_value_required([Var,_InitForm,IfPresent],Value,Env):-simple_atom_var(Var),make_bind_value_required(IfPresent,t,Env),make_bind_value_required(Var,Value,Env).


must_or(Goal,Else):- Goal->true;Else.

correct_formal_params(Mode,ReMode):-  correct_formal_params_c38(Mode,RMode1),
  must(correct_formal_params_destructuring(RMode1,ReMode)).
correct_formal_params_c38(Mode,ReMode):- atom(Mode),atom_concat('c38_',Sym,Mode),!,atom_concat_or_rtrace('&',Sym,ReMode).
correct_formal_params_c38(Mode,Mode):- \+ compound(Mode),!.
correct_formal_params_c38([F0|FormalParms0],[F|FormalParms]):- 
  correct_formal_params_c38(F0,F),correct_formal_params_c38(FormalParms0,FormalParms).
correct_formal_params_c38(Mode,Mode).

correct_formal_params_destructuring([A, B, C|R],[A, B, C,'&rest',R]):- simple_atom_var(A),simple_atom_var(B),simple_atom_var(R),!.
correct_formal_params_destructuring([A, B|R],[A, B, '&rest',R]):- simple_atom_var(A),simple_atom_var(B),simple_atom_var(R),!.
correct_formal_params_destructuring([A|R],[A,'&rest',R]):- simple_atom_var(A),simple_atom_var(R),!.
correct_formal_params_destructuring(AA,AA).

must_bind_parameters(Env,Whole,RestNKeys,FormalParms0,Symbol,Params,Env,Code):-
  always(((correct_formal_params(FormalParms0,FormalParms),
   ignore(Whole = [Symbol|Params]),
   bind_each_param(Env,Whole,RestNKeys,FormalParms,Params,Code)))),!.

bind_each_param(Env,Whole,RestNKeys, FormalParms, Arguments,BindCode):-
  % append_open_list(Env,bind),
  bind_parameters(Env,Whole,RestNKeys, 'required', FormalParms, Arguments, BindCode),!.


append_open_list(Env,Value):- append(_,[Value|_],Env),!.
append_open_list(EnvList,Value):- member(Env,EnvList),append(_,[Value|_],Env),!.
append_open_list(ClosedList,Value):- ClosedList = [_Env|List], setarg(2,ClosedList,[Value|List]).

bind_parameters(_Env,_Whole,_RestNKeys,_Mode, [], _, true):-!.

% Switch mode &optional, &key or &aux mode
bind_parameters(Env,Whole,RestNKeys,_,[Mode|FormalParms],Params,Code):- 
  arg(_,v('&optional','&key','&aux'),Mode), !, 
  bind_parameters(Env,Whole,RestNKeys,Mode,FormalParms,Params,Code).

% &rest
bind_parameters(Env,Whole,RestNKeys,_,['&rest',Var|FormalParms],Params,Code):- !, 
  make_bind_value_required(Var,Params,Env),
  bind_parameters(Env,Whole,RestNKeys,'&rest',FormalParms,Params,Code).

% &environment
bind_parameters(Env,Whole,RestNKeys,Mode,['&environment',Var|FormalParms],Params,(ignore(parent_env(ParentEnv)),Code)):- !, 
   enforce_atomic(Var),
   make_bind_value_required(Var,ParentEnv,Env),
   bind_parameters(Env,Whole,RestNKeys,Mode,FormalParms,Params,Code).

% &whole
bind_parameters(Env,Whole,RestNKeys,Mode,['&whole',Var|FormalParms],Params,Code):- !, 
   enforce_atomic(Var),
   make_bind_value_required(Var,Whole,Env),
   bind_parameters(Env,Whole,RestNKeys,Mode,FormalParms,Params,Code).

bind_parameters(Env,_Whole,_RestNKeys,'required',[Var],Value,true):- nonvar(Value), \+ is_list(Value),
  enforce_atomic(Var),make_bind_value_required(Var,Value,Env),!.

% Parsing required(s)
bind_parameters(Env,Whole,RestNKeys,'required',[Var|FormalParms],In,Code):-  must_or(In=[Value|Params],(break,throw(args_underflow))),
  enforce_atomic(Var),make_bind_value_required(Var,Value,Env),
  bind_parameters(Env,Whole,RestNKeys,'required',FormalParms,Params,Code).

% Parsing optional(s)
bind_parameters(Env,Whole,RestNKeys,'&optional',[NDM|FormalParms],Params,(Code1,Code)):- Params==[], !,
  make_bind_value_missing(NDM,Env,Code1),
  bind_parameters(Env,Whole,RestNKeys,'&optional',FormalParms,Params,Code).
bind_parameters(Env,Whole,RestNKeys,'&optional',[NDM|FormalParms],[Value|Params],Code):- !,
  make_bind_value_required(NDM,Value,Env),
  bind_parameters(Env,Whole,RestNKeys,'&optional',FormalParms,Params,Code).

% Parsing aux(s)
bind_parameters(Env,Whole,RestNKeys,'&aux',[NDM|FormalParms],Params,(Code1,Code)):- 
  make_bind_value_missing(NDM,Env,Code1),
  bind_parameters(Env,Whole,RestNKeys,'&aux',FormalParms,Params,Code).

% Parsing &allow-other-keys
bind_parameters(Env,Whole,RestNKeys,_,['&allow_other_keys'|FormalParms],Params,Code):- !,
  make_bind_value_required(':allow-other-keys',t),
  bind_parameters(Env,Whole,RestNKeys,aux,FormalParms,Params,Code).
bind_parameters(Env,Whole,RestNKeys,_,['&allow-other-keys'|FormalParms],Params,Code):- !,
  make_bind_value_required(':allow-other-keys',t),
  bind_parameters(Env,Whole,RestNKeys,aux,FormalParms,Params,Code).

% &body TODO
bind_parameters(Env,Whole,RestNKeys,_,['&body'.Var|FormalParms],Params,Code):- !,
  make_bind_value_required(Var,Params,Env),
  bind_parameters(Env,Whole,RestNKeys,'required',FormalParms,Params,Code).


% Parsing &key (key var)
bind_parameters(Env,Whole,RestNKeys,'&key',[[KWS,Var]|FormalParms],Params,Params,(Code1,Code)):- 
   simple_atom_var(KWS),simple_atom_var(Var),!,
  (append(_,[KWS,Value],Params) -> 
    (make_bind_value_required(Var,Value,Env),Code1=true);
      make_bind_value_missing(Var,Env,Code1)),!,
 bind_parameters(Env,Whole,RestNKeys,'&key',FormalParms,Params,Code).

% Parsing &key key
bind_parameters(Env,Whole,RestNKeys,'&key',[Var|FormalParms],Params,Params,Code):-
   simple_atom_var(Var),!,
  (append(_,[Var,Value],Params) -> make_bind_value_required(Var,Value,Env); make_bind_value_required(Var,[],Env)),!,
   bind_parameters(Env,Whole,RestNKeys,'&key',FormalParms,Params,Code).
 
% Parsing &key (key initform keyp)
% Parsing &key ((key name) initform keyp)
bind_parameters(Env,Whole,RestNKeys,'&key',[[KWSpec,InitForm,IfPresent]|FormalParms],Params,Params,(Code1,Code)):-
   from_kw_spec(KWSpec,KWS,Var),!,
  (append(_,[KWS,Value],Params) -> (make_bind_value_required(IfPresent,t,Env),make_bind_value_required(Var,Value,Env),Code1=true);
   make_bind_value_missing([KWSpec,InitForm,IfPresent],Env,Code1)),!,
   bind_parameters(Env,Whole,RestNKeys,'&key',FormalParms,Params,Code).

% Parsing &key (key initform)
% Parsing &key ((key name) initform)
bind_parameters(Env,Whole,RestNKeys,'&key',[[KWSpec,InitForm]|FormalParms],Params,Params,(Code1,Code)):- 
   from_kw_spec(KWSpec,KWS,Var),!,
  (append(_,[KWS,Value],Params) -> (make_bind_value_required(Var,Value,Env),Code1=true);
   make_bind_value_missing([KWSpec,InitForm],Env,Code1)),!,
  bind_parameters(Env,Whole,RestNKeys,'&key',FormalParms,Params,Code).


from_kw_spec([KWS,Var],KWS,Var):- !,simple_atom_var(KWS),simple_atom_var(Var).
from_kw_spec(KWSVar,KWSVar,KWSVar):- simple_atom_var(KWSVar).


:- fixup_exports.

