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
:- include('./header').

/*
 * (si::process-lambda-list lambda-list context)
 *
 * Parses different types of lambda lists. CONTEXT may be MACRO,
 * FTYPE, FUNCTION, METHOD or DESTRUCTURING-BIND, and determines the
 * valid sytax. The output is made of several values:
 *
 * MKCL_VALUES(0) = (N req1 ... )                       ; required values
 * MKCL_VALUES(1) = (N opt1 init1 flag1 ... )   ; optional values
 * MKCL_VALUES(2) = rest-var                            ; rest-variable, if any
 * MKCL_VALUES(3) = key-flag                            ; T if &key was supplied
 * MKCL_VALUES(4) = (N key1 var1 init1 flag1 ... )      ; keyword arguments
 * MKCL_VALUES(5) = allow-other-keys                    ; flag &allow-other-keys
 * MKCL_VALUES(6) = (N aux1 init1 ... )         ; auxiliary variables
 *
 * 1) The prefix "N" is an integer value denoting the number of
 * variables which are declared within this section of the lambda
 * list.
 *
 * 2) The INIT* arguments are lisp forms which are evaluated when
 * no value is provided.
 *
 * 3) The FLAG* arguments is the name of a variable which holds a
 * boolean value in case an optional or keyword argument was
 * provided. If it is NIL, no such variable exists.
 */

f_sys_process_lambda_list(LambdaList,Context):- throw(f_sys_process_lambda_list(LambdaList,Context)).


reserved_symbols(_Names,_PVars).

/*
get_env(Local,N,Env):- (get_var(Local,N,Env)->nonvar(Env))->true;
   (get_local_env(Local,Env)-> true ; (reenter_lisp(_Ctx,Env),set_local_env(Local,Env))).
*/

enforce_atomic(F):- (simple_atom_var(F)->true;(lisp_dump_break)).
arginfo_incr(Prop,ArgInfo):- arginfo_append(Prop,complex,ArgInfo),get_dict(Prop,ArgInfo,Old),New is Old +1, b_set_dict(Prop,ArgInfo,New).

arginfo_append(NewItem,Prop,ArgInfo):- 
  enforce_atomic(NewItem),
  (\+ member(Prop,[all,req,complex])-> arginfo_append(Prop,complex,ArgInfo) ; true),
  get_dict(Prop,ArgInfo,Old),
  (Old==0 -> b_set_dict(Prop,ArgInfo,[NewItem]);
  (is_list(Old) -> ('$expand':member_eq(NewItem,Old)-> true;(append(Old,[NewItem],New),b_set_dict(Prop,ArgInfo,New)));
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

:- discontiguous ordinary_args/11.

ordinary_args(_Ctx,_Env,__ArgInfo,_RestNKeys,_Whole,_, [],[],[],[],true):-!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% &allow-other-keys
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,_,['c38_allow-other-keys'|FormalParms],Params,Names,PVars,Code):- !,
  arginfo_incr(allow_other_keys,ArgInfo),
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,aux,FormalParms,Params,Names,PVars,Code).
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,_,['c38_allow_other_keys'|FormalParms],Params,Names,PVars,Code):- !,
  arginfo_incr(allow_other_keys,ArgInfo),
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,aux,FormalParms,Params,Names,PVars,Code).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% switch to aux/option/key modes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,_,['c38_aux'|FormalParms],Params,Names,PVars,Code):- !,
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,aux,FormalParms,Params,Names,PVars,Code).
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,_,['c38_optional'|FormalParms],Params,Names,PVars,Code):- !, 
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,'c38_optional',FormalParms,Params,Names,PVars,Code).
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,_,['c38_key'|FormalParms],Params,Names,PVars,Code):- !,
  always(ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,key,FormalParms,Params,Names,PVars,Code)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% &rest
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,_,['c38_rest',F|FormalParms],Params,[F|Names],[V|PVars],(as_rest(F,V,Count,RestNKeys),PCode)):- !, 
  arg_info_count(ArgInfo,opt,Count),
  arginfo_append(F,rest,ArgInfo),  
  arginfo_append(rest,complex,ArgInfo),
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,rest,FormalParms,Params,Names,PVars,PCode).

as_rest(_,V,Count,RestNKeys):- ((nonvar(RestNKeys),length(Left,Count),append(Left,Right,RestNKeys))->V=Right;V=[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% &body
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,_,['c38_body',F|FormalParms],Params,[F|Names],[V|PVars],PCode):- !, 
  arg_info_count(ArgInfo,opt,Count),
  arginfo_append(F,rest,ArgInfo),
  arginfo_append(F,body,ArgInfo),
  arginfo_append(body,complex,ArgInfo),
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,rest,FormalParms,Params,Names,PVars,Code),
  PCode = (Code,as_body(F,V,Count,RestNKeys)).

as_body(_,V,Count,RestNKeys):- ((nonvar(RestNKeys),length(Left,Count),append(Left,Right,RestNKeys))->V=Right;V=[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% &whole 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,MODE,['c38_whole',F|FormalParms],Params,[F|Names],[Whole|PVars],PCode):- !, 
  arginfo_append(F,whole,ArgInfo), 
  arginfo_append(whole,complex,ArgInfo),
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,MODE,FormalParms,Params,Names,PVars,PCode).  

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% &environment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,Mode,['c38_environment',F|FormalParms],Params,[F|Names],[V|PVars],
  (parent_env(V),Code)):- fail, !,
  arginfo_append(F,env,ArgInfo),
  arginfo_append(environment,complex,ArgInfo),
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,Mode,FormalParms,Params,Names,PVars,Code).
*/

ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,Mode,['c38_environment',F|FormalParms],Params,[F|Names],[V|PVars],
  (Code)):- Env=V, !,
  arginfo_append(F,env,ArgInfo),
  arginfo_append(environment,complex,ArgInfo),
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,Mode,FormalParms,Params,Names,PVars,Code).

ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,Mode,['c38_environment'],Params,Names,PVars,Code):-!,
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,Mode,['c38_environment','$env'],Params,Names,PVars,Code).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Destructuring(s)  like: (defmacro dolist ((var listform &optional resultform) &body body) ... )
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
  ArgInfo2 = arginfo{req:0,all:0,sublists:0,opt:0,rest:0,whole:0,body:0,key:0,aux:0,env:0,allow_other_keys:0,names:Names,complex:0,outer:Env},
  %destructure_para eters(Ctx,Env,_,SubFormal,_ZippedArgEnv,_RestNKeys2,Whole2,_RequiredArgs2,ArgInfo2,Names2,PVars2,Code2),  
  enter_ordinary_args(Ctx,Env,ArgInfo2,RestNKeys2,Whole2,required,SubFormal,SubReqParams,Names2,PVars2,Code2),  
  %debug_var('SubRestNKeys',RestNKeys2),
  %make_hea d_params(Ctx,Env,subSymbol,subFn,SubFormal,RestNKeys,Whole2,_Head2,_ZippedArgEnv,_Result,_HeadDefCode,Code2),Names=Names12,PVars=PVars12,
  %make_compiled(Ctx,Env,_MResult,_Symbol,SubFormal,Whole2,_HeadParms,_HeadDefCode,Code2),Names=Names12,PVars=PVars12,
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,required,FormalParms,Params,Names,PVars,Code),!,
  append(Names,Names2,Names12),append(PVars,PVars2,PVars12),
  append(SubReqParams,RestNKeys2,List).
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing required(s)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,required,[F|FormalParms],[V|Params],[F|Names],[V|PVars],Code):- !,
  enforce_atomic(F),
  arginfo_append(F,all,ArgInfo),
  arginfo_append(F,req,ArgInfo),
  ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,required,FormalParms,Params,Names,PVars,Code).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing &optional(s)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,'c38_optional',[ [F,InitForm,Present]|FormalParms],Params,[F,Present|Names],[V,PresentP|PVars],(InitCode,Code)):- !,
   arginfo_append(F,opt,ArgInfo),arg_info_count(ArgInfo,opt,Count),arginfo_append(F,all,ArgInfo),
   compile_init_opt(Env,RestNKeys,F,V,[InitForm],Count,PresentP,InitCode),
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,'c38_optional',FormalParms,Params,Names,PVars,Code).
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,'c38_optional',[[F,InitForm]|FormalParms],Params,[F|Names],[V|PVars],(InitCode,Code)):- !,
   arginfo_append(F,opt,ArgInfo),arg_info_count(ArgInfo,opt,Count),arginfo_append(F,all,ArgInfo),
   compile_init_opt(Env,RestNKeys,F,V,[InitForm],Count,_PresentP,InitCode),
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,'c38_optional',FormalParms,Params,Names,PVars,Code).
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,'c38_optional',[F|FormalParms],Params,Names,PVars,Code):- !,
   enforce_atomic(F),   
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,'c38_optional',[[F,[]]|FormalParms],Params,Names,PVars,Code).

compile_init_opt(Env,RestNKeys,Var,FinalResult,[InitForm],Count,PresentP,
  opt_var(Env,Var,FinalResult,Code,Result,Count,PresentP,RestNKeys)):- 
    debug_var("Optionals",RestNKeys),
    lisp_compile(Result,InitForm,Code),!.

% opt_var(Env,Var,FinalResult,Code,Result,Count,RestNKeys).
opt_var(Env,Var,FinalResult,Code,Result,Count,RestNKeys):- opt_var(Env,Var,FinalResult,Code,Result,Count,_PresentP,RestNKeys).
opt_var(Env, Var, FinalResult, _G, _Default, Nth,t, Optionals):- nonvar(Optionals), nth1(Nth,Optionals,Value),nonvar(Value),FinalResult=Value,set_var(Env,Var,Value).
opt_var(Env, Var, FinalResult, G, Default, _Nth,[],_Optionals):-  G, FinalResult=Value,FinalResult=Default,set_var(Env,Var,Value).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing &aux(s)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,aux,[[F,InitForm]|FormalParms],Params,[F|Names],[V|PVars],PCode):- !,
   enforce_atomic(F),   
   arginfo_append(F,aux,ArgInfo),    
   compile_aux_or_key(Env,F,V,[InitForm],InitCode),   
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,aux,FormalParms,Params,Names,PVars,Code),
   PCode = (InitCode,Code).
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,aux,[F|FormalParms],Params,Names,PVars,Code):-!, 
   enforce_atomic(F),   
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,aux,[[F,[]]|FormalParms],Params,Names,PVars,Code).

% compile_aux_or_key(Env,F,P,InitForm,InitCode)
compile_aux_or_key(Env,Var,FinalResult,[InitForm],
  (aux_var(Env,Var,FinalResult,Code,Result))):- 
    lisp_compile(Result,InitForm,Code),!.
compile_aux_or_key(Env,Var,FinalResult,[InitForm|_More],
  (aux_var(Env,Var,FinalResult,Code,Result))):- 
    lisp_dump_break,
    lisp_compile(Result,InitForm,Code).

aux_var(Env, Var, In, G, Thru):- var(In),!,G,set_var(Env,Var,Thru).
aux_var(Env, Var, In, _, Thru):- set_var(Env,Var,In),!,In=Thru.

simple_atom_var(Atom):- atom(Atom), Atom\=nil,Atom\=[], correct_formal_params_c38(Atom,CAtom),
  \+ arg(_, v('c38_optional', 'c38_key', 'c38_aux', 'c38_rest', 'c38_body', 'c38_environment'),CAtom).

% [name, 'package-designator', 'c38_optional', [error, t]]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parsing &key(s)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,key,[[KWF,InitForm,Present]|FormalParms],Params,[Name,Present|Names],[V,PresentP|PVars],PCode):- !,
   from_kw_name(KWF,Name,KW),
   arginfo_append(Name,key,ArgInfo),
   debug_var(Present,PresentP),   
   lisp_compile(Env,Else,InitForm,InitCode),
   body_cleanup_keep_debug_vars(Ctx,(InitCode,Else=V),InitElse),
   PCode = (get_kw(Env,RestNKeys,KW,Name,V,InitElse,PresentP),MoreCode),
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,key,FormalParms,Params,Names,PVars,MoreCode).

ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,key,[KWF|FormalParms],Params,[Name|Names],[V|PVars],PCode):- !,   
   from_kw_form(KWF,KW,Name, InitForm ,_Present),
   arginfo_append(Name,key,ArgInfo),
   debug_var([Name,'_Present'],PresentP),   
   lisp_compile(Env,Else,InitForm,InitCode),
   body_cleanup_keep_debug_vars(Ctx,(InitCode,Else=V),InitElse), 
   PCode = (get_kw(Env,RestNKeys,KW,Name,V,InitElse,PresentP),MoreCode),
   ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,key,FormalParms,Params,Names,PVars,MoreCode).

get_kw(_Env,RestNKeys,KW,F,Value,ElseInit,PresentP):- 
   ((append(_Left,[KW,Value|More],RestNKeys),length(More,Nth), \+ is_oddp(Nth)) -> PresentP=t ;
     ((F \== KW ,append(_Left,[KW,Value|More],RestNKeys),length(More,Nth), \+ is_oddp(Nth)) -> PresentP=t;
      (PresentP=[],ElseInit))).
     
from_kw_name([KW,Name],Name,KW):- atom(Name),atom(KW).
from_kw_name([Name],Name,KW):- atom(Name),to_kw(Name,KW).
from_kw_name(Name,Name,KW):- atom(Name),to_kw(Name,KW).

from_kw_form([F,InitForm,Present],KW,Name,InitForm,Present):- from_kw_name(F,Name,KW),!,break.
from_kw_form([F,InitForm],KW,Name,InitForm,_Present):- from_kw_name(F,Name,KW),!.
from_kw_form([[KW,F]],KW,Name, InitForm ,_Present):- from_kw_name([KW,F],Name,KW),!,no_init_form(InitForm).
from_kw_form([F],KW,Name, InitForm ,_Present):- from_kw_name(F,Name,KW),!,no_init_form(InitForm).
from_kw_form(F,KW,Name, InitForm ,_Present):- from_kw_name(F,Name,KW),!,no_init_form(InitForm).

no_init_form([]).



align_args_local(FN,RequiredArgs,RestNKeys,Whole,LB,_ArgInfo,PARAMS,wl:init_args(x,FN)):- 
  get_init_args(FN,x),!,
  LB = true,
  RestNKeys = _,
  PARAMS = RequiredArgs,
 % append(RequiredArgs,RestNKeys,Whole),
  RequiredArgs = Whole.

/*
% invoke([fn,r1,r2,r3],RET).
align_args_local(FN,RequiredArgs,RestNKeys,Whole,LB,_ArgInfo,PARAMS,wl:init_args(whole,FN)):-
  eval_bind_parameters(FN),!,
  LB = append([FN|RequiredArgs],RestNKeys,Whole),
  PARAMS = [Whole].

% invoke([r1,r2,r3],RET).
align_args_local(FN,RequiredArgs,RestNKeys,Whole,LB,_ArgInfo,PARAMS,wl:init_args(bind_parameters,FN)):-
  eval_uses_whole(FN),!,
  LB = append([FN|RequiredArgs],RestNKeys,Whole),
  PARAMS = [Whole].
 */

% invoke(r1,r2,[o3,key1,value1],RET).
align_args_local(FN,RequiredArgs,RestNKeys,Whole,LB,_ArgInfo,ArgsPlus,wl:init_args(N,FN)):- 
  get_init_args(FN,N),number(N),length(RequiredArgs,NN),ignore(N=NN),
  RestNKeys = _,
  append(RequiredArgs,[RestNKeys],BetterArgs),
  append(RequiredArgs,RestNKeys,Whole),
  % (ArgInfo.whole == 0 -> LB = true ; LB = append(RequiredArgs,RestNKeys,Whole)),
  LB = true,
  BetterArgs = ArgsPlus.

align_args_local(FN,RequiredArgs,RestNKeys,Whole,LB,_ArgInfo,GoodHeadParms,wl:init_args(Reqs,FN)):-
 always(is_list(RequiredArgs)),length(RequiredArgs,NN), (Reqs=NN),
 append(RequiredArgs,[RestNKeys],RARGS), 
 append(RequiredArgs,RestNKeys,Whole),
 %  (ArgInfo.whole == 0 -> LB = true ; LB = append(RequiredArgs,RestNKeys,Whole)),
 LB = true,
 align_args(FN,FN,RARGS,kILLiTT,HeadParms),!,
 break,append(GoodHeadParms,[kILLiTT],HeadParms).

align_args_local(FN,RequiredArgs,RestNKeys,Whole,LB,_ArgInfo,HeadParms,wl:init_args(Reqs,FN)):-
 always(is_list(RequiredArgs)),length(RequiredArgs,Reqs),
 append(RequiredArgs,RestNKeys,Whole),
  % (ArgInfo.whole == 0 -> LB = true ; LB = append(RequiredArgs,RestNKeys,Whole)),
 LB = true,
 append(RequiredArgs,[RestNKeys],HeadParms),!.

/*
make_head_p arams(Ctx,Env,Symbol,Macro,FormalParms,RestNKeys,Whole, HeadParms,ZippedArgEnv,ArgInfo, HeadDefCode,HeadCode):-
  make_head_ params(Ctx,Env,Symbol,Macro,FormalParms,Whole, HeadParms0,ZippedArgEnv,ArgInfo, HeadDefCode0,HeadCode0),!,
  (ArgInfo.env==0 -> 
    ((HeadCode=(global_env(Env),HeadCode0),HeadDefCode=HeadDefCode0,HeadParms=HeadParms0)) 
    ; 
    ((HeadCode=(global_env(Env),HeadCode0),HeadDefCode=(assert_lsp(Symbol,wl:declared(Macro,env_arg1)),HeadDefCode0), 
       HeadParms=HeadParms0))).
    */

% align_args_local
make_head_params(Ctx,Env,Symbol,FN,FormalParms,Whole,RequiredArgs,RestNKeys,HeadParms,ZippedArgEnv,HeadDefCode,BodyCode):-
   debug_var('RestNKeys',RestNKeys), debug_var('WholeArgs',Whole),

   always((destructure_parameters(Ctx,Env,FormalParms,ZippedArgEnv,RestNKeys,Whole,RequiredArgs,ArgInfo,_Names,_PVars,HeadCode),  
   PrevHeadDefCode = (assert_lsp(Symbol,wl:arglist_info(Symbol,FN,FormalParms,ArgInfo))),
   always(PrevHeadDefCode),
   align_args_local(FN,RequiredArgs,RestNKeys,_HideWhole,LB,ArgInfo,HeadParms,FnDecl),!,  
   %append(RequiredArgs,RestNKeys,Whole),
   assert_lsp(Symbol,FnDecl:-true),
   HeadDefCode= (PrevHeadDefCode,assert_lsp(Symbol,FnDecl)),
   BodyCode = (LB,HeadCode))).

/*
must_bind_ parameters(Ctx,Env,Whole,RestNKeys,FormalParms0,Symbol,Params,Env,Code):-
  always(((correct_formal_params(FormalParms0,FormalParms),
   ignore(Whole = [Symbol|Params]),
   bind_each_param(Env,RestNKeys,Whole,FormalParms,Params,Code)))),!.
*/

break_on(_RestNKeys):- !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
destructure_parameters(Ctx,Env,FormalParms0,ZippedArgEnv,RestNKeys,Whole,RequiredArgs,ArgInfo,Names,PVars,Code):-!,
   correct_formal_params(FormalParms0,FormalParms),
   freeze(Whole,break_on(Whole)),freeze(RestNKeys,break_on(RestNKeys)),
   ArgInfo = arginfo{req:0,all:0,sublists:0,opt:0,rest:0,whole:0,body:0,key:0,aux:0,env:0,allow_other_keys:0,names:Names,complex:0,outer:Env},
   enter_ordinary_args(Ctx,Env,ArgInfo,RestNKeys,Whole,required,FormalParms,RequiredArgs,Names,PVars,Code),
   zip_with(Names, PVars, [Var, Val, bv(Var,Val)]^true, ZippedArgEnv),!,
   nop(((ArgInfo.names == ArgInfo.req, ArgInfo.req\==0)-> RestNKeys=[] ; RestNKeys=_)).
        

make_bind_parameters(Ctx,EnvIn,FormalParms,Whole,Arguments,EnvForBody,BinderCode):-
  freeze(Whole,break),freeze(RestNKeys,break),
  destructure_parameters(Ctx,EnvForBody,FormalParms,ZippedArgEnv,RestNKeys,Whole,RequiredArgs,_ArgInfo,_Names,_PVars,Code),
  %del_attr(Whole,freeze),  
  debug_var('RestNKeys',RestNKeys), debug_var('WholeArgs',Whole),    
  (BinderCode = ((append(RequiredArgs,RestNKeys,Arguments),EnvForBody=[ZippedArgEnv|EnvIn],del_attr(RestNKeys,freeze),Code))),!.
     


must_or(Goal,Else):- Goal->true;Else.

correct_formal_params(Mode,ReMode):-  correct_formal_params_c38(Mode,RMode1),
  must(correct_formal_params_destructuring(RMode1,ReMode)).
correct_formal_params_c38(Mode,ReMode):- atom(Mode),atom_concat('&',Sym,Mode),!,atom_concat_or_rtrace('c38_',Sym,ReMode).
correct_formal_params_c38(Mode,Mode):- \+ compound(Mode),!.
correct_formal_params_c38([F0|FormalParms0],[F|FormalParms]):- 
  correct_formal_params_c38(F0,F),correct_formal_params_c38(FormalParms0,FormalParms).
correct_formal_params_c38(Mode,Mode).

correct_formal_params_destructuring([A, B, C|R],[A, B, C,'c38_rest',R]):- simple_atom_var(A),simple_atom_var(B),simple_atom_var(R),!.
correct_formal_params_destructuring([A, B|R],[A, B, 'c38_rest',R]):- simple_atom_var(A),simple_atom_var(B),simple_atom_var(R),!.
correct_formal_params_destructuring([A|R],[A,'c38_rest',R]):- simple_atom_var(A),simple_atom_var(R),!.
% Dotted HeadParms
correct_formal_params_destructuring(FormalParms0,FormalParms):-
   append(Req,Rest,FormalParms0),Rest\==[],\+ Rest=[_|_], 
   append(Req,['c38_rest',Rest],FormalParms1),
   correct_formal_params_destructuring(FormalParms1,FormalParms).
correct_formal_params_destructuring(AA,AA).

append_open_list(Env,Value):- append(_,[Value|_],Env),!.
append_open_list(EnvList,Value):- member(Env,EnvList),append(_,[Value|_],Env),!.
append_open_list(ClosedList,Value):- ClosedList = [_Env|List], setarg(2,ClosedList,[Value|List]).

:- fixup_exports.


