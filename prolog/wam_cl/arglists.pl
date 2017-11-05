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
:- include('header.pro').


currently_visible_package(_P).

lisp_operator(defpackage).
lisp_operator(if).
lisp_operator(S):-is_special_op(S,P),currently_visible_package(P).
lisp_operator(S):-compiler_macro_left_right(S,_,_).
lisp_operator(S):-user:macro_lambda(_Scope,S,_,_).

is_special_op(S,P):- symbol_info(S,P,function_type,T),arg(_,v('special-operator',macro),T).
is_special_op('%%allocate-closures', 'sb-c').
is_special_op('%cleanup-fun', 'sb-c').
is_special_op('%escape-fun', 'sb-c').
is_special_op('%funcall', 'sb-c').
is_special_op('%primitive', 'sb-sys').
is_special_op('%within-cleanup', 'sb-c').
is_special_op('compiler-let', ext).
is_special_op('do*', 'common-lisp').
is_special_op('eval-when', 'common-lisp').
is_special_op('global-function', 'sb-c').
is_special_op('let*', 'common-lisp').
is_special_op('load-time-value', 'common-lisp').
is_special_op('multiple-value-bind', 'common-lisp').
is_special_op('multiple-value-call', 'common-lisp').
is_special_op('multiple-value-list', 'common-lisp').
is_special_op('multiple-value-prog1', 'common-lisp').
is_special_op('multiple-value-setq', 'common-lisp').
is_special_op('nth-value', 'common-lisp').
is_special_op('prog*', 'common-lisp').
is_special_op('return-from', 'common-lisp').
is_special_op('symbol-macrolet', 'common-lisp').
is_special_op('truly-the', 'sb-ext').
is_special_op('unwind-protect', 'common-lisp').
is_special_op(block, 'common-lisp').
is_special_op(case, 'common-lisp').
is_special_op(catch, 'common-lisp').
is_special_op(cond, 'common-lisp').
is_special_op(do, 'common-lisp').
is_special_op(dolist, 'common-lisp').
is_special_op(dotimes, 'common-lisp').
is_special_op(flet, 'common-lisp').
is_special_op(function, 'common-lisp').
is_special_op(go, 'common-lisp').
is_special_op(if, 'common-lisp').
is_special_op(labels, 'common-lisp').
is_special_op(lambda, 'common-lisp').
is_special_op(let, 'common-lisp').
is_special_op(locally, 'common-lisp').
is_special_op(macrolet, 'common-lisp').
is_special_op(prog, 'common-lisp').
is_special_op(prog1, 'common-lisp').
is_special_op(prog2, 'common-lisp').
is_special_op(progn, 'common-lisp').
is_special_op(progv, 'common-lisp').
is_special_op(psetq, 'common-lisp').
is_special_op(quote, 'common-lisp').
is_special_op(return, 'common-lisp').
is_special_op(setq, 'common-lisp').
is_special_op(tagbody, 'common-lisp').
is_special_op(the, 'common-lisp').
is_special_op(throw, 'common-lisp').
is_special_op(unless, 'common-lisp').
is_special_op(when, 'common-lisp').


reserved_symbols(_Names,_PVars).
as_rest(_,R,R).
as_env(_,E,E).

enforce_atomic(F):- (atom(F)->true;(dumpST,break)).
arginfo_incr(Prop,ArgInfo):- get_dict(Prop,ArgInfo,Old),New is Old +1, b_set_dict(Prop,ArgInfo,New).
arginfo_set(Prop,ArgInfo,New):- nb_set_dict(Prop,ArgInfo,New).

ordinary_args(_ArgInfo,RestNKeysInOut,RestNKeysInOut,_, [],[],[],[],true):-!.
ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,_,['&allow-other-keys'|FormalParms],Params,Names,PVars,Code):- !,
  arginfo_incr(allow_other_keys,ArgInfo),
  ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,aux,FormalParms,Params,Names,PVars,Code).
ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,_,['&aux'|FormalParms],Params,Names,PVars,Code):- !,
  ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,aux,FormalParms,Params,Names,PVars,Code).
ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,_,['&optional'|FormalParms],Params,Names,PVars,Code):- !, 
  ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,optional,FormalParms,Params,Names,PVars,Code).

ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,_,['&key'|FormalParms],Params,Names,PVars,Code):- !,
  must_or_rtrace(ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,key,FormalParms,Params,Names,PVars,Code)).

% &body and &rest
ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,_,['&rest',F|FormalParms],Params,[F|Names],[V|PVars],PCode):- !, 
  arginfo_set(rest,ArgInfo,F),
  ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,rest,FormalParms,Params,Names,PVars,Code),
  PCode = (Code,(as_rest(F,V,RestNKeysOut))).
% &body and &rest
ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,_,['&body',F|FormalParms],Params,[F|Names],[V|PVars],PCode):- !, 
  arginfo_set(rest,ArgInfo,F),
  ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,rest,FormalParms,Params,Names,PVars,Code),
  PCode = (Code,(as_rest(F,V,RestNKeysOut))).

 
% &env
ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,Mode,['&env',F|FormalParms],Params,[F|Names],[V|PVars],(must(as_env(F,V,'$env')),Code)):- 
  arginfo_incr(env,ArgInfo),
  ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,Mode,FormalParms,Params,Names,PVars,Code).
ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,Mode,['&env'],Params,Names,PVars,Code):-!,
   arginfo_incr(env,ArgInfo),
   ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,Mode,['&env',env],Params,Names,PVars,Code).

% Parsing required(s)
ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,required,[F|FormalParms],[V|Params],[F|Names],[V|PVars],Code):- !,
  enforce_atomic(F),
  arginfo_incr(all,ArgInfo),arginfo_incr(req,ArgInfo),
  rw_add(V,w),
  ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,required,FormalParms,Params,Names,PVars,Code).


% Parsing &optional(s)
ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,optional,[ [F,InitForm,Supplied]|FormalParms],[V|Params],[F,Supplied|Names],[V,SuppliedV|PVars],PCode):- !,
   enforce_atomic(F),
   arginfo_incr(all,ArgInfo),arginfo_incr(opt,ArgInfo),
   ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,optional,FormalParms,Params,Names,PVars,Code),
   compile_init(F,V,[InitForm],Init),
   PCode = (arg_is_present(F,V,Supplied,SuppliedV),Init,Code).
ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,optional,[[F,InitForm]|FormalParms],[V|Params],[F|Names],[V|PVars],(Init,Code)):- !,
   enforce_atomic(F),
   compile_init(F,V,[InitForm],Init),
   arginfo_incr(all,ArgInfo),arginfo_incr(opt,ArgInfo),
   ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,optional,FormalParms,Params,Names,PVars,Code).
ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,optional,[F|FormalParms],Params,Names,PVars,Code):- !,
   enforce_atomic(F),   
   arginfo_incr(all,ArgInfo),arginfo_incr(opt,ArgInfo),
   ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,optional,[[F,[]]|FormalParms],Params,Names,PVars,Code).

% Parsing &aux(s)
ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,aux,[[F,InitForm]|FormalParms],Params,[F|Names],[V|PVars],PCode):- !,
   enforce_atomic(F),   
   arginfo_incr(aux,ArgInfo),
   compile_init(F,V,[InitForm],Init),
   ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,aux,FormalParms,Params,Names,PVars,Code),
   PCode = (Init,Code).
ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,aux,[F|FormalParms],Params,Names,PVars,Code):-!, 
   enforce_atomic(F),   
   ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,aux,[[F,[],[]]|FormalParms],Params,Names,PVars,Code).

% Parsing &key(s)
ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,key,[ [F,InitForm,KWP]|FormalParms],Params,[F,KWP|Names],[V,KWPV|PVars],PCode):- !,
   enforce_atomic(F),   
   arginfo_incr(key,ArgInfo),
   compile_init(F,V,[InitForm],Init),
   debug_var("RestNKeysMid",RestNKeysMid),
   ordinary_args(ArgInfo,RestNKeysOut,RestNKeysMid,key,FormalParms,Params,Names,PVars,Code),
   PCode = (kw_is_present(RestNKeysIn,F,KWP,KWPV),kw_obtain_value(RestNKeysIn,F,F,V,RestNKeysMid),Init,Code).
ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,key,[ [[KW,F],InitForm]|FormalParms],Params,[F|Names],[V|PVars],PCode):- !,
   enforce_atomic(F),   
   arginfo_incr(key,ArgInfo),
   compile_init(F,V,[InitForm],Init),
   ordinary_args(ArgInfo,RestNKeysOut,RestNKeysMid,key,FormalParms,Params,Names,PVars,Code),
   debug_var("RestNKeysMid",RestNKeysMid),
   PCode = (kw_obtain_value(RestNKeysIn,KW,F,V,RestNKeysMid),Init,Code).
ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,key,[[F,InitForm]|FormalParms],Params,Names,PVars,Code):- !, 
   enforce_atomic(F),   
   arginfo_incr(key,ArgInfo),
   ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,key,[[[F,F],InitForm]|FormalParms],Params,Names,PVars,Code).

ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,key,[F|FormalParms],Params,Names,PVars,Code):- !, 
   enforce_atomic(F),   
   ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,key,[[[F,F],[]]|FormalParms],Params,Names,PVars,Code).

tfa:- tfa([ item,list,'&key',key,[test, [function, eql], testp],['test-not', [], notp]]).
tfa(FormalParms):-
  dbmsg(:-tfa(FormalParms)),
  function_head_params(_Ctx,_Env,FormalParms,ZippedArgBindings,ActualArgs,ArgInfo,Names,PVars,Code0),
  maplist(debug_var,Names,PVars),
  dbmsg(arginfo(ArgInfo,formal=FormalParms,params=ActualArgs,names=Names,vars=PVars,zab=ZippedArgBindings)),
  body_cleanup(Code0,Code),
  dbmsg(:-Code),!.

% compile_init(F,P,InitForm,Init)
compile_init(Var,FinalResult,[InitForm],
  (set_symbol_value_if_missing('$env',Var,FinalResult,Code,Result))):- 
    lisp_compile(Result,InitForm,Code).
compile_init(Var,FinalResult,[InitForm|_More],
  (set_symbol_value_if_missing('$env',Var,FinalResult,Code,Result))):- 
    dumpST,
    break,
    lisp_compile(Result,InitForm,Code).
   
% [name, 'package-designator', '&optional', [error, t]]

:- dynamic(user:arglist_info/4).

set_symbol_value_if_missing(Env, Var, In, G, Thru):- var(In),!,G,set_symbol_value(Env,Var,Thru).
set_symbol_value_if_missing(Env, Var, In, _, Thru):- set_symbol_value(Env,Var,In),!,In=Thru.


% Creates a function Head and an argument unpacker using Code to unpack
expand_function_head(Ctx,Env,[FunctionName | FormalParms],Head,ZippedArgBindings, Result,HeadDefCode,HeadCodeOut):-
   member(Mode,FormalParms),arg(_, v('&optional', '&key', '&aux', '&rest', '&body', '&environment'),Mode),!,
   must_det_l((function_head_params(Ctx,Env,FormalParms,ZippedArgBindings,ActualArgs,ArgInfo,_Names,_PVars,_HeadCode),
   arginfo_incr(complex,ArgInfo),
   append([Arguments], [Result], HeadArgs),
   debug_var('ArgsIn',Arguments),
   debug_var('BinderCode',BindCode),
   HeadDefCode = (asserta(user:arglist_info(FunctionName,FormalParms,ActualArgs,ArgInfo))),
   HeadCodeOut = (bind_parameters(Env,FormalParms,Arguments,BindCode),call(BindCode)),
   Head =.. [FunctionName | HeadArgs])).

% Creates a function Head and an argument unpacker using Code to unpack
expand_function_head(Ctx,Env,[FunctionName | FormalParms],Head,ZippedArgBindings, Result,HeadDefCode,HeadCode):-!,
       function_head_params(Ctx,Env,FormalParms,ZippedArgBindings,ActualArgs,ArgInfo,_Names,_PVars,HeadCode),
       append(ActualArgs, [Result], HeadArgs),
       HeadDefCode = (asserta(user:arglist_info(FunctionName,FormalParms,ActualArgs,ArgInfo))),
       Head =.. [FunctionName | HeadArgs].
expand_function_head(Ctx,Env,FunctionName , Head, ZippedArgBindings, Result,HeadDefCode,HeadCode):-
    expand_function_head(Ctx,Env,[FunctionName], Head, ZippedArgBindings, Result,HeadDefCode,HeadCode).



function_head_params(_Ctx,Env,FormalParms,ZippedArgBindings,ActualArgs,ArgInfo,Names,PVars,Code):-!,
   debug_var("RestNKeysIn",RestNKeysIn),debug_var("Env",Env),debug_var("RestNKeysOut",RestNKeysOut),
   debug_var("Code",Code),debug_var("ActualArgs",ActualArgs),
   ArgInfo = arginfo{req:0,all:0,opt:0,rest:0,key:0,aux:0,env:0,allow_other_keys:0,names:Names,complex:0},
   ordinary_args(ArgInfo,RestNKeysOut,RestNKeysIn,required,FormalParms,ActualArgsMaybe,Names,PVars,Code),
   maplist(debug_var,Names,PVars),
        freeze(Arg,debug_var(Arg,Val)),
	zip_with(Names, PVars, [Arg, Val, bv(Arg, [Val|_])]^true, ZippedArgBindings),!,
   % RestNKeysOut=RestNKeysIn,
   ((\+ get_dict(rest,ArgInfo,0); \+ get_dict(key,ArgInfo,0)) ->  
     (append(ActualArgsMaybe,RestNKeysIn,ActualArgs00),ActualArgs0=[ActualArgs00]) ; ActualArgs0 = ActualArgsMaybe),
   ActualArgs0 = ActualArgs1,
    (\+ get_dict(env,ArgInfo,0) ->  append(ActualArgs1,[Env],ActualArgs) ; ActualArgs = ActualArgs1).

bind_formal_arginfo(ArgInfo, Arguments, OldEnv, NewEnv,BindCode):-
      ( ArgInfo.rest==0 -> bind_formal_arginfo_no_rest(ArgInfo, Arguments, OldEnv, NewEnv,BindCode);
      bind_formal_arginfo_with_rest(ArgInfo, Arguments, OldEnv, NewEnv,BindCode)).

bind_formal_arginfo_with_rest(ArgInfo, Arguments, OldEnv, NewEnv, _BindCode):- 
      length(Left,ArgInfo.all),
      must(append(Left,Rest,Arguments)), % Checks arg underflow
      append(OldEnv, bv(ArgInfo.rest,[Rest|_]),EnvMid),
      bind_formal_old(ArgInfo.names,Left,EnvMid,NewEnv).

bind_formal_arginfo_no_rest(ArgInfo, Arguments, OldEnv, NewEnv, _BindCode):- 
   bind_formal_old(ArgInfo.names,Arguments,OldEnv,NewEnv),
   

      
% Expands the arguments 
bind_formal_old(_LeftOver, [], Env, Env, true).
bind_formal_old([FormalParam|FormalParms], [ActualParam|ActualParams],
		Bindings0, Bindings,BindCode):- 
	bind_formal_old(FormalParms, ActualParams, 
		[bv(FormalParam, [ActualParam|_])|Bindings0], Bindings,BindCode).



% The idea here is that FunctionName/ArgNum may need evaluated or may have its own special evaluator 
expand_arguments(_Ctx,_Env,_FunctionName,_ArgNum,[], true, []).
expand_arguments(_Ctx,_Env,FunctionName,_, Args, true, Args):- lisp_operator(FunctionName),!.

expand_arguments(Ctx,Env,FunctionName,ArgNum,[Arg|Args], Body, [Result|Results]):-
       must_compile_body(Ctx,Env,Result,Arg, ArgBody),
       Body = (ArgBody, ArgsBody),
       ArgNum2 is ArgNum + 1,
       expand_arguments(Ctx,Env,FunctionName,ArgNum2,Args, ArgsBody, Results).


find_hole([_Val|Vals], Hole):-
      var(Vals) 
       -> Hole = Vals 
       ; find_hole(Vals, Hole).



make_bind_value_missing( Var,Env,true):-atom(Var),!,make_bind_value(Var,[],Env),!.
make_bind_value_missing([Var,InitForm],Env,Code):-!,atom(Var),compile_init_form(Env,Value,InitForm,Code),make_bind_value(Var,Value,Env),!.
make_bind_value_missing([Var,InitForm,IfPresent],Env,Code):-atom(Var),make_bind_value(IfPresent,[],Env),make_bind_value_missing([Var,InitForm],Env,Code).

make_bind_value(Var,Value,Env):-atom(Var),!,debug_var([Var,'_In'],Value),append_open_list(Env,bv(Var,[Value|_])).
make_bind_value([Var,_InitForm],Value,Env):-atom(Var),make_bind_value(Var,Value,Env).
make_bind_value([Var,_InitForm,IfPresent],Value,Env):-atom(Var),make_bind_value(IfPresent,t,Env),make_bind_value(Var,Value,Env).


must_or(Goal,Else):- Goal->true;Else.

bind_parameters(Env, [A, B|R], Arguments,BindCode):- atom(A),atom(B),atom(R),!,
  bind_parameters(Env, [A, B, '&rest',R], Arguments,BindCode).
bind_parameters(Env, FormalParms, Arguments,BindCode):-
  % append_open_list(Env,bind),
  bind_parameters(Env, 'required', FormalParms, Arguments, BindCode),!.


append_open_list(Env,Value):- append(_,[Value|_],Env),!.
append_open_list(EnvList,Value):- member(Env,EnvList),append(_,[Value|_],Env),!.

bind_parameters(_Env,_Mode, [], _, true):-!.

% Switch mode &optional, &key or &aux mode
bind_parameters(Env,_,[Mode|FormalParms],Params,Code):- 
  arg(_,v('&optional','&key','&aux'),Mode), !, 
  bind_parameters(Env,Mode,FormalParms,Params,Code).

% &rest
bind_parameters(Env,_,['&rest',Var|FormalParms],Params,Code):- !, 
  make_bind_value(Var,Params,Env),
  bind_parameters(Env,'&rest',FormalParms,Params,Code).

% &environment
bind_parameters(Env,_,['&environment',Var|FormalParms],Params,(make_bind_value(Var,'$env',Env),Code)):- !,   
  bind_parameters(Env,'&rest',FormalParms,Params,Code).

% Parsing required(s)
bind_parameters(Env,'required',[Var|FormalParms],In,Code):- !, must_or(In=[Value|Params],throw(args_underflow)),
  enforce_atomic(Var),make_bind_value(Var,Value,Env),
  bind_parameters(Env,'required',FormalParms,Params,Code).

% Parsing optional(s)
bind_parameters(Env,'&optional',[NDM|FormalParms],Params,(Code1,Code)):- Params==[], !,
  make_bind_value_missing(NDM,Env,Code1),
  bind_parameters(Env,'&optional',FormalParms,Params,Code).
bind_parameters(Env,'&optional',[NDM|FormalParms],[Value|Params],Code):- !,
  make_bind_value(NDM,Value,Env),
  bind_parameters(Env,'&optional',FormalParms,Params,Code).

% Parsing aux(s)
bind_parameters(Env,'&aux',[NDM|FormalParms],Params,(Code1,Code)):- 
  make_bind_value_missing(NDM,Env,Code1),
  bind_parameters(Env,'&optional',FormalParms,Params,Code).

% Parsing &allow-other-keys
bind_parameters(Env,_,['&allow-other-keys'|FormalParms],Params,Code):- !,
  make_bind_value(':allow-other-keys',t),
  bind_parameters(Env,aux,FormalParms,Params,Code).

% &body TODO
bind_parameters(Env,_,['&body'.Var|FormalParms],Params,Code):- !,
  make_bind_value(Var,Params,Env),
  bind_parameters(Env,'required',FormalParms,Params,Code).


% Parsing &key (key var)
bind_parameters(Env,'&key',[[KWS,Var]|FormalParms],Params,Params,(Code1,Code)):- 
   atom(KWS),atom(Var),!,
  (append(_,[KWS,Value],Params) -> (make_bind_value(Var,Value,Env),Code1=true);
   make_bind_value_missing(Var,Env,Code1)),!,
   bind_parameters(Env,'&key',FormalParms,Params,Code).

% Parsing &key key
bind_parameters(Env,'&key',[Var|FormalParms],Params,Params,Code):-
   atom(Var),!,
  (append(_,[Var,Value],Params) -> make_bind_value(Var,Value,Env); make_bind_value(Var,[],Env)),!,
   bind_parameters(Env,'&key',FormalParms,Params,Code).
 
% Parsing &key (key initform keyp)
% Parsing &key ((key name) initform keyp)
bind_parameters(Env,'&key',[[KWSpec,InitForm,IfPresent]|FormalParms],Params,Params,(Code1,Code)):-
   from_kw_spec(KWSpec,KWS,Var),!,
  (append(_,[KWS,Value],Params) -> (make_bind_value(IfPresent,t,Env),make_bind_value(Var,Value,Env),Code1=true);
   make_bind_value_missing([KWSpec,InitForm,IfPresent],Env,Code1)),!,
   bind_parameters(Env,'&key',FormalParms,Params,Code).

% Parsing &key (key initform)
% Parsing &key ((key name) initform)
bind_parameters(Env,'&key',[[KWSpec,InitForm]|FormalParms],Params,Params,(Code1,Code)):- 
   from_kw_spec(KWSpec,KWS,Var),!,
  (append(_,[KWS,Value],Params) -> (make_bind_value(Var,Value,Env),Code1=true);
   make_bind_value_missing([KWSpec,InitForm],Env,Code1)),!,
  bind_parameters(Env,'&key',FormalParms,Params,Code).


from_kw_spec([KWS,Var],KWS,Var):- !,atom(KWS),atom(Var).
from_kw_spec(KWSVar,KWSVar,KWSVar):- atom(KWSVar).


:- fixup_exports.

