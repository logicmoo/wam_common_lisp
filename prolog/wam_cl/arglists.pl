/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (arglists.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 *
 *******************************************************************/
:- module(arglists, []).
:- set_module(class(library)).
:- include('header.pro').


currently_visible_package(_P).

lisp_operator(defpackage).
lisp_operator(if).
lisp_operator(S):-is_special_op(S,P),currently_visible_package(P).
lisp_operator(S):-compiler_macro_left_right(S,_,_).
lisp_operator(S):-macro_lambda(S,_,_).

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

formal_args(_ArgInfo,RestNKeysInOut,RestNKeysInOut,_, [],[],[],[],true):-!.
formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,_,['&allow-other-keys'|FormalArgs],Params,Names,PVars,Code):- !,
  arginfo_incr(allow_other_keys,ArgInfo),
  formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,aux,FormalArgs,Params,Names,PVars,Code).
formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,_,['&aux'|FormalArgs],Params,Names,PVars,Code):- !,
  formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,aux,FormalArgs,Params,Names,PVars,Code).
formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,_,['&optional'|FormalArgs],Params,Names,PVars,Code):- !, 
  formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,optional,FormalArgs,Params,Names,PVars,Code).

formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,_,['&key'|FormalArgs],Params,Names,PVars,Code):- !,
  must_or_rtrace(formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,key,FormalArgs,Params,Names,PVars,Code)).

% &body and &rest
formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,_,['&rest',F|FormalArgs],Params,[F|Names],[V|PVars],PCode):- !, 
  arginfo_incr(rest,ArgInfo),
  formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,rest,FormalArgs,Params,Names,PVars,Code),
  PCode = (Code,must(as_rest(F,V,RestNKeysOut))).
% &body and &rest
formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,_,['&body',F|FormalArgs],Params,[F|Names],[V|PVars],PCode):- !, 
  arginfo_incr(rest,ArgInfo),
  formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,rest,FormalArgs,Params,Names,PVars,Code),
  PCode = (Code,must(as_rest(F,V,RestNKeysOut))).

 
% &env
formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,Mode,['&env',F|FormalArgs],Params,[F|Names],[V|PVars],(must(as_env(F,V,'$env')),Code)):- 
  arginfo_incr(env,ArgInfo),
  formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,Mode,FormalArgs,Params,Names,PVars,Code).
formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,Mode,['&env'],Params,Names,PVars,Code):-!,
   arginfo_incr(env,ArgInfo),
   formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,Mode,['&env',env],Params,Names,PVars,Code).

% Parsing required(s)
formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,required,[F|FormalArgs],[V|Params],[F|Names],[V|PVars],Code):- !,
  enforce_atomic(F),
  arginfo_incr(all,ArgInfo),arginfo_incr(req,ArgInfo),
  formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,required,FormalArgs,Params,Names,PVars,Code).


% Parsing &optional(s)
formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,optional,[ [F,InitForm,Supplied]|FormalArgs],[V|Params],[F,Supplied|Names],[V,SuppliedV|PVars],PCode):- !,
   enforce_atomic(F),
   arginfo_incr(all,ArgInfo),arginfo_incr(opt,ArgInfo),
   formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,optional,FormalArgs,Params,Names,PVars,Code),
   compile_init(F,V,[InitForm],Init),
   PCode = (arg_is_present(F,V,Supplied,SuppliedV),Init,Code).
formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,optional,[[F,InitForm]|FormalArgs],[V|Params],[F|Names],[V|PVars],(Init,Code)):- !,
   enforce_atomic(F),
   compile_init(F,V,[InitForm],Init),
   arginfo_incr(all,ArgInfo),arginfo_incr(opt,ArgInfo),
   formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,optional,FormalArgs,Params,Names,PVars,Code).
formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,optional,[F|FormalArgs],Params,Names,PVars,Code):- !,
   enforce_atomic(F),   
   arginfo_incr(all,ArgInfo),arginfo_incr(opt,ArgInfo),
   formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,optional,[[F,[]]|FormalArgs],Params,Names,PVars,Code).

% Parsing &aux(s)
formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,aux,[[F,InitForm]|FormalArgs],Params,[F|Names],[V|PVars],PCode):- !,
   enforce_atomic(F),   
   arginfo_incr(aux,ArgInfo),
   compile_init(F,V,[InitForm],Init),
   formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,aux,FormalArgs,Params,Names,PVars,Code),
   PCode = (Init,Code).
formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,aux,[F|FormalArgs],Params,Names,PVars,Code):-!, 
   enforce_atomic(F),   
   formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,aux,[[F,[],[]]|FormalArgs],Params,Names,PVars,Code).

% Parsing &key(s)
formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,key,[ [F,InitForm,KWP]|FormalArgs],Params,[F,KWP|Names],[V,KWPV|PVars],PCode):- !,
   enforce_atomic(F),   
   arginfo_incr(key,ArgInfo),
   compile_init(F,V,[InitForm],Init),
   debug_var("RestNKeysMid",RestNKeysMid),
   formal_args(ArgInfo,RestNKeysOut,RestNKeysMid,key,FormalArgs,Params,Names,PVars,Code),
   PCode = (kw_is_present(RestNKeysIn,F,KWP,KWPV),kw_obtain_value(RestNKeysIn,F,F,V,RestNKeysMid),Init,Code).
formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,key,[ [[KW,F],InitForm]|FormalArgs],Params,[F|Names],[V|PVars],PCode):- !,
   enforce_atomic(F),   
   arginfo_incr(key,ArgInfo),
   compile_init(F,V,[InitForm],Init),
   formal_args(ArgInfo,RestNKeysOut,RestNKeysMid,key,FormalArgs,Params,Names,PVars,Code),
   debug_var("RestNKeysMid",RestNKeysMid),
   PCode = (kw_obtain_value(RestNKeysIn,KW,F,V,RestNKeysMid),Init,Code).
formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,key,[[F,InitForm]|FormalArgs],Params,Names,PVars,Code):- !, 
   enforce_atomic(F),   
   arginfo_incr(key,ArgInfo),
   formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,key,[[[F,F],InitForm]|FormalArgs],Params,Names,PVars,Code).

formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,key,[F|FormalArgs],Params,Names,PVars,Code):- !, 
   enforce_atomic(F),   
   formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,key,[[[F,F],[]]|FormalArgs],Params,Names,PVars,Code).

tfa:- tfa([ item,list,'&key',key,[test, [function, eql], testp],['test-not', [], notp]]).
tfa(FormalArgs):-
  dbmsg(:-tfa(FormalArgs)),
  function_head_params(_Ctx,_Env,FormalArgs,ZippedArgBindings,ActualArgs,ArgInfo,Names,PVars,Code0),
  maplist(debug_var,Names,PVars),
  dbmsg(arginfo(ArgInfo,formal=FormalArgs,params=ActualArgs,names=Names,vars=PVars,zab=ZippedArgBindings)),
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


% Creates a function Head and an argument unpacker using Code to unpack
expand_function_head(Ctx,Env,[FunctionName | FormalArgs],Head,ZippedArgBindings, Result,Code):-!,
       function_head_params(Ctx,Env,FormalArgs,ZippedArgBindings,ActualArgs,ArgInfo,_Names,_PVars,Code0),
       append(ActualArgs, [Result], HeadArgs),
       Code = (assert(arglist_info(FunctionName,FormalArgs,ActualArgs,ArgInfo)),Code0),
       Head =.. [FunctionName | HeadArgs].
expand_function_head(Ctx,Env,FunctionName , Head, ZippedArgBindings, Result,Code):-
    expand_function_head(Ctx,Env,[FunctionName], Head, ZippedArgBindings, Result,Code).

       

function_head_params(_Ctx,Env,FormalArgs,ZippedArgBindings,ActualArgs,ArgInfo,Names,PVars,Code):-!,
   debug_var("RestNKeysIn",RestNKeysIn),debug_var("Env",Env),debug_var("RestNKeysOut",RestNKeysOut),
   debug_var("Code",Code),debug_var("ActualArgs",ActualArgs),
   ArgInfo = arginfo{req:0,all:0,opt:0,rest:0,key:0,aux:0,env:0,allow_other_keys:0},
   formal_args(ArgInfo,RestNKeysOut,RestNKeysIn,required,FormalArgs,ActualArgsMaybe,Names,PVars,Code),
   maplist(debug_var,Names,PVars),
        freeze(Arg,debug_var(Arg,Val)),
	zip_with(Names, PVars, [Arg, Val, bv(Arg, [Val|_])]^true, ZippedArgBindings),!,
   % RestNKeysOut=RestNKeysIn,
   ((\+ get_dict(rest,ArgInfo,0); \+ get_dict(key,ArgInfo,0)) ->  (append(ActualArgsMaybe,RestNKeysIn,ActualArgs00),ActualArgs0=[ActualArgs00]) ; ActualArgs0 = ActualArgsMaybe),
   ActualArgs0 = ActualArgs1,
   %(RESTP==t ->  (append(ActualArgsMaybe,RestNKeysIn,ActualArgs00),ActualArgs0=[ActualArgs00]) ; ActualArgs0 = ActualArgsMaybe),
   %(KeysP==t ->  (append(ActualArgs0,RestNKeysOut,ActualArgs11),ActualArgs1=[ActualArgs11]) ; ActualArgs1 = ActualArgs0),
   %(KeysP==t ->  append(ActualArgs0,[RestNKeysOut],ActualArgs1) ; ActualArgs1 = ActualArgs0),
   (\+ get_dict(env,ArgInfo,0) ->  append(ActualArgs1,[Env],ActualArgs) ; ActualArgs = ActualArgs1).



% Expands the arguments 
bind_formal_parameters([], [], Env, Env, true).
bind_formal_parameters([FormalParam|FormalParams], [ActualParam|ActualParams],
		Bindings0, Bindings,BindCode):- 
	bind_formal_parameters(FormalParams, ActualParams, 
		[bv(FormalParam, [ActualParam|_])|Bindings0], Bindings,BindCode).


% The idea here is that FunctionName/ArgNum may need evaluated or may have its own special evaluator 
expand_arguments(_Ctx,_Env,_FunctionName,_ArgNum,[], true, []).
expand_arguments(_Ctx,_Env,FunctionName,_, Args, true, Args):- lisp_operator(FunctionName),!.
expand_arguments(Ctx,Env,FunctionName,ArgNum,[Arg|Args], Body, [Result|Results]):-
       must_compile_body(Ctx,Env,Result,Arg, ArgBody),
       Body = (ArgBody, ArgsBody),
       ArgNum2 is ArgNum + 1,
       expand_arguments(Ctx,Env,FunctionName,ArgNum2,Args, ArgsBody, Results).

:- fixup_exports.

