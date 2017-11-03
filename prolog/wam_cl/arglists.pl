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

bind_formal_parameters(Formal, Actual, Bindings):-
	env_bind_variables(Formal, Actual, [], Bindings).


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

%%formal_args(Form  alArgs,Params,(reserved_symbols(Names,PVars),Code)):- formal_args(_NRD,Rest,required,FormalArgs,Params,Names,PVars,Code).
 
formal_args(NRD,Rest,_, [],[Rest],[],[],true):-NRD==t,!.
formal_args(_,_Rst,_, [],    [],[],[],true):-!.
formal_args(_,R,_,['&rest',F|FormalArgs],Params,[F|Names],[P|PVars],(must(as_rest(F,P,R)),Code)):- formal_args(t,R,rest,FormalArgs,Params,Names,PVars,Code).
formal_args(_,R,_,['&body',F|FormalArgs],Params,[F|Names],[P|PVars],(must(as_rest(F,P,R)),Code)):- formal_args(t,R,rest,FormalArgs,Params,Names,PVars,Code).
formal_args(NRD,R,_,['&aux'|FormalArgs],Params,Names,PVars,Code):- formal_args(NRD,R,aux,FormalArgs,Params,Names,PVars,Code).
formal_args(NRD,R,_,['&key'|FormalArgs],Params,Names,PVars,Code):- formal_args(NRD,R,key,FormalArgs,Params,Names,PVars,Code).
formal_args(NRD,R,_,['&optional'|FormalArgs],Params,Names,PVars,Code):- formal_args(NRD,R,optional,FormalArgs,Params,Names,PVars,Code).
formal_args(NRD,R,required,[F|FormalArgs],[P|Params],[F|Names],[P|PVars],Code):- !, atomic(F),!,formal_args(NRD,R,required,FormalArgs,Params,Names,PVars,Code).

formal_args(NRD,R,optional,[[F|InitForm]|FormalArgs],[P|Params],[F|Names],[P|PVars],(Init,Code)):- atomic(F),!,compile_init(F,P,InitForm,Init),formal_args(NRD,R,optional,FormalArgs,Params,Names,PVars,Code).
formal_args(NRD,R,optional,[F|FormalArgs],Params,Names,PVars,Code):- !, atomic(F),!,formal_args(NRD,R,optional,[[F,[],[]]|FormalArgs],Params,Names,PVars,Code).

formal_args(NRD,R,aux,[[F|InitForm]|FormalArgs],Params,[F|Names],[P|PVars],(Init,Code)):- atomic(F),!,compile_init(F,P,InitForm,Init),formal_args(NRD,R,aux,FormalArgs,Params,Names,PVars,Code).
formal_args(NRD,R,aux,[F|FormalArgs],Params,Names,PVars,Code):- !, atomic(F),!,formal_args(NRD,R,aux,[[F,[],[]]|FormalArgs],Params,Names,PVars,Code).

formal_args(t,R,key,[ [[KW,F]|InitForm]|FormalArgs],Params,[F|Names],[P|PVars],(kw_obtain_value(R,KW,F,P),Init,Code)):- atomic(F),!,compile_init(F,P,InitForm,Init),formal_args(t,R,key,FormalArgs,Params,Names,PVars,Code).
formal_args(t,R,key,[ [F,InitForm,KWP]|FormalArgs],Params,[F,KWP|Names],[P,KWPV|PVars],(kw_obtain_value(R,F,F,P),kw_is_present(R,F,KWP,KWPV),Init,Code)):- atomic(F),!,compile_init(F,P,InitForm,Init),formal_args(t,R,key,FormalArgs,Params,Names,PVars,Code).
formal_args(t,R,key,[[F|InitForm]|FormalArgs],Params,Names,PVars,Code):- !, atomic(F),!,formal_args(t,R,key,[[[F,F]|InitForm]|FormalArgs],Params,Names,PVars,Code).
formal_args(t,R,key,[F|FormalArgs],Params,Names,PVars,Code):- !, atomic(F),!,formal_args(t,R,key,[[[F,F]]|FormalArgs],Params,Names,PVars,Code).

tfa:- test_formal_args([ item,list,'&key',key,[test, [function, eql], testp],['test-not', [], notp]]).
test_formal_args(TFA):-
  debug_var("Rest",Rest),
  debug_var("Code",Code),
  debug_var("ActualArgs",ActualArgs),
  formal_args(NRD,Rest,required,TFA,ActualArgs,Names,PVars,Code0),
  maplist(debug_var,Names,PVars),
  ignore(NRD=nil),
  dbmsg(formal_args(uses_rest=NRD,Rest,formal=TFA,params=ActualArgs,names=Names,vars=PVars)),
  body_cleanup(Code0,Code),
  dbmsg(:-Code).


% compile_init(F,P,InitForm,Init)
compile_init(Var,FinalResult,InitForm,(set_symbol_value_if_missing('$env',Var,FinalResult,Code,Result))):- lisp_compile(Result,InitForm,Code).
   
% [name, 'package-designator', '&optional', [error, t]]


% Creates a function Head and an argument unpacker
expand_function_head([FunctionName | FormalArgs], Head, ArgBindings, Result,Code):-!,
        formal_args(_NRD,Rest,required,FormalArgs,ActualArgs,Names,PVars,Code),
   debug_var("Rest",Rest),
   debug_var("Code",Code),
   debug_var("ActualArgs",ActualArgs),
        freeze(Arg,debug_var(Arg,Val)),
	zip_with(Names, PVars, [Arg, Val, bv(Arg, [Val|_])]^true, ArgBindings),
	append(ActualArgs, [Result], HeadArgs),
	Head =.. [FunctionName | HeadArgs].
expand_function_head(FunctionName , Head, ArgBindings, Result,Code):-
    expand_function_head([FunctionName], Head, ArgBindings, Result,Code).
/*
expand_function_head([FunctionName | FormalArgs], Head, ArgBindings, Result):-!,
        % freeze(Arg,debug_var(Arg,Val)),
	zip_with(FormalArgs, ActualArgs, [Arg, Val, bv(Arg, [Val|_])]^true, ArgBindings),
	append(ActualArgs, [Result], HeadArgs),
	Head =.. [FunctionName | HeadArgs].
expand_function_head(FunctionName , Head, ArgBindings, Result):-
    expand_function_head([FunctionName], Head, ArgBindings, Result).
*/

% Expands the arguments 
bind_formal_parameters([], [], Env, Env).
bind_formal_parameters([FormalParam|FormalParams], [ActualParam|ActualParams],
		Bindings0, Bindings):- 
	bind_formal_parameters(FormalParams, ActualParams, 
		[bv(FormalParam, [ActualParam|_])|Bindings0], Bindings).


% The idea here is that FunctionName/ArgNum may need evaluated or may have its own special evaluator 
expand_arguments(_Ctx,_Env,_FunctionName,_ArgNum,[], true, []).
expand_arguments(_Ctx,_Env,FunctionName,_, Args, true, Args):- lisp_operator(FunctionName),!.
expand_arguments(Ctx,Env,FunctionName,ArgNum,[Arg|Args], Body, [Result|Results]):-
       must_compile_body(Ctx,Env,Result,Arg, ArgBody),
       Body = (ArgBody, ArgsBody),
       ArgNum2 is ArgNum + 1,
       expand_arguments(Ctx,Env,FunctionName,ArgNum2,Args, ArgsBody, Results).

:- fixup_exports.

