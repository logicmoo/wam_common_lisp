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


lisp_operator(defpackage).
lisp_operator(if).

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

