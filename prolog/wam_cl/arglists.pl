/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (symbol_places.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(arglists, []).
:- set_module(class(library)).
:- include('header.pro').

bind_formal_parameters(Formal, Actual, Bindings):-
	env_bind_variables(Formal, Actual, [], Bindings).

% Creates a function Head and an argument unpacker
expand_function_head([FunctionName | FormalArgs], Head, ArgBindings, Result):-!,
        freeze(Arg,debug_var(Arg,Val)),
	zip_with(FormalArgs, ActualArgs, [Arg, Val, bv(Arg, [Val|_])]^true, ArgBindings),
	append(ActualArgs, [Result], HeadArgs),
	Head =.. [FunctionName | HeadArgs].
expand_function_head(FunctionName , Head, ArgBindings, Result):-
    expand_function_head([FunctionName], Head, ArgBindings, Result).


% Expands the arguments 
bind_formal_parameters([], [], Env, Env).
bind_formal_parameters([FormalParam|FormalParams], [ActualParam|ActualParams],
		Bindings0, Bindings):- 
	bind_formal_parameters(FormalParams, ActualParams, 
		[bv(FormalParam, [ActualParam|_])|Bindings0], Bindings).


% The idea here is that FunctionName/ArgNum may need evaluated or may have its own special evaluator 
expand_arguments(_Ctx,_Env,_FunctionName,_ArgNum,[], true, []).
expand_arguments(Ctx,Env,FunctionName,ArgNum,[Arg|Args], Body, [Result|Results]):-
       must_compile_body(Ctx,Env,Result,Arg, ArgBody),
       Body = (ArgBody, ArgsBody),
       ArgNum2 is ArgNum + 1,
       expand_arguments(Ctx,Env,FunctionName,ArgNum2,Args, ArgsBody, Results).

:- fixup_exports.

