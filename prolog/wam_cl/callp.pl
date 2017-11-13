/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (builtin_lisp_functions.pl)
 *
 * (c) Neil Smith, 2001
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * This program provides some built-in functionality for the 
 * Lisp compiler.  It requires that the file lisp_compiler.pl has 
 * already been successfully compiled.
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(callp,[]).

:- set_module(class(library)).

:- include('header.pro').


/*
TODO fix prolog_call

tst:is_local_test(block2,[block,block2,[tagbody,setq(b,2),[go,tag2],setq(a,1),(tag1),
                     prolog_call([a,b],plus(a,b,C)),prolog_call(writeln(C)),
                     'return-from'(block2,c),(tag2),setq(a,4),[go,tag1]]],6).
*/

shared_lisp_compiler:plugin_expand_function_body(Ctx,Env,Result,InstrS,Code):- 
  compile_as_prolog(Ctx,Env,Result,InstrS,Code),!.

% (prolog-trace)
compile_prolog_call(_Ctx,_Env,_Result,[u_prolog_trace], trace).

%compile_body_h(_Ctx,_Env,Result, nop(X),  nop(X)):- !, debug_var("_NopResult",Result).

compile_as_prolog(_Ctx,_Env,_Result,call(Body), call(Body) ):-!.
compile_as_prolog(Ctx,Env,ResultOut,prolog_call(Body), call(BodyResolved) ):-
   compile_prolog_call(Ctx,Env,[],ResultOut,Body,BodyResolved),!.
compile_as_prolog(Ctx,Env,ResultOut,prolog_call(Resolve,Body), call(BodyResolved) ):-
   compile_prolog_call(Ctx,Env,Resolve,ResultOut,Body,BodyResolved),!.


:- fixup_exports.

