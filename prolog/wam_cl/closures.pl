/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (lisp_compiler.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 * Changes since 2001:
 *
 *
 *******************************************************************/
:- module(cl0z3rs, []).
:- set_module(class(library)).
:- include('header').



must_compile_closure_body(Ctx,Env,Result,Function, Body):-
  must_compile_body(Ctx,Env,Result,Function, Body0),
  body_cleanup_keep_debug_vars(Ctx,Body0,Body).

% =============================================================================
% = LAMBDA/CLOSURES = 
% =============================================================================

% ((function (lambda ... ) ...)
compile_closures(Ctx,Env,Result,[Closure|ActualParams], Body):- p_or_s(Closure,function,[List]),is_list(List),
  List = [lambda,FormalParams| LambdaBody],
  compile_closures(Ctx,Env,Result,[[lambda,FormalParams| LambdaBody]|ActualParams], Body).

% (function (lambda ... ))
compile_closures(Ctx,Env,Result,Closure, Body):- p_or_s(Closure,function,[List]),is_list(List),
  List = [lambda,FormalParams| LambdaBody],
  compile_closures(Ctx,Env,Result,[lambda,FormalParams| LambdaBody], Body).

% (function .)
compile_closures(Ctx,Env,Result,Closure, Pre):- p_or_s(Closure,function,[Symbol]), assertion(nonvar(Symbol)),
  find_operator_else_function(Ctx,Env,kw_function,Symbol,Result,Pre),!.

% (lambda ...)
compile_closures(Ctx,Env,Result,[lambda,FormalParams|LambdaBody], Body):- Symbol=[lambda,FormalParams|LambdaBody],!,
   must_compile_closure_body(Ctx,ClosureEnvironment,ClosureResult,[progn|LambdaBody],  ClosureBody),
   debug_var('LArgs',FormalParams),debug_var('LResult',ClosureResult),debug_var('LambdaResult',Result),
   debug_var('ClosureEnvironment',ClosureEnvironment),debug_var('Whole',Whole),debug_var('Symbol',Symbol),
   compile_closures(Ctx,Env,Result,closure(kw_function,ClosureEnvironment,Whole,ClosureResult,FormalParams,ClosureBody,Symbol),Body).

% ((function .) ...)
compile_closures(Ctx,Env,Result,[Closure|ActualParams],(Pre,Body)):- p_or_s(Closure,function,[Symbol]), assertion(nonvar(Symbol)),
  find_operator_else_function(Ctx,Env,kw_function,Symbol,FResult,Pre),
  must_compile_body(Ctx,Env,Result,[FResult|ActualParams],Body).

% ((lambda ...) ...)
compile_closures(Ctx,Env,Result,[Closure|ActualParams],Body):- 
   p_or_s(Closure,lambda,[FormalParams|LambdaBody]),Symbol=[lambda,FormalParams|LambdaBody],
   must_compile_closure_body(Ctx,ClosureEnvironment,ClosureResult,[progn|LambdaBody],  ClosureBody),
   compile_closures(Ctx,Env,Result,
     closure(kw_function,[ClosureEnvironment|Env],[Symbol|ActualParams],ClosureResult,FormalParams,ClosureBody,Symbol,ActualParams,Result),Body).

% ((closure ...) ...)
compile_closures(Ctx,Env,Result,[Closure|ActualParams],Body):- 
   p_or_s(Closure,closure,[FType,ClosureEnvironment,Whole,ClosureResult,FormalParams,ClosureBody,Symbol]),   
   compile_closures(Ctx,Env,Result,
      closure(FType,[ClosureEnvironment|Env],Whole,ClosureResult,FormalParams,ClosureBody,Symbol,ActualParams,Result),Body).

% Complete (closure ...)
compile_closures(Ctx,Env,ResultO,Closure,(ArgsBody,BinderCode,ClosureBody)):-
  p_or_s(Closure,closure,[FType,ClosureEnvironment,Whole,Result,FormalParams,ClosureBody,Symbol,ActualParams,Result]),
   ignore(Whole = [Symbol|ActualParams]),
   (FType==kw_function -> expand_arguments_maybe_macro(Ctx,Env,funcall,1,Params,ActualParams, ArgsBody);
   (FType==kw_macro -> (Params=ActualParams, ArgsBody = f_eval(Result,ResultO));
     true -> Params=ActualParams, ArgsBody = true,  =(Result,ResultO))), 
  must_bind_parameters(ClosureEnvironment,Whole,_RestNKeys,FormalParams,Symbol,Params,_EnvOut, BinderCode),!.
  

% Incomplete (closure .) 
compile_closures(_Ctx,Env,Result,Closure,Body):- 
   p_or_s(Closure,closure,[FType,ClosureEnvironment,Whole,ClosureResult,FormalParams,ClosureBody,Symbol]),
   Result = closure(FType,[ClosureEnvironment|Env],Whole,ClosureResult,FormalParams,ClosureBody,Symbol), 
   Body = true.  

% Called by Incomplete Closures (Lambdas)
closure(FType,ClosureEnvironment,Whole,Result,FormalParams,ClosureBody,Symbol,ActualParams,ResultO):-  
  (FType==kw_function -> expand_arguments_maybe_macro(_Ctx,_Env,funcall,1,Params,ActualParams, ArgsBody);
  (FType==kw_macro -> (Params=ActualParams, ArgsBody = f_eval(Result,ResultO));
    true -> Params=ActualParams, ArgsBody = true,  =(Result,ResultO))),   
  M = closure(kw_function,ClosureEnvironment,ClosureResult,FormalParams,ClosureBody,ActualParams,ClosureResult),
  del_attrs_of(M,dif), del_attrs_of(M,vn),
  must_bind_parameters(ClosureEnvironment,Whole,_RestNKeys,FormalParams,Symbol,Params,_EnvOut, BinderCode),
  ignore(Whole = [Symbol|ActualParams]),
  always(user:ArgsBody),
  always(user:BinderCode),
  always(user:ClosureBody).



apply_c(_EnvIns,function, [A],[function,A]).
apply_c(EnvIn,[lambda, FormalParams| Body], ActualParams, Result):-
        Symbol = [lambda, FormalParams|Body],
	!,        
	must_bind_parameters(EnvIn,Whole,_RestNKeys,FormalParams,Symbol, ActualParams,EnvOut,BinderCode),!,
        ignore(Whole = [Symbol|ActualParams]),
        always(BinderCode),
	f_sys_env_eval(EnvOut, Body, Result),
	!.
apply_c(EnvIn,closure(FType,ClosureEnvironment,Whole,ClosureResult,Symbol,FormalParams,ClosureBody), ActualParams, Result):-
	closure(FType,[ClosureEnvironment|EnvIn],Whole,ClosureResult,FormalParams,Symbol,ClosureBody,ActualParams, Result).
    
apply_c(EnvIn, ProcedureName, ActualParams, Result):-
        Whole = [ProcedureName|ActualParams],
	get_lambda_def(defmacro,ProcedureName,FormalParams, LambdaExpression),!,
	must_bind_parameters(EnvIn,Whole,_RestNKeys,FormalParams,ProcedureName, ActualParams, Env,BinderCode),
        always(BinderCode),
        f_sys_env_eval(Env,LambdaExpression, Result),
	!.
/*apply_c(Env,ProcedureName, Args, Result):-
	named_lambda(ProcedureName, LambdaExpression),!,
	apply_c(Env,LambdaExpression, Args, Result),
	!.
*/

apply_c(_,F,ActualParams,R):- atom(F),append(ActualParams,[R],RARGS),always(length(RARGS,A)),current_predicate(F/A),!,apply(F,RARGS),!.
apply_c(_,F,ActualParams,R):- atom(F),CALL=..[F|ActualParams],current_predicate(_,CALL),!,(catch(CALL,E,(dumpST,dbginfo(CALL->E),!,fail))->R=t;R=[]).
apply_c(EnvIn,X, _, R):- ignore(R=[]),
        (debugging(lisp(eval))->dumpST;true),
	write('ERROR!  apply_c apply a procedure description for `'),
	write(X),
	write(''''),nl,
        write('EnvIn'=EnvIn),nl,
        
	!.

:- fixup_exports.

