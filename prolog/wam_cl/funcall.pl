/*******************************************************************
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
 * The program is a *HUGE* common-lisp compiler/interpreter. 
 *
 *******************************************************************/
:- module(funcall, []).

:- set_module(class(library)).

:- include('header').



:- discontiguous(compile_funop/5).

compile_funop(Ctx,Env,Result,[function(Op) | FunctionArgs], Body):- nonvar(Op),!,
  must_compile_body(Ctx,Env,Result,[Op | FunctionArgs], Body).

%compile_funop(Ctx,Env,Result,[[quote, Op] | FunctionArgs], Body):- nonvar(Op),!,
%  must_compile_body(Ctx,Env,Result,[Op | FunctionArgs], Body).

% Messed Progn?
% compile_body(Ctx,Env,Result,[Form|MORE], Code):- is_list(Form), !,compile_forms(Ctx,Env,Result,[Form|MORE], Code).


% Use a previous DEFMACRO
compile_funop(Ctx,Env,Result,LispCode,CompileBody):-
  fail, %DISABLED
  macroexpand_1_or_fail(LispCode,[],CompileBody0Result),
  lmsg(macroexpand:-LispCode),
  lmsg(into:-CompileBody0Result),
  must_compile_body(Ctx,Env,Result,CompileBody0Result, CompileBody),
  lmsg(code:-CompileBody),
  !.
 
compile_funop(Ctx,Env,Result,[Op | FunctionArgs], Body):- nonvar(Op),wl:op_replacement(Op,Op2), !,
  must_compile_body(Ctx,Env,Result,[Op2 | FunctionArgs],Body).

compile_funop(Ctx,Env,Result,[eval , Form],(FormBody,cl_eval(ResultForm,Result))):-!,
   must_compile_body(Ctx,Env,ResultForm,Form,FormBody).

compile_funop(Ctx,Env,Result,[funcall, FN| FunctionArgs], Body):- as_lisp_funcallable(FN,FNC),!,
  must_compile_body(Ctx,Env,Result,[ FNC | FunctionArgs], Body).

compile_funop(Ctx,Env,Result,[apply, FN, FunctionArgs], Body):- is_list(FunctionArgs), as_lisp_funcallable(FN,FNC),!,
  must_compile_body(Ctx,Env,Result,[ FNC | FunctionArgs], Body).

compile_funop(Ctx,CallEnv,Result,[FN | FunctionArgs], Body):- is_list(FunctionArgs), as_lisp_funcallable(FN,FNC),!,
      expand_arguments_maybe_macro(Ctx,CallEnv,FNC,0,FunctionArgs,ArgBody, Args),
      %debug_var([FN,'_Ret'],Result),      
      make_function_or_macro_call(Ctx,CallEnv,FNC,Args,Result,ExpandedFunction),      
      Body = (ArgBody,ExpandedFunction).

% TODO- HOW DID WE GET HERE?
compile_funop(_Ctx,_Env,Result,[FN | FunctionArgs],cl_eval([FN|FunctionArgs],Result)). 

  
   
/*
% progn mismatch?
compile_funop(Ctx,Env,Result,[FN ], Body):- is_list(FN),!,
  trace,must_compile_body(Ctx,Env,Result,FN,Body).

compile_funop(Ctx,Env,Result,[FN | FunctionArgs], Body):- 
   show_call(must_compile_body(Ctx,Env,Result,[eval,[FN| FunctionArgs]],Body)).
*/





lisp_env_eval(_Env, _Pt1^Body, _Result):- !,
  always(Body).
lisp_env_eval(Env, Expression, Result):-
  lisp_compile(Env,Result,Expression,Body),
  user:always(Body).

function(X,function(X)).
closure(ClosureEnvironment,ClosureResult,FormalParams,ClosureBody,ActualParams,ClosureResult):-
  M = closure(ClosureEnvironment,ClosureResult,FormalParams,ClosureBody,ActualParams,ClosureResult),
  del_attrs_of(M,dif),
  del_attrs_of(M,vn),
  arglists:must_bind_parameters(ClosureEnvironment,_RestNKeys,FormalParams, ActualParams,_EnvOut, BinderCode),
  always(user:BinderCode),
  always(user:ClosureBody).

cl_eval(Form,Result):- lisp_compile(Result,Form,Body),always(Body).

wl:init_args(1,cl_funcall).
cl_funcall(function(F),More,R):-!,cl_funcall(F,More,R).
cl_funcall(ProcedureName,Args,Result):- cl_apply(ProcedureName, [Args], Result).
% cl_funcall([F|More],R):- append([More],[R],ARGS), lpa_apply(F,ARGS).


apply_c(_EnvIns,function, [A],[function,A]).
apply_c(EnvIn,[lambda, FormalParams, Body], ActualParams, Result):-
	!,
	must_bind_parameters(EnvIn,_RestNKeys,FormalParams, ActualParams,EnvOut,BinderCode),!,
        always(BinderCode),
	lisp_env_eval(EnvOut, Body, Result),
	!.
apply_c(EnvIn,closure(ClosureEnvironment,ClosureResult,FormalParams,ClosureBody), ActualParams, Result):-
	closure([ClosureEnvironment|EnvIn],ClosureResult,FormalParams,ClosureBody,ActualParams, Result).
    
apply_c(EnvIn, ProcedureName, ActualParams, Result):-
	get_lambda_def(defmacro,ProcedureName,FormalParams, LambdaExpression),!,
	must_bind_parameters(EnvIn,_RestNKeys,FormalParams, ActualParams, Env,BinderCode),
        always(BinderCode),
        lisp_env_eval(Env,LambdaExpression, Result),
	!.
/*apply_c(Env,ProcedureName, Args, Result):-
	named_lambda(ProcedureName, LambdaExpression),!,
	apply_c(Env,LambdaExpression, Args, Result),
	!.
*/

apply_c(_,F,ARGS,R):- atom(F),append(ARGS,[R],RARGS),always(length(RARGS,A)),current_predicate(F/A),!,apply(F,RARGS),!.
apply_c(_,F,ARGS,R):- atom(F),CALL=..[F|ARGS],current_predicate(_,CALL),!,(catch(CALL,E,(dumpST,lmsg(CALL->E),!,fail))->R=t;R=[]).
apply_c(EnvIn,X, _, R):- ignore(R=[]),
        (debugging(lisp(eval))->dumpST;true),
	write('ERROR!  apply_c apply a procedure description for `'),
	write(X),
	write(''''),nl,
        write('EnvIn'=EnvIn),nl,
        
	!.


:- fixup_exports.


