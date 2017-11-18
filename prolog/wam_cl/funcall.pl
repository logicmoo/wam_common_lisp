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

:- include('header.pro').

lisp_env_eval(_Pt1^Body, _Env, _Result):- !,
  call(Body).
lisp_env_eval(Expression, Env, Result):-
  lisp_compile(Env,Result,Expression,Body),
  user:call(Body).

function(X,function(X)).
closure(ClosureEnvironment,ClosureResult,FormalParams,ClosureBody,ActualParams,ClosureResult):-
  must_bind_parameters(ClosureEnvironment,FormalParams, ActualParams,_EnvOut,BinderCode),
  must_or_rtrace(user:BinderCode),
  must_or_rtrace(user:ClosureBody).


cl_funcall([function(F)|More],R):-!,cl_funcall([F|More],R).
cl_funcall([ProcedureName|Args],Result):- env_current(Env),apply_c(Env,ProcedureName, Args, Result).
% cl_funcall([F|More],R):- append([More],[R],ARGS), lpa_apply(F,ARGS).


apply_c(_EnvIns,function, [A],[function,A]).
apply_c(EnvIn,[lambda, FormalParams, Body], ActualParams, Result):-
	!,
	must_bind_parameters(EnvIn,FormalParams, ActualParams,EnvOut,BinderCode),!,
        must_or_rtrace(BinderCode),
	lisp_env_eval(Body, EnvOut, Result),
	!.
apply_c(EnvIn,closure(ClosureEnvironment,ClosureResult,FormalParams,ClosureBody), ActualParams, Result):-
	closure([ClosureEnvironment|EnvIn],ClosureResult,FormalParams,ClosureBody,ActualParams, Result).
    
apply_c(EnvIn,ProcedureName, ActualParams, Result):-
	user:macro_lambda(_Scope,ProcedureName,FormalParams, LambdaExpression,[]),!,
	must_bind_parameters(EnvIn,FormalParams, ActualParams, Env,BinderCode),
        must_or_rtrace(BinderCode),
        lisp_env_eval(LambdaExpression, Env, Result),
	!.
apply_c(Env,ProcedureName, Args, Result):-
	named_lambda(ProcedureName, LambdaExpression),!,
	apply_c(Env,LambdaExpression, Args, Result),
	!.

apply_c(_,F,ARGS,R):- atom(F),append(ARGS,[R],RARGS),length(RARGS,A),current_predicate(F/A),!,apply(F,RARGS),!.
apply_c(_,F,ARGS,R):- atom(F),CALL=..[F|ARGS],current_predicate(_,CALL),!,(catch(CALL,E,(dumpST,dmsg(CALL->E),!,fail))->R=t;R=[]).
apply_c(EnvIn,X, _, R):- ignore(R=[]),
        (debugging(lisp(eval))->dumpST;true),
	write('ERROR!  apply_c apply a procedure description for `'),
	write(X),
	write(''''),nl,
        write('EnvIn'=EnvIn),nl,
        
	!.



:- fixup_exports.

