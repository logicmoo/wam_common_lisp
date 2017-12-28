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



cl_eval(Form,Result):- lisp_compile(Result,Form,Body),always(Body).

wl:init_args(1,cl_funcall).
cl_funcall(function(F),More,R):-!,cl_funcall(F,More,R).
cl_funcall(ProcedureName,Args,Result):- cl_apply(ProcedureName, [Args], Result).
% cl_funcall([F|More],R):- append([More],[R],ARGS), lpa_apply(F,ARGS).



wl:init_args(1,cl_apply).
cl_apply(closure(Environment,ClosureResult,FormalArgs,Body), [Arguments], Result):-!,
  closure(Environment,ClosureResult,FormalArgs,Body,Arguments,Result).
cl_apply(function(FunctionName), Arguments, Result):-!,cl_apply((FunctionName), Arguments, Result).
cl_apply((FunctionName), [Arguments], Result):-!,
  lisp_compiled_eval([FunctionName|Arguments],Result).



:- discontiguous(compile_funop/5).

compile_funop(Ctx,Env,Result,[function(Op) | FunctionArgs], Body):- nonvar(Op),!,
  must_compile_body(Ctx,Env,Result,[Op | FunctionArgs], Body).

%compile_funop(Ctx,Env,Result,[[quote, Op] | FunctionArgs], Body):- nonvar(Op),!,
%  must_compile_body(Ctx,Env,Result,[Op | FunctionArgs], Body).

% Messed Progn?
% compile_body(Ctx,Env,Result,[Form|MORE], Code):- is_list(Form), !,must_compile_progn(Ctx,Env,Result,[Form|MORE], Code).

 
compile_funop(Ctx,Env,Result,[FN| Args], Code):- var(FN),!,
      compile_apply(Ctx,Env,FN,Args,Result,Code).


% compile_funop(Ctx,Env,Result,[list|Args], Body):- expand_arguments(Ctx,Env,list,1,Args,Result,Body).


% Use a previous DEFMACRO
compile_funop(Ctx,Env,Result,LispCode,CompileBody):-
  fail, %DISABLED
  macroexpand_1_or_fail(LispCode,[],CompileBody0Result),
  dbginfo(macroexpand:-LispCode),
  dbginfo(into:-CompileBody0Result),
  must_compile_body(Ctx,Env,Result,CompileBody0Result, CompileBody),
  !.

compile_funop(Ctx,Env,Result,[Op | FunctionArgs], Body):- nonvar(Op), wl:op_replacement(Op,Op2), !,
  must_compile_body(Ctx,Env,Result,[Op2 | FunctionArgs],Body).

compile_funop(Ctx,Env,Result,[eval , Form],(FormBody,cl_eval(ResultForm,Result))):-!,
   must_compile_body(Ctx,Env,ResultForm,Form,FormBody).

% malformed funcall
compile_funop(Ctx,Env,Result,[funcall, FN| FunctionArgs], Body):- \+ is_list(FunctionArgs),
     compile_funop(Ctx,Env,Result,[apply, FN, [list|FunctionArgs]], Body).

% malformed call
compile_funop(Ctx,Env,Result,[FN| FunctionArgs], Body):- fail,\+ is_list(FunctionArgs),
     compile_funop(Ctx,Env,Result,[apply, [quote, FN], [list|FunctionArgs]], Body).


compile_funop(Ctx,Env,Result,[funcall, FN| FunctionArgs], Body):- 
      must_compile_body(Ctx,Env,F,FN,ArgsBody1),
      var(FN),expand_arguments(Ctx,Env,funcall,2,FunctionArgs,ArgsBody2,Args),
      compile_apply(Ctx,Env,F,Args,Result,Code),
      Body = (ArgsBody1,ArgsBody2,Code).

compile_funop(Ctx,Env,Result,[funcall, FN| FunctionArgs], Body):- 
      must_compile_body(Ctx,Env,F,FN,ArgsBody1),
      expand_arguments(Ctx,Env,F,1,FunctionArgs,ArgsBody2,Args),
      compile_apply(Ctx,Env,F,Args,Result,Code),
      Body = (ArgsBody1,ArgsBody2,Code).

compile_funop(Ctx,Env,Result,[apply, FN, [List,FunctionArg]], Body):- List == list,
      compile_funop(Ctx,Env,Result,[funcall, FN, FunctionArg], Body).

compile_funop(Ctx,Env,Result,[apply, FN, FunctionArgs], (ArgsBody1,ArgsBody2,Body)):- fail, is_list(FunctionArgs),
  must_compile_body(Ctx,Env,F,FN,ArgsBody1),
  expand_arguments(Ctx,Env,FN,1,FunctionArgs,ArgsBody2,Args),
  compile_apply(Ctx,Env,F,Args,Result,Body).

compile_funop(Ctx,Env,Result,[apply, FN, FunctionArgs], (ArgsBody1,Body)):- fail, var(FunctionArgs),
  must_compile_body(Ctx,Env,F,FN,ArgsBody1),
  compile_apply(Ctx,Env,F,FunctionArgs,Result,Body).

compile_funop(Ctx,Env,Result,[apply, FN| FunctionArgs], (ArgsBody1,Body)):- fail, var(FunctionArgs),
   must_compile_body(Ctx,Env,F,FN,ArgsBody1),
    compile_apply(Ctx,Env,F,FunctionArgs,Result,Body).

compile_funop(Ctx,Env,Result,[apply, FN, FunctionArgs], (ArgsBody1,ArgsBody2,Body)):- fail,
  must_compile_body(Ctx,Env,F,FN,ArgsBody1),
  expand_arguments(Ctx,Env,FN,1,FunctionArgs,ArgsBody2,Args),
  compile_apply(Ctx,Env,F,Args,Result,Body).

compile_funop(Ctx,Env,Result,[apply, FN| FunctionArgs], (ArgsBody2, Body)):- 
  %must_compile_body(Ctx,Env,F,FN,ArgsBody1),
  expand_arguments(Ctx,Env,FN,1,FunctionArgs,ArgsBody2,Args),
  compile_apply(Ctx,Env,FN,Args,Result,Body).

compile_funop(Ctx,CallEnv,Result,[FN | FunctionArgs], Body):-
      expand_arguments_maybe_macro(Ctx,CallEnv,FN,0,[FN|FunctionArgs],ArgsBody,[FNC|Args]),
      compile_apply(Ctx,CallEnv,FNC,Args,Result,ExpandedFunction),
      Body = (ArgsBody,ExpandedFunction).

% TODO- HOW DID WE GET HERE?
compile_funop(_Ctx,_Env,Result,[FN | FunctionArgs],cl_eval([FN|FunctionArgs],Result)). 


compile_apply_function_or_macro_call(Ctx,Env,FN,Args,Result,ExpandedFunction):-
   (is_list(Args)->length(Args,ArgsLen);true),
   foc_operator(Ctx,Env,FN,ArgsLen, ProposedName),!,
   align_args_or_fallback(FN,ProposedName,Args,Result,ArgsPlusResult),
   ExpandedFunction =.. [ ProposedName | ArgsPlusResult].



make_apply_list(ArgLast,_):-var(ArgLast),!,fail.
make_apply_list([ArgLast],ArgLast):-!,is_list(ArgLast),!.
make_apply_list([A|FunctionArgs],[A|FunctionArgsCorrect]):-
  make_apply_list(FunctionArgs,FunctionArgsCorrect).


compile_apply(Ctx,Env,F,Args,Result,ExpandedFunction):- atom(F),once((append(Left,[List],Args),eval_uses_exact_and_restkeys(F,N))),
 length(Left,N),
 append(Left,[List,Result],NewArgs),
 foc_operator(Ctx,Env,F,_, ProposedName),!,
 ExpandedFunction =.. [ ProposedName | NewArgs].

compile_apply(_Ctx,_Env,F,Args,Result,cl_apply(F,Args,Result)):- (var(F); \+ is_list(Args)),!.

compile_apply(Ctx,Env,F,Args,Result,ExpandedFunction):- atom(F),
 compile_apply_function_or_macro_call(Ctx,Env,F,Args,Result,ExpandedFunction),!.

compile_apply(Ctx,Env,function(F),Args,Result,ExpandedFunction):- atom(F),
 compile_apply(Ctx,Env,F,Args,Result,ExpandedFunction),!.

%compile_apply(_Ctx,_Env,cl_array_row_major_index, [Array_Get12, Subscripts_Get],Result,ExpandedFunction):-
% ExpandedFunction =.. [ cl_array_row_major_index ,Array_Get12, Subscripts_Get,Result].


compile_apply(_Ctx,_Env,F,Args,Result,cl_apply(F,Args,Result)).


  
   
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

closure(ClosureEnvironment,ClosureResult,FormalParams,ClosureBody,ActualParams,ClosureResult):-
  M = closure(ClosureEnvironment,ClosureResult,FormalParams,ClosureBody,ActualParams,ClosureResult),
  del_attrs_of(M,dif),
  del_attrs_of(M,vn),
  arglists:must_bind_parameters(ClosureEnvironment,_RestNKeys,FormalParams, ActualParams,_EnvOut, BinderCode),
  always(user:BinderCode),
  always(user:ClosureBody).

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
apply_c(_,F,ARGS,R):- atom(F),CALL=..[F|ARGS],current_predicate(_,CALL),!,(catch(CALL,E,(dumpST,dbginfo(CALL->E),!,fail))->R=t;R=[]).
apply_c(EnvIn,X, _, R):- ignore(R=[]),
        (debugging(lisp(eval))->dumpST;true),
	write('ERROR!  apply_c apply a procedure description for `'),
	write(X),
	write(''''),nl,
        write('EnvIn'=EnvIn),nl,
        
	!.


:- fixup_exports.


