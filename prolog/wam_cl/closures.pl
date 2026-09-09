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
:- include('./header').



must_compile_closure_body(Ctx,Env,Result,Function, Body):-
  must_compile_body(Ctx,Env,Result,Function, Body0),
  body_cleanup_keep_debug_vars(Ctx,Body0,Body).

% =============================================================================
% = LAMBDA/CLOSURES = 
% =============================================================================
 :- discontiguous compile_closures/5.

% ((function (lambda ... ) ...)
compile_closures(Ctx,Env,Result,[Closure|ActualParams], Body):- p_or_s(Closure,function,[Arg1]),is_list(Arg1),
  Arg1 = [lambda,FormalParms| LambdaBody],
  compile_closures(Ctx,Env,Result,[[lambda,FormalParms| LambdaBody]|ActualParams], Body).

% (function (lambda ... ))
compile_closures(Ctx,Env,Result,Closure, Body):- p_or_s(Closure,function,[Arg1]),is_list(Arg1),
  Arg1 = [lambda,FormalParms| LambdaBody],
  compile_closures(Ctx,Env,Result,[lambda,FormalParms| LambdaBody], Body).



% (function .)
compile_closures(Ctx,Env,Result,Closure, Pre):- p_or_s(Closure,function,[Symbol]), assertion(nonvar(Symbol)),
  find_operator_else_function(Ctx,Env,kw_function,Symbol,Result,Pre),!.

% (lambda ...)
compile_closures(Ctx,Env,Result,[lambda,FormalParms|LambdaBody], Body):- Symbol=[lambda,FormalParms|LambdaBody],!,
   make_bind_parameters(Ctx,Env,FormalParms,Whole,ActualParams,ClosureEnvironment,BinderCode),
   ActualParams = Whole,
   must_compile_closure_body(Ctx,ClosureEnvironment,ClosureResult,[progn|LambdaBody],  ClosureBody),
   debug_var('LArgs',FormalParms),debug_var('LResult',ClosureResult),debug_var('LambdaResult',Result),
   debug_var('ClosureEnvironment',ClosureEnvironment),debug_var('Whole',Whole),debug_var('Symbol',Symbol),
   Result = closure(kw_function,closure_environment(ClosureEnvironment,Env),
                    Whole,ClosureResult,FormalParms,(BinderCode,ClosureBody),Symbol),
   Body = true.


wl:init_args(1, lambda).
%:- set_opv(lambda, symbol_function, sf_lambda).
sf_lambda(ReplEnv, FormalParms, LambdaBody, Result) :- break,
  compile_closures(ReplEnv,ReplEnv,Result,[lambda,FormalParms|LambdaBody], Body),
  break,always(Body).



% ((function .) ...)
compile_closures(Ctx,Env,Result,[Closure|ActualParams],(Pre,Body)):- p_or_s(Closure,function,[Symbol]), assertion(nonvar(Symbol)),
  find_operator_else_function(Ctx,Env,kw_function,Symbol,FResult,Pre),Closure\==FResult,!,
  must_compile_body(Ctx,Env,Result,[FResult|ActualParams],Body).

% ((lambda ...) ...)
compile_closures(Ctx,Env,Result,[Closure|ActualParams],Body):- 
   p_or_s(Closure,lambda,[FormalParms|LambdaBody]),!,
   must_compile_body(Ctx,Env,Result,[destructuring_bind,FormalParms,[list|ActualParams]|LambdaBody],Body).
   
   /*

   must_compile_closure_body(Ctx,ClosureEnvironment,ClosureResult,[progn|LambdaBody],  ClosureBody),
   compile_closures(Ctx,Env,Result,
     closure(kw_function,[ClosureEnvironment|Env],[Symbol|ActualParams],ClosureResult,FormalParms,ClosureBody,Symbol,ActualParams,Result),Body).
*/
% ((closure ...) ...)
compile_closures(Ctx,Env,Result,[Closure|ActualParams],Body):- 
   p_or_s(Closure,closure,[FType,ClosureEnvironment,Whole,ClosureResult,FormalParms,ClosureBody,Symbol]),   
   compile_closures(Ctx,Env,Result,
      closure(FType,[ClosureEnvironment|Env],Whole,ClosureResult,FormalParms,ClosureBody,Symbol,ActualParams,Result),Body).

% Prolog closure
compile_closures(_Ctx,_Env,Result,Closure,(Result=Closure)):- compound(Closure),functor(Closure,closure,_).
  

% Complete (closure ...)
compile_closures(Ctx,Env,ResultO,Closure,(ArgsBody,BinderCode,ClosureBody)):-
  p_or_s(Closure,closure,[FType,ClosureEnvironment,Whole,Result,FormalParams,ClosureBody,Symbol,ActualParams,Result]),   
  ignore(Whole = ActualParams),
   (FType==kw_function -> expand_arguments_maybe_macro(Ctx,Env,funcall,1,Params,ActualParams, ArgsBody);
   (FType==kw_macro -> (Params=ActualParams, ArgsBody = f_eval(Result,ResultO));
     true -> Params=ActualParams, ArgsBody = true,  =(Result,ResultO))), 
  WholeVar = Whole,
  must(make_bind_parameters(WholeVar,ClosureEnvironment,Whole,FormalParams,Symbol,Params,_EnvOut, BinderCode)),!.
  

% Incomplete (closure .) 
compile_closures(_Ctx,Env,Result,Closure,Body):- 
   p_or_s(Closure,closure,[FType,ClosureEnvironment,Whole,ClosureResult,FormalParams,ClosureBody,Symbol]),
   Result = closure(FType,[ClosureEnvironment|Env],Whole,ClosureResult,FormalParams,ClosureBody,Symbol), 
   Body = true.  



closure(kw_function,EnvironmentSpec,Whole,Result,_FormalParms,ClosureBody,_Symbol,Params,ResultOut):-
  closure_environment_parts(EnvironmentSpec,_ActivationEnvironment,OuterEnvironment),
  ResultBox=closure_result(no_result),
  ( once(( always(Whole=Params),
           always(ClosureBody),
           preserve_closure_result(ResultBox,Result,Params,
                                   OuterEnvironment,ClosureBody)
         )),
    fail
  ; true
  ),
  arg(1,ResultBox,ResultDescriptor),
  restore_closure_result(ResultDescriptor,Params,OuterEnvironment,ResultOut).

closure_environment_parts(closure_environment(ActivationEnvironment,OuterEnvironment),
                          ActivationEnvironment,OuterEnvironment):-!.
closure_environment_parts(ActivationEnvironment,ActivationEnvironment,[]).

preserve_closure_result(ResultBox,Result,Params,_OuterEnvironment,_ClosureBody):-
  nth0(Index,Params,Parameter),
  same_term(Result,Parameter),!,
  nb_setarg(1,ResultBox,parameter(Index)).
preserve_closure_result(ResultBox,Result,_Params,OuterEnvironment,_ClosureBody):-
  closure_environment_binding(OuterEnvironment,Name,Captured),
  same_term(Result,Captured),!,
  nb_setarg(1,ResultBox,captured(Name)).
preserve_closure_result(ResultBox,Result,_Params,_OuterEnvironment,ClosureBody):-
  closure_body_global_source(ClosureBody,Result,Name),!,
  nb_setarg(1,ResultBox,global(Name)).
preserve_closure_result(ResultBox,Result,_Params,_OuterEnvironment,_ClosureBody):-
  nb_setarg(1,ResultBox,value(Result)).

restore_closure_result(parameter(Index),Params,_OuterEnvironment,Result):-
  nth0(Index,Params,Result).
restore_closure_result(captured(Name),_Params,OuterEnvironment,Result):-
  get_var(OuterEnvironment,Name,Result).
restore_closure_result(global(Name),_Params,_OuterEnvironment,Result):-
  get_var(Name,Result).
restore_closure_result(value(Result),_Params,_OuterEnvironment,Result).

closure_environment_binding(bv(Name,Value),Name,Value).
closure_environment_binding([Head|_],Name,Value):-
  nonvar(Head),
  closure_environment_binding(Head,Name,Value).
closure_environment_binding([_|Tail],Name,Value):-
  nonvar(Tail),
  closure_environment_binding(Tail,Name,Value).

closure_body_global_source(get_var(_,Name,Source),Result,Name):-
  atom(Name),
  same_term(Result,Source).
closure_body_global_source(Term,Result,Name):-
  compound(Term),
  compound_name_arguments(Term,_,Arguments),
  member(Argument,Arguments),
  nonvar(Argument),
  closure_body_global_source(Argument,Result,Name).


% Called by Incomplete Closures (Lambdas)
closure(FType,ClosureEnvironment,Whole,Result,FormalParms,ClosureBody,Symbol,Params,ResultO):-   
  (FType==kw_function -> (expand_arguments_maybe_macro(Ctx,_Env,funcall,1,Params,ActualParams, ArgsBody),
                          PRECALL=ignore(Whole = [Symbol|ActualParams]),Result = ResultO);
  (FType==kw_macro -> (Params=ActualParams,ignore(Whole = [Symbol|ActualParams]),ArgsBody = f_eval(Result,ResultO),PRECALL=true);
    true -> (Params=ActualParams, ArgsBody = true,Result=ResultO,PRECALL=ignore(Whole = [Symbol|ActualParams]))
    )),   
  M = closure(kw_function,ClosureEnvironment,ClosureResult,FormalParms,ClosureBody,ActualParams,ClosureResult),
  del_attrs_of(M,dif), del_attrs_of(M,vn),
  make_bind_parameters(Ctx,ClosureEnvironment,FormalParms,Whole,Params,_EnvOut,BinderCode),  
  
  always(user:ArgsBody),
  always(PRECALL),
  always(user:BinderCode),  
  always(user:ClosureBody).



apply_c(_EnvIns,function, [A],[function,A]).
apply_c(EnvIn,[lambda, FormalParms| Body], ActualParams, Result):-
        Symbol = [lambda, FormalParms|Body],
	!,        
	make_bind_parameters(EnvIn,EnvIn,FormalParms,Whole,ActualParams,EnvOut,BinderCode),
        ignore(Whole = [Symbol|ActualParams]),
        break,always(BinderCode),
	f_sys_env_eval(EnvOut, Body, Result),
	!.
apply_c(EnvIn,closure(FType,ClosureEnvironment,Whole,ClosureResult,Symbol,FormalParms,ClosureBody), ActualParams, Result):-
	closure(FType,[ClosureEnvironment|EnvIn],Whole,ClosureResult,FormalParms,Symbol,ClosureBody,ActualParams, Result).
    
apply_c(EnvIn, ProcedureName, ActualParams, Result):-
        get_lambda_def(EnvIn,EnvIn,defmacro,ProcedureName,FormalParms, LambdaExpression),!,
	break,make_bind_parameters(EnvIn,EnvIn,FormalParms,Whole,ActualParams,EnvOut,BinderCode),
        ignore(Whole = [ProcedureName|ActualParams]),
        always(BinderCode),
        f_sys_env_eval(EnvOut,LambdaExpression, Result),
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
