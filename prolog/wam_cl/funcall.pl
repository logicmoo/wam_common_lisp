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

lisp_env_eval(_Pt1^Body, _Env, _Result):- !,
  always(Body).
lisp_env_eval(Expression, Env, Result):-
  lisp_compile(Env,Result,Expression,Body),
  user:always(Body).

function(X,function(X)).
closure(ClosureEnvironment,ClosureResult,FormalParams,ClosureBody,ActualParams,ClosureResult):-
  must_bind_parameters(ClosureEnvironment,FormalParams, ActualParams,_EnvOut,BinderCode),
  always(user:BinderCode),
  always(user:ClosureBody).

cl_eval(Form,Result):- lisp_compile(Result,Form,Body),always(Body).

wl:init_args(1,cl_funcall).
cl_funcall(function(F),More,R):-!,cl_funcall(F,More,R).
cl_funcall(ProcedureName,Args,Result):- env_current(Env),apply_c(Env,ProcedureName, Args, Result).
% cl_funcall([F|More],R):- append([More],[R],ARGS), lpa_apply(F,ARGS).


apply_c(_EnvIns,function, [A],[function,A]).
apply_c(EnvIn,[lambda, FormalParams, Body], ActualParams, Result):-
	!,
	must_bind_parameters(EnvIn,FormalParams, ActualParams,EnvOut,BinderCode),!,
        always(BinderCode),
	lisp_env_eval(Body, EnvOut, Result),
	!.
apply_c(EnvIn,closure(ClosureEnvironment,ClosureResult,FormalParams,ClosureBody), ActualParams, Result):-
	closure([ClosureEnvironment|EnvIn],ClosureResult,FormalParams,ClosureBody,ActualParams, Result).
    
apply_c(EnvIn,ProcedureName, ActualParams, Result):-
	get_lambda_def(defmacro,ProcedureName,FormalParams, LambdaExpression),!,
	must_bind_parameters(EnvIn,FormalParams, ActualParams, Env,BinderCode),
        always(BinderCode),
        lisp_env_eval(LambdaExpression, Env, Result),
	!.
/*apply_c(Env,ProcedureName, Args, Result):-
	named_lambda(ProcedureName, LambdaExpression),!,
	apply_c(Env,LambdaExpression, Args, Result),
	!.
*/

apply_c(_,F,ARGS,R):- atom(F),append(ARGS,[R],RARGS),length(RARGS,A),current_predicate(F/A),!,apply(F,RARGS),!.
apply_c(_,F,ARGS,R):- atom(F),CALL=..[F|ARGS],current_predicate(_,CALL),!,(catch(CALL,E,(dumpST,dmsg(CALL->E),!,fail))->R=t;R=[]).
apply_c(EnvIn,X, _, R):- ignore(R=[]),
        (debugging(lisp(eval))->dumpST;true),
	write('ERROR!  apply_c apply a procedure description for `'),
	write(X),
	write(''''),nl,
        write('EnvIn'=EnvIn),nl,
        
	!.


:- discontiguous(compile_funop/5).

% Use a previous DEFMACRO
compile_funop(Ctx,Env,Result,LispCode,CompileBody):-
  % fail, %DISABLED
  macroexpand_1_or_fail(LispCode,[],CompileBody0Result),
  must_compile_body(Ctx,Env,Result,CompileBody0Result, CompileBody).

macroexpand_all(LispCode,MacroEnv,Result):-
  macroexpand_1_or_fail(LispCode,MacroEnv,Mid) ->
    macroexpand_all(Mid,MacroEnv,Result) ; Result=LispCode.

get_macro_function(Ctx,Env,Procedure, Arguments,MResult,FnResult,CallBody):- 
       atom(Procedure),
   length(Arguments,ArgsLen),
   find_function_or_macro_name(Ctx,Env,Procedure,ArgsLen, ProposedName),!,
   align_args_or_fallback(Procedure,ProposedName,Arguments,FnResult,ArgsPlusResult),
   ExpandedFunction =.. [ ProposedName | ArgsPlusResult],
   clause(ExpandedFunction,Conj),
   unify_conj(Conj,(CallBody,cl_eval(MResult, FnResult))).

unify_conj(Conj,To):- nonvar(Conj),Conj=To,!.
unify_conj((CA,(CB,CC)),(A,B)):- var(A),nonvar(B),!, unify_conj(((CA,CB),CC),(A,B)).
unify_conj((CA,(CB,CC)), AB):- unify_conj(((CA,CB),CC),AB).

   


macroexpand_1_or_fail([Procedure|Arguments],MacroEnv,MResult):- nonvar(Procedure),   
   get_macro_function(_Ctx,MacroEnv,Procedure, Arguments, MResult, _FnResult, CallBody),!,always(CallBody),!.

macroexpand_1_or_fail([Procedure|Arguments],MacroEnv,CompileBody0Result):- nonvar(Procedure),
   debug_var('MacroEnvArgs',MacroEnv),
   get_lambda_def(defmacro,Procedure, FormalParams, LambdaExpression),!,
   always((debug_var('EnvThru',EnvThru),debug_var('NewEnv',NewEnv),
   debug_var('Env',Env),debug_var('NextEnv',NextEnv),debug_var('CommaResult',CommaResult),
   must_bind_parameters(NewEnv, FormalParams, Arguments,EnvThru,BindCode),!,
   append(_,[],NewEnv),!,
   NextEnv = [NewEnv|Env],  
   always(BindCode),
   always(expand_commas(NewEnv,CommaResult,LambdaExpression,CodeS)),
   body_cleanup_keep_debug_vars(Ctx,CodeS,Code),
   % (local_override(with_forms,lisp_grovel)-> (lisp_dumpST) ; true),
   always(Code),
   must_compile_body(Ctx,NextEnv,CompileBody0Result,CommaResult, MCBR),
   always(MCBR),
   dbmsg_cmt((macroResult(BindCode,Code,CommaResult,CompileBody0Result))))),!.


% macroexpand-1
wl:init_args(0,cl_macroexpand).
cl_macroexpand_1([LispCode|Optionals],Result):- 
  nth_value(Optionals,1,'$env',MacroEnv),
  macroexpand_1_or_fail(LispCode,MacroEnv,R)->push_values([R,t],Result);push_values([LispCode,[]],Result).

% macroexpand
wl:init_args(0,cl_macroexpand_1).
cl_macroexpand([LispCode|Optionals],Result):- 
  nth_value(Optionals,1,'$env',MacroEnv),
  macroexpand_all(LispCode,MacroEnv,R),!,
  (R\==LispCode->push_values([R,t],Result);push_values([R,[]],Result)).



% Operator
expand_arguments_maybe_macro(_Ctx,_CallEnv,FN,_N,FunctionArgs,true, FunctionArgs):- is_lisp_operator(FN),!.
expand_arguments_maybe_macro(Ctx,CallEnv,FN,0,FunctionArgs,ArgBody, Args):-
  expand_arguments(Ctx,CallEnv,FN,0,FunctionArgs,ArgBody, Args).


compile_funop(Ctx,Env,Result,[Op | FunctionArgs], Body):- nonvar(Op),wl:op_replacement(Op,Op2), !,
  must_compile_body(Ctx,Env,Result,[Op2 | FunctionArgs],Body).

% progn mismatch?
compile_funop(Ctx,Env,Result,[FN ], Body):- is_list(FN),!,
  trace,must_compile_body(Ctx,Env,Result,FN,Body).

compile_funop(Ctx,Env,Result,[FN , A| FunctionArgs], Body):- is_list(FN),!,
  must_compile_body(Ctx,Env,Result,[funcall,FN, A | FunctionArgs],Body).

compile_funop(Ctx,Env,Result,[FN | FunctionArgs], Body):- \+ atom(FN),!,
  dumpST,trace,must_compile_body(Ctx,Env,Result,[funcall_obj,FN | FunctionArgs],Body).


compile_funop(Ctx,CallEnv,Result,[FN | FunctionArgs], Body):- nonvar(FN),
      expand_arguments_maybe_macro(Ctx,CallEnv,FN,0,FunctionArgs,ArgBody, Args),
      %debug_var([FN,'_Ret'],Result),      
      find_function_or_macro(Ctx,CallEnv,FN,Args,Result,ExpandedFunction),      
      Body = (ArgBody,ExpandedFunction).


% FUNCTION APPLY
%compile_body(Ctx,Env,Result,['apply',Function|ARGS], Body):- atom(Function)
% FUNCTION FUNCALL
% compile_body(Ctx,Env,Result,['funcall',Function|ARGS], Body):- ...

find_lisp_function(FN,ARITY,ProposedName):-
  find_function_or_macro_name(_Ctx,_Env,FN,ARITY, ProposedName).

find_function_or_macro(Ctx,Env,FN,Args,Result,ExpandedFunction):-
   length(Args,ArgsLen),
   find_function_or_macro_name(Ctx,Env,FN,ArgsLen, ProposedName),!,
   align_args_or_fallback(FN,ProposedName,Args,Result,ArgsPlusResult),
   ExpandedFunction =.. [ ProposedName | ArgsPlusResult].
   
find_function_or_macro_name(_Ctx,_Env,FN,_Len, ProposedName):- 
  get_opv(FN,function,ProposedName),!.
find_function_or_macro_name(_Ctx,_Env,FN,ArgLen, ProposedName):-
  some_defined_function_or_macro(FN,ArgLen,['','cl_','pf_','sf_','mf_','f_'],ProposedName),!.

find_function_or_macro_name(_Ctx,_Env,FN,ArgsLen, ProposedName):- upcase_atom(FN,FN),
    Arity is ArgsLen+1,is_defined(FN,Arity),ProposedName=FN.

find_function_or_macro_name(_Ctx,_Env,FN,_Len, ProposedName):-
    maybe_symbol_package(FN,Package),
    (pl_symbol_name(FN,Name) ->
      function_case_name(Name,Package,ProposedName);
      function_case_name(FN,Package,ProposedName)).


uses_exact(F):-premute_names(F,FF),uses_exact0(FF).

uses_exact0(F):- wl:init_args(exact_only,F),!.
uses_exact0(FN):-  function_arg_info(FN,ArgInfo),!,ArgInfo.complex ==0,ArgInfo.opt==0,ArgInfo.rest==0,ArgInfo.env==0,length(ArgInfo.names,ArgInfo.req).

function_arg_info(FN,ArgInfo):- wl:arglist_info(FN,_,_,_,ArgInfo).
function_arg_info(FN,ArgInfo):- wl:arglist_info(_,FN,_,_,ArgInfo).
% exact_and_restkeys(FN,Requireds):- current_predicate(FN/N), Requireds is N-2,Requireds>0.

exact_and_restkeys(F,N):- premute_names(F,FF), exact_and_restkeys0(FF,N).

exact_and_restkeys0(F,N):- wl:init_args(N,F),integer(N),!.
exact_and_restkeys0(F,_):- uses_exact0(F),!,fail.
exact_and_restkeys0(F,N):- function_arg_info(F,ArgInfo),ArgInfo.req=N,ArgInfo.all\==N,!.
exact_and_restkeys0(F,0):- uses_rest_only0(F),!.


premute_names(F,F).
premute_names(F,FF):- atom_concat_or_rtrace('f_',F,FF).
premute_names(F,FF):- atom_concat_or_rtrace('cl_',F,FF).
premute_names(F,FF):- atom_concat_or_rtrace('f_',FF,F).
premute_names(F,FF):- atom_concat_or_rtrace('cl_',FF,F).

uses_rest_only_p(F):- premute_names(F,FF),uses_rest_only0(FF),!.

uses_rest_only0(F):- wl:init_args(0,F),!.
uses_rest_only0(F):- function_arg_info(F,ArgInfo),ArgInfo.req==0,ArgInfo.all\==0,!.
uses_rest_only0(F):- same_symbol(F,FF),wl:declared(FF,lambda(['&rest'|_])),!.
%uses_rest_only0(F):- wl:init_args(req(0),F),!.

% Non built-in function expands into an explicit function call

% invoke(r1,r2,r3,RET).
align_args(FN,ProposedName,Args,Result,ArgsPlusResult):- 
  (uses_exact(FN);uses_exact(ProposedName)),
   append(Args, [Result], ArgsPlusResult).

% invoke(r1,r2,[o3,key1,value1],RET).
align_args(FN,ProposedName,Args,Result,ArgsPlusResult):- 
  (exact_and_restkeys(FN,N);exact_and_restkeys(ProposedName,N)),
  length(Left,N),append(Left,Rest,Args),
  append(Left, [Rest,Result], ArgsPlusResult).

% invoke([r1,r2,r3],RET).
align_args(FN,ProposedName,Args,Result,[Args,Result]):-
  (uses_rest_only_p(FN);uses_rest_only_p(ProposedName)).


% guess invoke(r1,RET).
%align_args(_FN,ProposedName,[Arg],Result,[Arg,Result]):- 
% is_defined(ProposedName,2),is_defined(ProposedName,3).

% guess invoke([r1,r2,r3],RET).
%align_args(FN,ProposedName,Args,Result,[Args,Result]):- 
%  only_arity(FN,2);only_arity(ProposedName,2).

  
% fallback to invoke([r1,r2,r3],RET).
%align_args(FN, ProposedName,Args,Result,[Args,Result]):- 
%  (is_lisp_operator(FN) ; is_lisp_operator(ProposedName)),!.

/*
% guess invoke(r1,r2,r3,RET).
*/
align_args_or_fallback(FN,ProposedName,Args,Result,ArgsPlusResult):- align_args(FN,ProposedName,Args,Result,ArgsPlusResult),!.
align_args_or_fallback(_,_ProposedName,Args,Result,ArgsPlusResult):- append(Args, [Result], ArgsPlusResult).


only_arity(ProposedName,N):-
  is_defined(ProposedName,N),
  forall((between(0,6,Other),Other\=N),  \+ is_defined(ProposedName,Other)).

is_defined(ProposedName,N):- functor(G,ProposedName,N),current_predicate(_,G).

maybe_symbol_package(Symbol,Package):-  get_opv(Symbol,package,Package),!.
maybe_symbol_package(_Symbol,Package):- reading_package(Package).


some_defined_function_or_macro(FN,ArgLen,[Name|NameS],NewName):-
   atom_concat_or_rtrace(Name,FN,ProposedPName),   
   (((ProposedPName = ProposedName; prologcase_name(ProposedPName,ProposedName)),
    guess_lisp_functor(P,ProposedName,ArgLen),current_predicate(_,P),
    \+ predicate_property(user:P,imported_from(system)))-> ProposedName=NewName;
   some_defined_function_or_macro(FN,ArgLen,NameS,NewName)).

guess_lisp_functor(P,F,ArgLen):- (number(ArgLen)->A is ArgLen+1; A= _),!,guess_prolog_functor(P,F,A). 
guess_prolog_functor(P,F,A):- (var(F);var(A)),!,current_predicate(F/A),functor(P,F,A).
guess_prolog_functor(P,F,A):- functor(P,F,A).



:- fixup_exports.


