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
:- module(funop, []).

:- set_module(class(library)).

:- include('header.pro').

:- discontiguous(compile_funop/5).

% Use a previous DEFMACRO
compile_funop(Ctx,Env,RResult,[Procedure|Arguments],CompileBodyCode):- nonvar(Procedure),
  user:macro_lambda(_Scope,Procedure, FormalParams, LambdaExpression,_),!,

  must_bind_parameters(NewEnv, FormalParams, Arguments,BindCode),!,
  append(_,[],NewEnv),!,
  NextEnv = [NewEnv|Env],  
  call(BindCode),
  must_or_rtrace(expand_commas(NewEnv,CommaResult,LambdaExpression,CodeS)),
  body_cleanup_keep_debug_vars(Ctx,CodeS,Code),
  dbmsg(comment(macroResult(BindCode,Code,CommaResult))),
  (local_override(with_forms,lisp_grovel)-> (lisp_dumpST) ; true),
  call(Code),
  must_compile_body(Ctx,NextEnv,CompileBody0Result,CommaResult, MCBR),
  call(MCBR),
  must_compile_body(Ctx,NextEnv,RResult,CompileBody0Result, CompileBody),
  CompileBodyCode = (CompileBody).


:- dynamic(op_replacement/2).
/*
op_replacement(+,plus).
op_replacement(-,minus).
op_replacement(*,mult).
op_replacement(<,lessThan).
op_replacement(>,greaterThan).
*/

% Operator
expand_arguments_maybe_macro(_Ctx,_CallEnv,FunctionName,_N,FunctionArgs,true, FunctionArgs):- is_lisp_operator(FunctionName),!.
expand_arguments_maybe_macro(Ctx,CallEnv,FunctionName,0,FunctionArgs,ArgBody, Args):-
  expand_arguments(Ctx,CallEnv,FunctionName,0,FunctionArgs,ArgBody, Args).


compile_funop(Ctx,Env,Result,[Op | FunctionArgs], Body):- nonvar(Op),user:op_replacement(Op,Op2), !,
  must_compile_body(Ctx,Env,Result,[Op2 | FunctionArgs],Body).


compile_funop(Ctx,Env,Result,[FunctionName ], Body):- is_list(FunctionName),!,
  must_compile_body(Ctx,Env,Result,FunctionName,Body).

compile_funop(Ctx,Env,Result,[FunctionName , A| FunctionArgs], Body):- is_list(FunctionName),!,
  must_compile_body(Ctx,Env,Result,[funcall,FunctionName, A | FunctionArgs],Body).

compile_funop(Ctx,Env,Result,[FunctionName | FunctionArgs], Body):- \+ atom(FunctionName),!,
  dumpST,trace,must_compile_body(Ctx,Env,Result,[funcall_obj,FunctionName | FunctionArgs],Body).


compile_funop(Ctx,CallEnv,Result,[FunctionName | FunctionArgs], Body):- nonvar(FunctionName),
      expand_arguments_maybe_macro(Ctx,CallEnv,FunctionName,0,FunctionArgs,ArgBody, Args),
      debug_var([FunctionName,'_Ret'],Result),      
      find_function_or_macro(FunctionName,Args,Result,ExpandedFunction),      
      Body = (ArgBody,ExpandedFunction).


% FUNCTION APPLY
%compile_body(Ctx,Env,Result,['apply',Function|ARGS], Body):- atom(Function)
% FUNCTION FUNCALL
% compile_body(Ctx,Env,Result,['funcall',Function|ARGS], Body):- ...




uses_exact(FunctionName,ArgInfo):-  user:arglist_info(FunctionName,_,_,ArgInfo),!,ArgInfo.complex ==0 .
uses_rest(FunctionName,ArgInfo):-  user:arglist_info(FunctionName,_,_,ArgInfo),!,ArgInfo.complex \==0 .
% Non built-in function expands into an explicit function call

find_function_or_macro(FunctionName,Args,Result,ExpandedFunction):-
   length(Args,Len0),Len is Len0+1,
   find_function_or_macro_name(FunctionName,Len, ProposedName),!,
   align_args(FunctionName,ProposedName,Args,Result,ArgsPlusResult),
   ExpandedFunction =.. [ ProposedName | ArgsPlusResult].
   
find_function_or_macro_name(FunctionName,_Len, ProposedName):- 
  get_opv(FunctionName,function,ProposedName),!.
find_function_or_macro_name(FunctionName,Len, ProposedName):-
  some_function_or_macro(FunctionName,Len,['','cl_','pf_','sf_','mf_','f_'],ProposedName),!.
find_function_or_macro_name(FunctionName,_Len, ProposedName):-
    maybe_symbol_package(FunctionName,Package),
    (cl_symbol_name(FunctionName,Name) ->
      function_case_name(Name,Package,ProposedName);
      function_case_name(FunctionName,Package,ProposedName)).


align_args(_FunctionName,ProposedName,Args,Result,[Result,Args]):-
    return_arg_is_first(ProposedName),!.

align_args(_FunctionName,ProposedName,Args,Result,ArgsPlusResult):- 
   append(Args, [Result], ArgsPlusResult),
   length(ArgsPlusResult,Len),
   functor(G,ProposedName,Len),
   current_predicate(_,G),!.

align_args(FunctionName,ProposedName,Args,Result,ArgsPlusResult):- 
   (uses_exact(FunctionName,_ArgInfo);uses_exact(ProposedName,_ArgInfo)),
   append(Args, [Result], ArgsPlusResult).

align_args(FunctionName,ProposedName,Args,Result,ArgsPlusResult):- 
   (uses_rest(FunctionName,_ArgInfo);uses_rest(ProposedName,_ArgInfo)),
   append([Args], [Result], ArgsPlusResult).

align_args(_FunctionName,ProposedName,[Arg],Result,[Arg,Result]):- functor(G,ProposedName,2),current_predicate(_,G),!.

align_args(_FunctionName,ProposedName,Args,Result,ArgsPlusResult):- functor(G,ProposedName,2),current_predicate(_,G),!,
   append([Args], [Result], ArgsPlusResult).

align_args(_FunctionName,_ProposedName,Args,Result,ArgsPlusResult):- 
   append(Args, [Result], ArgsPlusResult).


maybe_symbol_package(Symbol,Package):-  get_opv(Symbol,package,Package),!.
maybe_symbol_package(_Symbol,Package):- reading_package(Package).


some_function_or_macro(FunctionName,Len,[Name|NameS],NewName):-
   atom_concat(Name,FunctionName,ProposedPName),   
   (((ProposedPName = ProposedName; prologcase_name(ProposedPName,ProposedName)),
    guess_functor(P,ProposedName,Len),current_predicate(_,P),\+ predicate_property(user:P,imported_from(system)))-> ProposedName=NewName;
   some_function_or_macro(FunctionName,Len,NameS,NewName)).

guess_functor(P,ProposedName,Len):- var(P),var(Len),!,current_predicate(ProposedName/Len),functor(P,ProposedName,Len).
guess_functor(P,ProposedName,Len):- functor(P,ProposedName,Len).

:- fixup_exports.

