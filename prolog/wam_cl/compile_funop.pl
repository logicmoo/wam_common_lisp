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
compile_funop(Ctx,Env,Result,LispCode,CompileBody):-
  fail, %DISABLED
  macroexpand_1_or_fail(LispCode,[],CompileBody0Result),
  must_compile_body(Ctx,Env,Result,CompileBody0Result, CompileBody).


macroexpand_1_or_fail([Procedure|Arguments],MacroEnv,CompileBody0Result):- nonvar(Procedure),
   debug_var('MacroEnvArgs',MacroEnv),
   user:macro_lambda(defmacro(Procedure),_FProcedure, FormalParams, LambdaExpression,_),!,
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
   dbmsg(comment(macroResult(BindCode,Code,CommaResult,CompileBody0Result))))),!.

:- dynamic(wl:uses_rest_only/1).
:- discontiguous(wl:uses_rest_only/1).

wl:uses_rest_only(cl_macroexpand_1).
wl:uses_rest_only(cl_macroexpand).

cl_macroexpand_1([LispCode|Optional],Result):- macroexpand_1_or_fail(LispCode,Optional,Result)->true;Result=LispCode.
cl_macroexpand([LispCode|Optional],Result):-
  macroexpand_1_or_fail(LispCode,Optional,Mid)->
    cl_macroexpand([Mid|Optional],Result)
     ; Result=LispCode.


% Operator
expand_arguments_maybe_macro(_Ctx,_CallEnv,FunctionName,_N,FunctionArgs,true, FunctionArgs):- is_lisp_operator(FunctionName),!.
expand_arguments_maybe_macro(Ctx,CallEnv,FunctionName,0,FunctionArgs,ArgBody, Args):-
  expand_arguments(Ctx,CallEnv,FunctionName,0,FunctionArgs,ArgBody, Args).


compile_funop(Ctx,Env,Result,[Op | FunctionArgs], Body):- nonvar(Op),wl:op_replacement(Op,Op2), !,
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
      find_function_or_macro(Ctx,CallEnv,FunctionName,Args,Result,ExpandedFunction),      
      Body = (ArgBody,ExpandedFunction).


% FUNCTION APPLY
%compile_body(Ctx,Env,Result,['apply',Function|ARGS], Body):- atom(Function)
% FUNCTION FUNCALL
% compile_body(Ctx,Env,Result,['funcall',Function|ARGS], Body):- ...


find_function_or_macro(Ctx,Env,FunctionName,Args,Result,ExpandedFunction):-
   length(Args,Len0),Len is Len0+1,
   find_function_or_macro_name(Ctx,Env,FunctionName,Len, ProposedName),!,
   align_args(FunctionName,ProposedName,Args,Result,ArgsPlusResult),
   ExpandedFunction =.. [ ProposedName | ArgsPlusResult].
   
find_function_or_macro_name(_Ctx,_Env,FunctionName,_Len, ProposedName):- 
  get_opv(FunctionName,function,ProposedName),!.
find_function_or_macro_name(_Ctx,_Env,FunctionName,Len, ProposedName):-
  some_function_or_macro(FunctionName,Len,['','cl_','pf_','sf_','mf_','f_'],ProposedName),!.
find_function_or_macro_name(_Ctx,_Env,FunctionName,_Len, ProposedName):-
    maybe_symbol_package(FunctionName,Package),
    (cl_symbol_name(FunctionName,Name) ->
      function_case_name(Name,Package,ProposedName);
      function_case_name(FunctionName,Package,ProposedName)).

/*

exact = ;; takes whatever is supplied
  append(p1,p2,R)

uses_rest_only_p =  ;; requires param parsing
  defclass([Arg1|ArgS],R)

exact_and_restkeys =  ;; requires param parsing
  defclass(r1,r2,[Arg3|ArgS],R)

reversed = ;; simplifes hand coding of complex
  defclass(R,[Arg1|ArgS])

environmented
  fun(E),


 fun(r1,r2,[op1,opt2|RestAndkeys],R)
*/


uses_exact(FunctionName):-  wl:arglist_info(FunctionName,_,_,ArgInfo),!,ArgInfo.complex ==0 .

exact_and_restkeys(_FunctionName,_Exacts):- fail.

uses_rest_only_p(P):- uses_rest_only0(P),!.
uses_rest_only_p(P):- atom_concat_or_rtrace('f_',P,PP), uses_rest_only0(PP).
uses_rest_only_p(P):- atom_concat_or_rtrace('f_',PP,P), uses_rest_only0(PP).

uses_rest_only0(FunctionName):-  wl:arglist_info(FunctionName,_,_,ArgInfo),!,ArgInfo.complex \==0 .
uses_rest_only0(FunctionName):- same_symbol(FunctionName,F),wl:declared(F,lambda(['&rest',_])),!.
uses_rest_only0(P):- wl:arg_lambda_type(rest_only,P),!.
uses_rest_only0(P):- wl:uses_rest_only(P).

% Non built-in function expands into an explicit function call

% invoke(r1,r2,r3,RET).
align_args(FunctionName,ProposedName,Args,Result,ArgsPlusResult):- 
  (uses_exact(FunctionName);uses_exact(ProposedName)),
   append(Args, [Result], ArgsPlusResult).

% invoke([r1,r2,r3],RET).
align_args(FunctionName,ProposedName,Args,Result,[Args,Result]):-
  (uses_rest_only_p(FunctionName);uses_rest_only_p(ProposedName)).

% invoke(r1,r2,[r3],RET).
align_args(FunctionName,ProposedName,Args,Result,ArgsPlusResult):- 
  (exact_and_restkeys(FunctionName,N);exact_and_restkeys(ProposedName,N)),
  length(Left,N),append(Left,Rest,Args),
  append(Left, [Rest,Result], ArgsPlusResult).

% begin to guess
align_args(_FunctionName,ProposedName,Args,Result,ArgsPlusResult):- 
   append(Args, [Result], ArgsPlusResult),
   length(ArgsPlusResult,Len),
   functor(G,ProposedName,Len),
   current_predicate(_,G),!.

% guess invoke(r1,RET).
align_args(_FunctionName,ProposedName,[Arg],Result,[Arg,Result]):- functor(G,ProposedName,2),current_predicate(_,G),!.

% guess invoke([r1,r2,r3],RET).
align_args(_FunctionName,ProposedName,Args,Result,ArgsPlusResult):- functor(G,ProposedName,2),current_predicate(_,G),!,
  append([Args], [Result], ArgsPlusResult).

% fallback to invoke([r1,r2,r3],RET).
align_args(FunctionName, ProposedName,Args,Result,[Args,Result]):-
  (is_lisp_operator(FunctionName) ; is_lisp_operator(ProposedName)),!.

/*
% guess invoke(r1,r2,r3,RET).
*/
align_args(_FunctionName,_ProposedName,Args,Result,ArgsPlusResult):-  
   append(Args, [Result], ArgsPlusResult).


maybe_symbol_package(Symbol,Package):-  get_opv(Symbol,package,Package),!.
maybe_symbol_package(_Symbol,Package):- reading_package(Package).


some_function_or_macro(FunctionName,Len,[Name|NameS],NewName):-
   atom_concat_or_rtrace(Name,FunctionName,ProposedPName),   
   (((ProposedPName = ProposedName; prologcase_name(ProposedPName,ProposedName)),
    guess_functor(P,ProposedName,Len),current_predicate(_,P),\+ predicate_property(user:P,imported_from(system)))-> ProposedName=NewName;
   some_function_or_macro(FunctionName,Len,NameS,NewName)).

guess_functor(P,ProposedName,Len):- var(P),var(Len),!,current_predicate(ProposedName/Len),functor(P,ProposedName,Len).
guess_functor(P,ProposedName,Len):- functor(P,ProposedName,Len).

:- fixup_exports.

