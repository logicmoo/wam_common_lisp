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

:- include('./header').



f_eval(Form,Result):- lisp_compile(Result,Form,Body),always(Body).

wl:init_args(1,funcall).
%f_funcall(function(F),More,R):-!,f_funcall(F,More,R).
f_funcall(ProcedureName,Args,Result):- f_apply(ProcedureName, [Args], Result).
% f_funcall([F|More],R):- append([More],[R],ARGS), lpa_apply(F,ARGS).



wl:init_args(1,apply).
f_apply(FunctionName,Arguments,Result):- (var(FunctionName);FunctionName==[]),!,lisp_dump_break,Result=Arguments.
f_apply(FunctionName, Arguments, Result):- is_list(FunctionName),!,
  append(FunctionName, Arguments,FunWithArguments),
  lisp_compiled_eval(FunWithArguments,Result).
f_apply(function(FunctionName), Arguments, Result):-nonvar(FunctionName),!,f_apply((FunctionName), Arguments, Result).
%f_apply(FunctionName, Arguments, Result):- atom(FunctionName),!,lisp_compiled_eval([FunctionName|Arguments],Result).
%f_apply(closure(kw_function,Environment,ClosureResult,FormalArgs,Body), [Arguments], Result):-!,always(closure(kw_function,Environment,ClosureResult,FormalArgs,Body,Arguments,Result)).
f_apply(Compound, Arguments, Result):- always(callable(Compound)),!, Compound=..FunctionName,
  append(FunctionName, [Arguments,Result],Funcall),
  Call=..Funcall,
  always(Call).




:- discontiguous(compile_funop/5).

compile_funop(Ctx,Env,Result,[function(Op) | FunctionArgs], Body):- nonvar(Op),!,
  must_compile_body(Ctx,Env,Result,[Op | FunctionArgs], Body).

%compile_funop(Ctx,Env,Result,[[quote, Op] | FunctionArgs], Body):- nonvar(Op),!,
%  must_compile_body(Ctx,Env,Result,[Op | FunctionArgs], Body).

% Messed Progn?
% compile_body(Ctx,Env,Result,[Form|MORE], Code):- is_list(Form), !,must_compile_progn(Ctx,Env,Result,[Form|MORE], Code).

 
compile_funop(Ctx,Env,Result,[FN| Args], Code):- var(FN),!,
      compile_apply(Ctx,Env,FN,Args,Result,Code).


% compile_funop(Ctx,Env,Result,[list|Args], Body):- expand_arguments(Ctx,Env,list,1,Result,Args,Body).

% Use a previous DEFMACRO
compile_funop(Ctx,Env,Result,LispCode,CompileBody):-
  %fail, %DISABLED
  macroexpand_1_or_fail(Ctx,Env,LispCode,CompileBody0Result),
  dbginfo(macroexpand:-LispCode),
  dbginfo(into:-CompileBody0Result),
  must_compile_body(Ctx,Env,Result,CompileBody0Result, CompileBody),
  !.
      

compile_funop(Ctx,Env,Result,[Op | FunctionArgs], Body):- nonvar(Op), wl:op_replacement(Op,Op2), !,
  must_compile_body(Ctx,Env,Result,[Op2 | FunctionArgs],Body).

compile_funop(Ctx,Env,Result,[eval , Form],(FormBody,f_eval(ResultForm,Result))):-!,slow_trace,
   must_compile_body(Ctx,Env,ResultForm,Form,FormBody).

% malformed funcall
compile_funop(Ctx,Env,Result,[funcall, FN| FunctionArgs], Body):- \+ is_list(FunctionArgs),
     compile_funop(Ctx,Env,Result,[apply, FN, [list|FunctionArgs]], Body).

compile_funop(Ctx,Env,Result,[funcall, FN| FunctionArgs], Body):- 
      must_compile_body(Ctx,Env,F,FN,ArgsBody1),
      var(FN),expand_arguments(Ctx,Env,funcall,2,Args,FunctionArgs,ArgsBody2),
      compile_apply(Ctx,Env,F,Args,Result,Code),
      Body = (ArgsBody1,ArgsBody2,Code).

compile_funop(Ctx,Env,Result,[funcall, FN| FunctionArgs], Body):- 
      must_compile_body(Ctx,Env,F,FN,ArgsBody1),
      expand_arguments(Ctx,Env,F,1,Args,FunctionArgs,ArgsBody2),
      compile_apply(Ctx,Env,F,Args,Result,Code),
      Body = (ArgsBody1,ArgsBody2,Code).

compile_funop(Ctx,Env,Result,[apply, FN, [List,FunctionArg]], Body):- List == list,
      compile_funop(Ctx,Env,Result,[funcall, FN, FunctionArg], Body).

compile_funop(Ctx,Env,Result,[apply, FN, FunctionArgs], Body):-
  must_compile_body(Ctx,Env,F,FN,ArgsBody1),
      must_compile_body(Ctx,Env,Args,FunctionArgs,ArgsBody2),
      compile_apply(Ctx,Env,F,Args,Result,Code),
      Body = (ArgsBody1,ArgsBody2,Code).

% malformed call
compile_funop(Ctx,Env,Result,[FN| FunctionArgs], Body):- \+ is_list(FunctionArgs),trace,
     compile_funop(Ctx,Env,Result,[apply, [quote, FN], [list|FunctionArgs]], Body).


% Special Operator
compile_funop(Ctx,CallEnv,Result,[FN | FunctionArgs], Body):-  is_lisp_operator(Ctx,CallEnv,FN),!,      
      compile_apply(Ctx,CallEnv,FN,FunctionArgs,Result,ExpandedFunction), 
      Body = (ExpandedFunction).

% Macro
compile_funop(Ctx,CallEnv,Result,[FN | FunctionArgs], Body):-  is_lisp_operator(Ctx,CallEnv,FN),!,      
      compile_apply(Ctx,CallEnv,FN,FunctionArgs,Result,ExpandedFunction),
      Body = (ExpandedFunction).


% NORMAL CALL
compile_funop(Ctx,CallEnv,Result,[FN | FunctionArgs], Body):- 
      expand_arguments_maybe_macro(Ctx,CallEnv,FN,0,[FNC|Args],[FN|FunctionArgs],ArgsBody),
      compile_apply(Ctx,CallEnv,FNC,Args,Result,ExpandedFunction),
      Body = (ArgsBody,ExpandedFunction).

% TODO- HOW DID WE GET HERE?
compile_funop(_Ctx,_Env,Result,[FN | FunctionArgs],f_eval([FN|FunctionArgs],Result)). 


check_foc_operator(Ctx,Env,BindType,F,Args,BetterName,PushPreArgs):-
   foc_operator(Ctx,Env,BindType,F,Args,ProposedName),
   always(do_check_foc_operator(Ctx,Env,BindType,F,Args,ProposedName, BetterName,PushPreArgs)),!.


do_check_foc_operator(_Ctx,Env,_BindType,_F,_Args,ProposedName, BetterName,[Env]):-
  atom_concat('mf_',Root,ProposedName),  
  atom_concat('sf_',Root,BetterName),
  current_predicate(BetterName/N),N>=1,
  wdmsg(rename(ProposedName,BetterName)),!.

do_check_foc_operator(_Ctx,Env,_BindType,_F,_Args,ProposedName, BetterName,[Env]):-
  atom_concat('sf_',Root,ProposedName),  
  atom_concat('sf_',Root,BetterName),
  %current_predicate(BetterName/N),N>=1,
  !.

do_check_foc_operator(_Ctx,_Env,_BindType,_F,_Args,ProposedName, BetterName,[]):-
  atom_concat('mf_',Root,ProposedName),  
  atom_concat('f_',Root,BetterName),
  current_predicate(BetterName/N),N>=1,
  wdmsg(rename(ProposedName,BetterName)),!.

do_check_foc_operator(_Ctx,Env,_BindType,_F,_Args,ProposedName, BetterName,[Env]):-
  atom_concat('mf_',Root,ProposedName),  
  atom_concat('sf_',Root,BetterName),
  wdmsg(rename(ProposedName,BetterName)),!.

do_check_foc_operator(_Ctx,_Env,_BindType,_F,_Args,ProposedName, BetterName,[]):-   
   current_predicate(ProposedName/N),N>=1,!,ProposedName= BetterName.

do_check_foc_operator(_Ctx,_Env,_BindType,_F,_Args,ProposedName, ProposedName,[]).
   
   
  
compile_apply_function_or_macro_call(Ctx,Env,FN,Args,Result,ExpandedFunction):-
 always((
   (is_list(Args)->length(Args,ArgsLen);(integer(Args)->ArgsLen=Args;true)),
   check_foc_operator(Ctx,Env,BindType,FN,ArgsLen, ProposedName,Extra),!,  
   (BindType == kw_function -> true ; wdmsg(BindType->ProposedName)),
   align_args_or_fallback1(Ctx,Env,FN, ProposedName,Args,Result,ArgsPlusResult),!,
   append(Extra,ArgsPlusResult,ExtraArgsPlusResult),
   ExpandedFunction =.. [ ProposedName | ExtraArgsPlusResult])),!.


compile_apply(Ctx,Env,F,Args,Result,ExpandedFunction):- 
   always(compile_apply0(Ctx,Env,F,Args,Result,ExpandedFunction)),!.

compile_apply0(Ctx,Env,F,Args,Result,ExpandedFunction):-  F==[],!,
   trace_or_throw(compile_apply0(Ctx,Env,F,Args,Result,ExpandedFunction)).
compile_apply0(Ctx,Env,F,Args,Result,ExpandedFunction):-
  compile_apply1(Ctx,Env,F,Args,Result,ExpandedFunction),!.

compile_apply1(Ctx,Env,F,Args,Result,ExpandedFunction):- atom(F),
 ((get_init_args(F,N),integer(N)); ( F==list,N=0)),
 length(Left,N),
 append(Left,IntoList,Args),
 append(Left,[IntoList,Result],NewArgs),
 check_foc_operator(Ctx,Env,_Non_Macro_kw_function,F,_, ProposedName, Extra),!,
 append(Extra,NewArgs,NewNewArgs),
 ExpandedFunction =.. [ ProposedName | NewNewArgs].

compile_apply1(_Ctx,_Env,F,Args,Result,Out):- (var(F); \+ is_list(Args)),!,
            Out = f_apply(F,Args,Result).

compile_apply1(Ctx,Env,F,Args,Result,ExpandedFunction):- atom(F),
 compile_apply_function_or_macro_call(Ctx,Env,F,Args,Result,ExpandedFunction),!.

compile_apply1(Ctx,Env,function(F),Args,Result,ExpandedFunction):- atom(F),
 compile_apply1(Ctx,Env,F,Args,Result,ExpandedFunction),!.


compile_apply1(Ctx,Env,function(F),Args,Result,ExpandedFunction):- atom(F),
 compile_apply_function_or_macro_call(Ctx,Env,F,Args,Result,ExpandedFunction),!.

compile_apply1(_Ctx,_Env,F,Args,Result,Out):- 
   Out = f_apply(F,Args,Result).


  
   
/*
% progn mismatch?
compile_funop(Ctx,Env,Result,[FN ], Body):- is_list(FN),!,
  trace,must_compile_body(Ctx,Env,Result,FN,Body).

compile_funop(Ctx,Env,Result,[FN | FunctionArgs], Body):- 
   show_call(must_compile_body(Ctx,Env,Result,[eval,[FN| FunctionArgs]],Body)).
*/

current_predicatef_sys_env_eval(_Env, _Pt1^Body, _Result):- !,
  always(Body).
f_sys_env_eval(Env, Expression, Result):-
  lisp_compile(Env,Result,Expression,Body),
  user:always(Body).

:- fixup_exports.


