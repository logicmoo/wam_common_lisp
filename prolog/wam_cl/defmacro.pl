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
:- module(macr, []).

:- set_module(class(library)).

:- include('header').

:- discontiguous compile_macro_ops/5.

% DEFMACRO
wl:init_args(2,cl_defmacro).
cl_defmacro(Symbol,FormalParms,FunctionBody,Return):- reenter_lisp(Ctx,Env),compile_macro_ops(Ctx,Env,Return,[defmacro,Symbol,FormalParms|FunctionBody],Code),dbmsg_real(Code).
compile_macro_ops(Ctx,Env,Result,[defmacro,Symbol,FormalParms|FunctionBody], (Code,FunDef,Result=Symbol)):-
  compile_macro(Ctx,Env,[Symbol,FormalParms|FunctionBody],_Sym,Function,Code),
  debug_var('DefMacroResult',Result),
  FunDef = (set_opv(Function,classof,claz_macro),set_opv(Symbol,compile_as,kw_operator),set_opv(Symbol,function,Function)),   
  always((FunDef,Code)).  

% MACROLET
wl:init_args(1,cl_macrolet).
cl_macrolet(Inits,Progn,Result):- reenter_lisp(Ctx,Env), compile_macro_ops(Ctx,Env,Result,[macrolet,Inits|Progn],Code), always(Code).  
compile_macro_ops(Ctx,Env,Result,[macrolet,MACROLETS|Progn], CompileBody):- 
    must_maplist(define_each_macro(Ctx,Env,macrolet),MACROLETS,News,Decls),
    maplist(always,Decls),
    compile_forms(Ctx,Env,Result,Progn, CompileBody),
    maplist(remove_symbol_fbounds(Ctx),News).


define_each_macro(Ctx,Env,_LabelsOrFLET,[Symbol|DEFN],Sym,CompileBody)  :-    
   (always(find_function_or_macro_name(Ctx,Env,Symbol,_Len, Function)),suffix_by_context(Ctx,Function,Macro)),
   gensym(Macro,UniqueCtxFunction),
   compile_macro(Ctx,Env,[Symbol|DEFN],Sym,UniqueCtxFunction,CompileBody),
   add_symbol_fbounds(Ctx,Sym,kw_function,UniqueCtxFunction),
   always(CompileBody),
   dbmsg_real(CompileBody).


compile_macro(Ctx,Env,[Symbol,FormalParms|FunctionBody0],Symbol,Macro, CompileBody):-
   maybe_get_docs(function,Symbol,FunctionBody0,FunctionBody,DocCode),

   (var(Macro) -> (always(find_function_or_macro_name(Ctx,Env,Symbol,_Len, Function)),suffix_by_context(Ctx,Function,Macro)); 
     true),

   LabelSymbol = '', % LabelSymbol =Symbol 
      debug_var('MFResult',MFResult),debug_var('FnResult',FResult),
 within_labels_context(Ctx,LabelSymbol,((
   expand_function_head(Ctx,Env,Symbol,FormalParms,_Whole, HeadParms,_HeadEnv, HeadDefCode,HeadCode),
   append([Macro|HeadParms],[FResult],HeadV),
   CallableHead =.. HeadV,
   must_compile_body(Ctx,Env,MFResult,[block,Symbol|FunctionBody],MFBody),   
   body_cleanup_keep_debug_vars(Ctx,((CallableHead  :- ((HeadCode,MFBody), cl_eval(MFResult,FResult)))),MacroAssert),
   body_cleanup_keep_debug_vars(Ctx,((DocCode,
   assert_lsp(Symbol,wl:lambda_def(defmacro,(Symbol),Macro, FormalParms, [progn | FunctionBody])),
   HeadDefCode,
   assert_lsp(Symbol,MacroAssert))),CompileBody)))).


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


macroexpand_all(LispCode,MacroEnv,Result):-
  macroexpand_1_or_fail(LispCode,MacroEnv,Mid) ->
    macroexpand_all(Mid,MacroEnv,Result) ; Result=LispCode.

get_macro_function(Ctx,Env,Procedure, Arguments,MResult,FnResult,CallBody):- 
       atom(Procedure),
   length(Arguments,ArgsLen),
   find_function_or_macro_name(Ctx,Env,Procedure,ArgsLen, ProposedName),!,
   align_args_or_fallback(Procedure,ProposedName,Arguments,FnResult,ArgsPlusResult),
   ExpandedFunction =.. [ ProposedName | ArgsPlusResult],
   clause_interface(ExpandedFunction,Conj),
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
   must_bind_parameters(NewEnv, _RestNKeys, FormalParams, Arguments,EnvThru,BindCode),!,
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


% Operator
expand_arguments_maybe_macro(Ctx,_CallEnv,FN,_N,FunctionArgs,true, FunctionArgs):- is_lisp_operator(Ctx,FN),!.
expand_arguments_maybe_macro(Ctx,Env,FN,0,FunctionArgs,ArgBody, Args):-
  expand_arguments(Ctx,Env,FN,0,FunctionArgs,ArgBody, Args).


:- fixup_exports.

