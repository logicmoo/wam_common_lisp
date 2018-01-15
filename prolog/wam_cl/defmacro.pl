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

/*
[11:30] <dmiles> what i am trying to figurte out is if my macro's lambda list has &environment e  in it and if the e is passed to  macroexpand will that cause the same result if i had not passed it e
[11:32] <beach> dmiles: If your macroexpander needs to call another macro expander for a macro defined in its lexical environment, you might get the wrong answer if you don't use the right environment.
[11:33] <pjb> dmiles: nope.
[11:33] <beach> dmiles: Right, pjb's answer is the short version.
[11:35] <beach> pjb: Are you mellowing with age or something?
[11:35] <pjb> dmiles: a simple example: (define-symbol-macro s 42) (defmacro m (x &environment e) `(list ,(macroexpand-1 x e))) (symbol-macrolet ((s 33)) (m s)) --> (33)
[11:35] <dmiles> ok so the '&environment e' that my macro uses is the enviroment beelonging to whom.. the environemnt of my caller or the enviroment i would manipulate from labels?
[11:36] <pjb> or: (define-symbol-macro s 42) (defmacro m (x &environment e) `(list ,(macroexpand-1 x e) ,(macroexpand-1 x))) (symbol-macrolet ((s 33)) (m s))  --> (33 42)
[11:36] <pjb> beach: it's irregular :-)
[11:36] <beach> Ah, yes, I see.
[11:37] <beach> Anyway, hope to see you again at ELS2018.
[11:38] <pjb> Of course, macroexpand-1 also expands normal macros (and macrolet), and other functions take an environment argument.
[11:39] <beach> jmercouris: You need to meet him in person.  A very sweet person is my impression.
[11:45] <pjb> dmiles: even if it's not allowed to refer at macroexpand time to lexical variable in a local macro;  local macros can still refer to surrounding local macros.
[11:45] * pedh (~pedh@106.39.67.33) Quit (Ping timeout: 256 seconds)
[11:46] <pjb> shka: if you leak the dynamic environment outside of the blocks where the local macros are defined, and use it, you could expand using local macros that are not active anymore.
[11:46] <shka> Shinmera: btw, i hooked docstring formatter into documentation-utils, i have few issues to sort out though
[11:46] <pjb> Also, you have to consider that local macros are defined and expanded at compilation time, while the dynamic environment is returned at run-time.
[11:46] <dmiles> pjb: heh, i think my impl of continuations is what inspired a willingness to do this with envs
[11:47] <pjb> dmiles: right, if you have them. But consider also this compilation-time vs. run-time.
[11:47] <shka> pjb: interesting
[11:48] <pjb> dmiles: or to make it useful, you could expand the environments to include more.   And letting an environment escape would be equivalent to returning a closure.
[11:48] <dmiles> actually yes.. the problem is as pjb points out is do i see function macros i *should* be seeing due to runtime extent or do i see what is overridden 
[11:49] <dmiles> function (i meant to backspec over the word macros)
[11:49] <shka> dmiles: have you figured out some other way to do continuations?
[11:49] <shka> somewhat more efficient perhaps?
[11:50] <pjb> for example, find-class modifies the environment at run-time.  while defmacro provides a compilation-time environment, and assumedly macroexpand is used at compilation time too (but not necessarily).  So you have the problem of transporting environment (and their enclosed bindings) from compilation time to run-time (possibly thru a FASL, so now you have to be able to write environment and load them!).
[11:50] <pjb> Much fun.
[11:50] <dmiles> oh yeah and about what pjb is saying.. this is kinda been a big issue.. i currently have to pass two envs to around
[11:51] <pjb> But then, if you map them to closures, it should be doable.
[11:51] <dmiles> i have a compilations env and a runtimes env :\
[11:52] <dmiles> i been trying to come up with a way to merge them somehow.. its not a big deal for my compiler.. i pass two arround.. i just trying to decide how that will be at runtime
[11:53] <dmiles> shka: well i dont know if its any more efficient or not
[11:53] <beach> For what it's worth, environments figure more prominently in SICL.  Most (all?) Common Lisp function that take an &optional environment argument dispatch to some SICL-specific generic function that dispatches on that argument.
[11:54] <pjb> dmiles: well, they should be merged anyways, because you can also macroexpand at run-time!
[11:54] <shka> dmiles: i see
[11:55] <dmiles> pjb: yes .. it turns out the runtime env needs basically almost everythig the compiler env would have had
[11:56] <dmiles> the fact that they (should?) scope differntly is all that the problem is
[11:57] <pjb> The thing is rather that the way clhs specifies it (that environment have only dynamic extend, without leaking them), let the compiler optimize out the local macros and symbol-macro, so they don't have to go thru to the run-time environment.
*/
:- discontiguous compile_macro_ops/5.

% DEFMACRO
wl:init_args(2,defmacro).
sf_defmacro(Symbol,FormalParms,MacroBody,Return):- reenter_lisp(Ctx,Env),compile_macro_ops(Ctx,Env,Return,[defmacro,Symbol,FormalParms|MacroBody],Code),cmpout(Code).

compile_macro_ops(Ctx,Env,Result,[defmacro,Symbol,FormalParms|MacroBody], (Code,FunDef,Result=Symbol)):-
  compile_macro(Ctx,Env,[Symbol,FormalParms|MacroBody],_Sym,Macro,Code),
  debug_var('DefMacroResult',Result),
  FunDef = (set_opv(Macro,type_of,sys_macro),set_opv(Symbol,symbol_function,Macro)).  

% MACROLET
wl:init_args(1,macrolet).
mf_macrolet(Inits,Progn,Result):- reenter_lisp(Ctx,Env), compile_macro_ops(Ctx,Env,Result,[macrolet,Inits|Progn],Code), always(Code).  

compile_macro_ops(Ctx,Env,Result,[macrolet,MACROLETS|Progn], (maplist(always,Decls),CompileBody)):- 
    must_maplist(define_each_macro(Ctx,Env,macrolet),MACROLETS,FBOUNDS,Decls),    
    must_compile_progn([FBOUNDS|Ctx],[FBOUNDS|Env],Result,Progn, CompileBody).
    


define_each_macro(Ctx,Env,_MacroLet,[Symbol|DEFN],fbound(Sym)=bound_type(kw_macro,UniqueMacroName),CompileBody)  :-    
   (always(foc_operator(Ctx,Env,kw_macro,Symbol,_Len, Macro0)),suffix_by_context(Ctx,Macro0,Macro)),
   gensym(Macro,UniqueMacroName),
   compile_macro(Ctx,Env,[Symbol|DEFN],Sym,UniqueMacroName,CompileBody),
   always(CompileBody).


   
compile_macro(Ctx,Env,[Symbol|FormalParmsMacroBody],Symbol,Macro, (CompileBody,assert_lsp(Symbol,MacroAssert))):-
  debug_var('MFResult',MFResult),debug_var('FnResult',FResult),
  compile_macro_function(Ctx,Env,Symbol,FormalParmsMacroBody,Macro,HeadParms,EnvAssign,HeadCode,MFBody,MFResult,CompileBody),
    append([Macro|HeadParms],[MFResult],CallableHeadV), CallableHead =.. CallableHeadV,   
    % CallableMF =.. [MF,Whole,Env],
    MFResult=FResult,
   body_cleanup_keep_debug_vars(Ctx,
     ((CallableHead  :- ((nop(defmacro),EnvAssign,HeadCode,MFBody)))),
       MacroAssert).
   

compile_macro_function(Ctx,Env,Symbol,[FormalParms|MacroBody0],Macro,HeadParms,EnvAssign,HeadCode,MFBody,MFResult,CompileBody):-
   maybe_get_docs(function,Symbol,MacroBody0,MacroBody,DocCode),

   (var(Macro) -> (always(foc_operator(Ctx,Env,kw_macro,Symbol,_Len, Macro0)),suffix_by_context(Ctx,Macro0,Macro)); 
     true),

   LabelSymbol = '', % LabelSymbol =Symbol       
within_labels_context(Ctx,LabelSymbol,((
   Whole=[Symbol|_],
   expand_function_head_macro(Ctx,Env,Symbol,Macro,FormalParms,Whole, HeadParms,ZippedArgEnv,_ArgInfo, HeadDefCode,HeadCode),   
   make_env_append(Ctx,Env,HeadEnv,ZippedArgEnv,EnvAssign),
   must_compile_body(Ctx,HeadEnv,MFResult,[block,Symbol|MacroBody],MFBody), 
   body_cleanup_keep_debug_vars(Ctx,((DocCode,
     assert_lsp(Symbol,wl:lambda_def(defmacro,Symbol,Macro, FormalParms, [progn | MacroBody])),HeadDefCode)),CompileBody)))).

wl:plugin_expand_progbody_1st(Ctx,Env,Result,[macroexpand_1,LispCode|ARGS],_PreviousResult,
      (LispCodeEval,ARGSCode,f_macroexpand_1([LispCodeResult|ARGSResult],Result))):-
   compile_each(Ctx,Env,ARGSResult,ARGS,ARGSCode),
   must_compile_body(Ctx,Env,LispCodeResult,LispCode,LispCodeEval),!.

wl:plugin_expand_progbody_1st(Ctx,Env,Result,[macroexpand,LispCode|ARGS],_PreviousResult,
      (LispCodeEval,ARGSCode,f_macroexpand([LispCodeResult|ARGSResult],Result))):-
   compile_each(Ctx,Env,ARGSResult,ARGS,ARGSCode),
   must_compile_body(Ctx,Env,LispCodeResult,LispCode,LispCodeEval),!.
  
 
% macroexpand-1
wl:init_args(0,macroexpand).
f_macroexpand_1([LispCode|Optionals],Result):- 
  nth_param(Optionals,1,Local,MacroEnv),
  parent_env(Local),
  macroexpand_1_or_fail(LispCode,MacroEnv,R)->f_values_list([R,t],Result);f_values_list([LispCode,[]],Result).

% macroexpand
wl:init_args(0,macroexpand_1).
f_macroexpand([LispCode|Optionals],Result):- 
  nth_param(Optionals,1,kw_undefined,MacroEnvArg),
  ((MacroEnvArg=kw_undefined)->parent_env(MacroEnv);MacroEnvArg=MacroEnv),
  macroexpand_all(LispCode,MacroEnv,R),!,
  (R\==LispCode->f_values_list([R,t],Result);f_values_list([R,[]],Result)).

f_sys_pf_symbol_macroexpand(_Place_Get, _Env_Get,Result):- Result=[].

wl:interned_eval(("`sys:set-symbol-macro-function")).
f_sys_set_symbol_macro_function(Symbol,FunctionSpec,ResultIsMacro):- 
  as_symbol_macro_function(_Ctx,_Env,Symbol,FunctionSpec,ResultIsMacro),
  set_opv(Symbol,symbol_function,ResultIsMacro),set_opv(ResultIsMacro,type_of,sys_macro).

wl:interned_eval(("`sys:set-symbol-macro")).
f_sys_set_symbol_macro(Symbol,FunctionSpec,ResultIsMacro):- 
  as_symbol_macro_function(_Ctx,_Env,Symbol,FunctionSpec,ResultIsMacro),
  set_opv(Symbol,symbol_function,ResultIsMacro),set_opv(ResultIsMacro,type_of,sys_macro).

as_symbol_macro_function(Ctx,Env,_Symbol,function(MF),ResultIsMacro):-eval(Ctx,Env,function(MF),ResultIsMacro).
as_symbol_macro_function(Ctx,Env,_Symbol,MF,ResultIsMacro):- \+ is_list(MF),eval(Ctx,Env,MF,ResultIsMacro).
as_symbol_macro_function(Ctx,Env,Symbol,FormalParmsMacroBody,Macro):- assertion(is_list(FormalParmsMacroBody)),    
  % debug_var('MFResult',MFResult),debug_var('FnResult',FResult),
  compile_macro_function(Ctx,Env,Symbol,FormalParmsMacroBody,Macro,HeadParms,EnvAssign,HeadCode,MFBody,MFResult,CompileBody),
  MFResult=FResult,
     append([Macro|HeadParms],[FResult],CallableHeadV), CallableHead =.. CallableHeadV,   
     % CallableMF =.. [MF,Whole,Env],
    body_cleanup_keep_debug_vars(Ctx,
      ((CallableHead  :- ((global_env(Env),nop(as_symbol_macro_function),EnvAssign,HeadCode,MFBody)
        ))),
        MacroAssert),
    always((CompileBody,assert_lsp(Symbol,MacroAssert))).


macroexpand_all(LispCode,MacroEnv,Result):-
  macroexpand_1_or_fail(LispCode,MacroEnv,Mid) ->
    macroexpand_all(Mid,MacroEnv,Result) ; Result=LispCode.


f_macro_function(Procedure,ProposedName):-
    (find_operator(_Ctx,_Env,kw_macro,Procedure,_ArgsLen, ProposedName);ProposedName=[]),!.


get_macro_function(Ctx,Env,Procedure,Arguments,MResult,CallBody):-  atom(Procedure),
   length(Arguments,ArgsLen),
   notrace(find_operator(Ctx,Env,kw_special,Procedure,ArgsLen, ProposedName)),!,
   notrace(align_args_or_fallback(Ctx,Env,Procedure,ProposedName,Arguments,_FnResult,ArgsPlusResult)),
   ExpandedMacro =.. [ ProposedName | ArgsPlusResult],
   get_mf_interface(ExpandedMacro,CallBody,MResult).


get_mf_interface(MacroSymbol,CallBody,MResult):- 
   MacroSymbol =.. [ ProposedName | ArgsPlusResult],
   atom_concat('mf_',ProposedName,ProposedName_macroexpand1_),
   MacroSymbol_macroexpand1_ =.. [ ProposedName_macroexpand1_ | ArgsPlusResult],
   clause_interface(MacroSymbol_macroexpand1_,CallBody),
   append(_,[MResult],ArgsPlusResult).

get_mf_interface(OperatorSymbol,CallBody,MResult):- 
   OperatorSymbol =.. [ ProposedName | ArgsPlusResult],
   atom_concat('sf_',ProposedName,ProposedName_macroexpand1_),
   OperatorSymbol_macroexpand1_ =.. [ ProposedName_macroexpand1_ | ArgsPlusResult],
   clause_interface(OperatorSymbol_macroexpand1_,Conj),
   unify_conj(Conj,(CallBody,f_eval(MResult, _))).

unify_conj(Conj,To):- nonplainvar(Conj),Conj=To,!.
unify_conj((CA,(CB,CC)),(A,B)):- var(A),nonplainvar(B),!, unify_conj(((CA,CB),CC),(A,B)).
unify_conj((CA,(CB,CC)), AB):- unify_conj(((CA,CB),CC),AB).

wl:declared(sys_is,interpret).
 

macroexpand_1_or_fail([Procedure|_],_,_):- wl:declared(Procedure,interpret),!,fail.

macroexpand_1_or_fail([Procedure|Arguments],MacroEnv,MResult):- nonplainvar(Procedure),   
   get_macro_function(_Ctx,MacroEnv,Procedure, Arguments, MResult, CallBody),!,always(CallBody),!.

macroexpand_1_or_fail([Procedure|Arguments],MacroEnv,CompileBody0Result):- nonplainvar(Procedure),
   debug_var('MacroEnvArgs',MacroEnv),
   get_lambda_def(defmacro,Procedure, FormalParams, LambdaExpression),!,
   always((debug_var('EnvThru',EnvThru),debug_var('NewEnv',NewEnv),
   debug_var('MEnv',Env),debug_var('NextEnv',NextEnv),debug_var('CommaResult',CommaResult),
   Whole = [Procedure|Arguments],
   must_bind_parameters(NewEnv,Whole, _RestNKeys, FormalParams,Procedure, Arguments,EnvThru,BindCode),!,
   append(_,[],NewEnv),!,
   NextEnv = [NewEnv|Env],  
   always(BindCode),
   always(expand_commas(Ctx,1,NewEnv,CommaResult,LambdaExpression,CodeS)),
   body_cleanup_keep_debug_vars(Ctx,CodeS,Code),
   % (local_override(with_forms,lisp_grovel)-> (lisp_dumpST) ; true),
   always(Code),
   must_compile_body(Ctx,NextEnv,CompileBody0Result,CommaResult, MCBR),
   always(MCBR),
   dbginfo((macroResult(Procedure,Code,CommaResult,CompileBody0Result))))),!.



:- fixup_exports.

