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

:- include('./header').

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
sf_defmacro(Env,Symbol,FormalParms,MacroBody,Return):- reenter_lisp(Ctx,Env),compile_macro_ops(Ctx,Env,Return,[defmacro,Symbol,FormalParms|MacroBody],Code),cmpout(Code).

wl:init_args(2,define_compiler_macro).
sf_define_compiler_macro(Env,Symbol,FormalParms,MacroBody,Return):- reenter_lisp(Ctx,Env),compile_macro_ops(Ctx,Env,Return,[defmacro,Symbol,FormalParms|MacroBody],Code),cmpout(Code).

compile_macro_ops(Ctx,Env,Result,[defmacro,Symbol,FormalParms|MacroBody], (Code,FunDef,Result=Symbol)):-
  compile_defmacro(Ctx,Env,[Symbol,FormalParms|MacroBody],Macro,Code),
  debug_var('DefMacroResult',Result),
  FunDef = (set_opv(Macro,type_of,sys_macro),set_opv(Symbol,symbol_function,Macro)).  

compile_defmacro(Ctx,Env,[Symbol|FormalParmsMacroBody], Macro, (CompileBody,assert_lsp(Symbol,MacroAssert))):-
  debug_var('MFResult',MFResult),debug_var('FResult',FResult),
  compile_macro_function(Ctx,Env,Symbol,FormalParmsMacroBody,Macro,HeadParms,Whole,EnvAssign,HeadCode,MFBody,MFResult,CompileBody),
  foc_operator(Ctx,Env,kw_special,Symbol,_Len, Special),
  debug_var('MacroEnv',Env),  
  append([Macro|[Whole,Env]],[MFResult],CallableHeadMF), CallableMF =.. CallableHeadMF,
  append([Special,Env|HeadParms],[FResult],CallableHeadSF), CallableSF =.. CallableHeadSF,
  body_cleanup_keep_debug_vars(Ctx,
     (((CallableSF  :- ((CallableMF,f_sys_env_eval(Env,MFResult,FResult))))),
      ((CallableMF  :- ((nop(defmacro),EnvAssign,HeadCode,MFBody))))),
       MacroAssert).



/*  (I dont quite know if this relates .. but in my system, (and CYC) each user assertion A has an enforcement strength which allows the system to decide how to allow the future worlds to inheret A of the present:  
   "[]A" "A is monotonically true"
   or  "<>A -> []A" which says "Only if/when A is actualy possible would A be true"
   Though what i failed to mention is A is not required to be single arity 0 Propostion but can represent a single Axiom (an implicative rule) or Fact o                                                                                                                                                                                                                                                                                              r a slury of Axioms/Facts
*/

% MACROLET
wl:init_args(1,macrolet).
mf_macrolet(Inits,Progn,Result):- reenter_lisp(Ctx,Env), compile_macro_ops(Ctx,Env,Result,[macrolet,Inits|Progn],Code), always(Code).  

compile_macro_ops(Ctx,Env,Result,[macrolet,MACROLETS|Progn], (maplist(always,Decls),
   must_compile_progn([FBOUNDS|Ctx],[FBOUNDS|Env],Result,Progn, CompileBody),CompileBody)):- 
    must_maplist(define_each_macro(Ctx,Env,macrolet),MACROLETS,FBOUNDS,Decls).
    
    

define_each_macro(_Ctx,_Env,_MacroLet,[Symbol,Params|Body],(fbound(Symbol,kw_macro)=[lambda,Params,[block,Symbol|Body]]),true) :- !.

define_each_macro(Ctx,Env,_MacroLet,[Symbol,Params|Body],fbound(Symbol,kw_macro)=UniqueMacroName,CompileBody)  :-    
 gensym(Symbol,UniqueSymbol),atom_concat('mf_',Symbol,UniqueSymbol),
 compile_macro(Ctx,Env,[Symbol,Params|Body],UniqueMacroName,CompileBody),
   always(CompileBody).


compile_macro(Ctx,Env,[Symbol|FormalParmsMacroBody], Macro, (CompileBody,assert_lsp(Symbol,MacroAssert))):-
  debug_var('MFResult',MFResult),
  compile_macro_function(Ctx,Env,Symbol,FormalParmsMacroBody,Macro,HeadParms,_Whole,EnvAssign,HeadCode,MFBody,MFResult,CompileBody),       
  append([Macro|HeadParms],[MFResult],CallableHeadV), CallableMF =.. CallableHeadV,   
    % CallableMF =.. [MF,Whole,Env],
  body_cleanup_keep_debug_vars(Ctx,
     ((CallableMF  :- ((nop(defmacro),EnvAssign,HeadCode,MFBody)))),
       MacroAssert).
   

compile_macro_function(Ctx,Env,Symbol,[FormalParms|MacroBody0],Macro,HeadParms,Whole,EnvAssign,HeadCode,MFBody,MFResult,CompileBody):-
   maybe_get_docs(function,Symbol,MacroBody0,MacroBody,DocCode),

   (var(Macro) -> (always(foc_operator(Ctx,Env,kw_macro,Symbol,_Len, Macro0)),suffix_by_context(Ctx,Macro0,Macro)); 
     true),


   LabelSymbol = '', % LabelSymbol =Symbol       
 within_labels_context(Ctx,LabelSymbol,((
   make_head_params(Ctx,Env,Symbol,Macro,FormalParms,Whole,RequiredArgs,RestNKeys,HeadParms,ZippedArgEnv,HeadDefCode,HeadCode), 
   del_attr(Whole,freeze),
   nop((contains_var(Whole,ZippedArgEnv)->append([Symbol|RequiredArgs],RestNKeys,Whole);true)),
   append([Symbol|RequiredArgs],RestNKeys,Whole),
   make_env_append(Ctx,Env,HeadEnv,ZippedArgEnv,EnvAssign),
   must_compile_body(Ctx,HeadEnv,MFResult,[block,Symbol|MacroBody],MFBody), 
   body_cleanup_keep_debug_vars(Ctx,((DocCode,
     assert_lsp(Symbol,wl:lambda_def(defmacro,Symbol,Macro, FormalParms, MacroBody)),HeadDefCode)),CompileBody)))).















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
  reenter_lisp(Ctx,Env),nth_param(Optionals,1,[],MacroEnv),
  macroexpand_1_or_fail(Ctx,[MacroEnv|Env],LispCode,R)->f_values_list([R,t],Result);f_values_list([LispCode,[]],Result).

% macroexpand
wl:init_args(0,macroexpand_1).
f_macroexpand([LispCode|Optionals],Result):-
  reenter_lisp(Ctx,Env),nth_param(Optionals,1,[],MacroEnv),
  macroexpand_all(Ctx,[MacroEnv|Env],LispCode,R),!,
  (R\==LispCode->f_values_list([R,t],Result);f_values_list([R,[]],Result)).

wl:interned_eval(("`sys:set-symbol-macro-function")).
f_sys_set_symbol_macro_function(Symbol,FunctionSpec,ResultIsMacro):- 
  as_symbol_macro_function(_Ctx,_Env,Symbol,FunctionSpec,ResultIsMacro),
  set_opv(Symbol,symbol_function,ResultIsMacro),set_opv(ResultIsMacro,type_of,sys_macro).

eval(_Ctx, Env, Eval,Result):- eval(Eval, Env, Result).

as_symbol_macro_function(Ctx,Env,_Symbol,function(MF),ResultIsMacro):-eval(Ctx,Env,function(MF),ResultIsMacro).
as_symbol_macro_function(Ctx,Env,_Symbol,MF,ResultIsMacro):- \+ is_list(MF),eval(Ctx,Env,MF,ResultIsMacro).
as_symbol_macro_function(Ctx,Env,Symbol,FormalParmsMacroBody,Macro):- assertion(is_list(FormalParmsMacroBody)),    
  % debug_var('MFResult',MFResult),debug_var('FnResult',FResult),
  compile_macro_function(Ctx,Env,Symbol,FormalParmsMacroBody,Macro,HeadParms,_Whole,EnvAssign,HeadCode,MFBody,MFResult,CompileBody),
  MFResult=FResult,
     append([Macro|HeadParms],[FResult],CallableHeadV), CallableMF =.. CallableHeadV,   
     % CallableMF =.. [MF,Whole,Env],
    body_cleanup_keep_debug_vars(Ctx,
      ((CallableMF  :- ((nop(as_symbol_macro_function),EnvAssign,HeadCode,MFBody)
        ))),
        MacroAssert),
    always((CompileBody,assert_lsp(Symbol,MacroAssert))).


macroexpand_all(Ctx,Env,LispCode,Result):-
  macroexpand_1_or_fail(Ctx,Env,LispCode,Mid) ->
    macroexpand_all(Ctx,Env,Mid,Result) ; Result=LispCode.


% (MACROLET ((foo (&environment env)(IF (macro-function 'bar env) ''yes ''no) ) )(LIST (foo)(MACROLET ((bar NIL :beep) )(foo) ) ) )

f_macro_function(Procedure,Env,ProposedName):-
    (find_operator(Env,Env,kw_macro,Procedure,_ArgsLen, ProposedName);ProposedName=[]),!.
f_macro_function(Procedure,ProposedName):-
    (find_operator(_Ctx,_Env,kw_macro,Procedure,_ArgsLen, ProposedName);ProposedName=[]),!.


get_macro_function(Ctx,Env,Procedure,Arguments,MFResult,CallBody):-  
   once(notrace(find_operator(Ctx,Env,Type,Procedure,Arguments, ProposedName))),
   Type == kw_macro,
   atom(ProposedName),
   Macroexpand1 =.. [ ProposedName ,[Procedure| Arguments] , MFResult],
   clause_interface(Macroexpand1,CallBody).

get_macro_function(Ctx,Env,Procedure,Arguments,MResult,CallBody):-  atom(Procedure),
   length(Arguments,ArgsLen),
   (find_operator(Ctx,Env,kw_macro,Procedure,ArgsLen, ProposedName)),!,
   notrace(align_args_or_fallback1(Ctx,Env,Procedure,ProposedName,Arguments,_FnResult,ArgsPlusResult)),
   ExpandedMacro =.. [ ProposedName | ArgsPlusResult],
   get_mf_interface(Env,ExpandedMacro,CallBody,MResult).



get_mf_interface(Env,MacroSymbol,CallBody,MResult):- 
   MacroSymbol =.. [ ProposedName | ArgsPlusResult],
   atom_concat('mf_',ProposedName,SFME),
   MacroSymbol_macroexpand1_ =.. [ SFME , Env | ArgsPlusResult],
   clause_interface(MacroSymbol_macroexpand1_,CallBody),
   append(_,[MResult],ArgsPlusResult).

get_mf_interface(Env,OperatorSymbol,CallBody,MResult):- 
   OperatorSymbol =.. [ ProposedName | ArgsPlusResult],
   atom_concat('sf_',ProposedName,SFME),
   OperatorSymbol_macroexpand1_ =.. [ SFME,Env | ArgsPlusResult],
   clause_interface(OperatorSymbol_macroexpand1_,Conj),
   unify_conj(Conj,(CallBody,f_sys_env_eval(Env,MResult, _))).

unify_conj(Conj,To):- nonplainvar(Conj),Conj=To,!.
unify_conj((CA,(CB,CC)),(A,B)):- var(A),nonplainvar(B),!, unify_conj(((CA,CB),CC),(A,B)).
unify_conj((CA,(CB,CC)), AB):- unify_conj(((CA,CB),CC),AB).

wl:declared_as(sys_is,interpret).
macroexpand_1_or_fail(_Ctx,Env,Symbol,Macro):- atom(Symbol),!,
  expand_symbol_macro(Env,Symbol,Macro),!,Symbol\==Macro.

macroexpand_1_or_fail(_Ctx,_Env,[Procedure|_],_):-  
  atom(Procedure),wl:declared_as(Procedure,interpret),!,fail.

macroexpand_1_or_fail(Ctx,Env,[Procedure|Arguments],MFResult):- atom(Procedure),
   get_symbol_fbounds(Ctx,Env,Procedure,kw_macro,EXEPR),
   EXEPR = [lambda,FormalParms|LambdaExpression],!,
   debug_var('MFResult',MFResult),   
   quotify_each(Ctx,Env,QuotedArgs,Arguments,Code),
   always(Code),
   Expr = [[lambda,FormalParms|LambdaExpression]|QuotedArgs],
   dbginfo(foo1(Expr)),   
   lisp_compile(Ctx,Env,MFResult,Expr,BodyCode),
   body_cleanup_keep_debug_vars(Ctx,(Code,BodyCode),SCode),
   copy_term(SCode,SCodeO),always(BodyCode),
   dbginfo((macroResult([Procedure|Arguments],SCodeO,MFResult))),!.

macroexpand_1_or_fail(Ctx,Env,[Procedure|Arguments],MResult):- atom(Procedure), nonplainvar(Procedure),   
   get_macro_function(Ctx,Env, Procedure, Arguments, MResult, CallBody),!,always(CallBody),!.
        
macroexpand_1_or_fail(Ctx,Env,[Procedure|Arguments],MFResult):- atom(Procedure),
   find_operator(Ctx,Env,kw_macro,Procedure,Arguments,MFName), atom(MFName),
   is_defined(MFName,3),!,call(MFName,[Procedure|Arguments],[Ctx,Env],MFResult).

macroexpand_1_or_fail(Ctx,Env,[Procedure|Arguments],CompileBody0Result):- 
   atom(Procedure), nonplainvar(Procedure),
   get_lambda_def(Ctx,Env,defmacro,Procedure, FormalParms, LambdaExpression),!,
   debug_var('MEnv',Env),debug_var('NewEnv',NewEnv),debug_var('CommaResult',CommaResult),
   make_bind_parameters(Ctx,Env,FormalParms,Whole,Arguments,NewEnv,BinderCode),!,   
   Whole = [Procedure|Arguments],
   ignore(Ctx =  Env),
   always(BinderCode),
   %lisp_compile(Ctx,Env,MFResult,Expr,BodyCode),
   dmsg(LambdaExpression),
   always(expand_commas(Ctx,1,NewEnv,CommaResult,LambdaExpression,CodeS)),
   body_cleanup_keep_debug_vars(Ctx,CodeS,Code),
   dmsg(:- (Code)),
   % (local_override(with_forms,lisp_grovel)-> (lisp_dumpST) ; true),
   % trace,
   always(Code),
   must_compile_body(Ctx,NewEnv,CompileBody0Result,CommaResult, MCBR),
   always(MCBR),
   dbginfo((macroResult([Procedure|Arguments],Code,CommaResult,CompileBody0Result))),!.

/*
macroexpand_1_or_fail(Ctx,Env,[Procedure|Arguments],MFResult):- atom(Procedure), nonplainvar(Procedure),
   get_lambda_def(Ctx,Env,defmacro,Procedure, FormalParms, LambdaExpression),!,   
  debug_var('MFResult',MFResult),
   quotify_each(Ctx,Env,QuotedArgs,Arguments,Code),
   always(Code),
   Expr = [[lambda,FormalParms|LambdaExpression]|QuotedArgs],
   dbginfo(foo3(Expr)),  
   lisp_compile(Ctx,Env,MFResult,Expr,BodyCode),   
   copy_term(BodyCode,SCodeO),!,
   always(BodyCode),
   dbginfo((macroResult([Procedure|Arguments],SCodeO,MFResult))),!.

macroexpand_1_or_fail(Ctx,Env,[Procedure|Arguments],MFResult):- atom(Procedure),
   get_lambda_def(Ctx,Env,defmacro,Procedure, FormalParms, LambdaExpression),!,
   debug_var('MFResult',MFResult),  
   quotify_each(Ctx,Env,QuotedArgs,Arguments,Code),
   always(Code),
   Expr = [[lambda,FormalParms|LambdaExpression]|QuotedArgs],
   dbginfo(foo2(Expr)),   
   lisp_compile(Ctx,Env,MFResult,Expr,BodyCode),
   body_cleanup_keep_debug_vars(Ctx,(Code,BodyCode),SCode),
   copy_term(SCode,SCodeO),!,
   always(BodyCode),
   dbginfo((macroResult([Procedure|Arguments],SCodeO,MFResult))),!.
*/

/*


macroexpand_1_or_fail(Ctx,Env,[Procedure|Arguments],CompileBody0Result):- atom(Procedure), nonplainvar(Procedure),
   get_lambda_def(Ctx,Env,defmacro,Procedure, FormalParms, LambdaExpression),!,
   debug_var('MEnv',Env),debug_var('NewEnv',NewEnv),debug_var('CommaResult',CommaResult),  
   must_bind_para meters(Ctx,Env,Whole,_RestNKeys,FormalParms,Procedure, Arguments,NewEnv,BinderCode),!,
   ignore(Ctx =  Env),
   always(BinderCode),
   always(expand_commas(Ctx,1,NewEnv,CommaResult,LambdaExpression,CodeS)),
   body_cleanup_keep_debug_vars(Ctx,CodeS,Code),
   % (local_override(with_forms,lisp_grovel)-> (lisp_dumpST) ; true),
   always(Code),
   must_compile_body(Ctx,NewEnv,CompileBody0Result,CommaResult, MCBR),
   always(MCBR),
   dbginfo((macroResult([Procedure|Arguments],Code,CommaResult,CompileBody0Result))),!.

*/


:- fixup_exports.

