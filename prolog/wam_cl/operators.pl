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
:- module(decls, []).
:- set_module(class(library)).
:- include('header').
:- set_module(class(library)).
:- ensure_loaded(utils_for_swi).


cl_special_operator_p(Obj,RetVal):- t_or_nil(is_lisp_operator(Obj),RetVal).

cl_functionp(Obj,RetVal):- t_or_nil(is_functionp(Obj),RetVal).
is_functionp(X):- \+ atom(X),!,fail.
is_functionp(X):- atom_concat_or_rtrace('f_',_,X),!.
is_functionp(X):- atom_concat_or_rtrace('cl_',_,X),!.
is_functionp(X):- atom(X),current_predicate(X/N),N>0. 


% DEFSETF (short form)
%compile_body(Ctx,Env,Symbol,[defsetf,AccessFun,UpdateFn],assert(defsetf_short(AccessFun,UpdateFn))).
%compile_body(Ctx,Env,Symbol,[defsetf,AccessFun,FormalParms,Variables|FunctionBody0],assertz(defsetf_short(AccessFun,UpdateFn)))

% handler-caserestart-casedestructuring-bind

% SOURCE GROVEL MODE
%compile_body(_Ctx,_Env,[],_, true):- local_override(with_forms,lisp_grovel),!.

% DEFMACRO
compile_decls(Ctx,Env,Symbol,[defmacro,Symbol,FormalParms|FunctionBody0], CompileBody):-
   within_labels_context('', % TOPEVEL
     compile_macro(Ctx,Env,_Function,[Symbol,FormalParms|FunctionBody0], CompileBody)).



% MACROLET
compile_decls(Ctx,Env,Result,[macrolet,[]|Progn], CompileBody):-
   compile_decls(Ctx,Env,Result,[progn,Progn], CompileBody).
compile_decls(Ctx,Env,Result,[macrolet,[MACROLET|MORE]|Progn], CompileBody):-
    compile_macro(Ctx,Env,_New,MACROLET, CompileBody0),
    compile_decls(Ctx,Env,Result,[macrolet,MORE|Progn], CompileBody1),
    CompileBody = (CompileBody0,CompileBody1).

% DEFUN
compile_decls(Ctx,Env,Symbol,[defun,Symbol,FormalParms|FunctionBody], CompileBody):-
   within_labels_context('', % TOPEVEL
     compile_function(Ctx,Env,_Function,[Symbol,FormalParms|FunctionBody], CompileBody)).
    
% LABELS
compile_decls(Ctx,Env,Result,[LABELS,[]|Progn], CompileBody):-  member(LABELS,[labels,flet]),
   compile_decls(Ctx,Env,Result,[progn,Progn], CompileBody).   
compile_decls(Ctx,Env,Result,[LABELS,[MACROLET|MORE]|Progn], CompileBody):- member(LABELS,[labels,flet]),
    compile_function(Ctx,Env,_New,[MACROLET], CompileBody0),
    compile_decls(Ctx,Env,Result,[LABELS,MORE|Progn], CompileBody1),
    CompileBody = (CompileBody0,CompileBody1).


compile_decls(_Ctx,_Env,Symbol,[Fun,Symbol,A2|AMORE],assert_lsp(P)):- notrace(is_def_at_least_two_args(Fun)),\+ is_fboundp(Fun),!,P=..[Fun,Symbol,A2,AMORE].
compile_decls(_Ctx,_Env,Symbol,[Fun0,Symbol,A2|AMORE],assert_lsp(P)):- notrace((is_def_at_least_two_args(Fun),same_symbol(Fun,Fun0))),\+ is_fboundp(Fun),!,P=..[Fun,Symbol,A2,AMORE].


is_def_at_least_two_args(defgeneric).
is_def_at_least_two_args(define_compiler_macro).
is_def_at_least_two_args(define_method_combination).
is_def_at_least_two_args(define_setf_expander).
is_def_at_least_two_args(defmethod).
is_def_at_least_two_args(defsetf).
is_def_at_least_two_args(deftype).
is_def_at_least_two_args(symbol_macrolet).

combine_setfs(Name0,Name):-atom(Name0),!,Name0=Name.
combine_setfs(Name0,Name):-atomic_list_concat(['f_combined'|Name0],'__',Name).


wl:init_args(1,cl_labels).
cl_labels(Inits,Progn,Result):-
  reenter_lisp(Ctx,Env),
  compile_decls(Ctx,Env,Result,[labels,Inits|Progn],Code),
  always(Code).  

wl:init_args(1,cl_macrolet).
cl_macrolet(Inits,Progn,Result):-
  reenter_lisp(Ctx,Env),
  compile_decls(Ctx,Env,Result,[macrolet,Inits|Progn],Code),
  always(Code).  


wl:init_args(1,cl_flet).
cl_flet(Inits,Progn,Result):-
  reenter_lisp(Ctx,Env),
  compile_decls(Ctx,Env,Result,[flet,Inits|Progn],Code),
  always(Code).  

wl:init_args(2,cl_defmacro).
cl_defmacro(Name,FormalParms,FunctionBody,Result):-
  reenter_lisp(Ctx,Env),
  compile_decls(Ctx,Env,Result,[defmacro,Name,FormalParms|FunctionBody],Code),
  always(Code).  
  

compile_macro(Ctx,CallEnv,Macro,[Name0,FormalParms|FunctionBody0], CompileBody):-
   combine_setfs(Name0,Combined),
   suffix_by_context(Combined,Symbol),
   always(find_function_or_macro_name(Ctx,CallEnv,Symbol,_Len, Macro)),
   add_alphas(Ctx,Macro),
   always(maybe_get_docs(function,Macro,FunctionBody0,FunctionBody,DocCode)),
   %reader_intern_symbols
   MacroHead=[Macro|FormalParms],
   FunDef = (set_opv(Macro,classof,claz_macro),set_opv(Symbol,compile_as,kw_operator),set_opv(Symbol,function,Macro)),
   FunDef,
   within_labels_context(Symbol, make_mcompiled(Ctx,CallEnv,MResult,Symbol,MacroHead,FunctionBody,
     NewMacroHead,HeadDefCode,BodyCode,Fun)),
   %NewMacroHead=..[M|ARGS],RNewMacroHead=..[MM|ARGS], atom_concat_or_rtrace(M,'_mexpand1',MM),   
   %get_alphas(Ctx,Alphas),
   debug_var('FnResult',FResult),debug_var('Fun',Fun),
   subst(NewMacroHead,MResult,FResult,FunctionHead),
   
 CompileBody = (
   DocCode,
   HeadDefCode,
   assert_lsp(wl:lambda_def(defmacro,(Name0),Macro, FormalParms, [progn | FunctionBody])),
   assert_lsp((user:FunctionHead  :- 
    (BodyCode, 
       cl_eval(MResult,FResult)))),
   % nop((user:RNewMacroHead  :- BodyCode)),
   FunDef).

varuse:attr_unify_hook(_,Other):- var(Other).

make_mcompiled(Ctx,_UnusedEnv,CResult,Symbol,FunctionHead,FunctionBody,Head,HeadDefCode,(BodyCode),Fun):-
    always(((expand_function_head(Ctx,CallEnv,FunctionHead, Head, HeadEnv, CResult,HeadDefCode,HeadCode)),
    debug_var("CallEnv",CallEnv),debug_var('CResult',CResult),debug_var('MResult',MResult),
    compile_body(Ctx,CallEnv,MResult,[block,Symbol|FunctionBody],Body0),
    show_ctx_info(Ctx),
   ((fail,sub_term(Sub,Body0),compound(Sub),(Sub= (Body=Var)),var(Var),Var==MResult, is_list(Body))->
     (lisp_compile(CResult,Body,BodyCode0),
      subst(Body0,Sub,BodyCode0,BodyCode),Fun=t);
    (((var(MResult)))
    -> (MResult=CResult,body_cleanup_keep_debug_vars(Ctx,((CallEnv=HeadEnv,HeadCode,Body0)),BodyCode))
     ; (MResult=CResult,body_cleanup_no_optimize(Ctx,((CallEnv=HeadEnv,HeadCode,Body0,MResult=CResult)),BodyCode)))))).


wl:init_args(2,cl_defun).
cl_defun(Name,FormalParms,FunctionBody,Result):-
  reenter_lisp(Ctx,Env),
  compile_decls(Ctx,Env,Result,[defun,Name,FormalParms|FunctionBody],Code),
  always(Code).

compile_function(Ctx,Env,Function,[Name,FormalParms|FunctionBody0], CompileBody):-
   combine_setfs(Name,Combined),
   suffix_by_context(Combined,Symbol),
   always(find_function_or_macro_name(Ctx,Env,Symbol,_Len, Function)),
   always(maybe_get_docs(function,Function,FunctionBody0,FunctionBody,DocCode)),
   FunctionHead=[Function|FormalParms],
   within_labels_context(Symbol, make_compiled(Ctx,Env,MResult,Symbol,FunctionHead,FunctionBody,Head,HeadDefCode,BodyCode)),
 CompileBody = (
   DocCode,
   HeadDefCode,
   assert_lsp(wl:lambda_def(defun,(Name),Function, FormalParms, FunctionBody)),
   assert_lsp((user:Head  :- BodyCode)),
   set_opv(Function,classof,claz_compiled_function),
   set_opv(Symbol,compile_as,kw_function),
   set_opv(Symbol,function,Function)),
   debug_var('MResult',MResult).

make_compiled(Ctx,_UnusedEnv,FResult,Symbol,FunctionHead,FunctionBody,Head,HeadDefCode,(BodyCode)):-
   always(( expand_function_head(Ctx,CallEnv,FunctionHead, Head, HeadEnv, FResult,HeadDefCode,HeadCode),
    debug_var("RET",Result),debug_var("CallEnv",CallEnv),debug_var('FResult',FResult),
    %put_attr(FResult,varuse,retval),
    FResult=Result,
    compile_body(Ctx,CallEnv,Result,[block,Symbol|FunctionBody],Body0),
    show_ctx_info(Ctx),
    (((var(Result)))
    -> body_cleanup_keep_debug_vars(Ctx,((CallEnv=HeadEnv,HeadCode,Body0)),BodyCode)
     ; (body_cleanup_no_optimize(Ctx,((CallEnv=HeadEnv,HeadCode,Body0,FResult=Result)),BodyCode))))).



currently_visible_package(P):- reading_package(Package),
  (P=Package;package_use_list(Package,P)).

is_lisp_operator(G):- notrace(lisp_operator(G)).


lisp_operator(defpackage).
lisp_operator(if).
lisp_operator('data-assrt').
lisp_operator('define-caller-pattern').
lisp_operator('define-variable-pattern').
lisp_operator(u_define_caller_pattern).
lisp_operator(f_u_define_caller_pattern).
lisp_operator(S):- nonvar(S),compiler_macro_left_right(S,_,_).
lisp_operator(S):- get_lambda_def(defmacro,S,_,_).
lisp_operator(S):-is_special_op(S,P),currently_visible_package(P).
%lisp_operator(S):-is_special_op(S,_P).

get_lambda_def(DefType,ProcedureName,FormalParams,LambdaExpression):-
  wl:lambda_def(DefType,ProcedureName,_,FormalParams,LambdaExpression).
get_lambda_def(DefType,ProcedureName,FormalParams,LambdaExpression):-
  wl:lambda_def(DefType,_,ProcedureName,FormalParams,LambdaExpression).


is_special_op(S,P):- get_opv(S,compile_as,kw_operator),get_opv(S,package,P).
is_special_op('%%allocate-closures', pkg_sbc).
is_special_op('%cleanup-fun', pkg_sbc).
is_special_op('%escape-fun', pkg_sbc).
is_special_op('%funcall', pkg_sbc).
is_special_op('%primitive', pkg_sys).
is_special_op('%within-cleanup', pkg_sbc).
is_special_op('compiler-let', pkg_ext).
is_special_op('do*', pkg_cl).
is_special_op('eval-when', pkg_cl).
is_special_op('global-function', pkg_sbc).
is_special_op('let*', pkg_cl).
is_special_op('load-time-value', pkg_cl).
is_special_op('multiple-value-bind', pkg_cl).
is_special_op('multiple-value-call', pkg_cl).
is_special_op('multiple-value-list', pkg_cl).
is_special_op('multiple-value-prog1', pkg_cl).
is_special_op('multiple-value-setq', pkg_cl).
is_special_op('nth-value', pkg_cl).
is_special_op('prog*', pkg_cl).
is_special_op('return-from', pkg_cl).
is_special_op('symbol-macrolet', pkg_cl).
% is_special_op('truly-the', 'sb-ext').
is_special_op('unwind-protect', pkg_cl).

is_special_op(block, pkg_cl).
is_special_op(case, pkg_cl).
is_special_op(catch, pkg_cl).
is_special_op(cond, pkg_cl).
is_special_op(do, pkg_cl).
is_special_op(dolist, pkg_cl).
is_special_op(dotimes, pkg_cl).
is_special_op(flet, pkg_cl).
is_special_op(function, pkg_cl).
is_special_op(go, pkg_cl).
is_special_op(if, pkg_cl).
is_special_op(labels, pkg_cl).
is_special_op(lambda, pkg_cl).
is_special_op(let, pkg_cl).
is_special_op(locally, pkg_cl).
is_special_op(macrolet, pkg_cl).
is_special_op(prog, pkg_cl).
is_special_op(prog1, pkg_cl).
is_special_op(prog2, pkg_cl).
is_special_op(progn, pkg_cl).
is_special_op(progv, pkg_cl).
is_special_op(psetq, pkg_cl).
is_special_op(quote, pkg_cl).
is_special_op(return, pkg_cl).
is_special_op(setq, pkg_cl).
is_special_op(tagbody, pkg_cl).
is_special_op(the, pkg_cl).
is_special_op(throw, pkg_cl).
is_special_op(unless, pkg_cl).
is_special_op(while, pkg_user).
is_special_op(u_while, pkg_user).
is_special_op(when, pkg_cl).
is_special_op(defclass, pkg_cl).
is_special_op(defstruct, pkg_cl).

:- fixup_exports.



