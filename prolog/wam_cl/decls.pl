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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 * Changes since 2001:
 *
 *
 *******************************************************************/
:- module(decls, []).
:- set_module(class(library)).
:- include('header.pro').
:- set_module(class(library)).
:- ensure_loaded(utils_for_swi).


% DEFSETF (short form)
%compile_body(Ctx,Env,Symbol,[defsetf,AccessFun,UpdateFn],assert(defsetf_short(AccessFun,UpdateFn))).
%compile_body(Ctx,Env,Symbol,[defsetf,AccessFun,FormalParms,Variables|FunctionBody0],assert(defsetf_short(AccessFun,UpdateFn)))

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


compile_decls(_Ctx,_Env,Symbol,[Fun,Symbol,A2|AMORE],assert(P)):- notrace(is_def_at_least_two_args(Fun)),!,P=..[Fun,Symbol,A2,AMORE].
compile_decls(_Ctx,_Env,Symbol,[Fun0,Symbol,A2|AMORE],assert(P)):- notrace((is_def_at_least_two_args(Fun),same_symbol(Fun,Fun0))),!,P=..[Fun,Symbol,A2,AMORE].

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

cl_defmacro(Name,FormalParms,FunctionBody,Result):-
  reenter_lisp(Ctx,Env),
  compile_decls(Ctx,Env,Result,[defmacro,Name,FormalParms|FunctionBody],Code),
  call(Code).  
  

compile_macro(Ctx,CallEnv,Function,[Name0,FormalParms|FunctionBody0], CompileBody):-
   combine_setfs(Name0,Combined),
   suffix_by_context(Combined,Symbol),
   must_or_rtrace(find_function_or_macro_name(Ctx,CallEnv,Symbol,_Len, Function)),
   add_alphas(Ctx,Function),
   must_or_rtrace(maybe_get_docs(function,Function,FunctionBody0,FunctionBody,DocCode)),
   %reader_intern_symbols
   FunctionHead=[Function|FormalParms],
   set_opv(Function,classof,claz_macro),
   set_opv(Symbol,compile_as,kw_operator),
   set_opv(Symbol,function,Function),
   within_labels_context(Symbol, make_compiled(Ctx,CallEnv,Symbol,FunctionHead,FunctionBody,Head,HeadDefCode,BodyCode)),
   get_alphas(Ctx,Alphas),
 CompileBody = (
   DocCode,
   HeadDefCode,
   asserta(user:macro_lambda(defmacro(Name0),Function, FormalParms, [progn | FunctionBody],Alphas)),
   asserta((user:Head  :- BodyCode)),
   set_opv(Function,classof,claz_macro),
   set_opv(Symbol,compile_as,kw_operator),
   set_opv(Symbol,function,Function)).


cl_defun(Name,FormalParms,FunctionBody,Result):-
  reenter_lisp(Ctx,Env),
  compile_decls(Ctx,Env,Result,[defun,Name,FormalParms|FunctionBody],Code),
  call(Code).

compile_function(Ctx,Env,Function,[Name,FormalParms|FunctionBody0], CompileBody):-
   combine_setfs(Name,Combined),
   suffix_by_context(Combined,Symbol),
   must_or_rtrace(find_function_or_macro_name(Ctx,Env,Symbol,_Len, Function)),
   must_or_rtrace(maybe_get_docs(function,Function,FunctionBody0,FunctionBody,DocCode)),
   FunctionHead=[Function|FormalParms],
   within_labels_context(Symbol, make_compiled(Ctx,Env,Symbol,FunctionHead,FunctionBody,Head,HeadDefCode,BodyCode)),
 CompileBody = (
   DocCode,
   HeadDefCode,
   asserta(user:function_lambda(defun(Name),Function, FormalParms, FunctionBody)),   
   asserta((user:Head  :-  BodyCode)),
   set_opv(Function,classof,claz_compiled_function),
   set_opv(Symbol,compile_as,kw_function),
   set_opv(Symbol,function,Function)).

make_compiled(Ctx,_UnusedEnv,Symbol,FunctionHead,FunctionBody,Head,HeadDefCode,(BodyCode)):-
    expand_function_head(Ctx,CallEnv,FunctionHead, Head, HeadEnv, HResult,HeadDefCode,HeadCode),
    debug_var("RET",Result),debug_var("CallEnv",CallEnv),debug_var('HResult',HResult),
    compile_body(Ctx,CallEnv,Result,[block,Symbol|FunctionBody],Body0),
    show_ctx_info(Ctx),
    (((var(Result),HResult=Result))
    -> body_cleanup(Ctx,((CallEnv=HeadEnv,HeadCode,Body0)),BodyCode)
     ; body_cleanup(Ctx,((CallEnv=HeadEnv,HeadCode,Body0,HResult=Result)),BodyCode)).

:- fixup_exports.



