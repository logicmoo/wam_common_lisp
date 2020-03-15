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
:- module(fn, []).

:- set_module(class(library)).

:- include('./header').

:- discontiguous compile_defun_ops/5.

% DEFUN 
wl:init_args(2,defun).
% (DEFUN (SETF CAR) ....)


%f_defun(Symbol,FormalParms,FunctionBody,Result):- is_setf_op(Symbol,Accessor),!,sf_defsetf(Accessor,[FormalParms|FunctionBody],Result).
% DEFUN SYMBOL
sf_defun(Env,Symbol,FormalParms,FunctionBody,Return):- reenter_lisp(Ctx,Env),compile_defun_ops(Ctx,Env,Return,[defun,Symbol,FormalParms|FunctionBody],Code),cmpout(Code).
compile_defun_ops(Ctx0,Env,Result,[defun,[setf,Name],FormalParms|FunctionBody], (PushDecl,CODE)):-
  combine_setfs([setf,Name],Symbol),
  PushDecl = assert_lsp(Name,wl:declared_as(Name,defun_setf(Symbol))),
  %compile_defun_ops(Ctx0,Env,Result,[defun,Symbol,['&environment','$env'|FormalParms]|FunctionBody], CODE),!. 
  compile_defun_ops(Ctx0,Env,Result,[defun,Symbol,FormalParms|FunctionBody], CODE),!. 
compile_defun_ops(Ctx0,Env,Result,[defun,Symbol,FormalParms|FunctionBody], (Code,FunDef,Result=Symbol)):-  
  duplicate_term(Ctx0,Ctx),Ctx0=Ctx,
  always(foc_operator(Ctx,Env,kw_function,Symbol,_Len, Function)),
  % FunDef = (set_opv(Symbol,symbol_function,Function),   set_opv(Function,type_of,compiled_function)),
  FunDef = (set_opv(Symbol,symbol_function,Function)), 
  always(FunDef),
  compile_function(Ctx,Env,[Symbol,FormalParms|FunctionBody],_Sym,Function,Code),
  debug_var('DefunResult',Result),
  always(Code).  

% FLET
wl:init_args(1,flet).
sf_flet(Env,Inits,Progn,Result):- reenter_lisp(Ctx,Env), compile_defun_ops(Ctx,Env,Result,[flet,Inits|Progn],Code), !, always(Code). 
compile_defun_ops(Ctx,Env,Result,[flet,FLETS|Progn], (Conj,CompileBody)):- 
    must_maplist(define_each(Ctx,Env,flet),FLETS,FBOUNDS,Decls),
    list_to_conjuncts(Decls,Conj),
    must_compile_progn([FBOUNDS|Ctx],[FBOUNDS|Env],Result,Progn, CompileBody),!.    

% LABELS
wl:init_args(1,labels).
sf_labels(Env,Inits,Progn,Result):- reenter_lisp(Ctx,Env),compile_defun_ops(Ctx,Env,Result,[labels,Inits|Progn],Code),always(Code).  
compile_defun_ops(Ctx,Env,Result,[labels,LABELS|Progn], (Conj,CompileBody)):-
    must_maplist(define_each(Ctx,Env,labels),LABELS,FBOUNDS,Decls),
    list_to_conjuncts(Decls,Conj),
    must_maplist(add_symbol_fbounds(Ctx,Env),FBOUNDS),
    must_compile_progn(Ctx,Env,Result,Progn, CompileBody).   

% wl:needs_env(f_special_operator_p).
define_each(Ctx,Env,_LabelsOrFLET,[SymbolName|DEFN],(fbound(Sym,kw_function)=function(UniqueCtxFunction)),CompileBody)  :-    
    combine_setfs(SymbolName,Symbol),
   (always(foc_operator(Ctx,Env,kw_function,Symbol,_Len, Function)),suffix_by_context(Ctx,Function,CtxFunction)),
   gensym(CtxFunction,UniqueCtxFunction),
   compile_function(Ctx,Env,[Symbol|DEFN],Sym,UniqueCtxFunction,CompileBody).

% undefine_each(Ctx,Env,What,Gensym,DEFN,New,CompileBody)  :- nop(dbginfo(undefine_each(Ctx,Env,What,Gensym,DEFN,New,CompileBody))).

varuse:attr_unify_hook(_,Other):- trace,var(Other).


compile_function(Ctx,Env,[Symbol,FormalParms|FunctionBody0],Symbol,CtxFunction,CompileBodyOpt):-

   (var(CtxFunction) -> (always(foc_operator(Ctx,Env,kw_function,Symbol,_Len, Function)),suffix_by_context(Ctx,Function,CtxFunction)); 
     true),

   LabelSymbol = '', % LabelSymbol =Symbol 
   within_labels_context(Ctx,LabelSymbol,((
   always(maybe_get_docs(function,Symbol,FunctionBody0,FunctionBody,DocCode)),
   
   debug_var("Env",Env),
   debug_var('FnResult',Result),      
     (make_head_params(Ctx,Env,Symbol,CtxFunction,FormalParms,Whole,RequiredArgs,RestNKeys,HeadParms,ZippedArgEnv,HeadDefCode,HeadCode),      
      must_compile_body(Ctx,HeadEnv,Result,[block,Symbol|FunctionBody],BodyCode))),      
   append([CtxFunction|HeadParms],[Result],HeadV),
   CallableHead =.. HeadV,
   make_env_append(Ctx,Env,HeadEnv,ZippedArgEnv,EnvAssign),
   nop((contains_var(Whole,ZippedArgEnv)->append([Symbol|RequiredArgs],RestNKeys,Whole);true)),
 CompileBody = (
   DocCode,
   assert_lsp(Symbol,wl:lambda_def(defun,Symbol,CtxFunction, FormalParms, FunctionBody)),
   HeadDefCode,
   assert_lsp(Symbol,CallableHead  :- (EnvAssign,BodyCodeO))),
 debug_var('Result',Result), 
  body_cleanup_keep_debug_vars(Ctx,(HeadCode,BodyCode),BodyCodeO),
  body_cleanup_keep_debug_vars(Ctx,CompileBody,CompileBodyOpt))).

%global_env(_Env).
:- fixup_exports.

end_of_file.

/*
 (flet ((temp (n) (+ n n)))
    (flet ((temp (n) (+ 2 (temp n))))
      (temp 2)))



    (flet ((temp (n) (+ 2 n))) (temp 2))


(FLET ((m NIL m) )
   (FLET ((femp (n)(+ n n) ) )(LET ((a 2) ) a))
   (labels ((lemp (n)(+ n n)))(LET ((b 2) ) b))
   (macrolet ((macr (n)`(+ ,n ,n))) (defun ttt () (macr 2)))
  (wemp 5))

(macrolet ((macr (n)`(+ ,n ,n))) (let ((a (macr 2))) a))


*/

