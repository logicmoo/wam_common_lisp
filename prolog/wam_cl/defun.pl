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

:- include('header').

:- discontiguous compile_defun_ops/5.

% DEFUN 
wl:init_args(2,cl_defun).
% (DEFUN (SETF CAR) ....)
cl_defun(Symbol,FormalParms,FunctionBody,Result):- is_setf_op(Symbol,Accessor),!,cl_defsetf(Accessor,[FormalParms|FunctionBody],Result).
% DEFUN SYMBOL
cl_defun(Symbol,FormalParms,FunctionBody,Return):- reenter_lisp(Ctx,Env),compile_defun_ops(Ctx,Env,Return,[defun,Symbol,FormalParms|FunctionBody],Code),cmpout(Code).
compile_defun_ops(Ctx0,Env,Result,[defun,Symbol,FormalParms|FunctionBody], (Code,FunDef,Result=Symbol)):-
  duplicate_term(Ctx0,Ctx),Ctx0=Ctx,
  compile_function(Ctx,Env,[Symbol,FormalParms|FunctionBody],_Sym,Function,Code),
  debug_var('DefunResult',Result),
  FunDef = (set_opv(Function,classof,claz_function),set_opv(Symbol,compile_as,kw_function),set_opv(Symbol,function,Function)),   
  always((FunDef,Code)).  

% FLET
wl:init_args(1,cl_flet).
cl_flet(Inits,Progn,Result):- reenter_lisp(Ctx,Env), compile_defun_ops(Ctx,Env,Result,[flet,Inits|Progn],Code), !, always(Code). 
compile_defun_ops(Ctx,Env,Result,[flet,FLETS|Progn], (CompileBody)):- 
    must_maplist(define_each(Ctx,Env,flet),FLETS,FBOUNDS,Decls),
    always(Decls),
    compile_forms([FBOUNDS|Ctx],[FBOUNDS|Env],Result,Progn, CompileBody),!.    

% LABELS
wl:init_args(1,cl_labels).
cl_labels(Inits,Progn,Result):- reenter_lisp(Ctx,Env),compile_defun_ops(Ctx,Env,Result,[labels,Inits|Progn],Code),always(Code).  
compile_defun_ops(Ctx,Env,Result,[labels,LABELS|Progn], (must_maplist(must_det_l,Decls),CompileBody)):- 
    must_maplist(define_each(Ctx,Env,labels),LABELS,FBOUNDS,Decls),
    must_maplist(add_symbol_fbounds(Ctx,Env),FBOUNDS),
    compile_forms(Ctx,Env,Result,Progn, CompileBody).   

% wl:needs_env(cl_special_operator_p).
define_each(Ctx,Env,_LabelsOrFLET,[Symbol|DEFN],(fbound(Sym)=bound_type(kw_function,UniqueCtxFunction)),CompileBody)  :-    
   (always(foc_operator(Ctx,Env,Symbol,_Len, Function)),suffix_by_context(Ctx,Function,CtxFunction)),
   gensym(CtxFunction,UniqueCtxFunction),
   compile_function(Ctx,Env,[Symbol|DEFN],Sym,UniqueCtxFunction,CompileBody).

% undefine_each(Ctx,Env,What,Gensym,DEFN,New,CompileBody)  :- nop(dbginfo(undefine_each(Ctx,Env,What,Gensym,DEFN,New,CompileBody))).

varuse:attr_unify_hook(_,Other):- trace,var(Other).


compile_function(Ctx,Env,[Symbol,FormalParms|FunctionBody0],Symbol,CtxFunction,CompileBodyOpt):-

   (var(CtxFunction) -> (always(foc_operator(Ctx,Env,Symbol,_Len, Function)),suffix_by_context(Ctx,Function,CtxFunction)); 
     true),

   LabelSymbol = '', % LabelSymbol =Symbol 
   within_labels_context(Ctx,LabelSymbol,((
   always(maybe_get_docs(function,Symbol,FunctionBody0,FunctionBody,DocCode)),
   
   debug_var("Env",Env),
   debug_var('FnResult',Result),   
     (expand_function_head(Ctx,Env,Symbol,CtxFunction,FormalParms,_Whole, HeadParms,ZippedArgEnv, HeadDefCode,HeadCode),
      must_compile_body(Ctx,HeadEnv,Result,[block,Symbol|FunctionBody],BodyCode))),      
   append([CtxFunction|HeadParms],[Result],HeadV),
   CallableHead =.. HeadV,
   make_env_append(Ctx,Env,HeadEnv,ZippedArgEnv,EnvAssign),
 CompileBody = (
   DocCode,
   assert_lsp(Symbol,wl:lambda_def(defun,Symbol,CtxFunction, FormalParms, FunctionBody)),
   HeadDefCode,
   assert_lsp(Symbol,CallableHead  :- (nop(global_env(Env)),EnvAssign,  BodyCodeO))),
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
