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



:- nb_setval('$labels_suffix','').
suffix_by_context(_Ctx,Atom,SuffixAtom):- nb_current('$labels_suffix',Suffix),atom_concat_suffix(Atom,Suffix,SuffixAtom).
suffixed_atom_concat(Ctx,L,R,LRS):- atom_concat_or_rtrace(L,R,LR),suffix_by_context(Ctx,LR,LRS).
push_labels_context(Ctx,Atom):- suffix_by_context(Ctx,Atom,SuffixAtom),b_setval('$labels_suffix',SuffixAtom).
within_labels_context(_Ctx,Label,G):- nb_current('$labels_suffix',Suffix),
   setup_call_cleanup(b_setval('$labels_suffix',Label),G,b_setval('$labels_suffix',Suffix)).
gensym_in_labels(Ctx,Stem,GenSym):- suffix_by_context(Ctx,Stem,SuffixStem),gensym(SuffixStem,GenSym).

get_label_suffix(_Ctx,Suffix):-nb_current('$labels_suffix',Suffix).
  

show_ctx_info(Ctx):- term_attvars(Ctx,CtxVars),maplist(del_attr_rev2(freeze),CtxVars),show_ctx_info2(Ctx).
show_ctx_info2(Ctx):- ignore((get_attr(Ctx,tracker,Ctx0),in_comment(show_ctx_info3(Ctx0)))).
show_ctx_info3(Ctx):- is_rbtree(Ctx),!,forall(rb_in(Key, Value, Ctx),fmt9(Key=Value)).
show_ctx_info3(Ctx):- fmt9(ctx=Ctx).
     


   
/*
% progn mismatch?
compile_funop(Ctx,Env,Result,[FN ], Body):- is_list(FN),!,
  trace,must_compile_body(Ctx,Env,Result,FN,Body).

compile_funop(Ctx,Env,Result,[FN | FunctionArgs], Body):- 
   show_call(must_compile_body(Ctx,Env,Result,[eval,[FN| FunctionArgs]],Body)).
*/

as_lisp_funcallable(Atom,Atom):-atom(Atom),!.
as_lisp_funcallable(function(P),P).
as_lisp_funcallable([quote,P],P).
as_lisp_funcallable(P,P):- compound(P),functor(P,F,A),is_lisp_funcallable(F,A).
is_lisp_funcallable(closure,_).

% FUNCTION APPLY
%compile_body(Ctx,Env,Result,['apply',Function|ARGS], Body):- atom(Function)
% FUNCTION FUNCALL
% compile_body(Ctx,Env,Result,['funcall',Function|ARGS], Body):- ...

find_lisp_function(FN,ARITY,ProposedName):-
  find_function_or_macro_name(_Ctx,_Env,FN,ARITY, ProposedName).

make_function_or_macro_call(Ctx,Env,FN,Args,Result,ExpandedFunction):-
   length(Args,ArgsLen),
   find_function_or_macro_name(Ctx,Env,FN,ArgsLen, ProposedName),!,
   align_args_or_fallback(FN,ProposedName,Args,Result,ArgsPlusResult),
   ExpandedFunction =.. [ ProposedName | ArgsPlusResult].


get_each_search_suffix(Ctx,Each):-
   ((get_label_suffix(Ctx,Whole),atomic_list_concat(List,'_',Whole),
   append(_,EachList,List),atomic_list_concat(EachList,'_',Each)))->true;Each=''.


find_function_or_macro_name(Ctx,Env,FN, Len, ProposedName):- 
   get_each_search_suffix(Ctx,Each),atom_concat_suffix(FN,Each,EachFN),
   exists_function_or_macro_name(Ctx,Env,EachFN,Len, ProposedName),!.
find_function_or_macro_name(Ctx,Env,FN, Len, ProposedName):- 
   exists_function_or_macro_name(Ctx,Env,FN,Len, ProposedName),!.
find_function_or_macro_name(Ctx,_Env,FN, _Len, ProposedName):- 
   generate_function_or_macro_name(Ctx,FN,ProposedName),!.




exists_function_or_macro_name(_Ctx,_Env,FN,_Len, ProposedName):- 
  get_opv(FN,function,ProposedName),!.
exists_function_or_macro_name(_Ctx,_Env,FN,ArgLen, ProposedName):-
  some_defined_function_or_macro(FN,ArgLen,['','cl_','pf_','sf_','mf_','f_'],ProposedName),!.
exists_function_or_macro_name(_Ctx,_Env,FN,ArgsLen, ProposedName):- upcase_atom(FN,FN),
    Arity is ArgsLen+1,is_defined(FN,Arity),ProposedName=FN.


generate_function_or_macro_name(Ctx,FN,NewProposedName):-
    maybe_symbol_package(FN,Package),
    (pl_symbol_name(FN,Name) ->
      function_case_name(Name,Package,ProposedName);
      function_case_name(FN,Package,ProposedName)),
   suffix_by_context(Ctx,ProposedName,NewProposedName),!.



eval_uses_exact(F):- quietly((premute_names(F,FF),uses_exact0(FF))).

uses_exact0(F):- wl:init_args(exact_only,F),!.
uses_exact0(FN):-  function_arg_info(FN,ArgInfo),!,
   ArgInfo.complex==0,ArgInfo.opt==0,ArgInfo.rest==0,ArgInfo.env==0,ArgInfo.whole==0,
   length(ArgInfo.names,NN),
   arg_info_count(ArgInfo,req,N),!,
   N==NN.
   

function_arg_info(FN,ArgInfo):- wl:arglist_info(FN,_,_,ArgInfo).
function_arg_info(FN,ArgInfo):- wl:arglist_info(FN,_,_,_,ArgInfo).
function_arg_info(FN,ArgInfo):- wl:arglist_info(_,FN,_,_,ArgInfo).


eval_uses_bind_parameters(F):- quietly((premute_names(F,FF), wl:init_args(bind_parameters,FF))),!.

% eval_uses_exact_and_restkeys(FN,Requireds):- current_predicate(FN/N), Requireds is N-2,Requireds>0.

eval_uses_exact_and_restkeys(F,N):- quietly((premute_names(F,FF), exact_and_restkeys(FF,N))).

exact_and_restkeys(F,N):- wl:init_args(V,F),integer(V),!,V=N.
exact_and_restkeys(F,1):- is_any_place_op(F),!.
exact_and_restkeys(F,_):- uses_exact0(F),!,fail.
exact_and_restkeys(F,N):- function_arg_info(F,ArgInfo),ArgInfo.req=L,ArgInfo.all\==L,!,arg_info_count(ArgInfo,req,N).
exact_and_restkeys(F,0):- uses_rest_only0(F),!.

arg_info_count(ArgInfo,Prop,N):- 
  Value=ArgInfo.Prop,
   (number(Value)->N=Value;
     (is_list(Value)->length(Value,N);
       (atom(Value)->N=1;
         (throw(arg_info_count(ArgInfo,Prop,Value)))))).

premute_names(F,F).
premute_names(F,FF):- atom_concat_or_rtrace('f_',F,FF).
premute_names(F,FF):- atom_concat_or_rtrace('cl_',F,FF).
premute_names(F,FF):- atom_concat_or_rtrace('f_',FF,F).
premute_names(F,FF):- atom_concat_or_rtrace('cl_',FF,F).

eval_uses_rest_only(F):- quietly((premute_names(F,FF),uses_rest_only0(FF))),!.

uses_rest_only0(F):- wl:init_args(0,F),!.
uses_rest_only0(F):- function_arg_info(F,ArgInfo),ArgInfo.req==0,ArgInfo.all\==0,!.
%uses_rest_only0(F):- same_symbol(F,FF),wl:declared(FF,lambda(['&rest'|_])),!.

% Non built-in function expands into an explicit function call

% invoke(r1,r2,r3,RET).
align_args(FN,ProposedName,Args,Result,ArgsPlusResult):- 
  (eval_uses_exact(FN);eval_uses_exact(ProposedName)),
   append(Args, [Result], ArgsPlusResult).

% invoke(r1,r2,[o3,key1,value1],RET).
align_args(FN,ProposedName,Args,Result,ArgsPlusResult):- 
  (eval_uses_exact_and_restkeys(FN,N);eval_uses_exact_and_restkeys(ProposedName,N)),
  always(length(Left,N)),append(Left,Rest,Args),
  append(Left, [Rest,Result], ArgsPlusResult).

% invoke([r1,r2,r3],RET).
align_args(FN,ProposedName,Args,Result,[Args,Result]):-
  (eval_uses_rest_only(FN);eval_uses_rest_only(ProposedName)).

% invoke([r1,r2,r3],RET).
align_args(FN,ProposedName,Args,Result,[Args,Result]):-
  (eval_uses_bind_parameters(FN);eval_uses_bind_parameters(ProposedName)).


% guess invoke(r1,RET).
%align_args(_FN,ProposedName,[Arg],Result,[Arg,Result]):- 
% is_defined(ProposedName,2),is_defined(ProposedName,3).

% guess invoke([r1,r2,r3],RET).
%align_args(FN,ProposedName,Args,Result,[Args,Result]):- 
%  only_arity(FN,2);only_arity(ProposedName,2).

  
% fallback to invoke([r1,r2,r3],RET).
%align_args(FN, ProposedName,Args,Result,[Args,Result]):- 
%  (is_lisp_operator(FN) ; is_lisp_operator(ProposedName)),!.

/*
% guess invoke(r1,r2,r3,RET).
*/
align_args_or_fallback(FN,ProposedName,Args,Result,ArgsPlusResult):- align_args(FN,ProposedName,Args,Result,ArgsPlusResult),!.
align_args_or_fallback(_,_ProposedName,Args,Result,ArgsPlusResult):- append(Args, [Result], ArgsPlusResult).


only_arity(ProposedName,N):-
  is_defined(ProposedName,N),
  forall((between(0,6,Other),Other\=N),  \+ is_defined(ProposedName,Other)).

is_defined(ProposedName,N):- functor(G,ProposedName,N),current_predicate(_,G).

maybe_symbol_package(Symbol,Package):-  get_opv(Symbol,package,Package),!.
maybe_symbol_package(_Symbol,Package):- reading_package(Package).


some_defined_function_or_macro(FN,ArgLen,[Name|NameS],NewName):-
   atom_concat_or_rtrace(Name,FN,ProposedPName),   
   (((ProposedPName = ProposedName; prologcase_name(ProposedPName,ProposedName)),
    guess_lisp_functor(P,ProposedName,ArgLen),current_predicate(_,P),
    \+ predicate_property(user:P,imported_from(system)))-> ProposedName=NewName;
   some_defined_function_or_macro(FN,ArgLen,NameS,NewName)).

guess_lisp_functor(P,F,ArgLen):- (number(ArgLen)->A is ArgLen+1; A= _),!,guess_prolog_functor(P,F,A). 
guess_prolog_functor(P,F,A):- (var(F);var(A)),!,current_predicate(F/A),functor(P,F,A).
guess_prolog_functor(P,F,A):- functor(P,F,A).


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



