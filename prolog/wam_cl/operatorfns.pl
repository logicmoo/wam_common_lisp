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

wl:declared(f_special_operator_p,needs_env).
f_special_operator_p(Env,Obj,RetVal):- t_or_nil(is_special_operator_p(Env,Obj),RetVal).
f_special_operator_p(Obj,RetVal):- t_or_nil(is_special_operator_p(Obj),RetVal).

is_special_operator_p(Env,Obj):- is_lisp_operator(_,Env,Obj).
is_special_operator_p(Obj):- is_lisp_operator(_,_,Obj).

symbol_foperator(_Symbol).

% GROVELED f_functionp(Obj,RetVal):- t_or_nil(is_functionp(Obj),RetVal).

is_functionp(X):-  atom(X),is_functionp0(X),!.
is_functionp0(X):- atom_concat_or_rtrace('sf_',Symbol,X),!,symbol_foperator(Symbol).
is_functionp0(X):- atom_concat_or_rtrace('f_',Symbol,X),!,symbol_foperator(Symbol).

is_macrop(X):- atom_concat_or_rtrace('mf_',Symbol,X),symbol_foperator(Symbol).


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

remove_symbol_fbounds(Ctx,Sym):-
  remove_env_attribute(Ctx,fbound(Sym)).

get_symbol_fbounds(Ctx,Env,Sym,BoundType,ProposedName):-
  get_env_attribute(Ctx,fbound(Sym),bound_type(BoundType,ProposedName));
   get_env_attribute(Env,fbound(Sym),bound_type(BoundType,ProposedName)).

add_symbol_fbounds(Ctx,Env,Name=Value):- 
  always((set_env_attribute(Ctx,Name,Value),
  set_env_attribute(Env,Name,Value))).
   

show_ctx_info(Ctx):- term_attvars(Ctx,CtxVars),maplist(del_attr_rev2(freeze),CtxVars),show_ctx_info2(Ctx).
show_ctx_info2(Ctx):- ignore((get_tracker(Ctx,Ctx0),in_comment(show_ctx_info3(Ctx0)))).
show_ctx_info3(Ctx):- is_rbtree(Ctx),!,forall(rb_in(Key, Value, Ctx),fmt9(Key=Value)).
show_ctx_info3(Ctx):- fmt9(ctx=Ctx).
     


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
  find_operator(_Ctx,_Env,kw_function,FN,ARITY, ProposedName).
find_lisp_function(FN,ARITY,ProposedName):-
  find_operator(_Ctx,_Env,kw_special,FN,ARITY, ProposedName).

make_function_or_macro_call(Ctx,Env,FN,Args,Result,ExpandedFunction):-
   (is_list(Args)->length(Args,ArgsLen);true),
   foc_operator(Ctx,Env,symbol_function,FN,ArgsLen, ProposedName),!,
   align_args_or_fallback(Ctx,Env,FN, ProposedName,Args,Result,ArgsPlusResult),
   ExpandedFunction =.. [ ProposedName | ArgsPlusResult].


get_each_search_suffix(Ctx,Each):-
   ((get_label_suffix(Ctx,Whole),atomic_list_concat(List,'_',Whole),
   append(_,EachList,List),atomic_list_concat(EachList,'_',Each)))->true;Each=''.


as_funcallable(_Symbol,Function,Funcallable):- is_functionp(Function),!, Function=Funcallable.
as_funcallable(_Symbol,FunctionSymbol,Funcallable):- atom(FunctionSymbol),find_lisp_function(FunctionSymbol, _Len, Funcallable),!.
as_funcallable(_Symbol,function(Function),Funcallable):- is_functionp(Function),!, Function=Funcallable.
as_funcallable(_Symbol,function(FunctionSymbol),Funcallable):- atom(FunctionSymbol),find_lisp_function(FunctionSymbol, _Len, Funcallable),!.
as_funcallable(Symbol,LAMBDAAndBody,Funcallable):- 
    compile_function(_Ctx,_Env,[Symbol|LAMBDAAndBody],Symbol,Funcallable,CompileBodyOpt),always(CompileBodyOpt).


find_operator(Ctx,Env,BindType,FN, Len, ProposedName):- assertion(nonvar(FN)),
   get_each_search_suffix(Ctx,Each),atom_concat_suffix(FN,Each,EachFN),
   existing_operator(Ctx,Env,BindType,EachFN,Len, ProposedName),!.
find_operator(Ctx,Env,BindType,FN, Len, ProposedName):- 
   existing_operator(Ctx,Env,BindType,FN,Len, ProposedName),!.

find_operator_else_function(Ctx,Env,BindType,Symbol,ProposedName,true):- 
  find_operator(Ctx,Env,BindType,Symbol, _Len, ProposedName),!.
find_operator_else_function(_Ctx,Env,BindType,Symbol,ProposedName,Pre):- 
   Pre = find_operator_or_die(Env,BindType,Symbol, ProposedName),!.
find_operator_else_function(_Cxt,_Env,_BindType,Symbol,function(Symbol),true).

find_operator_or_die(Env,BindType,Symbol, ProposedName):- nonvar(Symbol), find_operator(Env,Env,BindType,Symbol, _Len, ProposedName),!.
find_operator_or_die(_Env,kw_function,Symbol, function(Symbol)).
find_operator_or_die(Env,BindType,Symbol, R):- trace_or_throw(find_operator_or_die(Env,BindType,Symbol, R)).



f_sys_coerce_to_function(FN,ProposedName):- find_lisp_function(FN,_ARITY,ProposedName).

foc_operator(Ctx,Env,BindType,FN, Len, ProposedName):-  find_operator(Ctx,Env,BindType,FN, Len, ProposedName),!.
foc_operator(Ctx,_Env,BindType,FN, _Len, ProposedName):- 
  show_call_trace((generate_function_or_macro_name(Ctx,FN,BindType,ProposedName))),!.



existing_operator(Ctx,Env,BindType,FN, _Len, ProposedName):-  show_success(get_symbol_fbounds(Ctx,Env,FN,BindType,ProposedName)),!.

existing_operator(_Ctx,_Env,kw_function,FN,_Len, ProposedName):- get_opv(FN,symbol_function,ProposedName),
   (latom_starts_with(ProposedName,'f_');latom_starts_with(ProposedName,'sf_')).
existing_operator(_Ctx,_Env,kw_special,FN,_Len, ProposedName):- get_opv(FN,symbol_function,ProposedName),
   (latom_starts_with(ProposedName,'f_');latom_starts_with(ProposedName,'sf_')).
existing_operator(_Ctx,_Env,kw_macro,FN,_Len, ProposedName):- get_opv(FN,symbol_function,ProposedName),
   (latom_starts_with(ProposedName,'mf_')).

existing_operator(_Ctx,_Env,_,FN,_Len, ProposedName):- get_opv(FN,symbol_function,ProposedName),!.

existing_operator(_Ctx,_Env,kw_function,FN,ArgsLen, ProposedName):- atom(FN),upcase_atom(FN,FN),
  (number(ArgsLen)-> Arity is ArgsLen+1; between(1,5,Arity)),is_defined(FN,Arity),ProposedName=FN.
%existing_operator(_Ctx,_Env,kw_macro,FN,ArgsLen, ProposedName):- atom(FN),upcase_atom(FN,FN),                           
%  (number(ArgsLen)-> Arity is ArgsLen+1; between(1,5,Arity)),is_defined(FN,Arity),ProposedName=FN.
%existing_operator(_Ctx,_Env,_BindType,FN,_ArgLen, ProposedName):-some_defined_function_or_macro(FN,2,['mf_'],ProposedName),!.
existing_operator(_Ctx,_Env,_BindType,FN,ArgLen, ProposedName):-some_defined_function_or_macro(FN,ArgLen,['sf_','f_'],ProposedName),!. % 'mf_'

latom_starts_with(ProposedName,Start):- atom(ProposedName),atom_concat(Start,_,ProposedName).



generate_function_or_macro_name(Ctx,FN,BindType,NewProposedName):-
    maybe_symbol_package(FN,Package),
    (pl_symbol_name(FN,Name) ->
      function_case_name(BindType,Name,Package,ProposedName);
      function_case_name(BindType,FN,Package,ProposedName)),
   suffix_by_context(Ctx,ProposedName,NewProposedName),!.


eval_uses_env_arg1(F):- quietly((premute_names(F,FF),wl:declared(FF,env_arg1))).


eval_uses_exact(F):- quietly((premute_names(F,FF),uses_exact0(FF))).

uses_exact0(F):- wl:init_args(x,F),!.
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

eval_uses_exact_and_restkeys(F,N):- quietly((premute_names(F,FF), exact_and_restkeys(FF,N))),!.

exact_and_restkeys(F,N):- wl:init_args(V,F),integer(V),!,V=N.
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
premute_names(F,FF):- atom_concat_or_rtrace('mf_',F,FF).
premute_names(F,FF):- atom_concat_or_rtrace('sf_',F,FF).
premute_names(F,FF):- atom_concat_or_rtrace('f_',FF,F).
premute_names(F,FF):- atom_concat_or_rtrace('mf_',FF,F).
premute_names(F,FF):- atom_concat_or_rtrace('sf_',FF,F).

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
align_args_or_fallback(_Ctx,Env,FN,ProposedName,Args,Result,ArgsPlusResult):- 
   always((align_args_or_fallback_skip_env(FN,ProposedName,Args,Result,ArgsPlusResult),!,
   ((fail,eval_uses_env_arg1(FN))->
      append([Env|Args], [Result], ArgsPlusResult);
      append(Args, [Result], ArgsPlusResult)))).

   
align_args_or_fallback_skip_env(FN,ProposedName,Args,Result,ArgsPlusResult):- 
   align_args(FN,ProposedName,Args,Result,ArgsPlusResult),!.
align_args_or_fallback_skip_env(_,_ProposedName,Args,Result,ArgsPlusResult):- 
   append(Args, [Result], ArgsPlusResult).


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

is_lisp_operator(Ctx,Env,Sym):- get_symbol_fbounds(Ctx,Env,Sym,kw_special,_).
is_lisp_operator(_,_,G):- notrace(lisp_operator(G)).


lisp_operator(X):- atom_concat_or_rtrace('mf_',Symbol,X),symbol_foperator(Symbol).
lisp_operator(X):- atom_concat_or_rtrace('sf_',Symbol,X),symbol_foperator(Symbol).
lisp_operator(defpackage).
lisp_operator(Sym):- wl:declared(Sym,kw_operator).
lisp_operator(Sym):- wl:declared(Sym,kw_macro).
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


is_special_op(S,P):- get_opv(S,symbol_function,kw_special),get_opv(S,symbol_package,P).
is_special_op('%%allocate-closures', pkg_sbc).
is_special_op('%cleanup-fun', pkg_sbc).
is_special_op('%escape-fun', pkg_sbc).
is_special_op('%funcall', pkg_sbc).
is_special_op('%primitive', pkg_sys).
is_special_op('%within-cleanup', pkg_sbc).
is_special_op('compiler-let', pkg_sys).
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
is_special_op('truly-the', pkg_sys).
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
is_special_op(while, pkg_cl).
is_special_op(when, pkg_cl).
is_special_op(defclass, pkg_cl).
is_special_op(defstruct, pkg_cl).
 
:- fixup_exports.



