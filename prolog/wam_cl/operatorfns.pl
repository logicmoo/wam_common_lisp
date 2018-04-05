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
:- include('./header').
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
find_lisp_function(FN,ARITY,ProposedName):- fail,
  find_operator(_Ctx,_Env,kw_special,FN,ARITY, ProposedName).

/*
make_function_or_macro_call(Ctx,Env,FN,Args,Result,ExpandedFunction):-
   (is_list(Args)->length(Args,ArgsLen);true),
   foc_ope rator(Ctx,Env,symbol_function,FN,ArgsLen, ProposedName),!,
   align_ args  _or_fallback(Ctx,Env,FN, ProposedName,Args,Result,ArgsPlusResult),
   ExpandedFunction =.. [ ProposedName | ArgsPlusResult].
*/

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
%find_operator_else_function(_Cxt,_Env,_BindType,Symbol,function(Symbol),true).

find_operator_or_die(Env,BindType,Symbol, ProposedName):- nonvar(Symbol), find_operator(Env,Env,BindType,Symbol, _Len, ProposedName),!.
%find_operator_or_die(_Env,kw_function,Symbol, function(Symbol)).
find_operator_or_die(Env,BindType,Symbol, R):- trace_or_throw(find_operator_or_die(Env,BindType,Symbol, R)).



f_sys_coerce_to_function(FN,ProposedName):- find_lisp_function(FN,_ARITY,ProposedName).

foc_operator(Ctx,Env,BindType,FN, Len, ProposedName):-  find_operator(Ctx,Env,BindType,FN, Len, ProposedName),!.
foc_operator(Ctx,_Env,BindType,FN, _Len, ProposedName):- 
  always((generate_function_or_macro_name(Ctx,FN,BindType,ProposedName))),!.

bind_type_naming_of(BindType,FN,Named):- wdmsg(bind_type_naming_of(BindType,FN,Named)).
bind_type_naming(kw_function,FN,ProposedName):- (atom_concat('f_',FN,ProposedName)),!.
bind_type_naming(kw_special,FN,ProposedName):-  (atom_concat('sf_',FN,ProposedName)),!.
%bind_type_naming(kw_function,FN,ProposedName):- (atom_concat('sf_',FN,ProposedName)),!.
%bind_type_naming(kw_special,FN,ProposedName):-  (atom_concat('f_',FN,ProposedName)),!.
bind_type_naming(kw_macro,FN,ProposedName):- atom_concat('mf_',FN,ProposedName).

existing_operator(Ctx,Env,BindType,FN, _Len, ProposedName):- show_success(get_symbol_fbounds(Ctx,Env,FN,BindType,ProposedName)),!.
existing_operator(_,_,BindType,FN,_, ProposedName):- get_opv(FN,symbol_function,ProposedName),!,bind_type_naming(BindType,_,ProposedName).
existing_operator(_Ctx,_Env,BindType,FN,_Len, ProposedName):- bind_type_naming(BindType,FN,ProposedName),is_defined(ProposedName,_).
%existing_operator(_Ctx,_Env,BindType,FN,_Len, ProposedName):- get_opv(FN,symbol_function,ProposedName),!,var(BindType).
existing_operator(_Ctx,_Env,BindType,FN,_,ProposedName):- bind_type_naming(BindType,_,FN),ProposedName = FN.
existing_operator(_Ctx,_Env,kw_function,FN,ArgsLen, ProposedName):- atom(FN),upcase_atom(FN,FN),
  (number(ArgsLen)-> Arity is ArgsLen+1; between(1,5,Arity)),is_defined(FN,Arity),ProposedName=FN.
%existing_operator(_Ctx,_Env,kw_macro,FN,ArgsLen, ProposedName):- atom(FN),upcase_atom(FN,FN),                           
%  (number(ArgsLen)-> Arity is ArgsLen+1; between(1,5,Arity)),is_defined(FN,Arity),ProposedName=FN.
%existing_operator(_Ctx,_Env,_BindType,FN,_ArgLen, ProposedName):-some_defined_function_or_macro(FN,2,['mf_'],ProposedName),!.
existing_operator(_Ctx,_Env,BindType,FN,ArgLen, ProposedName):-
   some_defined_function_or_macro(FN,ArgLen,['sf_','f_','mf_'],ProposedName),!,
   bind_type_naming(BindType,_,ProposedName). % 


generate_function_or_macro_name(_Ctx,FN,BindType,ProposedName):-
    bind_type_naming(BindType,_,FN),ProposedName = FN.

generate_function_or_macro_name(Ctx,FN,BindType,NewProposedName):-
    maybe_symbol_package(FN,Package),
    (pl_symbol_name(FN,Name) ->
      function_case_name(BindType,Name,Package,ProposedName);
      function_case_name(BindType,FN,Package,ProposedName)),
   suffix_by_context(Ctx,ProposedName,NewProposedName),!.


eval_uses_env_arg1(F):- quietly((premute_names(F,FF),wl:declared(FF,env_arg1))).

   

function_arg_info(FN,ArgInfo):- wl:arglist_info(FN,_,_,ArgInfo).
function_arg_info(FN,ArgInfo):- wl:arglist_info(FN,_,_,_,ArgInfo).
function_arg_info(FN,ArgInfo):- wl:arglist_info(_,FN,_,_,ArgInfo).


%eval_uses_whole(F):- quietly((premute_names(F,FF), get_init_args(FF,whole))),!.
%eval_bind_parameters(F):- quietly((premute_names(F,FF), get_init_args(FF,bind_parameters))),!.

% get_init_args(FN,Requireds):- current_predicate(FN/N), Requireds is N-2,Requireds>0.
get_init_args(F,_):- is_list(F),!,fail.
get_init_args(F,Args):- nonvar(Args),!,get_init_args(F,ArgsV),ArgsV=Args.
get_init_args(F,N):- quietly((premute_names(F,FF), exact_and_restkeys(FF,N))),!.


get_ftype_lambda_list(F,List):- get_opv(F,ftype_lambda_list,List).
get_ftype_lambda_list(F,List):- wl:lambda_def(_, F,_, List, _).
get_ftype_lambda_list(F,List):- atom(F),wl:lambda_def(_, _,F, List, _).


exact_and_restkeys_l(F,N):- no_repeats(F,get_ftype_lambda_list(F,List)),
  once(((append(Left,[c38_optional|_],List);append(Left,[c38_rest|_],List);
     append(Left,[c38_key|_],List))->length(Left,N);(length(List,X),N=x(X)))).

exact_and_restkeys(F,N):- wl:init_args(N,F),!,integer(N).
exact_and_restkeys(F,N):- exact_and_restkeys_l(F,N),integer(N),!.
exact_and_restkeys(F,N):- function_arg_info(F,ArgInfo),ArgInfo.req=L,ArgInfo.all\==L,!,arg_info_count(ArgInfo,req,N).
exact_and_restkeys(F,0):- wl:declared(F,lambda(['&rest'|_])),!.
exact_and_restkeys(F,0):- function_arg_info(F,ArgInfo),ArgInfo.req==0,ArgInfo.all\==0,!.
exact_and_restkeys(FN,x):-  function_arg_info(FN,ArgInfo),!,
   ArgInfo.complex==0,ArgInfo.opt==0,ArgInfo.rest==0,ArgInfo.env==0,ArgInfo.whole==0,
   length(ArgInfo.names,NN),
   arg_info_count(ArgInfo,req,N),!,
   N==NN.


arg_info_count(ArgInfo,Prop,N):- 
  Value=ArgInfo.Prop,
   (number(Value)->N=Value;
     (is_list(Value)->length(Value,N);
       (atom(Value)->N=1;
         (throw(arg_info_count(ArgInfo,Prop,Value)))))).

premute_names(F,F).
premute_names(F,FF):- atom_concat_or_rtrace('f_',F,FF).
% premute_names(F,FF):- atom_concat_or_rtrace('mf_',F,FF).
%premute_names(F,FF):- atom_concat_or_rtrace('sf_',F,FF).
premute_names(F,FF):- atom_concat_or_rtrace('f_',FF,F).
% premute_names(F,FF):- atom_concat_or_rtrace('mf_',FF,F).
%premute_names(F,FF):- atom_concat_or_rtrace('sf_',FF,F).


% Non built-in function expands into an explicit function call

% invoke(r1,r2,r3,RET).
align_args(FN,ProposedName,Args,Result,ArgsPlusResult):- 
  (get_init_args(FN,x);get_init_args(ProposedName,x)),
   append(Args, [Result], ArgsPlusResult).

% invoke(r1,r2,[o3,key1,value1],RET).
align_args(FN,ProposedName,Args,Result,ArgsPlusResult):- 
  (get_init_args(FN,N);get_init_args(ProposedName,N)),number(N),
  always(length(Left,N)),append(Left,Rest,Args),
  append(Left, [Rest,Result], ArgsPlusResult).

% invoke([fn,r1,r2,r3],RET).
%align_args(FN,ProposedName,Args,Result,[[FN|Args],Result]):- (eval_uses_whole(FN);eval_uses_whole(ProposedName)).


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

align_args_or_fallback1(_Ctx,_Env,FN,ProposedName,Args,Result,ArgsPlusResult):- 
   align_args_or_fallback_skip_env(FN,ProposedName,Args,Result,ArgsPlusResult),!.
      
align_args_or_fallback_skip_env(FN,ProposedName,Args,Result,ArgsPlusResult):- 
   align_args(FN,ProposedName,Args,Result,ArgsPlusResult),!.
align_args_or_fallback_skip_env(_,_ProposedName,Args,Result,ArgsPlusResult):- 
   append(Args, [Result], ArgsPlusResult).

/*align_args_or_fallback_skip_env(FN,ProposedName,Args,Result,ArgsPlusResult):- 
   compile_apply_function_or_macro_call(Ctx,Env,FM,Args,Result,ExpandedFunction),!.
  
*/

only_arity(ProposedName,N):-
  is_defined(ProposedName,N),
  forall((between(0,6,Other),Other\=N),  \+ is_defined(ProposedName,Other)).

is_defined(ProposedName,N):- integer(N),atom(ProposedName),!,functor(G,ProposedName,N),current_predicate(_,G),!.
is_defined(ProposedName,N):- \+ compound(ProposedName),!,current_predicate(ProposedName/N).
is_defined(ProposedName,N):- wdmsg(is_defined(ProposedName,N)),!,break,fail.

is_implemented(FN):- is_fboundp(FN),
  foc_operator(_Ctx,_Env,_BindType,FN,_,ProposedName),  
  ProposedName\==FN,is_defined(ProposedName,NN),!,
  ((get_init_args(FN,N)->integer(N))->NN==N;true).

maybe_symbol_package(Symbol,Package):-  get_opv(Symbol,symbol_package,Package),!.
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

is_lisp_operator(Ctx,Env,Sym):- get_symbol_fbounds(Ctx,Env,Sym,BindType,_),!,BindType\==kw_function.
is_lisp_operator(_,_,G):- notrace(lisp_operator(G)).


lisp_operator(FN):- \+ atom(FN),!,fail.
lisp_operator(FN):- wl:declared(FN,kw_function),!,fail.
lisp_operator(X):- atom_concat_or_rtrace('mf_',Symbol,X),symbol_foperator(Symbol).
lisp_operator(X):- atom_concat_or_rtrace('sf_',Symbol,X),symbol_foperator(Symbol).
lisp_operator(defpackage).
lisp_operator(FN):- wl:declared(FN,kw_operator).
lisp_operator(FN):- wl:declared(FN,kw_special).
lisp_operator(FN):- wl:declared(FN,kw_macro).
lisp_operator(if).
lisp_operator('data-assrt').
lisp_operator('define-caller-pattern').
lisp_operator('define-variable-pattern').
lisp_operator(u_define_caller_pattern).
lisp_operator(f_u_define_caller_pattern).
lisp_operator(S):- get_opv(S,symbol_function,FN),!, lisp_operator(FN).
lisp_operator(S):- nonvar(S),compiler_macro_left_right(S,_,_).
lisp_operator(S):- get_lambda_def(_Ctx,_Env,defmacro,S,_,_).
lisp_operator(S):- is_special_op(S,P),currently_visible_package(P).
%lisp_operator(S):-is_special_op(S,_P).

is_special_op(S,P):- get_opv(S,symbol_function,SF),get_opv(S,symbol_package,P),is_special_operator_p(SF).
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



