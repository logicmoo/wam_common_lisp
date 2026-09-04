:- module(gvlib,
[
 is_gvar/3,       % +Module, +Self, -Name
 gvar_must/4,     % +Module, +GVar, +Func, ?Value
 gvar_call/4     % +Module, +GVar, +Func, ?Value
 %'$was_gvar'/2       % +Module, +GVar
 ]).
/** <module> gvlib - Global Variable Syntax

    Author:        Douglas R. Miles
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    Copyright (C): 2017
                       
    This program is free software; you can redistribute it and/or
    modify it.

*/
:- set_module(class(library)).

:- autoload(library(system),[lock_predicate/1,unlock_predicate/1]).
:- autoload(library(when),[when/2]).

:- meta_predicate(gvar_call(+,?,?,?)).
:- set_prolog_flag(generate_debug_info, false).
:- multifile(dot_cfg:using_dot_type/2).
:- dynamic(dot_cfg:using_dot_type/2).
:- system:use_module(library(logicmoo_common)).

:- use_module(library(debuggery/bugger)).
:- reexport(library(debug),[debug/3]).
:- system:reexport(library(dicts)).
:- system:reexport(library(gvar_globals_api)).

:- multifile(dot_eval/4).
:- dynamic(dot_eval/4).
:- module_transparent(dot_eval/4).
:- meta_predicate(dot_eval(?,?,?,?)).
:- '$dicts':import(dot_eval/4).
:- 'system':import(dot_eval/4).


:- 'gvlib':export(expand_gvs_head/6).
:- 'system':import(expand_gvs_head/6).

/*
 
 rtrace($varA.value()=X).
 rtrace($varA.set(9)).
 set_prolog_flag(access_level,system),set_prolog_flag(mpred_te,false).



  rtrace($varA.set(_{})).
*/

install_dot_intercept:- current_prolog_flag(dot_eval,installed),!.
install_dot_intercept:-
   set_prolog_flag(dot_eval,installed),
   unlock_predicate('$dicts':('.'/3)),
   % clause('.'(Dict, Func, Value),BODY),
   redefine_system_predicate('$dicts':'.'(_Dict, _Func, _Value)),
   'system':abolish('$dicts':('.'/3)),
   % dynamic('$dicts':('.'/3)),
   % multifile('$dicts':('.'/3)),
   module_transparent('$dicts':('.'/3)),
   '$set_source_module'('$dicts'),
   '$dicts':compile_aux_clauses([
      (('.'(SSelf,Func,Value) :- strip_module(SSelf,M,Self), dot_eval(M, Self,Func,Value))) %,
      % ('.'(Dict, Func, Value) ':-' BODY)
      ]),
   '$set_source_module'(gvlib),
   lock_predicate('$dicts':('.'/3)),
   'system':import('$dicts':('.'/3)).


:- if(\+ current_prolog_flag(gvar_callable_syntax,false)).
%:- multifile(system:(.)/2).
:- module_transparent(system:(.)/2).
:- dynamic(system:(.)/2).
:- module_transparent(system:(.)/2).
:- Head=..['.',SSelf, Func], % dot3(VarMemb,Var,Memb)
  system:assert((( 
  Head :- strip_module(SSelf,M,Self), 
          % nop(debugging(gvs,gvar_callable_syntax(Self, Func))),
          dot_eval(M, Self, Func,_)))).
:- export(system:('.')/2).
:- 'system':import(('.')/2).
:- endif.


:- if(false).
:- if(\+ current_prolog_flag(gvar_callable_syntax,false)).
:-
   unlock_predicate('$dicts':(':'/4)),
   % clause(':'(Dict, Func, Value),BODY),
   redefine_system_predicate('$dicts':':'(_Dict, _Func, _Value)),
   'system':abolish('$dicts':(':'/4)),
   % dynamic('$dicts':(':'/4)),
   % multifile('$dicts':(':'/4)),
   module_transparent('$dicts':(':'/4)),
   '$set_source_module'('$dicts'),
   '$dicts':compile_aux_clauses([
      ((':'(S1,Func,SSelf,Value) :- strip_module(SSelf,M,Self), missing_dict_function(M, S1,Self,Func,Value))) %,
      % (':'(Dict, Func, Value) ':-' BODY)
      ]),
   '$set_source_module'(gvlib),
   lock_predicate('$dicts':(':'/4)),
   'system':import('$dicts':(':'/4)).

%missing_dict_function(M, S1,Self,Func,Value):- throw(existence_error(procedure,'$dicts':(:)/4)).

:- endif.
:- endif.



:- multifile(dictoo:is_dot_hook/4).
:- dynamic(dictoo:is_dot_hook/4).
:- module_transparent(dictoo:is_dot_hook/4).
dictoo:is_dot_hook(pldoc_wiki,_,_,_):-!,fail.

:- multifile(dictoo:dot_overload_hook/4).
:- dynamic(dictoo:dot_overload_hook/4).
:- module_transparent(dictoo:dot_overload_hook/4).

:- export(gvlib:dot_call/2).
:- module_transparent(gvlib:dot_call/2).
:- 'system':import(gvlib:dot_call/2).
dot_call(A,B):- strip_module(B,M,BB),dot_eval(M,A,BB,_).

:- 'system':import(dot_eval/4).
:- module_transparent(dot_eval/4).

dot_eval(M, Self,Func,Value):- dictoo:is_dot_hook(M, Self,Func,Value),!,dictoo:dot_overload_hook(M,Self,Func,Value).
dot_eval(_, Self,Func,Value):- is_dict(Self),!,dot_dict_swi(Self, Func, Value).
dot_eval(_, Self,Func,Value):- nonvar(Func),compound(Self),Self=t(_,_,_,_),!,rb_lookup(Func,Value,Self),!.
dot_eval(_, Self,Func,Value):- is_rbtree(Self),!,rb_lookup(Func,Value,Self),!.
%dot_eval(M, Self,Func,Value):- is_rbtree(Self),!,rb_visit(Self,Pairs),strip_module(Func,_,Key),member(Key-Value,Pairs),!.

dot_eval(_, Self,Func,Value):- is_assoc(Self),!,get_assoc(Func,Self,Value),!.
dot_eval(M, Self,Func,Value):- \+ is_dict(Self), dot_eval,dot_intercept(M, Self,Func,Value).


dot_eval:- true.

:- module_transparent(dot_intercept/4).
% dot_intercept(M,Self,Func,Value):- quietly((use_dot(_,M),nonvar(Value), \+ current_prolog_flag(gvar_lazy,false))),!,Value =.. ['.',Self,Func].


%dot_intercept(_M,Self,Func,Value):-  compound(Self),Self=t(_,_,_,_), trace,!,rb_lookup(Func,Value,Self).
dot_intercept(M,Self,Func,Value):- 
   ((once((show_failure(dictoo:is_dot_hook(M,Self,Func,Value))->show_failure(use_dot(_,M)))) 
      -> dictoo:dot_overload_hook(M,Self,Func,Value)) *-> true ;
   ((quietly(is_gvar(M,Self,Name)) -> gvar_call(M,Name,Func,Value) ) *-> true ; 
     dot_intercept_lazy(M,Self,Func,Value))).


:- multifile(dictoo:dot_overload_hook/4).
:- dynamic(dictoo:dot_overload_hook/4).
:- module_transparent(dictoo:dot_overload_hook/4).
dictoo:dot_overload_hook(_M,_NewName, _Memb, _Value):- fail.

:- create_prolog_flag(gvar_lazy, false, [keep(true),type(term)]).
:- module_transparent(dot_intercept_lazy/4).

% '__index_wiki_pages'
gvs:never_dot_intercept(pldoc_wiki).
dot_intercept_lazy(_M,Self,Func,Value):-  compound(Self),Self=t(_,_,_,_),!,notrace(rb_lookup(Func,Value,Self)).
dot_intercept_lazy(M,_Self,_Func,_Value):- gvs:never_dot_intercept(M),!, fail.
dot_intercept_lazy(M,Self,Func,Value):- \+ is_dict(Self), 
  \+ current_prolog_flag(gvar_lazy, false),!,
  % Value =.. ['.',Self,Func],  
  (var(Self) -> on_bind(Self,dot_intercept(M,Self,Func,Value));
  (ignore(\+ \+ ((ignore((Self==objs,bt)),on_f_rtrace(dictoo:is_dot_hook(M,Self,Func,Value))))),
                 must(dictoo:dot_overload_hook(M,Self,Func,ValueO)),!,Value=ValueO)).
dot_intercept_lazy(M,Self,Func,Value):- M:dot_dict_swi(Self, Func, Value).


use_dot(Type):- 
  notrace(( prolog_load_context(module, M),
   use_dot(Type,M))).

use_dot(Type,M):- 
   notrace((  \+ current_prolog_flag(gvs_syntax, false),
   (current_prolog_flag(break_level, 0);source_location(_,_)),
   dot_cfg:using_dot_type(Type,M))),!.


:- '$dicts':export('$dicts':eval_dict_function/4).
:- '$dicts':export('$dicts':'$get_dict_ex'/3).
:- '$dicts':export('$dicts':'$type_error'/3).
:- system:import('$dicts':eval_dict_function/4).
:- system:import('$dicts':'$get_dict_ex'/3).
:- system:import('$dicts':'$type_error'/3).


%!  dot_dict(+R, +L, -Result)
%
%   Evaluate dot expressions. Note that  '$get_dict_ex' fails if the
%   first argument is not a dict or the second is not a valid key or
%   unbound.
:- module_transparent(dot_dict/3).
:- 'system':import(dot_dict/3).
dot_dict(Data, Func, Value) :- 
  strip_module(Func,M, SFunct),
  (dot_overload_hook(M, Data, SFunct, Value)*->true;
    dot_dict_swi(Data, Func, Value)).

:- module_transparent(dot_dict_swi/3).
:- 'system':import(dot_dict_swi/3).
dot_dict_swi(Data, Func, Value) :-
    (   '$get_dict_ex'(Func, Data, V0)
    *-> Value = V0
    ;   is_dict(Data, Tag)
    ->  eval_dict_function(Func, Tag, Data, Value)
    ;   is_list(Data)
    ->  (   (atomic(Func) ; var(Func))
        ->  dict_create(Dict, _, Data),
            '$get_dict_ex'(Func, Dict, Value)
        ;   '$type_error'(atom, Func)
        )
    ;   '$type_error'(dict, Data)
    ).

%! is_gvar(+Module, +Self, -Name) is det.
%
%  Tests to see if Self
%  is $(Name) or '$was_gvar'(Module,$Name).
%
is_gvar(_,Self,Name):- % fail,
    nonvar(Self),
    (Self='$'(Name); (Self='$was_gvar'(_,DName),compound(DName),DName='$'(Name))),
    !,atom(Name).

%! '$was_gvar'(+Module, +GVar) is det.
%
%  Wrapper that is callable
%
system:'$was_gvar'(_,_).



%! gvar_must(+Module, +GVar, +Func, ?Value) is det.
%
%  Get/Set GVar or call the previous 
%  Dict interpretor
%
:- module_transparent(gvar_must/4).
gvar_must(M,Name, Memb, Value) :-  gvar_call(M,Name, Memb, Value)*-> true ; make_dot(M,Name,Memb,Value).

gvar_call(M, Name, Memb, Value) :- atom(Name),nonvar(Memb),
    (  \+ current_prolog_flag(gvar_syntax_scope,prepend_module)
      -> gvar_interp(M,Name,Name, Memb, Value) ;
      (atomic_list_concat([M,':',Name],NewName),
      gvar_interp(M,Name,NewName, Memb, Value))),!.


:- meta_predicate(call_has_functions(0)).
call_has_functions(G):- call(G).

is_dict_function(_,get(_Key)).
is_dict_function(_,put(_Key)).
is_dict_function(_,put(_Key,_Value)).
is_dict_function(Tag,Func):- atom(Tag),current_module(Tag),
   compound_name_arity(Func,F,A), 
   A2 is A+2,
   current_predicate(Tag:F/A2),!,
   assertion(\+ is_gvar_function(Tag,Func)).

is_gvar_function(_,current()).
is_gvar_function(_,get()).
is_gvar_function(_,value()).
is_gvar_function(_,getval()).
is_gvar_function(_,unify()).
is_gvar_function(_,Func):- is_gvar_set_function(Func).

is_gvar_set_function(let()).
is_gvar_set_function(set()).
is_gvar_set_function(let(_)).
is_gvar_set_function(set(_)).
is_gvar_set_function(clear()).
is_gvar_set_function(delete()).

nc_arg(Term):- \+ compound(Term),!.
nc_arg(Term):- compound_name_arity(Term,_,0).


:- module_transparent(gvar_interp/5).
gvar_interp(M,SN, Name, Func, Value):- 
  (tracing->true;(debugging(gvar(trace))->trace;true)),
  ((M:nb_current_value(Name,Data) 
   -> (is_dict(Data, Tag) 
      -> gvar_interp5(M,dict(Data,Tag), SN, Name, Func, Value)
      ; gvar_interp5(M,value(Data), SN, Name, Func, Value))
  ; gvar_interp5(M,missing, SN, Name, Func, Value))).


gvar_interp5(M,dict(Data,_Tag), _SN, _Name, Func, Value):- var(Func),!,M:get_dict(Func, Data, Value).
gvar_interp5(M,dict(Data,Tag), _SN, _Name, Func, Value):- 
  (is_dict_function(Tag,Func)
      -> (!,eval_dict_function(Func, Tag, Data, Value))
      ; (atom(Func)
         -> M:get_dict(Func, Data, Value))).

% checked above?
% gvar_interp5(M,_, _, Name, Var, Value):- var(Var),!,make_dot(M,Name, Var,Value).
gvar_interp5(M,_, _, _,Func, _Value):- (\+ compound(Func) ; \+ is_gvar_function(M,Func),!,fail).
gvar_interp5(M,_, _, Name, unify(), Value):-!, gvar_unify(M,Name,Value).
gvar_interp5(M,_, _, Name, current(),Value):-!, M:nb_current_value(Name,Value).
gvar_interp5(M,_, _, Name, get(),Value):-!, M:b_get_value(Name,Value).
gvar_interp5(M,_, _, Name, getval(),Value):-!, M:b_get_value(Name,Value).

gvar_interp5(M,_, _, Name, value(),Value):-!, M:((nb_current_value(Name,Value); on_bind(Value,nb_link_value(Name,Value)))),!.


% TODO needs to survive later nb_* calls 
gvar_interp5(M,_, _, Name, let(), Value):- !, M:b_set_value(Name,Value). 
gvar_interp5(M,_, SN,Name, let(Value),'$was_gvar'(M,'$'(SN))):- M:b_set_value(Name,Value).


gvar_interp5(M,_, _, Name, set(), Value):- must(M:nb_link_value(Name,Value)), !, on_bind(Value,gvar_put(M,Name,Value)).
gvar_interp5(M,_, SN,Name, set(Value),'$was_gvar'(M,'$'(SN))):- gvar_put(M,Name,Value),!.


gvar_interp5(M,_, SN,Name, clear(),'$was_gvar'(M,'$'(SN))):-!, M:nb_set_value(Name,_).
gvar_interp5(M,_, SN,Name, delete(),'$was_gvar'(M,'$'(SN))):-!, M:nb_delete(Name).


make_dot(M,Name, Missed,M:Value):- Value =.. ['.',$(Name),Missed].

% gvar_unify(M,Name,Value):- nb_current_value(Name,Ref),!,on_bind(Value,Value=Ref).
gvar_unify(M,Name,Value):- M:nb_current_value(Name,Was),!,Value=Was.
gvar_unify(M,Name,Value):- var(Value),!, M:nb_link_value(Name,Value),on_bind(Value,gvar_put(M,Name, Value)).
gvar_unify(M,Name,Value):- gvar_put(M,Name,Value).

% Sets a copy and then unifies the value to the copy
gvar_put(M,Name,ValueI):- duplicate_term(ValueI,Value),ValueI=Value,
   M:nb_set_value(Name,Value),!, % after dupe_term    
   must(M:nb_current_value(Name,ValueO)),!,
   sanity(Value=@=ValueO),Value=ValueO. %  we still want the same variables (if possible)



on_bind(V,G):- var(V),!,freeze(V,G).
on_bind(V,G):-term_variables(V,Vs),
  (Vs==[] ->G ; (make_binds(Vs,VsG),!,when(VsG,G))).

make_binds([V],nonvar(V)).
make_binds([V|Vs],(nonvar(V);VsG)):-make_binds(Vs,VsG).

                
hook_function_expand:- call(( abolish('$expand':function/2), 
  asserta((
  '$expand':function(FHead, _) :- 
      compound(FHead),
      FHead =.. [.,_,_],
      \+ current_prolog_flag(gvar_syntax,true),
      \+ functor([_|_], (.), _))))).

% :- set_prolog_flag(gvar_syntax,true).

/*

expand_dict_function((QFHead := V0 :- Body), (QHead :- Body, Eval)) :-
    fqhead(QFHead, FHead, Head, QHead),
    compound(FHead),
    FHead =.. [.,R,M],
    callable(M),
    !,
    '$expand':replace_functions(V0, Eval, V, _Ctx),
    compound_name_arguments(M, Name, Args0),
    '$append'(Args0, [R,V], Args),
    compound_name_arguments(Head, Name, Args).
expand_dict_function((QFHead := V0), (QHead :- Eval)) :-
    fqhead(QFHead, FHead, Head, QHead),
    compound(FHead),
    FHead =.. [.,R,M],
    callable(M),
    !,
    '$expand':replace_functions(V0, Eval, V, _Ctx),
    compound_name_arguments(M, Name, Args0),
    '$append'(Args0, [R,V], Args),
    compound_name_arguments(Head, Name, Args).

fqhead(M:FHead, FHead, Head, M:Head) :- !.
fqhead(FHead,   FHead, Head,   Head).


system:term_expansion(FDecl, Clause) :-
    expand_dict_function(FDecl, Clause).
system:term_expansion(M:FDecl, QClause) :-
    expand_dict_function(FDecl, Clause),
    !,
    QClause = M:Clause.

*/

:- export(is_dvar/1).

is_dvar(Var):- \+ compound(Var),!,fail.
is_dvar($(_)):- !, use_dot(_).
is_dvar(the(_)):- !, use_dot(thevars).
is_dvar(FA):-compound_name_arity(FA,F,_),dvar_type(F,Type,_,DO),!,(use_dot(Type);DO\==error).

:- export(dvar_name/4).
dvar_name(_M,GVAR,TYPE,NAME):- is_dvar(GVAR),GVAR=..[TYPE,NAME|_].

dvar_type('$',tvars,deftoplevel,error).
dvar_type('@',atvars,defglobal,nb_set_value).
dvar_type('?',qvars,deflexical,b_set_value).
dvar_type('&',ampvars,defstatic,error).
dvar_type('#',hashvars,defconst,error).

dot3(VarMemb,Var,Memb):- VarMemb=..['.',Var,Memb].

is_var_memb(VarMemb,Var,Memb):- 
  compound(VarMemb),
  dot3(VarMemb,Var,Memb),
  is_dvar(Var),!.

gvs_expanded_op( = ).
gvs_expanded_op( := ).
% gvs_expanded_op( _ ).

:- module_transparent(expand_gvs_head/6).

expand_gvs_head(Head,_OP,_M,_VarO,_Memb,_Value):- \+ compound(Head),!,fail.
  
% $mod:var.memb = value.                            
expand_gvs_head(Head,OP,M,VarO,Memb,Value):-
  Head =.. [OP , ':'(DM,VarMemb),Value],
  gvs_expanded_op(OP),
  is_dvar(DM),DM =..[F,M],
  compound(VarMemb),
  dot3(VarMemb,Var,Memb),!,
  VarO =.. [F,Var].

% mod: $var.memb = value.
expand_gvs_head(Head,OP,M,Var,Memb,Value):-
  Head =.. [OP , :(M,VarMemb),Value],
  gvs_expanded_op(OP),
  is_var_memb(VarMemb,Var,Memb).

% $var.memb = value.
expand_gvs_head(Head,OP,M,Var,Memb,Value):-
  Head =.. [OP , MVarMemb,Value],
  compound(MVarMemb),
  gvs_expanded_op(OP),
  strip_module(MVarMemb,M,VarMemb),
  is_var_memb(VarMemb,Var,Memb),!.

% $var.memb mod:= value.
expand_gvs_head(M:Head,OP,M,Var,Memb,Value):- 
  Head =.. [OP , VarMemb,Value],
  gvs_expanded_op(OP),
  is_var_memb(VarMemb,Var,Memb),!.


show_gvar(Name):-
 (nb_current_value(Name,Value)->true;Value='$missing'),
 format('~w =~t~12|~p~n', [Name, Value]).


:- multifile(dot_cfg:dictoo_decl/8).
:- dynamic(dot_cfg:dictoo_decl/8).
%:- discontiguous(dot_cfg:dictoo_decl/8).

:- module_transparent(simpl_dot_eval/3).
simpl_dot_eval(_,IO,IO):- nc_arg(IO),!.
simpl_dot_eval(M,(A,B),O):- 
  compound(A),compound(B), 
  A = dot_eval(M, Self, Memb, Value), 
  B= (Var=Value),
  var(Value),
  Value=Var,
  O= dot_eval(M,Self, Memb, Value).
simpl_dot_eval(M,Term,TermO):-
  Term=.. [F|ARGS],
  maplist(simpl_dot_eval(M),ARGS,ARGSO),
  TermO=.. [F|ARGSO].

show_gvs:- 
   listing(dot_cfg:dictoo_decl/8),
   listing(dot_cfg:using_dot_type/2),   
   maplist(show_gvar,['$goal_term','$term','$term_user','$variable_names','$query_term']),
   forall(prolog_debug:debugging(gvar(Name), Value, _),
    format('~w =~t~12|~p~n', [gvar(Name), Value])),
   ignore(print_toplevel_variables),
   !.

%TODO not yet used
/*
may_expand(Goal):-
    nb_current('$gvs_skip',Was),!,
    Was\==Goal,
    \+ (compound(Was),arg(_,Was,Each),Each==Goal).
*/
may_expand(_).

:- export(dot_ge/3).
dot_ge(Goal, P, _):- 
  var(P), \+ source_location(_,_),  
  % quietly(use_dot(_Type)), 
  show_call(gvar(syntax),nb_set_value('$goal_term',Goal)),
  fail.  
dot_ge(Var,_,_):- nc_arg(Var),!,fail.
dot_ge(call_has_functions(Goal),_, call_has_functions(Goal)):- !.
dot_ge(Goal, _, GoalO):- 
 prolog_load_context(module,M),
 % may_expand(Goal),
 use_dot(_Type),
 (tracing->true;(debugging(gvar(syntax))->trace;true)),  
  gvs_ge(Goal, Goal0),
  gvs_ge_pass2(Goal0, GoalM),
  simpl_dot_eval(M,GoalM,GoalO),Goal\== GoalO,!.
dot_ge(Goal, _, GoalO):-
  use_dot(_Type),
  gvs_ge_pass2(Goal, GoalM),!,
  prolog_load_context(module,M),
  simpl_dot_eval(M,GoalM,GoalO),Goal\== GoalO.

gvs_ge(Var,Var):- \+ compound(Var),!.
gvs_ge(M:Term, M:TermO):- !, gvs_module_ge(M,Term, TermO).
gvs_ge(MTerm, TermO):- !, strip_module(MTerm,M,Term),gvs_module_ge(M,Term, TermO).

gvs_module_ge(_,Term, Term ):- nc_arg(Term),!.
gvs_module_ge(M,Term, TermO):-
    predicate_property(M:Term,meta_predicate(MP)),
    Term=.. [F|ARGS],
    gvs_n_ge(M,MP,ARGS,1,ARGSO),
    TermO=.. [F|ARGSO],!.
gvs_module_ge(M,Term, TermO):- 
    Term=.. [F|ARGS],
    maplist(gvs_module_ge(M),ARGS,ARGSO),
    TermO=.. [F|ARGSO].

% Want to go outside things like assert/1 and DCG?
% (basically thast was the default before)
meta_arg_spec_outside(//). 
meta_arg_spec_outside(:). 

meta_arg_spec(*).
% meta_arg_spec(S):-meta_arg_spec_outside(S).
meta_arg_spec(S):-integer(S).

gvs_n_ge(_,_,[],_,[]):-!.
gvs_n_ge(M,MP,[I|ARGS],N,[O|ARGSO]):- 
    arg(N,MP,Type),
    (meta_arg_spec(Type)-> gvs_ge_pass2(I, O) ; gvs_module_ge(M, I, O)),
    N2 is N +1,
    gvs_n_ge(M,MP,ARGS,N2,ARGSO),!.

gvs_ge_pass2(Var,Var):- nc_arg(Var),!.
gvs_ge_pass2(M:Term, M:TermO):- !, gvs_ge_pass2(Term, TermO).
gvs_ge_pass2(call_has_functions(Goal), call_has_functions(Goal)):- !.
gvs_ge_pass2(FHead, dot_call(A,B)):- FHead =.. ['.',A,B],!.
gvs_ge_pass2('.'(A,B,C), dot_eval(M,A,B,C)):- prolog_load_context(module,M).
% gvar_op_call
gvs_ge_pass2(Goal,  gvar_op_call(OP,M,Var,Memb,Value) ):- 
   prolog_load_context(module, M),
   show_success(gvar(goal_expansion),    
     M:(expand_gvs_head(Goal,OP,M,Var,Memb,Value),show_failure(use_dot(_Type,M)))),!.
% gvar_op_call_rev
gvs_ge_pass2(RGoal,  gvar_op_call_rev(OP,M,Var,Memb,Value) ):- 
   RGoal =..[OP,X,Y],gvs_expanded_op(OP),
   Goal =..[OP,Y,X],
   prolog_load_context(module, M),
   show_success(gvar(goal_expansion),    
     M:(expand_gvs_head(Goal,OP,M,Var,Memb,Value),show_failure(use_dot(_Type,M)))),!.
gvs_ge_pass2((A,B),(AA,BB)):- !, gvs_ge_pass2(A,AA),gvs_ge_pass2(B,BB),!.
gvs_ge_pass2(MBody,  ExpandedBodyO):- expand_functions(MBody, ExpandedBody),MBody\== ExpandedBody,!,gvs_ge_pass2(ExpandedBody,ExpandedBodyO).
gvs_ge_pass2(IO,IO).


gvar_op_call(=,M,Var,Memb,Value):- dot_intercept(M,Var,Memb,Value).
gvar_op_call(:=,M,Var,Memb,Value):- dot_intercept(M,Var,Memb,Value).

gvar_op_call_rev(=,M,Var,Memb,Value):- gvar_op_call(=,M,Var,Memb,Value).


expand_functions(MBody, ExpandedBody):-
  strip_module(MBody,M,Body),
  expand_functions(M, Body, ExpandedBody).


:- include(gvar_fixup_exports).

/*
:- 
 source_location(S,SL), 
 writeq(source_location(S,SL)),
 prolog_load_context(source,S),
 forall(source_file(M:H,SF),
 ((SF=S,
 writeln(prolog_load_context(module,LC)),
 ignore((functor(H,F,A), \+ atom_concat('$',_,F), \+ lmconfig:never_export_named_gvar(F/_),
  ignore(((atom(LC),atom(M), LC\==M,M:export(M:F/A),LC:multifile(M:F/A),fail,atom_concat('$',_,F),LC:import(M:F/A)))),
  ignore(((\+ atom_concat('$',_,F),\+ atom_concat('__aux',_,F),LC:export(M:F/A), 
  ignore(((current_predicate(system:F/A)->true; system:import(M:F/A)))))))))))).
*/

:- fixup_exports.

:- install_dot_intercept.

% :- listing('$dicts':('.')/3).

expand_functions(_,Var,Var):- nc_arg(Var).
expand_functions(M, :- Fun, :- ExpFun):- !, expand_functions(M, Fun,  ExpFun).
expand_functions(M, Head:- Fun, Head:- ExpFun):- !, expand_functions(M, Fun,  ExpFun).
expand_functions(M, Body, ExpandedBody) :-
    '$expand':replace_functions(Body, Eval, Goal, M),
    (Eval = true -> ExpandedBody = Body ;
    (expand_functions(M, Eval,  ExpandedBody0),
     ExpandedBody = (ExpandedBody0,Goal))).


:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).

user:expand_query(Goal, _, Bindings, _ ):- quietly(use_dot(_Type)), nb_setval('$query_term',Goal-Bindings),fail.

user:expand_query(Goal, Expanded, Bindings, ExpandedBindings):- 
    % quietly(use_dot(_Type)),
    % Have vars to expand and varnames are empty
    ((Bindings\==[],prolog_load_context(variable_names,Vs), Vs ==[])),
    b_setval('$variable_names', Bindings),  % this prevents the loop
    % debug(expand_query,'~q',[b_setval('$variable_names', Bindings)]),
    (toplevel_variables_expand_query(Goal, Expanded0, Bindings, ExpandedBindings0) -> true; 
      (Goal = Expanded0, Bindings = ExpandedBindings0)),
    (user:expand_query(Expanded0, Expanded, ExpandedBindings0, ExpandedBindings) -> true ; 
     (Expanded0 = Expanded, ExpandedBindings0 = ExpandedBindings)).

make_top_var(Name,Var,Value):- prolog_load_context(module, M),
   Value = Var,
   use_dot(_Core,M),!,
   % '$was_gvs'(M,Var),
   current_predicate(oo/1),
   oo(Var),
   add_var_to_env(Name,Var),
   toplevel_variables:assert_binding(Name,Value).

expand_vars(_, Var, Var) -->
    { var(Var) },
    !.
expand_vars(_, Atomic, Atomic) -->
    { atomic(Atomic) },
    !.
expand_vars(Bindings, $(Var), Value) -->
    { toplevel_variables:name_var(Var, Bindings, Name),
      (   toplevel_variables:toplevel_var(Name, Value)
      ->  !
      ;   (show_call(make_top_var(Name,Var,Value))-> true ; throw(error(existence_error(answer_variable, Name), _)))
      )
    },
    [ Name = Value ].
expand_vars(Bindings, Term, Expanded) -->
    { compound_name_arity(Term, Name, Arity),
      !,
      compound_name_arity(Expanded, Name, Arity),
      End is Arity + 1
    },
    expand_args(1, End, Bindings, Term, Expanded).


expand_args(End, End, _, _, _) --> !.
expand_args(Arg0, End, Bindings, T0, T) -->
    { arg(Arg0, T0, V0),
      arg(Arg0, T, V1),
      Arg1 is Arg0 + 1
    },
    expand_vars(Bindings, V0, V1),
    expand_args(Arg1, End, Bindings, T0, T).


toplevel_variables_expand_query(Query, Expanded, Bindings, ExpandedBindings) :- !,
    phrase(expand_vars(Bindings, Query, Expanded), NewBindings),
    term_variables(Expanded, Free),
    toplevel_variables:delete_bound_vars(Bindings, Free, ExpandedBindings0),
    '$append'(ExpandedBindings0, NewBindings, ExpandedBindings),
    (   toplevel_variables:verbose,
        Query \=@= Expanded
    ->  toplevel_variables:print_query(Expanded, ExpandedBindings)
    ;   true
    ).

toplevel_variables_expand_query(Goal, Expanded, Bindings, ExpandedBindings):- 
   \+ current_prolog_flag(toplevel_mode, recursive),
   use_dot(core),!,
  catch(toplevel_variables:expand_query(Goal, Expanded, Bindings, ExpandedBindings), Warn,
   ((dmsg(Warn), Goal = Expanded, Bindings = ExpandedBindings))).

toplevel_variables_expand_query(Goal, Expanded, Bindings, ExpandedBindings):-
  toplevel_variables:expand_query(Goal, Expanded, Bindings, ExpandedBindings).


/*
:- user:dynamic(term_expansion/4).
:- user:multifile(term_expansion/4).

user:term_expansion(FDecl,P, _, _) :- fail,
   nonvar(P),compound(FDecl),nb_current('$term',Was),
   FDecl==Was,FDecl=(_:-Goal),
   show_call(gvar(syntax),b_setval('$body_term',Goal)),
   fail.
*/

:- system:dynamic(goal_expansion/4).
:- system:multifile(goal_expansion/4).
:- user:dynamic(goal_expansion/4).
:- user:multifile(goal_expansion/4).

contains_dot(Goal):- compound(Goal),
  (compound_name_arity(Goal,'.',2);(arg(_,Goal,Arg),contains_dot(Arg))).

system:goal_expansion(Goal, P, NewGoal, PO):- 
  notrace((\+ current_prolog_flag(gvs_syntax,false))),
  notrace((compound(Goal),contains_dot(Goal))),(use_dot(_)),
  show_call(gvar(syntax),((dot_ge(Goal, P, NewGoal)))),  
  P=PO,!.

%:- set_prolog_flag(toplevel_goal_expansion,true).
%:- set_prolog_flag(scope_functions,true).



