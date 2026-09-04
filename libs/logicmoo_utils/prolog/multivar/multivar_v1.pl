/*  Part of SWI-Prolog
    Author:        Douglas R. Miles, Jan Wielemaker
    E-mail:        logicmoo@gmail.com, jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org http://www.logicmoo.org
    Copyright (C): 2015, University of Amsterdam
                                    VU University Amsterdam
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(multivar,
 [
   test_case_1/0,
   test_case_2/0,
   test_case_3/0,
   test_case_4/0
  /* mdwq/1, 
		  plvar/1,
          nb_var/1, nb_var/2,
          vdict/1, vdict/2,
		  un_mv/1, un_mv1/1,
		  mv_peek_value/2,mv_peek_value1/2,
		  mv_set/2,mv_set1/2,
		  mv_add1/2,mv_allow/2,
		  ic_text/1,

   is_mv/1, multivar/1 % create special varaible that cannot be bound
   */
   ]).

:- use_module(logicmoo_common).
:- meta_predicate user:attvar_variant(0,0).
:- use_module(library(option),[dict_options/2,option/2]).

%:- set_prolog_flag(access_level,system).
%:- set_prolog_flag(gc,false).

% use_module(library(multivar)),call(multivar(X)),trace,X=2.

mdwq(Q):- format(user_error,'~NMWQ: ~q~n',[Q]).

:- meta_predicate mdwq_call(*).
mdwq_call(Q):- !, call(Q).
mdwq_call(Q):- call(Q) *-> mdwq(success:Q); (mdwq(failed:Q),!,fail).
:- export(mdwq_call/1).

:- if(current_prolog_flag(attr_pre_unify_hook,true)).

:- module_transparent(user:attr_pre_unify_hook/3).
:- user:export(user:attr_pre_unify_hook/3).

:- '$set_source_module'('$attvar').

:- module_transparent(system : = /2).
:- module_transparent(wakeup/2).
:- module_transparent('$wakeup'/1).
wakeup(wakeup(Attribute, Value, Rest),M) :- !,
    begin_call_all_attr_uhooks(Attribute, Value, M),
    '$wakeup'(Rest).
wakeup(_,_).

:- import(user:attr_pre_unify_hook/3).
:- module_transparent(user:attr_pre_unify_hook/3).
% replaces call_all_attr_uhooks
begin_call_all_attr_uhooks(att('$VAR$', IDVar, Attrs),Value, M) :- !,
    M:attr_pre_unify_hook(IDVar, Value, Attrs).
begin_call_all_attr_uhooks(Attribute, Value, M) :-
    call_all_attr_uhooks(Attribute, Value, M).

orig_call_all_attr_uhooks([], _).
orig_call_all_attr_uhooks(att(Module, AttVal, Rest), Value) :-
    uhook(Module, AttVal, Value),
    orig_call_all_attr_uhooks(Rest, Value).


:- module_transparent(call_all_attr_uhooks/3).
call_all_attr_uhooks(att(Module, AttVal, Rest), Value, M) :- !,
    uhook(Module, AttVal, Value, M),
    call_all_attr_uhooks(Rest, Value, M).
call_all_attr_uhooks(_, _, _).

:- module_transparent(uhook/4).
uhook(freeze, Goal, Y, M) :-
 M:(
    !,
    (   attvar(Y)
    ->  (   get_attr(Y, freeze, G2)
        ->  put_attr(Y, freeze, '$and'(G2, Goal))
        ;   put_attr(Y, freeze, Goal)
        )
    ;   '$attvar':unfreeze(Goal)
    )).

uhook(Module, AttVal, Value, M) :-
  M:(
    true,
    Module:attr_unify_hook(AttVal, Value)).


:- ((abolish('$wakeup'/1),'$attvar':asserta('$wakeup'(M:G):-wakeup(G,M)))).
:- meta_predicate('$wakeup'(:)).

:- all_source_file_predicates_are_transparent.

:- '$set_source_module'('multivar').

:- module_transparent(attr_pre_unify_hook_m/4).
attr_pre_unify_hook_m(IDVar, Value, _, M):- \+ attvar(IDVar),!, M:(IDVar=Value).
attr_pre_unify_hook_m(Var,Value,Rest, M):- 
  mdwq_call('$attvar':call_all_attr_uhooks(Rest, Value, M)),
  nop(M:mv_add1(Var,Value)).

user:attr_pre_unify_hook(Var,Value,Rest):- strip_module(Rest,M,_), attr_pre_unify_hook_m(Var,Value,Rest,M).

:- else.

:- module_transparent(user:meta_unify/3).
user:meta_unify(Var,Rest,Value):- user:attr_pre_unify_hook(Var,Value,Rest).

%-----------------------------------------------------------------
% Blugened in version of verify_attributes/3


user:attr_pre_unify_hook(IDVar, Value, _):- \+ attvar(IDVar),!, IDVar=Value.
/*
user:attr_pre_unify_hook(IDVar, Value, Attrs):-
  call_verify_attributes(Attrs, Value, IDVar, [], Goals),
  nop(attv_bind(IDVar, Value)),
  maplist(call,Goals).
*/
%user:attr_pre_unify_hook(IDVar, Value, Attrs):-  '$attvar':call_all_attr_uhooks(att('$VAR$',IDVar,Attrs),Value).
user:attr_pre_unify_hook(Var,Value,Rest):- 
  mdwq_call('$attvar':call_all_attr_uhooks(Rest, Value)),
  nop(mv_add1(Var,Value)).

:- endif.



call_verify_attributes([], _, _) --> [].
call_verify_attributes(att(Module, _, Rest), Value, IDVar) -->
    { Module:verify_attributes(IDVar, Value, Goals) }, 
    Goals,
    call_verify_attributes(Rest, Value, IDVar).

% make code us verify_attributes/3 instead of attr_unify_hook/2
use_va(Var):-
  put_attr(Var,'$VAR$',Var).

%-----------------------------------------------------------------

verify_attributes(Var, _, Goals) :-
   get_attr(Var, '$VAR$', Info), !,
   \+ contains_var(Var,Info),
  Goals=[].

verify_attributes(_, _, []).


% Swi-pre-unify Case#1  not able to emulate in SWI  due to "Identity"

swiu_case_1 :-
 use_va(Y), put_attr(Y,'$VAR$',Y),
 Y = 4201.

% must fail
test_case_1 :-  \+  swiu_case_1.


%-----------------------------------------------------------------

% Swi-pre-unify Case#2   "Identity"

swiu_case_2 :-
   use_va(Y), put_attr(Y, '$VAR$', al(Y,a(X))),
   X = 420,
   Y = 420.

% must fail
test_case_2 :-  \+  swiu_case_2.


% -----------------------------------------------------------------
% Swi-pre-unify Case #3   "Identity" (fixed from last email)

swiu_case_3 :-
  use_va(Y), put_attr(Y,'$VAR$', a(420)),
  Y = 420.

% must Succeed
test_case_3 :-  swiu_case_3.



%-----------------------------------------------------------------
% Swi-pre-unify Case #4  more "Identity"

swiu_case_4 :-
 use_va(Y), put_attr(Y,'$VAR$', X),
 X = 420,
 Y = 420.

% must succeed
test_case_4 :-  swiu_case_4.


% ==========================================
%  Unify hook
% ==========================================

% 'unify':attr_unify_hook(_,_).  % OR tracing with 'unify':attr_unify_hook(N,Var):- mdwq(meta_unify_hook(N,Var)).


% multivar(Var):- put_attr(Var,unify,Var).
% multivar(Var):- put_attr(Var,'$VAR$',Var).

xvarx(Var):- 
   get_attr(Var,'$VAR$',MV)-> var(MV) ; 
   (get_attrs(Var,Attrs) -> put_attrs(Var,att('$VAR$',Var,Attrs)) ;
   (true -> put_attrs(Var,att('$VAR$',Var,[])))).

 

% is_mv(Var):- attvar(Var),get_attr(Var,unify,Waz),var(Waz).
is_mv(Var):- attvar(Var),get_attr(Var,'$VAR$',_Waz).

% ==========================================
% ATOM_dvard override TODO
% ==========================================

'$VAR$':attr_unify_hook(_,_).
'$VAR$':attribute_goals(Var) --> {is_implied_xvarx(Var)}->[] ; [xvarx(Var)].

is_implied_xvarx(MV):- get_attrs(MV,ATTS),is_implied_xvarx(MV,ATTS).
is_implied_xvarx(MV,att(M,Val,ATTS)):- ((Val==MV, \+ atom_concat('$',_,M)) -> true ; is_implied_xvarx(MV,ATTS)).
% ==========================================
% Variant override TODO
% ==========================================

'variant':attr_unify_hook(_,_).
user:attvar_variant(N,Var):- (N==Var -> true ;  mdwq_call( \+ \+ =(N,Var) )).

% ==========================================
% reference override TODO
% ==========================================

'references':attr_unify_hook(_,_).
user:attvar_references(N,Var):- (N==Var -> true ;  mdwq_call( \+ \+ =(N,Var) )).


% ==========================================
% Sets values
% ==========================================
multivar(Var):- var(Var)->multivar1(Var);(term_variables(Var,Vars),maplist(multivar1,Vars)).
multivar1(Var):- xvarx(Var),(get_attr(Var,'$value',lst(Var,_))->true; put_attr(Var,'$value',lst(Var,[]))).
'$value':attr_unify_hook(lst(Was,Values),Becoming):- var(Was),attvar(Becoming),!,mv_add_values(Becoming,Values).
'$value':attr_unify_hook(lst(Var,_Values),Value):- mv_add1(Var,Value).

%'$value':attribute_goals(_)-->!.
'$value':attribute_goals(Var)--> {get_attr(Var,'$value',lst(Var,Values))},[mv_set_values(Var,Values)].
mv_set_values(Var,Values):- put_attr(Var,'$value',lst(Var,Values)).
mv_set1(Var,Value):- put_attr(Var,'$value',lst(Var,[Value])).
mv_add1(Var,NewValue):- Var==NewValue,!.
mv_add1(Var,NewValue):- mv_prepend1(Var,'$value',NewValue).
mv_add_values(Becoming,Values):- maplist(mv_add1(Becoming),Values).


mv_prepend1(Var,Mod,Value):- get_attr(Var,Mod,lst(Var,Was))->(prepend_val(Value,Was,New)->put_attr(Var,Mod,lst(Var,New)));put_attr(Var,Mod,lst(Var,[Value])).
mv_prepend_values(Becoming,Mod,Values):- maplist(mv_prepend1(Becoming,Mod),Values).

prepend_val(Value,[],[Value]).
prepend_val(Value,Was,[Value|NewList]):-delete_identical(Was,Value,NewList).

delete_identical([],_,[]).
delete_identical([Elem0|NewList],Elem1,NewList):-Elem1==Elem0,!.
delete_identical([ElemKeep|List],Elem1,[ElemKeep|NewList]):-delete_identical(List,Elem1,NewList).

% faster than mv_prepend1 - might use?
mv_prepend(Var,Mod,Value):- get_attr(Var,Mod,lst(Var,Was))->
   put_attr(Var,Mod,lst(Var,[Value|Was]));
   put_attr(Var,Mod,lst(Var,[Value])).

% ==========================================
% Peeks values
% ==========================================

mv_peek_value(Var,Value):- mv_members(Var,'$value',Value).
mv_peek_value1(Var,Value):- mv_peek_value(Var,Value),!.



% ==========================================
% Peeks any
% ==========================================

mv_members(Var,Mod,Value):- get_attr(Var,Mod,lst(_,Values)),!,member(Value,Values).
% mv_get_attr1(Var,Mod,Value):- mv_members(Var,Mod,Value),!.
           

guard_from_var(V):- nonvar(V),!.
guard_from_var(V):- attvar(V),!.
guard_from_var(V):- xvarx(V),!.

% ==========================================
% Allow-only values
% ==========================================

check_allow(Var,Value):- get_attr(Var,'$allow',lst(Var,Disallow)), memberchk_variant_mv(Value,Disallow).
mv_allow(Var,Allow):- guard_from_var(Allow),mv_prepend(Var,'$allow',Allow).
'$allow':attr_unify_hook(lst(Var,Allow),Value):- \+ ((memberchk_variant_mv(Value,Allow)->true;get_attr(Var,ic_text,_))),!,fail.
'$allow':attr_unify_hook(lst(Was,Values),Becoming):- 
  ignore((var(Was),attvar(Becoming),!,mv_prepend_values(Becoming,'$allow',Values))).
'$allow':attribute_goals(Var)--> {get_attr(Var,'$allow',Allow)},[mv_allow(Var,Allow)].

% ==========================================
% Disallow-only values
% ==========================================

check_disallow(Var,Value):- (get_attr(Var,'$disallow',lst(Var,Disallow)) -> \+ memberchk_variant_mv(Value,Disallow) ; true).
mv_disallow(Var,Disallow):- guard_from_var(Disallow),mv_prepend(Var,'$disallow',Disallow).
'$disallow':attr_unify_hook(lst(_Var,Disallow),Value):-  memberchk_variant_mv(Value,Disallow),!,fail.
'$disallow':attr_unify_hook(lst(Was,Values),Becoming):- 
   ignore((var(Was),attvar(Becoming),!,mv_prepend_values(Becoming,'$disallow',Values))).
'$disallow':attribute_goals(Var)--> {get_attr(Var,'$disallow',Disallow)},[mv_disallow(Var,Disallow)].

%% memberchk_variant_mv( ?X, :TermY0) is semidet.
%
% Memberchk based on == for Vars else =@= .
%
memberchk_variant_mv(X, List) :- is_list(List),!, \+ atomic(List), C=..[v|List],(var(X)-> (arg(_,C,YY),X==YY) ; (arg(_,C,YY),X =@= YY)),!.
memberchk_variant_mv(X, Ys) :-  nonvar(Ys), var(X)->memberchk_variant0(X, Ys);memberchk_variant1(X,Ys).
memberchk_variant0(X, [Y|Ys]) :-  X==Y  ; (nonvar(Ys),memberchk_variant0(X, Ys)).
memberchk_variant1(X, [Y|Ys]) :-  X =@= Y ; (nonvar(Ys),memberchk_variant1(X, Ys)).



% ==========================================
% Label values
% ==========================================

un_mv(Var):-del_attr(Var,'$VAR$')->(mv_peek_value(Var,Value)*->Var=Value;true);true.
un_mv1(Var):-del_attr(Var,'$VAR$')->ignore(mv_peek_value1(Var,Var));true.


% ==========================================
% Examples
% ==========================================
/*

% ?- multivar(X),X=1,X=2,un_mv(X),writeq(X).
% ?- multivar(X),X=x(X),mv_allow(X,hello),mv_allow(X,hi), X=hello,X=hi,mv_peek_value(X,V)
% ?- multivar(X),mv_allow(X,hello),mv_allow(X,hi), X=hello,X=hi,writeq(X).
% ?- multivar(X),mv_allow(X,hello),mv_allow(X,hi),X=hello,X=hi,X=hello,un_mv(X).
% ?- multivar(X),mv_allow(X,hello),mv_allow(X,hi),X=hello,X=hi,X=hello,!,un_mv(X)
% ?- multivar(X),mv_allow(X,One),X=good,!,un_mv(X).
% ?- \+ (multivar(X),mv_allow(X,One),X=good,X=bad,un_mv(X)).


% ?- \+ (ic_text(X),X="GOOD",X=good,X=one).
% ?- ic_text(X),X=good,X=gooD,un_mv(X).
% ?- ic_text(X),X="GOOD",X=good.
% ?- ic_text(X),mv_allow(X,"GOOD"),mv_allow(X,"BAD"),X=good,X=baD.
% ?- \+ (ic_text(X),mv_allow(X,"GOOD"),mv_allow(X,"BAD"),X=good,X=one).

?- multivar(X),mv_disallow(X,1),mv_disallow(X,3).
multivar(X),
mv_disallow(X, lst(X, [3, 1])).

*/
% ==========================================
% Prolog-Like vars
% ==========================================
plvar(Var):- multivar(Var), put_attr(Var,plvar,Var).
plvar:attr_unify_hook(Var,Value):- mv_peek_value1(Var,Was)->Value=Was;mv_set1(Var,Value).
'plvar':attribute_goals(Var)--> {get_attr(Var,'plvar',Var)},[plvar(Var)].


% Maybe Variables entering the clause database
:- meta_predicate multivar_call(1,0).
multivar_call(Type,Goal):-term_variables(Goal,Vars),maplist(Type,Vars),call(Goal).


% ==========================================
% Symbol-Like Global vars
% ==========================================
nb_var(Var):- gensym(nb_var_,Symbol),nb_var(Symbol, Var).
nb_var(Symbol, Var):- multivar(Var), put_attr(Var,nb_var,lst(Var,Symbol)), nb_linkval(Symbol,Var).

% This should pretend to be be value1 slot instead
% so that we extext mv_peek_value1/2 and mv_set1/2
% to stroe things in GVAR in the case of a nb_var
nb_var:attr_unify_hook(lst(_Var,Symbol),Value):-
       nb_getval(Symbol,Prev),
       ( % This is how we produce a binding for +multivar "iterator"
          (var(Value),nonvar(Prev)) ->  Value=Prev;
         % same binding (effectively)
             Value==Prev->true;
         % On unification we will update the internal '$value'
             Value=Prev->nb_setval(Symbol,Prev)).

% ==========================================
% Hashmap-Like vars
% ==========================================


vdict(Var):- multivar(Var), put_attr(Var,vdict,Var).
vdict(Value,Var):- vdict(Var),Var=Value.
vdict:attr_unify_hook(Var,OValue):- to_dict(OValue,Value)-> mv_peek_value(Var,Prev), merge_dicts(Prev,Value,Result)-> mv_set1(Var,Result).


to_dict(Value,Value):- is_dict(Value),!.
to_dict(OValue,Value):- is_list(OValue),!,dict_options(Value,OValue).
to_dict(OValue,Value):- compound(OValue),!,option(OValue,[MValue]),!,dict_options(Value,[MValue]).
to_dict(OValue,Value):- option('$value'=OValue,[MValue]),!,dict_options(Value,[MValue]).
                                                              

merge_dicts(Value,Value,Value).
merge_dicts(Prev,Value,Prev):- Value :< Prev.
merge_dicts(Value,Prev,Prev):- Value :< Prev.
merge_dicts(Dict1,Dict2,Combined):- dicts_to_same_keys([Dict1,Dict2],dict_fill(_),[Combined,Combined]).



% ==========================================
% Insensitively cased text
% ==========================================

ic_text(Var):- put_attr(Var,ic_text,Var),multivar(Var),!.

ic_text:attr_unify_hook(Var,Value):- check_disallow(Var,Value),
 ((mv_members(Var,'$allow',One);mv_peek_value1(Var,One))*-> ic_unify(One,Value)).

'ic_text':attribute_goals(Var)--> {get_attr(Var,'ic_text',Var)},[ic_text(Var)].
/*
*/

ic_unify(One,Value):- (One=Value -> true ; (term_upcase(One,UC1),term_upcase(Value,UC2),UC1==UC2)).

term_upcase(Value,UC2):-catch(string_upper(Value,UC2),_,(format(string(UC1),'~w',Value),string_upper(UC1,UC2))).
/*
:-
 source_location(S,_), prolog_load_context(module,LC),
 forall(source_file(M:H,S),
 (functor(H,F,A),
  ignore(((\+ predicate_property(M:H,transparent), module_transparent(M:F/A), 
  \+ atom_concat('__aux',_,F),debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A])))),
    ignore(((\+ atom_concat('$',_,F),\+ atom_concat('__aux',_,F),LC:export(M:F/A), 
  (current_predicate('system':F/A)->true; 'system':import(M:F/A))))))).
*/

:- fixup_exports.

