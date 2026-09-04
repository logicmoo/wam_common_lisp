:- module(call_reorder, [freeze/1,contains_goal/2,freeze_each/2,freeze_atom_concat/3]).
:- set_module(class(library)).
/*  Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_util_varnames.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_varnames.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/
:- meta_predicate(freeze(:)).
freeze(Goal):-
 notrace(term_variables(Goal,Vs)),
 (Vs \= [_,_|_] -> call(Goal);
   ((
    notrace(( \+ (( contains_goal(Goal, call_reorder:freeze(Goal)), nop(dmsg(contains_goal(freeze(Goal)))))))),
    notrace(freeze_each(Vs, freeze(Goal)))))).

:- meta_predicate(freeze1(:)).
freeze1(Goal):-
 notrace(term_variables(Goal,Vs)),
 (Vs \= [_,_|_] -> call(Goal);
   ((
    notrace(freeze_each(Vs, freeze1(Goal)))))).


contains_goal(V,Goal):- term_attvars(V,AVs), !, contains_goal1(AVs,Goal).

contains_goal1([],_):-!,fail.
contains_goal1([V|Vs],Goal):- (frozen(V,GGs),in_conj(Goal,GGs))->true
 ; contains_goal1(Vs,Goal).

in_conj(Goal, GG):- in_conj1(Goal, GG).

in_conj1(Goal, GG):- Goal == GG -> true ; ( \+ callable(GG) ,!, fail).
in_conj1(Goal,(G1,G2)):- Goal == G1 -> true ; in_conj1(Goal,G2).

freeze_each([],_Goal):- !.
freeze_each([V|Vs],Goal):- freeze(V,Goal), freeze_each(Vs,Goal).

freeze_atom_concat(L,R,LR):- freeze(atom_concat(L,R,LR)).

:- fixup_exports.

