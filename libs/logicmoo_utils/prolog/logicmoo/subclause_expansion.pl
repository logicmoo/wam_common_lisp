/* Part of LogicMOO Base bb_env
% Provides a prolog database *env*
% ===================================================================
% File 'clause_expansion.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles logicmoo@gmail.com ;
% Version: 'clause_expansion.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2021/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/clause_expansion.pl
:- module(subclause_expansion, [save_pred_to/2,with_subclause_expansion/1]).

/** <module>Utility LOGICMOO SUBCLAUSE EXPANSION
This module fixes all the problems with prolog term expansion by designing how terms will be divided between clauses goals and basic terms. 
- @author Douglas R. Miles
- @license LGPL

Prolog compile-time and runtime source-code transformations
 This module specifies a set of more specialized term and goal expansions as they are read from a file before they are processed by the compiler.

The toplevel is expand_clause/2.  This uses other translators:

	* Conditional compilation
	* clause_expansion/2 rules provided by the user

Note that this ordering implies  that conditional compilation directives
cannot be generated  by  clause_expansion/2   rules:  they  must literally
appear in the source-code.

*/


:- set_module(class(library)).
% % % OFF :- system:use_module(library(apply)).

:- create_prolog_flag(subclause_expansion,true,[keep(true)]).


:- dynamic(sce:buffer_clauses/5).
:- volatile(sce:buffer_clauses/5).

mst(G):- catch((G*->true;writeln(failed_mst(G))),_E,writeln(err(G))).

call_pred_to(Where,List):-  is_list(List),!,maplist(call_pred_to(Where),List).
call_pred_to(Where,F/A):-   call_pred_to(Where,_:F/A).
call_pred_to(Where,M:F/A):- ground(F/A),functor(P,F,A),call_pred_to(Where,M:P).
call_pred_to(Where,M:F/A):- forall(current_predicate(F/A),((functor(P,F,A),call_pred_to(Where,M:P)))).
call_pred_to(Where,M:P):-   var(M),!,forall(current_module(M),call_pred_to(Where,M:P)).
call_pred_to(Where,M:P):-   !,call(Where,M,P).
call_pred_to(Where,P):-     forall(current_module(M),call_pred_to(Where,M:P)).


save_pred_to(Where,Each):-
  call_pred_to(save_pred_to_Act(Where),Each).

save_pred_to_Act(Where,M,P):-
  forall(clause(M:P,_,Ref), 
    (sce:buffer_clauses(Where,M,_,_,Ref)-> true;
     ( ((clause(H,B,Ref), (clause_property(Ref,module(_))->true;throw( clause(H,B,Ref))),
    ignore(((clause_property(Ref,module(M)),assert(sce:buffer_clauses(Where,M,H,B,Ref)),true)))))))).

erase_except(Where,Each):-
  call_pred_to(erase_except_Act(Where),Each).

erase_except_Act(Where,M,P):-
    forall(clause(M:P,_,Ref), 
    ((clause(HH,BB,Ref), 
     (clause_property(Ref,module(_))->true;throw( clause(HH,BB,Ref))),
     ignore(((clause_property(Ref,module(M)),\+ (sce:buffer_clauses(Where,M,HH,BB,Ref)),
              % writeln(erase(HH,BB,Ref)),
              set_prolog_flag(access_level,system),
              catch(M:erase(Ref),_E,mst(M:retract((HH:-BB)))))))))).

restore_preds(Where):-
 forall(sce:buffer_clauses(Where,M,H,B,Ref),
    (M:clause(H,B,Ref)->true; M:assert(H,B))).
 

erase_preds(Where):-
 forall(sce:buffer_clauses(Where,M,H,B,Ref),
    (M:clause(H,B,Ref)->erase(Ref);true)).
 


:- save_pred_to(load_expansion,[term_expansion/2,term_expansion/4,goal_expansion/2,goal_expansion/4]).


% :- listing(sce:buffer_clauses/5).

:- if( \+ current_predicate(system:each_call_cleanup/3)).
% % % OFF :- system:use_module(system:library(logicmoo/each_call)).
:- endif.

:- set_module(class(library)).

:- multifile((system:clause_expansion/2,
              system:directive_expansion/2,
              system:file_body_expansion/3)).
:- dynamic((  system:clause_expansion/2,
              system:directive_expansion/2,
              system:file_body_expansion/3)).

/*
:- multifile((user:clause_expansion/2,
              user:directive_expansion/2,
              user:file_body_expansion/3)).
:- dynamic((  user:clause_expansion/2,
              user:directive_expansion/2,
              user:file_body_expansion/3)).
*/

:- meta_predicate without_subclause_expansion(0).

% with_subclause_expansion(Goal):- current_prolog_flag(subclause_expansion,true),!,call(Goal).
with_subclause_expansion(Goal):- locally(set_prolog_flag(subclause_expansion,true),Goal).

% without_subclause_expansion(Goal):- current_prolog_flag(subclause_expansion,false),!,call(Goal).
without_subclause_expansion(Goal):- locally(set_prolog_flag(subclause_expansion,false),Goal).

:- multifile(system:goal_expansion/4).
:- dynamic(system:goal_expansion/4).
:- multifile(system:term_expansion/4).
:- dynamic(system:term_expansion/4).


:- nb_setval( '$term_user',[]).
:- initialization(nb_setval( '$term_user',[]),restore).
:- initialization(nb_setval( '$term_position',[]),restore).
:- initialization(nb_setval( '$term',[]),restore).


call_expansion_from(From, Type, In, Out):-
   functor(Type,F,A),APlus2 is A + 2,
  '$def_modules'(From:[F/APlus2], MList),
   call_expansions(MList,Type,[], In,  Out).

:- module_transparent(call_expansion_from/4).

call_expansions([],_,_, InOut, InOut).
call_expansions([M-_|T], Type,Completed, In, Out) :- 
  ((\+ memberchk(M,Completed), M:call(M:Type, In, Mid)) -> true ; In = Mid),
 call_expansions(T, Type,[M|Completed], Mid, Out).

:- module_transparent(call_expansions/5).

% directive_expansion
file_expansion(From,Term,(:- DirIn),(:- DirOut)):-
   (Term == (:- DirIn)) -> 
   call_expansion_from(From,directive_expansion,DirIn, DirOut),!.

% clause_expansion
file_expansion(From,Term,In,Out):- 
   Term == In ->  call_expansion_from(From,clause_expansion,In,Out),!.

% file_body_expansion
file_expansion(From,Term,(Head:-In),(Head:-Out)):-
   Term == (Head:-In) ->  call_expansion_from(From,file_body_expansion(Head),In,Out),!.

:- module_transparent(file_expansion/4).


subclause_term_expansion(In,Pos,Out):-   
  notrace(\+ current_prolog_flag(subclause_expansion,false)),
  \+ current_prolog_flag(xref,true),
  nonvar(Pos),nonvar(In),
  nb_current('$term',FileTerm),
  prolog_load_context(module,From),
  file_expansion(From,FileTerm,In,FileTermOut),!, In\=@=FileTermOut,
  %\+ current_prolog_flag(xref,true),
  Out=FileTermOut,
  b_setval('$term',FileTermOut).


system:term_expansion(In,Pos,Out,PosOut):- In\==end_of_file, 
   subclause_term_expansion(In,Pos,Out)->PosOut=Pos.
user:term_expansion(In,Pos,_,_):- nonvar(Pos), nb_setval('$term_user',In),fail.

/*
system:file_body_expansion(Head,I,_):- current_prolog_flag(show_expanders,true),dmsg(system:file_body_expansion(Head:-I)),fail.
system:clause_expansion(I,_):- current_prolog_flag(show_expanders,true),dmsg(system:clause_expansion(I)),fail.
system:directive_expansion(I,_):-  current_prolog_flag(show_expanders,true),dmsg(system:directive_expansion(I)),fail.
:- set_prolog_flag(show_expanders,true).
*/

:- fixup_exports.


