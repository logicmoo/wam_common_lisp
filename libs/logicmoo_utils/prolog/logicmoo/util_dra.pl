/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/

:- module(logicmoo_util_dra,[]).

end_of_file.


:- prolog_debug('MSG_DRA').
:- prolog_debug('MSG_METATERM').
%:- prolog_debug('MSG_GC_ASSIGNMENTS_MARK').
%:- prolog_debug('MSG_CALL_RESIDUE_VARS').
:- prolog_debug('MSG_CONTINUE').


:- set_prolog_flag(debugger_show_context,true).

:- dynamic(ffooo/0).
:- dynamic(ffooo/1).
% % % OFF :- system:use_module(library(drac)).
:- det(ffooo).
:- htb_nb_setcopy(ffooo,oldt,1).

% :- ensure_loaded(system:library(logicmoo_utils_all)).

:- dra_call interp
    ffooo/0,
    ffooo/1.

:- meta_predicate(dshow_all(0)).
dshow_all(G):-forall( G,(writeq(G),nl)).

% :- dshow_all(predicate_property(ffooo,_)).


% Simpler example than example12.pl, but the number of predicates involved in mutual recursion will also increase at runtime.

expected_variants([p(3,_),p(2,_),q(2,_),q(3,_),p(_,_)]).
% Note: p(3,_) and q(3,_) are empty tables, but they are there.
expected_answers_for_variant(p(_,_),[p(1,2),p(2,3),p(1,3)]).
expected_answers_for_variant(p(3,_),[]).
expected_answers_for_variant(p(2,_),[p(2,3)]).
expected_answers_for_variant(q(2,_),[q(2,3)]).
expected_answers_for_variant(q(3,_),[]).

gop :- once(p(_X,_Y)).

:- discontiguous p/2.
:- table(p/2).

p(X,Y) :- p(X,Z), q(Z,Y).
p(X,Y) :- e(X,Y).

:- discontiguous q/2.
:- table(q/2).

q(X,Y) :- p(X,Y).

e(1,2).
e(2,3).

avoids(Source,Target):-avoids0(Source,Target),\=(Source,Target).

:- discontiguous avoids0/2.
:- table avoids0/2.
avoids0(Source,Target) :- owes(Source,Target).
avoids0(Source,Target) :-
        owes(Source,Intermediate),
	avoids0(Intermediate,Target).

owes(andy,bill).
owes(bill,carl).
owes(carl,bill).

:- findall(avoids(Source,Target),avoids(Source,Target),List),
  List==[avoids(andy, bill), avoids(bill, carl), avoids(carl, bill), avoids(andy, carl)].



