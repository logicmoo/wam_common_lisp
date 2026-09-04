/* Part of LogicMOO Base An Implementation a MUD server in SWI-Prolog
% ===================================================================
% File 'dcg_meta.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles logicmoo@gmail.com ;
% Version: 'logicmoo_util_bugger.pl' 1.0.0
% Revision:  $Revision: 1.1 $
% Created: $Date: 2002/07/11 21:57:28 $
% Revised At:   $Date: 2021/07/11 21:57:28 $
% ===================================================================
*/
:- module(dcg_must,[
	 dcg_peek/3]).

/** <module>Utility LOGICMOO_DCG_MUST
	Allows you to debug DCGs easier. 
- @author Douglas R. Miles
- @license LGPL 
*/

:- set_module(class(library)).

:- use_module(library(logicmoo_common)).

%dcg_must_each_det(G, S, E):- phrase(G, S, E), !.
quietly(DCG, S, E):- setup_call_cleanup(quietly(phrase(DCG, S, E)),true,true).
% quietly(DCG,S,E):- quietly(phrase(DCG,S,E)).
notrace(DCG,S,E):- quietly(DCG,S,E). %notrace(phrase(DCG,S,E)).
must(DCG,S,E):- must(phrase(DCG,S,E)).
ignore_must(DCG,S,E):- ignore_must(phrase(DCG,S,E)).

dcg_if_defined(DCG,S,E):- catch(phrase(DCG,S,E),error(existence_error(procedure,_),context(_,_47656)),fail).

dcg_peek(DCG,S,S):- phrase(DCG,S,_).

dcg_must_each_det(_, S, _):- S == [], !, fail.
dcg_must_each_det((G1, G2), S, E):- !, must(phrase(G1, S, M)), !, dcg_must_each_det(G2, M, E), !.
dcg_must_each_det(G, S, E):- !, must(phrase(G, S, E)), !.

dcg_and(DCG1, DCG2, S, E) :- dcg_condition(DCG1, S, E), phrase(DCG2, S, E), !.
dcg_unless(DCG1, DCG2, S, E) :- \+ dcg_condition(DCG1, S, _), !, phrase(DCG2, S, E).
dcg_when(DCG1, DCG2, S, E) :- dcg_condition(DCG1, S, _),!, phrase(DCG2, S, E).
dcg_length(Len,S,E):- \+ var(Len) -> (length(L,Len), append(L,E,S)); 
   (length(S,Full),between(Full,0,Len),length(L,Len), append(L,E,S)).
dcg_from_right(DCG1, DCG2, S, E) :- length(S,Full), between(Full,0,Start), dcg_scan(DCG1,Start,DCG2,S,E).
dcg_from_left(DCG1,  DCG2, S, E) :- length(S,Full), between(0,Full,Start), dcg_scan(DCG1,Start,DCG2,S,E).

dcg_scan(DCG1,Start2,DCG2,S,E):- 
  length(Before,Start2), append(Before,Mid,S), \+ \+ phrase(DCG2, Mid, _), 
  phrase(DCG1, Before, []), phrase(DCG2, Mid, E).

dcg_condition([], S, _):- S \== [], !, fail.
dcg_condition(DCG, S, E):- phrase(DCG, S, E).


% Push a new term onto DCG stack
dcg_push(List, S, ListS):- is_list(List), !, =(List,ListO), append(ListO, S, ListS).
dcg_push(A, S, [B|S]):- =(A,B).

:- fixup_exports.
