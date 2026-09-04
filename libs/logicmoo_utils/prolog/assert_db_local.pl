:- module(assert_db_Local, [
 goal_L_expansion/3,  retractall_L_hp/2,  retractall_L/2,  retractall_L/1, 
 retract_L_hp/2,  retract_L/2,  retract_L/1,  into_hp_L/2,  
 check_L/1,  call_L/1,  call_L/2,  install_L/0,  reset_L/0,  last_cons/2, 
 assertz_L_hp/2,  assertz_L/2,  assertz_L/1,  assert_L/2,  
 assert_L/1,  asserta_L_hp/2,  asserta_L/2,  asserta_L/1,  erase_L/2,  clause_L/4,  
 ref_L/3,  clause_L/3,  erase_L/1,  show_L/1,  
 listing_L/1,  listing_L/0,  was_dynamic_L/2,  
 make_dynamic_L/3,  dynamic_L/1,  install_L/0, disable_L/0, enable_L/0]).

:- set_module(class(library)).
% :- use_module(library(logicmoo_common)).

/*  Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_utivarnames_L.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_utivarnames_L.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/

:- module_transparent((
 goal_L_expansion/3,  retractall_L_hp/2,  retractall_L/2,  retractall_L/1, 
 retract_L_hp/2,  retract_L/2,  retract_L/1,  into_hp_L/2,  
 check_L/1,  call_L/1,  call_L/2,  install_L/0,  reset_L/0,  last_cons/2, 
 assertz_L_hp/2,  assertz_L/2,  assertz_L/1,  assert_L/2,  
 assert_L/1,  asserta_L_hp/2,  asserta_L/2,  asserta_L/1,  erase_L/2,  clause_L/4,  
 ref_L/3,  clause_L/3,  erase_L/1,  show_L/1,  
 listing_L/1,  listing_L/0,  was_dynamic_L/2,  
 make_dynamic_L/3,  dynamic_L/1,  install_L/0, disable_L/0, enable_L/0)).

:- dynamic(tmpldb:is_dynamic_L/1).
:- volatile(tmpldb:is_dynamic_L/1).
:- dynamic(tmpldb:is_file_L/1).
:- volatile(tmpldb:is_file_L/1).

disable_L:- set_prolog_flag(enable_L,false).
enable_L:- set_prolog_flag(enable_L,true).
install_L:- prolog_load_context(source, F), asserta(tmpldb:is_file_L(F)).

dynamic_L((A, B)):- !, dynamic_L(A), dynamic_L(B).
dynamic_L(F/A):- current_prolog_flag(enable_L,false), !, dynamic(F/A).
dynamic_L(F/A):- functor(P, F, A), make_dynamic_L(P, F, A).

make_dynamic_L(P, _, _):- tmpldb:is_dynamic_L(P), !.
% only fresh preds (not defined elsewhere)
make_dynamic_L(P, F, A):- \+ current_prolog_flag(enable_L,true), predicate_property(P, defined), !, dynamic(F/A).
make_dynamic_L(P, F, A):-
   prolog_load_context(module, M),
   assert(tmpldb:is_dynamic_L(P)), !,
   assert((M:(P:- call_L(P)))),
   compile_predicates([M:F/A]).


was_dynamic_L(_, _):- current_prolog_flag(enable_L,false), !, fail.
was_dynamic_L((P:-_), DB):- !, was_dynamic_L(P, DB).
was_dynamic_L(P, DB):- tmpldb:is_dynamic_L(P), !, db_L(DB).
was_dynamic_L(P, DB):- \+ predicate_property(P, defined),
  functor(P, F, A), make_dynamic_L(P, F, A), !, db_L(DB), !.


db_L(DB):- nb_current('$db', DB).


listing_L:-
  listing(tmpldb:_),
  db_L(DB), listing_L(DB).

listing_L(val(H, _, _)):-
  format('~N~n', []),
  maplist(show_L, H),
  %format('~N% Tail~n', []),
  %maplist(show_L, T),
  !.

show_L(H:-B):- B==true, !, format('~NL: ~p.~n', [H]).
show_L(H:-B):- !, format('~NL: ~p.~n', [H:-B]).
show_L(HP):- HP=..[F,Body|ARGS], H=..[F|ARGS], show_L(H:-Body).

%! asserta_L(+P) is det
% 
% Assert P at the begining of the Prolog database
%  If this was a special local-only clause do this in the "localized" Prolog database
asserta_L(P):- was_dynamic_L(P, DB), !, asserta_L(DB, P), check_L(DB).
asserta_L(P):- asserta(P).
asserta_L(DB, P) :- into_hp_L(P, HP), asserta_L_hp(DB, HP).

assertz_L(P):- was_dynamic_L(P, DB), !, assertz_L(DB, P).
assertz_L(P):- assertz(P).
assertz_L(DB, P) :- into_hp_L(P, HP), assertz_L_hp(DB, HP), check_L(DB).

assert_L(P):- assertz_L(P).
assert_L(DB, H):- assertz_L(DB, H).

erase_L(Ref):- compound(Ref), db_L(DB), !, erase_L(DB, Ref).
erase_L(Ref):- erase(Ref). 

call_L(P):- was_dynamic_L(P, DB), !, call_L(DB, P).
call_L(P):- call(P).

call_L(val(List,_,_), P):- into_hp_L((P:-Body), HP), member(HP, List), call(Body).

clause_L(H, B, Ref):- was_dynamic_L(H, DB), !, clause_L(DB, H, B, Ref).
clause_L(H, B, Ref):- clause(H, B, Ref).

clause_L(val(List, _, _), H, B, Ref):- H=..[F|HL], HP=..[F,B|HL], ref_L(List, HP, Ref).

retract_L(P):- was_dynamic_L(P, DB), !, retract_L(DB, P).
retract_L(P):- retract(P).
retract_L(DB, P):- into_hp_L(P, HP), retract_L_hp(DB, HP).

retractall_L(P):- was_dynamic_L(P, DB), !, retractall_L(DB, P).
retractall_L(P):- retractall(P).
retractall_L(DB, P0 :- _ ):- !, into_hp_L(P0:- _, P), retractall_L_hp(DB, P).
retractall_L(DB, P0      ):-    into_hp_L(P0:- _, P), retractall_L_hp(DB, P).

reset_L:-
  nb_setval('$db', val( [empty], _, _)),
  db_L(DB),
  arg(1, DB, End),
  nb_linkarg(2, DB, End),
  db_L(DB2),
  check_real_L(DB2).



ref_L(List, E, OneLeft):- List=[H|T], (H=E -> OneLeft=List ; ref_L(T, E, OneLeft)).

into_hp_L((H:-B), HP):- H=..[F|HL], HP=..[F,B|HL].
into_hp_L(  H   , HP):- H=..[F|HL], HP=..[F,true|HL].


asserta_L_hp(DB, P):-
  nb_setarg(3, DB, P), arg(3, DB, CopyP),
  arg(1, DB, Front), nb_linkarg(1, DB, [CopyP|Front]).

assertz_L_hp(DB, P):-
  arg(2, DB, WasEnd),
  assertz_L_hp_3(DB, WasEnd, P), !.

assertz_L_hp_3( _, WasEnd, P):- arg(1, WasEnd, empty), !, nb_setarg(1, WasEnd, P).
assertz_L_hp_3(DB, WasEnd, P):-
  nb_setarg(3, DB, [P]), arg(3, DB, NewEnd),
  % NewEnd now has duplicated term
  nb_linkarg(2, WasEnd, NewEnd),
  nb_linkarg(2, DB, NewEnd).

retract_L_hp(DB, P):-
  arg(1, DB, List),
  notrace(ref_L(List, P, Ref)),
  check_L(DB),
  erase_L(DB, Ref).

erase_L(DB, Ref):- Ref=[_|T], T= [H1|T1],
   nb_linkarg(2, Ref, T1), nb_linkarg(1, Ref, H1),
   (T1 \==[] -> true ; nb_linkarg(2, DB, Ref)), !, check_L(DB).
erase_L(DB, Ref):- DB=val(_, Ref, _), nb_linkarg(1, Ref, empty), !, check_L(DB).

check_L(_).
check_real_L(val(List, End, _)):- notrace((last_cons(List, Last), (same_term(End, Last)->true;throw(not_same_term(End, Last))))).


last_cons(In, Last):- In=[_|List],
  ( ( \+ List=[_|_] ; var(List))
    -> Last=In
    ;  last_cons(List, Last)).

retractall_L_hp(DB, P):- \+ \+ retract_L_hp(DB, P), !, retractall_L_hp(DB, P).
retractall_L_hp(_, _).

goal_L_expansion(_, _, _):- prolog_load_context(source, F), \+ tmpldb:is_file_L(F), !, fail.
goal_L_expansion(_, I, O):- do_goal_L_expansion(I, O).

do_goal_L_expansion(dynamic(P), dynamic_L(P)).
do_goal_L_expansion(assert(P),  assert_L(P)).
do_goal_L_expansion(assertz(P), assertz_L(P)).
do_goal_L_expansion(asserta(P), asserta_L(P)).
do_goal_L_expansion(retract(P), retract_L(P)).
do_goal_L_expansion(retractall(P), retractall_L(P)).
do_goal_L_expansion(erase(P), erase_L(P)).
do_goal_L_expansion(clause(H, B), clause_L(H, B, _)).
do_goal_L_expansion(clause(H, B, R), clause_L(H, B, R)).

:- if(current_predicate(fixup_exports/0)).
:- fixup_exports.
:- else.
module_L:-
 prolog_load_context(source, S), prolog_load_context(module, M),
 forall(source_file(M:H, S),
 (functor(H, F, A),
 format('~N ~q/~q, ~n', [F, A]))).
:- endif.

:- reset_L.

:- system:import(goal_L_expansion/3).
:- '$toplevel':import(goal_L_expansion/3).

:- multifile(system:goal_expansion/2).
:- module_transparent(system:goal_expansion/2).
system:goal_expansion(I, O):-
  prolog_load_context(module, M),
  goal_L_expansion(M, I, O).

