:- module(nb_set_term, [nb_set_has/2, nb_set_add/2, nb_set_add1/2, nb_set_rem/2, nb_set_rem1/2]).
/** <module> Utility LOGICMOO NB SET TERM
This module allows non-backtrackable manipulation of prolog data. 
Manipulate and edit terms without worrying about them reverting. 

- @author Douglas R. Miles
- @license LGPL 
*/

*/
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

%nb_set_has(F, List):- arg(N, List, E), E =@= F, !.
nb_set_has(Set, F):- must_be(compound,Set),functor(Set,_, A),  
  ((arg(N, Set, E), N < A, E=@=F) -> true;
   (arg(A,Set,T), ((T==[];var(T)) -> (!,fail) ; nb_set_has(T, F)))).

nb_set_unify(Set, F, E):- must_be(compound,Set),functor(Set,_, A), 
  ((arg(N, Set, E), N < A, E=F) -> true;
   (arg(A,Set,T), ((T==[];var(T)) -> (!,fail) ; nb_set_unify(T, F, E)))).

nb_set_add(Set, List):- is_list(List), !, maplist(nb_set_add1(Set),List).
nb_set_add(Set, E):- nb_set_add1(Set,E), !.

nb_set_add1(Set, F):- must_be(compound,Set),functor(Set,_, A), 
  ((arg(N, Set, E), N < A, E=@=F) -> true;
   (arg(A,Set,T), ((T==[];var(T)) -> nb_linkarg(A, Set, [F]) ; nb_set_add1(T, F)))).

nb_set_rem(Set, List):- is_list(List), !, maplist(nb_set_rem1(Set),List).
nb_set_rem(Set, E):- nb_set_rem1(Set,E), !.

nb_set_rem1(Set, F):- must_be(compound,Set), functor(Set,_, A), 
  ((arg(N, Set, E), N < A, E=@=F) -> throw(cant_remove(arg(N, Set, E))) ;
   (arg(A,Set,T), ((T==[];var(T)) -> true ; nb_set_rem1(T, F)))).


nb_remove_first(List):- List = [_|Second],compound(Second),arg(1,Second,NewFirst),arg(2,Second,NewRest),nb_setarg(1,List,NewFirst),nb_setarg(2,List,NewRest),!.
remove_el_via_setarg(List,Value):- [Value,_|_] = List, !, nb_remove_first(List).
remove_el_via_setarg([_|List],Value):- remove_el_via_setarg(List,Value).

append_el_via_setarg(List,Value):- List = [_|T], (T==[] -> setarg(2,List,[Value]) ; append_el_via_setarg(T,Value)).

merge_nb_values(Into,From):- Into=@=From,!.
merge_nb_values(Into,From):- is_list(From),!,maplist(nb_set_add1(Into),From).
merge_nb_values(Into,From):- is_list(Into),!,maplist(merge_nb_values(Into),From).
merge_nb_values(Into,From):- \+ compound(From),!, nb_set_add1(Into,From).
merge_nb_values(Into,From):- compound(Into),
  compound_name_arguments(From,FF,ArgF),
  compound_name_arguments(Into,FI,ArgI),
  FF=FI, !, 
  maplist(merge_nb_values_if_differnt(Into),ArgI,ArgF).
merge_nb_values(Into,From):- duplicate_term(From,F),nb_set_add1(Into,F).

merge_nb_values_if_differnt(_,To,From):- To=@=From,!.
merge_nb_values_if_differnt(_,To,From):- is_list(To),!,merge_nb_values(To,From).
merge_nb_values_if_differnt(Into,_,From):-  merge_nb_values(Into,From),!.

find_subterm(P,Seg):- find_subterm(P,Seg,_).

find_subterm(P,Seg,S):- compound(P), sub_term(S,P),nonvar(S),Seg=S.




:- fixup_exports.

