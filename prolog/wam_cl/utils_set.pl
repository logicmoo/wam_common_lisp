%   File   : set_utilities.PL
%   Author : Lawrence Byrd + R.A.O'Keefe
%   Updated: 15 November 1983
%   Purpose: Set manipulation utilities

% Updated by Neil Smith, for compatability with LPA Prolog 

%   Sets are represented as lists with no repeated elements.
%   An ordered representation could be much more efficient, but
%   these routines were designed before sort/2 entered the language.
 
:- module(set_utilities,
      [ add_element/3,		%  Elem x Set -> Set
	del_element/3,		%  Elem x Set -> Set
	disjoint/1,		%  List ->
	disjoint/2,		%  Set x Set ->
	intersect/2,		%  Set x Set ->
	intersect/3,		%  Set x Set -> Set
	listtoset/2,		%  List -> Set
	% member/2,		%  Elem <- Set
	memberchk/2,		%  Elem x Set ->
	pairfrom/4,		%  Set -> Elem x Elem x Set
	select/3,		%  Elem <- Set -> Set
	seteq/2,		%  Set x Set ->
	subset/2,		%  Set x Set ->
	subtract/3,		%  Set x Set -> Set
	symdiff/3,		%  Set x Set -> Set
	union/3 		%  Set x Set -> Set
      ]).

:- ensure_loaded((utils_for_swi)).

/**********************
 *:- mode
*	memberchk(+, +),
*	pairfrom(?, ?, ?, ?),
*	select(?, ?, ?),
*	add_element(+, +, -),
*	del_element(+, +, -),
*	disjoint(+),
*	disjoint(+, +),
*	intersect(+, +),
*	subset(+, +),
*	seteq(+, +),
*	listtoset(+, ?),
*	intersect(+, +, ?),
*	subtract(+, +, ?),
*	symdiff(+, +, ?),
*	    symdiff(+, +, ?, ?),
*	union(+, +, ?).
**************************/ 

 
%   memberchk(+Element, +Set)
%   means the same thing as member/2, but may only be used to test 
%   whether a known Element occurs in a known Set.  In return for 
%   this limited use, it is more efficient when it is applicable.
 
% memberchk(Element, [Element|_]) :- !.
% memberchk(Element, [_|Rest]) :-
% 	memberchk(Element, Rest).

memberchk(Element, List) :-
	one member(Element, List).
 
 
 
%   add_element(Elem, Set1, Set2)
%   is true when Set1 and Set2 are sets represented as unordered lists,
%   and Set2 = Set1 U {Elem}.  It may only be used to calculate Set2
%   given Elem and Set1.  However, if Set1 is a list with a variable at
%   the end, it may still be used, and will add new elements at the end.
 
add_element(Elem, Set, Set) :-
	memberchk(Elem, Set),
	!.
add_element(Elem, Set, [Elem|Set]).
 
 
%   del_element(Elem, Set1, Set2)
%   is true when Set1 and Set2 are sets represented as unordered lists,
%   and Set2 = Set1 \ {Elem}.  It may only be used to calculate Set2
%   given Elem and Set1.  If Set1 does not contain Elem, Set2 and Set1
%   will be equal.  I wanted to call this predicate 'delete', but other
%   Prologs have used that for 'select'.  If Set1 is not an unordered
%   set, but contains more than one copy of Elem, only the first will
%   be removed.
 
del_element(Elem, [Elem|Set2], Set2) :- !.
del_element(Elem, [X|Set1], [X|Set2]) :- !,
	del_element(Elem, Set1, Set2).
del_element(_, [], []).
 
 
%   disjoint(+Set)
%   is true when Set is a list that contains no repeated elements.
%   disjoint/1 and disjoint/2 used to be defined using \+, but for
%   speed (as the Dec-10 compiler does not understand \+), this is
%   no longer so.  Sorry 'bout the !,fails, the price of speed.
 
disjoint([Head|Tail]) :-
	memberchk(Head, Tail),
	!, fail.
disjoint([_|Tail]) :- !,
	disjoint(Tail).
disjoint([]).
 
 
 
%   disjoint(+Set1, +Set2)
%   is true when the two given sets have no elements in common.
%   It is the opposite of intersect/2.
 
disjoint(Set1, Set2) :-
	member(Element, Set1),
	memberchk(Element, Set2),
	!, fail.
disjoint(_, _).
 
 
 
%   select(?Element, ?Set, ?Residue)
%   is true when Set is a list, Element occurs in Set, and Residue is
%   everything in Set except Element (things stay in the same order).
 
select(Element, [Element|Rest], Rest).
select(Element, [Head|Tail], [Head|Rest]) :-
	select(Element, Tail, Rest).
 
 
 
%   pairfrom(?Set, ?Element1, ?Element2, ?Residue)
%   is true when Set is a list, Element1 occurs in list, Element2
%   occurs in list after Element1, and Residue is everything in Set
%   bar the two Elements.  The point of this thing is to select
%   pairs of elements from a set without selecting the same pair
%   twice in different orders.
 
pairfrom([Element1|Set], Element1, Element2, Residue) :-
	select(Element2, Set, Residue).
pairfrom([Head|Tail], Element1, Element2, [Head|Rest]) :-
	pairfrom(Tail, Element1, Element2, Rest).
 
 
 
%   intersect(Set1, Set2)
%   is true when the two sets have a member in common.  It assumes
%   that both sets are known, and that you don't care which element
%   it is that they share.
 
intersect(Set1, Set2) :-
	member(Element, Set1),		%  generates Elements from Set1
	memberchk(Element, Set2),	%  tests them against Set2
	!.				%  if it succeeds once, is enough.
 
 
 
%   subset(+Set1, +Set2)
%   is true when each member of Set1 occurs in Set2.
%   It can only be used to test two given sets; it cannot be used
%   to generate subsets.  At the moment there is NO predicate for
%   generating subsets, but select/3 takes you part-way.
 
subset([], _).
subset([Element|Residue], Set) :-
	memberchk(Element, Set), !,
	subset(Residue, Set).
 
 
 
%   seteq(+Set1, +Set2)
%   is true when each Set is a subset of the other.  There are two
%   ways of doing this.  One is commented out.
 
seteq(Set1, Set2) :-
	subset(Set1, Set2),
	subset(Set2, Set1).
%	sort(Set1, Ord1),
%	sort(Set2, Ord2),
%	Ord1 == Ord2.
 
 
 
%   listtoset(+List, ?Set)
%   is true when List and Set are lists, and Set has the same elements
%   as List in the same order, except that it contains no duplicates.
%   The two are thus equal considered as sets.  If you really want to
%   convert a list to a set, list_to_ord_set is faster, but this way
%   preserves as much of the original ordering as possible.
 
listtoset([], []).
listtoset([Head|Tail], Set) :-
	memberchk(Head, Tail), !,
	listtoset(Tail, Set).
listtoset([Head|Tail], [Head|Set]) :-
	listtoset(Tail, Set).
 
 
 
%   intersect(+Set1, +Set2, ?Intersection)
%   is true when Intersection is the intersection of Set1 and Set2,
%   *taken in a particular order*.  In fact it is precisely the
%   elements of Set1 taken in that order, with elements not in Set2
%   deleted.  If Set1 contains duplicates, so may Intersection..
 
intersect([], _, []).
intersect([Element|Residue], Set, [Element|Intersection]) :-
	memberchk(Element, Set), !,
	intersect(Residue, Set, Intersection).
intersect([_|Rest], Set, Intersection) :-
	intersect(Rest, Set, Intersection).
 
 
 
%   subtract(+Set1, +Set2, ?Difference)
%   is like intersect, but this time it is the elements of Set1 which
%   *are* in Set2 that are deleted.
 
subtract([], _, []).
subtract([Element|Residue], Set, Difference) :-
	memberchk(Element, Set), !,
	subtract(Residue, Set, Difference).
subtract([Element|Residue], Set, [Element|Difference]) :-
	subtract(Residue, Set, Difference).
 
 
 
%   symdiff(+Set1, +Set2, ?Diff)
%   is true when Diff is the symmetric difference of Set1 and Set2,
%   that is, if each element of Union occurs in one of Set1 and Set2,
%   but not both.  The construction method is such that the answer
%   will contain no duplicates even if the Sets do.
 
symdiff(Set1, Set2, Diff) :-
	symdiff(Set1, Set2, Diff, Mid),
	symdiff(Set2, Set1, Mid, []).
 
symdiff([Elem|Rest], Avoid, Diff, Tail) :-
	memberchk(Elem, Avoid), !,
	symdiff(Rest, Avoid, Diff, Tail).
symdiff([Elem|Rest], Avoid, [Elem|Diff], Tail) :- !,
	symdiff(Rest, [Elem|Avoid], Diff, Tail).
symdiff([], _, Tail, Tail).
 
 
 
%   union(+Set1, +Set2, ?Union)
%   is true when subtract(Set1,Set2,Diff) and append(Diff,Set2,Union),
%   that is, when Union is the elements of Set1 that do not occur in
%   Set2, followed by all the elements of Set2.
 
union([], Set2, Set2).
union([Element|Residue], Set, Union) :-
	memberchk(Element, Set), !,
	union(Residue, Set, Union).
union([Element|Residue], Set, [Element|Union]) :-
	union(Residue, Set, Union).
 
