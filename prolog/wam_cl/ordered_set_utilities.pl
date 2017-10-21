%   File   : ORDSET.PL
%   Author : R.A.O'Keefe, Neil Smith
%   Updated: 28 February 2000
%   Purpose: Ordered set manipulation utilities


%   In this module, sets are represented by ordered lists with no
%   duplicates.  Thus {c,r,a,f,t} would be [a,c,f,r,t].  The default ordering
%   is defined by the @< family of term comparison predicates, which
%   is the ordering used by sort/2 and setof/3.
 
%   If required, all of these predicates can be given a comparator predicate
%   for defining sets with custom ordering.  This must have the same interface
%   as the standard compare/3 predicate, ie
%        my_compare(?Rel, +Item1, +Item2) with Rel one of { <, =, > }

%   The benefit of the ordered representation is that the elementary
%   set operations can be done in time proportional to the Sum of the
%   argument sizes rather than their Product.  Some of the unordered
%   set routines, such as member/2, length/2, select/3 can be used
%   unchanged.  The main difficulty with the ordered representation is
%   remembering to use it!
 
:- module(ordered_set_utilities, 
      [ insert_merge/3,         %  Ordset x Item -> Ordset			!!Does NOT preserve sets!!
	insert_merge/4,         %  Ordset x Item x Comparator -> Ordset 	!!Does NOT preserve sets!!
	merge/3,                %  OrdList x OrdList -> OrdList 		!!Does NOT preserve sets!!
	merge/4,                %  OrdList x OrdList x Comparator -> OrdList 	!!Does NOT preserve sets!!
	list_to_ord_set/2,      %  List -> Set
	list_to_ord_set/3,      %  List x Comparator -> Set
	insert/3,               %  Ordset x Item -> Ordset
	insert/4,               %  Ordset x Item x Comparator -> Ordset 
	ord_disjoint/2,         %  Set x Set ->
	ord_disjoint/2,         %  Set x Set x Comparator ->
	ord_intersect_chk/2,    %  Set x Set -> 
	ord_intersect_chk/3,    %  Set x Set x Comparator -> 
	ord_intersect/3,        %  Set x Set -> Set
	ord_intersect/4,        %  Set x Set x Comparator -> Set
	ord_seteq/2,            %  Set x Set ->
	ord_seteq/3,            %  Set x Set x Comparator ->
	ord_subset/2,           %  Set x Set ->
	ord_subset/3,           %  Set x Set x Comparator ->
	ord_subtract/3,         %  Set x Set -> Set
	ord_subtract/4,         %  Set x Set x Comparator -> Set
	ord_symdiff/3,          %  Set x Set -> Set
	ord_symdiff/4,          %  Set x Set x Comparator -> Set
	ord_union/3,            %  Set x Set -> Set
	ord_union/4,            %  Set x Set x Comparator -> Set
	ord_union_and_new/4,    %  Set x Set -> Set x Set
	ord_union_and_new/5     %  Set x Set x Comparator -> Set x Set
      ]).

:- ensure_loaded(library(higher_order)).

/**************
 * :- mode
 * 	list_to_ord_set(+, ?),
 * 	merge(+, +, -),
 * 	ord_disjoint(+, +),
 * 	    ord_disjoint(+, +, +, +, +),
 * 	ord_intersect(+, +),
 * 	    ord_intersect(+, +, +, +, +),
 * 	ord_intersect(+, +, ?),
 * 	    ord_intersect(+, +, +, +, +, ?),
 * 	ord_seteq(+, +),
 * 	ord_subset(+, +),
 * 	    ord_subset(+, +, +, +, +),
 * 	ord_subtract(+, +, ?),
 * 	    ord_subtract(+, +, +, +, +, ?),
 * 	ord_symdiff(+, +, ?),
 * 	    ord_symdiff(+, +, +, +, +, ?),
 * 	ord_union(+, +, ?),
 * 	    ord_union(+, +, +, +, +, ?).
 **************/
 

%   insert_merge(+List, +Item, -Merged)
%   insert_merge(+List, +Item, +Comparator, -Merged)
%   is true when Merged is the stable merge of Item into List.
%   If the two lists are not ordered, the merge doesn't mean a great
%   deal.  Merging is perfectly well defined when the inputs contain
%   duplicates, and all copies of an element are preserved in the
%   output, e.g. insert_merge([1,2,2,3,5,7], 3, [1,2,2,3,3,5,7]). 
%   New items are placed before existing items to which they are equal. 
%   Study this routine carefully, as it is the basis for all the rest.

insert_merge(Set0, Item, Set):-
	insert_merge(Set0, Item, compare, Set).

insert_merge([], Item, _, [Item]).
insert_merge([Element|Set0], Item, Comparator, Set):-
	call(Comparator, Order, Element, Item),
	insert_merge2(Order, Element, Item, Set0, Comparator, Set).

	insert_merge2(<, Element, Item, Set0, Comparator, [Element|Set]):-
		insert_merge(Set0, Item, Comparator, Set).
	insert_merge2(=, Element, Item, Set,  _,          [Item, Element|Set]).
	insert_merge2(>, Element, Item, Set,  _,          [Item, Element|Set]).

 
%   merge(+List1, +List2, -Merged)
%   merge(+List1, +List2, +Comparator, -Merged)
%   is true when Merged is the stable merge of the two given lists.
%   If the two lists are not ordered, the merge doesn't mean a great
%   deal.  Merging is perfectly well defined when the inputs contain
%   duplicates, and all copies of an element are preserved in the
%   output, e.g. merge("122357", "34568", "12233455678").  Study this
%   routine carefully, as it is the basis for all the rest.

merge(List1, List2, Merged):-
	merge(List1, List2, compare, Merged).

merge(List1, List2, Comparator, Merged):-
	foldl(List1, 
		[Merged0, Element, Merged1]^insert_merge(Merged0, Element, Comparator, Merged1), 
		List2, Merged).


%   insert(+Set0, +Item, ?Set)
%   insert(+Set0, +Item, +Comparator, ?Set)
%   is true when Set is the union of Set0 and the singleton set [Item]
%   if Item already exists in Set0, it is not replaced with the new item

insert(Set0, Item, Set):-
	insert(Set0, Item, compare, Set).

insert([], Item, _, [Item]).
insert([Element|Set0], Item, Comparator, Set):-
	call(Comparator, Order, Element, Item),
	insert2(Order, Element, Item, Set0, Comparator, Set).

	insert2(<, Element, Item, Set0, Comparator, [Element|Set]):-
		insert(Set0, Item, Comparator, Set).
	insert2(=, Element, _,    Set,  _,          [Element|Set]).
	insert2(>, Element, Item, Set,  _,          [Item, Element|Set]).


%   list_to_ord_set(+List, ?Set)
%   list_to_ord_set(+List, +Comparator, ?Set)
%   is true when Set is the ordered representation of the set represented
%   by the unordered representation List.  The only reason for giving it
%   a name at all is that you may not have realised that sort/2 could be
%   used this way.
 
list_to_ord_set(List, Set) :-
	sort(List, Set).

% Oh look, an insertion sort!
list_to_ord_set(List, Comparator, Set):-
	foldl(List, [Set0, Element, Set1]^insert(Set0, Element, Comparator, Set1), [], Set).


%   ord_intersect_chk(+Set1, +Set2)
%   ord_intersect_chk(+Set1, +Set2, +Comparator)
%   is true when the two ordered sets have at least one element in common.
%   Note that the test is == rather than = .
 
ord_intersect_chk(Set1, Set2):-
	ord_intersect_chk(Set1, Set2, compare).

ord_intersect_chk([H1|T1], Set2, Comparator):-
	ord_intersect_chk2(Set2, H1, T1, Comparator).

	ord_intersect_chk2([H2|T2], H1, T1, Comparator):-
		call(Comparator, Order, H1, H2),
		ord_intersect_chk3(Order, H1, T1, H2, T2, Comparator).

	ord_intersect_chk3(<, _H1, T1,  H2, T2, Comparator):-
		ord_intersect_chk2(T1,  H2, T2, Comparator).
	ord_intersect_chk3(=,  _,  _,  _,   _, _).
	ord_intersect_chk3(>,  H1, T1, _H2, T2, Comparator):-
		ord_intersect_chk2(T2,  H1, T1, Comparator).



%   ord_intersect(+Set1, +Set2, ?Intersection)
%   ord_intersect(+Set1, +Set2, +Comparator, ?Intersection)
%   is true when Intersection is the ordered representation of Set1
%   and Set2, provided that Set1 and Set2 are ordered sets.
 
ord_intersect(Set1, Set2, Intersection):-
	ord_intersect(Set1, Set2, compare, Intersection).

ord_intersect([], _, _, []).
ord_intersect([H1|T1], Set2, Comparator, Intersection):-
	ord_intersect2(Set2, H1, T1, Comparator, Intersection).

	ord_intersect2([], _, _, _, []).
	ord_intersect2([H2|T2], H1, T1, Comparator, Intersection):-
		call(Comparator, Order, H1, H2),
		ord_intersect3(Order, H1, T1, H2, T2, Comparator, Intersection).

	ord_intersect3(<, _H1, T1, H2, T2, Comparator, Intersection):-
		ord_intersect2(T1, H2, T2, Comparator, Intersection).
	ord_intersect3(=, H,  T1, _H,  T2, Comparator, [H|Intersection]):-
		ord_intersect(T1, T2, Comparator, Intersection).
	ord_intersect3(>, H1, T1, _H2, T2, Comparator, Intersection):-
		ord_intersect2(T2, H1, T1, Comparator, Intersection).


 
%   ord_seteq(+Set1, +Set2)
%   is true when the two arguments represent the same set.  Since they
%   are assumed to be ordered representations, they must be identical.
 
 
ord_seteq(Set1, Set2) :-
	Set1 == Set2.
 


%   ord_disjoint(+Set1, +Set2)
%   ord_disjoint(+Set1, +Set2, +Comparator)
%   is true when the two ordered sets have no element in common.  If the
%   arguments are not ordered, I have no idea what happens.

ord_disjoint(Set1, Set2):-
	ord_disjoint(Set1, Set2, compare).

ord_disjoint([], _, _).
ord_disjoint([H1|T1], Set2, Comparator):-
	ord_disjoint2(Set2, H1, T1, Comparator).

	ord_disjoint2([], _, _, _).
	ord_disjoint2([H2|T2], H1, T1, Comparator):-
		call(Comparator, Order, H1, H2),
		ord_disjoiont3(Order, H1, T1, H2, T2, Comparator).

	ord_disjoint3(<, _H1, T1,  H2, T2, Comparator):-
		ord_disjoint2(T1,  H2, T2, Comparator).
	ord_disjoint3(>,  H1, T1, _H2, T2, Comparator):-
		ord_disjoint2(T2,  H1, T1, Comparator).



%   ord_subset(+Set1, +Set2)
%   ord_subset(+Set1, +Set2, +Comparator)
%   is true when every element of the ordered set Set1 appears in the
%   ordered set Set2.
 
ord_subset(Set1, Set2):-
	ord_subset(Set1, Set2, compare).

ord_subset([], _, _).
ord_subset([H1|T1], Set2, Comparator):-
	ord_subset2(Set2, H1, T1, Comparator).

	ord_subset2([H2|T2], H1, T1, Comparator):-
		call(Comparator, Order, H1, H2),
		ord_subset3(Order, H1, T1, H2, T2, Comparator).

	ord_subset3(=, _H, T1, _H, T2, Comparator):-
		ord_subset(T1, T2, Comparator).
	ord_subset3(>, H1, T1, _H2, T2, Comparator):-
		ord_subset2(T2, H1, T1, Comparator).



%   ord_subtract(+Set1, +Set2, ?Difference)
%   ord_subtract(+Set1, +Set2, +Comparator, ?Difference)
%   is true when Difference contains all and only the elements of Set1
%   which are not also in Set2.
 
ord_subtract(Set1, Set2, Difference):-
	ord_subtract(Set1, Set2, compare, Difference).

ord_subtract([], _, _, []).
ord_subtract([H1|T1], Set2, Comparator, Difference):-
	ord_subtract2(Set2, H1, T1, Comparator, Difference).

	ord_subtract2([], H1, T1, _, [H1|T1]).
	ord_subtract2([H2|T2], H1, T1, Comparator, Difference):-
		call(Comparator, Order, H1, H2),
		ord_subtract3(Order, H1, T1, H2, T2, Comparator, Difference).

	ord_subtract2a([], _, _, _, []).
	ord_subtract2a([H1|T1], H2, T2, Comparator, Difference):-
		call(Comparator, Order, H1, H2),
		ord_subtract3(Order, H1, T1, H2, T2, Comparator, Difference).

	ord_subtract4(<, H1, T1, H2, T2, Comparator, [H1|Difference]):-
		ord_subtract2a(T1, H2, T2, Comparator, Difference).
	ord_subtract4(=, _H, T1, _H, T2, Comparator, Difference):-
		ord_subtract(T1, T2, Comparator, Difference).
	ord_subtract4(>, H1, T1, _H2, T2, Comparator, Difference):-
		ord_subtract2(T2, H1, T1, Comparator, Difference).


%   ord_symdiff(+Set1, +Set2, ?Difference)
%   ord_symdiff(+Set1, +Set2, +Comparator, ?Difference)
%   is true when Difference is the symmetric difference of Set1 and Set2.
%   That is, ord_symdiff(Set1, Set2) = union(Set1, Set2) - intersection(Set1, Set2)
 
ord_symmdiff(Set1, Set2, Difference):-
	ord_symmdiff(Set1, Set2, compare, Difference).

ord_symdiff([], Set2, _, Set2).
ord_symdiff([H1|T1], Set2, Comparator, Difference):-
	ord_symdiff2(Set2, H1, T1, Comparator, Difference).

	ord_symdiff2([], H1, T1, _, [H1|T1]).
	ord_symdiff2([H2|T2], H1, T1, Comparator, Difference):-
		call(Comparator, Order, H1, H2),
		ord_symdiff3(Order, H1, T1, H2, T2, Comparator, Difference).

	ord_symdiff3(<, H1, T1, H2, T2, Comparator, [H1|Difference]):-
		ord_symdiff2(T1, H2, T2, Comparator, Difference).
	ord_symdiff3(=, _H, T1, _H, T2, Comparator, Difference):-
		ord_symdiff(T1, T2, Comparator, Difference).
	ord_symdiff3(>, H1, T1, H2, T2, Comparator, [H2|Difference]):-
		ord_symdiff2(T2, H1, T1, Comparator, Difference).



%   ord_union(+Set1, +Set2, ?Union)
%   ord_union(+Set1, +Set2, +Comparator, ?Union)
%   is true when Union is the union of Set1 and Set2.  Note that when
%   something occurs in both sets, we want to retain only one copy.

ord_union(Set1, Set2, Union):-
	ord_union(Set1, Set2, compare, Union).

ord_union(Set1, Set2, Comparator, Union):-
	foldl(Set1, 
		[SetA, Element, SetB]^insert(SetA, Element, Comparator, SetB), 
		Set2, Union).



%   ord_union_and_new(+Set1, +Set2, ?Union, ?ReallyNew)
%   ord_union_and_new(+Set1, +Set2, +Comparator, ?Union, ?ReallyNew)
%   is true when Union is the union of Set1 and Set2 and ReallyNew
%   are those elements of Set2 that are not in Set1.  Note that when
%   something occurs in both sets, we want to retain only one copy.

ord_union_and_new(Set1, Set2, Union, ReallyNew):-
	ord_union_and_new(Set1, Set2, compare, Union, ReallyNew).

ord_union_and_new([], Set2, _, Set2, Set2).
ord_union_and_new([H1|T1], Set2, Comparator, Union, ReallyNew):-
	ord_union_and_new_2(Set2, H1, T1, Comparator, Union, ReallyNew).

	ord_union_and_new_2([], H1, T1, _, [H1|T1], []).
	ord_union_and_new_2([H2|T2], H1, T1, Comparator, Union, ReallyNew):-
		call(Comparator, Order, H1, H2),
		ord_union_and_new_3(Order, H1, T1, H2, T2, Comparator, Union, ReallyNew).

	ord_union_and_new_2a([], H2, T2, _, [H2|T2], [H2|T2]).
	ord_union_and_new_2a([H1|T1], H2, T2, Comparator, Union, ReallyNew):-
		call(Comparator, Order, H1, H2), 
		ord_union_and_new_3(Order, H1, T1, H2, T2, Comparator, Union, ReallyNew).

	ord_union_and_new_3(<, H1, T1, H2, T2, Comparator, [H1|Union], ReallyNew):-
		ord_union_and_new_2a(T1, H2, T2, Comparator, Union, ReallyNew).
	ord_union_and_new_3(=, H1, T1, _H2, T2, Comparator, [H1|Union], ReallyNew):-
		ord_union_and_new(T1, T2, Comparator, Union, ReallyNew).
	ord_union_and_new_3(>, H1, T1, H2, T2, Comparator, [H2|Union], [H2|ReallyNew]):-
		ord_union_and_new_2(T2, H1, T1, Comparator, Union, ReallyNew).
