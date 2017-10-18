%   File   : Higher_Order.PL
%   Author : Neil Smith
%   Updated: 25 January 2000
%   Purpose: Various "function" application routines based on lpa_apply/2.
%   Based on the 'applic.pl' library file from the Edinburgh Prolog libraries

:- module(higher_order, 
      [	% call/2,
       /* call/3,
	call/4,
	call/5,
	call/6,*/
	lpa_apply/1,
	lpa_apply/2,

	identity/2,
	complement/1,
	complement/2,
	every/2,
	some/2,
	somechk/2,
	map/3,
	filter/3,
	mapfilter/3,
	partition/4,
	foldl/4,
	foldl1/3,
	foldr/4,
	foldr1/3	]).


% call(X, A):- 
%	X =.. XL,
%	append(XL, [A], XLA),
%	XA =.. XLA,
%	XA.
:- if( \+ current_predicate(call/3) ).

call(X, A, B):- 
	X =.. XL,
	append(XL, [A, B], XLA),
	XA =.. XLA,
	XA.
call(X, A, B, C):- 
	X =.. XL,
	append(XL, [A, B, C], XLA),
	XA =.. XLA,
	XA.
call(X, A, B, C, D):- 
	X =.. XL,
	append(XL, [A, B, C, D], XLA),
	XA =.. XLA,
	XA.
call(X, A, B, C, D, E):- 
	X =.. XL,
	append(XL, [A, B, C, D, E], XLA),
	XA =.. XLA,
	XA.
:- endif.

%   lpa_apply(Pred, Args)
%   is the key to this whole module.  It is basically a variant of call/1
%   (see the Dec-10 Prolog V3.43 manual) where some of the arguments may
%   be already in the Pred, and the rest are passed in the list of Args.
%   Thus lpa_apply(foo, [X,Y]) is the same as call(foo(X,Y)),
%   and lpa_apply(foo(X), [Y]) is also the same as call(foo(X,Y)).
%   BEWARE: any goal given to lpa_apply is handed off to call/1, which is the
%   Prolog *interpreter*, so if you want to lpa_apply compiled predicates you
%   MUST have :- public declarations for them.  The ability to pass goals
%   around with some of their (initial) arguments already filled in is
%   what makes lpa_apply/2 so useful.  Don't bother compiling anything that
%   uses lpa_apply heavily, the compiler won't be able to help much.  LISP
%   has the same problem.  Later Prolog systems may have a simpler link
%   between compiled and interpreted code, or may fuse compilation and
%   interpretation, so lpa_apply/2 may come back into its own.  At the moment,
%   lpa_apply and the routines based on it are not well thought of.

:- if(\+ current_predicate(lpa_apply/1)).
lpa_apply(Pred):-
	lpa_apply(Pred, []).

lpa_apply(Pred, Args) :-
	(	atom(Pred)
	->	Goal =.. [Pred|Args]
        ;	Pred = complement(Term)
	->	Goal = complement(Term, Args)
	;	Pred = FormalArgs ^ Term
	->	copy_term(FormalArgs ^ Term, Args ^ Goal)
	;	Pred =.. OldList,
		append(OldList, Args, NewList),
		Goal =.. NewList	),
	!,
	call(call,Goal).

:- endif.


%   identity(Item, Item)
%   For those cases where predictes are needed, but values supplied

identity(Item, Item).


%   complement(Pred)
%   succeeds if Pred fails, otherwise it fails

complement(Pred):-
	complement(Pred, []).

complement(Pred, Args):-
	(	lpa_apply(Pred, Args)
	->	fail
	;	true	).


%   every(List, Pred)
%   suceeds when Pred(Elem) succeeds for each Elem in the List.

every([], _Pred):-!.
every([Head|Tail], Pred) :-
	lpa_apply(Pred, [Head]),
	every(Tail, Pred).


%   some(List, Pred)
%   succeeds when Pred(Elem) succeeds for some Elem in List.  It will
%   try all ways of proving Pred for each Elem, and will try each Elem
%   in the List.  somechk/2 is to some/2 as memberchk/2 is to member/2;
%   you are more likely to want somechk with its single solution.

some([Head|_], Pred) :-
	lpa_apply(Pred, [Head]).
some([_|Tail], Pred) :-
	some(Tail, Pred).


somechk([Head|_], Pred) :-
	lpa_apply(Pred, [Head]),
	!.
somechk([_|Tail], Pred) :-
	somechk(Tail, Pred).




%   map(OldList, Pred, NewList)
%   succeeds when Pred(Old,New) succeeds for each corresponding
%   Old in OldList, New in NewList.  

map([],     _,    []).
map([X|Xs], Pred, [Y|Ys]):-
	lpa_apply(Pred, [X, Y]),
	map(Xs, Pred, Ys).


%   filter(List, Pred, SubList)
%   succeeds when SubList is the sub-sequence of the List containing all
%   the Elems of List for which Pred(Elem) succeeds.

filter([],         _Pred, []).
filter([Head|List], Pred, SubList) :-
	(	lpa_apply(Pred, [Head])
	->	SubList = [Head|Rest]
	;	SubList = Rest	),
	filter(List, Pred, Rest).


%   mapfilter(OldList, Rewrite, NewList)
%   is a sort of hybrid of map/3 and filter/3.
%   Each element of NewList is the image under Rewrite of some
%   element of OldList, and order is preserved, but elements of
%   OldList on which Rewrite is undefined (fails) are not represented.
%   Thus if foo(X,Y) :- integer(X), Y is X+1.
%   then 
%            mapfilter([1,a,0,joe(99),101], foo, [2,1,102]).
%
%   (could be rewritten as
%            mapfilter([1,a,0,joe(99),101], [X,Y]^(integer(X), Y is X + 1), [2,1,102]).
%   )

mapfilter([],        _Pred, []).
mapfilter([Old|Olds], Pred, NewList) :-
	(	lpa_apply(Pred, [Old,New])
	->	NewList = [New|News]
	;	NewList = News	),
	mapfilter(Olds, Pred, News).


%   partition(Elements, Predicate, Trues, Falses)
%   Partitions a list according to Predicate.  Each element of Elements 
%   for which Predicate succeeds is in Trues, and in Falses otherwise.
partition([], _Pred, [], []).
partition([Head|List], Pred, Trues, Falses):-
	(	lpa_apply(Pred, [Head])
	->	Trues = [Head|RestTrues],
		Falses = RestFalses
	;	Trues = RestTrues,
		Falses = [Head|RestFalses]	),
	partition(List, Pred, RestTrues, RestFalses).



% foldl (Elements, Pred(Folded0, Element, Folded), Base, Result)
% Folds a list of items, by combining the leftmost items first
% Predicate is true if the Element folds into Folded0 to give Folded.
% The first item in the list is folded into the base item
foldl([], _, Term0, Term0).
foldl([X|Xs], Pred, Term0, Term):-
	lpa_apply(Pred, [Term0, X, Term1]),
	foldl(Xs, Pred, Term1, Term).


% foldl1 (Elements, Pred(Folded0, Element, Folded), Result)
% Folds a non-empty list of items, by combining the leftmost items first
% Predicate is true if the Element folds into Folded0 to give Folded.
% The first item in the list forms the base item
foldl1([X|Xs], Pred, Term):-
	foldl(Xs, Pred, X, Term).


% foldr (Elements, Pred(Element, Folded0, Folded), Base, Result)
% Folds a list of items, by combining the rightmost items first
% Predicate is true if the Element folds into Folded0 to give Folded.
% The empty list folds into the base item.
foldr([], _, Term0, Term0).
foldr([X|Xs], Pred, Term0, Term):-
	foldr(Xs, Pred, Term0, Term1),
	lpa_apply(Pred, [X, Term1, Term]).

% foldr (Elements, Pred(Element, Folded0, Folded), Result)
% Folds a non-empty list of items, by combining the rightmost items first
% Predicate is true if the Element folds into Folded0 to give Folded.
% The last element of the list forms the base item.

foldr1([X|Xs], Pred, Term):-
	(	Xs = []
	->	Term = X
	;	foldr1(Xs, Pred, Term0),
		lpa_apply(Pred, [X, Term0, Term])	).
