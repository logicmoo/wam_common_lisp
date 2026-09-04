:- module(dlist,[
 ]).

/*
HOMEWORK:

Create a term_expansion/2 that will magically convert naive sort into quick sort

       Which algrytems are better or worse at already sorted lists?

learn to spell 'algorithm'


cases:
% we have 100 elements with exactly 100 array indexes 

*/


cfib(0, 1) :- !.
cfib(1, 1) :- !.
cfib(N, F) :-  
   N1 is N-1, cfib(N1, F1), 
   N2 is N-2, cfib(N2, F2), 
   F is F1 + F2.

% fibonacci.pl
:- dynamic(stored/1).

memo(Goal) :- stored(Goal) -> true; Goal, assertz(stored(Goal)).

mfib(0,1).
mfib(1,1).
mfib(2,1).
mfib(N,F) :-
    N1 is N-1, memo(mfib(N1,F1)), 
    N2 is N-2, memo(mfib(N2,F2)), 
    F is F1 + F2.



mofib(N,F) :-
  (N =< 2) -> 
    F = 1 ; 
    (N1 is N-1, memo(mofib(N1,F1)), 
     N2 is N-2, memo(mofib(N2,F2)), 
     F is F1 + F2).



ofib(N,F) :-
 Self = ofib(N,F),
 repeat, 
  (arg(1,Self,N),
   arg(2,Self,F)),

  (N =< 2) -> 
    F = 1 ; 
    (N1 is N-1, ofib(N1,F1), 
     N2 is N-2, ofib(N2,F2), 
     F is F1 + F2).


    % interactive
% [fibonacci].
% ?- fib(16,X), write('...'), nl.

lmember(El, [H|T]) :-
    lmember_(T, El, H).

lmember_(_, El, El).
lmember_([H|T], El, _) :-
    lmember_(T, El, H).



mk_test(S,lo(S), L):- numlist(1, S, L).
mk_test(S,lr(S), R):- numlist(1, S, L),reverse(L,R).
mk_test(S,lu(S), R):- numlist(1, S, L),random_permutation(L,R).

:- dynamic(stest/2).
:- forall((mk_test(50,X,Y), \+ stest(X,_)),assert(stest(X,Y))).


test(lo1, [1]).
test(lo2, [1,2]).
test(lr2, [2,1]).
test(X,Y):- stest(X,Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  naive sort
% Naive sort is not very efficient algorithm. It generates all permutations and then it tests if the permutation is a sorted list.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



perm([], []).
perm(List, [First|Perm]) :-
   sselect(First, List, Rest),
   perm(Rest, Perm).

sselect(X, [X|Tail], Tail).
  sselect(Elem, [Head|Tail], [Head|Rest]) :-
  sselect(Elem, Tail, Rest).
   

do_while_loop(Test,Goal):-
  repeat,
  once(Goal),
  (Test->fail; (!)).


is_sorted([X,Y|T]):- !, X=<Y, is_sorted([Y|T]).
is_sorted(_).

is_sorted_nd(In):-
  List = value(In),
  repeat,
    (List = value([X,Y|T]) ->
       ((X=<Y->(nb_setarg(1,List,[Y|T]),fail);(!,fail))); !).


:- discontiguous dlist:is_sorter/1.

is_sorter(sort).

% Naive sort uses the generate and test approach to solving problems which is usually utilized in case when everything else failed. However, sort is not such case.
% is_sorter(naive_sort).
naive_sort(List,Sorted):-
  perm(List,Sorted),
  is_sorted(Sorted).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% insert sort
% Insert sort is a traditional sort algorithm. Prolog implementation of insert sort is based on idea of accumulator.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_sorter(insert_sort).
insert_sort(List,Sorted):-i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([H|T],Acc,Sorted):-insert(H,Acc,NAcc),i_sort(T,NAcc,Sorted).
   
insert(X,[Y|T],[Y|NT]):-X>Y,insert(X,T,NT).
insert(X,[Y|T],[X,Y|T]):-X=<Y.
insert(X,[],[X]).



stest:- 
 forall((is_sorter(A),
 SORT =..[A,Y,S]),
  forall((test(X,Y),nl,nl,dmsg(test(A,X)),dmsg(input=Y)),
  once((prolog_statistics:time(SORT),
  dmsg(output=S))))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% bubble sort
% Bubble sort is another traditional sort algorithm which is not very effective. 
% Again, we use accumulator to implement bubble sort.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_sorter(bubble_sort).
bubble_sort(List,Sorted):-b_sort(List,[],Sorted).
b_sort([],Acc,Acc).
b_sort([H|T],Acc,Sorted):-bubble(H,T,NT,Max),b_sort(NT,[Max|Acc],Sorted).
   
bubble(X,[],[],X).
bubble(X,[Y|T],[Y|NT],Max):-X>Y,bubble(X,T,NT,Max).
bubble(X,[Y|T],[X|NT],Max):-X=<Y,bubble(Y,T,NT,Max).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% merge sort
% Merge sort is usually used to sort large files but its idea can be utilized to every list. If properly implemented it could be a very efficient algorithm.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% is_sorter(merge_sort).
merge_sort([],[]):-!.     % empty list is already sorted
merge_sort([X],[X]):-!.   % single element list is already sorted
merge_sort(List,Sorted):-
    List=[_,_|_],
    divide_3(List,L1,L2),     % list with at least two elements is divided into two parts
	merge_sort(L1,Sorted1),merge_sort(L2,Sorted2),  % then each part is sorted
	merge(Sorted1,Sorted2,Sorted),!.                  % and sorted parts are merged

merge([],L,L).
merge(L,[],L):-L\=[].
merge([X|T1],[Y|T2],[X|T]):-X=<Y,merge(T1,[Y|T2],T).
merge([X|T1],[Y|T2],[Y|T]):-X>Y,merge([X|T1],T2,T).

% We can use distribution into even and odd elements of list

is_even(E):- (0 is E /\ 1).

even_odd(L,L1,L2):- partition(is_even, L, L1, L2).


halve([X,Y|L],[X|L1],[Y|L2]):- !, halve(L,L1,L2).
halve(X,[],X).

divide_3(L,L1,L2):-even_odd(L,L1,L2),!.
% or traditional distribution into first and second half (other distibutions are also possible)
divide_3(L,L1,L2):-halve(L,L1,L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  quick sort
% Quick sort is one of the fastest sort algorithms. However, its power is often overvalued. The efficiency of quick sort is sensitive to choice of pivot which is used to distribute list into two "halfs".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_sorter(quick_sort).
quick_sort([],[]).
quick_sort([H|T],Sorted):-
	pivoting(H,T,L1,L2),quick_sort(L1,Sorted1),quick_sort(L2,Sorted2),
	append(Sorted1,[H|Sorted2],Sorted).
   
append([], L, L).
append([H|T], L, [H|R]) :-
    append(T, L, R).

pivoting(_,[],[],[]).
pivoting(H,[X|T],[X|L],G):-X=<H,pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]):-X>H,pivoting(H,T,L,G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Similarly to merge sort, quick sort exploits the divide and conquer method of solving problems.
% The above implementation of quick sort using append is not very effective. We can write better program using accumulator.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_sorter(quick_sort2).
quick_sort2(List,Sorted):-q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):-
	pivoting(H,T,L1,L2),
	q_sort(L1,Acc,Sorted1),
        q_sort(L2,[H|Sorted1],Sorted).





uncons(H,T,[H|T]).
headOf([H|_],H).
tailOf([_|T],T).

% -1.404600 BTC 6,416.27186388 11,092

conj_goal(A,True,A):-True=='true',!.
conj_goal(True,A,A):-True=='true',!.
conj_goal(A,B,(A,B)).

unlistify_clause((P:-B),(NewP:-PBody)):- !,
   unlistify_head(P,NewP,Pre1),
   unlistify_goal(B,Body),
   conj_goal(Pre1,Body,PBody).
unlistify_clause(P,POut):- 
   unlistify_head(P,NewP,Pre1),!,
   (Pre1==true-> 
     POut= P;
     POut =(NewP:-Pre1)).


unlistify_goal(P,P):- \+ compound(P),!.
unlistify_goal((P,B),PBody):-!, 
   unlistify_goal(P,NewP),unlistify_goal(B,Body),
   conj_goal(NewP,Body,PBody).
unlistify_goal(P,PO):-
  unlistify_head(P,NewP,Pre),
  conj_goal(Pre,NewP,PO).

unlistify_head(P,NewP,Pre):- compound(P),!,unlistify_cmp(P,NewP,'true',Pre),!.
unlistify_head(P,P,'true').

unlistify_cmp(P,NewP,In,Out):- 
   P=..[F|ARGS],
   unlistify_args(P,F,ARGS,ARGSO,In,Out),
   NewP=..[F|ARGSO].
   
unlistify_args(_P,_F,[],[],In,In):-!.
unlistify_args(P,F,[E|ARGS],[E|ARGSO],In,Post):- \+ compound(E),!,
  unlistify_args(P,F,ARGS,ARGSO,In,Post).
unlistify_args(P,F,[[H|T]|ARGS],[NewE|ARGSO],In,Post):- 
  conj_goal(uncons(H,T,NewE),In,Pre),!,
  unlistify_args(P,F,ARGS,ARGSO,Pre,Post).
unlistify_args(P,F,[E|ARGS],[NewE|ARGSO],In,Post):-
  unlistify_cmp(E,NewE,In,Pre),
  unlistify_args(P,F,ARGS,ARGSO,Pre,Post).


:- fixup_exports.
