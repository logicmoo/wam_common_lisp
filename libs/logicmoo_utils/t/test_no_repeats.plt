
:- include(sanity_tests).

test(no_repeats_3, all(X == [1,2,3])) :-
    no_repeats(X,member(X,[1,2,3,3,3,2,1])).

test(no_repeats_3a, all(X == [3,2,1])) :-
    no_repeats(X,member(X,[3,2,3,3,3,2,1])).


