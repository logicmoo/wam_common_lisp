
%neg.pl : A negation metainterpreter using anti-subsumption constraints.
%
%This file contains a metainterpreter for negation that runs
%in Quintus Prolog.
%
%The treatment of negation is a generalization of the standard
%negation as failure.
%
%Consider a (positive) goal p(X).  The standard resolution proof of
%p(X) from a set of program axioms A yields a sequence of answer 
%substitutions T1, .., Tn, such that A |= p(T) for any T that is
%subsumed by some Ti in T1, .., Tn.
%
%Assuming the completion, A |= ~p(T) iff for all answer substitutions
%Ti from a resolution proof of p(X), T is not subsumed by any such Ti.
%The metainterpreter below passes around a list of such anti-subsumption
%constraints, and continually checks them to see if they are violated.
%In a system with more advanced control facilities, these checks can
%be efficiently implemented (you can *almost* do it with freeze and diff;
%you need diff/3, where diff(X,Y,Goal) calls Goal when X is unified with Y).
%
%
%Here are some samples.
%
%| ?- m1(~X=a, Cs).        % X is not a.  Cs is a list of antisubsumption
%                        % constraints.
%X = _63,
%Cs = [[X]/>[a]] ;        % X must not be subsumed by a.
%
%no
%| ?- m1(~X=Y, Cs).      %  This is diff(X,Y)
%
%X = _62,
%Y = _79,
%Cs = [[X,Y]/>[_465,_465]] ;   % The pair X,Y must not be subsumed by U,U
%
%no
%| ?- m1(~member(X,[a,b,c]), Cs).  % X is not a member of [a,b,c]
%
%X = _68,
%Cs = [[X]/>[a],[X]/>[b],[X]/>[c]] ;  % X must not be subsumed by a, b, or c.
%
%no
%| ?- m1(~member(X,[a,Y]), Cs).  % X is not a member of [a,Y]
%
%X = _68,
%Y = _91,
%Cs = [[X,Y]/>[a,_512],[X,Y]/>[_484,_484]] ;  % X must not be subsumed by a,
%                                             % and X,Y must not be subsumed by U,U
%no
%
% This next goal uses double negation and existential quantification.
% Literally it reads: L is a list, and it is not the case that there
% exists an X that is a member of L and not equal to a, i.e. every
% member of L is a.
%
%| ?- m1((list(L),~X^(member(X,L),~X=a)), Cs).
%
%L = Cs = [],
%X = _87 ;
%
%L = [a],
%X = _87,
%Cs = [] ;
%
%L = [a,a],
%X = _87,
%Cs = [] ;
%
%L = [a,a,a],
%X = _87,
%Cs = [] ....
:- op(950, xfx, <-).            % Program clause constructor
:- op(950, fx, ~).              % Negation operator
:- op(700, xfx, />).            % Anti-subsumption constraint
:- op(800, xfx, &).

:- ensure_loaded(library(basics)).
:- ensure_loaded(library(freevars)).
:- ensure_loaded(library(subsumes)).
:- ensure_loaded(library(findall)).

% Starts the metainterpreter.  G is the goal, and Cs is
% a list of anti-subsumption constraints from negations
% called in the proof.
m1(G, Cs) :-
        meta1([G], [], Cs).

% meta1(Goals, ConstraintsIn, ConstraintsOut)
% Goals is a list of goals to prove.
% ConstraintsIn-ConstraintsOut is a difference
% list of anti-subsumption constraints.
meta1([], Cs, Cs).
meta1([G|G1s], C0s, C2s) :-
        G <- G0s,
        append(G0s, G1s, G01s),
        check_negs(C0s, C1s),
        meta1(G01s, C1s, C2s).
meta1([~G|Gs], C0s, C3s) :-
        free_variables(G, [], [], FVs),
        findall(FVs&Cs, m1(G,Cs), NewCs),
        negate(NewCs, FVs, NegCs),
        append(NegCs, C0s, C1s),
        check_negs(C1s, C2s),
        meta1(Gs, C2s, C3s).

% check_negs(CIns, COuts)
% Checks anti-subsumption constraints.
check_negs([], []).
check_negs([T0/>T1|C0s], C1s) :-
        \+ T0=T1,                 % T1 can never subsume T0
        !,                        % so delete this constraint
        check_negs(C0s, C1s).
check_negs([T0/>T1|_], _) :-
        subsumes(T1, T0),         % T1 subsumes T0, so fail
        !,
        fail.
check_negs([C|C0s], [C|C1s]) :-
        check_negs(C0s, C1s).

% negate(ToNegate, FVs, Cs)
% Converts the output of the findall of a negated goal
% in meta1 into anti-subsumption constraints.
negate([], _, []).
negate([Vals&_|NewCs], FVs, [FVs/>Vals|Cs]) :-
        negate(NewCs, FVs, Cs).
negate([FVs&Negs|NewCs], FVs, Cs) :-
        member(FV1s/>FV1s, Negs),
        negate(NewCs, FVs, Cs).


/*********************************************************************
 *  t.pl  Example programs for negation metainterpreter m1.
 */

:- op(700, xfx, /=).

% The next two clauses should really be part of the interpreter.

_^G <- [                % skip existential variables in goal.
        G ].

(P1,P2) <- [            % handle conjunction.
        P1,
        P2 ].

X=X <- [].              % equality

% Inequality, i.e. the diff of Prolog II and Sicstus
X/=Y <- [ 
        ~X=Y ].

% member(X, Xs) iff X appears in list Xs.
member(X, [X|_]) <- [].
member(X, [_|Xs]) <- [
        member(X, Xs) ].

% append(L1, L2, L12) iff L12 is the append of L1 and L2
append([], Ys, Ys) <- [].
append([X|Xs], Ys, [X|XYs]) <- [
        append(Xs, Ys, XYs) ].

% list(L) iff L is a proper list
list([]) <- [].
list([_|Xs]) <- [
        list(Xs) ].

% list_of(E, Es) iff Es is a proper list, every
% member of which is E.
%| ?- m1(list_of(a, L), Cs).
%
%L = Cs = [] ;
%
%L = [a],
%Cs = [] ;
%
%L = [a,a],
%Cs = [] ;
%
%L = [a,a,a],
%Cs = [] ; ...
list_of(E, Es) <- [
        list(Es),
        ~ X^(member(X,Es),E/=X) ].

% every(X,P,Q) iff forall X, P(X) implies Q(X)
% which is equivalent to ~ exists X, P(X) and ~ Q(X)
every(X,P,Q) <- [
        ~ X^(P,~Q) ].

% duplicated(E,L) iff E appears at least twice in L.
duplicated(E, L) <- [
        append(_, [E|Es], L),
        member(E, Es) ].

% unique_list(L) iff no element appears twice in L,
% i.e. there does not exist an E in L duplicated.
%| ?- m1(unique_list(L), Cs).
%
%L = Cs = [] ;
%
%L = [_412],
%Cs = [] ;
%
%L = [_412,_545],
%Cs = [[_412,_545]/>[_940,_940]] ; ....
unique_list(L) <- [
        list(L),
        ~ E^duplicated(E, L) ].

% all_duplicates(L) iff every element in L appears at
% least once.
%| ?- m1(all_duplicates([a,b,U,V]), Cs).
%
%U = a,
%V = b,
%Cs = [] ;
%
%U = b,
%V = a,
%Cs = [] ;
%
%no
all_duplicates(L) <- [
        list(L),
        every(E, member(E,L), duplicated(E,L)) ].


