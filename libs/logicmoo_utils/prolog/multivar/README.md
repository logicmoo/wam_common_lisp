Multivars
=================

This library provides Prolog interface to DMiles' expermental C code for SWI-Prolog

% =====================================================
% File: sanity_tests.pl
% =====================================================

:- use_module(library(multivar)).
:- use_module(library(pfc)).

isa(i1,c1).
predicate_function_canonical(isa,instanceOf).

% weaken_goals/2 that converts arguments (from legacy code)
% into metaterms which allow logical constraints to be placed upon unification
% in the case of atoms, they are "weakened" to non ground terms
predicate_hold_aliases(Spec),{mpred_functor(Spec,F,A),functor(P,F,A)} 
  ==> (  P, { weaken_goal(P,Q) } ==> {ignore(call(retract,P))},Q ).
       
predicate_hold_aliases(loves/2).

% the predicate is weakened on read (all args)
loves(sue,joe).
loves(joe,fred).

/*
?- loves(X,joe).
X = _{ '$value'= X, iz = sue}.
*/

% so that one may use "typed unification"
tFemale(sue).
~tFemale(joe).

/*
?-  use_module(library(attvar_reader)).  % allows attvars to be read from files and console

?- loves( X{iza=tFemale},joe).
X = _{ '$value'= X, iz = sue, iza=[tFemale]}.
Yes.

?- loves( sue, Y{iza=tFemale}).
Y = _{ '$value'= X, iz = fred}.
Yes.

% this was Joe was asserted to specifically not to be a tFemale.
% However the gender of Fred is still unknown

*/

:- if(false).

%  @TODO  Move this to a different set of exmaples
% this gets hairy to the instances can belong to several intensional types, extensional collections and datatypes.
:- ensure_loaded(library('logicmoo/pfc/user_transitiveViaArg.pfc')).
% both arguments must have at least some type attributes in common
meta_argtypes(loves(X,X)).  % 


:- endif.

% =====================================================
end_of_file.
% =====================================================



?- p(X,X) = p(i1,instanceOf(c1)).

X = _{ '$value'= X, iz = c1, iza=[c1]}.
Yes

?- use_module(clause_attvars).

?- p(X,X) = p(i1,instanceOf(c1)), asserta(x_was(X)).

?- 




