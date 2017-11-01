/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (symbol_places.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(bq, []).
:- set_module(class(library)).
:- include('header.pro').
               

macro_expand([],[]):-!.
macro_expand([#, '''', X|Xs], [[function, MX]|MXs
 ]):-
	!,             
	macro_expand(X, MX),
	macro_expand(Xs, MXs).
macro_expand(['''', X|Xs], [[quote, MX]|MXs]):-
	!,
	macro_expand(X, MX),
	macro_expand(Xs, MXs).
macro_expand([X|Xs], [MX|MXs]):-
	!,
	macro_expand(X, MX),
	macro_expand(Xs, MXs).
macro_expand(X, X):-
	atomic(X),
	!.        

expand_commas(_,One,Out):- \+ compound(One),!,One=Out.
expand_commas(Bindings,['$COMMA',One],Out):- !, symbol_value(One,Bindings,Out).
expand_commas(Bindings,'$COMMA'(One),Out):- !, symbol_value(One,Bindings,Out).
expand_commas(Bindings,['$BQ',One],Out):- !, expand_commas(Bindings,One,Mid), (One==Mid ->  Out=['$BQ',Mid] ; Out=Mid),!.
expand_commas(Bindings,One,Out):- is_list(One),!,maplist(expand_commas(Bindings),One,Out).
expand_commas(Bindings,One,Out):-
  compound_name_arguments(One,F,Args),
  maplist(expand_commas(Bindings),Args,ArgsOut),
  Out=..[F|ArgsOut],!.

:- fixup_exports.

