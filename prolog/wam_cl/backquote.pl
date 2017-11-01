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
expand_commas(Env,['$COMMA',One],Out):- !, symbol_value(One,Env,Out).
expand_commas(Env,'$COMMA'(One),Out):- !, symbol_value(One,Env,Out).
expand_commas(Env,['$BQ',One],Out):- !, expand_commas(Env,One,Mid), (One==Mid ->  Out=['$BQ',Mid] ; Out=Mid),!.
expand_commas(Env,One,Out):- is_list(One),!,maplist(expand_commas(Env),One,Out).
expand_commas(Env,One,Out):-
  compound_name_arguments(One,F,Args),
  maplist(expand_commas(Env),Args,ArgsOut),
  Out=..[F|ArgsOut],!.


compile_bq(_,Result,Form,true):- \+ compound(Form),!,Result=Form.
compile_bq(Env,Result,['$COMMA',Form],Code):- lisp_compile(Env,Result,Form,Code).
compile_bq(Env,[quote,Result],['$BQ',Form],Code):- compile_bq(Env,Result,Form,Code).
compile_bq(_,SelfEval,SelfEval,true):- notrace(is_self_evaluationing_object(SelfEval)),!.
compile_bq(Env,[R|Result],[F|Forms],Code):-
  compile_bq(Env,R,F,Code1),
  compile_bq(Env,Result,Forms,Code2),
  Code = (Code1,Code2).
compile_bq(Env,Result,Forms,Code):-  
  compound_name_arguments(Forms,F,Args),
  compile_bq(Env,Args,ArgsOut,Code),
  Result=..[F|ArgsOut].


:- fixup_exports.

