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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(bq, []).
:- set_module(class(library)).
:- include('header').
               

macro_expand([],[]):-!.
macro_expand([#, '''', X|Xs], [[function, MX]|MXs ]
 ):-
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

expand_commas(Env,Result,Forms,Code):- always(expand_backquote(ec_lisp_eval,Env,Result,Forms,Code)),!.

compile_bq(Env,Result,Forms,Code):- always(expand_backquote(lisp_compile,Env,Result,Forms,Code)),!.

bq_call(With,Env,Result,Forms,Code):- always(call(With,Env,Result,Forms,Code)),!.

bq_append(A,B,C):- always(append(A,B,C)),!.



ec_lisp_eval(Env,Result,Form,true):- atom(Form),!,get_var(Env,Form,Result).
%bq_call(Eval,Env,Result,Result,true).
% @TODO fix this workaround 
ec_lisp_eval(Env,['eval_in_env',Result,Env],Result,true).


is_quoted(SelfEval):-SelfEval=[Q,_],atom(Q),fail,nop(is_quote(Q)).
%is_quote(quote).
%is_quote('#BQ').

expand_backquote(_Eval,_,Result,Form,true):- \+ compound(Form),!,Result=Form.
expand_backquote(_Eval,_,SelfEval,SelfEval,true):- notrace(is_self_evaluating_object(SelfEval)),!.
expand_backquote(_Eval,_,SelfEval,SelfEval,true):- is_quoted(SelfEval),!.

expand_backquote(Eval,Env,Result,['#COMMA',Form],Code):- bq_call(Eval,Env,Result,Form,Code).
expand_backquote(Eval,Env,[quote,Result],['#BQ',Form],Code):- expand_backquote(Eval,Env,Result,Form,Code).

expand_backquote(Eval,Env,FormResult,[['#BQ-COMMA-ELIPSE',Form]],Code):- Form\=['#COMMA',_],!,
  bq_call(Eval,Env,FormResult,Form,Code).
expand_backquote(Eval,Env,[R|Result],[F,['#BQ-COMMA-ELIPSE',Form]],Code):- Form\=['#COMMA',_],!,
  expand_backquote(Eval,Env,R,F,Code1),
  bq_call(Eval,Env,Result,Form,Code2),
  Code = (Code1,Code2).

expand_backquote(Eval,Env,Result,[['#BQ-COMMA-ELIPSE',Form]|MORE],Code):- always(MORE\==[]), Form\=['#COMMA',_],!,
  bq_call(Eval,Env,FormResult,Form,Code2),
  expand_backquote(Eval,Env,MOREResult,MORE,Code3),
  Code = (Code2,Code3,bq_append(FormResult,MOREResult,Result)).

expand_backquote(Eval,Env,Result,[F,['#BQ-COMMA-ELIPSE',Form]|MORE],Code):- always(MORE\==[]), Form\=['#COMMA',_],!,
  expand_backquote(Eval,Env,R,F,Code1),
  bq_call(Eval,Env,FormResult,Form,Code2),
  expand_backquote(Eval,Env,MOREResult,MORE,Code3),
  Code = (Code1,Code2,Code3,bq_append([R|FormResult],MOREResult,Result)).

expand_backquote(Eval,Env,[R|Result],[F|['#BQ-COMMA-ELIPSE',Form]],Code):- !, trace_or_throw([F|['#BQ-COMMA-ELIPSE',Form]]),lisp_dump_break, Form\=['#COMMA',_],
  expand_backquote(Eval,Env,R,F,Code1),
  bq_call(Eval,Env,Result,Form,Code2),
  Code = (Code1,Code2).

expand_backquote(Eval,Env,[R|Result],[F|Forms],Code):-
  expand_backquote(Eval,Env,R,F,Code1),
  expand_backquote(Eval,Env,Result,Forms,Code2),
  Code = (Code1,Code2).
expand_backquote(Eval,Env,Result,Forms,Code):-  
  compound_name_arguments(Forms,F,Args),
  expand_backquote(Eval,Env,Args,ArgsOut,Code),
  Result=..[F|ArgsOut].


:- fixup_exports.

