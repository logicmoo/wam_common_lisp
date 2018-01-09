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

expand_commas(Ctx,Depth,Env,Result,Forms,Code):-
  b_setval('$bq_depth',Depth),
  always(expand_backquote(Ctx,Depth,ec_lisp_eval,Env,Result,Forms,Code)),!,
  b_setval('$bq_depth',Depth).

compile_bq(Ctx,Depth,Env,Result,Forms,Code):- always(expand_backquote(Ctx,Depth,ec_lisp_compile,Env,Result,Forms,Code)),!.

bq_call(Ctx,Depth,With,Env,Result,Forms,Code):- always(call(With,Ctx,Depth,Env,Result,Forms,Code)),!.

bq_append(A,B,C):- always(append(A,B,C)),!.



get_bqd(BQD):- (nb_current('$bq_depth',BQD),number(BQD))->true;(BQD=1;b_setval('$bq_depth',BQD)).

ec_lisp_eval(Ctx,_Depth,Env, Result,Expression,Body):- lisp_compile(Ctx,Env,Result,Expression,Body),!.
ec_lisp_compile(_Ctx,2,_Env, Expression,Expression,true).
ec_lisp_compile(Ctx,_Depth,Env, Result,Expression,Body):- lisp_compile(Ctx,Env,Result,Expression,Body),!.

/*
ec_lisp_eval(Env,Result,Form,true):- atom(Form),!,get_var(Env,Form,Result).
%bq_call(Ctx,Depth,Eval,Env,Result,Result,true).
% @TODO fix this workaround 
ec_lisp_eval(Env, Result,Expression,true):- f_sys_env_eval(Env, Expression, Result).
%ec_lisp_eval(Env,Form,Form,true).
%ec_lisp_eval(Env,['sys_env_eval',Env,Form],Form,true).
*/

is_quoted(SelfEval):- fail,SelfEval=[Q,_],atom(Q),fail,nop(is_quote(Q)).
%is_quote(quote).
%is_quote('#BQ').
non_comma(Form):- Form\=['#COMMA',_].


expand_backquote(_Ctx,_Depth,_Eval,_,Result,Form,true):- \+ compound(Form),!,Result=Form.
expand_backquote(_Ctx,_Depth,_Eval,_,SelfEval,SelfEval,true):- notrace(is_self_evaluating_object(SelfEval)),!.
expand_backquote(_Ctx,_Depth,_Eval,_,SelfEval,SelfEval,true):- is_quoted(SelfEval),!.

expand_backquote(Ctx,0,Eval,Env,Result,Form,Code):-!,bq_call(Ctx,1,Eval,Env,Result,Form,Code).

expand_backquote(Ctx,1,Eval,Env,Result,['#COMMA',Form],Code):- bq_call(Ctx,1,Eval,Env,Result,Form,Code).

expand_backquote(Ctx,Depth,Eval,Env,['#COMMA',Result],['#COMMA',Form],Code):- 
  Depth2 is Depth-1,
  expand_backquote(Ctx,Depth2,Eval,Env,Result,Form,Code).

expand_backquote(Ctx,Depth,Eval,Env,['#BQ',Result],['#BQ',Form],Code):-
   Depth2 is Depth+1,
   expand_backquote(Ctx,Depth2,Eval,Env,Result,Form,Code).

expand_backquote(Ctx,Depth,Eval,Env,FormResult,[['#BQ-COMMA-ELIPSE',Form]],Code):- non_comma(Form),!,
  bq_call(Ctx,Depth,Eval,Env,FormResult,Form,Code).
expand_backquote(Ctx,Depth,Eval,Env,[R|Result],[F,['#BQ-COMMA-ELIPSE',Form]],Code):- non_comma(Form),!,
  expand_backquote(Ctx,Depth,Eval,Env,R,F,Code1),
  bq_call(Ctx,Depth,Eval,Env,Result,Form,Code2),
  Code = (Code1,Code2).

expand_backquote(Ctx,Depth,Eval,Env,Result,[['#BQ-COMMA-ELIPSE',Form]|MORE],Code):- always(MORE\==[]), non_comma(Form),!,
  bq_call(Ctx,Depth,Eval,Env,FormResult,Form,Code2),
  expand_backquote(Ctx,Depth,Eval,Env,MOREResult,MORE,Code3),
  Code = (Code2,Code3,bq_append(FormResult,MOREResult,Result)).

expand_backquote(Ctx,Depth,Eval,Env,Result,[F,['#BQ-COMMA-ELIPSE',Form]|MORE],Code):- always(MORE\==[]), non_comma(Form),!,
  expand_backquote(Ctx,Depth,Eval,Env,R,F,Code1),
  bq_call(Ctx,Depth,Eval,Env,FormResult,Form,Code2),
  expand_backquote(Ctx,Depth,Eval,Env,MOREResult,MORE,Code3),
  Code = (Code1,Code2,Code3,bq_append([R|FormResult],MOREResult,Result)).

expand_backquote(Ctx,Depth,Eval,Env,[R|Result],[F|['#BQ-COMMA-ELIPSE',Form]],Code):- !, trace_or_throw([F|['#BQ-COMMA-ELIPSE',Form]]),lisp_dump_break, non_comma(Form),
  expand_backquote(Ctx,Depth,Eval,Env,R,F,Code1),
  bq_call(Ctx,Depth,Eval,Env,Result,Form,Code2),
  Code = (Code1,Code2).

expand_backquote(Ctx,Depth,Eval,Env,[R|Result],[F|Forms],Code):-
  expand_backquote(Ctx,Depth,Eval,Env,R,F,Code1),
  expand_backquote(Ctx,Depth,Eval,Env,Result,Forms,Code2),
  Code = (Code1,Code2).
expand_backquote(Ctx,Depth,Eval,Env,Result,Forms,Code):-  
  compound_name_arguments(Forms,F,Args),
  expand_backquote(Ctx,Depth,Eval,Env,Args,ArgsOut,Code),
  Result=..[F|ArgsOut].


:- fixup_exports.

