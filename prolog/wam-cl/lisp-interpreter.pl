/*******************************************************************
 *
 * A Lisp interpreter, written in Prolog
 *
 * (lisp_interpreter.pl)
 *
 * (c) Neil Smith, 2001
 *
 * This program is a small interpreter for Lisp.  It was written
 * in LPA Prolog v3.6, running under Windows.  It should be fairly
 * easy to convert it to other Prologs.
 *
 * It supports real Lisp syntax, excessive brackets and all.  You don't 
 * need to terminate input with a full stop.  It also understands 
 * 'x for (quote x) and #'func for (function func)
 *
 * Variables are lexically scoped, except where defined as special.
 *
 * read_words code from "The Craft of Prolog", R.A.O'Keefe
 * lisp evaluator from "Lisp" (3rd ed), Winston & Horn.
 *
 *******************************************************************/

:- ensure_loaded(lpa_to_swi).

:-style_check.

:- dynamic 
	bindings/1,
	lambda/2.

:- initialization(lisp,main).


prompts(Old1,_Old2):- var(Old1) -> prompt(Old1,Old1) ; prompt(_,Old1).
bindings([]).

lisp:-
	write('Welcome to Pro-Lisp!'),nl,
	write('This is a miniscule Lisp interpreter, written in Prolog'),nl,
	prompt(Old, '> '),
	prompts(Old1, Old2),
	prompts('> ', '> '),
	tidy_database,
	repeat,
	read_eval_print(Result),
	Result = quit,
	prompt(_, Old),
	prompts(Old1, Old2).


tidy_database:-
	retract(bindings(_)),
	assert(bindings([])),
	retractall(lambda(_, _)).


read_eval_print(Result):-		% dodgy use of cuts to force a single evaluation
	read_and_parse(Expression),
	eval(Expression, Result),
	writeExpression(Result),
	!.



% basic EVAL statements for built-in procedures

eval(Expression, Result):-
	bindings(Bindings),
	eval(Expression, Bindings, Result).


macro_expand([],[]):-!.
macro_expand([#, '''', X|Xs], [[function, MX]|MXs]):-
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


eval(quit, _, quit):-!.
eval(nil, _, []):-!.
eval(t, _, t):-!.
eval([], _, []):-!.
eval([quote, X], _, X):-!.
eval([quit], _, quit):-!.
eval([defvar, Name], _, Name):-
	!,
	retract(bindings(GlobalBindings)),
	assert(bindings([binding(Name, [])|GlobalBindings])),
	!.
eval([setq, Name, Value], Bindings, EvalValue):-
	!,
	bindings(GlobalBindings),
	append(Pre, [binding(Name, _)|Post], GlobalBindings),
	eval(Value, Bindings, EvalValue),
	retract(bindings(GlobalBindings)),
	append(Pre, [binding(Name, EvalValue)|Post], GlobalBindings1),
	assert(bindings(GlobalBindings1)),
	!.
eval([defun, Name, FormalParms, Body], _, Name):-
	!,
	assert(lambda(Name, [lambda, FormalParms, Body])),
	!.
eval([lpa_apply|Arguments], Bindings, Result):-
	!,
	evalL(Arguments, Bindings, [Function, ActualParams]),
	lpa_apply(Function, ActualParams, Result),
	!.
eval([function, [lambda,  FormalParams, Body]], Bindings, 
		[closure, FormalParams, Body, Bindings]):-!.
eval([Procedure|Arguments], Bindings, Result):-
	evalL(Arguments, Bindings, EvalArguments),
	lpa_apply(Procedure, EvalArguments, Result),
	!.
eval(X, _, X):-
	number(X),	
	!.
eval(X, Bindings, Val):-
	atom(X),
		member(binding(X, Val), Bindings)
	;	(bindings(GlobalBindings),
		 member(binding(X, Val), GlobalBindings)),
	!.
eval(X, _, []):-
	write('ERROR!  Cannot find a binding for `'),
	write(X),
	write('`'),nl,
	!.


evalL([], _, []):-!.
evalL([H|T], Bindings, [EvalH|EvalT]):-
	eval(H, Bindings, EvalH),
	evalL(T, Bindings, EvalT),
	!.


lpa_apply(car, [[Result|_]], Result):-!.
lpa_apply(cdr, [[_|Result]], Result):-!.
lpa_apply(list, Args, Args):-!.
lpa_apply(cons, [Arg1, Arg2], [Arg1|Arg2]):-!.
lpa_apply(eq, [Arg1, Arg2], Result):-
	(Arg1 = Arg2 -> Result = Arg1 
		      ; Result = []),
	!.
lpa_apply(if, [Test, Success, Failure], Result):-
	eval(Test, TestResult),
	eval(Success, EvalSuccess),
	eval(Failure, EvalFailure),
	(TestResult = [] -> Result = EvalFailure
			  ; Result = EvalSuccess),
	!.
lpa_apply([lambda, FormalParams, Body], ActualParams, Result):-
	!,
	bind_variables(FormalParams, ActualParams, Bindings),
	eval(Body, Bindings, Result),
	!.
lpa_apply([closure, FormalParams, Body, Bindings0], ActualParams, Result):-
	!,
	bind_variables(FormalParams, ActualParams, Bindings0, Bindings),
	eval(Body, Bindings, Result),
	!.
lpa_apply(ProcedureName, Args, Result):-
	lambda(ProcedureName, LambdaExpression),
	lpa_apply(LambdaExpression, Args, Result),
	!.
lpa_apply(X, _, []):-
	write('ERROR!  Cannot find a procedure description for `'),
	write(X),
	write('`'),nl,
	!.
	


bind_variables(Formal, Actual, Bindings):-
	bind_variables(Formal, Actual, [], Bindings).

bind_variables([], [], Bindings, Bindings).
bind_variables([FormalParam|FormalParams], [ActualParam|ActualParams],
		Bindings0, Bindings):- 
	bind_variables(FormalParams, ActualParams, 
		[binding(FormalParam, ActualParam)|Bindings0], Bindings).




% read and parse a line of Lisp

read_and_parse(Expression):-
	read_words(TokenL),
	(	sexpr(Expression, TokenL, [])
	;
		( write('ERROR!  Could not parse `'),
		  writeTokenL(TokenL),
		  write('`'),nl,
		  Expression = [] )
	),
	!.



% read a line of supposed Lisp code

read_words(Words):-
	get0(C),
	read_words(C, Words).

read_words(C, []):-
	ends_line(C),	
	!.
read_words(C, Words):-
	whitespace(C),
	!,
	read_words(Words).
read_words(C, [Word|Words]):-
	punctuation(C),
	!,
	name(Word, [C]),
	read_words(Words).
read_words(C, [Word|Words]):-
	other(C),
	!,
	read_rest_of_word(Chars, LeftOver),
	name(UCWord, [C|Chars]),
	( atom(UCWord) -> lwrupr(Word, UCWord)
		;	
		Word = UCWord),
	read_words(LeftOver, Words).


read_rest_of_word(Chars, LeftOver):-
	get0(C),
	read_rest_of_word(C, Chars, LeftOver).


read_rest_of_word(C, [], C):-
	\+ other(C),
	!.
read_rest_of_word(C, [C|Chars], LeftOver):-
	other(C),
	!,
	read_rest_of_word(Chars, LeftOver).



ends_line(10).
ends_line(13).


whitespace(9).
whitespace(32).


punctuation(0'.).
punctuation(0'!).
punctuation(0'").
punctuation(0',).
punctuation(0'').
punctuation(0':).
punctuation(0';).
punctuation(0'?).
punctuation(0'().
punctuation(0')).
punctuation(0'[).
punctuation(0']).
% punctuation(0'#).


other(Char):-
	integer(Char),
	Char >= 0,
	Char =< 127,
	\+ ends_line(Char),
	\+ whitespace(Char),
	\+ punctuation(Char).


% Grammar rules for parsing Lisp s-expressions.
% Given a list of tokens, lisplist does all the nesting of lists


sexpr([function, Expression]) --> [#, ''''], !, sexpr(Expression).
sexpr([quote, Expression]) --> [''''], !, sexpr(Expression).
sexpr(Xs) --> ['('], lisplist(Xs), !.
sexpr(X) --> [X], {atomic(X), X \= '.'}, !.


lisplist([]) --> [')'], !.
lisplist([X|Xs]) --> sexpr(X), lisplist(Xs), !.
lisplist([X|Y]) --> sexpr(X), ['.'], sexpr(Y), [')'], !.



% writeExpression/1 displays a lisp expression

writeExpression(quit):-
	!,
	write('Terminating Pro-Lisp'),nl.
writeExpression(Expression):-
	sexpr(Expression, TokenL, []),
%	write('  '),
	writeTokenL(TokenL),
	nl.


writeTokenL([]).
writeTokenL(['(', ')'|TokenL]):-
	!,
	write('NIL '),
	writeTokenL(TokenL).
writeTokenL([Token|TokenL]):-
	atom(Token),
	!,
	lwrupr(Token, UCToken),
	write(UCToken),
	write(' '),
	writeTokenL(TokenL).
writeTokenL([Token|TokenL]):-
	number(Token),
	!,
	write(Token),
	write(' '),
	writeTokenL(TokenL).

:- writeln('
| ?- lisp.
Welcome to Pro-Lisp!
This is a miniscule Lisp interpreter, written in Prolog
> (cons 1 nil)
( 1 ) 
> (defun my_second (lst) (car (cdr lst)))
MY_SECOND 
> (my_second \'(a b c))
B 
> quit
Terminating Pro-Lisp
yes
'
).

