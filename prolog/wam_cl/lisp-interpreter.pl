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
:- ensure_loaded(('lisp_compiler')).

:-style_check.

:- dynamic 
	bindings/1,
	named_lambda/2,
        macro_lambda/3.

:- initialization(lisp,main).

:- meta_predicate(timel(:)).
timel(M:X):- prolog_statistics:time(M:X).

str_to_expression(Str, Expression):- parse_sexpr_untyped(string(Str), Expression),!.
str_to_expression(Str, Expression):- with_input_from_string(Str,read_and_parse(Expression)),!.

print_eval_string(Str):-
  str_to_expression(Str, Expression),
   dmsg(print_eval_string(Expression)),
   eval(Expression, Result),
   dmsg(print_Result(Result)),
     writeExpression(Expression),
     !.


eval_string(Str):-
  str_to_expression(Str, Expression),
   timel(eval(Expression, Result)),
        writeExpression(Result),
     !.

trace_eval_string(Str):-
  str_to_expression(Str, Expression),
   redo_call_cleanup(trace,eval(Expression, Result),notrace),
     writeExpression(Result),
     !.

rtrace_eval_string(Str):-
  str_to_expression(Str, Expression),
   rtrace(eval(Expression, Result)),
     writeExpression(Result),
     !.

:- meta_predicate(with_input_from_string(+,:)).
with_input_from_string(Str,Goal):-
 open_string(Str,In),
 with_input_from_stream(In,Goal).


:- meta_predicate(with_input_from_stream(+,:)).
with_input_from_stream(In,Goal):- 
   each_call_cleanup(see(In),Goal,seen).




prompts(Old1,_Old2):- var(Old1) -> prompt(Old1,Old1) ; prompt(_,Old1).
bindings([]).

lisp:-
	write('Welcome to WAM-CL!'),nl,
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

:- use_module(library('dialect/sicstus/arrays')).
is_self_evaluationing_object(X):- var(X),!.
is_self_evaluationing_object(X):- atomic(X),!,(number(X);string(X);blob(X,_);X=t;X=[]),!.
is_self_evaluationing_object(X):- (is_dict(X);is_array(X);is_rbtree(X)),!.


eval(X, Bindings, Val):-
	zotrace((atom(X),
      (member(bv(X, Val), Bindings)
	;	(bindings(GlobalBindings),
		 member(bv(X, Val), GlobalBindings)))
	)),!.
eval(X, _, X):- zotrace(is_self_evaluationing_object(X)),!. 
eval(quit, _, quit):-!.
eval(nil, _, []):-!.
eval([quote, X], _, X):-!.
eval([quit], _, quit):-!.
eval([defvar, Name], _, Name):-
	!,
	retract(bindings(GlobalBindings)),
	assert(bindings([bv(Name, [])|GlobalBindings])),
	!.
eval([setq, Name, Value], Bindings, EvalValue):-
	!,
	bindings(GlobalBindings),
	append(Pre, [bv(Name, _)|Post], GlobalBindings),
	eval(Value, Bindings, EvalValue),
	retract(bindings(GlobalBindings)),
	append(Pre, [bv(Name, EvalValue)|Post], GlobalBindings1),
	assert(bindings(GlobalBindings1)),
	!.
eval([defmacro, Name, FormalParms | Body], _, Name):-
	!,
	assert(macro_lambda(Name, FormalParms, Body)),
	!.
eval([defun, Name, FormalParms, Body], _, Name):-
	!,
	assert(named_lambda(Name, [lambda, FormalParms, Body])),
	!.

eval(['$BQ',One], Bindings, Out):-!, expand_commas(Bindings,One,Out).
eval([('`'),One], Bindings, Out):-!, expand_commas(Bindings,One,Out).

eval([apply|Arguments], Bindings, Result):-
	!,
	evalL(Arguments, Bindings, [Function, ActualParams]),
	apply_f(Bindings,Function, ActualParams, Result),
	!.


eval([function, [lambda,  FormalParams, Body]], Bindings, 
		[closure, FormalParams, Body, Bindings]):-!.

eval([Procedure|Arguments], Bindings, Result):-	lisp_operator(Procedure),!,
  apply_f(Bindings,Procedure, Arguments, Result),
  !.

eval([Procedure|Arguments], Bindings, Result):-
  macro_lambda(Procedure, FormalParams, LambdaExpression),
  bind_variables(FormalParams, Arguments, Bindings, NewBindings),
  eval(LambdaExpression, NewBindings, Result),
  !.

eval([Procedure|Arguments], Bindings, Result):-
	evalL(Arguments, Bindings, EvalArguments),
	apply_f(Bindings,Procedure, EvalArguments, Result),
	!.

eval(X, _, []):-
	write('ERROR!  Cannot find a bv for `'),
	write(X),
	write('`'),nl,
	!.

lisp_operator(if).

expand_commas(_,One,Out):- \+ compound(One),!,One=Out.
expand_commas(Bindings,['$COMMA',One],Out):- !, eval(One,Bindings,Out).
expand_commas(Bindings,['$BQ',One],Out):- !, expand_commas(Bindings,One,Mid), (One==Mid ->  Out=['$BQ',Mid] ; Out=Mid),!.
expand_commas(Bindings,'$COMMA'(One),Out):- !, eval(One,Bindings,Out).
expand_commas(Bindings,One,Out):- is_list(One),!,maplist(expand_commas(Bindings),One,Out).
expand_commas(Bindings,One,Out):-
  compound_name_arguments(One,F,Args),
  maplist(expand_commas(Bindings),Args,ArgsOut),
  Out=..[F|ArgsOut],!.

evalL([], _, []):-!.
evalL([H|T], Bindings, [EvalH|EvalT]):-
	eval(H, Bindings, EvalH),
	evalL(T, Bindings, EvalT),
	!.

pf_car(A,Out):- \+ is_list(A),type_error(list,A,car(A),pf_car(A,Out)).
pf_car([A|_],A).
pf_car(_,[]).

apply_f(_Binds,function, [A],[function,A]).
apply_f(_Binds,car, LIST, Result):-!,(LIST=[[Result|_]]->true;Result=[]).
apply_f(_Binds,cdr, LIST, Result):-!,(LIST=[[_|Result]]->true;Result=[]).
apply_f(_Binds,list, Args, Args):-!.
apply_f(_Binds,cons, [Arg1, Arg2], [Arg1|Arg2]):-!.
apply_f(_Binds,eq, [Arg1, Arg2], Result):-
	(Arg1 = Arg2 -> Result = Arg1 
		      ; Result = []),
	!.
apply_f(Bindings,if_wrong, [Test, Success, Failure], Result):-
	eval(Test,Bindings, TestResult),
	eval(Success,Bindings, EvalSuccess),
	eval(Failure,Bindings, EvalFailure),
	(TestResult = [] -> Result = EvalFailure
			  ; Result = EvalSuccess),
	!.
apply_f(Bindings,if, [Test, Success, Failure], Result):-  !,
	eval(Test, Bindings, TestResult),
	(TestResult \== [] -> eval(Success, Bindings, Result)
			  ; eval(Failure, Bindings, Result)),
	!.
apply_f(Binds,[lambda, FormalParams, Body], ActualParams, Result):-
	!,
	bind_variables(FormalParams, ActualParams,Binds, Bindings),!,
	eval(Body, Bindings, Result),
	!.
apply_f(_Binds,[closure, FormalParams, Body, Bindings0], ActualParams, Result):-
	!,
	bind_variables(FormalParams, ActualParams, Bindings0, Bindings),
	eval(Body, Bindings, Result),
	!.

apply_f(_Binds,ProcedureName, ActualParams, Result):-
	macro_lambda(ProcedureName,FormalParams, LambdaExpression),!,
	bind_variables(FormalParams, ActualParams, Bindings),
        eval(LambdaExpression, Bindings, Result),
	!.
apply_f(Bindings,ProcedureName, Args, Result):-
	named_lambda(ProcedureName, LambdaExpression),!,
	apply_f(Bindings,LambdaExpression, Args, Result),
	!.

apply_f(_,=,[X,Y],R):-!, X \= Y -> R=[] ; R=t.
apply_f(_,-,[X,Y],R):-!, R is X - Y.
apply_f(_,+,[X,Y],R):-!, R is X + Y.
apply_f(_Binds,X, _, _):-
        dumpST,
	write('ERROR!  Cannot apply a procedure description for `'),
	write(X),
	write('`'),nl,
        break,
	!.
apply_f(_Binds,X, _, []):-
	write('ERROR!  Cannot apply a procedure description for `'),
	write(X),
	write('`'),nl,
	!.
	


bind_variables(Formal, Actual, Bindings):-
	bind_variables(Formal, Actual, [], Bindings).

bind_variables([], [], Bindings, Bindings).
bind_variables([FormalParam|FormalParams], [ActualParam|ActualParams],
		Bindings0, Bindings):- 
	bind_variables(FormalParams, ActualParams, 
		[bv(FormalParam, ActualParam)|Bindings0], Bindings).




:- if(exists_source(library(sexpr_reader))).
:- use_module(library(sexpr_reader)).
read_and_parse_i(Expr):- current_input(In), parse_sexpr_untyped(In, Expr).
:- endif.

% read and parse a line of Lisp
read_and_parse1(Expression):-
	read_words(TokenL),
	(	sexpr1(Expression, TokenL, [])
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
ends_line(0).
ends_line(-1).


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


sexpr1([function, Expression]) --> [#, ''''], !, sexpr1(Expression).
sexpr1([quote, Expression]) --> [''''], !, sexpr1(Expression).
sexpr1(X) --> {is_ftVar(X),get_var_name(X,N)},[',',N].
sexpr1(['$BQ',X])--> ['`'],sexpr1(X).
sexpr1('$COMMA'(X)) --> [','],sexpr1(X).
sexpr1(Xs) --> ['('], lisplist(Xs), !.
sexpr1(X) --> [X], {atomic(X), X \= '.'}, !.


lisplist([]) --> [')'], !.
lisplist([X|Xs]) --> sexpr1(X), lisplist(Xs), !.
lisplist([X|Y]) --> sexpr1(X), ['.'], sexpr1(Y), [')'], !.



% writeExpression/1 displays a lisp expression

writeExpression(quit):-
	!,
	write('Terminating Pro-Lisp'),nl.
writeExpression(Expression):-
	sexpr1(Expression, TokenL, []),
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


run666(S):-'format'('~n~s~n',[S]),run666(S,V),writeq(V).

parsing(Program, Forms0):- sformat(S,'(\n~s\n)\n',[Program]),str_to_expression(S,Forms0).
run666(Program, Values) :-
    quietly(parsing(Program, Forms)),
    maplist(see_and_do(eval),Forms,Values),
    last(Values,Last),
    writeExpression(Last).

see_and_do(Pred2, I,O):-
  dmsg(seeingFormala(I)),
  call(Pred2,I,O),
  dmsg(result(O)).



% if_script_file_time666(_):-!.
if_script_file_time666(X):- prolog_statistics:time(user:X).

% Append:
test(0) :- if_script_file_time666(run666("
        (defun append (x y)
          (if x
              (cons (car x) (append (cdr x) y))
            y))

        (append '(a b) '(3 4 5))")).

    %@ V = [append, [a, b, 3, 4, 5]].
    

% Fibonacci, naive version:
test(1) :- if_script_file_time666(run666("
        (defun fib (n)
          (if (= 0 n)
              0
            (if (= 1 n)
                1
              (+ (fib (- n 1)) (fib (- n 2))))))
        (fib 24)")).

    %@ % 14,255,802 inferences, 3.71 CPU in 3.87 seconds (96% CPU, 3842534 Lips)
    %@ V = [fib, 46368].
    

% Fibonacci, accumulating version:
test(2) :- if_script_file_time666(run666("
        (defun fib (n)
          (if (= 0 n) 0 (fib1 0 1 1 n)))

        (defun fib1 (f1 f2 i to)
          (if (= i to)
              f2
            (fib1 f2 (+ f1 f2) (+ i 1) to)))

        (fib 250)")).

    %@ % 39,882 inferences, 0.010 CPU in 0.013 seconds (80% CPU, 3988200 Lips)
    %@ V = [fib, fib1, 7896325826131730509282738943634332893686268675876375].
    

% Fibonacci, iterative version:
test(3):- if_script_file_time666(run666("
        (defun fib (n)
          (setq f (cons 0 1))
          (setq i 0)
          (while (< i n)
            (setq f (cons (cdr f) (+ (car f) (cdr f))))
            (setq i (+ i 1)))
          (car f))

        (fib 350)")).

    %@ % 34,233 inferences, 0.010 CPU in 0.010 seconds (98% CPU, 3423300 Lips)
    %@ V = [fib, 6254449428820551641549772190170184190608177514674331726439961915653414425].
    

% Higher-order programming and eval:
test(4):- if_script_file_time666(run666("
        (defun map (f xs)
          (if xs
              (cons (eval (list f (car xs))) (map f (cdr xs)))
            ()))

        (defun plus1 (x) (+ 1 x))

        (map 'plus1 '(1 2 3))
        "
        )).

    %@ V = [map, plus1, [2, 3, 4]].
 

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
> (defun fib (n) (if (> n 1) (+ (fib (- n 1)) (fib (- n 2)))1))

> quit
Terminating Pro-Lisp
yes
'
).

:- eval_string("(defun append (x y) (if x (cons (car x) (append (cdr x) y))  y))").

