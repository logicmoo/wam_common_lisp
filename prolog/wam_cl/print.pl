/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(print, []).
:- set_module(class(library)).
:- include('header.pro').


% Grammar rules for printing Lisp s-expressions.
% Given a list of tokens, lisplist does all the nesting of lists

sexpr1(X) --> {is_ftVar(X),(get_var_name(X,N)->format(atom(NN),'~w',[N]);format(atom(NN),'~w',[X]))},!,[NN].
sexpr1(Str)--> {string(Str)},!,[Str].
sexpr1([function, Expression]) --> ['#'''], !, sexpr1(Expression).
sexpr1([quote, Expression]) --> [''''], !, sexpr1(Expression).
sexpr1('ugly'(T,X)) --> ['#<'],sexpr1(T),sexpr1(X),['>'].
sexpr1('$COMMA'(X)) --> [','],sexpr1(X).
sexpr1(['$COMMA',X]) --> [','],sexpr1(X).
sexpr1(['$BQ',X])--> ['`'],sexpr1(X).
sexpr1([X|Y]) --> !, ['('],  sexpr1(X), lisplist(Y).
sexpr1(X) --> {compound(X),X=..[F|ARGS]}, '#<',[F],lisplist_sp(ARGS,'>').
sexpr1(X) --> [X].


lisplist([]) --> [')'], !.
lisplist([X|Xs]) --> sexpr1(X), !, lisplist(Xs).
lisplist(X):- ['.'], sexpr1(X), [')'].

lisplist_sp([],EQ) --> [EQ], !.
lisplist_sp([X|Xs],EQ) --> sexpr1(X), !, lisplist_sp(Xs,EQ).
lisplist_sp(X,EQ):- ['.'], sexpr1(X), [EQ].



% writeExpression/1 displays a lisp expression

writeExpression(quit):-
	!,
	write('Terminating WAM-CL'),nl.

writeExpression('$COMMENT0'([])):- 	writeln(';'),!.
writeExpression('$COMMENT'(S)):- 	write(';'),writeln(S),!.
writeExpression('$COMMENT1'(S)):- 	write('#|'),write(S),writeln('|#').
writeExpression('$COMMENT'(S,_,_)):- 	write('#|'),write(S),writeln('|#').
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
	% lwrupr(Token, UCToken),
        =(Token, UCToken),
	write(UCToken),
	write(' '),
	writeTokenL(TokenL).
writeTokenL([UCToken|TokenL]):-
	string(UCToken),
   write(' '),
   writeq(UCToken),	
   write(' '),
	writeTokenL(TokenL).
writeTokenL([Token|TokenL]):-
	number(Token),
	!,
	write(Token),
	write(' '),
	writeTokenL(TokenL).



:- fixup_exports.


