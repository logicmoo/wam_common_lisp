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
sexpr1([X|Y]) --> !, ['('],  sexpr1(X), lisplist(Y,')').
sexpr1(X) --> {compound(X),compound_name_arguments(X,F,ARGS)}, ['#<'],[F],lisplist(ARGS,'>').
sexpr1(X) --> [X].

lisplist([],EQ) --> [EQ], !.
lisplist([X|Xs],EQ) --> sexpr1(X), !, lisplist(Xs,EQ).
lisplist(X,EQ) --> ['.'], sexpr1(X), [EQ].


cl_format(Stream,Fmt,Args):-wdmsg(cl_format(Stream,Fmt,Args)).
cl_format(Stream,Fmt,Arg1,Arg2):-wdmsg(cl_format(Stream,Fmt,Arg1,Arg2)).

cl_print(X,X):-writeExpression(X).


% writeExpression/1 displays a lisp expression

writeExpression(Var):- is_ftVar(Var),!,writeln(Var).
writeExpression([]):- writeln('NIL').
% writeExpression(quit):- !, write('Terminating WAM-CL'),nl.
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
	atom(Token),!,
	write_symbol_name(Token),
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


write_symbol_name(UCToken):- 
  writing_package(WP),
  write_symbol_name(UCToken,WP),!.
write_symbol_name(S):-write(S).
 
write_symbol_name(Symbol,WP):- symbol_info(Symbol,SP,name,S),symbol_info(Symbol,SP,package,IntExt),must(write_symbol_name(S,WP,SP,IntExt)).

write_symbol_name(S,_WP,keyword,_):- write(':'),write(S).
write_symbol_name(S,WP,SP,_):- SP==WP, !,write(S).
write_symbol_name(S,_P,SP,kw_internal):-!, write(SP),write('::'),write(S).
write_symbol_name(S,WP,SP,kw_external):- package_use_list(WP,SP),!,write(S).
write_symbol_name(S,_P,SP,kw_external):- write(SP),write(':'),write(S).

:- fixup_exports.


