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

trim_full_stop(SPClosure,TSPClosure):-atom_concat(SPClosureN,'\n',SPClosure),!,trim_full_stop(SPClosureN,TSPClosure).
trim_full_stop(SPClosure,TSPClosure):-atom_concat(SPClosureN,' ',SPClosure),!,trim_full_stop(SPClosureN,TSPClosure).
trim_full_stop(SPClosure,SPClosureN):-atom_concat(SPClosureN,'.',SPClosure).
trim_full_stop(SPClosure,SPClosure).

sexpr1(X) --> {is_ftVar(X),(get_var_name(X,N)->format(atom(NN),'~w',[N]);format(atom(NN),'~w',[X]))},!,[NN].
sexpr1(Str)--> {string(Str)},!,[Str].
sexpr1([function, Expression]) --> ['#'''], !, sexpr1(Expression).
sexpr1(PClosure) --> {compound(PClosure),functor(PClosure,closure,_),with_output_to(atom(SPClosure),fmt9(PClosure)),trim_full_stop(SPClosure,TSPClosure)}, ['{',TSPClosure,'}.'], !.
sexpr1([quote, Expression]) --> [''''], !, sexpr1(Expression).
sexpr1(Dict) --> {is_dict(Dict,T),Dict=..[_,_|Rest]},!, ['#<'],sexpr1(T),lisplist(Rest,'>').
sexpr1('$OBJ'(T,X)) --> ['#<'],sexpr1(T),sexpr1(X),['>'].
sexpr1('$COMMA'(X)) --> [','],sexpr1(X).
sexpr1(['$COMMA',X]) --> [','],sexpr1(X).
sexpr1(['$BQ',X])--> ['`'],sexpr1(X).
sexpr1([]) --> !, ['(',')'].
sexpr1([X|Y]) --> ['('],  sexpr1(X), lisplist(Y,')').
sexpr1(X) --> {compound(X),compound_name_arguments(X,F,ARGS)}, ['#<'],[F],lisplist(ARGS,'>').
sexpr1(X) --> [X].

lisplist([],EQ) --> [EQ], !.
lisplist([X|Xs],EQ) --> sexpr1(X), !, lisplist(Xs,EQ).
lisplist(X,EQ) --> ['.'], sexpr1(X), [EQ].


cl_format(Stream,Fmt,Args):-wdmsg(cl_format(Stream,Fmt,Args)).
cl_format(Stream,Fmt,Arg1,Arg2):-wdmsg(cl_format(Stream,Fmt,Arg1,Arg2)).

cl_print(X,X):-copy_term(X,Y),writeExpression(Y).


% writeExpression/1 displays a lisp expression

writeExpression(X):- is_ftVar(X),(get_var_name(X,N)->format('~w~w)',[N,X]);format('~w',[X])),!.
writeExpression([]):- write('NIL').
% writeExpression(quit):- !, write('Terminating WAM-CL'),nl.
writeExpression('$COMMENT0'([])):- 	writeln(';'),!.
writeExpression('$COMMENT'(S)):- 	write(';'),writeln(S),!.
writeExpression('$COMMENT1'(S)):- 	write('#|'),write(S),writeln('|#').
writeExpression('$COMMENT'(S,_,_)):- 	write('#|'),write(S),writeln('|#').
writeExpression(Expression):-
	sexpr1(Expression, TokenL, []), !, %	write('  '),
	writeTokenL(TokenL).

no_right_padding('(').
no_right_padding(')').
no_right_padding(X):-need_right_padding(X),!,fail.
no_right_padding(Atom):- \+ atom(Atom),!,fail.
no_right_padding(Atom):- \+ atom_length(Atom,1),!,fail.
% no_right_padding(Atom):- upcase_atom(Atom,Atom).
need_right_padding('.').


writeTokenL([]).

writeTokenL(['(', ')'|TokenL]):- !,
	write('NIL '),
	writeTokenL(TokenL).
writeTokenL([NRP|TokenL]):- no_right_padding(NRP),!, write(NRP), writeTokenL(TokenL).
writeTokenL([')', '('|TokenL]):- !, writeTokenL([')('|TokenL]).
writeTokenL(['('|TokenL]):- write(' ('), writeTokenL(TokenL).
writeTokenL([')'|TokenL]):- write(') '), writeTokenL(TokenL).
writeTokenL([Token|TokenL]):- atomic(Token), \+ atom(Token),!,
   writeq(Token),
   write(' '),
   writeTokenL(TokenL).
writeTokenL([Token|TokenL]):-
   atom(Token),write_atom_obj(Token),!,
   write(' '),
   writeTokenL(TokenL).
writeTokenL([Token|TokenL]):-
   writeq(Token),
   write(' '),
   writeTokenL(TokenL).



write_atom_obj(Package):- package_name(Package,Name),!,write('#<PACKAGE '),write(Name),write('>').
write_atom_obj(Symbol):-  print_symbol(Symbol),!.


:- fixup_exports.


