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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(print, []).
:- set_module(class(library)).
:- include('header.pro').


% Grammar rules for printing Lisp s-expressions.
% Given a list of tokens, lisplist does all the nesting of lists

trim_full_stop(SPClosure,TSPClosure):-atom_concat_or_rtrace(SPClosureN,'\n',SPClosure),!,trim_full_stop(SPClosureN,TSPClosure).
trim_full_stop(SPClosure,TSPClosure):-atom_concat_or_rtrace(SPClosureN,' ',SPClosure),!,trim_full_stop(SPClosureN,TSPClosure).
trim_full_stop(SPClosure,SPClosureN):-atom_concat_or_rtrace(SPClosureN,'.',SPClosure).
trim_full_stop(SPClosure,SPClosure).

lisp_chars_to_pl_string(List,SS):- always((maplist(to_prolog_codes,List,Codes),text_to_string(Codes,SS))).

shrink_lisp_strings(Str,PStr):- \+ compound(Str),!,Str=PStr.
%shrink_lisp_strings(Str,PStr):- is_stringp(Str),!,to_prolog_string(Str,PStr).
shrink_lisp_strings(Str,PStr):- is_list(Str),maplist(is_characterp,Str),lisp_chars_to_pl_string(Str,PStr),!.
shrink_lisp_strings(C1,C2):- compound_name_arguments(C1,F,C1O),must_maplist(shrink_lisp_strings,C1O,C2O),C2=..[F|C2O].

sexpr1(X) --> {is_ftVar(X),(get_var_name(X,N)->format(atom(NN),'~w',[N]);format(atom(NN),'~w',[X]))},!,[NN].
sexpr1(Str)--> {is_stringp(Str),to_prolog_string(Str,PStr)},!,[PStr].
sexpr1([function, Expression]) --> ['#'''], !, sexpr1(Expression).
sexpr1(('$CHAR'(X))) --> {format(atom(NN),'#\\~w',[X])},!,[NN].
sexpr1('$OBJ'('$CHAR',(X))) --> sexpr1(['$CHAR',X]).
sexpr1(PClosure) --> {compound(PClosure),functor(PClosure,closure,_),with_output_to(atom(SPClosure),fmt9(PClosure)),trim_full_stop(SPClosure,TSPClosure)}, ['{',TSPClosure,'}.'], !.
sexpr1([quote, Expression]) --> [''''], !, sexpr1(Expression).
sexpr1(Dict) --> {is_dict(Dict,T),Dict=..[_,_|Rest]},!, ['#<'],sexpr1(T),lisplist(Rest,'>').
sexpr1('$OBJ'([T,X])) --> sexpr1('$OBJ'(T,X)).
sexpr1('$OBJ'([T|X])) --> sexpr1('$OBJ'(T,X)).
sexpr1('$OBJ'(T,X)) --> {T==claz_prolog,with_output_to(atom(SPClosure),fmt9(X)),trim_full_stop(SPClosure,TSPClosure)}, 
  ['{',TSPClosure,'}.'], !.
sexpr1('$OBJ'(T,X)) --> {T==claz_vector},['#'],sexpr1(X).
sexpr1('$OBJ'(T,X)) --> {T==claz_pathname},['#P'],sexpr1(X).
sexpr1('$COMPLEX'(R,I)) --> ['#C('],sexpr1(R),sexpr1(I),[')'].
sexpr1('$RATIO'(R,I)) --> [''],sexpr1(R),['/'],sexpr1(I),[''].

sexpr1('$NUMBER'(claz_double_float,V)) --> {format_number(O,'d',V)},[O].
sexpr1('$NUMBER'(claz_single_float,V)) --> {format_number(O,'e',V)},[O].
sexpr1('$NUMBER'(claz_long_float,V)) --> {format_number(O,'L',V)},[O]. % SBCL = claz_double_float
sexpr1('$NUMBER'(claz_short_float,V)) --> {format_number(O,'s',V)},[O]. % SBCL = claz_single_float
sexpr1('$NUMBER'(T,V)) --> {format_number(O,T,V)},[O].
sexpr1('$S'(X)) --> ['#S'],sexpr1(X).
sexpr1('$OBJ'(T,X)) --> ['#S'],{is_list(X),is_structure_class(T),claz_to_symbol(T,TP)},sexpr1(TP),sexpr1(X).
sexpr1('$OBJ'(T,X)) --> ['#<'],{claz_to_symbol(T,TP)},!,sexpr1(TP),sexpr1(X),['>'].
sexpr1('$OBJ'(T,X)) --> ['#<'],!,sexpr1(T),sexpr1(X),['>'].
%sexpr1('$COMMA'(X)) --> [','],sexpr1(X).

sexpr1(['$COMMA',X]) --> [','],sexpr1(X).
sexpr1(['$BQ',X])--> ['`'],sexpr1(X).
sexpr1(['$BQ-COMMA-ELIPSE',X])--> [',@'],sexpr1(X).
sexpr1([]) --> !, ['(',')'].
sexpr1([X|Y]) --> ['('],  sexpr1(X), lisplist(Y,')').
sexpr1(X) --> {compound(X),compound_name_arguments(X,F,ARGS)}, ['#<'],[F],lisplist(ARGS,'>').
sexpr1(X) --> [X].

format_number(O,T,V):-format(atom(S),'~w',[V]),atomic_list_concat([L|RS],'e',S),
   ((RS=[_]) -> atomic_list_concat([L|RS],T,O) ; atomic_list_concat([L,0],T,O)).


lisplist([],EQ) --> [EQ], !.
lisplist([X|Xs],EQ) --> sexpr1(X), !, lisplist(Xs,EQ).
lisplist(X,EQ) --> ['.'], sexpr1(X), [EQ].

wl:declared(cl_format,lambda(['&rest',r])).
cl_format([Stream,Fmt],t):- !, cl_print(cl_format(Stream,Fmt),_).
cl_format([Stream,Fmt|ArgS],t):-cl_print(cl_format(Stream,Fmt,ArgS),_).

cl_prin1(X,X):-copy_term(X,Y),writeExpression(Y),nl.
cl_princ(X,X):-copy_term(X,Y),writeExpression(Y),nl.
cl_print(X,X):-cl_prin1(X,X),nl.
cl_terpri(t):-nl.
cl_write_line(X,Y):-cl_princ(X,Y),nl.

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

no_right_padding(')'):-!,fail.
no_right_padding('#'):-!,fail.
no_right_padding('#''').
no_right_padding('#C(').
no_right_padding('`').
no_right_padding('#<').
no_right_padding('#P').
no_right_padding('#\'').
no_right_padding('''').
no_right_padding('#S').
no_right_padding(',').
no_right_padding('(').
no_right_padding(',@').

no_right_padding(S):- atom_concat(_,'(',S).
no_right_padding(S):- atom_concat('#',_,S).
%no_right_padding(')').            
no_right_padding(X):-need_right_padding(X),!,fail.
no_right_padding(Atom):- \+ atom(Atom),!,fail.
no_right_padding(Atom):- \+ atom_length(Atom,1),!,fail.
% no_right_padding(Atom):- upcase_atom(Atom,Atom).
need_right_padding('.').

no_left_padding(S):- atom(S), atom_concat(')',_,S).

maybe_write_space([]):-!.
maybe_write_space([S|_]):- no_left_padding(S),!.
maybe_write_space(_):- write(' ').


writeTokenL([]).
writeTokenL([''|Rest]):- !,writeTokenL3(Rest).
writeTokenL(['{',Code,'}.'|TokenL]):- write({Code}),write('.'), writeTokenL(TokenL).
writeTokenL(['(', ')'|TokenL]):- !,
	write('NIL '),
	writeTokenL(TokenL).
writeTokenL([NRP|TokenL]):- no_right_padding(NRP),!, write(NRP), writeTokenL(TokenL).
writeTokenL([')', '('|TokenL]):- !, writeTokenL([')('|TokenL]).
writeTokenL(['('|TokenL]):- write(' ('), writeTokenL(TokenL).
writeTokenL([')'|TokenL]):- write(') '), writeTokenL(TokenL).
writeTokenL([Token|TokenL]):- writeTokenL2([Token|TokenL]).

writeTokenL2([Token|TokenL]):- atomic(Token), \+ atom(Token),!,
   writeq(Token),
   maybe_write_space(TokenL),
   writeTokenL(TokenL).
writeTokenL2([Token|TokenL]):-
   atom(Token),
   write_atom_obj(Token),!,
   maybe_write_space(TokenL),
   writeTokenL(TokenL).
writeTokenL2([Token|TokenL]):-
   writeq(Token),
   maybe_write_space(TokenL),
   writeTokenL(TokenL).

writeTokenL3([]):-!.
writeTokenL3([Token|TokenL]):- atomic(Token), \+ atom(Token),!,
   writeq(Token),
   writeTokenL3(TokenL).
writeTokenL3([Token|TokenL]):-
   atom(Token),
   write_atom_obj(Token),!,
   writeTokenL3(TokenL).
writeTokenL3([Token|TokenL]):-
   writeq(Token),
   writeTokenL3(TokenL).



write_atom_obj(Package):- package_name(Package,Name),!,write('#<PACKAGE '),write(Name),write('>').
write_atom_obj(Atom):- atom(Atom),atomic_list_concat([Type,Named],'_znst_',Atom),
   atomic_concat('claz_',Type,Kind),!,
   write('#<'),write(Kind),write(' '),write(Named),write('>').
write_atom_obj(Symbol):-  print_symbol(Symbol),!.


:- fixup_exports.


