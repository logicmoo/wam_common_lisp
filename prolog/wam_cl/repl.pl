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
:- module(cl, []).
:- set_module(class(library)).
:- include('header.pro').

:- use_module(sreader).

:- dynamic 
	named_lambda/2,
        macro_lambda/4.

:- initialization((lisp,prolog),main).

:- meta_predicate(timel(:)).
timel(M:X):- prolog_statistics:time(M:X).

must_or_rtrace(G):-
  (catch(quietly(G),E,(dbmsg(uncaught(E:-G)),!,fail))->true;rtrace(G)),!.

expand_pterm_to_sterm(SNIL,NIL):- SNIL=='NIL',!,NIL=[].
expand_pterm_to_sterm(SNIL,NIL):- SNIL=='nil',!,NIL=[].
expand_pterm_to_sterm(VAR,VAR):- \+ compound(VAR),!.
expand_pterm_to_sterm([X|L],[Y|Ls]):-!,expand_pterm_to_sterm(X,Y),expand_pterm_to_sterm(L,Ls),!.
expand_pterm_to_sterm(X,[F|Y]):- compound_name_arguments(X,F,L),expand_pterm_to_sterm(L,Y),!.
expand_pterm_to_sterm(X,X).

str_to_expression(Str, Expression):- lisp_add_history(Str),parse_sexpr_untyped(string(Str), Expression),!.
str_to_expression(Str, Expression):- with_input_from_string(Str,read_and_parse(Expression)),!.

as_sexp(NIL,NIL):-NIL==[],!.
as_sexp(Stream,Expression):- is_stream(Stream),!,must(parse_sexpr_untyped(Stream,Expression)).
as_sexp(s(Str),Expression):- must(parse_sexpr_untyped(string(Str),Expression)),!.
as_sexp(Str,Expression):- notrace(catch(text_to_string(Str,String),_,fail)),!, must(parse_sexpr_untyped(string(String),Expression)),!.
as_sexp(Str,Expression):- is_list(Str),!,maplist(expand_pterm_to_sterm,Str,Expression).
as_sexp(Str,Expression):- expand_pterm_to_sterm(Str,Expression),!.

dbmsg(X):- writeln('/*'), dbmsg0(X),writeln('*/').
dbmsg0(Str):- string(Str),!,colormsg1(Str,[]).
dbmsg0((Textbody:-Body)):-body==Textbody,colormsg1('==>'(body)),!,dbmsg0(Body).
dbmsg0(Var):- var(Var),!,colormsg1(dbmsg_var(Var)).
dbmsg0(:-((asserta(A),B))):- dbmsg0("~N:- asserta((~n"),dbmsg0(A),dbmsg0("~N)).~N"),dbmsg0(:- B).
dbmsg0(((asserta(A),B))):- dbmsg0("~N asserta((~n"),dbmsg0(A),dbmsg0("~N)).~N"),dbmsg0(:- B).
% dbmsg0((A,B)):-compound(A),compound(B),functor(A,F,N),functor(B,F,N),!,dbmsg0(A),dbmsg0(B).
dbmsg0(asserta(A)):- dbmsg0("~Nasserta((~n"),dbmsg0(A),dbmsg0("~N)).~N").
dbmsg0(:- asserta(A)):- dbmsg0("~N:- asserta((~n"),dbmsg0(A),dbmsg0("~N)).~N").
dbmsg0(ABody):- ABody=..[A,Body],nonvar(Body), Body = (H :- B) , !, colormsg1((dbmsg(A,H) :- B)).
dbmsg0(H :- Body):- !,colormsg1(H :- Body),!.
dbmsg0(:- Body):- !,colormsg1(:- Body),!.
dbmsg0(Body):- !,colormsg1(:- Body),!.
% dbmsg(:- Body):- !, dmsg(:- Body).

colormsg1(Msg,Args):- mesg_color(Msg,Ctrl),!,ansicall(Ctrl,format(Msg,Args)).
colormsg1(Msg):- mesg_color(Msg,Ctrl),!,ansicall(Ctrl,fmt90(Msg)).

print_eval_string(Str):-
  str_to_expression(Str, Expression),
   dmsg(print_eval_string(Expression)),
   eval(Expression, Result),!,
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
lisp:- write('
__        ___    __  __        ____ _
\\ \\      / / \\  |  \\/  |      / ___| |
 \\ \\ /\\ / / _ \\ | |\\/| |_____| |   | |
  \\ V  V / ___ \\| |  | |_____| |___| |___
   \\_/\\_/_/   \\_\\_|  |_|      \\____|_____|
'),nl,
	write('Common Lisp, written in Prolog'),nl,
	prompt(Old, '> '),
	prompts(Old1, Old2),
	prompts('> ', '> '),
	% tidy_database,
	repeat,
   	once(read_eval_print(Result)),
   	Result == quit,!,
   	prompt(_, Old),
   	prompts(Old1, Old2),!.


tidy_database:-
	retract(lisp_global_bindings(_)),
	asserta(lisp_global_bindings([])),
	retractall(lambda(_, _)).


read_eval_print(Result):-		% dodgy use of cuts to force a single evaluation
	read_and_parse(Expression),
        lisp_add_history(Expression),
        catch((
        eval(Expression,Result),
	writeExpression(Result)),E,dmsg(uncaught(E))),
	!.





/*
:- if(exists_source(library(sexpr_reader))).
:- use_module(library(sexpr_reader)).
read_and_parse_i(Expr):- current_input(In), parse_sexpr_untyped(In, Expr).
:- endif.
*/

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

 
lisp_add_history(end_of_file):-!.
lisp_add_history(_):- prolog_load_context(reload,true),!.
lisp_add_history(_):- prolog_load_context(reloading,true),!.
lisp_add_history(Expression):- atom(Expression),!,
        prolog:history(user_input, add(Expression)).
lisp_add_history(Expression):- string(Expression),!,
        prolog:history(user_input, add(Expression)).
lisp_add_history(Expression):-
        with_output_to(string(S),writeExpression(Expression)),
        (string_upper(S,S)->string_lower(S,Store);Store=S),
        prolog:history(user_input, add(Store)).

:- set_prolog_flag(lisp_compile,true).
eval_compiled(SExpression,Result):-
    locally(set_prolog_flag(lisp_compile,true),eval_int(SExpression,Result)).

% basic EVAL statements for built-in procedures
eval_int(Var,  R):- var(Var),!, R=Var.
eval_int(SExpression,Result):- 
  as_sexp(SExpression,Expression),
  must_or_rtrace(eval(Expression,Result)).

eval(SExpression,Result):-eval_repl(SExpression,Result).
eval(Expression, Result):-
   current_prolog_flag(lisp_compile,true),!,
   dbmsg(lisp_compile(Expression)),
   maybe_ltrace(lisp_compile(Result,Expression,Code)),  % in compile.pl
   dbmsg(Code),
   maybe_ltrace(call(Code)),!.
eval(Expression, Result):-
   ensure_loaded(interp),
   lisp_global_bindings(Bindings),
   eval(Expression, Bindings, Result). % in interp.pl


/*:- if(exists_source(library(sexpr_reader))).
:- use_module(library(sexpr_reader)).
:- endif.
*/
read_and_parse(Expr):- current_input(In),parse_sexpr_untyped(In, Expr).


is_keyword_p(X):- atom(X),atom_concat(':',_,X).

:- use_module(library('dialect/sicstus/arrays')).
% :- use_module(library('dialect/sicstus')).
is_self_evaluationing_object(X):- var(X),!.
is_self_evaluationing_object(X):- atomic(X),!,(number(X);is_keyword_p(X);string(X);(blob(X,T),T\==text);X=t;X=[]),!.
is_self_evaluationing_object(X):- (is_dict(X);is_array(X);is_rbtree(X)),!.


eval_repl(nil,  []):-!.
eval_repl(Atom, R):- atom(Atom),atom_concat(_,'.',Atom),notrace(catch(read_term_from_atom(Atom,Term,[variable_names(Vs),syntax_errors(true)]),_,fail)),
  callable(Term),current_predicate(_,Term),b_setval('$variable_names',Vs),t_or_nil((call(Term)*->dmsg(Term);(dmsg(no(Term)),fail)),R).
eval_repl([quote, X], X):-!.
eval_repl([debug,A], t):- debug(lisp(A)).
eval_repl([nodebug,A], t):- nodebug(lisp(A)).
eval_repl([X], R):- eval_repl_atom( X, R),!.
eval_repl( X , R):- eval_repl_atom( X, R),!.

maybe_ltrace(G):- current_prolog_flag(lisp_trace,true)->rtrace(G);must_or_rtrace(G).

eval_repl_atom(end_of_file, quit):-!.
eval_repl_atom(quit, quit):-!.
eval_repl_atom(make, O):- !, must_or_rtrace((make, cl_compile_file('xabcl/',O))).
eval_repl_atom(prolog, t):- !, prolog.
eval_repl_atom(debug, t):- debug(lisp(_)).
eval_repl_atom(ltrace, t):- set_prolog_flag(lisp_trace,true).
eval_repl_atom(noltrace, t):- set_prolog_flag(lisp_trace,false).
eval_repl_atom(nodebug, t):- nodebug(lisp(_)).
eval_repl_atom(show, t):- 
  listing([named_lambda/2,
        user:macro_lambda/4,
        lisp_global_bindings/1]).


% invoke_eval(['in-package', "SYSTEM"], In_package_Ret):
:- initialization((lisp,prolog),main).
:- lisp_add_history("lisp.").

:- fixup_exports.
