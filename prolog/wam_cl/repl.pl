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
	user:named_lambda/2,
        user:macro_lambda/5,
        user:function_lambda/4.
:- multifile 
	user:named_lambda/2,
        user:macro_lambda/5,
        user:function_lambda/4.

:- initialization((lisp,prolog),main).

:- meta_predicate(timel(+,:)).
timel(What,M:X):- write('% '),writeln(What),prolog_statistics:time(M:X).

both_outputs(G):-
  notrace((current_output(O),stream_property(CO,alias(user_output)),
  (CO\==O -> with_output_to(CO,G) ; true),G)).

lisp_dumpST:- both_outputs(dumpST).

always_catch(G):- catch(catch(G,'$aborted',notrace),_,notrace).

gripe_problem(Problem,G):- always_catch(gripe_problem0(Problem,G)).
gripe_problem0(Problem,G):-
     notrace, 
     wdmsg((Problem:-G)),lisp_dumpST,
     dbmsg((Problem:-G)),
     (rtrace(G)*->(notrace,break);(wdmsg(failed_rtrace(G)),notrace,break,!,fail)).

with_nat_term(G):-
  \+ \+ ((
  (term_attvars(G,Vs),
    maplist(del_attr_rev2(freeze),Vs),
    maplist(del_attr_rev2(tracker),Vs),
   G))).

quietly_must_or_rtrace(G):- 
  (catch(quietly(G),E,gripe_problem(uncaught(E),G)) 
   *-> true ; (gripe_problem(fail_must_or_rtrace_failed,G),!,fail)),!.
nonquietly_must_or_rtrace(G):- 
  (catch((G),E,gripe_problem(uncaught(E),G)) 
   *-> true ; (gripe_problem(fail_must_or_rtrace_failed,G),!,fail)),!.

must_or_rtrace((A,B)):-!,must_or_rtrace(A),must_or_rtrace(B).
must_or_rtrace(G):-
   %notrace(with_nat_term(wdmsg(tracing(G)))),
   notrace(tracing),!,
   (catch((G),E,gripe_problem(uncaught(E),G)) 
    *-> true ; (gripe_problem(fail_must_or_rtrace_failed,G),!,fail)),!.
must_or_rtrace(G):- nonquietly_must_or_rtrace(G).

expand_pterm_to_sterm(VAR,VAR):- notrace(is_ftVar(VAR)),!.
expand_pterm_to_sterm('NIL',[]):-!.
expand_pterm_to_sterm(nil,[]):-!.
expand_pterm_to_sterm(VAR,VAR):- \+ compound(VAR),!.
expand_pterm_to_sterm([X|L],[Y|Ls]):-!,expand_pterm_to_sterm(X,Y),expand_pterm_to_sterm(L,Ls),!.
expand_pterm_to_sterm(X,STerm):- compound_name_arguments(X,F,L),expand_pterm_to_sterm(L,Y),!,maybe_sterm(F,Y,STerm).
expand_pterm_to_sterm(X,X).
maybe_sterm(F,Y,PTerm):- keep_as_compund(F),PTerm=..[F|Y].
maybe_sterm(F,Y,[F|Y]).
keep_as_compund(function).
keep_as_compund(closure).
keep_as_compund(prolog).
keep_as_compund(ugly).
keep_as_compund('$OBJ').
keep_as_compund('$CHAR').
keep_as_compund(v).
keep_as_compund(obj).
keep_as_compund(D):-atom_concat('$',_,D).

str_to_expression(Str, Expression):- lisp_add_history(Str),parse_sexpr_untyped_read(string(Str), Expression),!.
str_to_expression(Str, Expression):- with_input_from_string(Str,read_and_parse(Expression)),!.

remove_comments(IO,IO):- \+ compound(IO),!.
remove_comments([I|II],O):- is_comment(I,_),!,remove_comments(II,O).
remove_comments([I|II],[O|OO]):-remove_comments(I,O),!,remove_comments(II,OO).
remove_comments(IO,IO).

as_sexp(I,O):- as_sexp1(I,M),remove_comments(M,O).

as_sexp1(NIL,NIL):-NIL==[],!.
as_sexp1(Stream,Expression):- is_stream(Stream),!,must(parse_sexpr_untyped(Stream,SExpression)),!,as_sexp2(SExpression,Expression).
as_sexp1(s(Str),Expression):- !, must(parse_sexpr_untyped(string(Str),SExpression)),!,as_sexp2(SExpression,Expression).
as_sexp1(Str,Expression):- notrace(catch(text_to_string(Str,String),_,fail)),!, 
    must_or_rtrace(parse_sexpr_untyped(string(String),SExpression)),!,as_sexp2(SExpression,Expression).
as_sexp1(Str,Expression):- as_sexp2(Str,Expression),!.

as_sexp2(Str,Expression):- is_list(Str),!,maplist(expand_pterm_to_sterm,Str,Expression).
as_sexp2(Str,Expression):- expand_pterm_to_sterm(Str,Expression),!.

dbmsg(X):- both_outputs(dbmsg0(X)).

in_comment(X):- notrace((write('/* '),(X),writeln(' */'))).


dbmsg0(Var):- var(Var),!,in_comment(colormsg1(dbmsg_var(Var))).
dbmsg0(Str):- string(Str),!,in_comment(colormsg1(Str,[])).
dbmsg0(:- asserta(A)):- !, colormsg1(A).
dbmsg0(:- assert(A)):- !, colormsg1(A).
dbmsg0(:-((B,asserta(A)))):- !, dbmsg0(:- B), dbmsg0(:-asserta(A)).
dbmsg0(:-((asserta(A),B))):- !, dbmsg0(:-asserta(A)),dbmsg0(:- B).
dbmsg0(comment(S)):- in_comment(fmt9(S)).
dbmsg0(N=V):- in_comment(fmt9(N=V)).
dbmsg0(H :- Body):- !,colormsg1(H :- Body),!.
dbmsg0(:- Body):- !,colormsg1(:- Body),!.
dbmsg0(Body):- in_comment(colormsg1(:- Body)),!.
% dbmsg(:- Body):- !, dmsg(:- Body).

colormsg1(Msg,Args):- mesg_color(Msg,Ctrl),!,ansicall(Ctrl,format(Msg,Args)).
colormsg1(Msg):- mesg_color(Msg,Ctrl),!,ansicall(Ctrl,fmt90(Msg)).

print_eval_string(Str):-
   str_to_expression(Str, Expression),
   dmsg(:- print_eval_string(Expression)),
   eval_at_repl(Expression, Result),!,
   write_results(Result),
   !.

eval_string(Str):-
   str_to_expression(Str, Expression),
   eval_at_repl(Expression, Result),!,
   write_results(Result),!.


trace_eval_string(Str):-
  str_to_expression(Str, Expression),
   redo_call_cleanup(trace,eval(Expression, Result),notrace),
     write_results(Result),
     !.

rtrace_eval_string(Str):-
  str_to_expression(Str, Expression),
   rtrace(eval(Expression, Result)),
     write_results(Result),
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
	%tidy_database,
	repeat,
        notrace,
   	catch(once(read_eval_print(Result)),_,notrace),
   	Result == end_of_file,!,
   	prompt(_, Old),
   	prompts(Old1, Old2),!.


tidy_database:-
	nb_delete('$env_current'),
        env_current(_Env),
	retractall(lambda(_, _)).

show_uncaught_or_fail((A,B)):-!,show_uncaught_or_fail(A),show_uncaught_or_fail(B).
show_uncaught_or_fail(G):- flush_all_output_safe,
  (catch(G,E,wdmsg(uncaught(E)))*->true;(wdmsg(failed(G)),!,fail)).

read_eval_print(Result):-		% dodgy use of cuts to force a single evaluation
        show_uncaught_or_fail(read_no_parse(Expression)),!,
        show_uncaught_or_fail(lisp_add_history(Expression)),!,
        nb_linkval('$mv_return',[Result]),
        show_uncaught_or_fail(eval_at_repl(Expression,Result)),!,
        show_uncaught_or_fail(write_results(Result)),!.
	

write_results(Result):- 
 writeExpression(Result),
 ignore((nb_current('$mv_return',[Result|Push]),writeExtraValues(Push))),
 nl.

writeExtraValues(X):- maplist(writeExpressionEV,X),!.
writeExpressionEV(X):- writeln(' ;'),writeExpression(X),flush_all_output_safe.

/*
:- if(exists_source(library(sexpr_reader))).
:- use_module(library(sexpr_reader)).
read_and_parse_i(Expr):- current_input(In), parse_sexpr_untyped_read(In, Expr).
:- endif.
*/

% read and parse a line of Lisp
/*
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
*/
 
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

:- set_prolog_flag(lisp_no_compile,false).

% basic EVAL statements for built-in procedures
eval_at_repl(Var,  R):- var(Var),!, R=Var.
eval_at_repl(Expression, Result):- eval_repl_hooks(Expression,Result),!.
eval_at_repl(Expression,Result):- 
  notrace(as_sexp(Expression,SExpression)),
  reader_intern_symbols(SExpression,LExpression),
  dbmsg(:- lisp_compiled_eval(LExpression)),
  env_current(Env),
  timel('COMPILER',always_catch(maybe_ltrace(lisp_compile(Env,Result,LExpression,Code)))),
  dbmsg(:-Code),  
  timel('EXEC',always_catch(ignore(must_or_rtrace(maybe_ltrace(call(user:Code)))))),!.

eval(Expression, Result):- env_current(Env), eval(Expression, Env, Result).

eval(Expression, Env, Result):-
   always_catch(maybe_ltrace(lisp_compile(Env,Result,Expression,Code))), 
   always_catch(ignore(must_or_rtrace(maybe_ltrace(call(user:Code))))),!.


/*:- if(exists_source(library(sexpr_reader))).
:- use_module(library(sexpr_reader)).
:- endif.
*/
read_and_parse(Expr):-  flush_all_output_safe,current_input(In),parse_sexpr_untyped_read(In, Expr).
read_no_parse(Expr):-  flush_all_output_safe,current_input(In),parse_sexpr_untyped(In, Expr).

flush_all_output_safe:- forall(stream_property(S,mode(write)),notrace(catch(flush_output(S),_,true))).

parse_sexpr_untyped_read(In, Expr):- 
  parse_sexpr_untyped(In,ExprS),!,
  as_sexp(ExprS,ExprS1),!,reader_intern_symbols(ExprS1,Expr).


eval_repl_hooks(nil,  []):-!.
eval_repl_hooks(Atom, R):- atom(Atom),atom_concat(_,'.',Atom),notrace(catch(read_term_from_atom(Atom,Term,[variable_names(Vs),syntax_errors(true)]),_,fail)),
  callable(Term),current_predicate(_,Term),b_setval('$variable_names',Vs),t_or_nil((user:call(Term)*->dmsg(Term);(dmsg(no(Term)),fail)),R).
eval_repl_hooks([quote, X], X):-!.
eval_repl_hooks([debug,A], t):- debug(lisp(A)).
eval_repl_hooks([nodebug,A], t):- nodebug(lisp(A)).
eval_repl_hooks([X], R):- eval_repl_atom( X, R),!.
eval_repl_hooks( X , R):- eval_repl_atom( X, R),!.

maybe_ltrace(G):- current_prolog_flag(lisp_trace,true)->rtrace(G);must_or_rtrace(G).

eval_repl_atom(end_of_file, end_of_file):-!.
eval_repl_atom(quit, end_of_file):-!.
eval_repl_atom(prolog, t):- !, prolog.
eval_repl_atom(ltrace, t):- set_prolog_flag(lisp_trace,true).
eval_repl_atom(noltrace, t):- set_prolog_flag(lisp_trace,false).

eval_repl_atom(debug, t):- debug(lisp(_)),debug,debugging.
eval_repl_atom(nodebug, t):- nodebug(lisp(_)),nodebug,debugging.

eval_repl_atom(make, O):- !, must_or_rtrace((make, cl_compile_file(pack('wam_commmon_lisp/prolog/wam_cl/xabcl'),O))).

eval_repl_atom(show, t):- 
  listing([user:named_lambda/2,
        user:macro_lambda/5,
        user:function_lambda/4]).

:- fixup_exports.
:- cddd.
% invoke_eval(['in-package', "SYSTEM"], In_package_Ret):
%lisp_add_history("prolog.")
:- initialization((lisp,prolog),main).
%:- lisp_add_history("lisp.").

:- set_prolog_flag(verbose_autoload,false).



