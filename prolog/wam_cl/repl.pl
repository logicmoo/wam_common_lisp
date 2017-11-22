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
timel(What,M:X):- notrace(( write('% '),writeln(What))),prolog_statistics:time(M:X).

both_outputs(G):-
  notrace((current_output(O),stream_property(CO,alias(user_output)),
  (CO\==O -> with_output_to(CO,G) ; true),G)).

lisp_dump_break:- lisp_dumpST,break.
lisp_dumpST:- both_outputs(dumpST).

always_catch(G):- G. %  catch(catch(G,'$aborted',notrace),_,notrace).

gripe_problem(Problem,G):- always_catch(gripe_problem0(Problem,G)).
gripe_problem0(Problem,G):-
     notrace(( 
     wdmsg((Problem:-G)),lisp_dumpST,
     dbmsg((Problem:-G)))),
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
must_or_rtrace(G):- notrace(tracing),G. % nonquietly_must_or_rtrace(G).
must_or_rtrace(G):- nonquietly_must_or_rtrace(G).


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
        catch(read_eval_print(Result),'$aborted',fail),
   	notrace(Result == end_of_file),!,
   	prompt(_, Old),
   	prompts(Old1, Old2),!.


tidy_database:-
	nb_delete('$env_current'),
        env_current(_Env),
	retractall(lambda(_, _)).

show_uncaught_or_fail((A,B)):-!,show_uncaught_or_fail(A),show_uncaught_or_fail(B).
show_uncaught_or_fail(G):- notrace(flush_all_output_safe),
  (catch(G,E,notrace((wdmsg(uncaught(E)),!,fail)))*->true;notrace((wdmsg(failed(G)),!,fail))).

read_eval_print(Result):-		% dodgy use of cuts to force a single evaluation
        quietly(show_uncaught_or_fail(read_no_parse(Expression))),!,
        quietly(show_uncaught_or_fail(lisp_add_history(Expression))),!,
        nb_linkval('$mv_return',[Result]),
        show_uncaught_or_fail(eval_at_repl(Expression,Result)),!,
        quietly(show_uncaught_or_fail(write_results(Result))),!.
	

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
eval_at_repl(Var,  R):- notrace(var(Var)),!, R=Var.
eval_at_repl(Expression, Result):- quietly(eval_repl_hooks(Expression,Result)),!.
eval_at_repl(Expression,Result):- notrace(tracing), !, eval_at_repl_tracing(Expression,Result).
eval_at_repl(Expression,Result):-
  notrace(as_sexp(Expression,SExpression)),
  quietly(reader_intern_symbols(SExpression,LExpression)),
  notrace(dbmsg(:- lisp_compiled_eval(LExpression))),
  notrace(debug_var('ReplEnv',Env)),
  timel('COMPILER',always_catch(maybe_ltrace(lisp_compile(Env,Result,LExpression,Code)))),
  notrace(dbmsg(:-Code)),
  (notrace(tracing)-> Code ; 
   timel('EXEC',always_catch(ignore(must_or_rtrace(maybe_ltrace(call(user:Code))))))),!.

eval_at_repl_tracing(Expression,Result):-
  quietly(as_sexp(Expression,SExpression)),
  quietly(reader_intern_symbols(SExpression,LExpression)),
  notrace(debug_var('ReplEnv',Env)),
  notrace(cls),
   notrace((writeln(==================================================================))),
   notrace((writeln(==================================================================))),
   notrace((writeln(==================================================================))),
   notrace(dbmsg(:- lisp_compiled_eval(LExpression))),
   notrace((writeln(==================================================================))),
   notrace((writeln(==================================================================))),
   notrace((writeln(==================================================================))),
  lisp_compile(Env,Result,LExpression,Code),
  % notrace(cls),
   notrace((writeln(==================================================================))),
   notrace((writeln(==================================================================))),
   notrace((writeln(==================================================================))),
   notrace(dbmsg(:-Code)),
   notrace((writeln(==================================================================))),
   notrace((writeln(==================================================================))),
   notrace((writeln(==================================================================))),
  Code.

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


eval_repl_hooks(V,_):-var(V),!,fail.
eval_repl_hooks(nil,  []):-!.
eval_repl_hooks(Atom, R):- atom(Atom),atom_concat(_,'.',Atom),notrace(catch(read_term_from_atom(Atom,Term,[variable_names(Vs),syntax_errors(true)]),_,fail)),
  callable(Term),current_predicate(_,Term),b_setval('$variable_names',Vs),t_or_nil((user:call(Term)*->dmsg(Term);(dmsg(no(Term)),fail)),R).
eval_repl_hooks([quote, X], X):-!.
eval_repl_hooks([debug,A], t):- !,debug(lisp(A)).
eval_repl_hooks([nodebug,A], t):- !, nodebug(lisp(A)).
eval_repl_hooks([UC|A], R):- atom(UC),downcase_atom(UC,DC),DC\==UC,!,eval_repl_hooks([DC|A], R).
eval_repl_hooks([X], R):-!, eval_repl_atom( X, R),!.
eval_repl_hooks( X , R):- eval_repl_atom( X, R),!.

maybe_ltrace(G):- current_prolog_flag(lisp_trace,true)->rtrace(G);must_or_rtrace(G).

eval_repl_atom(V,_):-var(V),!,fail.
eval_repl_atom(end_of_file, end_of_file):-!.
eval_repl_atom(quit, end_of_file):-!.
eval_repl_atom(prolog, t):- !, prolog.
eval_repl_atom(ltrace, t):- set_prolog_flag(lisp_trace,true),debug(lisp(trace)),debug,debugging.
eval_repl_atom(noltrace, t):- set_prolog_flag(lisp_trace,false),nodebug(lisp(trace)),nodebug,debugging.

eval_repl_atom(debug, t):- debug(lisp(_)),debug,debugging.
eval_repl_atom(nodebug, t):- nodebug(lisp(_)),nodebug,debugging.

eval_repl_atom(make, O):- !, must_or_rtrace((make, cl_compile_file_mask(pack('wam_commmon_lisp/prolog/wam_cl/lisp/'),keys([]),O))).

eval_repl_atom(UC, R):- atom(UC),downcase_atom(UC,DC),DC\==UC,eval_repl_atom(DC, R).


eval_repl_atom(show, t):- 
  listing([user:named_lambda/2,
        user:macro_lambda/5,
        user:function_lambda/4]).

lw:- cl_load("wam-cl-params",_).
%:- cddd.
% invoke_eval(['in-package', "SYSTEM"], In_package_Ret):
%lisp_add_history("prolog.")
:- initialization((lisp,prolog),main).
%:- lisp_add_history("lisp.").

:- set_prolog_flag(verbose_autoload,false).
:- fixup_exports.



