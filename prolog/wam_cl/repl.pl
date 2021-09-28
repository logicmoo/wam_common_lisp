#!/usr/bin/env clif
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
:- module(cl, []). 

:- include('./header').

% :- cls.
set_stack_gb:- Six = 6, 
   Size is Six*10**9,
   set_prolog_flag(stack_limit, Size),
   set_prolog_stack(global, limit(Size)),
   set_prolog_stack(local, limit(Size)),
   set_prolog_stack(trail, limit(Size)).
%:- set_prolog_flag(occurs_check,error).
%:- set_prolog_flag(gc,true).
%:- set_prolog_flag(gc,false).

:- meta_predicate maybe_ltrace(0).
:- meta_predicate show_uncaught_or_fail(0).
:- meta_predicate with_prompt_str(+,0).
              

repl:- 
 in_md(cl,(
   lisp_banner,   
   set_prolog_flag(lisp_primordial,false), % requires  "PACKAGE:SYM" to already externally exists
   with_prompt_str('> ',
   ((	repeat,
        catch(read_eval_print(Result),'$aborted',fail),
   	quietly(Result == end_of_file)))))),!.



print_eval_string(Str):-
   str_to_expression(Str, Expression),
   userout(:- print_eval_string(Expression)),
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


% :- '$hide'(show_uncaught_or_fail/1).
show_uncaught_or_fail((A,B)):-!,show_uncaught_or_fail(A),show_uncaught_or_fail(B).
show_uncaught_or_fail(G):- quietly(flush_all_output_safe),
  (catch(G,E,quietly((wdmsg(uncaught(E)),rtrace(G),!,fail)))*->true;quietly((wdmsg(failed(G)),!,fail))).

prompts(Old1,_Old2):- var(Old1) -> prompt(Old1,Old1) ; prompt(_,Old1).
with_prompt_str(Str,G):- 
       prompt(Old, Str),
	prompts(Old1, Old2),
	prompts(Str,Str),
        call_cleanup(G,
                     (prompt(_, Old),
                       prompts(Old1, Old2))),!.

get_prompt_from_package(Suffix,Prompt):-
        reading_package(Package),
        short_package_or_hash(Package,Name0),
        (Name0=="U"->Name="CL-USER";Name=Name0),
        always(nonvar(Name)),
        atom_concat(Name,Suffix,Prompt),!.
get_prompt_from_package(Prompt,Prompt).

set_prompt_from_package:-
   get_prompt_from_package('>>> ',Prompt),
   prompt(_,Prompt).

read_eval_print(Result):-
        ignore(catch(lquietly(set_prompt_from_package),_,true)),
        set_md_lang(cl),
        get_prompt_from_package('> ',Prompt),prompt1(Prompt),
        lquietly(show_uncaught_or_fail(read_no_parse(Expression))),!,       
        lquietly(show_uncaught_or_fail(lisp_add_history(Expression))),!,
        nb_linkval('$mv_return',[Result]),
        set_md_lang(prolog),
        show_uncaught_or_fail(eval_at_repl(Expression,Result)),!,
        lquietly(show_uncaught_or_fail(write_results(Result))),!.
	

write_results(Result):- 
 writeExpression(Result),
 ignore((nb_current('$mv_return',[Result|Push]),writeExtraValues(Push))),
 nl.

writeExtraValues(X):- maplist(writeExpressionEV,X),!.
writeExpressionEV(X):- writeln(' ;'),writeExpression(X),flush_all_output_safe.


lisp_add_history(Var):-var(Var),!.
lisp_add_history(end_of_file):-!.
lisp_add_history([]):-!.
lisp_add_history(_):- prolog_load_context(reloading,true),!.
lisp_add_history(Expression):- atom(Expression),!,
        lisp_add_history_event(add(Expression)).
lisp_add_history(Expression):- string(Expression),!,string_to_atom(Expression,Atom),lisp_add_history(Atom).
lisp_add_history(Expression):- is_stringp(Expression),!,to_prolog_string(Expression,ExpressionS),lisp_add_history(ExpressionS).
lisp_add_history(Expression):-  
        with_output_to(string(S),writeExpression(Expression)),
        ((fail,string_upper(S,S))->string_lower(S,Store);Store=S),
        lisp_add_history(Store).


lisp_add_history_event(Store):- thread_signal(main,prolog:history(user_input, (Store))),!.
lisp_add_history_event(_Store):-!.
%lisp_add_history_event(Store):- current_input(Input),prolog:history(Input, (Store)).

:- set_prolog_flag(lisp_no_compile,false).

% basic EVAL statements for built-in procedures
eval_at_repl(Var,  R):- quietly(var(Var)),!, R=Var.
eval_at_repl(Expression, Result):- eval_repl_hooks(Expression,Result),!.
eval_at_repl(Expression,Result):- notrace((tracing, \+ t_l:rtracing)),call_cleanup(eval_at_repl_tracing(Expression,Result),trace).
eval_at_repl(Expression,Result):-
  lquietly(as_sexp(Expression,SExpression)),
  (reader_intern_symbols(SExpression,LExpression)),
  quietly(dbginfo(:- lisp_compiled_eval(LExpression))),
  quietly(debug_var('ReplEnv',Env)),
  timel('COMPILER',always_catch(maybe_ltrace(lisp_compile(Env,Result,LExpression,Code)))),
  quietly(dbginfo(:-Code)),
  (notrace(tracing)-> (user:Code) ; 
   timel('EXEC',always_catch(ignore(always(maybe_ltrace(call(user:Code))))))),!.

draw_cline :- notrace((writeln(';;; =================================================================='))).
:- '$hide'(draw_cline/0).
eval_at_repl_tracing(Expression,Result):-
  lquietly(as_sexp(Expression,SExpression)),
  (reader_intern_symbols(SExpression,LExpression)),
  writeq((reader_intern_symbols(SExpression,LExpression))),nl,
  quietly(debug_var('ReplEnv',Env)),
  %quietly(cls),
   draw_cline,draw_cline,draw_cline,
   userout(:- lisp_compiled_eval(LExpression)),
   draw_cline,draw_cline,draw_cline,
  timel('COMPILE',(offer_rtrace(lisp_compile(Env,Result,LExpression,Code)))),
  % quietly(cls),
   draw_cline,draw_cline,draw_cline,
   userout(:-Code),
   draw_cline,timel('PREEXEC',(offer_rtrace((user:Code)))),
   draw_cline,draw_cline,draw_cline,
  timel('EXEC',(offer_rtrace((user:Code)))).


eval(Expression, Result):- current_env(Env), eval(Expression, Env, Result).

eval(Expression, Env, Result):-
   always_catch(maybe_ltrace(lisp_compile(Env,Result,Expression,Code))), 
   always_catch(ignore(always(maybe_ltrace(call(user:Code))))),!.




flush_all_output_safe:- notrace((forall(stream_property(S,mode(write)),quietly(catch(flush_output(S),_,true))))).

read_no_parse(Expr):- flush_all_output_safe,current_input(In), read_no_parse(In,Expr).
read_no_parse(In, ExprO):- parse_sexpr_untyped(In,ExprS),(ExprS='$COMMENT'(_) -> (!,read_no_parse(In, ExprO)); ExprS=ExprO).

read_and_parse(Expr):-  flush_all_output_safe,current_input(In),read_and_parse(In, Expr).
read_and_parse(In, Expr):- read_no_parse(In, ExprS),as_sexp(ExprS,ExprS1),!,reader_intern_symbols(ExprS1,Expr),!.


eval_repl_hooks(V,_):-var(V),!,fail.
eval_repl_hooks(nil,  []):-!.

eval_repl_hooks(KW, Ret):- atom(KW),atom_concat(':',UC,KW),downcase_atom(UC,DC),atom_concat('kw_',DC,US),!,eval_repl_hooks(US,Ret).
% :cd t
eval_repl_hooks(KW, Ret):- is_keywordp(KW),to_prolog_string(KW,PStr),name(UC,PStr),downcase_atom(UC,Atom),
  user:((current_predicate(Atom/2)-> (read_prolog_object(PrologArg),call(Atom,PrologArg,Ret));
  (current_predicate(Atom/1)-> (read_prolog_object(PrologArg),!,t_or_nil(call(Atom,PrologArg),Ret));
  (current_predicate(Atom/0)-> (call(Atom)))))).

% make.  ls.  pwd. 
eval_repl_hooks(Atom, R):- atom(Atom),atom_concat_or_rtrace(_,'.',Atom),
  quietly(catch(read_term_from_atom(Atom,Term,[variable_names(Vs),syntax_errors(true)]),_,fail)),
  callable(Term),current_predicate(_,Term),b_setval('$variable_names',Vs),
  t_or_nil((user:call(Term)*->userout(yes(Term));(userout(no(Term)),fail)),R).

eval_repl_hooks([debug,A], t):- !,debug(lisp(A)).
eval_repl_hooks([nodebug,A], t):- !, nodebug(lisp(A)).

eval_repl_hooks([UC|A], R):- atom(UC),downcase_atom(UC,DC),DC\==UC,!,eval_repl_hooks([DC|A], R).

eval_repl_hooks([X], R):-!, eval_repl_atom( X, R),!.
eval_repl_hooks( X , R):- eval_repl_atom( X, R),!.

maybe_ltrace(G):- current_prolog_flag(lisp_trace,true)->rtrace(G);always(G).

eval_repl_atom(V,_):-var(V),!,fail.
eval_repl_atom(end_of_file, end_of_file):-!.
eval_repl_atom(quit, end_of_file):-!.
eval_repl_atom(prolog, t):- !, prolog.
eval_repl_atom(ltrace, t):- set_prolog_flag(lisp_trace,true),debug(lisp(trace)),debug,debugging.
eval_repl_atom(noltrace, t):- set_prolog_flag(lisp_trace,false),nodebug(lisp(trace)),nodebug,debugging.

eval_repl_atom(debug, t):- debug(lisp(_)),debug,debugging.
eval_repl_atom(nodebug, t):- nodebug(lisp(_)),nodebug,debugging.

%eval_repl_atom(make, O):- !, always((make, f_sys_compile_file_mask(pack('wam_commmon_lisp/prolog/wam_cl/lisp/'),keys([]),O))).

eval_repl_atom(UC, R):- atom(UC),downcase_atom(UC,DC),DC\==UC,eval_repl_atom(DC, R).


eval_repl_atom(show, t):- 
  listing([user:named_lambda/2,
        user:macro_lambda/5,
        user:function_lambda/4]).

lw:- f_load("wam-cl-params",_).
%:- cddd.
% invoke_eval(['in-package', "SYSTEM"], In_package_Ret):
%lisp_add_history("prolog.")
%:- lisp_add_history("lisp.").


%:- cd('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/').


:- fixup_exports.

% :- set_prolog_flag(verbose_autoload,false).

:- use_module(library(shell)).

:- initialization((in1t:lisp_repl),main).

:- if(false).
:- if(getuid(1006)).
:- use_module(library(eggdrop)).
:- initialization((do_wamcl_inits,egg_go_fg),main).

eggdrop:lisp_call([S|TERM],_Vs,R):- lisp_compiled_eval([S|TERM],R).
:- endif. % getuid(1006)
:- endif. % false
%:- process_si.
%:- cddd.


