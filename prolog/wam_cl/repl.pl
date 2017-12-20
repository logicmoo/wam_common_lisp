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
:- set_module(class(library)).
:- include('header').


:- dynamic 
	user:named_lambda/2,
        user:macro_lambda/3,
        user:function_lambda/4.
:- multifile 
	user:named_lambda/2,
        user:macro_lambda/3,
        user:function_lambda/4.


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
   redo_call_cleanup(trace,eval(Expression, Result),quietly),
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


lisp_banner:- 
 write('
__        ___    __  __        ____ _
\\ \\      / / \\  |  \\/  |      / ___| |
 \\ \\ /\\ / / _ \\ | |\\/| |_____| |   | |
  \\ V  V / ___ \\| |  | |_____| |___| |___
   \\_/\\_/_/   \\_\\_|  |_|      \\____|_____|
'),nl,
	write('Common Lisp, written in Prolog'),nl.

prompts(Old1,_Old2):- var(Old1) -> prompt(Old1,Old1) ; prompt(_,Old1).


classof:attr_unify_hook(A,B):- trace,wdmsg(classof:attr_unify_hook(A,B)),lisp_dump_break. %  break.

code_load_hooks:-
   reset_env,
  (current_prolog_flag(os_argv,Y)->handle_all_os_program_args(Y)),
  set_prolog_flag(lisp_autointern,true),
   forall(retract(wl:interned_eval(G)),always(do_interned_eval(G))),
  set_prolog_flag(lisp_autointern,false).                 


do_before_tpl:- code_load_hooks,
  (current_prolog_flag(argv,Y)->handle_all_program_args(Y)).
  

lisp:-
       prompt(Old, '> '),
	prompts(Old1, Old2),
	prompts('> ', '> '),
	%tidy_database,
        do_before_tpl,
        call_cleanup(repl_loop,
                     (prompt(_, Old),
                       prompts(Old1, Old2))),!.


repl_loop:- current_prolog_flag(lisp_repl_goal,Else),Else\==repl_loop,!,Else.
repl_loop:- % lisp_banner,
	repeat,
        catch(read_eval_print(Result),'$aborted',fail),
   	quietly(Result == end_of_file),!.


show_help:- writeln('
WAM-CL (https://github.com/TeamSPoon/wam_common_lisp) is an ANSI Common Lisp implementation.
Usage:  wamcl [prolog-options] [wamcl-options] [lispfile [argument ...]]

Host Prolog options:

-x state         Start from Image state (must be first)
                 (may be used to debug saved lisp EXEs)

-[LGT]size[KMG]  Specify {Local,Global,Trail} limits
[+/-]tty         Allow tty control
-O               Optimised compilation
--nosignals      Do not modify any signal handling
--nodebug        Omit generation of debug info
--version        Print the Prolog version information



WAM-CL Options:

 -?, --help    - print this help and exit

Lisp Startup actions:
 --ansi        - more ANSI CL compliance  (TODO)
 -p package    - start in the package
 -norc         - do not load the user ~/.wamclrc file   (TODO)
 -lp dir       - add dir to *LOAD-PATHS* (can be repeated)    (TODO)
 -i file       - load initfile (can be repeated)

Compiler actions put WAM-CL into a batch mode:
 -x expressions - execute the expressions (mixed into compiler actions)
 -c [-l] lispfile [-o outputfile] - compile or load a lispfile
               [--exe outputfile] - make a platform binary

Which are overridden by:

  --repl                Enter the interactive read-eval-print loop when done
  --load <filename>     File to load 
  --eval <form>         Form to eval

Default action is an interactive read-eval-print loop.

  --quit, -norepl       Exit with status code (instead) from prevous option processing.
                        Otherwise, an interactive read-eval-print loop is entered.

   "lispfile"           When "lispfile" is given, it is loaded (via --load)

Remaining arguments are placed in EXT:*ARGS* as strings.


Examples:

$PACKDIR/wam_common_lisp/prolog/wam_cl$

# creating your first image
$ swipl ../wamcl.pl --exe wamcl
# try it
$ ./wamcl
$ ./wamcl -c hello.lisp -o hello.pl --exe hello
$ ./hello world
$ swipl -x hello --repl
$ swipl hello.pl
$ swipl -x wamcl.prc
% swipl -x wamcl.prc hello.lisp world

').

:- set_prolog_flag(backtrace,true).
:- set_prolog_flag(backtrace_depth,500).
:- set_prolog_flag(backtrace_goal_depth,10).
:- set_prolog_flag(backtrace_show_lines,true).
:- set_prolog_flag(toplevel_print_anon,true).

set_lisp_option(verbose):- 
   set_prolog_flag(verbose_load,full),
   set_prolog_flag(verbose,normal),
   set_prolog_flag(verbose_autoload,true),
   set_prolog_flag(verbose_file_search,true).

set_lisp_option(quiet):- 
 set_prolog_flag(verbose,silent),
 set_prolog_flag(verbose_autoload,false),
 set_prolog_flag(verbose_load,silent),
 set_prolog_flag(verbose_file_search,false).

set_lisp_option(debug):-
  set_lisp_option(verbose),
  set_prolog_flag(debug,true).

set_lisp_option(optimize):- 
  set_prolog_flag(last_call_optimisation,true).


handle_all_os_program_args(ARGV):- 
  ignore((memberchk('-O',ARGV),set_lisp_option(quiet),set_lisp_option(optimize))),
  ignore((memberchk('--nodebug',ARGV),set_lisp_option(quiet))),
  ignore((memberchk('--debug',ARGV),set_lisp_option(debug))).
  

handle_all_program_args([N,V|More]):- handle_1program_arg(N=V),!,handle_all_program_args(More).
handle_all_program_args([N|More]):- handle_1program_arg(N),!,handle_all_program_args(More).
handle_all_program_args(More):- maplist(to_lisp_string,More,List),set_var(ext_xx_args_xx,List).

set_interactive(TF):-
 (TF -> set_prolog_flag(lisp_repl_goal,repl_loop);
    set_prolog_flag(lisp_repl_goal,halt)).

wl:interned_eval(("(defparameter EXT:*ARGS* ())")).

handle_1program_arg(N=V):-  handle_program_args(N,_,V),!.
handle_1program_arg(N=V):-!,handle_program_args(N,_,V).
handle_1program_arg(N):- handle_program_args(N,_),!.
handle_1program_arg(N):- handle_program_args(_,N),!.

:- discontiguous handle_program_args/2. 
:- discontiguous handle_program_args/3. 

% helpfull
handle_program_args('--help','-?'):- listing(handle_program_args),show_help,set_interactive(false).
handle_program_args('--debug','-debug'):- cl_push_new(xx_features_xx,kw_debugger).
handle_program_args('--package','-p',Package):- cl_inpackage(Package).

% compiler
handle_program_args('--exe','-o',File):- qsave_program(File),set_interactive(false).
handle_program_args('--compile','-c',File):- cl_compile_file(File,[],_),set_interactive(false).
handle_program_args('--l','-l',File):- cl_load(File,[],_),set_interactive(false).
handle_program_args('--quit','-norepl'):- set_interactive(false).

% interactive
handle_program_args('--load','-i',File):- set_interactive(true), cl_load(File,[],_).
handle_program_args('--eval','-x',Form):- set_interactive(true), lisp_compiled_eval(Form,_).
handle_program_args('--repl','-repl'):- set_interactive(true).

% incomplete 
handle_program_args('--ansi','-ansi'):- cl_push_new(xx_features_xx,kw_ansi).

tidy_database:-
	nb_delete('$env_current'),
        nb_delete('$env_global'),
        current_env(_Env),
	retractall(lambda(_, _)).

show_uncaught_or_fail((A,B)):-!,show_uncaught_or_fail(A),show_uncaught_or_fail(B).
show_uncaught_or_fail(G):- quietly(flush_all_output_safe),
  (catch(G,E,quietly((wdmsg(uncaught(E)),rtrace(G),!,fail)))*->true;quietly((wdmsg(failed(G)),!,fail))).

set_prompt_from_package:-
  lquietly((ignore((get_var(_ReplEnv, xx_package_xx, Package),
        short_package_or_hash(Package,Name0),
        (Name0=="U"->Name="CL-USER";Name=Name0),
        always(nonvar(Name)),
        atom_concat(Name,'> ',Prompt),
        prompt(_,Prompt))))).

read_eval_print(Result):-		% dodgy use of cuts to force a single evaluation
        set_prompt_from_package,

        %lquietly
        (show_uncaught_or_fail(read_no_parse(Expression))),!,
        lquietly(show_uncaught_or_fail(lisp_add_history(Expression))),!,
        nb_linkval('$mv_return',[Result]),
        show_uncaught_or_fail(eval_at_repl(Expression,Result)),!,
        lquietly(show_uncaught_or_fail(write_results(Result))),!.
	

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
lisp_add_history(Expression):- is_stringp(Expression),!,
       to_prolog_string(Expression,ExpressionS),
        prolog:history(user_input, add(ExpressionS)).
lisp_add_history(Expression):-
        with_output_to(string(S),writeExpression(Expression)),
        (string_upper(S,S)->string_lower(S,Store);Store=S),
        prolog:history(user_input, add(Store)).

:- set_prolog_flag(lisp_no_compile,false).

% basic EVAL statements for built-in procedures
eval_at_repl(Var,  R):- quietly(var(Var)),!, R=Var.
eval_at_repl(Expression, Result):- lquietly(eval_repl_hooks(Expression,Result)),!.
eval_at_repl(Expression,Result):- notrace(tracing), !, call_cleanup(eval_at_repl_tracing(Expression,Result),trace).
eval_at_repl(Expression,Result):-
  lquietly(as_sexp(Expression,SExpression)),
  (reader_intern_symbols(SExpression,LExpression)),
  quietly(dbmsg_cmt(:- lisp_compiled_eval(LExpression))),
  quietly(debug_var('ReplEnv',Env)),
  timel('COMPILER',always_catch(maybe_ltrace(lisp_compile(Env,Result,LExpression,Code)))),
  quietly(dbmsg_real(:-Code)),
  (notrace(tracing)-> (user:Code) ; 
   timel('EXEC',always_catch(ignore(always(maybe_ltrace(call(user:Code))))))),!.

eval_at_repl_tracing(Expression,Result):-
 notrace,
  lquietly(as_sexp(Expression,SExpression)),
  (reader_intern_symbols(SExpression,LExpression)),
  writeq((reader_intern_symbols(SExpression,LExpression))),nl,
  quietly(debug_var('ReplEnv',Env)),
  %quietly(cls),
   quietly((writeln(==================================================================))),
   quietly((writeln(==================================================================))),
   quietly((writeln(==================================================================))),
   dbmsg(:- lisp_compiled_eval(LExpression)),
   quietly((writeln(==================================================================))),
   quietly((writeln(==================================================================))),
   quietly((writeln(==================================================================))),
  timel('COMPILE',(offer_rtrace(lisp_compile(Env,Result,LExpression,Code)))),
  % quietly(cls),
   quietly((writeln(==================================================================))),
   quietly((writeln(==================================================================))),
   quietly((writeln(==================================================================))),
   show_call_trace((dbmsg_real(:-Code))),
   quietly((writeln(==================================================================))),
   timel('PREEXEC',(offer_rtrace((user:Code)))),
   quietly((writeln(==================================================================))),
   quietly((writeln(==================================================================))),
   quietly((writeln(==================================================================))),
  timel('EXEC',(offer_rtrace((user:Code)))).

eval(Expression, Result):- current_env(Env), eval(Expression, Env, Result).

eval(Expression, Env, Result):-
   always_catch(maybe_ltrace(lisp_compile(Env,Result,Expression,Code))), 
   always_catch(ignore(always(maybe_ltrace(call(user:Code))))),!.


/*:- if(exists_source(library(sexpr_reader))).
:- use_module(library(sexpr_reader)).
:- endif.
*/
read_and_parse(Expr):-  flush_all_output_safe,current_input(In),parse_sexpr_untyped_read(In, Expr).
read_no_parse(Expr):-  flush_all_output_safe,current_input(In),parse_sexpr_untyped(In, Expr).

flush_all_output_safe:- forall(stream_property(S,mode(write)),quietly(catch(flush_output(S),_,true))).

parse_sexpr_untyped_read(In, Expr):- 
  parse_sexpr_untyped(In,ExprS),!,
  as_sexp(ExprS,ExprS1),!,reader_intern_symbols(ExprS1,Expr).


eval_repl_hooks(V,_):-var(V),!,fail.
eval_repl_hooks(nil,  []):-!.

eval_repl_hooks(KW, Ret):- atom(KW),atom_concat(':',UC,KW),downcase_atom(UC,DC),atom_concat('kw_',DC,US),!,trace,!,eval_repl_hooks(US,Ret).
% :cd t
eval_repl_hooks(KW, Ret):- is_keywordp(KW),to_prolog_string(KW,PStr),name(UC,PStr),downcase_atom(UC,Atom),
  
  (current_predicate(Atom/2)-> (read_prolog_object(PrologArg),call(Atom,PrologArg,Ret));
  (current_predicate(Atom/1)-> (trace,read_prolog_object(PrologArg),!,t_or_nil(call(Atom,PrologArg),Ret));
  (current_predicate(Atom/0)-> (call(Atom))))).

% make.  ls.  pwd. 
eval_repl_hooks(Atom, R):- atom(Atom),atom_concat_or_rtrace(_,'.',Atom),quietly(catch(read_term_from_atom(Atom,Term,[variable_names(Vs),syntax_errors(true)]),_,fail)),
  callable(Term),current_predicate(_,Term),b_setval('$variable_names',Vs),t_or_nil((user:call(Term)*->dmsg(Term);(dmsg(no(Term)),fail)),R).

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

%eval_repl_atom(make, O):- !, always((make, cl_compile_file_mask(pack('wam_commmon_lisp/prolog/wam_cl/lisp/'),keys([]),O))).

eval_repl_atom(UC, R):- atom(UC),downcase_atom(UC,DC),DC\==UC,eval_repl_atom(DC, R).


eval_repl_atom(show, t):- 
  listing([user:named_lambda/2,
        user:macro_lambda/5,
        user:function_lambda/4]).

lw:- cl_load("wam-cl-params",_).
%:- cddd.
% invoke_eval(['in-package', "SYSTEM"], In_package_Ret):
%lisp_add_history("prolog.")
%:- lisp_add_history("lisp.").


%:- cd('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/').


:- fixup_exports.

:- set_prolog_flag(verbose_autoload,false).
%:- initialization(lisp,restore).
:- initialization(code_load_hooks).
:- initialization(lisp_banner).

:- initialization(lisp,main).
%:- initialization((lisp,prolog),main).

%:- process_si.
%:- cddd.

