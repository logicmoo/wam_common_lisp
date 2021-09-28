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
:- module(in1t, []).

:- meta_predicate show_must(0).
:- meta_predicate without_gc(0).
:- meta_predicate set_interactive(0).
:- meta_predicate imply_interactive(0).

:- use_module(library(backcomp)).

:- set_prolog_flag(backtrace,true).
:- set_prolog_flag(backtrace_depth,500).
%:- set_prolog_flag(backtrace_goal_depth,30).
:- set_prolog_flag(backtrace_show_lines,true).
:- set_prolog_flag(toplevel_print_anon,true).
:- set_prolog_flag(last_call_optimisation,false).
:- set_prolog_flag(lisp_verbose,1).
:- set_prolog_flag(lisp_primordial,starting).
:- set_prolog_flag(lisp_markdown,false).
:- set_prolog_flag(lisp_exe,[]).
:- set_prolog_flag(lisp_main,[]).
:- create_prolog_flag(wamcl_init_level,0,[keep(true)]).

set_lisp_option(verbose):- 
   set_prolog_flag(verbose_load,full),
   set_prolog_flag(verbose,normal),
   set_prolog_flag(verbose_autoload,true),
   set_prolog_flag(verbose_file_search,true),
   set_prolog_flag(lisp_verbose,3).


set_lisp_option(quiet):- 
 set_prolog_flag(verbose,silent),
 set_prolog_flag(verbose_autoload,false),
 set_prolog_flag(verbose_load,silent),
 set_prolog_flag(verbose_file_search,false),
 set_prolog_flag(lisp_verbose,0).

set_lisp_option(debug):-
  set_lisp_option(verbose),
  set_prolog_flag(debug,true),
  set_prolog_flag(lisp_pre_goal,rtrace).

set_lisp_option(optimize):- 
  set_prolog_flag(last_call_optimisation,true).

lisp:- lisp_repl.

lisp_repl:- 
 must(do_wamcl_inits), 
 must(lisp_goal).

lisp_goal:- is_using_lisp_main, call_lisp_main(Exit), !, main_exit(Exit).
lisp_goal:- is_making_lisp_exe, lisp_goal_pt1, make_exe,!,lisp_goal_pt2.
lisp_goal:- lisp_goal_pt1, lisp_goal_pt2.

lisp_goal_pt1:-   % so we can add debug switches?
  current_prolog_flag(lisp_pre_goal,Else),Else\==[],
  call(Else),!.
lisp_goal_pt1:- !.

lisp_goal_pt2:-   % so we can get in on a break % current_prolog_flag(wamcl_init_level,7),
  current_prolog_flag(lisp_repl_goal,Else),Else\==[],
  call(Else),!.

%lisp_goal_pt2:- repl.
lisp_goal_pt2:- !.

call_lisp_main(Exit):- 
  get_lisp_main(Name),!,
  reader_intern_symbols(Name,Symbol),
  find_lisp_function(Symbol,_,Function),
  call(Function,Exit).

main_exit(N):- integer(N),!,halt(N).
main_exit([]):- !,halt(1).
main_exit(_):-halt(0). 

get_lisp_main(Name):- current_prolog_flag(lisp_main,Name),Name\==[].

is_using_lisp_main:- current_prolog_flag(lisp_main,Name),Name\==[], \+ is_making_lisp_exe.

is_making_lisp_exe:- current_prolog_flag(lisp_exe,File),File\==[].

make_exe :- 
  always(is_making_lisp_exe),
  current_prolog_flag(lisp_exe,File),
  % so when we load the program we dont just make another exe
  set_prolog_flag(lisp_exe,[]),
  set_prolog_flag(wamcl_init_level, 0),
 % set_prolog_flag(lisp_repl_goal, []),
  set_prolog_flag(os_argv, [swipl]),
  set_prolog_flag(argv, []),
  qsave_program(File,[goal((true)),autoload(false)]).


:- set_prolog_flag(lisp_pre_goal,true).

do_after_load(G):- assertz(wl:interned_eval(call(G))).
% set if already set
set_interactive(TF):-
 (TF -> set_prolog_flag(lisp_repl_goal,repl);
    set_prolog_flag(lisp_repl_goal,true)).
% doesnt set if already set
imply_interactive(TF):- current_prolog_flag(lisp_repl_goal,_)->true;set_interactive(TF).
imply_flag(Name,Value):- atom_concat(lisp_,Name,LName), ((current_prolog_flag(LName,N),N\==1,N\==[])->true;set_prolog_flag(LName,Value)).



handle_all_os_program_args(ARGV):- 
  ignore((memberchk('-O',ARGV),set_lisp_option(quiet),set_lisp_option(optimize))),
  ignore((memberchk('--nodebug',ARGV),set_lisp_option(quiet))),
  ignore((memberchk('--debug',ARGV),set_lisp_option(debug))).
  

handle_all_program_args([N,V|More]):- handle_1program_arg(N=V),!,handle_all_program_args(More).
handle_all_program_args([N|More]):- handle_1program_arg(N),!,handle_all_program_args(More).
handle_all_program_args([N|More]):- 
 exists_file(N),imply_flag(verbose,0),imply_interactive(false),!,
 do_after_load(((set_program_args(More),f_load(N,_)))).
handle_all_program_args(More):- do_after_load(((set_program_args(More)))).

set_program_args(More):- maplist(to_lisp_string,More,List),set_var(sys_xx_args_xx,List).



wl:interned_eval(("(defparameter EXT:*ARGS* ())")).

handle_1program_arg(N=V):-  handle_program_args(N,_,V),!.
handle_1program_arg(N=V):-!,handle_program_args(N,_,V).
handle_1program_arg(N):- handle_program_args(N,_),!.
handle_1program_arg(N):- handle_program_args(_,N),!.

:- discontiguous handle_program_args/2. 
:- discontiguous handle_program_args/3. 

% helpfull
handle_program_args('--help','-?'):- listing(handle_program_args),show_help,imply_interactive(false).
handle_program_args('--debug','-debug'):- nop(pl_pushnew(xx_features_xx,kw_debugger)),set_lisp_option(debug).
handle_program_args('--package','-p',Package):- do_after_load(f_in_package(Package)).
handle_program_args('--quiet','--silent'):- set_lisp_option(quiet).

% compiler
handle_program_args('--exe','-o',File):- set_prolog_flag(lisp_exe,File),imply_interactive(false).
handle_program_args('--main','-main',Main):- set_prolog_flag(lisp_main,Main),imply_interactive(false).
handle_program_args('--compile','-c',File):- do_after_load(f_compile_file(File,[],_)),nop(imply_interactive(false)).
handle_program_args('--l','-l',File):- do_after_load(pl_load(File,[],_)). % ,imply_interactive(false).
handle_program_args('--quit','-quit'):- set_interactive(false).

% interactive
handle_program_args('--load','-i',File):- do_after_load(pl_load(File,[],_)).
handle_program_args('--eval','-x',Form):- do_after_load(lisp_compiled_eval(Form,_)).
handle_program_args('--repl','-repl'):- set_interactive(true).
handle_program_args('--norepl','-norepl'):- set_interactive(false).
handle_program_args('--test','--markdown'):- set_prolog_flag(lisp_markdown,true).
handle_program_args('--sock','--lispsock',Form):- atom_number(Form,Port)->start_lspsrv(repl,Port,"Lisp Repl");start_lspsrv(repl,3301,"Lisp Repl").



% incomplete 
handle_program_args('--ansi','-ansi'):- pl_pushnew(xx_features_xx,kw_ansi).

pl_pushnew(Symbol,Item):- f_symbol_value(Symbol,Old),f_adjoin(Item,Old,[],New),f_sys_set_symbol_value(Symbol,New).

tidy_database:-
        reset_env,
        ensure_env(_Env).


% Called after primordial init 
%lisp_banner:- current_prolog_flag(lisp_verbose,0),!.
lisp_banner:- 
 write('
__        ___    __  __        ____ _
\\ \\      / / \\  |  \\/  |      / ___| |
 \\ \\ /\\ / / _ \\ | |\\/| |_____| |   | |
  \\ V  V / ___ \\| |  | |_____| |___| |___
   \\_/\\_/_/   \\_\\_|  |_|      \\____|_____|
'),nl, write('Common Lisp, written in Prolog'),nl.

:- module_transparent(without_gc/1).
without_gc(Goal):-
  current_prolog_flag(gc,Was),
  set_prolog_flag(gc, true),
  garbage_collect,
  trim_stacks,
  cleanup_strings,
  garbage_collect_atoms,
  garbage_collect_clauses,
  set_prolog_flag(gc, false),
  call_cleanup(Goal,set_prolog_flag(gc, Was)).

% primordial inits
primordial_init:- current_prolog_flag(wamcl_init_level,N),N>0,!.
primordial_init:- 
 set_prolog_flag(wamcl_init_level,1),
   set_prolog_flag(logicmoo_message_hook, break),   
  without_gc(must_det_l((
  % allows "PACKAGE:SYM" to be created on demand (could this be an initials a default)  
  set_opv(xx_package_xx,symbol_value, pkg_sys),
  ensure_env,
  set_opv(xx_package_xx,symbol_value,pkg_sys),
  set_prolog_flag(lisp_primordial,true),  
  current_prolog_flag(os_argv,Y),handle_all_os_program_args(Y),  
  show_must(set_opv(sym('sys:*LISP-HOME*'), symbol_value, ".")),  
  (clear_op_buffer),
  set_prolog_flag(wamcl_init_level,2),
  current_prolog_flag(argv,Y2),handle_all_program_args(Y2),
  (clear_op_buffer),
  set_prolog_flag(wamcl_init_level,3)))).
 

%must_or_rtrace(X):- on_x_rtrace(X)->true;rtrace(X).

% system sourcefile load hooks
do_wamcl_inits:- current_prolog_flag(wamcl_init_level,N),N>5,!.
do_wamcl_inits:-
 set_prolog_flag(logicmoo_message_hook, break),
 current_prolog_flag(access_level, W),
 without_gc((setup_call_cleanup(set_prolog_flag(access_level, system),
 call_each(must_or_rtrace,
 ((  
  
  primordial_init,
  set_prolog_flag(wamcl_init_level,6),  
  clear_op_buffer,
  set_opv(xx_package_xx,symbol_value,pkg_user),
  set_prolog_flag(wamcl_gvars,true),
  set_prolog_flag(wamcl_init_level,7)))),
 set_prolog_flag(access_level, W)))).

:- ensure_loaded(eightball).
:- include(header).

% program inits
clear_op_buffer:- % trace('8ball':always/1,[+call,+exit,+fail]),
   forall((clause(wl:interned_eval(G),Body,R),dmsg(wl:interned_eval(G):-Body)),
         (forall((show_must((Body))),(show_must((do_interned_eval(G))))),
           erase(R))),
   fail.
clear_op_buffer:- % trace('8ball':always/1,[-call,-exit,+fail]), 
   dmsg(clear_op_buffer_complete),!.
clear_op_buffer:- true.

show_must(true):-!.

show_must(G):-dmsg(doing(G)),!, (must_or_rtrace(G)*->dmsg(did(G));must(G)).
show_must(G):-dmsg(doing(G)),(catch(G,Err,(dmsg(Err->G),rtrace(G)))*->dmsg(did(G));must(G)).

show_help:- writeln(
'WAM-CL (https://github.com/logicmoo/wam_common_lisp) is an ANSI Common Lisp implementation.
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
 -c [-l] lispfile [--main pkg::symbol] - compile or load a lispfile
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

$PACKDIR/wam_common_lisp/t$

# creating your first image
$ swipl ../prolog/wamcl.pl --exe wamcl
# try it
$ ./wamcl --repl
$ swipl ../prolog/wamcl.pl --compile hello.lisp --main sys::main --exe hello
$ ./hello world
$ swipl -x hello --repl
$ swipl hello.pl
$ swipl -x wamcl.prc
% swipl -x wamcl.prc hello.lisp world

').

:- set_opv(xx_package_xx,symbol_value,pkg_sys).

:- fixup_exports.

:- include('./header').


%:- set_prolog_flag(verbose_autoload,false).

%:- initialization(primordial_init,restore).
%:- initialization(do_wamcl_inits,restore).
%:- initialization(writeln(restore_goal),restore).

%:- initialization(lisp,program).
%:- initialization(lisp,main).

%:- process_si.
%:- cddd.

