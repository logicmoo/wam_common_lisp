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
:- set_module(class(library)).

:- set_prolog_flag(backtrace,true).
:- set_prolog_flag(backtrace_depth,500).
:- set_prolog_flag(backtrace_goal_depth,30).
:- set_prolog_flag(backtrace_show_lines,true).
:- set_prolog_flag(toplevel_print_anon,true).
:- set_prolog_flag(last_call_optimisation,false).
:- set_prolog_flag(lisp_verbose,1).
:- set_prolog_flag(lisp_autointern,true).
:- set_prolog_flag(lisp_markdown,false).
:- set_prolog_flag(lisp_exe,[]).
:- set_prolog_flag(lisp_main,[]).
:- create_prolog_flag(wamcl_init_level,0,[keep(true)]).

:- include('header').

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
  set_prolog_flag(debug,true).

set_lisp_option(optimize):- 
  set_prolog_flag(last_call_optimisation,true).


lisp:-
 do_wamcl_inits,
 lisp_goal.

lisp_goal:- is_using_lisp_main, call_lisp_main(Exit), !, main_exit(Exit).
lisp_goal:- is_making_lisp_exe, make_exe,!,lisp_goal_pt2.
lisp_goal:- lisp_goal_pt2.

lisp_goal_pt2:-   % so we can get in on a break 
  % current_prolog_flag(wamcl_init_level,7),
  current_prolog_flag(lisp_repl_goal,Else),Else\==[],call(Else),!.
lisp_goal_pt2:- repl.

call_lisp_main(Exit):- 
  get_lisp_main(Name),!,
  reader_intern_symbols(Name,Symbol),
  find_function_or_macro_name(_Ctx,_Env,Symbol,_Len, Function),
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


do_after_load(G):- assertz(wl:interned_eval(call(G))).
% set if already set
set_interactive(TF):-
 (TF -> set_prolog_flag(lisp_repl_goal,repl);
    set_prolog_flag(lisp_repl_goal,halt)).
% doesnt set if already set
imply_interactive(TF):- current_prolog_flag(lisp_repl_goal,_)->true;set_interactive(TF).
imply_flag(Name,Value):- atom_concat(lisp_,Name,LName), ((current_prolog_flag(LName,N),N\==1,N\==[])->true;set_prolog_flag(LName,Value)).



handle_all_os_program_args(ARGV):- 
  ignore((memberchk('-O',ARGV),set_lisp_option(quiet),set_lisp_option(optimize))),
  ignore((memberchk('--nodebug',ARGV),set_lisp_option(quiet))),
  ignore((memberchk('--debug',ARGV),set_lisp_option(debug))).
  

handle_all_program_args([N,V|More]):- handle_1program_arg(N=V),!,handle_all_program_args(More).
handle_all_program_args([N|More]):- handle_1program_arg(N),!,handle_all_program_args(More).
handle_all_program_args([N|More]):- exists_file(N),imply_flag(verbose,0),imply_interactive(false),!,do_after_load(((set_program_args(More),cl_load(N,_)))).
handle_all_program_args(More):- do_after_load(((set_program_args(More)))).

set_program_args(More):- maplist(to_lisp_string,More,List),set_var(ext_xx_args_xx,List).



wl:interned_eval(("(defparameter EXT:*ARGS* ())")).

handle_1program_arg(N=V):-  handle_program_args(N,_,V),!.
handle_1program_arg(N=V):-!,handle_program_args(N,_,V).
handle_1program_arg(N):- handle_program_args(N,_),!.
handle_1program_arg(N):- handle_program_args(_,N),!.

:- discontiguous handle_program_args/2. 
:- discontiguous handle_program_args/3. 

% helpfull
handle_program_args('--help','-?'):- listing(handle_program_args),show_help,imply_interactive(false).
handle_program_args('--debug','-debug'):- cl_push_new(xx_features_xx,kw_debugger),set_lisp_option(debug).
handle_program_args('--package','-p',Package):- do_after_load(cl_inpackage(Package)).
handle_program_args('--quiet','--silent'):- set_lisp_option(quiet).

% compiler
handle_program_args('--exe','-o',File):- set_prolog_flag(lisp_exe,File),imply_interactive(false).
handle_program_args('--main','-main',Main):- set_prolog_flag(lisp_main,Main),imply_interactive(false).
handle_program_args('--compile','-c',File):- do_after_load(cl_compile_file(File,[],_)),imply_interactive(false).
handle_program_args('--l','-l',File):- do_after_load(pl_load(File,[],_)),imply_interactive(false).
handle_program_args('--quit','-quit'):- set_interactive(false).

% interactive
handle_program_args('--load','-i',File):- do_after_load(pl_load(File,[],_)).
handle_program_args('--eval','-x',Form):- do_after_load(lisp_compiled_eval(Form,_)).
handle_program_args('--repl','-repl'):- set_interactive(true).
handle_program_args('--norepl','-norepl'):- set_interactive(false).
handle_program_args('--test','--markdown'):- set_prolog_flag(lisp_markdown,true).


% incomplete 
handle_program_args('--ansi','-ansi'):- cl_push_new(xx_features_xx,kw_ansi).

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


% primordial inits
primordial_init:- current_prolog_flag(wamcl_init_level,N),N>0,!.
primordial_init:- 
 set_prolog_flag(wamcl_init_level,1),
 always((
  % allows "PACKAGE:SYM" to be created on demand (could this be an initials a default)  
  set_opv(xx_package_xx, value, pkg_sys),
  ensure_env,
  set_opv(xx_package_xx,value,pkg_sys),
  set_prolog_flag(lisp_autointern,true),
  current_prolog_flag(os_argv,Y),handle_all_os_program_args(Y),
  clear_op_buffer,  
  set_prolog_flag(wamcl_init_level,2),
  current_prolog_flag(argv,Y2),handle_all_program_args(Y2),
  clear_op_buffer,
  set_prolog_flag(wamcl_init_level,3))). 

% system sourcefile load hooks
do_wamcl_inits:- current_prolog_flag(wamcl_init_level,N),N>5,!.
do_wamcl_inits:-
  primordial_init,
  set_prolog_flag(wamcl_init_level,6),
  clear_op_buffer,
  set_opv(xx_package_xx,value,pkg_user),
  set_prolog_flag(wamcl_init_level,7).

% program inits
clear_op_buffer:-   
   forall(clause(wl:interned_eval(G),Body,R),
    (forall(Body,always(do_interned_eval(G))),
     erase(R))).
clear_op_buffer.


show_help:- writeln(
'WAM-CL (https://github.com/TeamSPoon/wam_common_lisp) is an ANSI Common Lisp implementation.
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

:- set_opv(xx_package_xx,value,pkg_sys).

:- fixup_exports.

%:- set_prolog_flag(verbose_autoload,false).

%:- initialization(primordial_init,restore).
%:- initialization(do_wamcl_inits,restore).
%:- initialization(writeln(restore_goal),restore).

%:- initialization(lisp,program).
%:- initialization(lisp,main).

%:- process_si.
%:- cddd.

