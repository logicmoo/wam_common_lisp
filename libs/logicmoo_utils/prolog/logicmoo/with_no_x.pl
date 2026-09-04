/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File 'with_no_x.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles logicmoo@gmail.com ;
% Version: 'with_no_x.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_with_assertions.pl
:- module(with_no_x,[ with_no_x/1, with_no_xdbg/1, no_x_flags/0, with_no_x_flag/2, no_xdbg_flags/0, with_no_xdbg_flag/2]).
/** <module> Utility LOGICMOO WITH NO X
Suspends use of X Windows temporarily for headless code. 

- @author Douglas R. Miles
- @license LGPL 
*/

:- meta_predicate with_no_x(:).
:- meta_predicate with_no_xdbg(:).

:- thread_local(tlbugger:show_must_go_on/1).
% WAS OFF  :- system:use_module(library(gui_tracer)).

%% with_no_x( :Goal) is nondet.
%
% Using No X.
%

with_no_x_flag(gui_tracer,false).
with_no_x_flag(nodebugx,true).
with_no_x_flag(xpce,false).
with_no_x_flag(no_sandbox,true).

no_x_flags:- forall(with_no_x_flag(X,V),set_prolog_flag(X,V)).

with_no_x(G):- getenv('DISPLAY',DISP),!,call_cleanup((unsetenv('DISPLAY'),with_no_x(G)),setenv('DISPLAY',DISP)).
with_no_x(G):- with_no_x_flag(X,V),current_prolog_flag(X,Was),Was\==V,!,
 call_cleanup((set_prolog_flag(X,V),with_no_x(G)),set_prolog_flag(X,Was)).
with_no_x(G):- locally_each(tlbugger:show_must_go_on(true),call(G)).

with_no_xdbg_flag(gui_tracer, false).
with_no_xdbg_flag(backtrace,false).
with_no_xdbg_flag(debug_threads,false).
with_no_xdbg_flag(debug,false).
with_no_xdbg_flag(report_error,false).
with_no_xdbg_flag(debug_on_error,false).
%with_no_xdbg_flag(autoload,true).
with_no_xdbg_flag(trace_gc,false).
with_no_xdbg_flag(debug_term_position,false).
with_no_xdbg_flag(warn_override_implicit_import,false).

with_no_xdbg_flag(verbose_file_search,false).
with_no_xdbg_flag(verbose_autoload,false).
with_no_xdbg_flag(verbose_load,false).
with_no_xdbg_flag(verbose,silent).

%with_no_xdbg_flag(unknown,warning).

no_xdbg_flags:- forall(with_no_xdbg_flag(X,V),set_prolog_flag(X,V)).

with_no_xdbg(G):- tracing,!,call_cleanup((notrace,with_no_xdbg(G)),trace).
with_no_xdbg(G):- with_no_xdbg_flag(X,V),
  current_prolog_flag(X,Was),Was\==V,!,call_cleanup((set_prolog_flag(X,V),with_no_xdbg(G)),set_prolog_flag(X,Was)).
with_no_xdbg(G):- with_no_x(G).


