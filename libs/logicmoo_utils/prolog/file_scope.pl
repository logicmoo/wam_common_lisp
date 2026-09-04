/* Part of LogicMOO Base logicmoo_util_bb_env
% Provides a prolog database *env*
% ===================================================================
% File 'logicmoo_util_clause_expansion.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_clause_expansion.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2016/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_body_file_scope.pl
:- module(file_scope,[]).
:- use_module(logicmoo_startup).
:- define_into_module([
          loading_source_file/1,
          assert_until_eof/2,
          assert_until_eof/1,
          asserta_until_eof/2,
          asserta_until_eof/1,
          set_prolog_flag_until_eof/2,
          call_on_eom/1, 
          loading_source_file0/1,
          call_on_eof/1, 
          call_on_eof/2, 
          disable_in_file/1,
          is_current_source_term/1,
          enable_in_file/1,
          is_file_enabling/1,  
          do_eof_actions/2,
          signal_eom/1,
          signal_eof/1, 
          signal_eof/0,
          signal_eom/1, 
          check_skip_id/3,
          set_skip_file_expansion/2,
          filescope_did/1,
          contains_f/2,
          contains_eq/2,
          %cfunctor/3,
          term_expansion_option/3,
          add_did_id/3,
          notice_file/3
          ]).


/** <module> Utility LOGICMOO FILE SCOPE
This module allows changes to prolog state to be maintained and to only happen locally to a file module. 

- @author Douglas R. Miles
- @license LGPL 


Prolog compile-time and runtime source-code "Settings"

 This module specifies a set of settings for files as they are read from a file before they are processed by the compiler.

The toplevel is expand_clause/2.  This uses other translators:

	* Conditional compilation
	* clause_expansion/2 rules provided by the user

Note that this ordering implies that conditional compilation directives
cannot be generated  by  clause_expansion/2   rules:  they  must literally
appear in the source-code.


Prolog Flag Scopes:

Module,
File,
Thread.

*/
%:-must(forall(retract(at_eof_action(CALL)),must(CALL))).
% :-must((asserta((user:term_expansion(A,B):-cyc_to_clif_notify(A,B),!),CLREF),asserta(at_eof_action(erase(CLREF))))).

:- set_module(class(library)).

/*
:- multifile '$exported_op'/3. 
:- dynamic '$exported_op'/3. 
:- discontiguous '$exported_op'/3. 
'$exported_op'(_,_,_):- fail.
*/

:- multifile '$pldoc'/4. 
:- dynamic '$pldoc'/4. 
:- discontiguous '$pldoc'/4. 
'$pldoc'(_,_,_):- fail.

:- multifile '$autoload'/3. 
:- discontiguous '$autoload'/3.
:- dynamic '$autoload'/3.
'$autoload'(_,_,_):- fail.

%:- system:reexport(library(debug),[debug/3]).
%:- system:reexport(library(debuggery/bugger)).

%:- system:reexport(library(must_sanity)).
:- if( \+ current_predicate(nop/1)).
system:nop(_).
:- export(system:nop/1).
:- endif.
:- meta_predicate l_once(0).
system:qdmsg(_):- current_prolog_flag(dmsg_level,never),!.
system:qdmsg(M):-compound(M),cfunctor(M,F,_),!,debug(logicmoo(F),'~q',[M]).
system:qdmsg(M):-debug(logicmoo(M),'QMSG: ~q',[M]).
:- export(system:qdmsg/1).



/*
cfunctor(A,B,C):- compound(A)->compound_name_arity(A,B,C);functor(A,B,C).
:- system:import(cnas/3).
:- system:import(cfunctor/3).
:- system:export(cfunctor/3).
*/

is_current_source_term(H):- notrace(is_current_source_term0(H)).
is_current_source_term0((H:-B)):- !, (is_current_source_term1((H:-B))->true; (B==true -> is_current_source_term1(H))),!.
is_current_source_term0((H)):- is_current_source_term1(H) -> true ; is_current_source_term1((H:-true)).
is_current_source_term1(In):-
    prolog_load_context('term',Term), % dmsg(Term=In),
    (Term==In ; Term=@=In).

 
:- use_module(library(occurs)).

% % % OFF :- system:use_module(library(must_sanity)).
% % % OFF :- system:use_module(library(logicmoo/misc_terms)).

contains_eq(USub,Term):- sub_term(Sub,Term),USub=@=Sub.
contains_f(F,Term):- sub_term(Sub,Term),(compound(Sub)->compound_name_arity(Sub,F,_);(callable(Sub)-> functor(Sub,F,_))).

:- thread_local('$file_scope':opened_file/2).

:- meta_predicate(call_on_eof(:)).
:- meta_predicate(call_on_eof(+,:)).
:- meta_predicate(assert_until_eof(:)).
:- meta_predicate(assert_until_eof(+,:)).
:- meta_predicate(asserta_until_eof(:)).
:- meta_predicate(asserta_until_eof(+,:)).
:- meta_predicate(loading_source_file(-)).
:- module_transparent((
          assert_until_eof/2,
          assert_until_eof/1,
          call_on_eom/1, 
          call_on_eof/1, 
          call_on_eof/2, 
          disable_in_file/1,
          notice_file/3,
          enable_in_file/1,
          is_file_enabling/1,
          loading_source_file/1,
          set_prolog_flag_until_eof/2,
          do_eof_actions/2,
          signal_eof/0,
          signal_eof/1, 
          signal_eom/1, 
          check_skip_id/3,
          filescope_did/1,
          contains_f/2,
          contains_eq/2,
          term_expansion_option/3,
          add_did_id/3)).


:- thread_local(t_l:pretend_loading_file/1).

file_local_flag(dialect_pfc).
file_local_flag(subclause_expansion).

 % :- set_prolog_flag(subclause_expansion,default).
 % :- set_prolog_flag(dialect_pfc,default).

begin_file_scope :- loading_source_file(File),begin_file_scope(File).

begin_file_scope(File):- (\+ prolog_load_context(source,File); \+ prolog_load_context(file,File)),!.
begin_file_scope(File):- nop(dmsg(begin_file_scope(File))),
  forall(file_local_flag(Flag),
     ignore((current_prolog_flag(Flag,Value), call_on_eof(File,set_prolog_flag(Flag,Value))))).
  

%% loading_source_file( ?File) is det.
%
% Loading Source File.
%

loading_source_file(File):- must(loading_source_file0(File0)),!,File=File0.

:- export(loading_source_file0/1).

% end_loading_source_file(File):-  prolog_load_context(file,File).

% This is the main file to ensure we only process signal_eof directive at the end of the actual source files
loading_source_file0(File):- t_l:pretend_loading_file(File).
loading_source_file0(File):- prolog_load_context(source,File), prolog_load_context(file,File),!.
loading_source_file0(File):- prolog_load_context(source,File), prolog_load_context(file,IFile),IFile\==File,!.
loading_source_file0(File):- prolog_load_context(source,File). % maybe warn the above didnt catch it
loading_source_file0(File):- prolog_load_context(file,File),dumpST, break.
loading_source_file0(File):- loading_file(File).
loading_source_file0(File):- '$current_source_module'(Module),module_property(Module, file(File)).
loading_source_file0(File):- 'context_module'(Module),module_property(Module, file(File)).
loading_source_file0(File):- '$current_typein_module'(Module),module_property(Module, file(File)).
loading_source_file0(unknown).


:- dynamic(t_l:eof_hook/2).
:- thread_local(t_l:eof_hook/2).
:- export(t_l:eof_hook/2).


:- multifile(user:global_eof_hook/3).
:- dynamic(user:global_eof_hook/3).
:- export(user:global_eof_hook/3).


% trace_if_debug:- flag_call(runtime_debug > true) -> trace;true.

%% signal_eof() is det.
%
% Do End Of File Actions for current File.
%
signal_eof:- must(prolog_load_context(file,F);source_location(F,_)),signal_eof(F).

%% signal_eof(+File) is det.
%
% Do End of file actions queued for File.
%
signal_eof(File):- must(prolog_load_context(module,Module)),do_eof_actions(Module,File),fail.
signal_eof(File):- prolog_load_context(source,File), must(prolog_load_context(module,Module)), must(signal_eom(Module)),!.
signal_eof(_).

%% signal_eom(+Module) is det.
%
% Do End Of Module Actions
%
signal_eom(_Module):- !.
signal_eom(Module):- 
  module_property(Module,file(File)),prolog_load_context(file,File),
    \+ \+ ((module_property(Module,file(LittleFile)),LittleFile\==File)),
   must((forall(module_property(Module,file(LittleFile)),must(do_eof_actions(Module,LittleFile))))),fail.
signal_eom(Module):- must(prolog_load_context(module,Module)),
  % dmsg(info(load_mpred_file_complete(Module:File))),
   GETTER=t_l:eof_hook(_File,TODO),
   must((forall(clause(GETTER,Body,Ref),(qdmsg(found_eom_hook(GETTER:-Body)),
        doall((forall(Body,  ((qdmsg(do_eof_hook(on_f_log_ignore(Module,GETTER))),
        show_failure(eom_action(Module),on_f_log_ignore(Module,TODO))))))),ignore(erase(Ref)))))),fail.
signal_eom(Module):- nop(dmsg(signal_eom(Module))),!.


%% do_eof_actions(+Module,+File) is det.
%
% Do End Of File Actions for Module+File.
%

do_eof_actions(Module,File):-
   qdmsg(info(load_mpred_file_complete(Module:File))),
   GETTER=user:global_eof_hook(WasM,File,TODO),   
   must((forall(clause(GETTER,Body,_Ref),(qdmsg(found_eof_hook(GETTER:-Body)),
        doall((forall(Body,  ((qdmsg(call_eof_hook(on_f_log_ignore(Module,GETTER))),
        show_failure(signal_eom(Module),on_f_log_ignore(WasM,TODO))))))))))),fail.

do_eof_actions(Module,File):- must(prolog_load_context(module,Module)),
   qdmsg(info(load_mpred_file_complete(Module:File))),
    GETTER=t_l:eof_hook(File,TODO),

    must((forall(clause(GETTER,Body,Ref),(qdmsg(found_eof_hook(GETTER:-Body)),
         doall((forall(Body,  ((qdmsg(call_eof_hook(on_f_log_ignore(Module,GETTER))),
         show_failure(signal_eom(Module),on_f_log_ignore(Module,TODO))))))),ignore(erase(Ref)))))),fail.
do_eof_actions(Module,File):- nop(dmsg(do_eof_actions(Module,File))),!.



%% call_on_eof( ?Call) is det.
%
%  Whenever at End Of File execute Call
%
call_on_eof(Call):- loading_source_file(File)-> call_on_eof(File,Call).
call_on_eof(File,Call):- strip_module(Call,Module,P),
    % install our term expander
    prolog_load_context(module,LoadingModule),
    LoadingModule:use_module(library(file_scope)),
    asserta(t_l:eof_hook(File,Module:P)),

   %sanity(must(File \== '/home/prologmud_server/lib/swipl/pack/pfc/prolog/pfc2.0/mpred_header.pi')),

   must(Module\==logicmoo_util_with_assertions),
   qdmsg(eof_hook(register,Module,File,P)).

call_on_eom(Call):- must(prolog_load_context(source,File)),call_on_eof(File,Call).


%% assert_until_eof( ?Fact) is det.
%
% Assert Until Eof.
%
assert_until_eof(Fact):- must_det_l((loading_source_file(File),assert_until_eof(File,Fact))).

assert_until_eof(File,Fact):-  
  qdmsg(eof_hook(assert_until_eof,File,Fact)),
  must_det_l((assertz(Fact,Ref),call_on_eof(File,erase(Ref)))).

asserta_until_eof(Fact):- must_det_l((loading_source_file(File),asserta_until_eof(File,Fact))).

asserta_until_eof(File,Fact):-  
  qdmsg(eof_hook(asserta_until_eof,File,Fact)),
  must_det_l((asserta(Fact,Ref),call_on_eof(File,erase(Ref)))).

%% set_prolog_flag_until_eof(+FlagName,+Value) is det.
%
% Assert Until Eof.
%

% set_prolog_flag_until_eof(FlagName,Value):- current_prolog_flag(FlagName,Value),!.
set_prolog_flag_until_eof(FlagName,Value):- \+ current_prolog_flag(FlagName,_),!,
       qdmsg(warn(no_previous_value(set_prolog_flag_until_eof(FlagName,Value)))),
       set_prolog_flag(FlagName,Value).

set_prolog_flag_until_eof(FlagName,Value):- 
 (current_prolog_flag(FlagName,PrevValue)->true;PrevValue=unknown_error_value),
 qdmsg(eof_hook(set_prolog_flag_until_eof,FlagName,Value)),
   call_on_eof(set_prolog_flag(FlagName,PrevValue)),
   set_prolog_flag(FlagName,Value).

:- dynamic(lmfs_data:file_option/1).

file_option_to_db(Option,DB):- 
  retractall(lmfs_data:file_option(Option)),
  asserta(lmfs_data:file_option(Option)),
  loading_source_file(File),!,
  DBP=..[Option,File],
  DB=t_l:DBP,
  MFA=t_l:Option/1,
  ( predicate_property(DB,defined)->true;(thread_local(MFA),volatile(MFA))).

enable_in_file(Option):- file_option_to_db(Option,DB),assert_if_new(DB),set_prolog_flag_until_eof(Option,true).

disable_in_file(Option):- file_option_to_db(Option,DB),retractall(DB),set_prolog_flag_until_eof(Option,false).

is_file_enabling(Option):- file_option_to_db(Option,DB),call(DB),!, \+ current_prolog_flag(Option,false).



check_skip_id(Option,_ ,(filescope_did(List),_)):- compound(List),!,member(Option,List).
check_skip_id(_,Head ,Body):- ( \+ compound(Body) ; \+ compound(Head)),!.

add_did_id((filescope_did(List),Body),Option,(filescope_did(List2),Body)):- compound(List),
   ( memberchk(Option,List)->true;List2=[Option|List] ).
add_did_id(NewBody,Option,(filescope_did([Option]),NewBody)).

filescope_did(_).


:- if( \+ current_predicate(set_skip_file_expansion/2)).
:- dynamic(lmcache:skip_file_expansion/2).

set_skip_file_expansion(File,TF):-   
   retractall(lmcache:skip_file_expansion(File,_)),
   asserta(lmcache:skip_file_expansion(File,TF)),
   SKIP = (notrace((prolog_load_context(file,S),lmcache:skip_file_expansion(S,true))),!),
   HEAD1 = term_expansion(X,P,X,P),
   HEAD2 = term_expansion(X,X),
   forall(member(M,[file_scope,baseKB,user,system]),
    (((system:clause(M:HEAD1,WAS),WAS=@=SKIP) -> true ; system:asserta(M:HEAD1:-SKIP)),
     ((system:clause(M:HEAD2,WAS),WAS=@=SKIP) -> true ; system:asserta(M:HEAD2:-SKIP)))),
   !.
:- endif.

:- ignore((source_location(S,_),prolog_load_context(module,M),
 forall(source_file(M:H,S),
 ignore((functor(H,F,A),
  ignore(((\+ atom_concat('$',_,F),(export(F/A) , current_predicate(system:F/A)->true; system:import(M:F/A))))),
  ignore(((\+ predicate_property(M:H,transparent), module_transparent(M:F/A), \+ atom_concat('__aux',_,F),debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A]))))))))).


l_once(G):- once(G),!.
l_once(G):- trace, G,!.

:-meta_predicate(term_expansion_option(:,+,-)).
term_expansion_option(Option,(:-Body),Out):- 
   \+  check_skip_id(Option,(:-Body) ,Body),
   prolog_load_context(module,Module),
   Module:call(Option,(:-Body),[],Body,NewBody),!,
   Body\=@=NewBody, 
   add_did_id(NewBody,Option,NewBody2),
   user:expand_term((:- NewBody2),Out),dmsg(portray(Out)).

term_expansion_option(Option,(Head:-Body),Out):- 
   \+  check_skip_id(Option,Head ,Body),
   prolog_load_context(module,Module),
   Module:call(Option,Head,[],Body,NewBody),!,
   Body\=@=NewBody, 
   add_did_id(NewBody,Option,NewBody2),
   user:expand_term((Head:- NewBody2),Out),dmsg(portray(Out)).



/*
system:term_expansion(In,Pos,Out,Pos):- nonvar(Pos),compound(In),functor(In,(:-),_),
   lmfs_data:file_option(Option),
   is_file_enabling(Option)->
   term_expansion_option(Option,In,Out),!.
*/

notice_file(end_of_file,File,_LineNo):- !, l_once(signal_eof(File)),
  retractall('$file_scope':opened_file(File,_)).
notice_file(_,File,LineNo):- 
  '$file_scope':opened_file(File,_) -> true; 
   (asserta('$file_scope':opened_file(File,LineNo)), l_once(begin_file_scope(File))).


system:term_expansion(EOF,Pos,_,_):- 
 (nonvar(EOF),nonvar(Pos)),
 (prolog_load_context(file,File)->
  source_location(File,LineNo)->
  notice_file(EOF,File,LineNo))->fail.

