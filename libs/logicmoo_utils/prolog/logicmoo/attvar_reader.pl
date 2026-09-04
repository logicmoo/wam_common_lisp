:- module(attvar_reader, [read_attvars/1,read_attvars/0,install_attvar_expander/1,uninstall_attvar_expander/1]).

/** <module> Utility LOGICMOO_ATTVAR_READER
This module works with Utility LOGICMOO_ATTVAR_SERIALIZER to allows us to keep attributed variables and files inside the prolog database.

- @author Douglas R. Miles
- @license LGPL 
*/
:- set_module(class(library)).
/*  Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_util_varnames.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_varnames.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/
:- create_prolog_flag(assert_attvars,false,[keep(true)]).
:- create_prolog_flag(read_attvars,false,[keep(true)]).
:- module_transparent((read_attvars/1,read_attvars/0)).

:- use_module((toplevel_variable_names)).
:- if(\+ current_module(attvar_serializer)).
:- use_module(library(logicmoo/attvar_serializer)).
:- endif.

:- multifile(lmcache:use_attvar_expander/1).
:- dynamic(lmcache:use_attvar_expander/1).

:- multifile(lmcache:never_use_attvar_expander/1).
:- dynamic(lmcache:never_use_attvar_expander/1).

%% install_attvar_expander(+FileOrModule) is det.
%
% Installs Attvar Expander to File Or Module

install_attvar_expander(M):- lmcache:use_attvar_expander(M),!.
install_attvar_expander(M):-
  dmsg(install_attvar_expander(M)),
  asserta(lmcache:use_attvar_expander(M)),
  system:multifile(M:term_expansion/4),
  system:module_transparent(M:term_expansion/4),
  system:dynamic(M:term_expansion/4),
  asserta(((M:term_expansion(I,P,O,P):- system_expanded_attvars(M:term,P,I,O)))),
  system:multifile(M:goal_expansion/4),
  system:module_transparent(M:goal_expansion/4),
  system:dynamic(M:goal_expansion/4),
  set_prolog_flag(read_attvars,true),
  set_prolog_flag(assert_attvars,true),
  asserta(((M:goal_expansion(I,P,O,P):- system_expanded_attvars(M:goal,P,I,O)))),
  !.


%% uninstall_attvar_expander(+FileOrModule) is det.
%
% Uninstalls Attvar Expander from File Or Module

uninstall_attvar_expander(M):-
  retract(lmcache:use_attvar_expander(M)),!,
  ignore(retract((M:goal_expansion(I,P,O,P):- system_expanded_attvars(M:goal,P,I,O)))),
  ignore(retract((M:term_expansion(I,P,O,P):- system_expanded_attvars(M:term,P,I,O)))),
  (lmcache:use_attvar_expander(M)->true;set_prolog_flag(read_attvars,false)).
uninstall_attvar_expander(_):-set_prolog_flag(read_attvars,false).
  
  

read_attvars:- read_attvars(true).

read_attvars(TF):- 
  set_prolog_flag(read_attvars,TF),
%  set_prolog_flag(assert_attvars,TF),
  prolog_load_context(module,M),
  (TF==true->
     install_attvar_expander(M);
     uninstall_attvar_expander(M)).
:- export(read_attvars/1).
:- system:import(read_attvars/1).


user:expand_query(Goal, Expanded, Bindings, ExpandedBindings):- fail,
      prolog_load_context(module,LC),
      \+ lmcache:never_use_attvar_expander(LC),
      current_prolog_flag(read_attvars,true),
      \+ current_prolog_flag(read_attvars_toplevel,false),
      \+ current_prolog_flag(xref,true), 
      Struct= ['$variable_names'|Bindings],
      deserialize_attvars(Struct,Goal,Expanded),
      [_|ExpandedBindings] = Struct.


:- fixup_exports.

