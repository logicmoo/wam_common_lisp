/* Part of LogicMOO Base Logicmoo Path Setups
% ===================================================================
    Previous File:         'logicmoo_util_library.pl'
    Purpose:       To load the logicmoo libraries as needed
    Contact:       $Author: dmiles $@users.sourceforge.net ;
    Version:       'logicmoo_util_library.pl' 1.0.0
    Revision:      $Revision: 1.7 $
    Revised At:    $Date: 2002/07/11 21:57:28 $
    Author:        Douglas R. Miles
    Maintainers:   logicmoo
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    SCM:           https://github.com/logicmoo/PrologMUD/tree/master/pack/logicmoo_base
    Copyleft:      1999-2015, LogicMOO Prolog Extensions
    License:       Lesser GNU Public License
% ===================================================================
*/

:- module(logicmoo_common,[/*add_library_search_path/2,locally/2*/]).
/** <module> Utility LOGICMOO_COMMON
This module allows easier loading of common logicmoo utility predicates.

 - @author Douglas R. Miles
 - @license LGPL 
*/
:- set_module(class(library)).


:- op(700,xfx,prolog:('univ_safe')).
%:- discontiguous '$exported_op'/3.


:- system:use_module((logicmoo_startup)).

:- reexport((logicmoo_startup)).

/*
:- system:reexport(library(logicmoo/util_dlist)).
:- system:reexport(library(logicmoo/attvar_reader)).
:- system:reexport(library(logicmoo/attvar_serializer)).
:- system:reexport(library(logicmoo/portray_vars)).
:- system:reexport(library(logicmoo/util_engines)).
:- system:reexport(library(logicmoo/util_ctx_frame)).
:- system:reexport(library(logicmoo/butterfly)).
:- system:reexport(library(logicmoo/toplevel_variable_names)).
:- system:reexport(library(logicmoo/util_bb_env)).
:- system:reexport(library(logicmoo/util_structs)).
:- system:reexport(library(logicmoo/util_dra)).
:- system:reexport(library(logicmoo/util_bb_gvar)).
% :- system:reexport(library(xlisting/xlisting_web)).
*/
