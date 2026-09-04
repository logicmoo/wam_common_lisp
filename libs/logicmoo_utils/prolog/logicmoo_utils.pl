/* Part of LogicMOO Base Logicmoo Path Setups
% ===================================================================
    Previous File:         'logicmoo_util_library.pl'
    Purpose:       To load the logicmoo libraries as needed
    Contact:       $Author: logicmoo@gmail.com ;
    Version:       'logicmoo_util_library.pl' 1.0.0
    Revision:      $Revision: 1.7 $
    Revised At:    $Date: 2021/07/11 21:57:28 $
    Author:        Douglas R. Miles
    Maintainers:   logicmoo
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    SCM:           https://github.com/logicmoo/PrologMUD/tree/master/pack/logicmoo_base
    Copyleft:      1999-2021, LogicMOO Prolog Extensions
    License:       Lesser GNU Public License
% ===================================================================
*/

:- module(logicmoo_utils,[]).
/** <module> Utility LOGICMOO UTILS
This module holds less common utils that have to be loaded individually.  
 - @author Douglas R. Miles
 - @license LGPL
*/
:- set_prolog_flag(verbose_autoload,true),
 set_prolog_flag(verbose_load,full),!.
 
%:- discontiguous(logicmoo_utils:'$exported_op'/3).
%:- logicmoo_utils:use_module(library(logicmoo_common)).
:- ensure_loaded(library(logicmoo_common)).
:- ensure_loaded(library(logicmoo_startup)).


