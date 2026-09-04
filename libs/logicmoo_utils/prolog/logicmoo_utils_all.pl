/* Part of LogicMOO Base Logicmoo Path Setups
% ===================================================================
    File:         'logicmoo_util_library.pl'
    Purpose:       To load the logicmoo libraries as needed
    Contact:       $Author: logicmoo@gmail.com ;
    Version:       'logicmoo_util_library.pl' 1.0.0
    Revision:      $Revision: 1.7 $
    Revised At:    $Date: 2002/07/11 21:57:28 $
    Author:        Douglas R. Miles
    Maintainers:   logicmoo
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    SCM:           https://github.com/logicmoo/PrologMUD/tree/master/pack/logicmoo_base
    Copyleft:      1999-2021, LogicMOO Prolog Extensions
    License:       Lesser GNU Public License
% ===================================================================
*/

:- module(logicmoo_utils_all_legacy,[]).
:- discontiguous('$exported_op'/3).
/** <module> Utility LOGICMOO_UTILS_ALL
This module loads ALL (the most common and uncommon) utils at once.
@author Douglas R. Miles
@license LGPL
*/

%:- discontiguous(logicmoo_utils_all:'$exported_op'/3).
%:- logicmoo_utils_all:use_module(library(logicmoo_utils)).
%:- logicmoo_utils_all:reexport(library(logicmoo_utils)).
:- reexport(library(logicmoo_common)).


