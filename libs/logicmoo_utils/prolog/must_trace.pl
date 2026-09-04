:- module(must_trace_legacy,[]).
%:- discontiguous('$exported_op'/3).
%:- discontiguous('$pldoc'/4).

/** <module> Utility LOGICMOO_MUST_TRACE 
Trace with your eyes not your hands, the file is distributed, part of rtrace. 
	Whenever a predicate fails it begins rtracing it. 
- @author Douglas R. Miles
- @license LGPL 
*/

:- reexport(library(logicmoo_common)).
