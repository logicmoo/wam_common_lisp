/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * @TODO - add writable strings
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(string, []).

:- set_module(class(library)).

as_string_upper(C,SN):- compound(C),\+ is_list(C),functor(C,_P,A),arg(A,C,S),!, as_string_upper(S,SN).
as_string_upper(S,U):- cl_string(S,D),string_upper(D,U).


cl_stringp(A, R):- t_or_nil(string(A),R).

cl_string(SS,SS):- string(SS),!.
cl_string(S,SN):- is_symbolp(S),cl_symbol_name(S,SN),!.
% grabs ugly objects
cl_string(S,SN):- atom_concat(':',S0,S),!,cl_string(S0,SN).% TODO add a warjing that hte keyword was somehow misrepresented
cl_string(S,SN):- atom_concat('kw_',S0,S),!,cl_string(S0,SN). % TODO add a warjing that hte keyword was somehow missing
cl_string(S,SN):- notrace(catch(text_to_string(S,SN),_,fail)),!.

   
:- fixup_exports.



