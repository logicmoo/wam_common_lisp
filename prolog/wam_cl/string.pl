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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(string, []).

:- set_module(class(library)).

as_string_upper(C,SN):- compound(C),\+ is_list(C),functor(C,_P,A),arg(A,C,S),!, as_string_upper(S,SN).
as_string_upper(S,U):- to_prolog_string(S,D),string_upper(D,U).

is_stringp(X):- string(X).
is_stringp('$ARRAY'([_N],claz_base_char,List)):- nonvar(List).
cl_stringp(A, R):- t_or_nil(is_stringp(A),R).

cl_string(O,S):- to_prolog_string(O,PLS),to_lisp_string(PLS,S).

to_prolog_string(SS,SS):- string(SS),!.
to_prolog_string('$ARRAY'([_N],claz_base_char,List),SS):- !,maplist(to_prolog_codes,List,Codes),text_to_string(Codes,SS).
to_prolog_string(S,SN):- is_symbolp(S),pl_symbol_name(S,SN).
% grabs ugly objects
to_prolog_string(S,SN):- atom_concat_or_rtrace(':',S0,S),!,to_prolog_string(S0,SN).% TODO add a warjing that hte keyword was somehow misrepresented
to_prolog_string(S,SN):- atom_concat_or_rtrace('kw_',S0,S),!,to_prolog_string(S0,SN). % TODO add a warjing that hte keyword was somehow missing
to_prolog_string(S,SN):- notrace(catch(text_to_string(S,SN),_,fail)),!.

to_prolog_codes('$CHAR'(Int),Int).
to_prolog_codes((Int),Int).

to_lisp_string('$ARRAY'([N],claz_base_char,List),'$ARRAY'([N],claz_base_char,List)):-!.
to_lisp_string(Str,'$ARRAY'([_],claz_base_char,List)):- atom_chars(Str,Chars),maplist(make_character,Chars,List).

% SHARED SECTION
:- multifile(wl:coercion/3).
wl:coercion(In, plstring, Out):- to_prolog_string(In,Out).
wl:coercion(In, string, Out):- cl_string(In,Out).
wl:coercion(In, list, Out):- is_stringp(In),to_lisp_string(In,Out).
wl:coercion(In, sequence(string,chars), Out):- is_stringp(In),to_lisp_string(In,Out).
   
:- fixup_exports.



