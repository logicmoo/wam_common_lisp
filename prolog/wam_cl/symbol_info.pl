/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (pkg_xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (pkg_c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (pkg_YAP 4x faster).
 *
 *******************************************************************/
:- module(symp, []).
:- set_module(class(library)).
% :- include('header.pro').

:- style_check(-discontiguous).
:- dynamic symbol_info/4.
:- dynamic o_p_v/3.

:- dynamic get_symbol_info/4.

:- include('si.pro').
%:- include('si2.pro').

get_symbol_info(A,B,C,D):- symp:symbol_info(A,B,C,D).

non_prop(constant).
non_prop(variable).
non_prop(package).
non_prop(function_type).

symp:o_p_v(Symbol,kw_deftype,defconstant):- symp:o_p_v(Symbol,package,pkg_kw).

f_u_get_opv(O,Result):- findall([P|V],get_o_p_v(O,P,V),Result).
f_u_get_opv(O,P,V):- get_o_p_v(O,P,V).
	
add_o_p_v_maybe(O,P,_):- symp:o_p_v(O,P,_),!.
add_o_p_v_maybe(O,P,V):- add_o_p_v(O,P,V),!.

add_o_p_v(Symbol,value,SValue):- atom(SValue),
 (atom_contains(SValue,'(');atom_contains(SValue,' ')),
  (as_sexp(SValue,Value)->SValue\==Value),!,add_o_p_v(Symbol,value,Value).

add_o_p_v(O,P,V):- ( \+ symp:o_p_v(O,P,_) -> assert(symp:o_p_v(O,P,V)) ; true).

get_o_p_v(O,_,_):- string(O),!,fail.
get_o_p_v(O,P,V):- no_repeats(O-P,symp:o_p_v(O,P,V)).

update_o_p_v(O,P,V):- ignore(retract(symp:o_p_v(O,P,_))),assert(symp:o_p_v(O,P,V)).

/*
:- 
 forall((must(get_symbol_info(S,P,function_type,type_sub(T,ST))),must(get_symbol_info(S,P,function,F))),
  ((add_o_p_v(F,typeof,T)),
   (add_o_p_v(S,kw_compile_as,T)),
   (add_o_p_v(F,classof,ST)))).


:- 
 forall((get_symbol_info(Symbol,P,name,Name),get_symbol_info(Symbol,P,package,kw_external)),
   add_package_external_symbol(P,Name,Symbol)).

:- 
 forall((get_symbol_info(Symbol,P,name,Name),get_symbol_info(Symbol,P,package,kw_internal)),
   add_package_internal_symbol(P,Name,Symbol)).

:- 
 forall(get_symbol_info(Symbol,P,_,_),(add_o_p_v(Symbol,package,P))).

:- 
 forall(get_symbol_info(Symbol,_,_,_),(add_o_p_v(Symbol,typeof,symbol))).


:- 
 forall((get_symbol_info(Symbol,_P,Prop,Value),\+non_prop(Prop)),add_o_p_v(Symbol,Prop,Value)).

dov1:- 
 forall(get_symbol_info(Symbol,_,constant,Value),
   ((add_o_p_v_maybe(Symbol,value,Value)),
    (add_o_p_v(Symbol,kw_deftype,defconstant)))).
dov2:- 
 forall(get_symbol_info(Symbol,_,variable,Value),
   ((add_o_p_v_maybe(Symbol,value,Value)),
    (add_o_p_v(Symbol,kw_deftype,defparameter)))).

dov3:- tell(si),forall(symp:o_p_v(O,P,V),format('~q.~n',[o_p_v(O,P,V)])),
  forall(symp:package_external_symbols(O,P,V),format('~q.~n',[package_external_symbols(O,P,V)])),
  forall(symp:package_internal_symbols(O,P,V),format('~q.~n',[package_internal_symbols(O,P,V)])),  
  told.
*/

:- fixup_exports.

