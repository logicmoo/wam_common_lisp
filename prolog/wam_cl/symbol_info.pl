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


:- dynamic o_p_v/3.

o_p_v(Symbol,kw_deftype,defconstant):- o_p_v(Symbol,package,pkg_kw).
:- include('si.pro').
%:- include('si2.pro').

:- dynamic symbol_info/4.
get_symbol_info(A,B,C,D):- symp:symbol_info(A,B,C,D).

non_prop(constant).
non_prop(variable).
non_prop(package).
non_prop(function_type).

f_u_get_opv(O,Result):- findall([P|V],get_opv(O,P,V),Result).
f_u_get_opv(O,P,V):- get_opv(O,P,V).
	
add_opv_maybe(O,P,_):- symp:o_p_v(O,P,_),!.
add_opv_maybe(O,P,V):- add_opv(O,P,V),!.

add_opv(Symbol,value,SValue):- atom(SValue),
 (atom_contains(SValue,'(');atom_contains(SValue,' ')),
  (as_sexp(SValue,Value)->SValue\==Value),!,add_opv(Symbol,value,Value).

add_opv(O,P,V):- ( \+ symp:o_p_v(O,P,_) -> assert(symp:o_p_v(O,P,V)) ; true).

get_opv(O,_,_):- string(O),!,fail.
get_opv(O,P,V):- no_repeats(O-P,symp:o_p_v(O,P,V)).

update_opv(O,P,V):- ignore(retract(symp:o_p_v(O,P,_))),assert(symp:o_p_v(O,P,V)).

/*
:- 
 forall((must(get_symbol_info(S,P,function_type,type_sub(T,ST))),must(get_symbol_info(S,P,function,F))),
  ((add_opv(F,typeof,T)),
   (add_opv(S,kw_compile_as,T)),
   (add_opv(F,classof,ST)))).


:- 
 forall((get_symbol_info(Symbol,P,name,Name),get_symbol_info(Symbol,P,package,kw_external)),
   add_package_external_symbol(P,Name,Symbol)).

:- 
 forall((get_symbol_info(Symbol,P,name,Name),get_symbol_info(Symbol,P,package,kw_internal)),
   add_package_internal_symbol(P,Name,Symbol)).

:- 
 forall(get_symbol_info(Symbol,P,_,_),(add_opv(Symbol,package,P))).

:- 
 forall(get_symbol_info(Symbol,_,_,_),(add_opv(Symbol,typeof,symbol))).


:- 
 forall((get_symbol_info(Symbol,_P,Prop,Value),\+non_prop(Prop)),add_opv(Symbol,Prop,Value)).

dov1:- 
 forall(get_symbol_info(Symbol,_,constant,Value),
   ((add_opv_maybe(Symbol,value,Value)),
    (add_opv(Symbol,kw_deftype,defconstant)))).
dov2:- 
 forall(get_symbol_info(Symbol,_,variable,Value),
   ((add_opv_maybe(Symbol,value,Value)),
    (add_opv(Symbol,kw_deftype,defparameter)))).

dov3:- tell(si),forall(symp:o_p_v(O,P,V),format('~q.~n',[o_p_v(O,P,V)])),
  forall(symp:package_external_symbols(O,P,V),format('~q.~n',[package_external_symbols(O,P,V)])),
  forall(symp:package_internal_symbols(O,P,V),format('~q.~n',[package_internal_symbols(O,P,V)])),  
  told.
*/

:- fixup_exports.

