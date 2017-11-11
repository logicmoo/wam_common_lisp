/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(package, []).
:- set_module(class(library)).
:- include('header.pro').


cl_find_package(S,Obj):- find_package(S,Package),!,must(as_package_object(Package,Obj)).
cl_find_package(_,[]).


find_package(ugly(package,UP),Package):-!,find_package(UP,Package).
find_package(S,S):- is_lisp_package(S),!.
find_package(S,Package):- 
  as_string_upper(S,SN),
  (package_name(Package,SN) ; package_nickname(Package,SN)).

find_package_or_die(X,Y):- find_package(X,Y) -> true ; throw(find_package_or_die(X,Y)).  

as_package_object(Package,ugly(package,Package)).


reading_package(Package):- symbol_value('xx_package_xx',UP),find_package(UP,Package).
reading_package(pkg_user).
% TODO
writing_package(Package):- reading_package(Package).


package_find_symbol(Name,_,Symbol,kw_external):- atom_concat(':',KWName,Name),!,atom_concat('kw_',KWName,SymbolCI),prologcase_name(SymbolCI,Symbol).
package_find_symbol(Name,Package,Symbol,kw_external):- package_external_symbols(Package,Name,Symbol),!.
package_find_symbol(Name,Package,Symbol,kw_internal):- package_internal_symbols(Package,Name,Symbol),!.
package_find_symbol(Name,PW,Symbol,kw_inherited):-  package_use_list(PW,Package),package_external_symbols(Package,Name,Symbol).



:- fixup_exports.
