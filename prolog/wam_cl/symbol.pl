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
:- module(symbol, []).
:- set_module(class(library)).
:- include('header.pro').


cl_symbol_name(Symbol,Name):- get_o_p_v(Symbol,name,Name),!.
cl_symbol_package(Symbol,Package):- get_o_p_v(Symbol,package,Package),!.
cl_symbol_value(Symbol,Value):- do_or_die(get_o_p_v(Symbol,value,Value)),!.
cl_symbol_function(Symbol,Function):- do_or_die(get_o_p_v(Symbol,function,Function)),!.
do_or_die(G):- G->true;throw(do_or_die(G)).

is_constantp(Symbol):- get_o_p_v(Symbol,kw_deftype,defconstant).
is_constantp(Symbol):- get_o_p_v(Symbol,package,pkg_kw).
is_constantp(Object):- is_self_evaluationing_const(Object).



cl_constantp(S,R):- t_or_nil(is_constantp(S),R).
cl_boundp(Sym,R):- t_or_nil(get_o_p_v(Sym,value,_),R).
cl_fboundp(Sym,R):- t_or_nil(get_o_p_v(Sym,function,_),R).

cl_gensym(Symbol):- cl_gensym("G",Symbol).
cl_gensym(String,Symbol):- gensym(String,SymbolName),cl_make_symbol(SymbolName,Symbol).


is_symbolp(Symbol):- get_o_p_v(Symbol,typeof,symbol).
is_keywordp(Symbol):- get_o_p_v(Symbol,package,pkg_kw).


cl_find_symbol(String,Result):- reading_package(Package), cl_find_symbol(String,Package,Result).
cl_find_symbol(String,Package,Result):-  package_find_symbol(String,Package,Symbol,IntExt),push_values([Symbol,IntExt],Result),!.
cl_find_symbol(_Var,_P,Result):- push_values([[],[]],Result).

cl_import(Symbol,Result):- reading_package(Package),cl_import(Symbol,Package,Result).
cl_import(Symbol,Package,Result):- 
   cl_symbol_name(Symbol,String),
   package_find_symbol(String,Package,_OldSymbol,IntExt),
   add_package_internal_symbol(Package,String,Symbol),
   push_values([Symbol,IntExt],Result),!.

cl_export(Symbol,Result):- reading_package(Package),cl_export(Symbol,Package,Result).
cl_export(Symbol,Package,Result):- 
   cl_symbol_name(Symbol,String),
   package_find_symbol(String,Package,_OldSymbol,IntExt),
   add_package_external_symbol(Package,String,Symbol),
   push_values([Symbol,IntExt],Result),!.


cl_intern(Symbol,Result):- reading_package(Package),cl_intern(Symbol,Package,Result).
% cl_intern(Symbol,Package,Result):- \+ is_keywordp(Symbol),is_symbolp(Symbol),!,cl_intern_symbol(Symbol,Package,Result).
cl_intern(Name,Package,Result):-
  text_to_string(Name,String),
  intern_symbol(String,Package,Symbol,IntExt),
  push_values([Symbol,IntExt],Result),!.

intern_symbol(String,Package,Symbol,IntExt):- package_find_symbol(String,Package,Symbol,IntExt),!.
intern_symbol(String,Package,Symbol,IntExt):-
   ignore(symbol_case_name(String,Package,Symbol)),
   create_symbol(String,Package,Symbol),
   add_package_internal_symbol(Package,String,Symbol),   
   must(package_find_symbol(String,Package,Symbol,IntExt)).


cl_make_symbol(SymbolName,Symbol):- 
   prologcase_name(SymbolName,ProposedName),
   gensym(ProposedName,Symbol),
   create_symbol(SymbolName,[],Symbol).

create_symbol(String,Package,Symbol):-
   text_to_string(String,Name),
   add_o_p_v(Symbol,typeof,symbol),
   add_o_p_v(Symbol,name,Name),
   add_o_p_v(Symbol,package,Package),!.

create_keyword(Name,Symbol):- atom_concat(':',Make,Name),!,create_keyword(Make,Symbol).
create_keyword(Name,Symbol):- string_upper(Name,String),string_lower(Name,Lower),
   atom_concat('kw_',Lower,Symbol),
   create_symbol(String,pkg_kw,Symbol),
   add_package_external_symbol(pkg_kw,String,Symbol),!.

:- fixup_exports.
