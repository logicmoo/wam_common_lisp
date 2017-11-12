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


cl_symbol_name(Symbol,Name):- get_opv(Symbol,name,Name),!.
cl_symbol_package(Symbol,Package):- get_opv(Symbol,package,Package),!.
cl_symbol_value(Symbol,Value):- do_or_die(get_opv(Symbol,value,Value)),!.
cl_symbol_function(Symbol,Function):- do_or_die(get_opv(Symbol,function,Function)),!.
do_or_die(G):- G->true;throw(do_or_die(G)).

is_constantp(Symbol):- get_opv(Symbol,kw_deftype,defconstant),!.
is_constantp(Symbol):- get_opv(Symbol,package,pkg_kw),!.
is_constantp(Object):- is_self_evaluationing_const(Object),!.



cl_symbolp(S,R):-  t_or_nil(is_symbolp(S),R).
cl_keywordp(S,R):-  t_or_nil(is_keywordp(S),R).
cl_constantp(S,R):- t_or_nil(is_constantp(S),R).
cl_boundp(Sym,R):- t_or_nil(get_opv(Sym,value,_),R).
cl_fboundp(Sym,R):- t_or_nil(get_opv(Sym,function,_),R).

cl_gensym(Symbol):- cl_gensym("G",Symbol).
cl_gensym(String,Symbol):- gensym(String,SymbolName),cl_make_symbol(SymbolName,Symbol).


is_symbolp(Symbol):- get_opv(Symbol,typeof,clz_symbol).

is_keywordp(Symbol):- get_opv(Symbol,package,pkg_kw),!.
%is_keywordp(Symbol):- atom(Symbol),sanity((must(\+ atom_concat(':',_,Symbol)))),!,fail.



cl_find_symbol(String,Result):- reading_package(Package)->cl_find_symbol(String,Package,Result).
cl_find_symbol(String,Pack,Result):-  find_package_or_die(Pack,Package),
  package_find_symbol(String,Package,Symbol,IntExt),push_values([Symbol,IntExt],Result),!.
cl_find_symbol(_Var,_P,Result):- push_values([[],[]],Result).

% @TODO Add symbol shadowing 
cl_import(Symbol,Result):- reading_package(Package),cl_import(Symbol,Package,Result).
cl_import(Symbol,Pack,Result):- 
   find_package_or_die(Pack,Package),
   cl_symbol_name(Symbol,String),
   package_find_symbol(String,Package,_OldSymbol,IntExt),
   add_package_internal_symbol(Package,String,Symbol),
   push_values([Symbol,IntExt],Result),!.

cl_export(Symbol,Result):- reading_package(Package),cl_export(Symbol,Package,Result).
cl_export(Symbol,Pack,Result):-
   find_package_or_die(Pack,Package),
   cl_symbol_name(Symbol,String),
   package_find_symbol(String,Package,_OldSymbol,IntExt),
   add_package_external_symbol(Package,String,Symbol),
   push_values([Symbol,IntExt],Result),!.


cl_intern(Symbol,Result):- reading_package(Package),cl_intern(Symbol,Package,Result).
% cl_intern(Symbol,Package,Result):- \+ is_keywordp(Symbol),is_symbolp(Symbol),!,cl_intern_symbol(Symbol,Package,Result).
cl_intern(Name,Pack,Result):-
  find_package_or_die(Pack,Package),
  text_to_string(Name,String),
  intern_symbol(String,Package,Symbol,IntExt),
  push_values([Symbol,IntExt],Result),!.

intern_symbol(String,Package,Symbol,IntExt):- package_find_symbol(String,Package,Symbol,IntExt),!.
intern_symbol(String,Package,Symbol,IntExt):-
   ignore(symbol_case_name(String,Package,Symbol)),
   create_symbol(String,Package,Symbol),
   must((add_package_internal_symbol(Package,String,Symbol),   
   package_find_symbol(String,Package,Symbol,IntExt))).


cl_make_symbol(SymbolName,Symbol):- 
   prologcase_name(SymbolName,ProposedName),
   gensym(ProposedName,Symbol),
   create_symbol(SymbolName,[],Symbol).

create_symbol(String,Package,Symbol):-
   text_to_string(String,Name),
   add_opv(Symbol,typeof,symbol),
   add_opv(Symbol,classof,clz_symbol),
   add_opv(Symbol,name,Name),
   add_opv(Symbol,package,Package),!.

create_keyword(Name,Symbol):- atom_concat(':',Make,Name),!,create_keyword(Make,Symbol).
create_keyword(Name,Symbol):- string_upper(Name,String),string_lower(Name,Lower),
   atom_concat('kw_',Lower,Symbol),
   create_symbol(String,pkg_kw,Symbol),
   add_package_external_symbol(pkg_kw,String,Symbol),!.










print_symbol(Symbol):- 
   writing_package(Package),
   print_symbol_at(Symbol,Package),!.
print_symbol(S):-write(S).
 
print_symbol_at(Symbol,PrintP):- 
  cl_symbol_package(Symbol,SPackage),!,
  print_symbol_from(Symbol,PrintP,SPackage),!.

print_symbol_from(Symbol,_PrintP,kw_pkg):- !, cl_symbol_name(Symbol,Name),write(':'),write(Name).
print_symbol_from(Symbol,PrintP,SPackage):-
  cl_symbol_name(Symbol,Name),
  must(package_find_symbol(Name,SPackage,FoundSymbol,IntExt)),
  must( Symbol\== FoundSymbol -> 
    print_prefixed_symbol(Name,PrintP,SPackage,kw_internal);
    print_prefixed_symbol(Name,PrintP,SPackage,IntExt)).

print_package_or_hash([]):- write("#").
print_package_or_hash(P):- package_name(P,S),write(S).
print_package_or_hash(P):- write(P).

print_prefixed_symbol(S,_WP,pkg_kw,_):- write(':'),write(S).
print_prefixed_symbol(S,PrintP,SP,_):- SP==PrintP, !,write(S).
print_prefixed_symbol(S,_P,SP,kw_internal):-!, print_package_or_hash(SP),write('::'),write(S).
print_prefixed_symbol(S,PrintP,SP,kw_external):- package_use_list(PrintP,SP),!,write(S).
print_prefixed_symbol(S,_P,SP,kw_external):- !, print_package_or_hash(SP),write(':'),write(S).
print_prefixed_symbol(S,_,SP,_):- print_package_or_hash(SP),write('::'),write(S).




:- fixup_exports.


