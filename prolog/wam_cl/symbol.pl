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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(symbol, []).
:- set_module(class(library)).
:- include('header.pro').

wl:type_checked(cl_symbol_name(symbol,string)).
cl_symbol_name(Symbol,Name):- pl_symbol_name(Symbol,Name).
pl_symbol_name(Symbol,Name):- package_external_symbols(pkg_kw,Name,Symbol)->true;get_opv(Symbol,name,Name).
cl_symbol_package(Symbol,Package):- is_keywordp(Symbol)->Package=pkg_kw;get_opv(Symbol,package,Package).
cl_symbol_value(Symbol,Value):- is_keywordp(Symbol)->Symbol=Value;do_or_die(get_opv(Symbol,value,Value)).
cl_symbol_function(Symbol,Function):- do_or_die(get_opv(Symbol,function,Function)),!.
do_or_die(G):- G->true;throw(do_or_die(G)).


cl_boundp(Symbol,TF):-  t_or_nil(is_boundp(Symbol),TF).
cl_constantp(Symbol,TF):- t_or_nil(is_constantp(Symbol),TF).
cl_fboundp(Symbol,TF):-  t_or_nil(is_fboundp(Symbol),TF).
cl_keywordp(Symbol,TF):-  t_or_nil(is_keywordp(Symbol),TF).
cl_symbolp(Symbol,TF):-  t_or_nil(is_symbolp(Symbol),TF).

cl_gensym(Symbol):- cl_gensym("G",Symbol).
cl_gensym(Name,Symbol):- to_prolog_string(Name,String), gensym(String,SymbolName),cl_make_symbol(SymbolName,Symbol).


is_boundp(Symbol):- is_keywordp(Symbol);get_opv(Symbol,value,_).
is_constantp(Object):- is_self_evaluationing_const(Object);get_opv(Object,defined_as,defconstant).
is_fboundp(Symbol):- get_opv(Symbol,function,_).
is_keywordp(Symbol):- package:package_external_symbols(pkg_kw,_,Symbol).
is_symbolp(Symbol):- is_keywordp(Symbol);get_opv(Symbol,classof,claz_symbol).

%is_keywordp(Symbol):- atom(Symbol),sanity((must(\+ atom_concat_or_rtrace(':',_,Symbol)))),!,fail.



cl_find_symbol(String,Result):- reading_package(Package)->cl_find_symbol(String,Package,Result).
cl_find_symbol(String,Pack,Result):-  find_package_or_die(Pack,Package),
  package_find_symbol(String,Package,Symbol,IntExt),push_values([Symbol,IntExt],Result),!.
cl_find_symbol(_Var,_P,Result):- push_values([[],[]],Result).


cl_intern(Symbol,Result):- reading_package(Package),cl_intern(Symbol,Package,Result).
% cl_intern(Symbol,Package,Result):- \+ is_keywordp(Symbol),is_symbolp(Symbol),!,cl_intern_symbol(Symbol,Package,Result).
cl_intern(Name,Pack,Result):-
  find_package_or_die(Pack,Package),
  text_to_string(Name,String),
  intern_symbol(String,Package,Symbol,IntExt),
  push_values([Symbol,IntExt],Result),!.


intern_symbol(String,Package,Symbol,IntExt):- package_find_symbol(String,Package,Symbol,IntExt),!.
intern_symbol(String,Package,Symbol,IntExt):- 
   make_fresh_internal_symbol(Package,String,Symbol),
   always((package_find_symbol(String,Package,FoundSymbol,IntExt),FoundSymbol==Symbol)).


cl_unintern(Symbol,t):- 
   cl_symbol_package(Symbol,Package),
   (Package\==[]-> package_unintern_symbol(Package,Symbol) ; true),
   set_opv(Symbol,package,[]),
   delete_obj(Symbol).



cl_unintern(String,Package,Symbol,IntExt):- package_find_symbol(String,Package,Symbol,IntExt),!.
unintern_symbol(String,Package,Symbol,IntExt):- 
   make_fresh_uninternal_symbol(Package,String,Symbol),
   always((package_find_symbol(String,Package,FoundSymbol,IntExt),FoundSymbol==Symbol)).


cl_make_symbol(SymbolName,Symbol):- 
   prologcase_name(SymbolName,ProposedName),
   gensym(ProposedName,Symbol),
   create_symbol(SymbolName,[],Symbol).


create_symbol(String,pkg_kw,Symbol):-!,create_keyword(String,Symbol).
create_symbol(String,Package,Symbol):-
   text_to_string(String,Name),
   set_opv(Symbol,classof,claz_symbol),
   set_opv(Symbol,name,Name),
   set_opv(Symbol,package,Package),!.

create_keyword(Name,Symbol):- atom_concat_or_rtrace(':',Make,Name),!,create_keyword(Make,Symbol).
create_keyword(Name,Symbol):- string_upper(Name,String),
   prologcase_name(String,Lower),
   atom_concat_or_rtrace('kw_',Lower,Symbol),
   assert_if_new(package:package_external_symbols(pkg_kw,String,Symbol)).










print_symbol(Symbol):- 
   writing_package(Package),
   ((print_symbol_at(Symbol,Package))),!.
print_symbol(Symbol):-write(Symbol).
 
print_symbol_at(Symbol,PrintP):- 
  cl_symbol_package(Symbol,SPackage),!,
  print_symbol_from(Symbol,PrintP,SPackage),!.

print_symbol_from(Symbol,_PrintP,Pkg):- Pkg == kw_pkg, !, cl_symbol_name(Symbol,Name),write(':'),write(Name).
print_symbol_from(Symbol,PrintP,SPackage):-
  cl_symbol_name(Symbol,Name),
  must(package_find_symbol(Name,SPackage,FoundSymbol,IntExt)),
  must( Symbol\== FoundSymbol -> 
    print_prefixed_symbol(Name,PrintP,SPackage,kw_internal);
    print_prefixed_symbol(Name,PrintP,SPackage,IntExt)).

print_package_or_hash(Var):- var(Var),!,writeq(Var).
print_package_or_hash([]):- !,write("#").
print_package_or_hash(P):- package_name(P,Symbol),shorter_name(Symbol,Short),!,write(Short).
print_package_or_hash(P):- package_name(P,N),!,write(N).
print_package_or_hash(P):- trace,writeq(failed_print_package_or_hash(P)).


shorter_name(PN,NN):- package_nicknames(PN,NN),atom_length(PN,B),atom_length(NN,A),A<B.
shorter_name("SYSTEM","SYS").
shorter_name("COMMON-LISP","CL").
shorter_name("COMMON-LISP-USER","U").
shorter_name("SYSTEM","SYS").
shorter_name("EXTENSIONS","EXT").
shorter_name(S,S).

print_prefixed_symbol(Symbol,_WP,pkg_kw,_):- write(':'),write(Symbol).
print_prefixed_symbol(Symbol,PrintP,SP,_):- SP==PrintP, !,write(Symbol).
print_prefixed_symbol(Symbol,_P,SP,kw_internal):-!, print_package_or_hash(SP),write('::'),write(Symbol).
print_prefixed_symbol(Symbol,PrintP,SP,kw_external):- package_use_list(PrintP,SP),!,write(Symbol).
print_prefixed_symbol(Symbol,_P,SP,kw_external):- !, print_package_or_hash(SP),write(':'),write(Symbol).
print_prefixed_symbol(Symbol,_,SP,_):- print_package_or_hash(SP),write('::'),write(Symbol).




:- fixup_exports.


