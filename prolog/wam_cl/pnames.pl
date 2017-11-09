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
:- module(pnames, []).
:- set_module(class(library)).
:- include('header.pro').
:- set_prolog_flag(verbose_load,full).
:- set_prolog_flag(verbose,normal).
:- set_prolog_flag(verbose_autoload,true).



% debug_var(_A,_Var):-!.
debug_var(X,Y):- notrace(catch(debug_var0(X,Y),_,fail)) -> true ; rtrace(debug_var0(X,Y)).

p_n_atom(Cmpd,UP):- sub_term(Atom,Cmpd),nonvar(Atom),\+ number(Atom), Atom\==[], catch(p_n_atom0(Atom,UP),_,fail),!.
p_n_atom(Cmpd,UP):- term_to_atom(Cmpd,Atom),p_n_atom0(Atom,UP),!.

filter_var_chars([58|X],[107, 119, 95|Y]):- filter_var_chars_trim_95(X,Y).
filter_var_chars([95|X],[95|Y]):- !, filter_var_chars_trim_95(X,Y).
filter_var_chars(X,Y):- filter_var_chars_trim_95(X,Y).



filter_var_chars_trim_95(X,Y):- filter_var_chars0(X,M),trim_95(M,Y),!.

trim_95([95|M],Y):-!, trim_95(M,Y).
trim_95([X|L],[100,X|Y]):- char_type(X,digit), trim_96(L,Y).
trim_95([X|L],[97,X|Y]):- \+ char_type(X,alpha), trim_96(L,Y).
trim_95(X,Y):- trim_96(X,Y).

trim_96([95],[]).
trim_96([],[]).
trim_96([95,95|M],Y):- trim_96([95|M],Y).
trim_96([X|M],[X|Y]):- trim_96(M,Y).



filter_var_chars0([],[]).

%  `%` -> `_pf_`
filter_var_chars0([37|T],[95,112, 102, 95| Rest]):-!,filter_var_chars0(T,Rest).
%  `-` -> `_`
filter_var_chars0([45|T],[95|Rest]):-!,filter_var_chars0(T,Rest).
%  `*` -> `_xx_`
filter_var_chars0([42|T],[95,120,120,95|Rest]):-!,filter_var_chars0(T,Rest).
%  `:` -> `_`
filter_var_chars0([42|T],[95,120,95|Rest]):-!,filter_var_chars0(T,Rest).
filter_var_chars0([H|T],[H|Rest]):-  code_type(H, prolog_identifier_continue),!,filter_var_chars0(T,Rest).
filter_var_chars0([H|T],Rest):- number_codes(H,Codes), filter_var_chars0(T,Mid),append([95, 99|Codes],[95|Mid],Rest).

p_n_atom0(Atom,UP):- atom(Atom),!,name(Atom,[C|Was]),to_upper(C,U),filter_var_chars([U|Was],CS),name(UP,CS).
p_n_atom0(String,UP):- string(String),!,string_to_atom(String,Atom),!,p_n_atom0(Atom,UP).
p_n_atom0([C|S],UP):- !,notrace(catch(atom_codes(Atom,[C|S]),_,fail)),!,p_n_atom0(Atom,UP).

debug_var0(_,NonVar):-nonvar(NonVar),!.
debug_var0([C|S],Var):- notrace(catch(atom_codes(Atom,[C|S]),_,fail)),!,debug_var0(Atom,Var).
debug_var0([AtomI|Rest],Var):-!,maplist(p_n_atom,[AtomI|Rest],UPS),atomic_list_concat(UPS,NAME),debug_var0(NAME,Var),!.
debug_var0(Atom,Var):- p_n_atom(Atom,UP),  
  check_varname(UP),
  add_var_to_env_loco(UP,Var),!.

add_var_to_env_loco(UP,Var):- \+ atom_concat('_',_,UP), var(Var),
  get_var_name(Var,Name),atomic(Name),\+ atom_concat('_',_,Name),
  
  atom_concat(UP,Name,New),add_var_to_env(New,Var).
add_var_to_env_loco(UP,Var):-add_var_to_env(UP,Var).

check_varname(UP):- name(UP,[C|_]),(char_type(C,digit)->throw(check_varname(UP));true).
                        

reader_fix_symbols(Package,SymbolName,Symbol):-
   atom(SymbolName),atom_symbol(SymbolName,Package,Symbol),!.
reader_fix_symbols(_Package,Some,Some):- \+ compound(Some),!.
reader_fix_symbols(Package,[S|Some],[SR|SomeR]):- 
  reader_fix_symbols(Package,S,SR),
  reader_fix_symbols(Package,Some,SomeR).
reader_fix_symbols(_Package,Some,Some).

find_package_or_die(X,Y):- find_package(X,Y) -> true ; throw(find_package_or_die(X,Y)).

find_package(S,P):- cl_find_package([S],ugly(package,P)).

create_kw(Name,Symbol):- atom_concat(':',Make,Name),!,create_kw(Make,Symbol).
create_kw(Name,Symbol):- downcase_atom(Name,Lower),
   atom_concat('kw_',Name,Symbol),
   asserta(symbol_info(Symbol,keyword,name,Lower)),
   asserta(symbol_info(Symbol,keyword,package,kw_external)).
  
atom_symbol(SymbolName,Symbol):- reading_package(Package),atom_symbol(SymbolName,Package,Symbol).
atom_symbol(end_of_file,_,end_of_file):-!.
%atom_symbol('prolog.',_,make):-!.
%atom_symbol('prolog.',_,prolog):-!.
atom_symbol(SymbolName,Package,Symbol):- atomic_list_concat([SymbolName1|SymbolNameS],':',SymbolName),
  atom_symbol_s(SymbolName1,SymbolNameS,Package,Symbol),!.

atom_symbol_s('#',['',_SymbolName],_UPackage,_Symbol):- throw('@TODO *** - READ from #<INPUT CONCATENATED-STREAM #<INPUT STRING-INPUT-STREAM> #<IO TERMINAL-STREAM>>: token ":BAR" after #: should contain no colon').  
atom_symbol_s('#',[_SymbolName],_UPackage,_Symbol):- throw('@TODO gen a temp symbol').
atom_symbol_s(SymbolName,[],Package,Symbol):- find_symbol_from(SymbolName,Package,Symbol),!.
atom_symbol_s(PName,['',SymbolName],_UPackage,Symbol):-  find_package_or_die(PName,Package),string_upper(SymbolName,StringUpper),cl_intern([StringUpper,Package],Symbol).
atom_symbol_s(PName,[SymbolName],_UPackage,Symbol):- find_package_or_die(PName,Package),atom_symbol_ext_only(SymbolName,Package,Symbol).
atom_symbol_s(SymbolName,[],Package,Symbol):- atom_string(SymbolName,String),
  string_upper(String,StringUpper),
  cl_intern([StringUpper,Package],Symbol).

atom_symbol_ext_only(SymbolName,Package,Symbol):- find_symbol_from1(SymbolName,Package,Result,KW),!,atom_symbol_ext_only1(SymbolName,Package,KW,Result,Symbol).
atom_symbol_ext_only1(_SymbolName,_Package,kw_external,Symbol,Symbol):-!.
atom_symbol_ext_only1(SymbolName,Package,kw_internal,_Result,_Symbol):- throw('symbol_not_exported'(SymbolName,Package)).
atom_symbol_ext_only1(_SymbolName,_Package,kw_inherited,Result,Symbol):- Result=Symbol.

cl_find_package([S],Obj):-
  cl_symbol_name_or_string_as_atom(S,SN),
  (is_lisp_package(SN)-> PN=SN ; package_nickname(PN,SN)),
  as_package_object(PN,Obj).
cl_find_package(_,[]).

cl_symbol_name_or_string_as_atom(S,S):- atom(S),!.
cl_symbol_name_or_string_as_atom(S,SN):- notrace(catch(name(SN0,S),_,fail)),SN0\==S,!,cl_symbol_name_or_string_as_atom(SN0,SN).
cl_symbol_name_or_string_as_atom(PN,SN):- compound(PN),functor(PN,_P,A),arg(A,PN,S),!, cl_symbol_name_or_string_as_atom(S,SN).
cl_symbol_name_or_string_as_atom(PN,SN):- symbol_package_name(PN,_,SN0),!,cl_symbol_name_or_string_as_atom(SN0,SN).
cl_symbol_name_or_string_as_atom(S,SN):- name(SN,S).

cl_find_symbol([Var|P],Result):-
  ignore(P=[PP]), cl_find_symbol(Var,PP,Result).
cl_find_symbol(String,P,Result):- name(Var,String),reading_package(P),must(find_symbol_from(Var,P,Result)).
cl_find_symbol(_Var,_P,Result):- push_values([[],[]],Result).

reading_package(P):- symbol_value('xxx_package_xxx',ugly(package,P)),!.
reading_package('common-lisp-user').

writing_package(P):- reading_package(P).

cl_intern([Var|P],Result):-
  ignore(P=[PP]),cl_intern(Var,PP,Result).


  
cl_intern(String,P,Result):- downcase_atom(String,Lower),reading_package(P),find_symbol_from(Lower,P,Result),!.
%cl_intern(String,P,Result):- atom_symbol_ext_only(Name,keyword,kw_internal,Symbol):-!,create_kw(Name,Symbol),!.
cl_intern(String,P,Result):- downcase_atom(String,Lower),symbol_case_name(String,P,Symbol),
   asserta(symbol_info(Symbol,P,name,Lower)),asserta(symbol_info(Symbol,P,package,kw_internal)),
   push_values([Symbol,kw_internal],Result).


find_symbol_from(Name,P,Result):- downcase_atom(Name,DCName),find_symbol_from0(DCName,P,Result).
find_symbol_from0(Name,_,Result):- atom_concat(':',KWName,Name),!,atom_concat('kw_',KWName,SymbolCI),prologcase_name(SymbolCI,Symbol),push_values([Symbol,kw_external],Result).
find_symbol_from0(Name,P,Result):- find_symbol_from1(Name,P,Symbol,IntExt),push_values([Symbol,IntExt],Result).
find_symbol_from1(Name,P,Symbol,IntExt):- symbol_info(Symbol, P, name, Name), \+ package_shadowing_symbols(P, Name),!,symbol_info(Symbol, P, package, IntExt).
find_symbol_from1(Name,PW,Symbol,kw_inherited):-  package_use_list(PW,P),symbol_info(Symbol, P, name, Name), \+ package_shadowing_symbols(P, Name),symbol_info(Symbol, P, package, kw_external),!.

as_package_object(P,ugly(package,P)).

% symbol_info(S,P,function_type,macro),dif(macro,FT),clause(symbol_info(S,P,function_type,FT),true)
% ?- forall(symbol_info(Symbol,Package,Prop,Name),format('~q.~n',[symbol_info(Symbol,Package,Prop,Name)])).
% ?- forall(symbol_package_name_data(Symbol,Package,Name),format('~q.~n',[symbol_info(Symbol,Package,name,Name)])).
% ?- forall(symbol_package_function_data(Symbol,Package,FName),format('~q.~n',[symbol_info(Symbol,Package,function,FName)])).

% ?- forall(symbol_package_name(X,Y,Z),format('~q.~n',[symbol_package_name_data(X,Y,Z)])).
symbol_package_name(Symbol,Package,Name):- symbol_info(Symbol,Package,name,Name).
/*
symbol_package_name(Symbol,Package,Name):- symbol_package_name_data(Symbol,Package,Name).
symbol_package_name(Symbol,Package,Name):- user:old_symbol_info(Name, Package, package, _),symbol_case_name(Name,Package,Symbol), 
   \+ symbol_package_name_data(Symbol,Package,Name).
*/
% ?- forall(symbol_package_function(X,Y,Z),format('~q.~n',[symbol_package_function_data(X,Y,Z)])).
symbol_package_function(Symbol,Package,FName):- symbol_info(Symbol,Package,function,FName).
/*
symbol_package_function(Symbol,Package,FName):- symbol_package_function_data(Symbol,Package,FName).
symbol_package_function(Symbol,Package,FName):- symbol_package_name(Symbol,Package,Name),
   old_symbol_info(Name, Package, function_type, _),function_case_name(Name,Package,FName), 
   \+ symbol_package_function_data(Symbol,Package,FName).

fix_symbol_info(Name, Pack, Prop, Value,fixed_symbol_info(Symbol, Pack, Prop, Value)):-
  old_symbol_info(Name, Pack, Prop, Value),
  must(symbol_package_name(Symbol,Pack,Name)).
*/ 
  
  
% ?- forall(old_symbol_info(Name, Pack, Prop, Value), fix_symbol_info(Name, Pack, Prop, Value,Fixed)->format('~q.~n',[Fixed])).

 
% *PACKAGE* becomes xx_package_xx
% %MAKE-PACKAGE becomes pf_make_package
prologcase_name(Atom,ProposedName):- 
  string_lower(Atom,In),string_codes(In,Was),filter_var_chars(Was,CS),name(ProposedName,CS).

symbol_case_name(Atom,Package,ProposedName):- 
  package_prefix(Package,Prefix),!,
  atom_concat_if_new(Prefix,Atom,CasePN),prologcase_name(CasePN,ProposedName),!.

function_case_name(Atom,Package,ProposedName):- 
  package_prefix_sf(Package,Prefix),!,
  atom_concat_if_new(Prefix,Atom,CasePN),prologcase_name(CasePN,ProposedName),!.

package_prefix(A,B):- no_repeats(A,package_prefix0(A,B)).
package_prefix0('common-lisp','').
package_prefix0('keyword','kw_').
package_prefix0('system','sys_').
package_prefix0('common-lisp-user','u_').
package_prefix0('extensions','ext_').
package_prefix0(PN,Pre):- nonvar(PN),package_nickname(Pk,PN),!,package_prefix(Pk,Pre).
package_prefix0(Pk,Pre):- is_lisp_package(Pk),atom_concat(Pk,'_',Pre).

package_prefix_sf('common-lisp','cl_').
package_prefix_sf(PN,Pre):- package_nickname(Pk,PN),!,package_prefix_sf(Pk,Pre).
package_prefix_sf(Pk,Pre):-package_prefix(Pk,Pre0),atom_concat('f_',Pre0,Pre).


atom_concat_if_new(Prefix,Atom,NewAtom):-
  (atom_concat(Prefix,_,Atom)-> NewAtom=Atom ; atom_concat(Prefix,Atom,NewAtom)).

:- fixup_exports.

