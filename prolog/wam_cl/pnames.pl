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



string_list_concat(StrS,Sep,String):- atomic_list_concat(L,Sep,String),atomics_to_strings(L,StrS).
atomics_to_strings([A|L],[S|StrS]):-atom_string(A,S),!,atomics_to_strings(L,StrS).
atomics_to_strings([],[]).

create_kw(Name,Symbol):- atom_concat(':',Make,Name),!,create_kw(Make,Symbol).
create_kw(Name,Symbol):- string_upper(Name,Lower),
   atom_concat('kw_',Name,Symbol),
   asserta(symp:symbol_info(Symbol,pkg_kw,name,Lower)),
   asserta(symp:symbol_info(Symbol,pkg_kw,package,kw_external)).

atom_symbol_test(SymbolName,Symbol):- reading_package(Package),atom_symbol(SymbolName,Package,Symbol),!.

simple_atom_token(SymbolName):- atom_concat('$',_,SymbolName),upcase_atom(SymbolName,SymbolName).
simple_atom_token(SymbolName):- string_upper(SymbolName,UP),string_lower(SymbolName,DOWN),!,UP==DOWN.

atom_symbol(SymbolName,_,Symbol):- simple_atom_token(SymbolName),!,SymbolName=Symbol.
atom_symbol(SymbolName,Package,Symbol):-
  string_upper(SymbolName,SymbolNameU), 
  string_list_concat([SymbolName1|SymbolNameS],":",SymbolNameU),
  atom_symbol_s(SymbolName1,SymbolNameS,Package,Symbol),!.



gen_symbol(SymbolName,Symbol):- prologcase_name(SymbolName,ProposedName),
  gensym(genref_,Ref),
  atomic_list_concat([Ref,ProposedName],'_',Symbol),
  atom_string(SymbolName,String),
  asserta(symp:symbol_info(Symbol,[],name,String)),!.

gen_symbol(SymbolName,Symbol):- throw('@TODO gen a temp symbol'(gen_symbol(SymbolName,Symbol))).

% KEYWORD
atom_symbol_s("",[SymbolName],_UPackage,Symbol):- !,atom_symbol_s(SymbolName,[],pkg_kw,Symbol).
% NO PACKAGE
atom_symbol_s("#",["",SymbolName],UPackage,_Symbol):- throw('@TODO *** - READ from #<INPUT CONCATENATED-STREAM #<INPUT STRING-INPUT-STREAM> #<IO TERMINAL-STREAM>>: token ":BAR" after #: should contain no colon'(atom_symbol_s("#",["",SymbolName],UPackage))).  
% NO PACKAGE
atom_symbol_s("#",[SymbolName],_UPackage,Symbol):- gen_symbol(SymbolName,Symbol).

atom_symbol_s(SymbolName,[],Package,Symbol):- find_symbol_from(SymbolName,Package,Symbol),!.
atom_symbol_s(PName,["",SymbolName],_UPackage,Symbol):-  find_package_or_die(PName,Package),string_upper(SymbolName,StringUpper),cl_intern([StringUpper,Package],Symbol).
atom_symbol_s(PName,[SymbolName],_UPackage,Symbol):- find_package_or_die(PName,Package),atom_symbol_ext_only(SymbolName,Package,Symbol).
atom_symbol_s(SymbolName,[],Package,Symbol):- atom_string(SymbolName,String),
  string_upper(String,StringUpper),
  cl_intern(StringUpper,Package,Symbol).

atom_symbol_ext_only(SymbolName,Package,Symbol):- find_symbol_from1(SymbolName,Package,Result,KW),!,atom_symbol_ext_only1(SymbolName,Package,KW,Result,Symbol).
atom_symbol_ext_only1(_SymbolName,_Package,kw_external,Symbol,Symbol):-!.
atom_symbol_ext_only1(SymbolName,Package,kw_internal,_Result,_Symbol):- throw('symbol_not_exported'(SymbolName,Package)).
atom_symbol_ext_only1(_SymbolName,_Package,kw_inherited,Result,Symbol):- Result=Symbol.



cl_symbol_name(Symbol,Name):- symp:symbol_info(Symbol,_,name,Name),!.
cl_symbol_package(Symbol,Package):- symp:symbol_info(Symbol,Package,name,_),!.

cl_find_symbol(String,Result):- reading_package(P), cl_find_symbol(String,P,Result).
cl_find_symbol(String,P,Result):- reading_package(P),must(find_symbol_from(String,P,Result)).
cl_find_symbol(_Var,_P,Result):- push_values([[],[]],Result).

is_constantp(S):- symp:symbol_info(S, _Package, constant, _Value).
is_constantp(S):- symp:symbol_info(S, pkg_kw, _, _Value).

cl_constantp(S,R):- t_or_nil(is_constantp(S),R).
cl_boundp(Sym,R):- t_or_nil((cl_constantp([Sym],t);symp:symbol_info(Sym,_P,variable,_)),R).
cl_fboundp(Sym,R):- t_or_nil(symp:symbol_info(Sym,_P,function,_),R).

cl_gensym(Symbol):- cl_gensym("G",Symbol).
cl_gensym(S,Symbol):- gensym(S,SymbolName),gen_symbol(SymbolName,Symbol).


cl_find_package(S,Obj):- find_package(S,P),!,must(as_package_object(P,Obj)).
cl_find_package(_,[]).

find_package(ugly(package,UP),P):-!,find_package(UP,P).
find_package(S,S):- is_lisp_package(S),!.
find_package(S,P):- 
  cl_symbol_name_or_string_as_upper(S,SN),
  (package_name(P,SN) ; package_nickname(P,SN)).

find_package_or_die(X,Y):- find_package(X,Y) -> true ; throw(find_package_or_die(X,Y)).  

as_package_object(P,ugly(package,P)).

cl_symbol_name_or_string_as_upper(S,U):- cl_symbol_name_or_string(S,D),string_upper(D,U).

cl_symbol_name_or_string(S,SN):- cl_symbol_name(S,SN),!.
% grabs ugly objects
cl_symbol_name_or_string(C,SN):- compound(C),\+ is_list(C),functor(C,_P,A),arg(A,C,S),!, cl_symbol_name_or_string(S,SN).
cl_symbol_name_or_string(SS,SS):- string(SS),!.
cl_symbol_name_or_string(S,SN):- atom_concat(':',S0,S),!,cl_symbol_name_or_string(S0,SN).
cl_symbol_name_or_string(S,SN):- notrace(catch(text_to_string(S,SN),_,fail)),!.

reading_package(P):- symbol_value('xx_package_xx',UP),find_package(UP,P).
reading_package(pkg_user).
% TODO
writing_package(P):- reading_package(P).


cl_intern(Var,Result):- reading_package(P),cl_intern(Var,P,Result).
  
cl_intern(String,P,Result):- reading_package(P),find_symbol_from(String,P,Result),!.
%cl_intern(String,P,Result):- atom_symbol_ext_only(Name,pkg_kw,kw_internal,Symbol):-!,create_kw(Name,Symbol),!.
cl_intern(String,P,Result):- P=pkg_kw,!,symbol_case_name(String,P,Symbol),
   asserta(symp:symbol_info(Symbol,P,name,String)),
   asserta(symp:symbol_info(Symbol,P,package,kw_external)),
   push_values([Symbol,kw_external],Result).
cl_intern(String,P,Result):- symbol_case_name(String,P,Symbol),
   asserta(symp:symbol_info(Symbol,P,name,String)),
   asserta(symp:symbol_info(Symbol,P,package,kw_internal)),
   push_values([Symbol,kw_internal],Result).



find_symbol_from(Name,P,Result):- cl_symbol_name_or_string(Name,String), find_symbol_from0(String,P,Result).
find_symbol_from0(Name,_,Result):- atom_concat(':',KWName,Name),!,atom_concat('kw_',KWName,SymbolCI),prologcase_name(SymbolCI,Symbol),push_values([Symbol,kw_external],Result).
find_symbol_from0(Name,P,Result):- find_symbol_from1(Name,P,Symbol,IntExt),push_values([Symbol,IntExt],Result).
find_symbol_from1(Name,P,Symbol,IntExt):- symp:symbol_info(Symbol, P, name, Name), \+ package_shadowing_symbols(P, Name),!,symp:symbol_info(Symbol, P, package, IntExt).
find_symbol_from1(Name,PW,Symbol,kw_inherited):-  package_use_list(PW,P),symp:symbol_info(Symbol, P, name, Name), \+ package_shadowing_symbols(P, Name),symp:symbol_info(Symbol, P, package, kw_external),!.


% symp:symbol_info(S,P,function_type,macro),dif(macro,FT),clause(symp:symbol_info(S,P,function_type,FT),true)
% ?- forall(symp:symbol_info(Symbol,Package,Prop,Name),format('~q.~n',[symp:symbol_info(Symbol,Package,Prop,Name)])).
% ?- forall(symbol_package_name_data(Symbol,Package,Name),format('~q.~n',[symp:symbol_info(Symbol,Package,name,Name)])).
% ?- forall(symbol_package_function_data(Symbol,Package,FName),format('~q.~n',[symp:symbol_info(Symbol,Package,function,FName)])).

% ?- forall(symbol_package_name(X,Y,Z),format('~q.~n',[symbol_package_name_data(X,Y,Z)])).
symbol_package_name(Symbol,Package,Name):- symp:symbol_info(Symbol,Package,name,Name).
/*
symbol_package_name(Symbol,Package,Name):- symbol_package_name_data(Symbol,Package,Name).
symbol_package_name(Symbol,Package,Name):- user:old_symbol_info(Name, Package, package, _),symbol_case_name(Name,Package,Symbol), 
   \+ symbol_package_name_data(Symbol,Package,Name).
*/
% ?- forall(symbol_package_function(X,Y,Z),format('~q.~n',[symbol_package_function_data(X,Y,Z)])).
symbol_package_function(Symbol,Package,FName):- symp:symbol_info(Symbol,Package,function,FName).
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
package_prefix0(pkg_cl,'').
package_prefix0(pkg_kw,'kw_').
package_prefix0(pkg_sys,'sys_').
package_prefix0(pkg_user,'u_').
package_prefix0(pkg_ext,'ext_').
package_prefix0(PN,Pre):- nonvar(PN),package_nickname(Pk,PN),!,package_prefix(Pk,Pre).
package_prefix0(Pk,Pre):- is_lisp_package(Pk),atom_concat('pkg_',P,Pk),atom_concat(P,'_',Pre).

package_prefix_sf(pkg_cl,'cl_').
package_prefix_sf(PN,Pre):- package_nickname(Pk,PN),!,package_prefix_sf(Pk,Pre).
package_prefix_sf(Pk,Pre):- package_prefix(Pk,Pre0),atom_concat('f_',Pre0,Pre).


atom_concat_if_new(Prefix,Atom,NewAtom):-
  (atom_concat(Prefix,_,Atom)-> NewAtom=Atom ; atom_concat(Prefix,Atom,NewAtom)).

:- fixup_exports.

