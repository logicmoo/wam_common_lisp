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
%:- set_prolog_flag(verbose_load,full).
:- set_prolog_flag(verbose,normal).
%:- set_prolog_flag(verbose_autoload,true).



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
                        


% *PACKAGE* becomes xx_package_xx
% %MAKE-PACKAGE becomes pf_make_package
prologcase_name(String,ProposedName):- 
  string_lower(String,In),string_codes(In,Was),filter_var_chars(Was,CS),name(ProposedName,CS).

symbol_case_name(String,Package,ProposedName):- 
  package_symbol_prefix(Package,Prefix),!,
  atom_concat_if_new(Prefix,String,CasePN),prologcase_name(CasePN,ProposedName),!.

function_case_name(String,Package,ProposedName):- 
  package_function_prefix(Package,Prefix),!,
  atom_concat_if_new(Prefix,String,CasePN),prologcase_name(CasePN,ProposedName),!.

package_function_prefix(A,B):- no_repeats(A,package_fprefix(A,B)).
package_fprefix(pkg_cl,'cl_').
package_fprefix(Pk,Pre):- Pk\==pkg_cl, package_symbol_prefix(Pk,Pre0),atom_concat('f_',Pre0,Pre).

package_symbol_prefix(A,B):- no_repeats(A,package_prefix(A,B)).
package_prefix(pkg_cl,'').
package_prefix(pkg_kw,'kw_').
package_prefix(pkg_sys,'sys_').
package_prefix(pkg_user,'u_').
package_prefix(pkg_ext,'ext_').
package_prefix(PN,Pre):- nonvar(PN),package_nickname(Pk,PN),!,package_prefix(Pk,Pre).
package_prefix(Pk,Pre):- is_lisp_package(Pk),atom_concat('pkg_',Package,Pk),atom_concat(Package,'_',Pre).


atom_concat_if_new(Prefix,Atom,NewAtom):-
  (atom_concat(Prefix,_,Atom)-> NewAtom=Atom ; atom_concat(Prefix,Atom,NewAtom)).



as_string_upper(C,SN):- compound(C),\+ is_list(C),functor(C,_P,A),arg(A,C,S),!, as_string_upper(S,SN).
as_string_upper(S,U):- cl_string(S,D),string_upper(D,U).

cl_string(SS,SS):- string(SS),!.
cl_string(S,SN):- is_symbolp(S),cl_symbol_name(S,SN),!.
% grabs ugly objects
cl_string(S,SN):- atom_concat(':',S0,S),!,cl_string(S0,SN).
cl_string(S,SN):- notrace(catch(text_to_string(S,SN),_,fail)),!.



:- fixup_exports.

