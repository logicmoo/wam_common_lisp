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
:- module(package, []).
:- set_module(class(library)).
:- include('header').

:- multifile(xlisting_config:xlisting_always/1).
:- dynamic(xlisting_config:xlisting_always/1).

xlisting_config:xlisting_always(G):- G=package:_, current_predicate(_,G),predicate_property(G,dynamic),
  \+ predicate_property(G,imported_from(_)).


f_list_all_packages(Ret):- findall(P,package_name(P,_),List),list_to_set(List,Ret).

wl:init_args(x,sys_select_package).
wl:interned_eval("
(defmacro in-package (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (si::select-package ,(string name))))
").
% SYS::SELECT-PACKAGE
f_sys_select_package(S,Package):- find_package_or_die(S,Package),
   f_sys_set_symbol_value('xx_package_xx',Package).

wl:init_args(x,use_package).
f_use_package(Package,R):- reading_package(CurrentPackage),
                       f_use_package(Package,CurrentPackage,R).

f_use_package(Package,CurrentPackage, t):- Package==CurrentPackage,!.
f_use_package(Package,CurrentPackage, R):- 
  find_package(Package,Package0),
  Package\==Package0,!,
  f_use_package(Package0,CurrentPackage, R).
f_use_package(Package,CurrentPackage, R):- 
  find_package(CurrentPackage,CurrentPackage0),
  CurrentPackage0\==CurrentPackage,!,
  f_use_package(Package,CurrentPackage0, R).
f_use_package(Package,CurrentPackage, t):- 
   assert_lsp([Package,CurrentPackage],package_use_list(CurrentPackage,Package)),
   dbginfo(todo(check_for+package_symbolconflicts(package_use_list(CurrentPackage,Package)))).

 
wl:init_args(1,defpackage).
sf_defpackage(_ReplEnv,Name,Keys,R):- f_make_package(Name,Keys,R).


wl:init_args(1,make_package).
f_make_package(L,B,T):- to_prolog_string_if_needed(L,Loc),!,f_make_package(Loc,B,T).
f_make_package(AName,List,Package):-
  text_to_string(AName,Name),  
  atom_concat_or_rtrace(pkg_,Name,Down),prologcase_name(Down,Package),
  add_opv(Package,type_of,package),
  asserta_if_new(package_name(Package,Name)),
  init_instance_slots(claz_package,2,Package,List), 
  string_upper(Name,UName),
  (Name==UName -> true ; add_opv(Package,kw_nicknames,UName)).
  %instance_opv(Package,claz_package,[]).

f_find_package(S,Obj):- find_package(S,Package),!,always(as_package_object(Package,Obj)).
f_find_package(_,[]).

pl_package_name(S,Name):- find_package(S,Package),(get_opv(Package,name,Name)->true;package_name(Package,Name)).

f_package_name(P,N):- pl_package_name(P,S),to_lisp_string(S,N).

find_package(S,S):- is_packagep(S),!.
find_package('$OBJ'(claz_package,UP),Package):- !, find_package(UP,Package),!.
find_package(Obj,Res):- to_prolog_string_if_needed(Obj,F),!,find_package(F,Res).
find_package(S,Package):- 
  as_string_upper(S,SN),!,
  (package_name(Package,SN) ; package_nicknames(Package,SN) ; get_opv_i(Package,nicknames,SN) ; (atom_concat('SB!',_,SN)->Package=pkg_sys)),!.

find_package_or_die(X,Y):-
 find_package(X,Y) -> true ; break,trace_or_throw(find_package_or_die(X,Y)).  

as_package_object(Package,'$OBJ'(claz_package,Package)).


reading_package(Package):- always((get_opv('xx_package_xx',symbol_value,UP),find_package(UP,Package))),!.
reading_package(pkg_user).
% TODO
writing_package(Package):- reading_package(Package).


package_unintern_symbol(Package,Symbol):- 
  retractall(package:package_shadowing_symbols(Package,Symbol)),
  retractall(package:package_internal_symbols(Package,_,Symbol)),
  retractall(package:package_external_symbols(Package,_,Symbol)).



package_find_symbol_or_missing(String,Package,OldSymbol,IntExt):- package_find_symbol(String,Package,OldSymbol,IntExt),!.
package_find_symbol_or_missing(_String,_Package,_NoSymbol,'$missing').

%package_find_symbol(String,_,Symbol,kw_external):- atom_concat_or_rtrace(':',KWName,String),!,atom_concat_or_rtrace('kw_',KWName,SymbolCI),prologcase_name(SymbolCI,Symbol).

package_find_symbol(String,Package,Symbol,Found):- to_prolog_string_if_needed(String,PlString),!,package_find_symbol(PlString,Package,Symbol,Found).
package_find_symbol(String,Package,Symbol,kw_external):- package_external_symbols(Package,String,Symbol),!.
package_find_symbol(String,Package,Symbol,kw_internal):- package_internal_symbols(Package,String,Symbol),!.
package_find_symbol(String,Package,Symbol,kw_internal):- fail, get_opv(Symbol,symbol_name,String),get_opv(Symbol,symbol_package,Package),
  ((Package == pkg_cl -> (retract_all_1(soops:o_p_v(Symbol,_,_)), writeq(retract_all_1(soops:o_p_v(Symbol,_,_))))
    ;
  ((assertz(package:package_internal_symbols(Package,String,Symbol)),!,
  writeq(package:package_internal_symbols(Package,String,Symbol)),nl)))),!.
package_find_symbol(String,PW,Symbol,kw_inherited):-  package_use_list(PW,Package),package_external_symbols(Package,String,Symbol),!.
%package_find_symbol(String,Package,Symbol,Found):-  to_prolog_string_if_needed(String,PlString),!,package_find_symbol(PlString,Package,Symbol,Found).


retract_all_1(G):- forall(retract(G),true).

grab_missing_symbols:- 
 forall((get_opv(Symbol,symbol_name,String),get_opv(Symbol,symbol_package,Package)),package_find_symbol(String,Package,_,_)).
  
% @TODO Confirm symbol shadowing is correct
f_import(Symbol,Result):- reading_package(Package),f_import(Symbol,Package,Result).
%f_import(String,Package,R):- to_prolog_string_if_needed(String,PlString),!,f_import(PlString,Package,R).
f_import(Symbol,Pack,t):- 
   find_package_or_die(Pack,Package),
   pl_import(Package,Symbol).

pl_import(Pack,List):- is_list(List),maplist(pl_import(Pack),List).
pl_import(Package,Symbol):-
   pl_symbol_name(Symbol,String),
   package_find_symbol_or_missing(String,Package,OldSymbol,IntExt),!,
   package_import_symbol_step2(Package,Symbol,String,OldSymbol,IntExt).

package_import_symbol_step2(Package,Symbol,String,_OldSymbol,'$missing'):-
   assert_lsp(Symbol,package:package_internal_symbols(Package,String,Symbol)).
package_import_symbol_step2(_Package,Symbol,_String,OldSymbol,_IntExt):- Symbol == OldSymbol,!.
package_import_symbol_step2(Package,Symbol,String,OldSymbol,kw_iherited):-
   assert_lsp(Symbol,package:package_shadowing_symbols(Package,OldSymbol)),
   assert_lsp(Symbol,package:package_internal_symbols(Package,String,Symbol)).
package_import_symbol_step2(Package,Symbol,String,OldSymbol,kw_external):-
   retract(package:package_external_symbols(Package,String,OldSymbol)),
   assert_lsp(Symbol,package:package_shadowing_symbols(Package,OldSymbol)),
   assert_lsp(Symbol,package:package_internal_symbols(Package,String,Symbol)).
package_import_symbol_step2(Package,Symbol,String,OldSymbol,kw_internal):-
   ignore(retract(package:package_internal_symbols(Package,String,OldSymbol))),
   ((OldSymbol \== Symbol,nonvar(OldSymbol)) -> assert_lsp(Symbol,package:package_shadowing_symbols(Package,OldSymbol)) ; true),
   assert_lsp(Symbol,package:package_internal_symbols(Package,String,Symbol)).



% @TODO Confirm symbol shadowing is correct 
f_export(Symbol,Result):- reading_package(Package),f_export(Symbol,Package,Result).
%f_export(String,Package,R):- to_prolog_string_if_needed(String,PlString),!,f_export(PlString,Package,R).
f_export(Symbol,Pack,t):- 
   find_package_or_die(Pack,Package),
   pl_export(Package,Symbol).

pl_export(Pack,List):- is_list(List),maplist(pl_export(Pack),List).
pl_export(Package,Symbol):- 
   pl_symbol_name(Symbol,String),
   package_find_symbol_or_missing(String,Package,OldSymbol,IntExt),!,
   package_export_symbol_step2(Package,Symbol,String,OldSymbol,IntExt).

package_export_symbol_step2(Package,Symbol,String,_OldSymbol,'$missing'):-
   assert_lsp(Symbol,package:package_external_symbols(Package,String,Symbol)).
package_export_symbol_step2(_Package,Symbol,_String,OldSymbol,kw_exported):- Symbol == OldSymbol,!.
package_export_symbol_step2(Package,Symbol,String,OldSymbol,kw_inheritied):-
   assert_lsp(Symbol,package:package_shadowing_symbols(Package,OldSymbol)),
   assert_lsp(Symbol,package:package_external_symbols(Package,String,Symbol)).
package_export_symbol_step2(Package,Symbol,String,OldSymbol,kw_external):-
   retract(package:package_external_symbols(Package,String,OldSymbol)),
   assert_lsp(Symbol,package:package_shadowing_symbols(Package,OldSymbol)),
   assert_lsp(Symbol,package:package_external_symbols(Package,String,Symbol)).
package_export_symbol_step2(Package,Symbol,String,OldSymbol,kw_internal):-
   retract(package:package_internal_symbols(Package,String,OldSymbol)),
   assert_lsp(Symbol,package:package_shadowing_symbols(Package,OldSymbol)),
   assert_lsp(Symbol,package:package_external_symbols(Package,String,Symbol)).


f_unexport(Symbol,Result):- reading_package(Package),f_unexport(Symbol,Package,Result).
f_unexport(List,Pack,t):- is_list(List),maplist([Symbol]>>f_unexport(Symbol,Pack,_),List).
f_unexport(Symbol,Pack,t):- 
   find_package_or_die(Pack,Package),
   pl_symbol_name(Symbol,String),
   package_find_symbol_or_missing(String,Package,OldSymbol,IntExt),!,
   package_unexport_symbol_step2(Package,Symbol,String,OldSymbol,IntExt).

package_unexport_symbol_step2(_Package,Symbol,_String,OldSymbol,kw_internal):- OldSymbol==Symbol.
package_unexport_symbol_step2(_Package,_Symbol,_String,_OldSymbol,'$missing'):-!.
package_unexport_symbol_step2(Package,Symbol,String,OldSymbol,_):-
   retract(package:package_external_symbols(Package,String,OldSymbol)) -> 
     assert_lsp(Symbol,package:package_external_symbols(Package,String,Symbol));
     true.


f_shadow(Symbol,Result):- reading_package(Package),f_shadow(Symbol,Package,Result).
f_shadow(List,Pack,t):- is_list(List),maplist([Symbol]>>f_shadow(Symbol,Pack,_),List).
f_shadow(Symbol,Pack,t):- 
   find_package_or_die(Pack,Package),
   pl_symbol_name(Symbol,String),
   package_find_symbol_or_missing(String,Package,OldSymbol,IntExt),!,
   package_shadow_symbol_step2(Package,String,OldSymbol,IntExt).


package_shadow_symbol_step2(_Package,_String,_OldSymbol,kw_external).
package_shadow_symbol_step2(_Package,_String,_OldSymbol,kw_internal).
package_shadow_symbol_step2( Package,String,_OldSymbol,'$missing'):-
   make_fresh_internal_symbol(Package,String,_Symbol).
package_shadow_symbol_step2(Package,String,OldSymbol,kw_inherited):-
   assert_lsp(OldSymbol,package:package_shadowing_symbols(Package,OldSymbol)),
   make_fresh_internal_symbol(Package,String,_Symbol).


% caller is responsible for avoiding conflicts
make_fresh_internal_symbol(pkg_kw,String,Symbol):- !, create_keyword(String,Symbol).
make_fresh_internal_symbol(Package,String,Symbol):- 
   (var(Symbol)->symbol_case_name(String,Package,Symbol);true),
   create_symbol(String,Package,Symbol),
   assert_lsp(Symbol,package:package_internal_symbols(Package,String,Symbol)).



is_packagep(P):- package_name(P,_). 




print_package_or_hash(P):-short_package_or_hash(P,O),write(O).

   short_package_or_hash(Var,O):- var(Var),!,O=(Var).
   short_package_or_hash([],O):- !,O=("#").
   short_package_or_hash(P,O):- pl_package_name(P,Symbol),shorter_name(Symbol,Short),!,O=(Short).
   short_package_or_hash(P,O):- pl_package_name(P,N),!,O=(N).
   short_package_or_hash(P,O):- O=(failed_short_package_or_hash(P)).

   shorter_name("COMMON-LISP-USER","U").
   shorter_name("COMMON-LISP","CL").
   shorter_name("SYSTEM","SYS").
   shorter_name(PN,NN):- package_nicknames(PN,NN),atom_length(PN,B),atom_length(NN,A),A<B.
   %symbol printer might just use 
   %shorter_name("COMMON-LISP-USER","CL-USER").
   %shorter_name("SYSTEM","SYS").
   %shorter_name("EXTENSIONS","SB-EXT").
   shorter_name(S,S).


:- dynamic package_name/2.
:- dynamic package_nicknames/2.
:- dynamic package_use_list/2.
:- dynamic package_shadowing_symbols/2.
:- dynamic package_external_symbols/3.
:- dynamic package_internal_symbols/3.

package_name(pkg_cl,"COMMON-LISP").
package_name(pkg_clos,"CLOS"):- \+ current_prolog_flag(wamcl_pcl,true).
package_name(pkg_os,"POSIX").
package_name(pkg_threads,"THREADS").
package_name(pkg_charset,"CHARSET").
package_name(pkg_sys,"SYSTEM").
package_name(pkg_ext,"SB-EXT").
package_name(pkg_user,"COMMON-LISP-USER").
package_name(pkg_kw,"KEYWORD").
/*
package_name(pkg_sys,"PROLOG").
package_name(pkg_custom,"CUSTOM").
package_name(pkg_debug,"DEBUG").
package_name(pkg_ffi,"FFI").
package_name(pkg_format,"FORMAT").
package_name(pkg_loop,"LOOP").
package_name(pkg_readline,"READLINE").
package_name(pkg_regexp,"REGEXP").
package_name(pkg_screen,"SCREEN").
package_name(pkg_socket,"SOCKET").
package_name(pkg_threads,"THREADS").
package_name(pkg_tl,"TOP-LEVEL").
package_name(pkg_xp,"XP").
package_name(pkg_gray,"GRAY").
package_name(pkg_gstream,"GSTREAM").
package_name(pkg_i18n,"I18N").
package_name(pkg_java,"JAVA").
package_name(pkg_jvm,"JVM").
package_name(pkg_precompiler,"PRECOMPILER").
package_name(pkg_profiler,"PROFILER").
package_name(pkg_sequence,"SEQUENCE").
*/
:- decl_mapped_opv(claz_package,[name=package_name]).

package_nicknames(pkg_cl, "CL").
package_nicknames(pkg_cl, "LISP").
package_nicknames(pkg_cl, "EMACS-CL").

package_nicknames(pkg_user, "U").
package_nicknames(pkg_user, "USER").
package_nicknames(pkg_user, "CL-USER").
package_nicknames(pkg_user, "EMACS-CL-USER").

package_nicknames(pkg_ext, "SB!EXT").

%package_nicknames(pkg_sys, "P").
%package_nicknames(pkg_sys, "INT").

package_nicknames(pkg_sys, "SYS").
package_nicknames(pkg_sys, "EXCL").
package_nicknames(pkg_sys, "EXT").
package_nicknames(pkg_sys, "EXTENSIONS").
package_nicknames(pkg_sys, "C").
package_nicknames(pkg_sys, "SI").
package_nicknames(pkg_sys, "SB!C").
package_nicknames(pkg_sys, "SB!SYS").

package_nicknames(pkg_sys, "CCL").
package_nicknames(pkg_sys, "WAM-CL").

/*
package_nicknames(pkg_clos, "MOP").
package_nicknames(pkg_clos, "PCL").
package_nicknames(pkg_clos, "SB-PCL").
package_nicknames(pkg_clos, "CLOS").
*/
package_nicknames(pkg_sys, "MOP").
package_nicknames(pkg_sys, "PCL").
package_nicknames(pkg_sys, "SB-PCL").
package_nicknames(pkg_sys, "CLOS"):- \+ current_prolog_flag(wamcl_pcl,true).

package_nicknames(pkg_os, "OS").
/*
package_nicknames(pkg_tl, "TPL").
package_nicknames(pkg_precompiler, "PRE").
package_nicknames(pkg_profiler, "PROF").
*/


:- decl_mapped_opv(claz_package,[nicknames=package_nicknames]).

package_use_list(pkg_user, pkg_cl).
package_use_list(pkg_user, pkg_clos):- current_prolog_flag(wamcl_pcl,true).
package_use_list(pkg_user, pkg_threads).
package_use_list(pkg_user, pkg_os).
package_use_list(pkg_user, pkg_sys).
package_use_list(pkg_user, pkg_charset).
package_use_list(pkg_user, pkg_ext).
%package_use_list(pkg_user, pkg_sys).
%package_use_list(pkg_user, pkg_custom).


package_use_list(pkg_cl, pkg_sys).
package_use_list(pkg_cl, pkg_clos):- current_prolog_flag(wamcl_pcl,true).
package_use_list(pkg_cl, pkg_ext).

/*
package_use_list(pkg_clos, pkg_cl).
package_use_list(pkg_clos, pkg_ext).
package_use_list(pkg_clos, pkg_sys).
package_use_list(pkg_clos, pkg_sys).
*/
/*
package_use_list(pkg_sys, pkg_cl).
package_use_list(pkg_sys, pkg_ext).
package_use_list(pkg_sys, pkg_sys).
package_use_list(pkg_sys, pkg_clos):- current_prolog_flag(wamcl_pcl,true).
*/
/*
package_use_list(pkg_sys, pkg_custom).
package_use_list(pkg_sys, pkg_gray).
package_use_list(pkg_sys, pkg_gstream).
package_use_list(pkg_sys, pkg_i18n).
package_use_list(pkg_sys, pkg_os).
package_use_list(pkg_sys, pkg_socket).
*/

package_use_list(pkg_sys, pkg_threads).
%package_use_list(pkg_sys, pkg_os).
package_use_list(pkg_sys, pkg_cl).
package_use_list(pkg_sys, pkg_ext).


package_use_list(pkg_os, pkg_cl).
package_use_list(pkg_os, pkg_ext).

package_use_list(pkg_threads, pkg_cl).
package_use_list(pkg_threads, pkg_ext).
package_use_list(pkg_threads, pkg_sys).
package_use_list(pkg_threads, pkg_os).

/*
package_use_list(pkg_sys, pkg_sys).
package_use_list(pkg_tl, pkg_cl).
package_use_list(pkg_tl, pkg_ext).

package_use_list(pkg_ext, pkg_cl).
package_use_list(pkg_ext, pkg_custom).
package_use_list(pkg_ext, pkg_gray).
package_use_list(pkg_ext, pkg_gstream).
package_use_list(pkg_ext, pkg_i18n).
package_use_list(pkg_ext, pkg_os).
package_use_list(pkg_ext, pkg_socket).
package_use_list(pkg_ext, pkg_threads).
package_use_list(pkg_ext, pkg_clos):- current_prolog_flag(wamcl_pcl,true).
package_use_list(pkg_ext, pkg_sys).
package_use_list(pkg_ext, pkg_sys).

package_use_list(pkg_ffi, pkg_cl).
package_use_list(pkg_ffi, pkg_ext).
package_use_list(pkg_format, pkg_cl).
package_use_list(pkg_format, pkg_ext).

package_use_list(pkg_jvm, pkg_cl).
package_use_list(pkg_jvm, pkg_ext).
package_use_list(pkg_jvm, pkg_sys).
package_use_list(pkg_java, pkg_cl).
package_use_list(pkg_java, pkg_ext).
package_use_list(pkg_java, pkg_sys).

package_use_list(pkg_loop, pkg_cl).

package_use_list(pkg_precompiler, pkg_cl).
package_use_list(pkg_precompiler, pkg_ext).
package_use_list(pkg_precompiler, pkg_sys).
package_use_list(pkg_profiler, pkg_cl).
package_use_list(pkg_profiler, pkg_ext).
package_use_list(pkg_readline, pkg_cl).
package_use_list(pkg_readline, pkg_ext).
package_use_list(pkg_readline, pkg_ffi).
package_use_list(pkg_regexp, pkg_cl).
package_use_list(pkg_screen, pkg_cl).
package_use_list(pkg_screen, pkg_ext).
package_use_list(pkg_sequence, pkg_cl).

package_use_list(pkg_xp, pkg_cl).
*/

:- decl_mapped_opv(claz_package,[uses=package_use_list]).



symbol_case_name(Name,Package,ProposedName):- 
  to_prolog_string(Name,String),
  package_symbol_prefix(Package,Prefix),!,
  atom_concat_if_new(Prefix,String,CasePN),prologcase_name(CasePN,ProposedName),!.

function_case_name(BindType,String,Package,ProposedName):- is_list(String),notrace(catch(atomic_list_concat(String,'_',NewName),_,fail)),!,
  function_case_name(BindType,NewName,Package,ProposedName).
function_case_name(BindType,String,Package,ProposedName):- 
  package_function_prefix(BindType,Package,Prefix),!,
  atom_concat_if_new(Prefix,String,CasePN),prologcase_name(CasePN,ProposedName),!.

package_function_prefix(BindType,A,B):- no_repeats(A,package_fprefix(BindType,A,B)).
package_fprefix(kw_function,Pk,Pre):- package_symbol_prefix(Pk,Pre0),atom_concat_or_rtrace('f_',Pre0,Pre).
package_fprefix(kw_special,Pk,Pre):- package_symbol_prefix(Pk,Pre0),atom_concat_or_rtrace('sf_',Pre0,Pre).
package_fprefix(kw_macro,Pk,Pre):- package_symbol_prefix(Pk,Pre0),atom_concat_or_rtrace('mf_',Pre0,Pre).
package_fprefix(kw_operator,Pk,Pre):- trace,package_symbol_prefix(Pk,Pre0),atom_concat_or_rtrace('sf_',Pre0,Pre).

package_symbol_prefix(A,B):- no_repeats(B,package_prefix(A,B)).
package_prefix(pkg_cl,'').
package_prefix(pkg_sys,'sys_').
package_prefix(pkg_sys,'clos_').
package_prefix(pkg_ext,'ext_').
package_prefix(pkg_user,'u_').
package_prefix(pkg_kw,'kw_').
package_prefix(PN,Pre):- nonvar(PN),package_nicknames(Pk,PN),!,package_prefix(Pk,Pre).
package_prefix(Pk,Pre):- is_packagep(Pk),atom_concat_or_rtrace('pkg_',Package,Pk),atom_concat_or_rtrace(Package,'_',Pre).


save_pi:- tell('pi2.data'),
   forall(member(Assert,[
     package_external_symbols(_,_,_),
     package_internal_symbols(_,_,_),
     package_shadowing_symbols(_,_)]),
   forall(clause(package:Assert,true),
      ignore((format('~q.~n',[Assert]))))), told.

:- include('pi.data').

:- fixup_exports.
