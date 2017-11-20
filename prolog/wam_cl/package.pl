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



cl_in_package(S,Package):- find_package_or_die(S,Package),
   set_symbol_value('xx_package_xx',Package).

cl_use_package(Package,R):- reading_package(CurrentPackage),
                       cl_use_package(Package,CurrentPackage,R).

cl_use_package(Package,CurrentPackage, t):- Package==CurrentPackage,!.
cl_use_package(Package,CurrentPackage, R):- 
  find_package(Package,Package0),
  Package\==Package0,!,
  cl_use_package(Package0,CurrentPackage, R).
cl_use_package(Package,CurrentPackage, R):- 
  find_package(CurrentPackage,CurrentPackage0),
  CurrentPackage0\==CurrentPackage,!,
  cl_use_package(Package,CurrentPackage0, R).
cl_use_package(Package,CurrentPackage, t):- 
   asserta_if_new(package_use_list(CurrentPackage,Package)),
   dmsg(todo(check_for+package_symbolconflicts(package_use_list(CurrentPackage,Package)))).

 
cl_defpackage(Name,P1,P2,P3,R):- do_defpackage(Name,[P1,P2,P3],R).
cl_defpackage(Name,P1,P2,R):- do_defpackage(Name,[P1,P2],R).
cl_defpackage(Name,P1,R):- do_defpackage(Name,[P1],R).
cl_defpackage(Name,R):- do_defpackage(Name,[],R).


do_defpackage(AName,List,Package):-
  atom_string(AName,Name),  
  atom_concat(pkg_,Name,Down),prologcase_name(Down,Package),
  asserta(package_name(Package,Name)),
  init_slot_props(claz_package,2,Package,List), 
  string_upper(Name,UName),
  (Name==UName -> true ; add_kw_opv(Package,kw_nicknames,UName)).
  %instance_opv(Package,claz_package,[]).

cl_find_package(S,Obj):- find_package(S,Package),!,must(as_package_object(Package,Obj)).
cl_find_package(_,[]).

cl_package_name(S,Name):- find_package(S,Package),get_opv(Package,name,Name).

find_package('$OBJ'(package,UP),Package):-!,find_package(UP,Package).
find_package(S,S):- is_lisp_package(S),!.
find_package(S,Package):- 
  as_string_upper(S,SN),
  (package_name(Package,SN) ; package_nicknames(Package,SN)),!.

find_package_or_die(X,Y):- find_package(X,Y) -> true ; throw(find_package_or_die(X,Y)).  

as_package_object(Package,'$OBJ'(package,Package)).


reading_package(Package):- symbol_value('xx_package_xx',UP),find_package(UP,Package).
reading_package(pkg_user).
% TODO
writing_package(Package):- reading_package(Package).

package_find_symbol_or_missing(String,Package,OldSymbol,IntExt):- package_find_symbol(String,Package,OldSymbol,IntExt),!.
package_find_symbol_or_missing(_String,_Package,_NoSymbol,'$missing').

package_find_symbol(String,_,Symbol,kw_external):- atom_concat(':',KWName,String),!,atom_concat('kw_',KWName,SymbolCI),prologcase_name(SymbolCI,Symbol).
package_find_symbol(String,Package,Symbol,kw_external):- package_external_symbols(Package,String,Symbol),!.
package_find_symbol(String,Package,Symbol,kw_internal):- package_internal_symbols(Package,String,Symbol),!.
package_find_symbol(String,PW,Symbol,kw_inherited):-  package_use_list(PW,Package),package_external_symbols(Package,String,Symbol).

% @TODO Add symbol shadowing 
cl_import(Symbol,Result):- reading_package(Package),cl_import(Symbol,Package,Result).
cl_import(List,Pack,t):- is_list(List),maplist([Symbol]>>cl_import(Symbol,Pack,_),List).
cl_import(Symbol,Pack,t):- 
   find_package_or_die(Pack,Package),
   cl_symbol_name(Symbol,String),
   package_find_symbol_or_missing(String,Package,OldSymbol,IntExt),!,
   package_import_symbol_step2(Package,Symbol,String,OldSymbol,IntExt).

package_import_symbol_step2(Package,Symbol,String,_OldSymbol,'$missing'):-
   assert_if_new(package:package_internal_symbols(Package,String,Symbol)).
package_import_symbol_step2(_Package,Symbol,_String,OldSymbol,_IntExt):- Symbol == OldSymbol,!.
package_import_symbol_step2(Package,Symbol,String,OldSymbol,kw_iherited):-
   assert_if_new(package:package_shadowing_symbols(Package,OldSymbol)),
   assert_if_new(package:package_internal_symbols(Package,String,Symbol)).
package_import_symbol_step2(Package,Symbol,String,OldSymbol,kw_external):-
   retract(package:package_external_symbols(Package,String,OldSymbol)),
   assert_if_new(package:package_shadowing_symbols(Package,OldSymbol)),
   assert_if_new(package:package_internal_symbols(Package,String,Symbol)).
package_import_symbol_step2(Package,Symbol,String,OldSymbol,kw_internal):-
   retract(package:package_internal_symbols(Package,String,OldSymbol)),
   assert_if_new(package:package_shadowing_symbols(Package,OldSymbol)),
   assert_if_new(package:package_internal_symbols(Package,String,Symbol)).



cl_export(Symbol,Result):- reading_package(Package),cl_export(Symbol,Package,Result).
cl_export(List,Pack,t):- is_list(List),maplist([Symbol]>>cl_export(Symbol,Pack,_),List).
cl_export(Symbol,Pack,t):- 
   find_package_or_die(Pack,Package),
   cl_symbol_name(Symbol,String),
   package_find_symbol_or_missing(String,Package,OldSymbol,IntExt),!,
   package_export_symbol_step2(Package,Symbol,String,OldSymbol,IntExt).

package_export_symbol_step2(Package,Symbol,String,_OldSymbol,'$missing'):-
   assert_if_new(package:package_external_symbols(Package,String,Symbol)).
package_export_symbol_step2(_Package,Symbol,_String,OldSymbol,kw_exported):- Symbol == OldSymbol,!.
package_export_symbol_step2(Package,Symbol,String,OldSymbol,kw_inheritied):-
   assert_if_new(package:package_shadowing_symbols(Package,OldSymbol)),
   assert_if_new(package:package_external_symbols(Package,String,Symbol)).
package_export_symbol_step2(Package,Symbol,String,OldSymbol,kw_external):-
   retract(package:package_external_symbols(Package,String,OldSymbol)),
   assert_if_new(package:package_shadowing_symbols(Package,OldSymbol)),
   assert_if_new(package:package_external_symbols(Package,String,Symbol)).
package_export_symbol_step2(Package,Symbol,String,OldSymbol,kw_internal):-
   retract(package:package_internal_symbols(Package,String,OldSymbol)),
   assert_if_new(package:package_shadowing_symbols(Package,OldSymbol)),
   assert_if_new(package:package_external_symbols(Package,String,Symbol)).


cl_unexport(Symbol,Result):- reading_package(Package),cl_unexport(Symbol,Package,Result).
cl_unexport(List,Pack,t):- is_list(List),maplist([Symbol]>>cl_unexport(Symbol,Pack,_),List).
cl_unexport(Symbol,Pack,t):- 
   find_package_or_die(Pack,Package),
   cl_symbol_name(Symbol,String),
   package_find_symbol_or_missing(String,Package,OldSymbol,IntExt),!,
   package_unexport_symbol_step2(Package,Symbol,String,OldSymbol,IntExt).

package_unexport_symbol_step2(_Package,Symbol,_String,OldSymbol,kw_internal):- OldSymbol==Symbol.
package_unexport_symbol_step2(_Package,_Symbol,_String,_OldSymbol,'$missing'):-!.
package_unexport_symbol_step2(Package,Symbol,String,OldSymbol,_):-
   retract(package:package_external_symbols(Package,String,OldSymbol)) -> 
     assert_if_new(package:package_external_symbols(Package,String,Symbol));
     true.


cl_shadow(Symbol,Result):- reading_package(Package),cl_shadow(Symbol,Package,Result).
cl_shadow(List,Pack,t):- is_list(List),maplist([Symbol]>>cl_shadow(Symbol,Pack,_),List).
cl_shadow(Symbol,Pack,t):- 
   find_package_or_die(Pack,Package),
   cl_symbol_name(Symbol,String),
   package_find_symbol_or_missing(String,Package,OldSymbol,IntExt),!,
   package_shadow_symbol_step2(Package,String,OldSymbol,IntExt).


package_shadow_symbol_step2(_Package,_String,_OldSymbol,kw_external).
package_shadow_symbol_step2(_Package,_String,_OldSymbol,kw_internal).
package_shadow_symbol_step2( Package,String,_OldSymbol,'$missing'):-
   make_fresh_internal_symbol(Package,String,_Symbol).
package_shadow_symbol_step2(Package,String,OldSymbol,kw_inherited):-
   assert_if_new(package:package_shadowing_symbols(Package,OldSymbol)),
   make_fresh_internal_symbol(Package,String,_Symbol).


% caller is responsible for avoiding conflicts
make_fresh_internal_symbol(pkg_kw,String,Symbol):- !, create_keyword(String,Symbol).
make_fresh_internal_symbol(Package,String,Symbol):- 
   ignore(symbol_case_name(String,Package,Symbol)),
   create_symbol(String,Package,Symbol),
   assert_if_new(package:package_internal_symbols(Package,String,Symbol)).



is_lisp_package(P):- package_name(P,_). 

:- dynamic package_name/2.
:- dynamic package_nicknames/2.
:- dynamic package_use_list/2.
:- dynamic package_shadowing_symbols/2.
:- dynamic package_external_symbols/3.
:- dynamic package_internal_symbols/3.

package_name(pkg_cl,"COMMON-LISP").
package_name(pkg_user,"COMMON-LISP-USER").
package_name(pkg_tl,"TOP-LEVEL").
package_name(pkg_charset,"CHARSET").
package_name(pkg_clos,"CLOS").
package_name(pkg_custom,"CUSTOM").
package_name(pkg_ext,"EXTENSIONS").
package_name(pkg_ffi,"FFI").
package_name(pkg_format,"FORMAT").
package_name(pkg_gray,"GRAY").
package_name(pkg_gstream,"GSTREAM").
package_name(pkg_i18n,"I18N").
package_name(pkg_kw,"KEYWORD").
package_name(pkg_loop,"LOOP").
package_name(pkg_os,"POSIX").
package_name(pkg_precompiler,"PRECOMPILER").
package_name(pkg_profiler,"PROFILER").
package_name(pkg_readline,"READLINE").
package_name(pkg_regexp,"REGEXP").
package_name(pkg_screen,"SCREEN").
package_name(pkg_sequence,"SEQUENCE").
package_name(pkg_socket,"SOCKET").
package_name(pkg_sys,"SYSTEM").
package_name(pkg_threads,"THREADS").
package_name(pkg_xp,"XP").

package_name(pkg_java,"JAVA").
package_name(pkg_jvm,"JVM").

:- decl_mapped_opv(claz_package,[name=package_name]).

package_nicknames(pkg_cl, "CL").
package_nicknames(pkg_cl, "LISP").
package_nicknames(pkg_cl, "EMACS-CL").
package_nicknames(pkg_user, "U").
package_nicknames(pkg_user, "USER").
package_nicknames(pkg_user, "CL-USER").
package_nicknames(pkg_user, "EMACS-CL-USER").
package_nicknames(pkg_tl, "TPL").
package_nicknames(pkg_ext, "EXT").
package_nicknames(pkg_os, "OS").
package_nicknames(pkg_clos, "MOP").
package_nicknames(pkg_precompiler, "PRE").
package_nicknames(pkg_profiler, "PROF").
package_nicknames(pkg_sys, "SYS").
package_nicknames(pkg_sys, "WAM-CL").

:- decl_mapped_opv(claz_package,[nicknames=package_nicknames]).

package_use_list(pkg_cl, pkg_clos).
package_use_list(pkg_user, pkg_cl).
package_use_list(pkg_user, pkg_ext).
package_use_list(pkg_user, pkg_java).
package_use_list(pkg_tl, pkg_cl).
package_use_list(pkg_tl, pkg_ext).
package_use_list(pkg_clos, pkg_cl).
package_use_list(pkg_clos, pkg_ext).
package_use_list(pkg_clos, pkg_sys).
package_use_list(pkg_ext, pkg_cl).
package_use_list(pkg_ext, pkg_custom).
package_use_list(pkg_ext, pkg_gray).
package_use_list(pkg_ext, pkg_gstream).
package_use_list(pkg_ext, pkg_i18n).
package_use_list(pkg_ext, pkg_os).
package_use_list(pkg_ext, pkg_socket).
package_use_list(pkg_ext, pkg_threads).
package_use_list(pkg_ffi, pkg_cl).
package_use_list(pkg_ffi, pkg_ext).
package_use_list(pkg_format, pkg_cl).
package_use_list(pkg_format, pkg_ext).
package_use_list(pkg_java, pkg_cl).
package_use_list(pkg_java, pkg_ext).
package_use_list(pkg_jvm, pkg_cl).
package_use_list(pkg_jvm, pkg_ext).
package_use_list(pkg_jvm, pkg_sys).
package_use_list(pkg_lisp, pkg_cl).
package_use_list(pkg_lisp, pkg_ext).
package_use_list(pkg_lisp, pkg_sys).
package_use_list(pkg_loop, pkg_cl).
package_use_list(pkg_os, pkg_cl).
package_use_list(pkg_os, pkg_ext).
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
package_use_list(pkg_sys, pkg_cl).
package_use_list(pkg_sys, pkg_ext).
package_use_list(pkg_threads, pkg_cl).
package_use_list(pkg_threads, pkg_ext).
package_use_list(pkg_threads, pkg_sys).
package_use_list(pkg_xp, pkg_cl).

:- decl_mapped_opv(claz_package,[uses=package_use_list]).



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
package_prefix(PN,Pre):- nonvar(PN),package_nicknames(Pk,PN),!,package_prefix(Pk,Pre).
package_prefix(Pk,Pre):- is_lisp_package(Pk),atom_concat('pkg_',Package,Pk),atom_concat(Package,'_',Pre).


:- include('pi.pro').

:- fixup_exports.
