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

add_package_internal_symbol(Package,Name,Symbol):- Package==pkg_kw,!,add_package_external_symbol(Package,Name,Symbol).
add_package_internal_symbol(Package,Name,Symbol):- assert_if_new(package_internal_symbols(Package,Name,Symbol)).

add_package_external_symbol(Package,Name,Symbol):- assert_if_new(package_external_symbols(Package,Name,Symbol)).


is_lisp_package(P):- package_name(P,_). 

:- dynamic package_name/2.
:- dynamic package_nickname/2.
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
package_name(pkg_keyword,"KEYWORD").
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

package_nickname(pkg_cl, "CL").
package_nickname(pkg_cl, "LISP").
package_nickname(pkg_cl, "EMACS-CL").
package_nickname(pkg_user, "U").
package_nickname(pkg_user, "USER").
package_nickname(pkg_user, "CL-USER").
package_nickname(pkg_user, "EMACS-CL-USER").
package_nickname(pkg_tl, "TPL").
package_nickname(pkg_ext, "EXT").
package_nickname(pkg_os, "OS").
package_nickname(pkg_clos, "MOP").
package_nickname(pkg_precompiler, "PRE").
package_nickname(pkg_profiler, "PROF").
package_nickname(pkg_sys, "SYS").


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

:- include('pi.pro').

:- fixup_exports.
