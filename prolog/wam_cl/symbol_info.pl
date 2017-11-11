/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (pkg_xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (pkg_c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (pkg_YAP 4x faster).
 *
 *******************************************************************/
:- module(symp, []).
:- set_module(class(library)).
% :- include('header.pro').

:- style_check(-discontiguous).
:- dynamic package_name/2.
:- dynamic package_nickname/2.
:- dynamic package_use_list/2.
:- dynamic package_shadowing_symbols/2.
:- dynamic package_external_symbols/3.
:- dynamic package_internal_symbols/3.
:- dynamic o_p_v/3.

is_lisp_package(P):- package_name(P,_). 

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
package_nickname(pkg_user, "CL-USER").
package_nickname(pkg_user, "USER").
package_nickname(pkg_tl, "TPL").
package_nickname(pkg_ext, "EXT").
package_nickname(pkg_os, "OS").
package_nickname(pkg_clos, "MOP").
package_nickname(pkg_precompiler, "PRE").
package_nickname(pkg_profiler, "PROF").
package_nickname(pkg_sys, "SYS").


package_shadowing_symbols(pkg_os, 'defstruct').
package_shadowing_symbols(pkg_readline, 'def-c-const').
package_shadowing_symbols(pkg_readline, 'def-c-enum').
package_shadowing_symbols(pkg_readline, 'def-c-struct').
package_shadowing_symbols(pkg_readline, 'def-c-type').
package_shadowing_symbols(pkg_readline, 'def-c-var').
package_shadowing_symbols(pkg_readline, 'def-call-out').
package_shadowing_symbols(pkg_readline, 'defconstant').
package_shadowing_symbols(pkg_readline, 'defmacro').
package_shadowing_symbols(pkg_readline, 'defun').
package_shadowing_symbols(pkg_readline, 'defvar').
package_shadowing_symbols(pkg_sequence, 'copy-seq').
package_shadowing_symbols(pkg_sequence, 'count').
package_shadowing_symbols(pkg_sequence, 'count-if').
package_shadowing_symbols(pkg_sequence, 'count-if-not').
package_shadowing_symbols(pkg_sequence, 'delete').
package_shadowing_symbols(pkg_sequence, 'delete-duplicates').
package_shadowing_symbols(pkg_sequence, 'delete-if').
package_shadowing_symbols(pkg_sequence, 'delete-if-not').
package_shadowing_symbols(pkg_sequence, 'elt').
package_shadowing_symbols(pkg_sequence, 'fill').
package_shadowing_symbols(pkg_sequence, 'find').
package_shadowing_symbols(pkg_sequence, 'find-if').
package_shadowing_symbols(pkg_sequence, 'find-if-not').
package_shadowing_symbols(pkg_sequence, 'length').
package_shadowing_symbols(pkg_sequence, 'mismatch').
package_shadowing_symbols(pkg_sequence, 'nreverse').
package_shadowing_symbols(pkg_sequence, 'nsubstitute').
package_shadowing_symbols(pkg_sequence, 'nsubstitute-if').
package_shadowing_symbols(pkg_sequence, 'nsubstitute-if-not').
package_shadowing_symbols(pkg_sequence, 'position').
package_shadowing_symbols(pkg_sequence, 'position-if').
package_shadowing_symbols(pkg_sequence, 'position-if-not').
package_shadowing_symbols(pkg_sequence, 'reduce').
package_shadowing_symbols(pkg_sequence, 'remove').
package_shadowing_symbols(pkg_sequence, 'remove-duplicates').
package_shadowing_symbols(pkg_sequence, 'remove-if').
package_shadowing_symbols(pkg_sequence, 'remove-if-not').
package_shadowing_symbols(pkg_sequence, 'replace').
package_shadowing_symbols(pkg_sequence, 'reverse').
package_shadowing_symbols(pkg_sequence, 'search').
package_shadowing_symbols(pkg_sequence, 'sort').
package_shadowing_symbols(pkg_sequence, 'stable-sort').
package_shadowing_symbols(pkg_sequence, 'subseq').
package_shadowing_symbols(pkg_sequence, 'substitute').
package_shadowing_symbols(pkg_sequence, 'substitute-if').
package_shadowing_symbols(pkg_sequence, 'substitute-if-not').
package_shadowing_symbols(pkg_sys, 'version').
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

:- dynamic get_symbol_info/4.

:- include('si.pro').

get_symbol_info(A,B,C,D):- symp:symbol_info(A,B,C,D).

non_prop(constant).
non_prop(variable).
non_prop(package).
non_prop(function_type).


symp:o_p_v(Symbol,deftype,constant):- symp:o_p_v(Symbol,package,pkg_kw).

add_o_p_v(O,P,V):- ( \+ symp:o_p_v(O,P,V) -> assert(symp:o_p_v(O,P,V)) ; true).
add_package_external_symbol(Package,Name,Symbol):- assert_if_new(package_external_symbols(Package,Name,Symbol)).
add_package_internal_symbol(Package,Name,Symbol):- assert_if_new(package_internal_symbols(Package,Name,Symbol)).


:- 
 forall((must(get_symbol_info(S,P,function_type,type_sub(T,ST))),must(get_symbol_info(S,P,function,F))),
  ((add_o_p_v(F,typeof,T)),
   (add_o_p_v(S,kw_compiled_as,T)),
   (add_o_p_v(F,classof,ST)))).


:- 
 forall((get_symbol_info(Symbol,P,name,Name),get_symbol_info(Symbol,P,package,kw_external)),
   add_package_external_symbol(P,Name,Symbol)).

:- 
 forall((get_symbol_info(Symbol,P,name,Name),get_symbol_info(Symbol,P,package,kw_internal)),
   add_package_internal_symbol(P,Name,Symbol)).

:- 
 forall(get_symbol_info(Symbol,_,constant,Value),
   ((add_o_p_v(Symbol,value,Value)),
    (add_o_p_v(Symbol,kw_deftype,constant)))).

:- 
 forall(get_symbol_info(Symbol,_,variable,Value),
   ((add_o_p_v(Symbol,value,Value)),
    (add_o_p_v(Symbol,kw_deftype,special)))).

:- 
 forall(get_symbol_info(Symbol,P,_,_),(add_o_p_v(Symbol,package,P))).

:- 
 forall(get_symbol_info(Symbol,_,_,_),(add_o_p_v(Symbol,typeof,symbol))).


:- 
 forall((get_symbol_info(Symbol,_P,Prop,Value),\+non_prop(Prop)),add_o_p_v(Symbol,Prop,Value)).


:- fixup_exports.

