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
:- module(symp, []).
:- set_module(class(library)).
% :- include('header.pro').

:- style_check(-discontiguous).
:- dynamic package_nickname/2.
:- dynamic package_use_list/2.
:- dynamic package_shadowing_symbols/2.


is_lisp_package('common-lisp').
is_lisp_package('common-lisp-user').
is_lisp_package('top-level').
is_lisp_package(charset).
is_lisp_package(clos).
is_lisp_package(custom).
is_lisp_package(extensions).
is_lisp_package(ffi).
is_lisp_package(format).
is_lisp_package(gray).
is_lisp_package(gstream).
is_lisp_package(i18n).
is_lisp_package(java).
is_lisp_package(jvm).
is_lisp_package(keyword).
is_lisp_package(loop).
is_lisp_package(posix).
is_lisp_package(precompiler).
is_lisp_package(profiler).
is_lisp_package(readline).
is_lisp_package(regexp).
is_lisp_package(screen).
is_lisp_package(sequence).
is_lisp_package(socket).
is_lisp_package(system).
is_lisp_package(threads).
is_lisp_package(xp).

package_nickname('common-lisp', cl).
package_nickname('common-lisp', lisp).
package_nickname('common-lisp-user', 'cl-user').
package_nickname('common-lisp-user', user).
package_nickname('top-level', tpl).
package_nickname(extensions, ext).
package_nickname(posix, os).
package_nickname(clos, mop).
package_nickname(precompiler, pre).
package_nickname(profiler, prof).
package_nickname(system, sys).
package_nickname(keyword, kw).


package_shadowing_symbols(posix, 'defstruct').
package_shadowing_symbols(readline, 'def-c-const').
package_shadowing_symbols(readline, 'def-c-enum').
package_shadowing_symbols(readline, 'def-c-struct').
package_shadowing_symbols(readline, 'def-c-type').
package_shadowing_symbols(readline, 'def-c-var').
package_shadowing_symbols(readline, 'def-call-out').
package_shadowing_symbols(readline, 'defconstant').
package_shadowing_symbols(readline, 'defmacro').
package_shadowing_symbols(readline, 'defun').
package_shadowing_symbols(readline, 'defvar').
package_shadowing_symbols(sequence, 'copy-seq').
package_shadowing_symbols(sequence, 'count').
package_shadowing_symbols(sequence, 'count-if').
package_shadowing_symbols(sequence, 'count-if-not').
package_shadowing_symbols(sequence, 'delete').
package_shadowing_symbols(sequence, 'delete-duplicates').
package_shadowing_symbols(sequence, 'delete-if').
package_shadowing_symbols(sequence, 'delete-if-not').
package_shadowing_symbols(sequence, 'elt').
package_shadowing_symbols(sequence, 'fill').
package_shadowing_symbols(sequence, 'find').
package_shadowing_symbols(sequence, 'find-if').
package_shadowing_symbols(sequence, 'find-if-not').
package_shadowing_symbols(sequence, 'length').
package_shadowing_symbols(sequence, 'mismatch').
package_shadowing_symbols(sequence, 'nreverse').
package_shadowing_symbols(sequence, 'nsubstitute').
package_shadowing_symbols(sequence, 'nsubstitute-if').
package_shadowing_symbols(sequence, 'nsubstitute-if-not').
package_shadowing_symbols(sequence, 'position').
package_shadowing_symbols(sequence, 'position-if').
package_shadowing_symbols(sequence, 'position-if-not').
package_shadowing_symbols(sequence, 'reduce').
package_shadowing_symbols(sequence, 'remove').
package_shadowing_symbols(sequence, 'remove-duplicates').
package_shadowing_symbols(sequence, 'remove-if').
package_shadowing_symbols(sequence, 'remove-if-not').
package_shadowing_symbols(sequence, 'replace').
package_shadowing_symbols(sequence, 'reverse').
package_shadowing_symbols(sequence, 'search').
package_shadowing_symbols(sequence, 'sort').
package_shadowing_symbols(sequence, 'stable-sort').
package_shadowing_symbols(sequence, 'subseq').
package_shadowing_symbols(sequence, 'substitute').
package_shadowing_symbols(sequence, 'substitute-if').
package_shadowing_symbols(sequence, 'substitute-if-not').
package_shadowing_symbols(system, 'version').
package_use_list('common-lisp', clos).
package_use_list('common-lisp-user', 'common-lisp').
package_use_list('common-lisp-user', extensions).
package_use_list('common-lisp-user', java).
package_use_list('top-level', 'common-lisp').
package_use_list('top-level', extensions).
package_use_list(clos, 'common-lisp').
package_use_list(clos, extensions).
package_use_list(clos, system).
package_use_list(extensions, 'common-lisp').
package_use_list(extensions, custom).
package_use_list(extensions, gray).
package_use_list(extensions, gstream).
package_use_list(extensions, i18n).
package_use_list(extensions, posix).
package_use_list(extensions, socket).
package_use_list(extensions, threads).
package_use_list(ffi, 'common-lisp').
package_use_list(ffi, extensions).
package_use_list(format, 'common-lisp').
package_use_list(format, extensions).
package_use_list(java, 'common-lisp').
package_use_list(java, extensions).
package_use_list(jvm, 'common-lisp').
package_use_list(jvm, extensions).
package_use_list(jvm, system).
package_use_list(lisp, 'common-lisp').
package_use_list(lisp, extensions).
package_use_list(lisp, system).
package_use_list(loop, 'common-lisp').
package_use_list(posix, 'common-lisp').
package_use_list(posix, extensions).
package_use_list(precompiler, 'common-lisp').
package_use_list(precompiler, extensions).
package_use_list(precompiler, system).
package_use_list(profiler, 'common-lisp').
package_use_list(profiler, extensions).
package_use_list(readline, 'common-lisp').
package_use_list(readline, extensions).
package_use_list(readline, ffi).
package_use_list(regexp, 'common-lisp').
package_use_list(screen, 'common-lisp').
package_use_list(screen, extensions).
package_use_list(sequence, 'common-lisp').
package_use_list(system, 'common-lisp').
package_use_list(system, extensions).
package_use_list(threads, 'common-lisp').
package_use_list(threads, extensions).
package_use_list(threads, system).
package_use_list(xp, 'common-lisp').

:- dynamic symbol_info/4.

:- include('si.pro').

:- fixup_exports.

