```bash
root@gitlab:/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl# swipl repl.pl
Installed packages (38):

i clause_attvars@1.1.118    - An alternate interface to the clause database to allow attributed variables to be asserted
i dictoo@1.1.118            - Dict-like OO Syntax
i each_call_cleanup@1.1.118 - Each Call Redo Setup and Cleanup
i eggdrop@1.1.118           - Hook up to an existing IRC Client called an Eggdrop
i file_scope@1.1.118        - File local scoped efects
i fluxplayer-prolog-engine@0.0.1 - Prolog interface to Slack http://www.slack.com
i gvar_syntax@1.1.118       - Global Variable Syntax
i hook_hybrid@1.1.118       - Hook assert retract call of *specific* predicates
i instant_prolog_docs@1.1.118 - Magically document prolog source files based on predicate and variable naming conventions
i lib_atts@1.1.118          - Common atts.pl interface like https://sicstus.sics.se/sicstus/docs/4.0.0/html/sicstus/lib_002datts.html
i logicmoo_base@1.1.118     - LogicMOO - Extends Prolog Programming to support Dynamic Epistemic Logic (DEL) with Constraints
i logicmoo_experimental@1.1.118 - Various experimental packages - warning: HUGE amount of test data
i logicmoo_nlu@1.1.114      - Various English to Logic Convertors - warning: HUGE amount of test data
i logicmoo_packages@1.1.118 - Various packages - warning: HUGE amount of test data
i logicmoo_planner@1.1.118  - Various PDDLish planners - warning: HUGE amount of test data
i logicmoo_planners@1.1.118 - Various Hybrid HTN Planners speaking PDDLish and OCLh
i logicmoo_utils@1.1.118    - Common predicates used by external Logicmoo Utils and Base
i loop_check@1.1.118        - New simple loop checking
i mpi@1.0                   - Porting of the LAMMPI library of Yap Prolog to SWI-Prolog
i multimodal_dcg@1.1.118    - Reduce floundering of DCGs by constraining and narrowing search
i multivar@1.1.118          - User defined datatypes
i must_trace@1.1.118        - Trace with your eyeballs instead of your fingers
i no_repeats@1.1.118        - New ways to avoid duplicate solutions
i pfc@1.1.118               - Pfc -- a package for forward chaining in Prolog
i predicate_streams@1.1.118 - Implement your own Abstract Predicate Streams
i prologmud@1.1.118         - Online text adventure game - MUD Server
i prologmud_samples@1.1.118 - Online text adventure game - Sample
i s_expression@1.1.118      - Utilities for Handling of S-Expression Lisp/Scheme-Like forms and parsing of KIF, GDL, PDDL, CLIF
i slack_prolog@1.1.118      - Prolog interface to Slack http://www.slack.com
i subclause_expansion@1.1.118 - More use specific versions of term/goal expansion hooks
i tabling_dra@1.1.118       - SWI-Prolog interface to Table-handling procedures for the "dra" interpreter. Written by Feliks Kluzniak at UTD (March 2009)
i transpiler@0.1            - A universal translator for programming languages
i trill@4.1.0               - A tableau probabilistic reasoner in three different versions
i wam_common_lisp@1.1.118   - ANSI Common Lisp implemented in Prolog
i with_open_options@1.1.118 - Utilities to open various objects for read/write
i with_thread_local@1.1.118 - Call a Goal with local assertions
i xlisting@1.1.118          - Selective Interactive Non-Deterministic Tracing
i xlisting_web@1.1.118      - Manipulate and browse prolog runtime over www

```prolog
:- success(always(call((to_lisp_pathname('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl', '$OBJ'(claz_pathname, '$ARRAY'([*], claz_base_character, "/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl"))), set_opv(ext_xx_lisp_home_xx, value, '$OBJ'(claz_pathname, '$ARRAY'([*], claz_base_character, "/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl"))))))).
```
```prolog
:- success(always(call((to_lisp_pathname("", '$OBJ'(claz_pathname, '$ARRAY'([*], claz_base_character, []))), set_opv(xx_default_pathname_defaults_xx, value, '$OBJ'(claz_pathname, '$ARRAY'([*], claz_base_character, []))))))).
```
```prolog
:- success(always(call(notrace(grovel_math)))).
```
```prolog
:- success(always(lisp_compiled_eval('(defun floor (number &optional divisor)\n  "Return the greatest integer not greater than number, or number/divisor.\n  The second returned value is (mod number divisor)."\n  (if (null divisor)(setq divisor 1))\n  (multiple-value-bind (tru rem) (truncate number divisor)\n    (if (and (not (zerop rem))\n             (if (minusp divisor)\n               (plusp number)\n               (minusp number)))\n      (if (called-for-mv-p)\n        (values (1- tru) (+ rem divisor))\n        (1- tru))\n      (values tru rem))))',
                                     floor))).
```
```prolog
:- success(always(lisp_compiled_eval("(defparameter sys::*output-file-pathname* ())",
                                     sys_xx_output_file_pathname_xx))).
```
```prolog
:- success(always(lisp_compiled_eval('`sys:prolog-trace', sys_prolog_trace))).
```
```prolog
:- success(always(lisp_compiled_eval('`sys:trace', trace))).
```
```prolog
:- success(always(lisp_compiled_eval('`sys:prolog', sys_prolog))).
```
```prolog
:- success(always(lisp_compiled_eval('`sys:break', break))).
```
```prolog
:- success(always(lisp_compiled_eval('`sys:prolog-call', sys_prolog_call))).
```
```prolog
:- success(always(lisp_compiled_eval('`sys:prolog-inline', sys_prolog_inline))).
```
```prolog
:- success(always(lisp_compiled_eval("(defparameter sys::*compiler-mode* :execute)",
                                     sys_xx_compiler_mode_xx))).
```
```prolog
:- success(always(lisp_compiled_eval('`sys:get-iprops', sys_get_iprops))).
```
```prolog
:- success(always(lisp_compiled_eval('`sys:get-opv', sys_get_opv))).
```
```prolog
:- success(always(lisp_compiled_eval("(defparameter EXT:*ARGS* ())",
                                     ext_xx_args_xx))).
```
```prolog
:- success(always(call(set_program_args([])))).
```
```cl
__        ___    __  __        ____ _
\ \      / / \  |  \/  |      / ___| |
 \ \ /\ / / _ \ | |\/| |_____| |   | |
  \ V  V / ___ \| |  | |_____| |___| |___
   \_/\_/_/   \_\_|  |_|      \____|_____|

Common Lisp, written in Prolog
CL-USER> (compile-file "sanity-test")
```
```prolog
:- lisp_compiled_eval(['compile-file', '$ARRAY'([*], claz_base_character, "sanity-test")]).
## COMPILER:- cl_compile_file('$ARRAY'([*], claz_base_character, "sanity-test"),
		   [],
		   Compile_file_Ret).
## EXEC:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp'),
				  lisp_compile_to_prolog_output(<stream>(0x1a0a100)))).

;; $Id: examples.lisp,v 1.1 2003/10/21 17:30:56 nhabedi Exp $
;;                          EXAMPLES.LISP
;;           Nick Levine, Ravenbrook Limited, 2003-08-14
;; 
;; These are the examples I expect to use in the tutorial on CLOS
;; at the International Lisp Conference 2003.
;; 
;; This document is mainly for my operational convenience. You might
;; want to raid fragments to help you get started when building CLOS
;; into your Common Lisp applications. Nothing useful will happen if
;; you try to cl:load this document into a lisp image.
;;
;; This document is provided "as is", without any express or implied
;; warranty.  In no event will the author be held liable for any
;; damages arising from the use of this document.  You may make and
;; distribute verbatim copies of this document provided that you do
;; not charge a fee for this document or for its distribution.
 #+WAM-CL (prolog-call "cls.")
(defun mapcar-visualize (func l) (if (null l) () (cons (apply func (list (first l))) (mapcar func (rest l)))))

wl:lambda_def(defun, u_mapcar_visualize, f_u_mapcar_visualize, [u_func, u_l], [[if, [null, u_l], [], [cons, [apply, u_func, [list, [first, u_l]]], [mapcar, u_func, [rest, u_l]]]]]).
wl:arglist_info(u_mapcar_visualize, [u_func, u_l], [Func_Param, L_Param], arginfo{all:[u_func, u_l], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_func, u_l], opt:0, req:[u_func, u_l], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_mapcar_visualize).

```


### Compiled:  `U::MAPCAR-VISUALIZE` 
```prolog
f_u_mapcar_visualize(Func_Param, L_Param, FnResult) :-
	Env=[bv(u_func, Func_Param), bv(u_l, L_Param)],
	(   L_Param==[]
	->  FnResult=[]
	;   get_var(Env, list, List_Get),
	    cl_car(L_Param, Car_Ret),
	    f_u_func(List_Get, Car_Ret, Func_Ret),
	    cl_cdr(L_Param, Cdr_Ret),
	    cl_mapcar(Func_Param, [Cdr_Ret], Mapcar_Ret),
	    FnResult=[Func_Ret|Mapcar_Ret]
	).
:- set_opv(f_u_mapcar_visualize, classof, claz_function),
   set_opv(u_mapcar_visualize, compile_as, kw_function),
   set_opv(u_mapcar_visualize, function, f_u_mapcar_visualize),
   DefunResult=u_mapcar_visualize.
:- side_effect(assert_lsp(u_mapcar_visualize,
			  wl:lambda_def(defun, u_mapcar_visualize, f_u_mapcar_visualize, [u_func, u_l], [[if, [null, u_l], [], [cons, [apply, u_func, [list, [first, u_l]]], [mapcar, u_func, [rest, u_l]]]]]))).
:- side_effect(assert_lsp(u_mapcar_visualize,
			  wl:arglist_info(u_mapcar_visualize, [u_func, u_l], [Func_Param, L_Param], arginfo{all:[u_func, u_l], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_func, u_l], opt:0, req:[u_func, u_l], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(u_mapcar_visualize,
			  wl:init_args(exact_only, u_mapcar_visualize))).
(load "../prolog/wam_cl/wam-cl-init")

:- cl_load('$ARRAY'([*], claz_base_character, "../prolog/wam_cl/wam-cl-init"),
	   [],
	   _Ignored).
:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp'),
				  lisp_compile_to_prolog_output(<stream>(0x1d443d0)))).
(in-package "SYSTEM")

:- cl_in_package(system1, _Ignored4).
(defpackage "SYSTEM" (:nicknames "SYS"))
:- cl_defpackage('$ARRAY'([*], claz_base_character, "SYSTEM"),
		 [[kw_nicknames, '$ARRAY'([*], claz_base_character, "SYS")]],
		 _Ignored4).
:- side_effect(add_opv_new(pkg_system, nicknames, "SYS")).
(defpackage "COMMON-LISP" (:nicknames "CL" "LISP")(:uses "SYSTEM"))
:- cl_defpackage('$ARRAY'([*], claz_base_character, "COMMON-LISP"),
		 
		 [ 
		   [ kw_nicknames,
		     '$ARRAY'([*], claz_base_character, "CL"),
		     '$ARRAY'([*], claz_base_character, "LISP")
		   ],
		   [kw_uses, '$ARRAY'([*], claz_base_character, "SYSTEM")]
		 ],
		 _Ignored4).
:- side_effect(add_opv_new(pkg_common_lisp, nicknames, "CL")).
:- side_effect(add_opv_new(pkg_common_lisp, nicknames, "LISP")).
:- side_effect(add_opv_new(pkg_common_lisp, uses, "SYSTEM")).
(defpackage "COMMON-LISP-USER" (:nicknames "U" "USER" "CL-USER") (:uses "COMMON-LISP"))
:- cl_defpackage('$ARRAY'([*], claz_base_character, "COMMON-LISP-USER"),
		 
		 [ 
		   [ kw_nicknames,
		     '$ARRAY'([*], claz_base_character, "U"),
		     '$ARRAY'([*], claz_base_character, "USER"),
		     '$ARRAY'([*], claz_base_character, "CL-USER")
		   ],
		   [kw_uses, '$ARRAY'([*], claz_base_character, "COMMON-LISP")]
		 ],
		 _Ignored4).
:- side_effect(add_opv_new(pkg_common_lisp_user, nicknames, "U")).
:- side_effect(add_opv_new(pkg_common_lisp_user, nicknames, "USER")).
:- side_effect(add_opv_new(pkg_common_lisp_user, nicknames, "CL-USER")).
:- side_effect(add_opv_new(pkg_common_lisp_user, uses, "COMMON-LISP")).
(defvar *lisp-file-type* "lisp") 
:- set_var(TLEnv5,
	   defvar,
	   sys_xx_lisp_file_type_xx,
	   '$ARRAY'([*], claz_base_character, "lisp")).
(defvar *default-pathname-defaults* #P"")
 
:- set_var(TLEnv5,
	   defvar,
	   xx_default_pathname_defaults_xx,
	   '$OBJ'(claz_pathname, "")).
(defun dd () 
 (let ((*lisp-file-type* "cl") 
        (*default-pathname-defaults* (merge-pathnames "daydreamer/"))) (load "dd")))


wl:lambda_def(defun, sys_dd, f_sys_dd, [], [[let, [[sys_xx_lisp_file_type_xx, '$ARRAY'([*], claz_base_character, "cl")], [xx_default_pathname_defaults_xx, [merge_pathnames, '$ARRAY'([*], claz_base_character, "daydreamer/")]]], [load, '$ARRAY'([*], claz_base_character, "dd")]]]).
wl:arglist_info(sys_dd, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_dd).

```

### Compiled:  `SYS::DD` 
```prolog
f_sys_dd(FnResult) :-
	Env=[],
	cl_merge_pathnames('$ARRAY'([*], claz_base_character, "daydreamer/"),
			   Xx_default_pathname_defaults_xx_Init),
	LEnv=[[bv(sys_xx_lisp_file_type_xx, '$ARRAY'([*], claz_base_character, "cl"))]|Env],
	save_special(sv(xx_default_pathname_defaults_xx,
			Xx_default_pathname_defaults_xx_Init,
			value,
			Value)),
	cl_load('$ARRAY'([*], claz_base_character, "dd"), [], LetResult),
	restore_special(sv(xx_default_pathname_defaults_xx,
			   Xx_default_pathname_defaults_xx_Init,
			   value,
			   Value)),
	LetResult=FnResult.
:- set_opv(f_sys_dd, classof, claz_function),
   set_opv(sys_dd, compile_as, kw_function),
   set_opv(sys_dd, function, f_sys_dd),
   _Ignored4=sys_dd.
:- side_effect(assert_lsp(sys_dd,
			  wl:lambda_def(defun, sys_dd, f_sys_dd, [], [[let, [[sys_xx_lisp_file_type_xx, '$ARRAY'([*], claz_base_character, "cl")], [xx_default_pathname_defaults_xx, [merge_pathnames, '$ARRAY'([*], claz_base_character, "daydreamer/")]]], [load, '$ARRAY'([*], claz_base_character, "dd")]]]))).
:- side_effect(assert_lsp(sys_dd,
			  wl:arglist_info(sys_dd, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_dd, wl:init_args(exact_only, sys_dd))).
(defun show-ascii-art ()
        
(write-line "  __________    ")
(write-line " / ___  ___ \\   ")
(write-line "/ / @ \\/ @ \\ \\  ")
(write-line "\\ \\___/\\___/ /\\ ")
(write-line " \\____\\/____/|| ")
(write-line " /     /\\\\\\\\\\// ")
(write-line "|     |\\\\\\\\\\\\   ")
(write-line " \\      \\\\\\\\\\\\  ")
(write-line "   \\______/\\\\\\\\ ")
(write-line "    _||_||_     ")
(write-line "                "))

wl:lambda_def(defun, sys_show_ascii_art, f_sys_show_ascii_art, [], [[write_line, '$ARRAY'([*], claz_base_character, "  __________    ")], [write_line, '$ARRAY'([*], claz_base_character, " / ___  ___ \\   ")], [write_line, '$ARRAY'([*], claz_base_character, "/ / @ \\/ @ \\ \\  ")], [write_line, '$ARRAY'([*], claz_base_character, "\\ \\___/\\___/ /\\ ")], [write_line, '$ARRAY'([*], claz_base_character, " \\____\\/____/|| ")], [write_line, '$ARRAY'([*], claz_base_character, " /     /\\\\\\\\\\// ")], [write_line, '$ARRAY'([*], claz_base_character, "|     |\\\\\\\\\\\\   ")], [write_line, '$ARRAY'([*], claz_base_character, " \\      \\\\\\\\\\\\  ")], [write_line, '$ARRAY'([*], claz_base_character, "   \\______/\\\\\\\\ ")], [write_line, '$ARRAY'([*], claz_base_character, "    _||_||_     ")], [write_line, '$ARRAY'([*], claz_base_character, "                ")]]).
wl:arglist_info(sys_show_ascii_art, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_show_ascii_art).

```

### Compiled:  `SYS::SHOW-ASCII-ART` 
```prolog
f_sys_show_ascii_art(FnResult) :-
	Env=[],
	cl_write_line('$ARRAY'([*], claz_base_character, "  __________    "),
		      Write_line_Ret),
	cl_write_line('$ARRAY'([*], claz_base_character, " / ___  ___ \\   "),
		      Write_line_Ret15),
	cl_write_line('$ARRAY'([*], claz_base_character, "/ / @ \\/ @ \\ \\  "),
		      Write_line_Ret16),
	cl_write_line('$ARRAY'([*], claz_base_character, "\\ \\___/\\___/ /\\ "),
		      Write_line_Ret17),
	cl_write_line('$ARRAY'([*], claz_base_character, " \\____\\/____/|| "),
		      Write_line_Ret18),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       " /     /\\\\\\\\\\// "),
		      Write_line_Ret19),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       "|     |\\\\\\\\\\\\   "),
		      Write_line_Ret20),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       " \\      \\\\\\\\\\\\  "),
		      Write_line_Ret21),
	cl_write_line('$ARRAY'([*],
			       claz_base_character,
			       "   \\______/\\\\\\\\ "),
		      Write_line_Ret22),
	cl_write_line('$ARRAY'([*], claz_base_character, "    _||_||_     "),
		      Write_line_Ret23),
	cl_write_line('$ARRAY'([*], claz_base_character, "                "),
		      Write_line_Ret24),
	Write_line_Ret24=FnResult.
:- set_opv(f_sys_show_ascii_art, classof, claz_function),
   set_opv(sys_show_ascii_art, compile_as, kw_function),
   set_opv(sys_show_ascii_art, function, f_sys_show_ascii_art),
   _Ignored4=sys_show_ascii_art.
:- side_effect(assert_lsp(sys_show_ascii_art,
			  wl:lambda_def(defun, sys_show_ascii_art, f_sys_show_ascii_art, [], [[write_line, '$ARRAY'([*], claz_base_character, "  __________    ")], [write_line, '$ARRAY'([*], claz_base_character, " / ___  ___ \\   ")], [write_line, '$ARRAY'([*], claz_base_character, "/ / @ \\/ @ \\ \\  ")], [write_line, '$ARRAY'([*], claz_base_character, "\\ \\___/\\___/ /\\ ")], [write_line, '$ARRAY'([*], claz_base_character, " \\____\\/____/|| ")], [write_line, '$ARRAY'([*], claz_base_character, " /     /\\\\\\\\\\// ")], [write_line, '$ARRAY'([*], claz_base_character, "|     |\\\\\\\\\\\\   ")], [write_line, '$ARRAY'([*], claz_base_character, " \\      \\\\\\\\\\\\  ")], [write_line, '$ARRAY'([*], claz_base_character, "   \\______/\\\\\\\\ ")], [write_line, '$ARRAY'([*], claz_base_character, "    _||_||_     ")], [write_line, '$ARRAY'([*], claz_base_character, "                ")]]))).
:- side_effect(assert_lsp(sys_show_ascii_art,
			  wl:arglist_info(sys_show_ascii_art, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_show_ascii_art,
			  wl:init_args(exact_only, sys_show_ascii_art))).
(show-ascii-art)
:- f_sys_show_ascii_art(_Ignored4).
(load "wam-cl-init-1")
:- cl_load('$ARRAY'([*], claz_base_character, "wam-cl-init-1"), [], _Ignored4).
:- was_info(with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp'),
				  lisp_compile_to_prolog_output(<stream>(0x1d4a860)))).
;; setf.lisp
;;
;; Copyright (C) 2003-2006 Peter Graves
;; $Id$
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;; As a special exception, the copyright holders of this library give you
;; permission to link this library with independent modules to produce an
;; executable, regardless of the license terms of these independent
;; modules, and to copy and distribute the resulting executable under
;; terms of your choice, provided that you also meet, for each linked
;; independent module, the terms and conditions of the license of that
;; module.  An independent module is a module which is not derived from
;; or based on this library.  If you modify this library, you may extend
;; this exception to your version of the library, but you are not
;; obligated to do so.  If you do not wish to do so, delete this
;; exception statement from your version.
(in-package "SYSTEM")

:- cl_in_package('$ARRAY'([*], claz_base_character, "SYSTEM"), _Ignored6).
(defun get-setf-method-inverse (form inverse setf-function)
  (let ((new-var (gensym))
        (vars nil)
        (vals nil))
    (dolist (x (cdr form))
      (push (gensym) vars)
      (push x vals))
    (setq vals (nreverse vals))
    (values vars vals (list new-var)
            (if setf-function
                `(,@inverse ,new-var ,@vars)
                (if (functionp (car inverse))
                    `(funcall ,@inverse ,@vars ,new-var)
                    `(,@inverse ,@vars ,new-var)))
            `(,(car form) ,@vars))))

;;; If a macro, expand one level and try again.  If not, go for the
;;; SETF function.
wl:lambda_def(defun, sys_get_setf_method_inverse, f_sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], [[let, [[sys_new_var, [gensym]], [sys_vars, []], [sys_vals, []]], [dolist, [sys_x, [cdr, sys_form]], [push, [gensym], sys_vars], [push, sys_x, sys_vals]], [setq, sys_vals, [nreverse, sys_vals]], [values, sys_vars, sys_vals, [list, sys_new_var], [if, sys_setf_function, ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#COMMA', sys_new_var], ['#BQ-COMMA-ELIPSE', sys_vars]]], [if, [functionp, [car, sys_inverse]], ['#BQ', [funcall, ['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]], ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]]]], ['#BQ', [['#COMMA', [car, sys_form]], ['#BQ-COMMA-ELIPSE', sys_vars]]]]]]).
wl:arglist_info(sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], [Form_Param, Inverse_Param, Setf_function_Param], arginfo{all:[sys_form, sys_inverse, sys_setf_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_inverse, sys_setf_function], opt:0, req:[sys_form, sys_inverse, sys_setf_function], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_get_setf_method_inverse).

```

### Compiled:  `SYS::GET-SETF-METHOD-INVERSE` 
```prolog
f_sys_get_setf_method_inverse(Form_Param, Inverse_Param, Setf_function_Param, FnResult) :-
	Env=[bv(sys_form, Form_Param), bv(sys_inverse, Inverse_Param), bv(sys_setf_function, Setf_function_Param)],
	cl_gensym(New_var_Init),
	LEnv=[[bv(sys_new_var, New_var_Init), bv(sys_vars, []), bv(sys_vals, [])]|Env],
	cl_cdr(Form_Param, List),
	BV=bv(sys_x, Ele),
	Env2=[BV|LEnv],
	forall(member(Ele, List),
	       ( nb_setarg(2, BV, Ele),
		 cl_push([gensym], sys_vars, Vars),
		 cl_push(sys_x, sys_vals, Vals)
	       )),
	get_var(LEnv, sys_vals, Vals_Get31),
	cl_nreverse(Vals_Get31, Vals59),
	set_var(LEnv, sys_vals, Vals59),
	get_var(LEnv, sys_new_var, New_var_Get),
	get_var(LEnv, sys_vars, Vars_Get),
	CAR=[New_var_Get],
	(   Setf_function_Param\==[]
	->  get_var(LEnv, sys_new_var, New_var_Get37),
	    get_var(LEnv, sys_vars, Vars_Get38),
	    bq_append(Inverse_Param, [New_var_Get37|Vars_Get38], TrueResult51),
	    ElseResult52=TrueResult51
	;   cl_car(Inverse_Param, PredArgResult),
	    (   is_functionp(PredArgResult)
	    ->  get_var(LEnv, sys_new_var, New_var_Get45),
		get_var(LEnv, sys_vars, Vars_Get44),
		bq_append(Vars_Get44, [New_var_Get45], Bq_append_Ret),
		bq_append([funcall|Inverse_Param], Bq_append_Ret, TrueResult),
		ElseResult52=TrueResult
	    ;   get_var(LEnv, sys_new_var, New_var_Get48),
		get_var(LEnv, sys_vars, Vars_Get47),
		bq_append(Vars_Get47, [New_var_Get48], Bq_append_Ret61),
		bq_append(Inverse_Param, Bq_append_Ret61, ElseResult),
		ElseResult52=ElseResult
	    )
	),
	cl_car(Form_Param, Car_Ret),
	get_var(LEnv, sys_vars, Vars_Get54),
	nb_setval('$mv_return',
		  [Vars_Get, Vals_Get31, CAR, ElseResult52, [Car_Ret|Vars_Get54]]),
	Vars_Get=FnResult.
:- set_opv(f_sys_get_setf_method_inverse, classof, claz_function),
   set_opv(sys_get_setf_method_inverse, compile_as, kw_function),
   set_opv(sys_get_setf_method_inverse, function, f_sys_get_setf_method_inverse),
   _Ignored6=sys_get_setf_method_inverse.
:- side_effect(assert_lsp(sys_get_setf_method_inverse,
			  wl:lambda_def(defun, sys_get_setf_method_inverse, f_sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], [[let, [[sys_new_var, [gensym]], [sys_vars, []], [sys_vals, []]], [dolist, [sys_x, [cdr, sys_form]], [push, [gensym], sys_vars], [push, sys_x, sys_vals]], [setq, sys_vals, [nreverse, sys_vals]], [values, sys_vars, sys_vals, [list, sys_new_var], [if, sys_setf_function, ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#COMMA', sys_new_var], ['#BQ-COMMA-ELIPSE', sys_vars]]], [if, [functionp, [car, sys_inverse]], ['#BQ', [funcall, ['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]], ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]]]], ['#BQ', [['#COMMA', [car, sys_form]], ['#BQ-COMMA-ELIPSE', sys_vars]]]]]]))).
:- side_effect(assert_lsp(sys_get_setf_method_inverse,
			  wl:arglist_info(sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], [Form_Param, Inverse_Param, Setf_function_Param], arginfo{all:[sys_form, sys_inverse, sys_setf_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_inverse, sys_setf_function], opt:0, req:[sys_form, sys_inverse, sys_setf_function], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_get_setf_method_inverse,
			  wl:init_args(exact_only, sys_get_setf_method_inverse))).
;; If a macro, expand one level and try again.  If not, go for the
;; SETF function.
(defun expand-or-get-setf-inverse (form environment)
  (multiple-value-bind (expansion expanded)
      (macroexpand-1 form environment)
    (if expanded
        (get-setf-expansion expansion environment)
        (get-setf-method-inverse form `(funcall #'(setf ,(car form)))
                                 t))))

wl:lambda_def(defun, sys_expand_or_get_setf_inverse, f_sys_expand_or_get_setf_inverse, [sys_form, sys_environment], [[multiple_value_bind, [sys_expansion, sys_expanded], [macroexpand_1, sys_form, sys_environment], [if, sys_expanded, [get_setf_expansion, sys_expansion, sys_environment], [sys_get_setf_method_inverse, sys_form, ['#BQ', [funcall, function([setf, ['#COMMA', [car, sys_form]]])]], t]]]]).
wl:arglist_info(sys_expand_or_get_setf_inverse, [sys_form, sys_environment], [Form_Param, Environment_Param], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:0, req:[sys_form, sys_environment], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_expand_or_get_setf_inverse).

```

### Compiled:  `SYS::EXPAND-OR-GET-SETF-INVERSE` 
```prolog
f_sys_expand_or_get_setf_inverse(Form_Param, Environment_Param, FnResult) :-
	Env=[bv(sys_form, Form_Param), bv(sys_environment, Environment_Param)],
	LEnv=[[bv(sys_expansion, []), bv(sys_expanded, [])]|Env],
	cl_macroexpand_1([Form_Param, Environment_Param], Macroexpand_1_Ret),
	setq_from_values(LEnv, [sys_expansion, sys_expanded]),
	get_var(LEnv, sys_expanded, IFTEST),
	(   IFTEST\==[]
	->  get_var(LEnv, sys_expansion, Expansion_Get),
	    cl_get_setf_expansion(Expansion_Get,
				  [Environment_Param],
				  TrueResult),
	    FnResult=TrueResult
	;   f_sys_get_setf_method_inverse(Form_Param,
					  
					  [ funcall,
					    function(
						     [ setf,
						       
						       [ '#COMMA',
							 [car, sys_form]
						       ]
						     ])
					  ],
					  t,
					  ElseResult),
	    FnResult=ElseResult
	).
:- set_opv(f_sys_expand_or_get_setf_inverse, classof, claz_function),
   set_opv(sys_expand_or_get_setf_inverse, compile_as, kw_function),
   set_opv(sys_expand_or_get_setf_inverse,
	   function,
	   f_sys_expand_or_get_setf_inverse),
   _Ignored6=sys_expand_or_get_setf_inverse.
:- side_effect(assert_lsp(sys_expand_or_get_setf_inverse,
			  wl:lambda_def(defun, sys_expand_or_get_setf_inverse, f_sys_expand_or_get_setf_inverse, [sys_form, sys_environment], [[multiple_value_bind, [sys_expansion, sys_expanded], [macroexpand_1, sys_form, sys_environment], [if, sys_expanded, [get_setf_expansion, sys_expansion, sys_environment], [sys_get_setf_method_inverse, sys_form, ['#BQ', [funcall, function([setf, ['#COMMA', [car, sys_form]]])]], t]]]]))).
:- side_effect(assert_lsp(sys_expand_or_get_setf_inverse,
			  wl:arglist_info(sys_expand_or_get_setf_inverse, [sys_form, sys_environment], [Form_Param, Environment_Param], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:0, req:[sys_form, sys_environment], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_expand_or_get_setf_inverse,
			  wl:init_args(exact_only, sys_expand_or_get_setf_inverse))).
(defun get-setf-expansion (form &optional environment)
  (let (temp)
    (cond ((symbolp form)
           (multiple-value-bind (expansion expanded)
               (macroexpand-1 form environment)
             (if expanded
                 (get-setf-expansion expansion environment)
                 (let ((new-var (gensym)))
                   (values nil nil (list new-var)
                           `(setq ,form ,new-var) form)))))
          ((setq temp (get (car form) 'setf-inverse))
           (get-setf-method-inverse form `(,temp) nil))
          ((setq temp (get (car form) 'setf-expander))
           (funcall temp form environment))
          (t
           (expand-or-get-setf-inverse form environment)))))

wl:lambda_def(defun, get_setf_expansion, cl_get_setf_expansion, [sys_form, c38_optional, sys_environment], [[let, [sys_temp], [cond, [[symbolp, sys_form], [multiple_value_bind, [sys_expansion, sys_expanded], [macroexpand_1, sys_form, sys_environment], [if, sys_expanded, [get_setf_expansion, sys_expansion, sys_environment], [let, [[sys_new_var, [gensym]]], [values, [], [], [list, sys_new_var], ['#BQ', [setq, ['#COMMA', sys_form], ['#COMMA', sys_new_var]]], sys_form]]]]], [[setq, sys_temp, [get, [car, sys_form], [quote, sys_setf_inverse]]], [sys_get_setf_method_inverse, sys_form, ['#BQ', [['#COMMA', sys_temp]]], []]], [[setq, sys_temp, [get, [car, sys_form], [quote, sys_setf_expander]]], [funcall, sys_temp, sys_form, sys_environment]], [t, [sys_expand_or_get_setf_inverse, sys_form, sys_environment]]]]]).
wl:arglist_info(get_setf_expansion, [sys_form, c38_optional, sys_environment], [sys_form, sys_environment], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:[sys_environment], req:[sys_form], rest:0, sublists:0, whole:0}).
wl: init_args(1, get_setf_expansion).

```

### Compiled:  `CL:GET-SETF-EXPANSION` 
```prolog
cl_get_setf_expansion(Form_Param, RestNKeys, LetResult27) :-
	Env=[bv(sys_form, Form_Param), bv(sys_environment, Environment_Param)],
	opt_var(Env, sys_environment, Environment_Param, true, [], 1, RestNKeys),
	LEnv=[[bv(sys_temp, [])]|Env],
	(   is_symbolp(Form_Param)
	->  LEnv26=[[bv(sys_expansion, []), bv(sys_expanded, [])]|LEnv],
	    get_var(LEnv26, sys_environment, Environment_Get),
	    cl_macroexpand_1([Form_Param, Environment_Get], Macroexpand_1_Ret),
	    setq_from_values(LEnv26, [sys_expansion, sys_expanded]),
	    get_var(LEnv26, sys_expanded, IFTEST30),
	    (   IFTEST30\==[]
	    ->  get_var(LEnv26, sys_environment, Environment_Get34),
		get_var(LEnv26, sys_expansion, Expansion_Get),
		cl_get_setf_expansion(Expansion_Get,
				      [Environment_Get34],
				      TrueResult),
		LetResult27=TrueResult
	    ;   cl_gensym(New_var_Init),
		LEnv35=[[bv(sys_new_var, New_var_Init)]|LEnv26],
		get_var(LEnv35, sys_new_var, New_var_Get),
		CAR=[New_var_Get],
		get_var(LEnv35, sys_new_var, New_var_Get40),
		nb_setval('$mv_return',
			  
			  [ [],
			    [],
			    CAR,
			    [setq, Form_Param, New_var_Get40],
			    Form_Param
			  ]),
		LetResult27=[]
	    )
	;   cl_car(Form_Param, Get_Param),
	    cl_get(Get_Param, sys_setf_inverse, [], IFTEST45),
	    set_var(LEnv, sys_temp, IFTEST45),
	    (   IFTEST45\==[]
	    ->  get_var(LEnv, sys_temp, Temp_Get),
		f_sys_get_setf_method_inverse(Form_Param,
					      [Temp_Get],
					      [],
					      TrueResult60),
		LetResult27=TrueResult60
	    ;   cl_car(Form_Param, Get_Param67),
		cl_get(Get_Param67, sys_setf_expander, [], IFTEST51),
		set_var(LEnv, sys_temp, IFTEST51),
		(   IFTEST51\==[]
		->  get_var(LEnv, sys_environment, Environment_Get55),
		    f_sys_temp(Form_Param, Environment_Get55, TrueResult58),
		    LetResult27=TrueResult58
		;   get_var(LEnv, sys_environment, Environment_Get57),
		    f_sys_expand_or_get_setf_inverse(Form_Param,
						     Environment_Get57,
						     ElseResult),
		    LetResult27=ElseResult
		)
	    )
	).
:- set_opv(cl_get_setf_expansion, classof, claz_function),
   set_opv(get_setf_expansion, compile_as, kw_function),
   set_opv(get_setf_expansion, function, cl_get_setf_expansion),
   _Ignored6=get_setf_expansion.
:- side_effect(assert_lsp(get_setf_expansion,
			  wl:lambda_def(defun, get_setf_expansion, cl_get_setf_expansion, [sys_form, c38_optional, sys_environment], [[let, [sys_temp], [cond, [[symbolp, sys_form], [multiple_value_bind, [sys_expansion, sys_expanded], [macroexpand_1, sys_form, sys_environment], [if, sys_expanded, [get_setf_expansion, sys_expansion, sys_environment], [let, [[sys_new_var, [gensym]]], [values, [], [], [list, sys_new_var], ['#BQ', [setq, ['#COMMA', sys_form], ['#COMMA', sys_new_var]]], sys_form]]]]], [[setq, sys_temp, [get, [car, sys_form], [quote, sys_setf_inverse]]], [sys_get_setf_method_inverse, sys_form, ['#BQ', [['#COMMA', sys_temp]]], []]], [[setq, sys_temp, [get, [car, sys_form], [quote, sys_setf_expander]]], [funcall, sys_temp, sys_form, sys_environment]], [t, [sys_expand_or_get_setf_inverse, sys_form, sys_environment]]]]]))).
:- side_effect(assert_lsp(get_setf_expansion,
			  wl:arglist_info(get_setf_expansion, [sys_form, c38_optional, sys_environment], [sys_form, sys_environment], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:[sys_environment], req:[sys_form], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(get_setf_expansion,
			  wl:init_args(1, get_setf_expansion))).
(defmacro abcl-setf (&rest args &environment environment)
  (let ((numargs (length args)))
    (cond
     ((= numargs 2)
      (let ((place (first args))
            (value-form (second args)))
        (if (atom place)
            `(setq ,place ,value-form)
            (progn
              (multiple-value-bind (dummies vals store-vars setter getter)
                  (get-setf-expansion place environment)
                (let ((inverse (get (car place) 'setf-inverse)))
                  (if (and inverse (eq inverse (car setter)))
                      (if (functionp inverse)
                          `(funcall ,inverse ,@(cdr place) ,value-form)
                          `(,inverse ,@(cdr place) ,value-form))
                      (if (or (null store-vars) (cdr store-vars))
                          `(let* (,@(mapcar #'list dummies vals))
                             (multiple-value-bind ,store-vars ,value-form
                               ,setter))
                          `(let* (,@(mapcar #'list dummies vals)
                                    ,(list (car store-vars) value-form))
                               ,setter)))))))))
     ((oddp numargs)
      (error "Odd number of arguments to SETF."))
     (t
      (do ((a args (cddr a)) (l nil))
          ((null a) `(progn ,@(nreverse l)))
        (setq l (cons (list 'setf (car a) (cadr a)) l)))))))

;;; Redefined in define-modify-macro.lisp.
wl:lambda_def(defmacro, sys_abcl_setf, f_sys_abcl_setf, [c38_rest, args, c38_environment, sys_environment], [progn, [let, [[sys_numargs, [length, args]]], [cond, [[=, sys_numargs, 2], [let, [[sys_place, [first, args]], [sys_value_form, [second, args]]], [if, [atom, sys_place], ['#BQ', [setq, ['#COMMA', sys_place], ['#COMMA', sys_value_form]]], [progn, [multiple_value_bind, [sys_dummies, sys_vals, sys_store_vars, sys_setter, sys_getter], [get_setf_expansion, sys_place, sys_environment], [let, [[sys_inverse, [get, [car, sys_place], [quote, sys_setf_inverse]]]], [if, [and, sys_inverse, [eq, sys_inverse, [car, sys_setter]]], [if, [functionp, sys_inverse], ['#BQ', [funcall, ['#COMMA', sys_inverse], ['#BQ-COMMA-ELIPSE', [cdr, sys_place]], ['#COMMA', sys_value_form]]], ['#BQ', [['#COMMA', sys_inverse], ['#BQ-COMMA-ELIPSE', [cdr, sys_place]], ['#COMMA', sys_value_form]]]], [if, [or, [null, sys_store_vars], [cdr, sys_store_vars]], ['#BQ', [let_xx, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]]], [multiple_value_bind, ['#COMMA', sys_store_vars], ['#COMMA', sys_value_form], ['#COMMA', sys_setter]]]], ['#BQ', [let_xx, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]], ['#COMMA', [list, [car, sys_store_vars], sys_value_form]]], ['#COMMA', sys_setter]]]]]]]]]]], [[oddp, sys_numargs], [error, '$ARRAY'([*], claz_base_character, "Odd number of arguments to SETF.")]], [t, [do, [[sys_a, args, [cddr, sys_a]], [sys_l, []]], [[null, sys_a], ['#BQ', [progn, ['#BQ-COMMA-ELIPSE', [nreverse, sys_l]]]]], [setq, sys_l, [cons, [list, [quote, setf], [car, sys_a], [cadr, sys_a]], sys_l]]]]]]]).
wl:arglist_info(sys_abcl_setf, [c38_rest, args, c38_environment, sys_environment], [args, sys_environment], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[sys_environment], key:0, names:[args, sys_environment], opt:0, req:0, rest:[args], sublists:0, whole:0}).
wl: init_args(0, sys_abcl_setf).

```

### Compiled:  `SYS::ABCL-SETF` 
```prolog
f_sys_abcl_setf(Args_Param, FnResult) :-
	TLEnv7=[bv(args, Args_Param), bv(sys_environment, Environment_Param)],
	get_env(TLEnv7, sys_environment, Environment_Param),
	catch(( ( get_var(TLEnv7, args, Args_Get),
		  cl_length(Args_Get, Numargs_Init),
		  LEnv=[[bv(sys_numargs, Numargs_Init)]|TLEnv7],
		  get_var(LEnv, sys_numargs, Numargs_Get),
		  (   Numargs_Get=:=2
		  ->  get_var(LEnv, args, Args_Get29),
		      cl_car(Args_Get29, Place_Init),
		      get_var(LEnv, args, Args_Get30),
		      cl_second(Args_Get30, Value_form_Init),
		      LEnv27=[[bv(sys_place, Place_Init), bv(sys_value_form, Value_form_Init)]|LEnv],
		      get_var(LEnv27, sys_place, Place_Get),
		      (   Place_Get\=[CAR|CDR]
		      ->  get_var(LEnv27, sys_place, Place_Get37),
			  get_var(LEnv27, sys_value_form, Value_form_Get),
			  LetResult28=[setq, Place_Get37, Value_form_Get]
		      ;   LEnv39=[[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_store_vars, []), bv(sys_setter, []), bv(sys_getter, [])]|LEnv27],
			  get_var(LEnv39, sys_environment, Environment_Get),
			  get_var(LEnv39, sys_place, Place_Get41),
			  cl_get_setf_expansion(Place_Get41,
						[Environment_Get],
						Setf_expansion_Ret),
			  setq_from_values(LEnv39,
					   
					   [ sys_dummies,
					     sys_vals,
					     sys_store_vars,
					     sys_setter,
					     sys_getter
					   ]),
			  get_var(LEnv39, sys_place, Place_Get45),
			  cl_car(Place_Get45, Get_Param),
			  cl_get(Get_Param, sys_setf_inverse, [], Inverse_Init),
			  LEnv43=[[bv(sys_inverse, Inverse_Init)]|LEnv39],
			  get_var(LEnv43, sys_inverse, IFTEST49),
			  (   IFTEST49\==[]
			  ->  get_var(LEnv43, sys_inverse, Inverse_Get52),
			      get_var(LEnv43, sys_setter, Setter_Get),
			      cl_car(Setter_Get, Car_Ret),
			      cl_eq(Inverse_Get52, Car_Ret, TrueResult),
			      IFTEST47=TrueResult
			  ;   IFTEST47=[]
			  ),
			  (   IFTEST47\==[]
			  ->  get_var(LEnv43, sys_inverse, Inverse_Get56),
			      (   is_functionp(Inverse_Get56)
			      ->  get_var(LEnv43, sys_inverse, Inverse_Get59),
				  get_var(LEnv43, sys_place, Place_Get60),
				  cl_cdr(Place_Get60, Cdr_Ret),
				  get_var(LEnv43,
					  sys_value_form,
					  Value_form_Get61),
				  bq_append([Inverse_Get59|Cdr_Ret],
					    [Value_form_Get61],
					    Bq_append_Ret),
				  LetResult28=[funcall|Bq_append_Ret]
			      ;   get_var(LEnv43, sys_inverse, Inverse_Get62),
				  get_var(LEnv43, sys_place, Place_Get63),
				  cl_cdr(Place_Get63, Cdr_Ret146),
				  get_var(LEnv43,
					  sys_value_form,
					  Value_form_Get64),
				  bq_append([Inverse_Get62|Cdr_Ret146],
					    [Value_form_Get64],
					    ElseResult),
				  LetResult28=ElseResult
			      )
			  ;   (   get_var(LEnv43,
					  sys_store_vars,
					  Store_vars_Get),
				  cl_null(Store_vars_Get, FORM1_Res),
				  FORM1_Res\==[],
				  IFTEST66=FORM1_Res
			      ->  true
			      ;   get_var(LEnv43,
					  sys_store_vars,
					  Store_vars_Get69),
				  cl_cdr(Store_vars_Get69, Cdr_Ret147),
				  IFTEST66=Cdr_Ret147
			      ),
			      (   IFTEST66\==[]
			      ->  get_var(LEnv43, sys_dummies, Dummies_Get),
				  get_var(LEnv43, sys_vals, Vals_Get),
				  cl_mapcar(function(list),
					    [Dummies_Get, Vals_Get],
					    Mapcar_Ret),
				  get_var(LEnv43, sys_setter, Setter_Get75),
				  get_var(LEnv43,
					  sys_store_vars,
					  Store_vars_Get73),
				  get_var(LEnv43,
					  sys_value_form,
					  Value_form_Get74),
				  LetResult28=[let_xx, Mapcar_Ret, [multiple_value_bind, Store_vars_Get73, Value_form_Get74, Setter_Get75]]
			      ;   get_var(LEnv43, sys_dummies, Dummies_Get76),
				  get_var(LEnv43, sys_vals, Vals_Get77),
				  cl_mapcar(function(list),
					    [Dummies_Get76, Vals_Get77],
					    Bq_append_Param),
				  get_var(LEnv43,
					  sys_store_vars,
					  Store_vars_Get78),
				  cl_car(Store_vars_Get78, Car_Ret149),
				  get_var(LEnv43,
					  sys_value_form,
					  Value_form_Get79),
				  CAR151=[Car_Ret149, Value_form_Get79],
				  bq_append(Bq_append_Param,
					    [CAR151],
					    Bq_append_Ret150),
				  get_var(LEnv43, sys_setter, Setter_Get80),
				  LetResult28=[let_xx, Bq_append_Ret150, Setter_Get80]
			      )
			  )
		      )
		  ;   get_var(LEnv, sys_numargs, Numargs_Get88),
		      (   mth:is_oddp(Numargs_Get88)
		      ->  cl_error(
				   [ '$ARRAY'([*],
					      claz_base_character,
					      "Odd number of arguments to SETF.")
				   ],
				   TrueResult131),
			  LetResult28=TrueResult131
		      ;   get_var(LEnv, args, Args_Get93),
			  GoEnv=[[bv(sys_a, Args_Get93), bv(sys_l, [])]|LEnv],
			  catch(( call_addr_block(GoEnv,
						  (push_label(do_label_1), get_var(GoEnv, sys_a, IFTEST114), (IFTEST114==[]->cl_nreverse(L_Get107, Nreverse_Ret), throw(block_exit([], [progn|Nreverse_Ret])), _TBResult=ThrowResult118;cl_car(IFTEST96, Car_Ret153), cl_cadr(IFTEST96, Cadr_Ret), CAR155=[setf, Car_Ret153, Cadr_Ret], get_var(GoEnv, sys_l, L_Get124), L=[CAR155|L_Get124], set_var(GoEnv, sys_l, L), get_var(GoEnv, sys_a, A_Get125), cl_cddr(A_Get125, A), set_var(GoEnv, sys_a, A), goto(do_label_1, GoEnv), _TBResult=_GORES126)),
						  
						  [ addr(addr_tagbody_1_do_label_1,
							 do_label_1,
							 '$unused',
							 AEnv,
							 (get_var(AEnv, sys_a, IFTEST96), (IFTEST96==[]->get_var(AEnv, sys_l, L_Get107), cl_nreverse(L_Get107, Nreverse_Ret156), throw(block_exit([], [progn|Nreverse_Ret156])), _30242=ThrowResult;cl_car(IFTEST96, Car_Ret157), cl_cadr(IFTEST96, Cadr_Ret158), CAR159=[setf, Car_Ret157, Cadr_Ret158], Set_var_Ret=[CAR159|L_Get107], set_var(AEnv, sys_l, Set_var_Ret), cl_cddr(IFTEST96, Cddr_Ret), set_var(AEnv, sys_a, Cddr_Ret), goto(do_label_1, AEnv), _30242=_GORES)))
						  ]),
				  []=LetResult92
				),
				block_exit([], LetResult92),
				true),
			  LetResult28=LetResult92
		      )
		  )
		),
		LetResult28=MFResult
	      ),
	      block_exit(sys_abcl_setf, MFResult),
	      true),
	cl_eval(MFResult, FnResult).
:- set_opv(f_sys_abcl_setf, classof, claz_macro),
   set_opv(sys_abcl_setf, compile_as, kw_operator),
   set_opv(sys_abcl_setf, function, f_sys_abcl_setf),
   _Ignored6=sys_abcl_setf.
:- side_effect(assert_lsp(sys_abcl_setf,
			  wl:lambda_def(defmacro, sys_abcl_setf, f_sys_abcl_setf, [c38_rest, args, c38_environment, sys_environment], [progn, [let, [[sys_numargs, [length, args]]], [cond, [[=, sys_numargs, 2], [let, [[sys_place, [first, args]], [sys_value_form, [second, args]]], [if, [atom, sys_place], ['#BQ', [setq, ['#COMMA', sys_place], ['#COMMA', sys_value_form]]], [progn, [multiple_value_bind, [sys_dummies, sys_vals, sys_store_vars, sys_setter, sys_getter], [get_setf_expansion, sys_place, sys_environment], [let, [[sys_inverse, [get, [car, sys_place], [quote, sys_setf_inverse]]]], [if, [and, sys_inverse, [eq, sys_inverse, [car, sys_setter]]], [if, [functionp, sys_inverse], ['#BQ', [funcall, ['#COMMA', sys_inverse], ['#BQ-COMMA-ELIPSE', [cdr, sys_place]], ['#COMMA', sys_value_form]]], ['#BQ', [['#COMMA', sys_inverse], ['#BQ-COMMA-ELIPSE', [cdr, sys_place]], ['#COMMA', sys_value_form]]]], [if, [or, [null, sys_store_vars], [cdr, sys_store_vars]], ['#BQ', [let_xx, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]]], [multiple_value_bind, ['#COMMA', sys_store_vars], ['#COMMA', sys_value_form], ['#COMMA', sys_setter]]]], ['#BQ', [let_xx, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]], ['#COMMA', [list, [car, sys_store_vars], sys_value_form]]], ['#COMMA', sys_setter]]]]]]]]]]], [[oddp, sys_numargs], [error, '$ARRAY'([*], claz_base_character, "Odd number of arguments to SETF.")]], [t, [do, [[sys_a, args, [cddr, sys_a]], [sys_l, []]], [[null, sys_a], ['#BQ', [progn, ['#BQ-COMMA-ELIPSE', [nreverse, sys_l]]]]], [setq, sys_l, [cons, [list, [quote, setf], [car, sys_a], [cadr, sys_a]], sys_l]]]]]]]))).
:- side_effect(assert_lsp(sys_abcl_setf,
			  wl:arglist_info(sys_abcl_setf, [c38_rest, args, c38_environment, sys_environment], [args, sys_environment], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[sys_environment], key:0, names:[args, sys_environment], opt:0, req:0, rest:[args], sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_abcl_setf, wl:init_args(0, sys_abcl_setf))).
;; Redefined in define-modify-macro.lisp.
(defmacro incf (place &optional (delta 1))
  `(setf ,place (+ ,place ,delta)))

;;; Redefined in define-modify-macro.lisp.
wl:lambda_def(defmacro, incf, cl_incf, [sys_place, c38_optional, [sys_delta, 1]], [progn, ['#BQ', [setf, ['#COMMA', sys_place], [+, ['#COMMA', sys_place], ['#COMMA', sys_delta]]]]]).
wl:arglist_info(incf, [sys_place, c38_optional, [sys_delta, 1]], [sys_place, sys_delta], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_delta], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}).
wl: init_args(1, incf).

```

### Compiled:  `CL:INCF` 
```prolog
cl_incf(Place_Param, RestNKeys, FnResult) :-
	TLEnv7=[bv(sys_place, Place_Param), bv(sys_delta, Delta_Param)],
	opt_var(TLEnv7, sys_delta, Delta_Param, true, 1, 1, RestNKeys),
	get_var(TLEnv7, sys_delta, Delta_Get),
	[setf, Place_Param, [+, Place_Param, Delta_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_incf, classof, claz_macro),
   set_opv(incf, compile_as, kw_operator),
   set_opv(incf, function, cl_incf),
   _Ignored6=incf.
:- side_effect(assert_lsp(incf,
			  wl:lambda_def(defmacro, incf, cl_incf, [sys_place, c38_optional, [sys_delta, 1]], [progn, ['#BQ', [setf, ['#COMMA', sys_place], [+, ['#COMMA', sys_place], ['#COMMA', sys_delta]]]]]))).
:- side_effect(assert_lsp(incf,
			  wl:arglist_info(incf, [sys_place, c38_optional, [sys_delta, 1]], [sys_place, sys_delta], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_delta], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(incf, wl:init_args(1, incf))).
;; Redefined in define-modify-macro.lisp.
(defmacro decf (place &optional (delta 1))
  `(setf ,place (- ,place ,delta)))

;; (defsetf subseq (sequence start &optional (end nil)) (v)
;;   `(progn (replace ,sequence ,v :start1 ,start :end1 ,end)
;;      ,v))
wl:lambda_def(defmacro, decf, cl_decf, [sys_place, c38_optional, [sys_delta, 1]], [progn, ['#BQ', [setf, ['#COMMA', sys_place], [-, ['#COMMA', sys_place], ['#COMMA', sys_delta]]]]]).
wl:arglist_info(decf, [sys_place, c38_optional, [sys_delta, 1]], [sys_place, sys_delta], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_delta], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}).
wl: init_args(1, decf).

```

### Compiled:  `CL:DECF` 
```prolog
cl_decf(Place_Param, RestNKeys, FnResult) :-
	TLEnv7=[bv(sys_place, Place_Param), bv(sys_delta, Delta_Param)],
	opt_var(TLEnv7, sys_delta, Delta_Param, true, 1, 1, RestNKeys),
	get_var(TLEnv7, sys_delta, Delta_Get),
	[setf, Place_Param, [-, Place_Param, Delta_Get]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_decf, classof, claz_macro),
   set_opv(decf, compile_as, kw_operator),
   set_opv(decf, function, cl_decf),
   _Ignored6=decf.
:- side_effect(assert_lsp(decf,
			  wl:lambda_def(defmacro, decf, cl_decf, [sys_place, c38_optional, [sys_delta, 1]], [progn, ['#BQ', [setf, ['#COMMA', sys_place], [-, ['#COMMA', sys_place], ['#COMMA', sys_delta]]]]]))).
:- side_effect(assert_lsp(decf,
			  wl:arglist_info(decf, [sys_place, c38_optional, [sys_delta, 1]], [sys_place, sys_delta], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_delta], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(decf, wl:init_args(1, decf))).
; (defsetf subseq (sequence start &optional (end nil)) (v)
;   `(progn (replace ,sequence ,v :start1 ,start :end1 ,end)
;      ,v))
(defun %set-subseq (sequence start &rest rest)
  (let ((end nil) v)
    (ecase (length rest)
      (1
       (setq v (car rest)))
      (2
       (setq end (car rest)
             v (cadr rest))))
    (progn
      (replace sequence v :start1 start :end1 end)
      v)))

% ecase:-[[1,[setq,sys_v,[car,rest]]],[2,[setq,end,[car,rest],sys_v,[cadr,rest]]]].
% conds:-[[[eq,_55470,[quote,1]],[progn,[setq,sys_v,[car,rest]]]],[[eq,_55470,[quote,2]],[progn,[setq,end,[car,rest],sys_v,[cadr,rest]]]],[t,[type_error,_55706,[quote,[member,1,2]]]]].
wl:lambda_def(defun, sys_pf_set_subseq, f_sys_pf_set_subseq, [sequence, start, c38_rest, rest], [[let, [[end, []], sys_v], [ecase, [length, rest], [1, [setq, sys_v, [car, rest]]], [2, [setq, end, [car, rest], sys_v, [cadr, rest]]]], [progn, [replace, sequence, sys_v, kw_start1, start, kw_end1, end], sys_v]]]).
wl:arglist_info(sys_pf_set_subseq, [sequence, start, c38_rest, rest], [sequence, start, rest], arginfo{all:[sequence, start], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sequence, start, rest], opt:0, req:[sequence, start], rest:[rest], sublists:0, whole:0}).
wl: init_args(2, sys_pf_set_subseq).

```

### Compiled:  `SYS::%SET-SUBSEQ` 
```prolog
f_sys_pf_set_subseq(Sequence_Param, Start_Param, Rest_Param, FnResult) :-
	Env=[bv(sequence, Sequence_Param), bv(start, Start_Param), bv(rest, Rest_Param)],
	LEnv=[[bv(end, []), bv(sys_v, [])]|Env],
	get_var(LEnv, rest, Rest_Get),
	cl_length(Rest_Get, avar(PredArg1Result, att(preserved_var, t, []))),
	(   is_eq(avar(PredArg1Result, att(preserved_var, t, [])), 1)
	->  get_var(LEnv, rest, Rest_Get29),
	    cl_car(Rest_Get29, TrueResult36),
	    set_var(LEnv, sys_v, TrueResult36),
	    ElseResult37=TrueResult36
	;   is_eq(avar(PredArg1Result, att(preserved_var, t, [])), 2)
	->  get_var(LEnv, rest, Rest_Get32),
	    cl_car(Rest_Get32, End),
	    set_var(LEnv, end, End),
	    get_var(LEnv, rest, Rest_Get33),
	    cl_cadr(Rest_Get33, TrueResult),
	    set_var(LEnv, sys_v, TrueResult),
	    ElseResult37=TrueResult
	;   cl_type_error(avar(CAR, att(preserved_var, t, [])),
			  [member, 1, 2],
			  ElseResult),
	    ElseResult37=ElseResult
	),
	get_var(LEnv, end, End_Get),
	get_var(LEnv, sys_v, V_Get),
	cl_replace(Sequence_Param,
		   V_Get,
		   [kw_start1, Start_Param, kw_end1, End_Get],
		   Replace_Ret),
	get_var(LEnv, sys_v, V_Get42),
	V_Get42=FnResult.
:- set_opv(f_sys_pf_set_subseq, classof, claz_function),
   set_opv(sys_pf_set_subseq, compile_as, kw_function),
   set_opv(sys_pf_set_subseq, function, f_sys_pf_set_subseq),
   _Ignored6=sys_pf_set_subseq.
:- side_effect(assert_lsp(sys_pf_set_subseq,
			  wl:lambda_def(defun, sys_pf_set_subseq, f_sys_pf_set_subseq, [sequence, start, c38_rest, rest], [[let, [[end, []], sys_v], [ecase, [length, rest], [1, [setq, sys_v, [car, rest]]], [2, [setq, end, [car, rest], sys_v, [cadr, rest]]]], [progn, [replace, sequence, sys_v, kw_start1, start, kw_end1, end], sys_v]]]))).
:- side_effect(assert_lsp(sys_pf_set_subseq,
			  wl:arglist_info(sys_pf_set_subseq, [sequence, start, c38_rest, rest], [sequence, start, rest], arginfo{all:[sequence, start], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sequence, start, rest], opt:0, req:[sequence, start], rest:[rest], sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_subseq, wl:init_args(2, sys_pf_set_subseq))).
(defun %define-setf-macro (name expander inverse doc)
  (declare (ignore doc)) ; FIXME
  (when inverse
    (put name 'setf-inverse inverse))
  (when expander
    (put name 'setf-expander expander))
  name)

wl:lambda_def(defun, sys_pf_define_setf_macro, f_sys_pf_define_setf_type_macro, [sys_name, sys_expander, sys_inverse, sys_doc], [[declare, [ignore, sys_doc]], [when, sys_inverse, [sys_put, sys_name, [quote, sys_setf_inverse], sys_inverse]], [when, sys_expander, [sys_put, sys_name, [quote, sys_setf_expander], sys_expander]], sys_name]).
wl:arglist_info(sys_pf_define_setf_macro, [sys_name, sys_expander, sys_inverse, sys_doc], [Name_Param, Expander_Param, Inverse_Param, Doc_Param], arginfo{all:[sys_name, sys_expander, sys_inverse, sys_doc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_expander, sys_inverse, sys_doc], opt:0, req:[sys_name, sys_expander, sys_inverse, sys_doc], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_define_setf_macro).

```

### Compiled:  `SYS::%DEFINE-SETF-MACRO` 
```prolog
f_sys_pf_define_setf_type_macro(Name_Param, Expander_Param, Inverse_Param, Doc_Param, Name_Param) :-
	Env=[bv(sys_doc, Doc_Param)],
	cl_declare([ignore, sys_doc], Declare_Ret),
	(   Inverse_Param\==[]
	->  f_sys_put(Name_Param, sys_setf_inverse, Inverse_Param, TrueResult),
	    _24678=TrueResult
	;   _24678=[]
	),
	(   Expander_Param\==[]
	->  f_sys_put(Name_Param,
		      sys_setf_expander,
		      Expander_Param,
		      TrueResult33),
	    _24838=TrueResult33
	;   _24838=[]
	).
:- set_opv(f_sys_pf_define_setf_type_macro, classof, claz_function),
   set_opv(sys_pf_define_setf_macro, compile_as, kw_function),
   set_opv(sys_pf_define_setf_macro, function, f_sys_pf_define_setf_type_macro),
   _Ignored6=sys_pf_define_setf_macro.
:- side_effect(assert_lsp(sys_pf_define_setf_macro,
			  wl:lambda_def(defun, sys_pf_define_setf_macro, f_sys_pf_define_setf_type_macro, [sys_name, sys_expander, sys_inverse, sys_doc], [[declare, [ignore, sys_doc]], [when, sys_inverse, [sys_put, sys_name, [quote, sys_setf_inverse], sys_inverse]], [when, sys_expander, [sys_put, sys_name, [quote, sys_setf_expander], sys_expander]], sys_name]))).
:- side_effect(assert_lsp(sys_pf_define_setf_macro,
			  wl:arglist_info(sys_pf_define_setf_macro, [sys_name, sys_expander, sys_inverse, sys_doc], [Name_Param, Expander_Param, Inverse_Param, Doc_Param], arginfo{all:[sys_name, sys_expander, sys_inverse, sys_doc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_expander, sys_inverse, sys_doc], opt:0, req:[sys_name, sys_expander, sys_inverse, sys_doc], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_define_setf_macro,
			  wl:init_args(exact_only, sys_pf_define_setf_macro))).
 FIXME
(defmacro defsetf (access-function update-function)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (put ',access-function 'setf-inverse ',update-function)))

wl:lambda_def(defmacro, defsetf, cl_defsetf, [sys_access_function, sys_update_function], [progn, ['#BQ', [eval_when, [kw_load_toplevel, kw_compile_toplevel, kw_execute], [sys_put, [quote, ['#COMMA', sys_access_function]], [quote, sys_setf_inverse], [quote, ['#COMMA', sys_update_function]]]]]]).
wl:arglist_info(defsetf, [sys_access_function, sys_update_function], [Access_function_Param, Update_function_Param], arginfo{all:[sys_access_function, sys_update_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_access_function, sys_update_function], opt:0, req:[sys_access_function, sys_update_function], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, defsetf).

```

### Compiled:  `CL:DEFSETF` 
```prolog
cl_defsetf(Access_function_Param, Update_function_Param, FnResult) :-
	[eval_when, [kw_load_toplevel, kw_compile_toplevel, kw_execute], [sys_put, [quote, Access_function_Param], [quote, sys_setf_inverse], [quote, Update_function_Param]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_defsetf, classof, claz_macro),
   set_opv(defsetf, compile_as, kw_operator),
   set_opv(defsetf, function, cl_defsetf),
   _Ignored6=defsetf.
:- side_effect(assert_lsp(defsetf,
			  wl:lambda_def(defmacro, defsetf, cl_defsetf, [sys_access_function, sys_update_function], [progn, ['#BQ', [eval_when, [kw_load_toplevel, kw_compile_toplevel, kw_execute], [sys_put, [quote, ['#COMMA', sys_access_function]], [quote, sys_setf_inverse], [quote, ['#COMMA', sys_update_function]]]]]]))).
:- side_effect(assert_lsp(defsetf,
			  wl:arglist_info(defsetf, [sys_access_function, sys_update_function], [Access_function_Param, Update_function_Param], arginfo{all:[sys_access_function, sys_update_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_access_function, sys_update_function], opt:0, req:[sys_access_function, sys_update_function], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(defsetf, wl:init_args(exact_only, defsetf))).
(defun %set-caar (x v) (set-car (car x) v))
wl:lambda_def(defun, sys_pf_set_caar, f_sys_pf_set_caar, [sys_x, sys_v], [[sys_set_car, [car, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caar).

```

### Compiled:  `SYS::%SET-CAAR` 
```prolog
f_sys_pf_set_caar(X_Param, V_Param, FnResult) :-
	cl_car(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caar, classof, claz_function),
   set_opv(sys_pf_set_caar, compile_as, kw_function),
   set_opv(sys_pf_set_caar, function, f_sys_pf_set_caar),
   _Ignored6=sys_pf_set_caar.
:- side_effect(assert_lsp(sys_pf_set_caar,
			  wl:lambda_def(defun, sys_pf_set_caar, f_sys_pf_set_caar, [sys_x, sys_v], [[sys_set_car, [car, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_caar,
			  wl:arglist_info(sys_pf_set_caar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_caar,
			  wl:init_args(exact_only, sys_pf_set_caar))).
(defun %set-cadr (x v) (set-car (cdr x) v))
wl:lambda_def(defun, sys_pf_set_cadr, f_sys_pf_set_cadr, [sys_x, sys_v], [[sys_set_car, [cdr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cadr).

```

### Compiled:  `SYS::%SET-CADR` 
```prolog
f_sys_pf_set_cadr(X_Param, V_Param, FnResult) :-
	cl_cdr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadr, classof, claz_function),
   set_opv(sys_pf_set_cadr, compile_as, kw_function),
   set_opv(sys_pf_set_cadr, function, f_sys_pf_set_cadr),
   _Ignored6=sys_pf_set_cadr.
:- side_effect(assert_lsp(sys_pf_set_cadr,
			  wl:lambda_def(defun, sys_pf_set_cadr, f_sys_pf_set_cadr, [sys_x, sys_v], [[sys_set_car, [cdr, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cadr,
			  wl:arglist_info(sys_pf_set_cadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cadr,
			  wl:init_args(exact_only, sys_pf_set_cadr))).
(defun %set-cdar (x v) (set-cdr (car x) v))
wl:lambda_def(defun, sys_pf_set_cdar, f_sys_pf_set_cdar, [sys_x, sys_v], [[sys_set_cdr, [car, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdar).

```

### Compiled:  `SYS::%SET-CDAR` 
```prolog
f_sys_pf_set_cdar(X_Param, V_Param, FnResult) :-
	cl_car(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdar, classof, claz_function),
   set_opv(sys_pf_set_cdar, compile_as, kw_function),
   set_opv(sys_pf_set_cdar, function, f_sys_pf_set_cdar),
   _Ignored6=sys_pf_set_cdar.
:- side_effect(assert_lsp(sys_pf_set_cdar,
			  wl:lambda_def(defun, sys_pf_set_cdar, f_sys_pf_set_cdar, [sys_x, sys_v], [[sys_set_cdr, [car, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cdar,
			  wl:arglist_info(sys_pf_set_cdar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cdar,
			  wl:init_args(exact_only, sys_pf_set_cdar))).
(defun %set-cddr (x v) (set-cdr (cdr x) v))
wl:lambda_def(defun, sys_pf_set_cddr, f_sys_pf_set_cddr, [sys_x, sys_v], [[sys_set_cdr, [cdr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cddr).

```

### Compiled:  `SYS::%SET-CDDR` 
```prolog
f_sys_pf_set_cddr(X_Param, V_Param, FnResult) :-
	cl_cdr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddr, classof, claz_function),
   set_opv(sys_pf_set_cddr, compile_as, kw_function),
   set_opv(sys_pf_set_cddr, function, f_sys_pf_set_cddr),
   _Ignored6=sys_pf_set_cddr.
:- side_effect(assert_lsp(sys_pf_set_cddr,
			  wl:lambda_def(defun, sys_pf_set_cddr, f_sys_pf_set_cddr, [sys_x, sys_v], [[sys_set_cdr, [cdr, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cddr,
			  wl:arglist_info(sys_pf_set_cddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cddr,
			  wl:init_args(exact_only, sys_pf_set_cddr))).
(defun %set-caaar (x v) (set-car (caar x) v))
wl:lambda_def(defun, sys_pf_set_caaar, f_sys_pf_set_caaar, [sys_x, sys_v], [[sys_set_car, [caar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caaar).

```

### Compiled:  `SYS::%SET-CAAAR` 
```prolog
f_sys_pf_set_caaar(X_Param, V_Param, FnResult) :-
	cl_caar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caaar, classof, claz_function),
   set_opv(sys_pf_set_caaar, compile_as, kw_function),
   set_opv(sys_pf_set_caaar, function, f_sys_pf_set_caaar),
   _Ignored6=sys_pf_set_caaar.
:- side_effect(assert_lsp(sys_pf_set_caaar,
			  wl:lambda_def(defun, sys_pf_set_caaar, f_sys_pf_set_caaar, [sys_x, sys_v], [[sys_set_car, [caar, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_caaar,
			  wl:arglist_info(sys_pf_set_caaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_caaar,
			  wl:init_args(exact_only, sys_pf_set_caaar))).
(defun %set-cadar (x v) (set-car (cdar x) v))
wl:lambda_def(defun, sys_pf_set_cadar, f_sys_pf_set_cadar, [sys_x, sys_v], [[sys_set_car, [cdar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cadar).

```

### Compiled:  `SYS::%SET-CADAR` 
```prolog
f_sys_pf_set_cadar(X_Param, V_Param, FnResult) :-
	cl_cdar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadar, classof, claz_function),
   set_opv(sys_pf_set_cadar, compile_as, kw_function),
   set_opv(sys_pf_set_cadar, function, f_sys_pf_set_cadar),
   _Ignored6=sys_pf_set_cadar.
:- side_effect(assert_lsp(sys_pf_set_cadar,
			  wl:lambda_def(defun, sys_pf_set_cadar, f_sys_pf_set_cadar, [sys_x, sys_v], [[sys_set_car, [cdar, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cadar,
			  wl:arglist_info(sys_pf_set_cadar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cadar,
			  wl:init_args(exact_only, sys_pf_set_cadar))).
(defun %set-cdaar (x v) (set-cdr (caar x) v))
wl:lambda_def(defun, sys_pf_set_cdaar, f_sys_pf_set_cdaar, [sys_x, sys_v], [[sys_set_cdr, [caar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdaar).

```

### Compiled:  `SYS::%SET-CDAAR` 
```prolog
f_sys_pf_set_cdaar(X_Param, V_Param, FnResult) :-
	cl_caar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdaar, classof, claz_function),
   set_opv(sys_pf_set_cdaar, compile_as, kw_function),
   set_opv(sys_pf_set_cdaar, function, f_sys_pf_set_cdaar),
   _Ignored6=sys_pf_set_cdaar.
:- side_effect(assert_lsp(sys_pf_set_cdaar,
			  wl:lambda_def(defun, sys_pf_set_cdaar, f_sys_pf_set_cdaar, [sys_x, sys_v], [[sys_set_cdr, [caar, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cdaar,
			  wl:arglist_info(sys_pf_set_cdaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cdaar,
			  wl:init_args(exact_only, sys_pf_set_cdaar))).
(defun %set-cddar (x v) (set-cdr (cdar x) v))
wl:lambda_def(defun, sys_pf_set_cddar, f_sys_pf_set_cddar, [sys_x, sys_v], [[sys_set_cdr, [cdar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cddar).

```

### Compiled:  `SYS::%SET-CDDAR` 
```prolog
f_sys_pf_set_cddar(X_Param, V_Param, FnResult) :-
	cl_cdar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddar, classof, claz_function),
   set_opv(sys_pf_set_cddar, compile_as, kw_function),
   set_opv(sys_pf_set_cddar, function, f_sys_pf_set_cddar),
   _Ignored6=sys_pf_set_cddar.
:- side_effect(assert_lsp(sys_pf_set_cddar,
			  wl:lambda_def(defun, sys_pf_set_cddar, f_sys_pf_set_cddar, [sys_x, sys_v], [[sys_set_cdr, [cdar, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cddar,
			  wl:arglist_info(sys_pf_set_cddar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cddar,
			  wl:init_args(exact_only, sys_pf_set_cddar))).
(defun %set-caadr (x v) (set-car (cadr x) v))
wl:lambda_def(defun, sys_pf_set_caadr, f_sys_pf_set_caadr, [sys_x, sys_v], [[sys_set_car, [cadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caadr).

```

### Compiled:  `SYS::%SET-CAADR` 
```prolog
f_sys_pf_set_caadr(X_Param, V_Param, FnResult) :-
	cl_cadr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caadr, classof, claz_function),
   set_opv(sys_pf_set_caadr, compile_as, kw_function),
   set_opv(sys_pf_set_caadr, function, f_sys_pf_set_caadr),
   _Ignored6=sys_pf_set_caadr.
:- side_effect(assert_lsp(sys_pf_set_caadr,
			  wl:lambda_def(defun, sys_pf_set_caadr, f_sys_pf_set_caadr, [sys_x, sys_v], [[sys_set_car, [cadr, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_caadr,
			  wl:arglist_info(sys_pf_set_caadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_caadr,
			  wl:init_args(exact_only, sys_pf_set_caadr))).
(defun %set-caddr (x v) (set-car (cddr x) v))
wl:lambda_def(defun, sys_pf_set_caddr, f_sys_pf_set_caddr, [sys_x, sys_v], [[sys_set_car, [cddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caddr).

```

### Compiled:  `SYS::%SET-CADDR` 
```prolog
f_sys_pf_set_caddr(X_Param, V_Param, FnResult) :-
	cl_cddr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caddr, classof, claz_function),
   set_opv(sys_pf_set_caddr, compile_as, kw_function),
   set_opv(sys_pf_set_caddr, function, f_sys_pf_set_caddr),
   _Ignored6=sys_pf_set_caddr.
:- side_effect(assert_lsp(sys_pf_set_caddr,
			  wl:lambda_def(defun, sys_pf_set_caddr, f_sys_pf_set_caddr, [sys_x, sys_v], [[sys_set_car, [cddr, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_caddr,
			  wl:arglist_info(sys_pf_set_caddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_caddr,
			  wl:init_args(exact_only, sys_pf_set_caddr))).
(defun %set-cdadr (x v) (set-cdr (cadr x) v))
wl:lambda_def(defun, sys_pf_set_cdadr, f_sys_pf_set_cdadr, [sys_x, sys_v], [[sys_set_cdr, [cadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdadr).

```

### Compiled:  `SYS::%SET-CDADR` 
```prolog
f_sys_pf_set_cdadr(X_Param, V_Param, FnResult) :-
	cl_cadr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdadr, classof, claz_function),
   set_opv(sys_pf_set_cdadr, compile_as, kw_function),
   set_opv(sys_pf_set_cdadr, function, f_sys_pf_set_cdadr),
   _Ignored6=sys_pf_set_cdadr.
:- side_effect(assert_lsp(sys_pf_set_cdadr,
			  wl:lambda_def(defun, sys_pf_set_cdadr, f_sys_pf_set_cdadr, [sys_x, sys_v], [[sys_set_cdr, [cadr, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cdadr,
			  wl:arglist_info(sys_pf_set_cdadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cdadr,
			  wl:init_args(exact_only, sys_pf_set_cdadr))).
(defun %set-cdddr (x v) (set-cdr (cddr x) v))
wl:lambda_def(defun, sys_pf_set_cdddr, f_sys_pf_set_cdddr, [sys_x, sys_v], [[sys_set_cdr, [cddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdddr).

```

### Compiled:  `SYS::%SET-CDDDR` 
```prolog
f_sys_pf_set_cdddr(X_Param, V_Param, FnResult) :-
	cl_cddr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdddr, classof, claz_function),
   set_opv(sys_pf_set_cdddr, compile_as, kw_function),
   set_opv(sys_pf_set_cdddr, function, f_sys_pf_set_cdddr),
   _Ignored6=sys_pf_set_cdddr.
:- side_effect(assert_lsp(sys_pf_set_cdddr,
			  wl:lambda_def(defun, sys_pf_set_cdddr, f_sys_pf_set_cdddr, [sys_x, sys_v], [[sys_set_cdr, [cddr, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cdddr,
			  wl:arglist_info(sys_pf_set_cdddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cdddr,
			  wl:init_args(exact_only, sys_pf_set_cdddr))).
(defun %set-caaaar (x v) (set-car (caaar x) v))
wl:lambda_def(defun, sys_pf_set_caaaar, f_sys_pf_set_caaaar, [sys_x, sys_v], [[sys_set_car, [caaar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caaaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caaaar).

```

### Compiled:  `SYS::%SET-CAAAAR` 
```prolog
f_sys_pf_set_caaaar(X_Param, V_Param, FnResult) :-
	cl_caaar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caaaar, classof, claz_function),
   set_opv(sys_pf_set_caaaar, compile_as, kw_function),
   set_opv(sys_pf_set_caaaar, function, f_sys_pf_set_caaaar),
   _Ignored6=sys_pf_set_caaaar.
:- side_effect(assert_lsp(sys_pf_set_caaaar,
			  wl:lambda_def(defun, sys_pf_set_caaaar, f_sys_pf_set_caaaar, [sys_x, sys_v], [[sys_set_car, [caaar, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_caaaar,
			  wl:arglist_info(sys_pf_set_caaaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_caaaar,
			  wl:init_args(exact_only, sys_pf_set_caaaar))).
(defun %set-cadaar (x v) (set-car (cdaar x) v))
wl:lambda_def(defun, sys_pf_set_cadaar, f_sys_pf_set_cadaar, [sys_x, sys_v], [[sys_set_car, [cdaar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cadaar).

```

### Compiled:  `SYS::%SET-CADAAR` 
```prolog
f_sys_pf_set_cadaar(X_Param, V_Param, FnResult) :-
	cl_cdaar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadaar, classof, claz_function),
   set_opv(sys_pf_set_cadaar, compile_as, kw_function),
   set_opv(sys_pf_set_cadaar, function, f_sys_pf_set_cadaar),
   _Ignored6=sys_pf_set_cadaar.
:- side_effect(assert_lsp(sys_pf_set_cadaar,
			  wl:lambda_def(defun, sys_pf_set_cadaar, f_sys_pf_set_cadaar, [sys_x, sys_v], [[sys_set_car, [cdaar, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cadaar,
			  wl:arglist_info(sys_pf_set_cadaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cadaar,
			  wl:init_args(exact_only, sys_pf_set_cadaar))).
(defun %set-cdaaar (x v) (set-cdr (caaar x) v))
wl:lambda_def(defun, sys_pf_set_cdaaar, f_sys_pf_set_cdaaar, [sys_x, sys_v], [[sys_set_cdr, [caaar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdaaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdaaar).

```

### Compiled:  `SYS::%SET-CDAAAR` 
```prolog
f_sys_pf_set_cdaaar(X_Param, V_Param, FnResult) :-
	cl_caaar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdaaar, classof, claz_function),
   set_opv(sys_pf_set_cdaaar, compile_as, kw_function),
   set_opv(sys_pf_set_cdaaar, function, f_sys_pf_set_cdaaar),
   _Ignored6=sys_pf_set_cdaaar.
:- side_effect(assert_lsp(sys_pf_set_cdaaar,
			  wl:lambda_def(defun, sys_pf_set_cdaaar, f_sys_pf_set_cdaaar, [sys_x, sys_v], [[sys_set_cdr, [caaar, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cdaaar,
			  wl:arglist_info(sys_pf_set_cdaaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cdaaar,
			  wl:init_args(exact_only, sys_pf_set_cdaaar))).
(defun %set-cddaar (x v) (set-cdr (cdaar x) v))
wl:lambda_def(defun, sys_pf_set_cddaar, f_sys_pf_set_cddaar, [sys_x, sys_v], [[sys_set_cdr, [cdaar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cddaar).

```

### Compiled:  `SYS::%SET-CDDAAR` 
```prolog
f_sys_pf_set_cddaar(X_Param, V_Param, FnResult) :-
	cl_cdaar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddaar, classof, claz_function),
   set_opv(sys_pf_set_cddaar, compile_as, kw_function),
   set_opv(sys_pf_set_cddaar, function, f_sys_pf_set_cddaar),
   _Ignored6=sys_pf_set_cddaar.
:- side_effect(assert_lsp(sys_pf_set_cddaar,
			  wl:lambda_def(defun, sys_pf_set_cddaar, f_sys_pf_set_cddaar, [sys_x, sys_v], [[sys_set_cdr, [cdaar, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cddaar,
			  wl:arglist_info(sys_pf_set_cddaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cddaar,
			  wl:init_args(exact_only, sys_pf_set_cddaar))).
(defun %set-caadar (x v) (set-car (cadar x) v))
wl:lambda_def(defun, sys_pf_set_caadar, f_sys_pf_set_caadar, [sys_x, sys_v], [[sys_set_car, [cadar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caadar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caadar).

```

### Compiled:  `SYS::%SET-CAADAR` 
```prolog
f_sys_pf_set_caadar(X_Param, V_Param, FnResult) :-
	cl_cadar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caadar, classof, claz_function),
   set_opv(sys_pf_set_caadar, compile_as, kw_function),
   set_opv(sys_pf_set_caadar, function, f_sys_pf_set_caadar),
   _Ignored6=sys_pf_set_caadar.
:- side_effect(assert_lsp(sys_pf_set_caadar,
			  wl:lambda_def(defun, sys_pf_set_caadar, f_sys_pf_set_caadar, [sys_x, sys_v], [[sys_set_car, [cadar, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_caadar,
			  wl:arglist_info(sys_pf_set_caadar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_caadar,
			  wl:init_args(exact_only, sys_pf_set_caadar))).
(defun %set-caddar (x v) (set-car (cddar x) v))
wl:lambda_def(defun, sys_pf_set_caddar, f_sys_pf_set_caddar, [sys_x, sys_v], [[sys_set_car, [cddar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caddar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caddar).

```

### Compiled:  `SYS::%SET-CADDAR` 
```prolog
f_sys_pf_set_caddar(X_Param, V_Param, FnResult) :-
	cl_cddar(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caddar, classof, claz_function),
   set_opv(sys_pf_set_caddar, compile_as, kw_function),
   set_opv(sys_pf_set_caddar, function, f_sys_pf_set_caddar),
   _Ignored6=sys_pf_set_caddar.
:- side_effect(assert_lsp(sys_pf_set_caddar,
			  wl:lambda_def(defun, sys_pf_set_caddar, f_sys_pf_set_caddar, [sys_x, sys_v], [[sys_set_car, [cddar, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_caddar,
			  wl:arglist_info(sys_pf_set_caddar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_caddar,
			  wl:init_args(exact_only, sys_pf_set_caddar))).
(defun %set-cdadar (x v) (set-cdr (cadar x) v))
wl:lambda_def(defun, sys_pf_set_cdadar, f_sys_pf_set_cdadar, [sys_x, sys_v], [[sys_set_cdr, [cadar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdadar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdadar).

```

### Compiled:  `SYS::%SET-CDADAR` 
```prolog
f_sys_pf_set_cdadar(X_Param, V_Param, FnResult) :-
	cl_cadar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdadar, classof, claz_function),
   set_opv(sys_pf_set_cdadar, compile_as, kw_function),
   set_opv(sys_pf_set_cdadar, function, f_sys_pf_set_cdadar),
   _Ignored6=sys_pf_set_cdadar.
:- side_effect(assert_lsp(sys_pf_set_cdadar,
			  wl:lambda_def(defun, sys_pf_set_cdadar, f_sys_pf_set_cdadar, [sys_x, sys_v], [[sys_set_cdr, [cadar, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cdadar,
			  wl:arglist_info(sys_pf_set_cdadar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cdadar,
			  wl:init_args(exact_only, sys_pf_set_cdadar))).
(defun %set-cdddar (x v) (set-cdr (cddar x) v))
wl:lambda_def(defun, sys_pf_set_cdddar, f_sys_pf_set_cdddar, [sys_x, sys_v], [[sys_set_cdr, [cddar, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdddar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdddar).

```

### Compiled:  `SYS::%SET-CDDDAR` 
```prolog
f_sys_pf_set_cdddar(X_Param, V_Param, FnResult) :-
	cl_cddar(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdddar, classof, claz_function),
   set_opv(sys_pf_set_cdddar, compile_as, kw_function),
   set_opv(sys_pf_set_cdddar, function, f_sys_pf_set_cdddar),
   _Ignored6=sys_pf_set_cdddar.
:- side_effect(assert_lsp(sys_pf_set_cdddar,
			  wl:lambda_def(defun, sys_pf_set_cdddar, f_sys_pf_set_cdddar, [sys_x, sys_v], [[sys_set_cdr, [cddar, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cdddar,
			  wl:arglist_info(sys_pf_set_cdddar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cdddar,
			  wl:init_args(exact_only, sys_pf_set_cdddar))).
(defun %set-caaadr (x v) (set-car (caadr x) v))
wl:lambda_def(defun, sys_pf_set_caaadr, f_sys_pf_set_caaadr, [sys_x, sys_v], [[sys_set_car, [caadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caaadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caaadr).

```

### Compiled:  `SYS::%SET-CAAADR` 
```prolog
f_sys_pf_set_caaadr(X_Param, V_Param, FnResult) :-
	cl_caadr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caaadr, classof, claz_function),
   set_opv(sys_pf_set_caaadr, compile_as, kw_function),
   set_opv(sys_pf_set_caaadr, function, f_sys_pf_set_caaadr),
   _Ignored6=sys_pf_set_caaadr.
:- side_effect(assert_lsp(sys_pf_set_caaadr,
			  wl:lambda_def(defun, sys_pf_set_caaadr, f_sys_pf_set_caaadr, [sys_x, sys_v], [[sys_set_car, [caadr, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_caaadr,
			  wl:arglist_info(sys_pf_set_caaadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_caaadr,
			  wl:init_args(exact_only, sys_pf_set_caaadr))).
(defun %set-cadadr (x v) (set-car (cdadr x) v))
wl:lambda_def(defun, sys_pf_set_cadadr, f_sys_pf_set_cadadr, [sys_x, sys_v], [[sys_set_car, [cdadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cadadr).

```

### Compiled:  `SYS::%SET-CADADR` 
```prolog
f_sys_pf_set_cadadr(X_Param, V_Param, FnResult) :-
	cl_cdadr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadadr, classof, claz_function),
   set_opv(sys_pf_set_cadadr, compile_as, kw_function),
   set_opv(sys_pf_set_cadadr, function, f_sys_pf_set_cadadr),
   _Ignored6=sys_pf_set_cadadr.
:- side_effect(assert_lsp(sys_pf_set_cadadr,
			  wl:lambda_def(defun, sys_pf_set_cadadr, f_sys_pf_set_cadadr, [sys_x, sys_v], [[sys_set_car, [cdadr, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cadadr,
			  wl:arglist_info(sys_pf_set_cadadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cadadr,
			  wl:init_args(exact_only, sys_pf_set_cadadr))).
(defun %set-cdaadr (x v) (set-cdr (caadr x) v))
wl:lambda_def(defun, sys_pf_set_cdaadr, f_sys_pf_set_cdaadr, [sys_x, sys_v], [[sys_set_cdr, [caadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdaadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdaadr).

```

### Compiled:  `SYS::%SET-CDAADR` 
```prolog
f_sys_pf_set_cdaadr(X_Param, V_Param, FnResult) :-
	cl_caadr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdaadr, classof, claz_function),
   set_opv(sys_pf_set_cdaadr, compile_as, kw_function),
   set_opv(sys_pf_set_cdaadr, function, f_sys_pf_set_cdaadr),
   _Ignored6=sys_pf_set_cdaadr.
:- side_effect(assert_lsp(sys_pf_set_cdaadr,
			  wl:lambda_def(defun, sys_pf_set_cdaadr, f_sys_pf_set_cdaadr, [sys_x, sys_v], [[sys_set_cdr, [caadr, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cdaadr,
			  wl:arglist_info(sys_pf_set_cdaadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cdaadr,
			  wl:init_args(exact_only, sys_pf_set_cdaadr))).
(defun %set-cddadr (x v) (set-cdr (cdadr x) v))
wl:lambda_def(defun, sys_pf_set_cddadr, f_sys_pf_set_cddadr, [sys_x, sys_v], [[sys_set_cdr, [cdadr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cddadr).

```

### Compiled:  `SYS::%SET-CDDADR` 
```prolog
f_sys_pf_set_cddadr(X_Param, V_Param, FnResult) :-
	cl_cdadr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddadr, classof, claz_function),
   set_opv(sys_pf_set_cddadr, compile_as, kw_function),
   set_opv(sys_pf_set_cddadr, function, f_sys_pf_set_cddadr),
   _Ignored6=sys_pf_set_cddadr.
:- side_effect(assert_lsp(sys_pf_set_cddadr,
			  wl:lambda_def(defun, sys_pf_set_cddadr, f_sys_pf_set_cddadr, [sys_x, sys_v], [[sys_set_cdr, [cdadr, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cddadr,
			  wl:arglist_info(sys_pf_set_cddadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cddadr,
			  wl:init_args(exact_only, sys_pf_set_cddadr))).
(defun %set-caaddr (x v) (set-car (caddr x) v))
wl:lambda_def(defun, sys_pf_set_caaddr, f_sys_pf_set_caaddr, [sys_x, sys_v], [[sys_set_car, [caddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_caaddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_caaddr).

```

### Compiled:  `SYS::%SET-CAADDR` 
```prolog
f_sys_pf_set_caaddr(X_Param, V_Param, FnResult) :-
	cl_caddr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_caaddr, classof, claz_function),
   set_opv(sys_pf_set_caaddr, compile_as, kw_function),
   set_opv(sys_pf_set_caaddr, function, f_sys_pf_set_caaddr),
   _Ignored6=sys_pf_set_caaddr.
:- side_effect(assert_lsp(sys_pf_set_caaddr,
			  wl:lambda_def(defun, sys_pf_set_caaddr, f_sys_pf_set_caaddr, [sys_x, sys_v], [[sys_set_car, [caddr, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_caaddr,
			  wl:arglist_info(sys_pf_set_caaddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_caaddr,
			  wl:init_args(exact_only, sys_pf_set_caaddr))).
(defun %set-cadddr (x v) (set-car (cdddr x) v))
wl:lambda_def(defun, sys_pf_set_cadddr, f_sys_pf_set_cadddr, [sys_x, sys_v], [[sys_set_car, [cdddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cadddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cadddr).

```

### Compiled:  `SYS::%SET-CADDDR` 
```prolog
f_sys_pf_set_cadddr(X_Param, V_Param, FnResult) :-
	cl_cdddr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_cadddr, classof, claz_function),
   set_opv(sys_pf_set_cadddr, compile_as, kw_function),
   set_opv(sys_pf_set_cadddr, function, f_sys_pf_set_cadddr),
   _Ignored6=sys_pf_set_cadddr.
:- side_effect(assert_lsp(sys_pf_set_cadddr,
			  wl:lambda_def(defun, sys_pf_set_cadddr, f_sys_pf_set_cadddr, [sys_x, sys_v], [[sys_set_car, [cdddr, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cadddr,
			  wl:arglist_info(sys_pf_set_cadddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cadddr,
			  wl:init_args(exact_only, sys_pf_set_cadddr))).
(defun %set-cdaddr (x v) (set-cdr (caddr x) v))
wl:lambda_def(defun, sys_pf_set_cdaddr, f_sys_pf_set_cdaddr, [sys_x, sys_v], [[sys_set_cdr, [caddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cdaddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cdaddr).

```

### Compiled:  `SYS::%SET-CDADDR` 
```prolog
f_sys_pf_set_cdaddr(X_Param, V_Param, FnResult) :-
	cl_caddr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cdaddr, classof, claz_function),
   set_opv(sys_pf_set_cdaddr, compile_as, kw_function),
   set_opv(sys_pf_set_cdaddr, function, f_sys_pf_set_cdaddr),
   _Ignored6=sys_pf_set_cdaddr.
:- side_effect(assert_lsp(sys_pf_set_cdaddr,
			  wl:lambda_def(defun, sys_pf_set_cdaddr, f_sys_pf_set_cdaddr, [sys_x, sys_v], [[sys_set_cdr, [caddr, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cdaddr,
			  wl:arglist_info(sys_pf_set_cdaddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cdaddr,
			  wl:init_args(exact_only, sys_pf_set_cdaddr))).
(defun %set-cddddr (x v) (set-cdr (cdddr x) v))

wl:lambda_def(defun, sys_pf_set_cddddr, f_sys_pf_set_cddddr, [sys_x, sys_v], [[sys_set_cdr, [cdddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_cddddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_cddddr).

```

### Compiled:  `SYS::%SET-CDDDDR` 
```prolog
f_sys_pf_set_cddddr(X_Param, V_Param, FnResult) :-
	cl_cdddr(X_Param, Set_cdr_Param),
	f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
	Set_cdr_Ret=FnResult.
:- set_opv(f_sys_pf_set_cddddr, classof, claz_function),
   set_opv(sys_pf_set_cddddr, compile_as, kw_function),
   set_opv(sys_pf_set_cddddr, function, f_sys_pf_set_cddddr),
   _Ignored6=sys_pf_set_cddddr.
:- side_effect(assert_lsp(sys_pf_set_cddddr,
			  wl:lambda_def(defun, sys_pf_set_cddddr, f_sys_pf_set_cddddr, [sys_x, sys_v], [[sys_set_cdr, [cdddr, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_cddddr,
			  wl:arglist_info(sys_pf_set_cddddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_cddddr,
			  wl:init_args(exact_only, sys_pf_set_cddddr))).
(defsetf car set-car)
% macroexpand:-[defsetf,car,sys_set_car].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,car],[quote,sys_setf_inverse],[quote,sys_set_car]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(car,sys_setf_inverse,sys_set_car,_27680),_27680).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(car, sys_setf_inverse, sys_set_car, _Ignored6),
	   _Ignored6).
(defsetf cdr set-cdr)
% macroexpand:-[defsetf,cdr,sys_set_cdr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdr],[quote,sys_setf_inverse],[quote,sys_set_cdr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdr,sys_setf_inverse,sys_set_cdr,_27710),_27710).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdr, sys_setf_inverse, sys_set_cdr, _Ignored6),
	   _Ignored6).
(defsetf caar %set-caar)
% macroexpand:-[defsetf,caar,sys_pf_set_caar].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caar],[quote,sys_setf_inverse],[quote,sys_pf_set_caar]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caar,sys_setf_inverse,sys_pf_set_caar,_27744),_27744).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caar, sys_setf_inverse, sys_pf_set_caar, _Ignored6),
	   _Ignored6).
(defsetf cadr %set-cadr)
% macroexpand:-[defsetf,cadr,sys_pf_set_cadr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cadr],[quote,sys_setf_inverse],[quote,sys_pf_set_cadr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cadr,sys_setf_inverse,sys_pf_set_cadr,_27778),_27778).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cadr, sys_setf_inverse, sys_pf_set_cadr, _Ignored6),
	   _Ignored6).
(defsetf cdar %set-cdar)
% macroexpand:-[defsetf,cdar,sys_pf_set_cdar].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdar],[quote,sys_setf_inverse],[quote,sys_pf_set_cdar]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdar,sys_setf_inverse,sys_pf_set_cdar,_27812),_27812).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdar, sys_setf_inverse, sys_pf_set_cdar, _Ignored6),
	   _Ignored6).
(defsetf cddr %set-cddr)
% macroexpand:-[defsetf,cddr,sys_pf_set_cddr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cddr],[quote,sys_setf_inverse],[quote,sys_pf_set_cddr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cddr,sys_setf_inverse,sys_pf_set_cddr,_27846),_27846).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cddr, sys_setf_inverse, sys_pf_set_cddr, _Ignored6),
	   _Ignored6).
(defsetf caaar %set-caaar)
% macroexpand:-[defsetf,caaar,sys_pf_set_caaar].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caaar],[quote,sys_setf_inverse],[quote,sys_pf_set_caaar]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caaar,sys_setf_inverse,sys_pf_set_caaar,_27880),_27880).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caaar, sys_setf_inverse, sys_pf_set_caaar, _Ignored6),
	   _Ignored6).
(defsetf cadar %set-cadar)
% macroexpand:-[defsetf,cadar,sys_pf_set_cadar].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cadar],[quote,sys_setf_inverse],[quote,sys_pf_set_cadar]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cadar,sys_setf_inverse,sys_pf_set_cadar,_27914),_27914).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cadar, sys_setf_inverse, sys_pf_set_cadar, _Ignored6),
	   _Ignored6).
(defsetf cdaar %set-cdaar)
% macroexpand:-[defsetf,cdaar,sys_pf_set_cdaar].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdaar],[quote,sys_setf_inverse],[quote,sys_pf_set_cdaar]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdaar,sys_setf_inverse,sys_pf_set_cdaar,_27948),_27948).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdaar, sys_setf_inverse, sys_pf_set_cdaar, _Ignored6),
	   _Ignored6).
(defsetf cddar %set-cddar)
% macroexpand:-[defsetf,cddar,sys_pf_set_cddar].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cddar],[quote,sys_setf_inverse],[quote,sys_pf_set_cddar]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cddar,sys_setf_inverse,sys_pf_set_cddar,_27982),_27982).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cddar, sys_setf_inverse, sys_pf_set_cddar, _Ignored6),
	   _Ignored6).
(defsetf caadr %set-caadr)
% macroexpand:-[defsetf,caadr,sys_pf_set_caadr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caadr],[quote,sys_setf_inverse],[quote,sys_pf_set_caadr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caadr,sys_setf_inverse,sys_pf_set_caadr,_28016),_28016).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caadr, sys_setf_inverse, sys_pf_set_caadr, _Ignored6),
	   _Ignored6).
(defsetf caddr %set-caddr)
% macroexpand:-[defsetf,caddr,sys_pf_set_caddr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caddr],[quote,sys_setf_inverse],[quote,sys_pf_set_caddr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caddr,sys_setf_inverse,sys_pf_set_caddr,_28050),_28050).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caddr, sys_setf_inverse, sys_pf_set_caddr, _Ignored6),
	   _Ignored6).
(defsetf cdadr %set-cdadr)
% macroexpand:-[defsetf,cdadr,sys_pf_set_cdadr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdadr],[quote,sys_setf_inverse],[quote,sys_pf_set_cdadr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdadr,sys_setf_inverse,sys_pf_set_cdadr,_28084),_28084).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdadr, sys_setf_inverse, sys_pf_set_cdadr, _Ignored6),
	   _Ignored6).
(defsetf cdddr %set-cdddr)
% macroexpand:-[defsetf,cdddr,sys_pf_set_cdddr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdddr],[quote,sys_setf_inverse],[quote,sys_pf_set_cdddr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdddr,sys_setf_inverse,sys_pf_set_cdddr,_28118),_28118).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdddr, sys_setf_inverse, sys_pf_set_cdddr, _Ignored6),
	   _Ignored6).
(defsetf caaaar %set-caaaar)
% macroexpand:-[defsetf,caaaar,sys_pf_set_caaaar].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caaaar],[quote,sys_setf_inverse],[quote,sys_pf_set_caaaar]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caaaar,sys_setf_inverse,sys_pf_set_caaaar,_28152),_28152).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caaaar, sys_setf_inverse, sys_pf_set_caaaar, _Ignored6),
	   _Ignored6).
(defsetf cadaar %set-cadaar)
% macroexpand:-[defsetf,cadaar,sys_pf_set_cadaar].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cadaar],[quote,sys_setf_inverse],[quote,sys_pf_set_cadaar]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cadaar,sys_setf_inverse,sys_pf_set_cadaar,_28186),_28186).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cadaar, sys_setf_inverse, sys_pf_set_cadaar, _Ignored6),
	   _Ignored6).
(defsetf cdaaar %set-cdaaar)
% macroexpand:-[defsetf,cdaaar,sys_pf_set_cdaaar].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdaaar],[quote,sys_setf_inverse],[quote,sys_pf_set_cdaaar]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdaaar,sys_setf_inverse,sys_pf_set_cdaaar,_28220),_28220).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdaaar, sys_setf_inverse, sys_pf_set_cdaaar, _Ignored6),
	   _Ignored6).
(defsetf cddaar %set-cddaar)
% macroexpand:-[defsetf,cddaar,sys_pf_set_cddaar].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cddaar],[quote,sys_setf_inverse],[quote,sys_pf_set_cddaar]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cddaar,sys_setf_inverse,sys_pf_set_cddaar,_28254),_28254).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cddaar, sys_setf_inverse, sys_pf_set_cddaar, _Ignored6),
	   _Ignored6).
(defsetf caadar %set-caadar)
% macroexpand:-[defsetf,caadar,sys_pf_set_caadar].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caadar],[quote,sys_setf_inverse],[quote,sys_pf_set_caadar]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caadar,sys_setf_inverse,sys_pf_set_caadar,_28288),_28288).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caadar, sys_setf_inverse, sys_pf_set_caadar, _Ignored6),
	   _Ignored6).
(defsetf caddar %set-caddar)
% macroexpand:-[defsetf,caddar,sys_pf_set_caddar].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caddar],[quote,sys_setf_inverse],[quote,sys_pf_set_caddar]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caddar,sys_setf_inverse,sys_pf_set_caddar,_28322),_28322).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caddar, sys_setf_inverse, sys_pf_set_caddar, _Ignored6),
	   _Ignored6).
(defsetf cdadar %set-cdadar)
% macroexpand:-[defsetf,cdadar,sys_pf_set_cdadar].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdadar],[quote,sys_setf_inverse],[quote,sys_pf_set_cdadar]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdadar,sys_setf_inverse,sys_pf_set_cdadar,_28356),_28356).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdadar, sys_setf_inverse, sys_pf_set_cdadar, _Ignored6),
	   _Ignored6).
(defsetf cdddar %set-cdddar)
% macroexpand:-[defsetf,cdddar,sys_pf_set_cdddar].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdddar],[quote,sys_setf_inverse],[quote,sys_pf_set_cdddar]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdddar,sys_setf_inverse,sys_pf_set_cdddar,_28390),_28390).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdddar, sys_setf_inverse, sys_pf_set_cdddar, _Ignored6),
	   _Ignored6).
(defsetf caaadr %set-caaadr)
% macroexpand:-[defsetf,caaadr,sys_pf_set_caaadr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caaadr],[quote,sys_setf_inverse],[quote,sys_pf_set_caaadr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caaadr,sys_setf_inverse,sys_pf_set_caaadr,_28424),_28424).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caaadr, sys_setf_inverse, sys_pf_set_caaadr, _Ignored6),
	   _Ignored6).
(defsetf cadadr %set-cadadr)
% macroexpand:-[defsetf,cadadr,sys_pf_set_cadadr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cadadr],[quote,sys_setf_inverse],[quote,sys_pf_set_cadadr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cadadr,sys_setf_inverse,sys_pf_set_cadadr,_28458),_28458).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cadadr, sys_setf_inverse, sys_pf_set_cadadr, _Ignored6),
	   _Ignored6).
(defsetf cdaadr %set-cdaadr)
% macroexpand:-[defsetf,cdaadr,sys_pf_set_cdaadr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdaadr],[quote,sys_setf_inverse],[quote,sys_pf_set_cdaadr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdaadr,sys_setf_inverse,sys_pf_set_cdaadr,_28492),_28492).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdaadr, sys_setf_inverse, sys_pf_set_cdaadr, _Ignored6),
	   _Ignored6).
(defsetf cddadr %set-cddadr)
% macroexpand:-[defsetf,cddadr,sys_pf_set_cddadr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cddadr],[quote,sys_setf_inverse],[quote,sys_pf_set_cddadr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cddadr,sys_setf_inverse,sys_pf_set_cddadr,_28526),_28526).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cddadr, sys_setf_inverse, sys_pf_set_cddadr, _Ignored6),
	   _Ignored6).
(defsetf caaddr %set-caaddr)
% macroexpand:-[defsetf,caaddr,sys_pf_set_caaddr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,caaddr],[quote,sys_setf_inverse],[quote,sys_pf_set_caaddr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(caaddr,sys_setf_inverse,sys_pf_set_caaddr,_28560),_28560).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(caaddr, sys_setf_inverse, sys_pf_set_caaddr, _Ignored6),
	   _Ignored6).
(defsetf cadddr %set-cadddr)
% macroexpand:-[defsetf,cadddr,sys_pf_set_cadddr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cadddr],[quote,sys_setf_inverse],[quote,sys_pf_set_cadddr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cadddr,sys_setf_inverse,sys_pf_set_cadddr,_28594),_28594).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cadddr, sys_setf_inverse, sys_pf_set_cadddr, _Ignored6),
	   _Ignored6).
(defsetf cdaddr %set-cdaddr)
% macroexpand:-[defsetf,cdaddr,sys_pf_set_cdaddr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cdaddr],[quote,sys_setf_inverse],[quote,sys_pf_set_cdaddr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cdaddr,sys_setf_inverse,sys_pf_set_cdaddr,_28628),_28628).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cdaddr, sys_setf_inverse, sys_pf_set_cdaddr, _Ignored6),
	   _Ignored6).
(defsetf cddddr %set-cddddr)

% macroexpand:-[defsetf,cddddr,sys_pf_set_cddddr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,cddddr],[quote,sys_setf_inverse],[quote,sys_pf_set_cddddr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(cddddr,sys_setf_inverse,sys_pf_set_cddddr,_28664),_28664).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(cddddr, sys_setf_inverse, sys_pf_set_cddddr, _Ignored6),
	   _Ignored6).
(defsetf first set-car)
% macroexpand:-[defsetf,first,sys_set_car].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,first],[quote,sys_setf_inverse],[quote,sys_set_car]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(first,sys_setf_inverse,sys_set_car,_28696),_28696).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(first, sys_setf_inverse, sys_set_car, _Ignored6),
	   _Ignored6).
(defsetf second %set-cadr)
% macroexpand:-[defsetf,second,sys_pf_set_cadr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,second],[quote,sys_setf_inverse],[quote,sys_pf_set_cadr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(second,sys_setf_inverse,sys_pf_set_cadr,_28730),_28730).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(second, sys_setf_inverse, sys_pf_set_cadr, _Ignored6),
	   _Ignored6).
(defsetf third %set-caddr)
% macroexpand:-[defsetf,third,sys_pf_set_caddr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,third],[quote,sys_setf_inverse],[quote,sys_pf_set_caddr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(third,sys_setf_inverse,sys_pf_set_caddr,_28764),_28764).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(third, sys_setf_inverse, sys_pf_set_caddr, _Ignored6),
	   _Ignored6).
(defsetf fourth %set-cadddr)
% macroexpand:-[defsetf,fourth,sys_pf_set_cadddr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,fourth],[quote,sys_setf_inverse],[quote,sys_pf_set_cadddr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(fourth,sys_setf_inverse,sys_pf_set_cadddr,_28798),_28798).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(fourth, sys_setf_inverse, sys_pf_set_cadddr, _Ignored6),
	   _Ignored6).
(defun %set-fifth (x v) (set-car (cddddr x) v))
wl:lambda_def(defun, sys_pf_set_fifth, f_sys_pf_set_fifth, [sys_x, sys_v], [[sys_set_car, [cddddr, sys_x], sys_v]]).
wl:arglist_info(sys_pf_set_fifth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_fifth).

```

### Compiled:  `SYS::%SET-FIFTH` 
```prolog
f_sys_pf_set_fifth(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_fifth, classof, claz_function),
   set_opv(sys_pf_set_fifth, compile_as, kw_function),
   set_opv(sys_pf_set_fifth, function, f_sys_pf_set_fifth),
   _Ignored6=sys_pf_set_fifth.
:- side_effect(assert_lsp(sys_pf_set_fifth,
			  wl:lambda_def(defun, sys_pf_set_fifth, f_sys_pf_set_fifth, [sys_x, sys_v], [[sys_set_car, [cddddr, sys_x], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_fifth,
			  wl:arglist_info(sys_pf_set_fifth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_fifth,
			  wl:init_args(exact_only, sys_pf_set_fifth))).
(defsetf fifth %set-fifth)
% macroexpand:-[defsetf,fifth,sys_pf_set_fifth].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,fifth],[quote,sys_setf_inverse],[quote,sys_pf_set_fifth]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(fifth,sys_setf_inverse,sys_pf_set_fifth,_92048),_92048).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(fifth, sys_setf_inverse, sys_pf_set_fifth, _Ignored6),
	   _Ignored6).
(defun %set-sixth (x v) (set-car (cdr (cddddr x)) v))
wl:lambda_def(defun, sys_pf_set_sixth, f_sys_pf_set_sixth, [sys_x, sys_v], [[sys_set_car, [cdr, [cddddr, sys_x]], sys_v]]).
wl:arglist_info(sys_pf_set_sixth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_sixth).

```

### Compiled:  `SYS::%SET-SIXTH` 
```prolog
f_sys_pf_set_sixth(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Cdr_Param),
	cl_cdr(Cdr_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_sixth, classof, claz_function),
   set_opv(sys_pf_set_sixth, compile_as, kw_function),
   set_opv(sys_pf_set_sixth, function, f_sys_pf_set_sixth),
   _Ignored6=sys_pf_set_sixth.
:- side_effect(assert_lsp(sys_pf_set_sixth,
			  wl:lambda_def(defun, sys_pf_set_sixth, f_sys_pf_set_sixth, [sys_x, sys_v], [[sys_set_car, [cdr, [cddddr, sys_x]], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_sixth,
			  wl:arglist_info(sys_pf_set_sixth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_sixth,
			  wl:init_args(exact_only, sys_pf_set_sixth))).
(defsetf sixth %set-sixth)
% macroexpand:-[defsetf,sixth,sys_pf_set_sixth].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,sixth],[quote,sys_setf_inverse],[quote,sys_pf_set_sixth]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(sixth,sys_setf_inverse,sys_pf_set_sixth,_92288),_92288).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(sixth, sys_setf_inverse, sys_pf_set_sixth, _Ignored6),
	   _Ignored6).
(defun %set-seventh (x v) (set-car (cddr (cddddr x)) v))
wl:lambda_def(defun, sys_pf_set_seventh, f_sys_pf_set_seventh, [sys_x, sys_v], [[sys_set_car, [cddr, [cddddr, sys_x]], sys_v]]).
wl:arglist_info(sys_pf_set_seventh, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_seventh).

```

### Compiled:  `SYS::%SET-SEVENTH` 
```prolog
f_sys_pf_set_seventh(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Cddr_Param),
	cl_cddr(Cddr_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_seventh, classof, claz_function),
   set_opv(sys_pf_set_seventh, compile_as, kw_function),
   set_opv(sys_pf_set_seventh, function, f_sys_pf_set_seventh),
   _Ignored6=sys_pf_set_seventh.
:- side_effect(assert_lsp(sys_pf_set_seventh,
			  wl:lambda_def(defun, sys_pf_set_seventh, f_sys_pf_set_seventh, [sys_x, sys_v], [[sys_set_car, [cddr, [cddddr, sys_x]], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_seventh,
			  wl:arglist_info(sys_pf_set_seventh, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_seventh,
			  wl:init_args(exact_only, sys_pf_set_seventh))).
(defsetf seventh %set-seventh)
% macroexpand:-[defsetf,seventh,sys_pf_set_seventh].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,seventh],[quote,sys_setf_inverse],[quote,sys_pf_set_seventh]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(seventh,sys_setf_inverse,sys_pf_set_seventh,_92056),_92056).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(seventh, sys_setf_inverse, sys_pf_set_seventh, _Ignored6),
	   _Ignored6).
(defun %set-eighth (x v) (set-car (cdddr (cddddr x)) v))
wl:lambda_def(defun, sys_pf_set_eighth, f_sys_pf_set_eighth, [sys_x, sys_v], [[sys_set_car, [cdddr, [cddddr, sys_x]], sys_v]]).
wl:arglist_info(sys_pf_set_eighth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_eighth).

```

### Compiled:  `SYS::%SET-EIGHTH` 
```prolog
f_sys_pf_set_eighth(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Cdddr_Param),
	cl_cdddr(Cdddr_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_eighth, classof, claz_function),
   set_opv(sys_pf_set_eighth, compile_as, kw_function),
   set_opv(sys_pf_set_eighth, function, f_sys_pf_set_eighth),
   _Ignored6=sys_pf_set_eighth.
:- side_effect(assert_lsp(sys_pf_set_eighth,
			  wl:lambda_def(defun, sys_pf_set_eighth, f_sys_pf_set_eighth, [sys_x, sys_v], [[sys_set_car, [cdddr, [cddddr, sys_x]], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_eighth,
			  wl:arglist_info(sys_pf_set_eighth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_eighth,
			  wl:init_args(exact_only, sys_pf_set_eighth))).
(defsetf eighth %set-eighth)
% macroexpand:-[defsetf,eighth,sys_pf_set_eighth].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,eighth],[quote,sys_setf_inverse],[quote,sys_pf_set_eighth]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(eighth,sys_setf_inverse,sys_pf_set_eighth,_91500),_91500).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(eighth, sys_setf_inverse, sys_pf_set_eighth, _Ignored6),
	   _Ignored6).
(defun %set-ninth (x v) (set-car (cddddr (cddddr x)) v))
wl:lambda_def(defun, sys_pf_set_ninth, f_sys_pf_set_ninth, [sys_x, sys_v], [[sys_set_car, [cddddr, [cddddr, sys_x]], sys_v]]).
wl:arglist_info(sys_pf_set_ninth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_ninth).

```

### Compiled:  `SYS::%SET-NINTH` 
```prolog
f_sys_pf_set_ninth(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Cddddr_Param),
	cl_cddddr(Cddddr_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_ninth, classof, claz_function),
   set_opv(sys_pf_set_ninth, compile_as, kw_function),
   set_opv(sys_pf_set_ninth, function, f_sys_pf_set_ninth),
   _Ignored6=sys_pf_set_ninth.
:- side_effect(assert_lsp(sys_pf_set_ninth,
			  wl:lambda_def(defun, sys_pf_set_ninth, f_sys_pf_set_ninth, [sys_x, sys_v], [[sys_set_car, [cddddr, [cddddr, sys_x]], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_ninth,
			  wl:arglist_info(sys_pf_set_ninth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_ninth,
			  wl:init_args(exact_only, sys_pf_set_ninth))).
(defsetf ninth %set-ninth)
% macroexpand:-[defsetf,ninth,sys_pf_set_ninth].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,ninth],[quote,sys_setf_inverse],[quote,sys_pf_set_ninth]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(ninth,sys_setf_inverse,sys_pf_set_ninth,_90962),_90962).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(ninth, sys_setf_inverse, sys_pf_set_ninth, _Ignored6),
	   _Ignored6).
(defun %set-tenth (x v) (set-car (cdr (cddddr (cddddr x))) v))
wl:lambda_def(defun, sys_pf_set_tenth, f_sys_pf_set_tenth, [sys_x, sys_v], [[sys_set_car, [cdr, [cddddr, [cddddr, sys_x]]], sys_v]]).
wl:arglist_info(sys_pf_set_tenth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, sys_pf_set_tenth).

```

### Compiled:  `SYS::%SET-TENTH` 
```prolog
f_sys_pf_set_tenth(X_Param, V_Param, FnResult) :-
	cl_cddddr(X_Param, Cddddr_Param),
	cl_cddddr(Cddddr_Param, Cdr_Param),
	cl_cdr(Cdr_Param, Set_car_Param),
	f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
	Set_car_Ret=FnResult.
:- set_opv(f_sys_pf_set_tenth, classof, claz_function),
   set_opv(sys_pf_set_tenth, compile_as, kw_function),
   set_opv(sys_pf_set_tenth, function, f_sys_pf_set_tenth),
   _Ignored6=sys_pf_set_tenth.
:- side_effect(assert_lsp(sys_pf_set_tenth,
			  wl:lambda_def(defun, sys_pf_set_tenth, f_sys_pf_set_tenth, [sys_x, sys_v], [[sys_set_car, [cdr, [cddddr, [cddddr, sys_x]]], sys_v]]))).
:- side_effect(assert_lsp(sys_pf_set_tenth,
			  wl:arglist_info(sys_pf_set_tenth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(sys_pf_set_tenth,
			  wl:init_args(exact_only, sys_pf_set_tenth))).
(defsetf tenth %set-tenth)

% macroexpand:-[defsetf,tenth,sys_pf_set_tenth].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,tenth],[quote,sys_setf_inverse],[quote,sys_pf_set_tenth]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(tenth,sys_setf_inverse,sys_pf_set_tenth,_91392),_91392).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(tenth, sys_setf_inverse, sys_pf_set_tenth, _Ignored6),
	   _Ignored6).
(defsetf rest set-cdr)
;;Redefined in extensible-sequences-base.lisp
% macroexpand:-[defsetf,rest,sys_set_cdr].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,rest],[quote,sys_setf_inverse],[quote,sys_set_cdr]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(rest,sys_setf_inverse,sys_set_cdr,_29380),_29380).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(rest, sys_setf_inverse, sys_set_cdr, _Ignored6),
	   _Ignored6).
;Redefined in extensible-sequences-base.lisp
(defsetf elt %set-elt)
% macroexpand:-[defsetf,elt,sys_pf_set_elt].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,elt],[quote,sys_setf_inverse],[quote,sys_pf_set_elt]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(elt,sys_setf_inverse,sys_pf_set_elt,_29382),_29382).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(elt, sys_setf_inverse, sys_pf_set_elt, _Ignored6),
	   _Ignored6).
(defsetf nth %set-nth)
% macroexpand:-[defsetf,nth,sys_pf_set_nth].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,nth],[quote,sys_setf_inverse],[quote,sys_pf_set_nth]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(nth,sys_setf_inverse,sys_pf_set_nth,_29416),_29416).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(nth, sys_setf_inverse, sys_pf_set_nth, _Ignored6),
	   _Ignored6).
(defsetf svref svset)
% macroexpand:-[defsetf,svref,sys_svset].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,svref],[quote,sys_setf_inverse],[quote,sys_svset]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(svref,sys_setf_inverse,sys_svset,_29446),_29446).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(svref, sys_setf_inverse, sys_svset, _Ignored6),
	   _Ignored6).
(defsetf fill-pointer %set-fill-pointer)
% macroexpand:-[defsetf,fill_pointer,sys_pf_set_fill_pointer].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,fill_pointer],[quote,sys_setf_inverse],[quote,sys_pf_set_fill_pointer]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(fill_pointer,sys_setf_inverse,sys_pf_set_fill_pointer,_29522),_29522).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(fill_pointer,
		     sys_setf_inverse,
		     sys_pf_set_fill_pointer,
		     _Ignored6),
	   _Ignored6).
(defsetf subseq %set-subseq)
% macroexpand:-[defsetf,subseq,sys_pf_set_subseq].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,subseq],[quote,sys_setf_inverse],[quote,sys_pf_set_subseq]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(subseq,sys_setf_inverse,sys_pf_set_subseq,_29518),_29518).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(subseq, sys_setf_inverse, sys_pf_set_subseq, _Ignored6),
	   _Ignored6).
(defsetf symbol-value set)
% macroexpand:-[defsetf,symbol_value,set].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,symbol_value],[quote,sys_setf_inverse],[quote,set]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(symbol_value,sys_setf_inverse,set,_29552),_29552).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(symbol_value, sys_setf_inverse, set, _Ignored6),
	   _Ignored6).
(defsetf symbol-function %set-symbol-function)
% macroexpand:-[defsetf,symbol_function,sys_pf_set_symbol_function].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,symbol_function],[quote,sys_setf_inverse],[quote,sys_pf_set_symbol_function]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(symbol_function,sys_setf_inverse,sys_pf_set_symbol_function,_29630),_29630).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(symbol_function,
		     sys_setf_inverse,
		     sys_pf_set_symbol_function,
		     _Ignored6),
	   _Ignored6).
(defsetf symbol-plist %set-symbol-plist)
% macroexpand:-[defsetf,symbol_plist,sys_pf_set_symbol_plist].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,symbol_plist],[quote,sys_setf_inverse],[quote,sys_pf_set_symbol_plist]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(symbol_plist,sys_setf_inverse,sys_pf_set_symbol_plist,_29658),_29658).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(symbol_plist,
		     sys_setf_inverse,
		     sys_pf_set_symbol_plist,
		     _Ignored6),
	   _Ignored6).
(defsetf get put)
% macroexpand:-[defsetf,get,sys_put].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,get],[quote,sys_setf_inverse],[quote,sys_put]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(get,sys_setf_inverse,sys_put,_29648),_29648).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(get, sys_setf_inverse, sys_put, _Ignored6),
	   _Ignored6).
(defsetf gethash puthash)
% macroexpand:-[defsetf,gethash,sys_puthash].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,gethash],[quote,sys_setf_inverse],[quote,sys_puthash]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(gethash,sys_setf_inverse,sys_puthash,_29688),_29688).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(gethash, sys_setf_inverse, sys_puthash, _Ignored6),
	   _Ignored6).
(defsetf char set-char)
% macroexpand:-[defsetf,char,sys_set_char].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,char],[quote,sys_setf_inverse],[quote,sys_set_char]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(char,sys_setf_inverse,sys_set_char,_29722),_29722).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(char, sys_setf_inverse, sys_set_char, _Ignored6),
	   _Ignored6).
(defsetf schar set-schar)
% macroexpand:-[defsetf,schar,sys_set_schar].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,schar],[quote,sys_setf_inverse],[quote,sys_set_schar]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(schar,sys_setf_inverse,sys_set_schar,_29756),_29756).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(schar, sys_setf_inverse, sys_set_schar, _Ignored6),
	   _Ignored6).
(defsetf logical-pathname-translations %set-logical-pathname-translations)
% macroexpand:-[defsetf,logical_pathname_translations,sys_pf_set_logical_pathname_translations].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,logical_pathname_translations],[quote,sys_setf_inverse],[quote,sys_pf_set_logical_pathname_translations]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(logical_pathname_translations,sys_setf_inverse,sys_pf_set_logical_pathname_translations,_29852),_29852).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(logical_pathname_translations,
		     sys_setf_inverse,
		     sys_pf_set_logical_pathname_translations,
		     _Ignored6),
	   _Ignored6).
(defsetf readtable-case %set-readtable-case)

% macroexpand:-[defsetf,readtable_case,sys_pf_set_readtable_case].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,readtable_case],[quote,sys_setf_inverse],[quote,sys_pf_set_readtable_case]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(readtable_case,sys_setf_inverse,sys_pf_set_readtable_case,_29864),_29864).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(readtable_case,
		     sys_setf_inverse,
		     sys_pf_set_readtable_case,
		     _Ignored6),
	   _Ignored6).
(defsetf function-info %set-function-info)

% macroexpand:-[defsetf,sys_function_info,sys_pf_set_function_info].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,sys_function_info],[quote,sys_setf_inverse],[quote,sys_pf_set_function_info]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(sys_function_info,sys_setf_inverse,sys_pf_set_function_info,_29896),_29896).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(sys_function_info,
		     sys_setf_inverse,
		     sys_pf_set_function_info,
		     _Ignored6),
	   _Ignored6).
(defsetf stream-external-format %set-stream-external-format)

% macroexpand:-[defsetf,stream_external_format,sys_pf_set_stream_external_format].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,stream_external_format],[quote,sys_setf_inverse],[quote,sys_pf_set_stream_external_format]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(stream_external_format,sys_setf_inverse,sys_pf_set_stream_external_format,_29944),_29944).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(stream_external_format,
		     sys_setf_inverse,
		     sys_pf_set_stream_external_format,
		     _Ignored6),
	   _Ignored6).
(defsetf structure-ref structure-set)

% macroexpand:-[defsetf,sys_structure_ref,sys_structure_set].
% into:-[eval_when,[kw_load_toplevel,kw_compile_toplevel,kw_execute],[sys_put,[quote,sys_structure_ref],[quote,sys_setf_inverse],[quote,sys_structure_set]]].
% code:-do_when([kw_load_toplevel,kw_compile_toplevel,kw_execute],f_sys_put(sys_structure_ref,sys_setf_inverse,sys_structure_set,_29960),_29960).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
	   f_sys_put(sys_structure_ref,
		     sys_setf_inverse,
		     sys_structure_set,
		     _Ignored6),
	   _Ignored6).
'(load "wam-cl-init2")
'(load "wam-cl-init3")
'(write-line " WAM CommonLisp ")
'(read-eval-print-loop)

 

#|

;; (when (eq (symbol-package sym) p) (format t "fmt90_x1 fmt90_x2 fmt90_x3 "'(read-eval-print-loop)\r\n\r\n \r\n\r\n#|\r\n\r\n;; (when (eq (symbol-package sym) p) (format t \"~a ~a ~a ~a~%\" ......)) \r\n\r\n".
; (when (eq (symbol-package sym) p) (format t "fmt90_x1 fmt90_x2 fmt90_x3 "; (when (eq (symbol-package sym) p) (format t \"~a ~a ~a ~a~%\" ......)) ".


;; (when (eq (symbol-package sym) p) (format t "fmt90_x1 fmt90_x2 fmt90_x3 "\r\n\r\n;; (when (eq (symbol-package sym) p) (format t \"~a ~a ~a ~a~%\" ......)) \r\n\r\n(defun packagesyminfo (p0)\r\n (let ((p (find-package p0)))\r\n (do-all-symbols (sym)    \r\n  (when (eq (symbol-package sym) p)\r\n   (format t \"symbolinfo('~s','~s').~%\"\r\n    sn (package-name (symbol-package sym))\r\n    (constantp sym)\r\n    (special-operator-p sym)\r\n    (symbol-plist sym)\r\n    sn (symbol-package sym)\r\n    (if (boundp sym) (symbol-value sym))\r\n    (if (fboundp sym) (type-of (symbol-function sym)))\r\n    (fboundp sym)))))))\r\n(packagesyminfo :cl)\r\n\r\n\r\n\r\n\r\n\r\n(defun packagesyminfo (p0)\r\n (let ((p (find-package p0)))\r\n (do-all-symbols (sym)    \r\n  (when (eq (symbol-package sym) p)\r\n   (format t \"symbol_package('~a','~a').~%\"\r\n    sn (package-name (symbol-package sym)))))))\r\n(packagesyminfo :cl)\r\n\r\n\r\n".
(in-package "CL-USER")



;; Test macro
:- cl_in_package('$ARRAY'([*], claz_base_character, "CL-USER"), _Ignored).
; Test macro
(defmacro is (eqf expected actual)
  (let ((a (gensym "a")) (b (gensym "b")))
    `(let ((,a ,expected) (,b ,actual))
       (if (,eqf ,a ,b)
         (format t "OK: fmt90_x1 is fmt90_x2 to fmt90_x3"(defmacro is (eqf expected actual)\n  (let ((a (gensym \"a\")) (b (gensym \"b\")))\n    `(let ((,a ,expected) (,b ,actual))\n       (if (,eqf ,a ,b)\n         (format t \"OK: ~a is ~a to ~a~%\" ',expected ',eqf ',actual)\n         (progn\n           (format t \"FAILED: when matching ~a and ~a~%\" ,a ,b)\n\t   #+WAM-CL (prolog-inline \"trace\")\n\t   #+CLISP (BREAK)\n\t   #+CLISP (quit 1))\n         ))))\n\n\n".
wl:lambda_def(defmacro, u_is, f_u_is, [u_eqf, u_expected, u_actual], [progn, [let, [[u_a, [gensym, '$ARRAY'([*], claz_base_character, "a")]], [u_b, [gensym, '$ARRAY'([*], claz_base_character, "b")]]], ['#BQ', [let, [[['#COMMA', u_a], ['#COMMA', u_expected]], [['#COMMA', u_b], ['#COMMA', u_actual]]], [if, [['#COMMA', u_eqf], ['#COMMA', u_a], ['#COMMA', u_b]], [format, t, '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"), [quote, ['#COMMA', u_expected]], [quote, ['#COMMA', u_eqf]], [quote, ['#COMMA', u_actual]]], [progn, [format, t, '$ARRAY'([*], claz_base_character, "FAILED: when matching ~a and ~a~%"), ['#COMMA', u_a], ['#COMMA', u_b]], [sys_prolog_inline, '$ARRAY'([*], claz_base_character, "trace")]]]]]]]).
wl:arglist_info(u_is, [u_eqf, u_expected, u_actual], [Eqf_Param, Expected_Param, Actual_Param], arginfo{all:[u_eqf, u_expected, u_actual], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_eqf, u_expected, u_actual], opt:0, req:[u_eqf, u_expected, u_actual], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_is).

```

### Compiled:  `U::IS` 
```prolog
f_u_is(Eqf_Param, Expected_Param, Actual_Param, FnResult) :-
	TLEnv=[bv(u_eqf, Eqf_Param), bv(u_expected, Expected_Param), bv(u_actual, Actual_Param)],
	cl_gensym('$ARRAY'([*], claz_base_character, "a"), A_Init),
	cl_gensym('$ARRAY'([*], claz_base_character, "b"), B_Init),
	LEnv=[[bv(u_a, A_Init), bv(u_b, B_Init)]|TLEnv],
	get_var(LEnv, u_a, A_Get26),
	get_var(LEnv, u_b, B_Get27),
	[let, [[A_Get26, Expected_Param], [B_Get27, Actual_Param]], [if, [Eqf_Param, A_Get26, B_Get27], [format, t, '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"), [quote, Expected_Param], [quote, Eqf_Param], [quote, Actual_Param]], [progn, [format, t, '$ARRAY'([*], claz_base_character, "FAILED: when matching ~a and ~a~%"), A_Get26, B_Get27], [sys_prolog_inline, '$ARRAY'([*], claz_base_character, "trace")]]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_is, classof, claz_macro),
   set_opv(u_is, compile_as, kw_operator),
   set_opv(u_is, function, f_u_is),
   DefMacroResult=u_is.
:- side_effect(assert_lsp(u_is,
			  wl:lambda_def(defmacro, u_is, f_u_is, [u_eqf, u_expected, u_actual], [progn, [let, [[u_a, [gensym, '$ARRAY'([*], claz_base_character, "a")]], [u_b, [gensym, '$ARRAY'([*], claz_base_character, "b")]]], ['#BQ', [let, [[['#COMMA', u_a], ['#COMMA', u_expected]], [['#COMMA', u_b], ['#COMMA', u_actual]]], [if, [['#COMMA', u_eqf], ['#COMMA', u_a], ['#COMMA', u_b]], [format, t, '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"), [quote, ['#COMMA', u_expected]], [quote, ['#COMMA', u_eqf]], [quote, ['#COMMA', u_actual]]], [progn, [format, t, '$ARRAY'([*], claz_base_character, "FAILED: when matching ~a and ~a~%"), ['#COMMA', u_a], ['#COMMA', u_b]], [sys_prolog_inline, '$ARRAY'([*], claz_base_character, "trace")]]]]]]]))).
:- side_effect(assert_lsp(u_is,
			  wl:arglist_info(u_is, [u_eqf, u_expected, u_actual], [Eqf_Param, Expected_Param, Actual_Param], arginfo{all:[u_eqf, u_expected, u_actual], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_eqf, u_expected, u_actual], opt:0, req:[u_eqf, u_expected, u_actual], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(u_is, wl:init_args(exact_only, u_is))).
(write-line "Running smoke test!")

; (progn (prolog-inline "rtrace") (is eq 1 1))
:- cl_write_line('$ARRAY'([*], claz_base_character, "Running smoke test!"),
		 _Ignored).
 (progn (prolog-inline "rtrace") (is eq 1 1))
(is eq 1 1)
% macroexpand:-[u_is,eq,1,1].
% into:-[let,[[a11,1],[b11,1]],[if,[eq,a11,b11],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,1],[quote,eq],[quote,1]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a11,b11],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-_14720=[[bv(a11,1),bv(b11,1)]|_13340],get_var(_14720,a11,_19474),get_var(_14720,b11,_19940),(is_eq(_19474,_19940)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),1,eq,1],_20206),_13608=_20206;get_var(_14720,a11,_22844),get_var(_14720,b11,_24476),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_22844,_24476],_21324),trace,_13608=_21324).
:- LEnv=[[bv(a11, 1), bv(b11, 1)]|TLEnv],
   get_var(LEnv, a11, A11_Get),
   get_var(LEnv, b11, B11_Get),
   (   is_eq(A11_Get, B11_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   1,
		   eq,
		   1
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a11, A11_Get12),
       get_var(LEnv, b11, B11_Get13),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A11_Get12,
		   B11_Get13
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(is equal (list 1 'a 'b) (cons 1 '(a b)))

% macroexpand:-[u_is,equal,[list,1,[quote,u_a],[quote,u_b]],[cons,1,[quote,[u_a,u_b]]]].
% into:-[let,[[a21,[list,1,[quote,u_a],[quote,u_b]]],[b21,[cons,1,[quote,[u_a,u_b]]]]],[if,[equal,a21,b21],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[list,1,[quote,u_a],[quote,u_b]]],[quote,equal],[quote,[cons,1,[quote,[u_a,u_b]]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a21,b21],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_list([1,u_a,u_b],_15874),_15912=[1,u_a,u_b],_15536=[[bv(a21,_15874),bv(b21,_15912)]|_13706],get_var(_15536,a21,_24620),get_var(_15536,b21,_25086),(is_equal(_24620,_25086)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[list,1,[quote,u_a],[quote,u_b]],equal,[cons,1,[quote,[u_a,u_b]]]],_25622),_14244=_25622;get_var(_15536,a21,_28530),get_var(_15536,b21,_30162),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_28530,_30162],_27010),trace,_14244=_27010).
:- A21_Init=[1, u_a, u_b],
   B21_Init=[1, u_a, u_b],
   LEnv=[[bv(a21, A21_Init), bv(b21, B21_Init)]|TLEnv],
   get_var(LEnv, a21, A21_Get),
   get_var(LEnv, b21, B21_Get),
   (   is_equal(A21_Get, B21_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [list, 1, [quote, u_a], [quote, u_b]],
		   equal,
		   [cons, 1, [quote, [u_a, u_b]]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a21, A21_Get14),
       get_var(LEnv, b21, B21_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A21_Get14,
		   B21_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(is eq 2 (if nil 1 2))

% macroexpand:-[u_is,eq,2,[if,[],1,2]].
% into:-[let,[[a31,2],[b31,[if,[],1,2]]],[if,[eq,a31,b31],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,2],[quote,eq],[quote,[if,[],1,2]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a31,b31],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-([]\==[]->_15442=1;_15442=2),_15092=[[bv(a31,2),bv(b31,_15442)]|_13592],get_var(_15092,a31,_22648),get_var(_15092,b31,_23114),(is_eq(_22648,_23114)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),2,eq,[if,[],1,2]],_23452),_13932=_23452;get_var(_15092,a31,_26162),get_var(_15092,b31,_27794),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26162,_27794],_24642),trace,_13932=_24642).
:- (   []\==[]
   ->  B31_Init=1
   ;   B31_Init=2
   ),
   LEnv=[[bv(a31, 2), bv(b31, B31_Init)]|TLEnv],
   get_var(LEnv, a31, A31_Get),
   get_var(LEnv, b31, B31_Get),
   (   is_eq(A31_Get, B31_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   2,
		   eq,
		   [if, [], 1, 2]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a31, A31_Get15),
       get_var(LEnv, b31, B31_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A31_Get15,
		   B31_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(is eq t (keywordp :k))

% macroexpand:-[u_is,eq,t,[keywordp,kw_k]].
% into:-[let,[[a41,t],[b41,[keywordp,kw_k]]],[if,[eq,a41,b41],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,t],[quote,eq],[quote,[keywordp,kw_k]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a41,b41],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_keywordp(kw_k,_15434),_15084=[[bv(a41,t),bv(b41,_15434)]|_13644],get_var(_15084,a41,_22496),get_var(_15084,b41,_22962),(is_eq(_22496,_22962)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),t,eq,[keywordp,kw_k]],_23264),_13948=_23264;get_var(_15084,a41,_25938),get_var(_15084,b41,_27570),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_25938,_27570],_24418),trace,_13948=_24418).
:- cl_keywordp(kw_k, B41_Init),
   LEnv=[[bv(a41, t), bv(b41, B41_Init)]|TLEnv],
   get_var(LEnv, a41, A41_Get),
   get_var(LEnv, b41, B41_Get),
   (   is_eq(A41_Get, B41_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   t,
		   eq,
		   [keywordp, kw_k]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a41, A41_Get13),
       get_var(LEnv, b41, B41_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A41_Get13,
		   B41_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(is eq 10 (if t 10 20))

% macroexpand:-[u_is,eq,10,[if,t,10,20]].
% into:-[let,[[a51,10],[b51,[if,t,10,20]]],[if,[eq,a51,b51],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,10],[quote,eq],[quote,[if,t,10,20]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a51,b51],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-(t\==[]->_15618=10;_15618=20),_15268=[[bv(a51,10),bv(b51,_15618)]|_13768],get_var(_15268,a51,_22824),get_var(_15268,b51,_23290),(is_eq(_22824,_23290)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),10,eq,[if,t,10,20]],_23628),_14108=_23628;get_var(_15268,a51,_26338),get_var(_15268,b51,_27970),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26338,_27970],_24818),trace,_14108=_24818).
:- (   t\==[]
   ->  B51_Init=10
   ;   B51_Init=20
   ),
   LEnv=[[bv(a51, 10), bv(b51, B51_Init)]|TLEnv],
   get_var(LEnv, a51, A51_Get),
   get_var(LEnv, b51, B51_Get),
   (   is_eq(A51_Get, B51_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   10,
		   eq,
		   [if, t, 10, 20]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a51, A51_Get15),
       get_var(LEnv, b51, B51_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A51_Get15,
		   B51_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(is eq t (stringp "abc"))

;;  "FAI "this has ben fix" LED: when matching fmt90_x1 and fmt90_x2"(is eq t (stringp \"abc\"))\n\n;;  \"FAI \"this has ben fix\" LED: when matching ~a and ~a~%\", ['$CHAR'(b), '$CHAR'(c)], \"bc\", t).\n".
% macroexpand:-[u_is,eq,t,[stringp,'$ARRAY'([*],claz_base_character,"abc")]].
% into:-[let,[[a61,t],[b61,[stringp,'$ARRAY'([*],claz_base_character,"abc")]]],[if,[eq,a61,b61],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,t],[quote,eq],[quote,[stringp,'$ARRAY'([*],claz_base_character,"abc")]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a61,b61],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_stringp('$ARRAY'([*],claz_base_character,"abc"),_15738),_15388=[[bv(a61,t),bv(b61,_15738)]|_13904],get_var(_15388,a61,_22800),get_var(_15388,b61,_23266),(is_eq(_22800,_23266)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),t,eq,[stringp,'$ARRAY'([*],claz_base_character,"abc")]],_23568),_14252=_23568;get_var(_15388,a61,_26242),get_var(_15388,b61,_27874),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26242,_27874],_24722),trace,_14252=_24722).
:- cl_stringp('$ARRAY'([*], claz_base_character, "abc"), B61_Init),
   LEnv=[[bv(a61, t), bv(b61, B61_Init)]|TLEnv],
   get_var(LEnv, a61, A61_Get),
   get_var(LEnv, b61, B61_Get),
   (   is_eq(A61_Get, B61_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   t,
		   eq,
		   [stringp, '$ARRAY'([*], claz_base_character, "abc")]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a61, A61_Get13),
       get_var(LEnv, b61, B61_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A61_Get13,
		   B61_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
;  "FAI "this has ben fix" LED: when matching fmt90_x1 and fmt90_x2";  \"FAI \"this has ben fix\" LED: when matching ~a and ~a~%\", ['$CHAR'(b), '$CHAR'(c)], \"bc\", t).".
(is equal (subseq "abc" 1) "bc")

% macroexpand:-[u_is,equal,[subseq,'$ARRAY'([*],claz_base_character,"abc"),1],'$ARRAY'([*],claz_base_character,"bc")].
% into:-[let,[[a71,[subseq,'$ARRAY'([*],claz_base_character,"abc"),1]],[b71,'$ARRAY'([*],claz_base_character,"bc")]],[if,[equal,a71,b71],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[subseq,'$ARRAY'([*],claz_base_character,"abc"),1]],[quote,equal],[quote,'$ARRAY'([*],claz_base_character,"bc")]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a71,b71],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_subseq('$ARRAY'([*],claz_base_character,"abc"),1,_15920),_15582=[[bv(a71,_15920),bv(b71,'$ARRAY'([*],claz_base_character,"bc"))]|_14034],get_var(_15582,a71,_23292),get_var(_15582,b71,_23758),(is_equal(_23292,_23758)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[subseq,'$ARRAY'([*],claz_base_character,"abc"),1],equal,'$ARRAY'([*],claz_base_character,"bc")],_24078),_14434=_24078;get_var(_15582,a71,_26770),get_var(_15582,b71,_28402),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26770,_28402],_25250),trace,_14434=_25250).
:- cl_subseq('$ARRAY'([*], claz_base_character, "abc"), 1, A71_Init),
   LEnv=[[bv(a71, A71_Init), bv(b71, '$ARRAY'([*], claz_base_character, "bc"))]|TLEnv],
   get_var(LEnv, a71, A71_Get),
   get_var(LEnv, b71, B71_Get),
   (   is_equal(A71_Get, B71_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [subseq, '$ARRAY'([*], claz_base_character, "abc"), 1],
		   equal,
		   '$ARRAY'([*], claz_base_character, "bc")
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a71, A71_Get13),
       get_var(LEnv, b71, B71_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A71_Get13,
		   B71_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(is eq 1 (if t 1 2))
% macroexpand:-[u_is,eq,1,[if,t,1,2]].
% into:-[let,[[a81,1],[b81,[if,t,1,2]]],[if,[eq,a81,b81],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,1],[quote,eq],[quote,[if,t,1,2]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a81,b81],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-(t\==[]->_15880=1;_15880=2),_15530=[[bv(a81,1),bv(b81,_15880)]|_14030],get_var(_15530,a81,_23086),get_var(_15530,b81,_23552),(is_eq(_23086,_23552)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),1,eq,[if,t,1,2]],_23890),_14370=_23890;get_var(_15530,a81,_26600),get_var(_15530,b81,_28232),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26600,_28232],_25080),trace,_14370=_25080).
:- (   t\==[]
   ->  B81_Init=1
   ;   B81_Init=2
   ),
   LEnv=[[bv(a81, 1), bv(b81, B81_Init)]|TLEnv],
   get_var(LEnv, a81, A81_Get),
   get_var(LEnv, b81, B81_Get),
   (   is_eq(A81_Get, B81_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   1,
		   eq,
		   [if, t, 1, 2]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a81, A81_Get15),
       get_var(LEnv, b81, B81_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A81_Get15,
		   B81_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(is eq 2 (if nil 1 2))

% macroexpand:-[u_is,eq,2,[if,[],1,2]].
% into:-[let,[[a91,2],[b91,[if,[],1,2]]],[if,[eq,a91,b91],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,2],[quote,eq],[quote,[if,[],1,2]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a91,b91],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-([]\==[]->_15970=1;_15970=2),_15620=[[bv(a91,2),bv(b91,_15970)]|_14120],get_var(_15620,a91,_23176),get_var(_15620,b91,_23642),(is_eq(_23176,_23642)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),2,eq,[if,[],1,2]],_23980),_14460=_23980;get_var(_15620,a91,_26690),get_var(_15620,b91,_28322),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26690,_28322],_25170),trace,_14460=_25170).
:- (   []\==[]
   ->  B91_Init=1
   ;   B91_Init=2
   ),
   LEnv=[[bv(a91, 2), bv(b91, B91_Init)]|TLEnv],
   get_var(LEnv, a91, A91_Get),
   get_var(LEnv, b91, B91_Get),
   (   is_eq(A91_Get, B91_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   2,
		   eq,
		   [if, [], 1, 2]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a91, A91_Get15),
       get_var(LEnv, b91, B91_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A91_Get15,
		   B91_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(defun fib (n)
  (if (> n 1)
    (+ (fib (- n 1))
       (fib (- n 2)))
    1))

wl:lambda_def(defun, u_fib, f_u_fib, [n], [[if, [>, n, 1], [+, [u_fib, [-, n, 1]], [u_fib, [-, n, 2]]], 1]]).
wl:arglist_info(u_fib, [n], [N_Param], arginfo{all:[n], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[n], opt:0, req:[n], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_fib).

```

### Compiled:  `U::FIB` 
```prolog
f_u_fib(N_Param, FnResult) :-
	(   N_Param>1
	->  -(N_Param, 1, Fib_Param),
	    f_u_fib(Fib_Param, Fib_Ret),
	    -(N_Param, 2, Fib_Param22),
	    f_u_fib(Fib_Param22, Fib_Ret24),
	    +(Fib_Ret, Fib_Ret24, TrueResult),
	    FnResult=TrueResult
	;   FnResult=1
	).
:- set_opv(f_u_fib, classof, claz_function),
   set_opv(u_fib, compile_as, kw_function),
   set_opv(u_fib, function, f_u_fib),
   DefunResult=u_fib.
:- side_effect(assert_lsp(u_fib,
			  wl:lambda_def(defun, u_fib, f_u_fib, [n], [[if, [>, n, 1], [+, [u_fib, [-, n, 1]], [u_fib, [-, n, 2]]], 1]]))).
:- side_effect(assert_lsp(u_fib,
			  wl:arglist_info(u_fib, [n], [N_Param], arginfo{all:[n], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[n], opt:0, req:[n], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(u_fib, wl:init_args(exact_only, u_fib))).
(disassemble #'fib)


:- cl_disassemble(function(u_fib), _Ignored).
(is eql 89 (fib 10))



% macroexpand:-[u_is,eql,89,[u_fib,10]].
% into:-[let,[[a101,89],[b101,[u_fib,10]]],[if,[eql,a101,b101],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,89],[quote,eql],[quote,[u_fib,10]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a101,b101],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-f_u_fib(10,_16044),_15694=[[bv(a101,89),bv(b101,_16044)]|_14254],get_var(_15694,a101,_23232),get_var(_15694,b101,_23698),(is_eql(_23232,_23698)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),89,eql,[u_fib,10]],_24000),_14558=_24000;get_var(_15694,a101,_26716),get_var(_15694,b101,_28390),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26716,_28390],_25154),trace,_14558=_25154).
:- f_u_fib(10, B101_Init),
   LEnv=[[bv(a101, 89), bv(b101, B101_Init)]|TLEnv],
   get_var(LEnv, a101, A101_Get),
   get_var(LEnv, b101, B101_Get),
   (   is_eql(A101_Get, B101_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   89,
		   eql,
		   [u_fib, 10]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a101, A101_Get13),
       get_var(LEnv, b101, B101_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A101_Get13,
		   B101_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(defun accum (r) (if (= 0 r) (list 0) (cons r (accum (- r 1)))))

wl:lambda_def(defun, u_accum, f_u_accum, [u_r], [[if, [=, 0, u_r], [list, 0], [cons, u_r, [u_accum, [-, u_r, 1]]]]]).
wl:arglist_info(u_accum, [u_r], [R_Param], arginfo{all:[u_r], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_r], opt:0, req:[u_r], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_accum).

```

### Compiled:  `U::ACCUM` 
```prolog
f_u_accum(R_Param, FnResult) :-
	(   0=:=R_Param
	->  FnResult=[0]
	;   -(R_Param, 1, Accum_Param),
	    f_u_accum(Accum_Param, Accum_Ret),
	    FnResult=[R_Param|Accum_Ret]
	).
:- set_opv(f_u_accum, classof, claz_function),
   set_opv(u_accum, compile_as, kw_function),
   set_opv(u_accum, function, f_u_accum),
   DefunResult=u_accum.
:- side_effect(assert_lsp(u_accum,
			  wl:lambda_def(defun, u_accum, f_u_accum, [u_r], [[if, [=, 0, u_r], [list, 0], [cons, u_r, [u_accum, [-, u_r, 1]]]]]))).
:- side_effect(assert_lsp(u_accum,
			  wl:arglist_info(u_accum, [u_r], [R_Param], arginfo{all:[u_r], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_r], opt:0, req:[u_r], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(u_accum, wl:init_args(exact_only, u_accum))).
(disassemble #'accum)
#| DISASSEMBLY FOR:f_u_accum
:- dynamic f_u_accum/2.

f_u_accum(A, G) :-
	(   0=:=A
	->  G=[0]
	;   C is A - 1,
	    f_u_accum(C, D),
	    G=[A|D]
	).

|#
:- cl_disassemble(function(u_accum), _Ignored).
 DISASSEMBLY FOR:f_u_accum
:- dynamic f_u_accum/2.

f_u_accum(A, G) :-
	(   0=:=A
	->  G=[0]
	;   C is A - 1,
	    f_u_accum(C, D),
	    G=[A|D]
	).

(is equal (list 4 3 2 1 0) (accum 4))

% macroexpand:-[u_is,equal,[list,4,3,2,1,0],[u_accum,4]].
% into:-[let,[[a12,[list,4,3,2,1,0]],[b12,[u_accum,4]]],[if,[equal,a12,b12],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[list,4,3,2,1,0]],[quote,equal],[quote,[u_accum,4]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a12,b12],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_list([4,3,2,1,0],_16526),f_u_accum(4,_16576),_16188=[[bv(a12,_16526),bv(b12,_16576)]|_14580],get_var(_16188,a12,_25014),get_var(_16188,b12,_25480),(is_equal(_25014,_25480)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[list,4,3,2,1,0],equal,[u_accum,4]],_25890),_14992=_25890;get_var(_16188,a12,_28672),get_var(_16188,b12,_30304),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_28672,_30304],_27152),trace,_14992=_27152).
:- A12_Init=[4, 3, 2, 1, 0],
   f_u_accum(4, B12_Init),
   LEnv=[[bv(a12, A12_Init), bv(b12, B12_Init)]|TLEnv],
   get_var(LEnv, a12, A12_Get),
   get_var(LEnv, b12, B12_Get),
   (   is_equal(A12_Get, B12_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [list, 4, 3, 2, 1, 0],
		   equal,
		   [u_accum, 4]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a12, A12_Get14),
       get_var(LEnv, b12, B12_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A12_Get14,
		   B12_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(defmacro defwrap (name) `(defun ,name () 1))
;;; :- ensure_loaded('sanity-test.lisp.trans.pl').
wl:lambda_def(defmacro, u_defwrap, f_u_defwrap, [sys_name], [progn, ['#BQ', [defun, ['#COMMA', sys_name], [], 1]]]).
wl:arglist_info(u_defwrap, [sys_name], [Name_Param], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:[sys_name], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_defwrap).

```

### Compiled:  `U::DEFWRAP` 
```prolog
f_u_defwrap(Name_Param, FnResult) :-
	[defun, Name_Param, [], 1]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(f_u_defwrap, classof, claz_macro),
   set_opv(u_defwrap, compile_as, kw_operator),
   set_opv(u_defwrap, function, f_u_defwrap),
   DefMacroResult=u_defwrap.
:- side_effect(assert_lsp(u_defwrap,
			  wl:lambda_def(defmacro, u_defwrap, f_u_defwrap, [sys_name], [progn, ['#BQ', [defun, ['#COMMA', sys_name], [], 1]]]))).
:- side_effect(assert_lsp(u_defwrap,
			  wl:arglist_info(u_defwrap, [sys_name], [Name_Param], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:[sys_name], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(u_defwrap, wl:init_args(exact_only, u_defwrap))).
;; :- ensure_loaded('sanity-test.lisp.trans.pl').
(defwrap foo)
% macroexpand:-[u_defwrap,u_foo].
% into:-[defun,u_foo,[],1].
% code:-assert_lsp(u_foo,wl:lambda_def(defun,u_foo,f_u_foo,[],[1])),assert_lsp(u_foo,wl:arglist_info(u_foo,[],[],arginfo{all:0,allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[],opt:0,req:0,rest:0,sublists:0,whole:0})),!,assert_lsp(u_foo,wl:init_args(exact_only,u_foo)),assert_lsp(u_foo,(f_u_foo(_50856):-_46002=[],1=_50856)),set_opv(f_u_foo,classof,claz_function),set_opv(u_foo,compile_as,kw_function),set_opv(u_foo,function,f_u_foo),_17750=u_foo.
wl:lambda_def(defun, u_foo, f_u_foo, [], [1]).
wl:arglist_info(u_foo, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_foo).

```

### Compiled:  `U::FOO` 
```prolog
f_u_foo(FnResult) :-
	Env=[],
	1=FnResult.
:- set_opv(f_u_foo, classof, claz_function),
   set_opv(u_foo, compile_as, kw_function),
   set_opv(u_foo, function, f_u_foo),
   DefunResult=u_foo.
:- side_effect(assert_lsp(u_foo, wl:lambda_def(defun, u_foo, f_u_foo, [], [1]))).
:- side_effect(assert_lsp(u_foo,
			  wl:arglist_info(u_foo, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(u_foo, wl:init_args(exact_only, u_foo))).
(is eq 1 (foo))
% macroexpand:-[u_is,eq,1,[u_foo]].
% into:-[let,[[a13,1],[b13,[u_foo]]],[if,[eq,a13,b13],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,1],[quote,eq],[quote,[u_foo]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a13,b13],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-f_u_foo(_16438),_16088=[[bv(a13,1),bv(b13,_16438)]|_14690],get_var(_16088,a13,_23462),get_var(_16088,b13,_23928),(is_eq(_23462,_23928)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),1,eq,[u_foo]],_24212),_14976=_24212;get_var(_16088,a13,_26868),get_var(_16088,b13,_28500),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26868,_28500],_25348),trace,_14976=_25348).
:- f_u_foo(B13_Init),
   LEnv=[[bv(a13, 1), bv(b13, B13_Init)]|TLEnv],
   get_var(LEnv, a13, A13_Get),
   get_var(LEnv, b13, B13_Get),
   (   is_eq(A13_Get, B13_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   1,
		   eq,
		   [u_foo]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a13, A13_Get13),
       get_var(LEnv, b13, B13_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A13_Get13,
		   B13_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(is equal (macroexpand-1 '(defwrap foo)) '(defun foo nil 1))

% macroexpand:-[u_is,equal,[macroexpand_1,[quote,[u_defwrap,u_foo]]],[quote,[defun,u_foo,[],1]]].
% into:-[let,[[a14,[macroexpand_1,[quote,[u_defwrap,u_foo]]]],[b14,[quote,[defun,u_foo,[],1]]]],[if,[equal,a14,b14],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[macroexpand_1,[quote,[u_defwrap,u_foo]]]],[quote,equal],[quote,[quote,[defun,u_foo,[],1]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a14,b14],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_macroexpand_1([[u_defwrap,u_foo]],_17040),_16702=[[bv(a14,_17040),bv(b14,[defun,u_foo,[],1])]|_14974],get_var(_16702,a14,_24776),get_var(_16702,b14,_25242),(is_equal(_24776,_25242)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[macroexpand_1,[quote,[u_defwrap,u_foo]]],equal,[quote,[defun,u_foo,[],1]]],_25724),_15458=_25724;get_var(_16702,a14,_28578),get_var(_16702,b14,_30210),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_28578,_30210],_27058),trace,_15458=_27058).
:- cl_macroexpand_1([[u_defwrap, u_foo]], A14_Init),
   LEnv=[[bv(a14, A14_Init), bv(b14, [defun, u_foo, [], 1])]|TLEnv],
   get_var(LEnv, a14, A14_Get),
   get_var(LEnv, b14, B14_Get),
   (   is_equal(A14_Get, B14_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [macroexpand_1, [quote, [u_defwrap, u_foo]]],
		   equal,
		   [quote, [defun, u_foo, [], 1]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a14, A14_Get13),
       get_var(LEnv, b14, B14_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A14_Get13,
		   B14_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(write-line "PASSED")

:- cl_write_line('$ARRAY'([*], claz_base_character, "PASSED"), _Ignored).
(defun fifteen ()
  (let (val)
    (tagbody
      (setq val 1)
      (go point-a)
      (incf val 16)
     point-c
      (incf val 04)
      (go point-b)
      (incf val 32)
     point-a
     point-u ;; unused
      (incf val 02)
      (go point-c)
      (incf val 64)
     point-b
      (incf val 08))
    val))

% macroexpand:-[incf,u_val,4].
% into:-[setf,u_val,[+,u_val,4]].
% code:-get_var(_23320,u_val,_20284),+(_20284,4,_20136),set_place(_23320,setf,[value,u_val],[_20136],_19972).
% macroexpand:-[incf,u_val,2].
% into:-[setf,u_val,[+,u_val,2]].
% code:-get_var(_22044,u_val,_21430),+(_21430,2,_37040),set_place(_22044,setf,[value,u_val],[_37040],_23044).
% macroexpand:-[incf,u_val,2].
% into:-[setf,u_val,[+,u_val,2]].
% code:-get_var(_22198,u_val,_21578),+(_21578,2,_37194),set_place(_22198,setf,[value,u_val],[_37194],_23198).
% macroexpand:-[incf,u_val,8].
% into:-[setf,u_val,[+,u_val,8]].
% code:-get_var(_22458,u_val,_21832),+(_21832,8,_37454),set_place(_22458,setf,[value,u_val],[_37454],_23458).
wl:lambda_def(defun, u_fifteen, f_u_fifteen, [], [[let, [u_val], [tagbody, [setq, u_val, 1], [go, u_point_a], [incf, u_val, 16], u_point_c, [incf, u_val, 4], [go, u_point_b], [incf, u_val, 32], u_point_a, u_point_u, [incf, u_val, 2], [go, u_point_c], [incf, u_val, 64], u_point_b, [incf, u_val, 8]], u_val]]).
wl:arglist_info(u_fifteen, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_fifteen).

```

### Compiled:  `U::FIFTEEN` 
```prolog
f_u_fifteen(FnResult) :-
	Env=[],
	catch(( ( GoEnv=[[bv(u_val, [])]|Env],
		  call_addr_block(GoEnv,
				  (set_var(GoEnv, setq, u_val, 1), goto(u_point_a, GoEnv)),
				  
				  [ addr(addr_tagbody_2_u_point_c,
					 u_point_c,
					 '$used',
					 Setf_Env,
					 (get_var(Setf_Env, u_val, Get_var_Ret), +(Get_var_Ret, 4, CAR49), set_place(Setf_Env, setf, [value, u_val], [CAR49], Set_place_Ret), goto(u_point_b, Setf_Env))),
				    addr(addr_tagbody_2_u_point_a,
					 u_point_a,
					 '$used',
					 Setf_Env,
					 (push_label(u_point_u), get_var(Setf_Env, u_val, Val_Get21), +(Val_Get21, 2, CAR25), set_place(Setf_Env, setf, [value, u_val], [CAR25], Setf_R23), goto(u_point_c, Setf_Env))),
				    addr(addr_tagbody_2_u_point_u,
					 u_point_u,
					 '$unused',
					 Setf_Env,
					 (get_var(Setf_Env, u_val, Val_Get28), +(Val_Get28, 2, CAR32), set_place(Setf_Env, setf, [value, u_val], [CAR32], Setf_R30), goto(u_point_c, Setf_Env))),
				    addr(addr_tagbody_2_u_point_b,
					 u_point_b,
					 '$used',
					 _GEnv36,
					 (get_var(_GEnv36, u_val, Val_Get35), +(Val_Get35, 8, CAR39), set_place(_GEnv36, setf, [value, u_val], [CAR39], _GORES26)))
				  ]),
		  get_var(GoEnv, u_val, Val_Get43)
		),
		Val_Get43=FnResult
	      ),
	      block_exit(u_fifteen, FnResult),
	      true).
:- set_opv(f_u_fifteen, classof, claz_function),
   set_opv(u_fifteen, compile_as, kw_function),
   set_opv(u_fifteen, function, f_u_fifteen),
   DefunResult=u_fifteen.
:- side_effect(assert_lsp(u_fifteen,
			  wl:lambda_def(defun, u_fifteen, f_u_fifteen, [], [[let, [u_val], [tagbody, [setq, u_val, 1], [go, u_point_a], [incf, u_val, 16], u_point_c, [incf, u_val, 4], [go, u_point_b], [incf, u_val, 32], u_point_a, u_point_u, [incf, u_val, 2], [go, u_point_c], [incf, u_val, 64], u_point_b, [incf, u_val, 8]], u_val]]))).
:- side_effect(assert_lsp(u_fifteen,
			  wl:arglist_info(u_fifteen, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(u_fifteen, wl:init_args(exact_only, u_fifteen))).
; unused
(disassemble #'fifteen)

#|

/* this first one should get deleted since its inlined away in f_u_fifteen */

addr_tagbody_1_addr_enter_1(Env10) :-
        symbol_setter(Env10, setq, u_val, 1),
        addr_tagbody_1_u_point_a(Env10).
addr_tagbody_1_u_point_c(Incf_Env) :-
        place_op(Incf_Env, incf, [value, u_val], [4], Incf_R),
        addr_tagbody_1_u_point_b(Incf_Env).
addr_tagbody_1_u_point_a(Incf_Env19) :-
        place_op(Incf_Env19, incf, [value, u_val], [2], Incf_R18),
        addr_tagbody_1_u_point_c(Incf_Env19).
addr_tagbody_1_u_point_u(Incf_Env23) :-
        place_op(Incf_Env23, incf, [value, u_val], [2], Incf_R22),
        addr_tagbody_1_u_point_c(Incf_Env23).
addr_tagbody_1_u_point_b(Incf_Env27) :-
        place_op(Incf_Env27, incf, [value, u_val], [8], _GORES15).

f_u_fifteen(MResult) :-
        Env=[],
        catch(( TBEnv=[[bv(u_val, [])]|Env],
                symbol_setter(TBEnv, setq, u_val, 1),
                addr_tagbody_1_u_point_a(TBEnv),
                symbol_value(TBEnv, u_val, U_val_Get),
                U_val_Get=MResult
              ),
              block_exit(u_fifteen, MResult),
              true).

|#

:- cl_disassemble(function(u_fifteen), _Ignored).


/* this first one should get deleted since its inlined away in f_u_fifteen */

addr_tagbody_1_addr_enter_1(Env10) :-
        symbol_setter(Env10, setq, u_val, 1),
        addr_tagbody_1_u_point_a(Env10).
addr_tagbody_1_u_point_c(Incf_Env) :-
        place_op(Incf_Env, incf, [value, u_val], [4], Incf_R),
        addr_tagbody_1_u_point_b(Incf_Env).
addr_tagbody_1_u_point_a(Incf_Env19) :-
        place_op(Incf_Env19, incf, [value, u_val], [2], Incf_R18),
        addr_tagbody_1_u_point_c(Incf_Env19).
addr_tagbody_1_u_point_u(Incf_Env23) :-
        place_op(Incf_Env23, incf, [value, u_val], [2], Incf_R22),
        addr_tagbody_1_u_point_c(Incf_Env23).
addr_tagbody_1_u_point_b(Incf_Env27) :-
        place_op(Incf_Env27, incf, [value, u_val], [8], _GORES15).

f_u_fifteen(MResult) :-
        Env=[],
        catch(( TBEnv=[[bv(u_val, [])]|Env],
                symbol_setter(TBEnv, setq, u_val, 1),
                addr_tagbody_1_u_point_a(TBEnv),
                symbol_value(TBEnv, u_val, U_val_Get),
                U_val_Get=MResult
              ),
              block_exit(u_fifteen, MResult),
              true).

(is eq 15 (fifteen))

% macroexpand:-[u_is,eq,15,[u_fifteen]].
% into:-[let,[[a15,15],[b15,[u_fifteen]]],[if,[eq,a15,b15],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,15],[quote,eq],[quote,[u_fifteen]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a15,b15],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-f_u_fifteen(_16902),_16552=[[bv(a15,15),bv(b15,_16902)]|_15154],get_var(_16552,a15,_23926),get_var(_16552,b15,_24392),(is_eq(_23926,_24392)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),15,eq,[u_fifteen]],_24676),_15440=_24676;get_var(_16552,a15,_27332),get_var(_16552,b15,_28964),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_27332,_28964],_25812),trace,_15440=_25812).
:- f_u_fifteen(B15_Init),
   LEnv=[[bv(a15, 15), bv(b15, B15_Init)]|TLEnv],
   get_var(LEnv, a15, A15_Get),
   get_var(LEnv, b15, B15_Get),
   (   is_eq(A15_Get, B15_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   15,
		   eq,
		   [u_fifteen]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a15, A15_Get13),
       get_var(LEnv, b15, B15_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A15_Get13,
		   B15_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(defun do-four () (DO ((temp-one 1 (1+ temp-one) )(temp-two 0 (1- temp-two) ) )((> (- temp-one temp-two) 5) temp-one)() ))

wl:lambda_def(defun, u_do_four, f_u_do_four, [], [[do, [[u_temp_one, 1, ['1+', u_temp_one]], [u_temp_two, 0, ['1-', u_temp_two]]], [[>, [-, u_temp_one, u_temp_two], 5], u_temp_one], []]]).
wl:arglist_info(u_do_four, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_do_four).

```

### Compiled:  `U::DO-FOUR` 
```prolog
f_u_do_four(FnResult) :-
	Env=[],
	catch(( ( GoEnv=[[bv(u_temp_one, 1), bv(u_temp_two, 0)]|Env],
		  catch(( call_addr_block(GoEnv,
					  (push_label(do_label_2), get_var(GoEnv, u_temp_one, Temp_one_Get31), get_var(GoEnv, u_temp_two, Temp_two_Get32), -(Temp_one_Get31, Temp_two_Get32, PredArg1Result34), (PredArg1Result34>5->throw(block_exit([], Temp_one_Get21)), _TBResult=ThrowResult36;get_var(GoEnv, u_temp_one, Temp_one_Get39), '1+'(Temp_one_Get39, Temp_one), get_var(GoEnv, u_temp_two, Temp_two_Get40), '1-'(Temp_two_Get40, Temp_two), set_var(GoEnv, u_temp_one, Temp_one), set_var(GoEnv, u_temp_two, Temp_two), goto(do_label_2, GoEnv), _TBResult=_GORES41)),
					  
					  [ addr(addr_tagbody_3_do_label_2,
						 do_label_2,
						 '$unused',
						 BlockExitEnv,
						 (get_var(BlockExitEnv, u_temp_one, Temp_one_Get21), get_var(BlockExitEnv, u_temp_two, Temp_two_Get24), -(Temp_one_Get21, Temp_two_Get24, _16714), (_16714>5->throw(block_exit([], Temp_one_Get21)), _16716=ThrowResult;'1+'(Temp_one_Get21, Set_var_Ret), '1-'(Temp_two_Get24, Set_var_Ret51), set_var(BlockExitEnv, u_temp_one, Set_var_Ret), set_var(BlockExitEnv, u_temp_two, Set_var_Ret51), goto(do_label_2, BlockExitEnv), _16716=_GORES)))
					  ]),
			  []=LetResult
			),
			block_exit([], LetResult),
			true)
		),
		LetResult=FnResult
	      ),
	      block_exit(u_do_four, FnResult),
	      true).
:- set_opv(f_u_do_four, classof, claz_function),
   set_opv(u_do_four, compile_as, kw_function),
   set_opv(u_do_four, function, f_u_do_four),
   DefunResult=u_do_four.
:- side_effect(assert_lsp(u_do_four,
			  wl:lambda_def(defun, u_do_four, f_u_do_four, [], [[do, [[u_temp_one, 1, ['1+', u_temp_one]], [u_temp_two, 0, ['1-', u_temp_two]]], [[>, [-, u_temp_one, u_temp_two], 5], u_temp_one], []]]))).
:- side_effect(assert_lsp(u_do_four,
			  wl:arglist_info(u_do_four, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(u_do_four, wl:init_args(exact_only, u_do_four))).
(is = 4  (do-four))

% macroexpand:-[u_is,=,4,[u_do_four]].
% into:-[let,[[a16,4],[b16,[u_do_four]]],[if,[=,a16,b16],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,4],[quote,=],[quote,[u_do_four]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a16,b16],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-f_u_do_four(_17154),_16804=[[bv(a16,4),bv(b16,_17154)]|_15406],get_var(_16804,a16,_24012),get_var(_16804,b16,_24478),(_24012=:=_24478->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),4,=,[u_do_four]],_24762),_15692=_24762;get_var(_16804,a16,_27418),get_var(_16804,b16,_29050),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_27418,_29050],_25898),trace,_15692=_25898).
:- f_u_do_four(B16_Init),
   LEnv=[[bv(a16, 4), bv(b16, B16_Init)]|TLEnv],
   get_var(LEnv, a16, A16_Get),
   get_var(LEnv, b16, B16_Get),
   (   A16_Get=:=B16_Get
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   4,
		   (=),
		   [u_do_four]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a16, A16_Get13),
       get_var(LEnv, b16, B16_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A16_Get13,
		   B16_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(is eq 'string_l (DEFUN string_l (x )(COND ((STRINGP x )x )((SYMBOLP x )(symbol-name x ))(T (ERROR "type error" )))))

% macroexpand:-[u_is,eq,[quote,u_string_l],[defun,u_string_l,[u_x],[cond,[[stringp,u_x],u_x],[[symbolp,u_x],[symbol_name,u_x]],[t,[error,'$ARRAY'([*],claz_base_character,"type error")]]]]].
% into:-[let,[[a17,[quote,u_string_l]],[b17,[defun,u_string_l,[u_x],[cond,[[stringp,u_x],u_x],[[symbolp,u_x],[symbol_name,u_x]],[t,[error,'$ARRAY'([*],claz_base_character,"type error")]]]]]],[if,[eq,a17,b17],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_string_l]],[quote,eq],[quote,[defun,u_string_l,[u_x],[cond,[[stringp,u_x],u_x],[[symbolp,u_x],[symbol_name,u_x]],[t,[error,'$ARRAY'([*],claz_base_character,"type error")]]]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a17,b17],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-assert_lsp(u_string_l,wl:lambda_def(defun,u_string_l,f_u_string_l,[u_x],[[cond,[[stringp,u_x],u_x],[[symbolp,u_x],[symbol_name,u_x]],[t,[error,'$ARRAY'([*],claz_base_character,"type error")]]]])),assert_lsp(u_string_l,wl:arglist_info(u_string_l,[u_x],[_18966],arginfo{all:[u_x],allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[u_x],opt:0,req:[u_x],rest:0,sublists:0,whole:0})),!,assert_lsp(u_string_l,wl:init_args(exact_only,u_string_l)),assert_lsp(u_string_l,(f_u_string_l(_18966,_19384):-is_stringp(_18966)->_19384=_18966;is_symbolp(_18966)->cl_symbol_name(_18966,_19258),_19384=_19258;cl_error(['$ARRAY'([*],claz_base_character,"type error")],_19324),_19384=_19324)),set_opv(f_u_string_l,classof,claz_function),set_opv(u_string_l,compile_as,kw_function),set_opv(u_string_l,function,f_u_string_l),_21112=u_string_l,_18386=[[bv(a17,u_string_l),bv(b17,_21112)]|_16160],get_var(_18386,a17,_28208),get_var(_18386,b17,_28674),(is_eq(_28208,_28674)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_string_l],eq,[defun,u_string_l,[u_x],[cond,[[stringp,u_x],u_x],[[symbolp,u_x],[symbol_name,u_x]],[t,[error,'$ARRAY'([*],claz_base_character,"type error")]]]]],_83066),_16990=_83066;get_var(_18386,a17,_32478),get_var(_18386,b17,_34110),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_32478,_34110],_83414),trace,_16990=_83414).
wl:lambda_def(defun, u_string_l, f_u_string_l, [u_x], [[cond, [[stringp, u_x], u_x], [[symbolp, u_x], [symbol_name, u_x]], [t, [error, '$ARRAY'([*], claz_base_character, "type error")]]]]).
wl:arglist_info(u_string_l, [u_x], [X_Get23], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_string_l).

```

### Compiled:  `U::STRING_L` 
```prolog
f_u_string_l(X_Get23, ElseResult27) :-
	(   is_stringp(X_Get23)
	->  ElseResult27=X_Get23
	;   is_symbolp(X_Get23)
	->  cl_symbol_name(X_Get23, TrueResult),
	    ElseResult27=TrueResult
	;   cl_error(['$ARRAY'([*], claz_base_character, "type error")],
		     ElseResult),
	    ElseResult27=ElseResult
	).
:- set_opv(f_u_string_l, classof, claz_function),
   set_opv(u_string_l, compile_as, kw_function),
   set_opv(u_string_l, function, f_u_string_l),
   DefunResult=u_string_l,
   LEnv=[[bv(a17, u_string_l), bv(b17, DefunResult)]|TLEnv],
   get_var(LEnv, a17, A17_Get),
   get_var(LEnv, b17, B17_Get),
   (   is_eq(A17_Get, B17_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_string_l],
		   eq,
		   
		   [ defun,
		     u_string_l,
		     [u_x],
		     
		     [ cond,
		       [[stringp, u_x], u_x],
		       [[symbolp, u_x], [symbol_name, u_x]],
		       
		       [ t,
			 
			 [ error,
			   '$ARRAY'([*], claz_base_character, "type error")
			 ]
		       ]
		     ]
		   ]
		 ],
		 TrueResult39),
       LetResult=TrueResult39
   ;   get_var(LEnv, a17, A17_Get37),
       get_var(LEnv, b17, B17_Get38),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A17_Get37,
		   B17_Get38
		 ],
		 ElseResult40),
       trace,
       LetResult=ElseResult40
   ).
:- side_effect(assert_lsp(u_string_l,
			  wl:lambda_def(defun, u_string_l, f_u_string_l, [u_x], [[cond, [[stringp, u_x], u_x], [[symbolp, u_x], [symbol_name, u_x]], [t, [error, '$ARRAY'([*], claz_base_character, "type error")]]]]))).
:- side_effect(assert_lsp(u_string_l,
			  wl:arglist_info(u_string_l, [u_x], [X_Get23], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(u_string_l, wl:init_args(exact_only, u_string_l))).
(is eq () (TAGBODY 1 (PRINT "hi" )))

% macroexpand:-[u_is,eq,[],[tagbody,1,[print,'$ARRAY'([*],claz_base_character,"hi")]]].
% into:-[let,[[a18,[]],[b18,[tagbody,1,[print,'$ARRAY'([*],claz_base_character,"hi")]]]],[if,[eq,a18,b18],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[]],[quote,eq],[quote,[tagbody,1,[print,'$ARRAY'([*],claz_base_character,"hi")]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a18,b18],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-call_addr_block(_15810,(push_label(1),cl_print('$ARRAY'([*],claz_base_character,"hi"),_16356)),[addr(addr_tagbody_4_1,1,'$unused',_16482,cl_print('$ARRAY'([*],claz_base_character,"hi"),_16490))]),_16326=[[bv(a18,[]),bv(b18,[])]|_15810],get_var(_16326,a18,_16798),get_var(_16326,b18,_16826),(is_eq(_16798,_16826)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[tagbody,1,[print,'$ARRAY'([*],claz_base_character,"hi")]]],_16840),_16200=_16840;get_var(_16326,a18,_16860),get_var(_16326,b18,_16906),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_16860,_16906],_16842),trace,_16200=_16842).
:-call_addr_block(_15722,(push_label(1),cl_print('$ARRAY'([*],claz_base_character,"hi"),_15846)),[addr(addr_tagbody_4_1,1,'$unused',_15874,cl_print('$ARRAY'([*],claz_base_character,"hi"),_15876))]),_15818=[[bv(a18,[]),bv(b18,[])]|_15722],get_var(_15818,a18,_16098),get_var(_15818,b18,_16124),(is_eq(_16098,_16124)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[tagbody,1,[print,'$ARRAY'([*],claz_base_character,"hi")]]],_16136),_15706=_16136;get_var(_15818,a18,_16156),get_var(_15818,b18,_16196),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_16156,_16196],_16138),trace,_15706=_16138).
(is eq () (TAGBODY a (PRINT "hi" )))

% macroexpand:-[u_is,eq,[],[tagbody,u_a,[print,'$ARRAY'([*],claz_base_character,"hi")]]].
% into:-[let,[[a19,[]],[b19,[tagbody,u_a,[print,'$ARRAY'([*],claz_base_character,"hi")]]]],[if,[eq,a19,b19],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[]],[quote,eq],[quote,[tagbody,u_a,[print,'$ARRAY'([*],claz_base_character,"hi")]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a19,b19],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-call_addr_block(_15886,(push_label(u_a),cl_print('$ARRAY'([*],claz_base_character,"hi"),_16432)),[addr(addr_tagbody_5_u_a,u_a,'$unused',_16558,cl_print('$ARRAY'([*],claz_base_character,"hi"),_16566))]),_16402=[[bv(a19,[]),bv(b19,[])]|_15886],get_var(_16402,a19,_16874),get_var(_16402,b19,_16902),(is_eq(_16874,_16902)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[tagbody,u_a,[print,'$ARRAY'([*],claz_base_character,"hi")]]],_16916),_16276=_16916;get_var(_16402,a19,_16936),get_var(_16402,b19,_16982),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_16936,_16982],_16918),trace,_16276=_16918).
:-call_addr_block(_15798,(push_label(u_a),cl_print('$ARRAY'([*],claz_base_character,"hi"),_15922)),[addr(addr_tagbody_5_u_a,u_a,'$unused',_15950,cl_print('$ARRAY'([*],claz_base_character,"hi"),_15952))]),_15894=[[bv(a19,[]),bv(b19,[])]|_15798],get_var(_15894,a19,_16174),get_var(_15894,b19,_16200),(is_eq(_16174,_16200)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[tagbody,u_a,[print,'$ARRAY'([*],claz_base_character,"hi")]]],_16212),_15782=_16212;get_var(_15894,a19,_16232),get_var(_15894,b19,_16272),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_16232,_16272],_16214),trace,_15782=_16214).
(is eq () (LET ((val 1 ))NIL ))
% macroexpand:-[u_is,eq,[],[let,[[u_val,1]],[]]].
% into:-[let,[[a110,[]],[b110,[let,[[u_val,1]],[]]]],[if,[eq,a110,b110],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[]],[quote,eq],[quote,[let,[[u_val,1]],[]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a110,b110],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-_17618=[[bv(u_val,1)]|_15932],_17480=[[bv(a110,[]),bv(b110,[])]|_15932],get_var(_17480,a110,_22504),get_var(_17480,b110,_22970),(is_eq(_22504,_22970)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[let,[[u_val,1]],[]]],_23344),_16308=_23344;get_var(_17480,a110,_26132),get_var(_17480,b110,_27806),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26132,_27806],_24570),trace,_16308=_24570).
:- LEnv6=[[bv(u_val, 1)]|TLEnv],
   LEnv=[[bv(a110, []), bv(b110, [])]|TLEnv],
   get_var(LEnv, a110, A110_Get),
   get_var(LEnv, b110, B110_Get),
   (   is_eq(A110_Get, B110_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [],
		   eq,
		   [let, [[u_val, 1]], []]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a110, A110_Get15),
       get_var(LEnv, b110, B110_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A110_Get15,
		   B110_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(is eq () (LET ((val 1 )) ))

% macroexpand:-[u_is,eq,[],[let,[[u_val,1]]]].
% into:-[let,[[a201,[]],[b201,[let,[[u_val,1]]]]],[if,[eq,a201,b201],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[]],[quote,eq],[quote,[let,[[u_val,1]]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a201,b201],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-_17684=[[bv(u_val,1)]|_16016],_17546=[[bv(a201,[]),bv(b201,[])]|_16016],get_var(_17546,a201,_22558),get_var(_17546,b201,_23024),(is_eq(_22558,_23024)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[let,[[u_val,1]]]],_23380),_16374=_23380;get_var(_17546,a201,_26150),get_var(_17546,b201,_27824),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_26150,_27824],_24588),trace,_16374=_24588).
:- LEnv6=[[bv(u_val, 1)]|TLEnv],
   LEnv=[[bv(a201, []), bv(b201, [])]|TLEnv],
   get_var(LEnv, a201, A201_Get),
   get_var(LEnv, b201, B201_Get),
   (   is_eq(A201_Get, B201_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [],
		   eq,
		   [let, [[u_val, 1]]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a201, A201_Get15),
       get_var(LEnv, b201, B201_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A201_Get15,
		   B201_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(is eql 1 (LET ((val 1 ))val ))
% macroexpand:-[u_is,eql,1,[let,[[u_val,1]],u_val]].
% into:-[let,[[a22,1],[b22,[let,[[u_val,1]],u_val]]],[if,[eql,a22,b22],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,1],[quote,eql],[quote,[let,[[u_val,1]],u_val]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a22,b22],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-_18340=[[bv(u_val,1)]|_16124],get_var(_18340,u_val,_18368),_17672=[[bv(a22,1),bv(b22,_18368)]|_16124],get_var(_17672,a22,_32506),get_var(_17672,b22,_32972),(is_eql(_32506,_32972)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),1,eql,[let,[[u_val,1]],u_val]],_33346),_16500=_33346;get_var(_17672,a22,_36092),get_var(_17672,b22,_37724),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_36092,_37724],_34572),trace,_16500=_34572).
:- LEnv6=[[bv(u_val, 1)]|TLEnv],
   get_var(LEnv6, u_val, Val_Get),
   LEnv=[[bv(a22, 1), bv(b22, Val_Get)]|TLEnv],
   get_var(LEnv, a22, A22_Get),
   get_var(LEnv, b22, B22_Get),
   (   is_eql(A22_Get, B22_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   1,
		   eql,
		   [let, [[u_val, 1]], u_val]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a22, A22_Get17),
       get_var(LEnv, b22, B22_Get18),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A22_Get17,
		   B22_Get18
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(is eql 'world (LET ((a 'b) )(LET ((a 'world) )
  (LET ((a 'hello) )(LET ((a a)(*package* (find-package :keyword) ) )(PRINT a) ) )(PRINT a) ) ))


;; 3.1. Review of defstruct

% macroexpand:-[u_is,eql,[quote,u_world],[let,[[u_a,[quote,u_b]]],[let,[[u_a,[quote,u_world]]],[let,[[u_a,[quote,u_hello]]],[let,[[u_a,u_a],[xx_package_xx,[find_package,kw_keyword]]],[print,u_a]]],[print,u_a]]]].
% into:-[let,[[a23,[quote,u_world]],[b23,[let,[[u_a,[quote,u_b]]],[let,[[u_a,[quote,u_world]]],[let,[[u_a,[quote,u_hello]]],[let,[[u_a,u_a],[xx_package_xx,[find_package,kw_keyword]]],[print,u_a]]],[print,u_a]]]]],[if,[eql,a23,b23],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_world]],[quote,eql],[quote,[let,[[u_a,[quote,u_b]]],[let,[[u_a,[quote,u_world]]],[let,[[u_a,[quote,u_hello]]],[let,[[u_a,u_a],[xx_package_xx,[find_package,kw_keyword]]],[print,u_a]]],[print,u_a]]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a23,b23],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-_19936=[[bv(u_a,u_b)]|_16944],_20010=[[bv(u_a,u_world)]|_19936],_20072=[[bv(u_a,u_hello)]|_20010],get_var(_20072,u_a,_20306),cl_find_package(kw_keyword,_20284),_20134=[[bv(u_a,_20306)]|_20072],save_special(sv(xx_package_xx,_20284,value,_20342)),get_var(_20134,u_a,_20360),cl_print(_20360,_20100),restore_special(sv(xx_package_xx,_20284,value,_20342)),get_var(_20010,u_a,_20444),cl_print(_20444,_19964),_19566=[[bv(a23,u_world),bv(b23,_19964)]|_16944],get_var(_19566,a23,_29398),get_var(_19566,b23,_29864),(is_eql(_29398,_29864)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_world],eql,[let,[[u_a,[quote,u_b]]],[let,[[u_a,[quote,u_world]]],[let,[[u_a,[quote,u_hello]]],[let,[[u_a,u_a],[xx_package_xx,[find_package,kw_keyword]]],[print,u_a]]],[print,u_a]]]],_30886),_17966=_30886;get_var(_19566,a23,_34280),get_var(_19566,b23,_35912),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_34280,_35912],_32760),trace,_17966=_32760).
:- LEnv6=[[bv(u_a, u_b)]|TLEnv],
   LEnv8=[[bv(u_a, u_world)]|LEnv6],
   LEnv10=[[bv(u_a, u_hello)]|LEnv8],
   get_var(LEnv10, u_a, A_Get),
   cl_find_package(kw_keyword, Xx_package_xx_Init),
   LEnv12=[[bv(u_a, A_Get)]|LEnv10],
   save_special(sv(xx_package_xx, Xx_package_xx_Init, value, Value)),
   get_var(LEnv12, u_a, A_Get17),
   cl_print(A_Get17, LetResult11),
   restore_special(sv(xx_package_xx, Xx_package_xx_Init, value, Value)),
   get_var(LEnv8, u_a, A_Get20),
   cl_print(A_Get20, LetResult9),
   LEnv=[[bv(a23, u_world), bv(b23, LetResult9)]|TLEnv],
   get_var(LEnv, a23, A23_Get),
   get_var(LEnv, b23, B23_Get),
   (   is_eql(A23_Get, B23_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_world],
		   eql,
		   
		   [ let,
		     [[u_a, [quote, u_b]]],
		     
		     [ let,
		       [[u_a, [quote, u_world]]],
		       
		       [ let,
			 [[u_a, [quote, u_hello]]],
			 
			 [ let,
			   
			   [ [u_a, u_a],
			     [xx_package_xx, [find_package, kw_keyword]]
			   ],
			   [print, u_a]
			 ]
		       ],
		       [print, u_a]
		     ]
		   ]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a23, A23_Get30),
       get_var(LEnv, b23, B23_Get31),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A23_Get30,
		   B23_Get31
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
; 3.1. Review of defstruct
(progn #+WAM-CL (prolog-inline "nop(trace)")(is eq 'point (defstruct point x y z)))
;; (defstruct point x y z)

% macroexpand:-[u_is,eq,[quote,u_point],[defstruct,u_point,u_x,u_y,u_z]].
% into:-[let,[[a24,[quote,u_point]],[b24,[defstruct,u_point,u_x,u_y,u_z]]],[if,[eq,a24,b24],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_point]],[quote,eq],[quote,[defstruct,u_point,u_x,u_y,u_z]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a24,b24],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_defstruct([u_point,u_x,u_y,u_z],_19288),_18938=[[bv(a24,u_point),bv(b24,_19288)]|_16750],get_var(_18938,a24,_26554),get_var(_18938,b24,_27020),(is_eq(_26554,_27020)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_point],eq,[defstruct,u_point,u_x,u_y,u_z]],_27412),_17754=_27412;get_var(_18938,a24,_30176),get_var(_18938,b24,_31808),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_30176,_31808],_28656),trace,_17754=_28656).
:- nop(trace),
   cl_defstruct([u_point, u_x, u_y, u_z], B24_Init),
   LEnv=[[bv(a24, u_point), bv(b24, B24_Init)]|TLEnv],
   get_var(LEnv, a24, A24_Get),
   get_var(LEnv, b24, B24_Get),
   (   is_eq(A24_Get, B24_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_point],
		   eq,
		   [defstruct, u_point, u_x, u_y, u_z]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a24, A24_Get13),
       get_var(LEnv, b24, B24_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A24_Get13,
		   B24_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, alternate_metaclass, zlot_structure_object_alternate_metaclass))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_alternate_metaclass, zlot_structure_object_alternate_metaclass))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 1, zlot_structure_object_alternate_metaclass))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, conc_name, zlot_structure_object_conc_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_conc_name, zlot_structure_object_conc_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 2, zlot_structure_object_conc_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, constructors, zlot_structure_object_constructors))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_constructors, zlot_structure_object_constructors))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 3, zlot_structure_object_constructors))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, copier_name, zlot_structure_object_copier_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_copier_name, zlot_structure_object_copier_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 4, zlot_structure_object_copier_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, doc, zlot_structure_object_doc))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_doc, zlot_structure_object_doc))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 5, zlot_structure_object_doc))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, element_type, zlot_structure_object_element_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_element_type, zlot_structure_object_element_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 6, zlot_structure_object_element_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, include, zlot_structure_object_include))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_include, zlot_structure_object_include))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 7, zlot_structure_object_include))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, inherited_accessor_alist, zlot_structure_object_inherited_accessor_alist))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_inherited_accessor_alist, zlot_structure_object_inherited_accessor_alist))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 8, zlot_structure_object_inherited_accessor_alist))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, length, zlot_structure_object_length))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_length, zlot_structure_object_length))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 9, zlot_structure_object_length))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, name, zlot_structure_object_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_name, zlot_structure_object_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 10, zlot_structure_object_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, named, zlot_structure_object_named))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_named, zlot_structure_object_named))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 11, zlot_structure_object_named))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, null_lexenv_p, zlot_structure_object_null_lexenv_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_null_lexenv_p, zlot_structure_object_null_lexenv_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 12, zlot_structure_object_null_lexenv_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, offset, zlot_structure_object_offset))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_offset, zlot_structure_object_offset))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 13, zlot_structure_object_offset))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, predicate, zlot_structure_object_predicate))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_predicate, zlot_structure_object_predicate))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 14, zlot_structure_object_predicate))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, print_option, zlot_structure_object_print_option))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_print_option, zlot_structure_object_print_option))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 15, zlot_structure_object_print_option))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, printer_fname, zlot_structure_object_printer_fname))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_printer_fname, zlot_structure_object_printer_fname))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 16, zlot_structure_object_printer_fname))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, pure, zlot_structure_object_pure))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_pure, zlot_structure_object_pure))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 17, zlot_structure_object_pure))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, slots, zlot_structure_object_slots))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_slots, zlot_structure_object_slots))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 18, zlot_structure_object_slots))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, structure_class, zlot_structure_object_structure_class))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_structure_class, zlot_structure_object_structure_class))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 19, zlot_structure_object_structure_class))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, type, zlot_structure_object_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_type, zlot_structure_object_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 20, zlot_structure_object_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, symbolname, u_point))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, type, u_point))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, slot, u_x, zlot_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, keyword, kw_x, zlot_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, ordinal, 1, zlot_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, slot, u_y, zlot_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, keyword, kw_y, zlot_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, ordinal, 2, zlot_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, slot, u_z, zlot_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, keyword, kw_z, zlot_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, ordinal, 3, zlot_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, conc_name, "POINT-"))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, setter_fn, u_setf_point_x, zlot_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, accessor, u_point_x, zlot_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, setter_fn, u_setf_point_y, zlot_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, accessor, u_point_y, zlot_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, setter_fn, u_setf_point_z, zlot_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, accessor, u_point_z, zlot_point_z))).
; (defstruct point x y z)
(is eq 'point4d (defstruct point4d x y z t))

% macroexpand:-[u_is,eq,[quote,u_point4d],[defstruct,u_point4d,u_x,u_y,u_z,t]].
% into:-[let,[[a25,[quote,u_point4d]],[b25,[defstruct,u_point4d,u_x,u_y,u_z,t]]],[if,[eq,a25,b25],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_point4d]],[quote,eq],[quote,[defstruct,u_point4d,u_x,u_y,u_z,t]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a25,b25],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_defstruct([u_point4d,u_x,u_y,u_z,t],_19350),_19000=[[bv(a25,u_point4d),bv(b25,_19350)]|_17392],get_var(_19000,a25,_26646),get_var(_19000,b25,_27112),(is_eq(_26646,_27112)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_point4d],eq,[defstruct,u_point4d,u_x,u_y,u_z,t]],_27522),_17804=_27522;get_var(_19000,a25,_30304),get_var(_19000,b25,_31936),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_30304,_31936],_28784),trace,_17804=_28784).
:- cl_defstruct([u_point4d, u_x, u_y, u_z, t], B25_Init),
   LEnv=[[bv(a25, u_point4d), bv(b25, B25_Init)]|TLEnv],
   get_var(LEnv, a25, A25_Get),
   get_var(LEnv, b25, B25_Get),
   (   is_eq(A25_Get, B25_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_point4d],
		   eq,
		   [defstruct, u_point4d, u_x, u_y, u_z, t]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a25, A25_Get13),
       get_var(LEnv, b25, B25_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A25_Get13,
		   B25_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, alternate_metaclass, zlot_structure_object_alternate_metaclass))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_alternate_metaclass, zlot_structure_object_alternate_metaclass))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 1, zlot_structure_object_alternate_metaclass))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, conc_name, zlot_structure_object_conc_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_conc_name, zlot_structure_object_conc_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 2, zlot_structure_object_conc_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, constructors, zlot_structure_object_constructors))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_constructors, zlot_structure_object_constructors))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 3, zlot_structure_object_constructors))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, copier_name, zlot_structure_object_copier_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_copier_name, zlot_structure_object_copier_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 4, zlot_structure_object_copier_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, doc, zlot_structure_object_doc))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_doc, zlot_structure_object_doc))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 5, zlot_structure_object_doc))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, element_type, zlot_structure_object_element_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_element_type, zlot_structure_object_element_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 6, zlot_structure_object_element_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, include, zlot_structure_object_include))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_include, zlot_structure_object_include))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 7, zlot_structure_object_include))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, inherited_accessor_alist, zlot_structure_object_inherited_accessor_alist))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_inherited_accessor_alist, zlot_structure_object_inherited_accessor_alist))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 8, zlot_structure_object_inherited_accessor_alist))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, length, zlot_structure_object_length))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_length, zlot_structure_object_length))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 9, zlot_structure_object_length))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, name, zlot_structure_object_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_name, zlot_structure_object_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 10, zlot_structure_object_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, named, zlot_structure_object_named))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_named, zlot_structure_object_named))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 11, zlot_structure_object_named))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, null_lexenv_p, zlot_structure_object_null_lexenv_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_null_lexenv_p, zlot_structure_object_null_lexenv_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 12, zlot_structure_object_null_lexenv_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, offset, zlot_structure_object_offset))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_offset, zlot_structure_object_offset))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 13, zlot_structure_object_offset))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, predicate, zlot_structure_object_predicate))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_predicate, zlot_structure_object_predicate))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 14, zlot_structure_object_predicate))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, print_option, zlot_structure_object_print_option))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_print_option, zlot_structure_object_print_option))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 15, zlot_structure_object_print_option))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, printer_fname, zlot_structure_object_printer_fname))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_printer_fname, zlot_structure_object_printer_fname))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 16, zlot_structure_object_printer_fname))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, pure, zlot_structure_object_pure))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_pure, zlot_structure_object_pure))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 17, zlot_structure_object_pure))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, slots, zlot_structure_object_slots))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_slots, zlot_structure_object_slots))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 18, zlot_structure_object_slots))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, structure_class, zlot_structure_object_structure_class))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_structure_class, zlot_structure_object_structure_class))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 19, zlot_structure_object_structure_class))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, type, zlot_structure_object_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_type, zlot_structure_object_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 20, zlot_structure_object_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, symbolname, u_point4d))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, type, u_point4d))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, slot, u_x, zlot_point4d_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, keyword, kw_x, zlot_point4d_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, ordinal, 1, zlot_point4d_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, slot, u_y, zlot_point4d_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, keyword, kw_y, zlot_point4d_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, ordinal, 2, zlot_point4d_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, slot, u_z, zlot_point4d_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, keyword, kw_z, zlot_point4d_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, ordinal, 3, zlot_point4d_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, slot, t, zlot_point4d_t))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, keyword, kw_t, zlot_point4d_t))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, ordinal, 4, zlot_point4d_t))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, conc_name, "POINT4D-"))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, setter_fn, u_setf_point4d_x, zlot_point4d_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, accessor, u_point4d_x, zlot_point4d_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, setter_fn, u_setf_point4d_y, zlot_point4d_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, accessor, u_point4d_y, zlot_point4d_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, setter_fn, u_setf_point4d_z, zlot_point4d_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, accessor, u_point4d_z, zlot_point4d_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, setter_fn, u_setf_point4d_t, zlot_point4d_t))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point4d, accessor, u_point4d_t, zlot_point4d_t))).
(defun distance-from-origin (point)
  (let* ((x (point-x point))
         (y (point-y point))
         (z (point-z point)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

wl:lambda_def(defun, u_distance_from_origin, f_u_distance_from_origin, [u_point], [[let_xx, [[u_x, [u_point_x, u_point]], [u_y, [u_point_y, u_point]], [u_z, [u_point_z, u_point]]], [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]).
wl:arglist_info(u_distance_from_origin, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_distance_from_origin).

```

### Compiled:  `U::DISTANCE-FROM-ORIGIN` 
```prolog
f_u_distance_from_origin(Point_Param, FnResult) :-
	Env=[bv(u_point, Point_Param)],
	f_u_point_x(Point_Param, X_Init),
	LEnv=[[bv(u_x, X_Init)]|Env],
	f_u_point_y(Point_Param, Y_Init),
	LEnv16=[[bv(u_y, Y_Init)]|LEnv],
	f_u_point_z(Point_Param, Z_Init),
	LEnv20=[[bv(u_z, Z_Init)]|LEnv16],
	get_var(LEnv20, u_x, X_Get25),
	*(X_Get25, X_Get25, _19162),
	get_var(LEnv20, u_y, Y_Get27),
	*(Y_Get27, Y_Get27, _19260),
	+(_19162, _19260, _19380),
	get_var(LEnv20, u_z, Z_Get29),
	*(Z_Get29, Z_Get29, _19392),
	+(_19380, _19392, Sqrt_Param),
	cl_sqrt(Sqrt_Param, LetResult17),
	LetResult17=FnResult.
:- set_opv(f_u_distance_from_origin, classof, claz_function),
   set_opv(u_distance_from_origin, compile_as, kw_function),
   set_opv(u_distance_from_origin, function, f_u_distance_from_origin),
   DefunResult=u_distance_from_origin.
:- side_effect(assert_lsp(u_distance_from_origin,
			  wl:lambda_def(defun, u_distance_from_origin, f_u_distance_from_origin, [u_point], [[let_xx, [[u_x, [u_point_x, u_point]], [u_y, [u_point_y, u_point]], [u_z, [u_point_z, u_point]]], [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]))).
:- side_effect(assert_lsp(u_distance_from_origin,
			  wl:arglist_info(u_distance_from_origin, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(u_distance_from_origin,
			  wl:init_args(exact_only, u_distance_from_origin))).
(defun reflect-in-y-axis (point)
  (setf (point-y point)
        (- (point-y point))))

wl:lambda_def(defun, u_reflect_in_y_axis, f_u_reflect_in_y_axis, [u_point], [[setf, [u_point_y, u_point], [-, [u_point_y, u_point]]]]).
wl:arglist_info(u_reflect_in_y_axis, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_reflect_in_y_axis).

```

### Compiled:  `U::REFLECT-IN-Y-AXIS` 
```prolog
f_u_reflect_in_y_axis(Point_Param, FnResult) :-
	Env=[bv(u_point, Point_Param)],
	f_u_point_y(Point_Param, Point_y_Ret),
	-(0, Point_y_Ret, CAR),
	set_place(Env, setf, [u_point_y, Point_Param], [CAR], Setf_R),
	Setf_R=FnResult.
:- set_opv(f_u_reflect_in_y_axis, classof, claz_function),
   set_opv(u_reflect_in_y_axis, compile_as, kw_function),
   set_opv(u_reflect_in_y_axis, function, f_u_reflect_in_y_axis),
   DefunResult=u_reflect_in_y_axis.
:- side_effect(assert_lsp(u_reflect_in_y_axis,
			  wl:lambda_def(defun, u_reflect_in_y_axis, f_u_reflect_in_y_axis, [u_point], [[setf, [u_point_y, u_point], [-, [u_point_y, u_point]]]]))).
:- side_effect(assert_lsp(u_reflect_in_y_axis,
			  wl:arglist_info(u_reflect_in_y_axis, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(u_reflect_in_y_axis,
			  wl:init_args(exact_only, u_reflect_in_y_axis))).
(list (setf my-point (make-point :x 3 :y 4 :z 12)) (setf my-point2 (make-point :x 3 :y 4 :z 12)))
:- f_u_make_point([kw_x, 3, kw_y, 4, kw_z, 12], Make_point_Ret),
   set_place(TLEnv, setf, [value, u_my_point], [Make_point_Ret], Setf_R),
   f_u_make_point([kw_x, 3, kw_y, 4, kw_z, 12], Make_point_Ret8),
   set_place(TLEnv, setf, [value, u_my_point2], [Make_point_Ret8], Setf_R6),
   _Ignored=[Setf_R, Setf_R6].
(setf my-point3 #S(POINT :X 3 :Y 4 :Z 12))
:- create_struct([u_point, kw_x, 3, kw_y, 4, kw_z, 12], Create_struct_Ret),
   set_place(TLEnv, setf, [value, u_my_point3], [Create_struct_Ret], Setf_R).
(setf my-point4d (make-point4d :x 3 :y 4 :z 12 :t 1))


:- f_u_make_point4d([kw_x, 3, kw_y, 4, kw_z, 12, kw_t, 1], Make_point4d_Ret),
   set_place(TLEnv, setf, [value, u_my_point4d], [Make_point4d_Ret], Setf_R).
(is eq t (point-p my-point))

% macroexpand:-[u_is,eq,t,[u_point_p,u_my_point]].
% into:-[let,[[a26,t],[b26,[u_point_p,u_my_point]]],[if,[eq,a26,b26],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,t],[quote,eq],[quote,[u_point_p,u_my_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a26,b26],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-get_var(_18606,u_my_point,_20582),f_u_point_p(_20582,_20454),_20034=[[bv(a26,t),bv(b26,_20454)]|_18606],get_var(_20034,a26,_30162),get_var(_20034,b26,_30628),(is_eq(_30162,_30628)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),t,eq,[u_point_p,u_my_point]],_30930),_18910=_30930;get_var(_20034,a26,_33604),get_var(_20034,b26,_35236),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_33604,_35236],_32084),trace,_18910=_32084).
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_point_p(My_point_Get, B26_Init),
   LEnv=[[bv(a26, t), bv(b26, B26_Init)]|TLEnv],
   get_var(LEnv, a26, A26_Get),
   get_var(LEnv, b26, B26_Get),
   (   is_eq(A26_Get, B26_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   t,
		   eq,
		   [u_point_p, u_my_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a26, A26_Get14),
       get_var(LEnv, b26, B26_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A26_Get14,
		   B26_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(is eq 'point (type-of my-point))

% macroexpand:-[u_is,eq,[quote,u_point],[type_of,u_my_point]].
% into:-[let,[[a27,[quote,u_point]],[b27,[type_of,u_my_point]]],[if,[eq,a27,b27],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_point]],[quote,eq],[quote,[type_of,u_my_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a27,b27],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-get_var(_18728,u_my_point,_20764),cl_type_of(_20764,_20636),_20216=[[bv(a27,u_point),bv(b27,_20636)]|_18728],get_var(_20216,a27,_30394),get_var(_20216,b27,_30860),(is_eq(_30394,_30860)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_point],eq,[type_of,u_my_point]],_31198),_19068=_31198;get_var(_20216,a27,_33908),get_var(_20216,b27,_35540),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_33908,_35540],_32388),trace,_19068=_32388).
:- get_var(TLEnv, u_my_point, My_point_Get),
   cl_type_of(My_point_Get, B27_Init),
   LEnv=[[bv(a27, u_point), bv(b27, B27_Init)]|TLEnv],
   get_var(LEnv, a27, A27_Get),
   get_var(LEnv, b27, B27_Get),
   (   is_eq(A27_Get, B27_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_point],
		   eq,
		   [type_of, u_my_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a27, A27_Get14),
       get_var(LEnv, b27, B27_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A27_Get14,
		   B27_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
#+IGNORE #+WAM-CL (prolog-call "break")

(is eql 13 (progn (print (distance-from-origin my-point))))

;; #+CLISP (BREAK)
;; #+WAM-CL (prolog-call "break")

% macroexpand:-[u_is,eql,13,[progn,[print,[u_distance_from_origin,u_my_point]]]].
% into:-[let,[[a28,13],[b28,[progn,[print,[u_distance_from_origin,u_my_point]]]]],[if,[eql,a28,b28],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,13],[quote,eql],[quote,[progn,[print,[u_distance_from_origin,u_my_point]]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a28,b28],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-get_var(_18842,u_my_point,_19380),f_u_distance_from_origin(_19380,_19378),cl_print(_19378,_19374),_19342=[[bv(a28,13),bv(b28,_19374)]|_18842],get_var(_19342,a28,_19838),get_var(_19342,b28,_19866),(is_eql(_19838,_19866)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),13,eql,[progn,[print,[u_distance_from_origin,u_my_point]]]],_19880),_19216=_19880;get_var(_19342,a28,_19900),get_var(_19342,b28,_19946),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_19900,_19946],_19882),trace,_19216=_19882).
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_distance_from_origin(My_point_Get, Print_Param),
   cl_print(Print_Param, B28_Init),
   LEnv=[[bv(a28, 13), bv(b28, B28_Init)]|TLEnv],
   get_var(LEnv, a28, A28_Get),
   get_var(LEnv, b28, B28_Get),
   (   is_eql(A28_Get, B28_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   13,
		   eql,
		   [progn, [print, [u_distance_from_origin, u_my_point]]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a28, A28_Get14),
       get_var(LEnv, b28, B28_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A28_Get14,
		   B28_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
; #+CLISP (BREAK)
; #+WAM-CL (prolog-call "break")
(is = -4 (reflect-in-y-axis my-point))

% macroexpand:-[u_is,=,-4,[u_reflect_in_y_axis,u_my_point]].
% into:-[let,[[a29,-4],[b29,[u_reflect_in_y_axis,u_my_point]]],[if,[=,a29,b29],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,-4],[quote,=],[quote,[u_reflect_in_y_axis,u_my_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a29,b29],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-get_var(_18828,u_my_point,_20804),f_u_reflect_in_y_axis(_20804,_20676),_20256=[[bv(a29,-4),bv(b29,_20676)]|_18828],get_var(_20256,a29,_30230),get_var(_20256,b29,_30696),(_30230=:=_30696->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),-4,=,[u_reflect_in_y_axis,u_my_point]],_30998),_19132=_30998;get_var(_20256,a29,_33672),get_var(_20256,b29,_35304),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_33672,_35304],_32152),trace,_19132=_32152).
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_reflect_in_y_axis(My_point_Get, B29_Init),
   LEnv=[[bv(a29, -4), bv(b29, B29_Init)]|TLEnv],
   get_var(LEnv, a29, A29_Get),
   get_var(LEnv, b29, B29_Get),
   (   A29_Get=:=B29_Get
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   -4,
		   (=),
		   [u_reflect_in_y_axis, u_my_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a29, A29_Get14),
       get_var(LEnv, b29, B29_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A29_Get14,
		   B29_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(is eq my-point my-point)

% macroexpand:-[u_is,eq,u_my_point,u_my_point].
% into:-[let,[[a210,u_my_point],[b210,u_my_point]],[if,[eq,a210,b210],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_my_point],[quote,eq],[quote,u_my_point]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a210,b210],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-get_var(_18882,u_my_point,_25368),get_var(_18882,u_my_point,_23506),_20250=[[bv(a210,_25368),bv(b210,_23506)]|_18882],get_var(_20250,a210,_32968),get_var(_20250,b210,_33434),(is_eq(_32968,_33434)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),u_my_point,eq,u_my_point],_33700),_19150=_33700;get_var(_20250,a210,_36380),get_var(_20250,b210,_38054),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_36380,_38054],_34818),trace,_19150=_34818).
:- get_var(TLEnv, u_my_point, My_point_Get7),
   LEnv=[[bv(a210, My_point_Get7), bv(b210, My_point_Get7)]|TLEnv],
   get_var(LEnv, a210, A210_Get),
   get_var(LEnv, b210, B210_Get),
   (   is_eq(A210_Get, B210_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   u_my_point,
		   eq,
		   u_my_point
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a210, A210_Get16),
       get_var(LEnv, b210, B210_Get17),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A210_Get16,
		   B210_Get17
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(setf a-similar-point #s(point :x 3 :y -4 :z 12))

; (is eq t (equal my-point a-similar-point))

:- create_struct([u_point, kw_x, 3, kw_y, -4, kw_z, 12], Create_struct_Ret),
   set_place(TLEnv, setf, [value, u_a_similar_point], [Create_struct_Ret], Setf_R).
 (is eq t (equal my-point a-similar-point))
(is eq nil (eq my-point a-similar-point))

% macroexpand:-[u_is,eq,[],[eq,u_my_point,u_a_similar_point]].
% into:-[let,[[a301,[]],[b301,[eq,u_my_point,u_a_similar_point]]],[if,[eq,a301,b301],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[]],[quote,eq],[quote,[eq,u_my_point,u_a_similar_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a301,b301],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-get_var(_19046,u_my_point,_21070),get_var(_19046,u_a_similar_point,_22864),cl_eq(_21070,_22864,_20936),_20516=[[bv(a301,[]),bv(b301,_20936)]|_19046],get_var(_20516,a301,_33150),get_var(_20516,b301,_33616),(is_eq(_33150,_33616)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[eq,u_my_point,u_a_similar_point]],_33936),_19368=_33936;get_var(_20516,a301,_36670),get_var(_20516,b301,_38344),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_36670,_38344],_35108),trace,_19368=_35108).
:- get_var(TLEnv, u_a_similar_point, A_similar_point_Get),
   get_var(TLEnv, u_my_point, My_point_Get),
   cl_eq(My_point_Get, A_similar_point_Get, B301_Init),
   LEnv=[[bv(a301, []), bv(b301, B301_Init)]|TLEnv],
   get_var(LEnv, a301, A301_Get),
   get_var(LEnv, b301, B301_Get),
   (   is_eq(A301_Get, B301_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [],
		   eq,
		   [eq, u_my_point, u_a_similar_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a301, A301_Get15),
       get_var(LEnv, b301, B301_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A301_Get15,
		   B301_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(equalp my-point a-similar-point)

:- get_var(TLEnv, u_a_similar_point, A_similar_point_Get),
   get_var(TLEnv, u_my_point, My_point_Get),
   cl_equalp(My_point_Get, A_similar_point_Get, _Ignored).
(is eq t (equalp my-point a-similar-point) )


;; 3.2. defclass

% macroexpand:-[u_is,eq,t,[equalp,u_my_point,u_a_similar_point]].
% into:-[let,[[a32,t],[b32,[equalp,u_my_point,u_a_similar_point]]],[if,[eq,a32,b32],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,t],[quote,eq],[quote,[equalp,u_my_point,u_a_similar_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a32,b32],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-get_var(_19140,u_my_point,_21152),get_var(_19140,u_a_similar_point,_22946),cl_equalp(_21152,_22946,_21018),_20598=[[bv(a32,t),bv(b32,_21018)]|_19140],get_var(_20598,a32,_33106),get_var(_20598,b32,_33572),(is_eq(_33106,_33572)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),t,eq,[equalp,u_my_point,u_a_similar_point]],_33892),_19462=_33892;get_var(_20598,a32,_36584),get_var(_20598,b32,_38216),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_36584,_38216],_35064),trace,_19462=_35064).
:- get_var(TLEnv, u_a_similar_point, A_similar_point_Get),
   get_var(TLEnv, u_my_point, My_point_Get),
   cl_equalp(My_point_Get, A_similar_point_Get, B32_Init),
   LEnv=[[bv(a32, t), bv(b32, B32_Init)]|TLEnv],
   get_var(LEnv, a32, A32_Get),
   get_var(LEnv, b32, B32_Get),
   (   is_eq(A32_Get, B32_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   t,
		   eq,
		   [equalp, u_my_point, u_a_similar_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a32, A32_Get15),
       get_var(LEnv, b32, B32_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A32_Get15,
		   B32_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
; 3.2. defclass
(unintern 'point)

:- cl_unintern(u_point, _Ignored).
(defclass point ()
  (x
   y
   z))

:- cl_defclass([u_point, [], [u_x, u_y, u_z]], _Ignored).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, symbolname, u_point))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, type, u_point))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, include, []))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, slot, u_x, zlot_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, keyword, kw_x, zlot_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, ordinal, 1, zlot_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, slot, u_y, zlot_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, keyword, kw_y, zlot_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, ordinal, 2, zlot_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, slot, u_z, zlot_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, keyword, kw_z, zlot_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, ordinal, 3, zlot_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, slot, u_x, zlot_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, keyword, kw_x, zlot_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, ordinal, 1, zlot_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, slot, u_y, zlot_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, keyword, kw_y, zlot_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, ordinal, 2, zlot_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, slot, u_z, zlot_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, keyword, kw_z, zlot_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, ordinal, 3, zlot_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_point, prototype, u_point_prototypical))).
(setf my-point (make-instance 'point))

:- cl_make_instance([u_point], Make_instance_Ret),
   set_place(TLEnv, setf, [value, u_my_point], [Make_instance_Ret], Setf_R).
(is eq 'point (type-of my-point))

% macroexpand:-[u_is,eq,[quote,u_point],[type_of,u_my_point]].
% into:-[let,[[a33,[quote,u_point]],[b33,[type_of,u_my_point]]],[if,[eq,a33,b33],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_point]],[quote,eq],[quote,[type_of,u_my_point]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a33,b33],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-get_var(_19272,u_my_point,_21308),cl_type_of(_21308,_21180),_20760=[[bv(a33,u_point),bv(b33,_21180)]|_19272],get_var(_20760,a33,_30938),get_var(_20760,b33,_31404),(is_eq(_30938,_31404)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_point],eq,[type_of,u_my_point]],_31742),_19612=_31742;get_var(_20760,a33,_34452),get_var(_20760,b33,_36084),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_34452,_36084],_32932),trace,_19612=_32932).
:- get_var(TLEnv, u_my_point, My_point_Get),
   cl_type_of(My_point_Get, B33_Init),
   LEnv=[[bv(a33, u_point), bv(b33, B33_Init)]|TLEnv],
   get_var(LEnv, a33, A33_Get),
   get_var(LEnv, b33, B33_Get),
   (   is_eq(A33_Get, B33_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_point],
		   eq,
		   [type_of, u_my_point]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a33, A33_Get14),
       get_var(LEnv, b33, B33_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A33_Get14,
		   B33_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
(defun set-point-values (point x y z)
  (setf (slot-value point 'x) x
        (slot-value point 'y) y
        (slot-value point 'z) z))

wl:lambda_def(defun, u_set_point_values, f_u_set_point_values, [u_point, u_x, u_y, u_z], [[setf, [slot_value, u_point, [quote, u_x]], u_x, [slot_value, u_point, [quote, u_y]], u_y, [slot_value, u_point, [quote, u_z]], u_z]]).
wl:arglist_info(u_set_point_values, [u_point, u_x, u_y, u_z], [Point_Param, X_Param, Y_Param, Z_Param], arginfo{all:[u_point, u_x, u_y, u_z], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point, u_x, u_y, u_z], opt:0, req:[u_point, u_x, u_y, u_z], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_set_point_values).

```

### Compiled:  `U::SET-POINT-VALUES` 
```prolog
f_u_set_point_values(Point_Param, X_Param, Y_Param, Z_Param, FnResult) :-
	Env=[bv(u_point, Point_Param), bv(u_x, X_Param), bv(u_y, Y_Param), bv(u_z, Z_Param)],
	set_place(Env, setf, [slot_value, Point_Param, u_x], [X_Param], Setf_R),
	set_place(Env, setf, [slot_value, Point_Param, u_y], [Y_Param], Setf_R23),
	set_place(Env, setf, [slot_value, Point_Param, u_z], [Z_Param], Setf_R26),
	Setf_R26=FnResult.
:- set_opv(f_u_set_point_values, classof, claz_function),
   set_opv(u_set_point_values, compile_as, kw_function),
   set_opv(u_set_point_values, function, f_u_set_point_values),
   DefunResult=u_set_point_values.
:- side_effect(assert_lsp(u_set_point_values,
			  wl:lambda_def(defun, u_set_point_values, f_u_set_point_values, [u_point, u_x, u_y, u_z], [[setf, [slot_value, u_point, [quote, u_x]], u_x, [slot_value, u_point, [quote, u_y]], u_y, [slot_value, u_point, [quote, u_z]], u_z]]))).
:- side_effect(assert_lsp(u_set_point_values,
			  wl:arglist_info(u_set_point_values, [u_point, u_x, u_y, u_z], [Point_Param, X_Param, Y_Param, Z_Param], arginfo{all:[u_point, u_x, u_y, u_z], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point, u_x, u_y, u_z], opt:0, req:[u_point, u_x, u_y, u_z], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(u_set_point_values,
			  wl:init_args(exact_only, u_set_point_values))).
(set-point-values my-point 3 4 12)

:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_set_point_values(My_point_Get, 3, 4, 12, _Ignored).
(defun distance-from-origin (point)
  (with-slots (x y z)
      point
    (sqrt (+ (* x x) (* y y) (* z z)))))


wl:lambda_def(defun, u_distance_from_origin, f_u_distance_from_origin, [u_point], [[with_slots, [u_x, u_y, u_z], u_point, [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]).
wl:arglist_info(u_distance_from_origin, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}).
wl: init_args(exact_only, u_distance_from_origin).

```

### Compiled:  `U::DISTANCE-FROM-ORIGIN` 
```prolog
f_u_distance_from_origin(Point_Param, FnResult) :-
	Env=[bv(u_point, Point_Param)],
	cl_slot_value(Point_Param, u_x, X_Init),
	cl_slot_value(Point_Param, u_y, Y_Init),
	cl_slot_value(Point_Param, u_z, Z_Init),
	LEnv=[[bv(u_x, X_Init), bv(u_y, Y_Init), bv(u_z, Z_Init)]|Env],
	get_var(LEnv, u_x, X_Get21),
	*(X_Get21, X_Get21, _20146),
	get_var(LEnv, u_y, Y_Get23),
	*(Y_Get23, Y_Get23, _20302),
	+(_20146, _20302, _20480),
	get_var(LEnv, u_z, Z_Get25),
	*(Z_Get25, Z_Get25, _20492),
	+(_20480, _20492, Sqrt_Param),
	cl_sqrt(Sqrt_Param, LetResult),
	LetResult=FnResult.
:- set_opv(f_u_distance_from_origin, classof, claz_function),
   set_opv(u_distance_from_origin, compile_as, kw_function),
   set_opv(u_distance_from_origin, function, f_u_distance_from_origin),
   DefunResult=u_distance_from_origin.
:- side_effect(assert_lsp(u_distance_from_origin,
			  wl:lambda_def(defun, u_distance_from_origin, f_u_distance_from_origin, [u_point], [[with_slots, [u_x, u_y, u_z], u_point, [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]))).
:- side_effect(assert_lsp(u_distance_from_origin,
			  wl:arglist_info(u_distance_from_origin, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}))).
:- side_effect(assert_lsp(u_distance_from_origin,
			  wl:init_args(exact_only, u_distance_from_origin))).
(DISASSEMBLE #'distance-from-origin)


:- cl_disassemble(function(u_distance_from_origin), _Ignored).
(distance-from-origin my-point)

;; 3.3. classes are objects

:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_distance_from_origin(My_point_Get, _Ignored).
; 3.3. classes are objects
(find-class 'point)

:- cl_find_class(u_point, _Ignored).
(class-name (find-class 'point))

:- cl_find_class(u_point, Class_name_Param),
   cl_class_name(Class_name_Param, _Ignored).
(class-of my-point)

;; #-(or cormanlisp CLISP WAM-CL)
:- get_var(TLEnv, u_my_point, My_point_Get),
   cl_class_of(My_point_Get, _Ignored).
; #-(or cormanlisp CLISP WAM-CL)
(typep my-point (class-of my-point))

:- get_var(TLEnv, u_my_point, My_point_Get5),
   cl_class_of(My_point_Get5, Class_of_Ret),
   cl_typep(My_point_Get5, Class_of_Ret, _Ignored).
(is eq (find-class 'STANDARD-CLASS)
       (class-of (class-of my-point)))

;; 3.4. you don't need clos to use clos

% macroexpand:-[u_is,eq,[find_class,[quote,standard_class]],[class_of,[class_of,u_my_point]]].
% into:-[let,[[a34,[find_class,[quote,standard_class]]],[b34,[class_of,[class_of,u_my_point]]]],[if,[eq,a34,b34],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[find_class,[quote,standard_class]]],[quote,eq],[quote,[class_of,[class_of,u_my_point]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a34,b34],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_find_class(standard_class,_20094),get_var(_19526,u_my_point,_20112),cl_class_of(_20112,_20108),cl_class_of(_20108,_20104),_20062=[[bv(a34,_20094),bv(b34,_20104)]|_19526],get_var(_20062,a34,_20584),get_var(_20062,b34,_20612),(is_eq(_20584,_20612)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[find_class,[quote,standard_class]],eq,[class_of,[class_of,u_my_point]]],_20626),_19936=_20626;get_var(_20062,a34,_20646),get_var(_20062,b34,_20692),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_20646,_20692],_20628),trace,_19936=_20628).
:- cl_find_class(standard_class, A34_Init),
   get_var(TLEnv, u_my_point, My_point_Get),
   cl_class_of(My_point_Get, Class_of_Param),
   cl_class_of(Class_of_Param, B34_Init),
   LEnv=[[bv(a34, A34_Init), bv(b34, B34_Init)]|TLEnv],
   get_var(LEnv, a34, A34_Get),
   get_var(LEnv, b34, B34_Get),
   (   is_eq(A34_Get, B34_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [find_class, [quote, standard_class]],
		   eq,
		   [class_of, [class_of, u_my_point]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a34, A34_Get15),
       get_var(LEnv, b34, B34_Get16),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A34_Get15,
		   B34_Get16
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
; 3.4. you don't need clos to use clos
(let ((the-symbol-class (find-class 'symbol)))
  (values the-symbol-class
          (class-name the-symbol-class)
          (eq the-symbol-class (class-of 'symbol))
          (class-of the-symbol-class)))

:- cl_find_class(symbol, The_symbol_class_Init),
   LEnv=[[bv(u_the_symbol_class, The_symbol_class_Init)]|TLEnv],
   get_var(LEnv, u_the_symbol_class, The_symbol_class_Get8),
   cl_class_name(The_symbol_class_Get8, Class_name_Ret),
   get_var(LEnv, u_the_symbol_class, The_symbol_class_Get9),
   cl_class_of(symbol, Class_of_Ret),
   cl_eq(The_symbol_class_Get9, Class_of_Ret, Eq_Ret),
   get_var(LEnv, u_the_symbol_class, The_symbol_class_Get10),
   cl_class_of(The_symbol_class_Get10, Class_of_Ret14),
   nb_setval('$mv_return',
	     [The_symbol_class_Get8, Class_name_Ret, Eq_Ret, Class_of_Ret14]).
(find-class t)

:- cl_find_class(t, _Ignored).
(is eq 'foo (defstruct foo))

% macroexpand:-[u_is,eq,[quote,u_foo],[defstruct,u_foo]].
% into:-[let,[[a35,[quote,u_foo]],[b35,[defstruct,u_foo]]],[if,[eq,a35,b35],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[quote,u_foo]],[quote,eq],[quote,[defstruct,u_foo]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a35,b35],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_defstruct([u_foo],_21412),_21062=[[bv(a35,u_foo),bv(b35,_21412)]|_19574],get_var(_21062,a35,_28540),get_var(_21062,b35,_29006),(is_eq(_28540,_29006)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,u_foo],eq,[defstruct,u_foo]],_29344),_19914=_29344;get_var(_21062,a35,_32054),get_var(_21062,b35,_33686),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_32054,_33686],_30534),trace,_19914=_30534).
:- cl_defstruct([u_foo], B35_Init),
   LEnv=[[bv(a35, u_foo), bv(b35, B35_Init)]|TLEnv],
   get_var(LEnv, a35, A35_Get),
   get_var(LEnv, b35, B35_Get),
   (   is_eq(A35_Get, B35_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [quote, u_foo],
		   eq,
		   [defstruct, u_foo]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a35, A35_Get13),
       get_var(LEnv, b35, B35_Get14),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A35_Get13,
		   B35_Get14
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, alternate_metaclass, zlot_structure_object_alternate_metaclass))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_alternate_metaclass, zlot_structure_object_alternate_metaclass))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 1, zlot_structure_object_alternate_metaclass))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, conc_name, zlot_structure_object_conc_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_conc_name, zlot_structure_object_conc_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 2, zlot_structure_object_conc_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, constructors, zlot_structure_object_constructors))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_constructors, zlot_structure_object_constructors))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 3, zlot_structure_object_constructors))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, copier_name, zlot_structure_object_copier_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_copier_name, zlot_structure_object_copier_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 4, zlot_structure_object_copier_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, doc, zlot_structure_object_doc))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_doc, zlot_structure_object_doc))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 5, zlot_structure_object_doc))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, element_type, zlot_structure_object_element_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_element_type, zlot_structure_object_element_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 6, zlot_structure_object_element_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, include, zlot_structure_object_include))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_include, zlot_structure_object_include))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 7, zlot_structure_object_include))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, inherited_accessor_alist, zlot_structure_object_inherited_accessor_alist))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_inherited_accessor_alist, zlot_structure_object_inherited_accessor_alist))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 8, zlot_structure_object_inherited_accessor_alist))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, length, zlot_structure_object_length))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_length, zlot_structure_object_length))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 9, zlot_structure_object_length))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, name, zlot_structure_object_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_name, zlot_structure_object_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 10, zlot_structure_object_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, named, zlot_structure_object_named))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_named, zlot_structure_object_named))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 11, zlot_structure_object_named))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, null_lexenv_p, zlot_structure_object_null_lexenv_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_null_lexenv_p, zlot_structure_object_null_lexenv_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 12, zlot_structure_object_null_lexenv_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, offset, zlot_structure_object_offset))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_offset, zlot_structure_object_offset))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 13, zlot_structure_object_offset))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, predicate, zlot_structure_object_predicate))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_predicate, zlot_structure_object_predicate))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 14, zlot_structure_object_predicate))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, print_option, zlot_structure_object_print_option))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_print_option, zlot_structure_object_print_option))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 15, zlot_structure_object_print_option))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, printer_fname, zlot_structure_object_printer_fname))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_printer_fname, zlot_structure_object_printer_fname))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 16, zlot_structure_object_printer_fname))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, pure, zlot_structure_object_pure))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_pure, zlot_structure_object_pure))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 17, zlot_structure_object_pure))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, slots, zlot_structure_object_slots))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_slots, zlot_structure_object_slots))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 18, zlot_structure_object_slots))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, structure_class, zlot_structure_object_structure_class))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_structure_class, zlot_structure_object_structure_class))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 19, zlot_structure_object_structure_class))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, slot, type, zlot_structure_object_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, keyword, kw_type, zlot_structure_object_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_structure_object, ordinal, 20, zlot_structure_object_type))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_foo, symbolname, u_foo))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_foo, type, u_foo))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_foo, conc_name, "FOO-"))).
(is eq (find-class 'foo) (class-of (make-foo)))

;; 3.5 slots

% macroexpand:-[u_is,eq,[find_class,[quote,u_foo]],[class_of,[u_make_foo]]].
% into:-[let,[[a36,[find_class,[quote,u_foo]]],[b36,[class_of,[u_make_foo]]]],[if,[eq,a36,b36],[format,t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[quote,[find_class,[quote,u_foo]]],[quote,eq],[quote,[class_of,[u_make_foo]]]],[progn,[format,t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),a36,b36],[sys_prolog_inline,'$ARRAY'([*],claz_base_character,"trace")]]]].
% code:-cl_find_class(u_foo,_20458),f_u_make_foo([],_20472),cl_class_of(_20472,_20468),_20426=[[bv(a36,_20458),bv(b36,_20468)]|_19908],get_var(_20426,a36,_20820),get_var(_20426,b36,_20848),(is_eq(_20820,_20848)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[find_class,[quote,u_foo]],eq,[class_of,[u_make_foo]]],_20862),_20300=_20862;get_var(_20426,a36,_20882),get_var(_20426,b36,_20928),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_20882,_20928],_20864),trace,_20300=_20864).
:- cl_find_class(u_foo, A36_Init),
   f_u_make_foo([], Class_of_Param),
   cl_class_of(Class_of_Param, B36_Init),
   LEnv=[[bv(a36, A36_Init), bv(b36, B36_Init)]|TLEnv],
   get_var(LEnv, a36, A36_Get),
   get_var(LEnv, b36, B36_Get),
   (   is_eq(A36_Get, B36_Get)
   ->  cl_format(
		 [ t,
		   '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"),
		   [find_class, [quote, u_foo]],
		   eq,
		   [class_of, [u_make_foo]]
		 ],
		 TrueResult),
       LetResult=TrueResult
   ;   get_var(LEnv, a36, A36_Get14),
       get_var(LEnv, b36, B36_Get15),
       cl_format(
		 [ t,
		   '$ARRAY'([*],
			    claz_base_character,
			    "FAILED: when matching ~a and ~a~%"),
		   A36_Get14,
		   B36_Get15
		 ],
		 ElseResult),
       trace,
       LetResult=ElseResult
   ).
; 3.5 slots
(defclass daft-point ()
  ((x :accessor daft-x :initarg :x)
   (y :accessor daft-y :initform 3.14159)
   (z :reader daft-z :allocation :class)))

:- cl_defclass(
	       [ u_daft_point,
		 [],
		 
		 [ [u_x, kw_accessor, u_daft_x, kw_initarg, kw_x],
		   [u_y, kw_accessor, u_daft_y, kw_initform, 3.14159],
		   [u_z, kw_reader, u_daft_z, kw_allocation, kw_class]
		 ]
	       ],
	       _Ignored).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, symbolname, u_daft_point))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, type, u_daft_point))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, include, []))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, slot, u_x, zlot_daft_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, keyword, kw_x, zlot_daft_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, ordinal, 1, zlot_daft_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, name, u_x, zlot_daft_point_x))).
:- side_effect(maybe_add_function(u_daft_x,
				  [obj],
				  ['slot-value', obj, [quote, u_x]],
				  u_daft_x)).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, accessor, u_daft_x, zlot_daft_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, initarg, kw_x, zlot_daft_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, slot, u_y, zlot_daft_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, keyword, kw_y, zlot_daft_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, ordinal, 2, zlot_daft_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, name, u_y, zlot_daft_point_y))).
:- side_effect(maybe_add_function(u_daft_y,
				  [obj],
				  ['slot-value', obj, [quote, u_y]],
				  u_daft_y)).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, accessor, u_daft_y, zlot_daft_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, initform, 3.14159, zlot_daft_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, slot, u_z, zlot_daft_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, keyword, kw_z, zlot_daft_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, ordinal, 3, zlot_daft_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, reader, u_daft_z, zlot_daft_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, allocation, kw_class, zlot_daft_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, conc_name, "DAFT-POINT-"))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, setter_fn, u_setf_daft_point_x, zlot_daft_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, setter_fn, u_setf_daft_point_y, zlot_daft_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, setter_fn, u_setf_daft_point_z, zlot_daft_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, accessor, u_daft_point_z, zlot_daft_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, slot, u_x, zlot_daft_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, keyword, kw_x, zlot_daft_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, ordinal, 1, zlot_daft_point_x))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, slot, u_y, zlot_daft_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, keyword, kw_y, zlot_daft_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, ordinal, 2, zlot_daft_point_y))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, slot, u_z, zlot_daft_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, keyword, kw_z, zlot_daft_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, ordinal, 3, zlot_daft_point_z))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_daft_point, prototype, u_daft_point_prototypical))).
(setf (slot-value (make-instance 'daft-point) 'z) 42)

:- cl_make_instance([u_daft_point], Make_instance_Ret),
   set_place(TLEnv, setf, [slot_value, Make_instance_Ret, u_z], [42], Setf_R).
(setf my-daft-point (make-instance 'daft-point :x 19))


:- cl_make_instance([u_daft_point, kw_x, 19], Make_instance_Ret),
   set_place(TLEnv, setf, [value, u_my_daft_point], [Make_instance_Ret], Setf_R).
#+PERFECT 
(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (progn #+WAM-CL (prolog-trace) (daft-z my-daft-point)))

(let ((temp (make-instance 'daft-point)))
  (setf (daft-y temp) 999
        (slot-value temp 'z) 0))

:- cl_make_instance([u_daft_point], Temp_Init),
   LEnv=[[bv(u_temp, Temp_Init)]|TLEnv],
   get_var(LEnv, u_temp, Temp_Get),
   set_place(LEnv, setf, [u_daft_y, Temp_Get], [999], Setf_R),
   get_var(LEnv, u_temp, Temp_Get11),
   set_place(LEnv, setf, [slot_value, Temp_Get11, u_z], [0], Setf_R10).
#+PERFECT
(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (daft-z my-daft-point))

;; 3.6 Subclasses and inheritance

; 3.6 Subclasses and inheritance
(defclass animal ()
  ((legs :reader leg-count :initarg :legs)
   (comes-from :reader comes-from :initarg :comes-from)))

:- cl_defclass(
	       [ u_animal,
		 [],
		 
		 [ [u_legs, kw_reader, u_leg_count, kw_initarg, kw_legs],
		   
		   [ u_comes_from,
		     kw_reader,
		     u_comes_from,
		     kw_initarg,
		     kw_comes_from
		   ]
		 ]
	       ],
	       _Ignored).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, symbolname, u_animal))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, type, u_animal))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, include, []))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, slot, u_legs, zlot_animal_legs))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, keyword, kw_legs, zlot_animal_legs))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, ordinal, 1, zlot_animal_legs))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, reader, u_leg_count, zlot_animal_legs))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, initarg, kw_legs, zlot_animal_legs))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, slot, u_comes_from, zlot_animal_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, keyword, kw_comes_from, zlot_animal_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, ordinal, 2, zlot_animal_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, reader, u_comes_from, zlot_animal_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, initarg, kw_comes_from, zlot_animal_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, conc_name, "ANIMAL-"))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, setter_fn, u_setf_animal_legs, zlot_animal_legs))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, accessor, u_animal_legs, zlot_animal_legs))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, setter_fn, u_setf_animal_comes_from, zlot_animal_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, accessor, u_animal_comes_from, zlot_animal_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, slot, u_legs, zlot_animal_legs))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, keyword, kw_legs, zlot_animal_legs))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, ordinal, 1, zlot_animal_legs))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, slot, u_comes_from, zlot_animal_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, keyword, kw_comes_from, zlot_animal_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, ordinal, 2, zlot_animal_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_animal, prototype, u_animal_prototypical))).
(defclass mammal (animal)
  ((diet :initform 'antelopes :initarg :diet)))

:- cl_defclass(
	       [ u_mammal,
		 [u_animal],
		 [[u_diet, kw_initform, [quote, u_antelopes], kw_initarg, kw_diet]]
	       ],
	       _Ignored).
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, symbolname, u_mammal))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, type, u_mammal))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, include, u_animal))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, slot, u_diet, zlot_mammal_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, keyword, kw_diet, zlot_mammal_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, ordinal, 1, zlot_mammal_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, initform, [quote, u_antelopes], zlot_mammal_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, initarg, kw_diet, zlot_mammal_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, conc_name, "MAMMAL-"))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, setter_fn, u_setf_mammal_diet, zlot_mammal_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, accessor, u_mammal_diet, zlot_mammal_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, slot, u_diet, zlot_mammal_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, keyword, kw_diet, zlot_mammal_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, ordinal, 1, zlot_mammal_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_mammal, prototype, u_mammal_prototypical))).
(defclass aardvark (mammal)
  ((cute-p :accessor cute-p :initform nil)))

:- cl_defclass(
	       [ u_aardvark,
		 [u_mammal],
		 [[u_cute_p, kw_accessor, u_cute_p, kw_initform, []]]
	       ],
	       _Ignored).
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, symbolname, u_aardvark))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, type, u_aardvark))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, include, u_mammal))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, slot, u_cute_p, zlot_aardvark_cute_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, keyword, kw_cute_p, zlot_aardvark_cute_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, ordinal, 1, zlot_aardvark_cute_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, name, u_cute_p, zlot_aardvark_cute_p))).
:- side_effect(maybe_add_function(u_cute_p,
				  [obj],
				  ['slot-value', obj, [quote, u_cute_p]],
				  u_cute_p)).
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, accessor, u_cute_p, zlot_aardvark_cute_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, initform, [], zlot_aardvark_cute_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, conc_name, "AARDVARK-"))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, setter_fn, u_setf_aardvark_cute_p, zlot_aardvark_cute_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, slot, u_cute_p, zlot_aardvark_cute_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, keyword, kw_cute_p, zlot_aardvark_cute_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, ordinal, 1, zlot_aardvark_cute_p))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_aardvark, prototype, u_aardvark_prototypical))).
(#-allegro class-direct-superclasses #+allegro aclmop:class-direct-superclasses
   (find-class 'aardvark))

;; ACL needs to instantiate a class before its precedence-list becomes visible
;; #+allegro
:- cl_find_class(u_aardvark, Direct_superclasses_Param),
   f_clos_class_direct_superclasses(Direct_superclasses_Param, _Ignored).
; ACL needs to instantiate a class before its precedence-list becomes visible
; #+allegro
(make-instance 'aardvark)

:- cl_make_instance([u_aardvark], _Ignored).
(#-allegro class-precedence-list #+allegro aclmop:class-precedence-list
   (find-class 'aardvark))

:- cl_find_class(u_aardvark, Precedence_list_Param),
   f_clos_class_precedence_list(Precedence_list_Param, _Ignored).
(defclass figurine ()
  ((potter :accessor made-by :initarg :made-by)
   (comes-from :initarg :made-in)))

:- cl_defclass(
	       [ u_figurine,
		 [],
		 
		 [ [u_potter, kw_accessor, u_made_by, kw_initarg, kw_made_by],
		   [u_comes_from, kw_initarg, kw_made_in]
		 ]
	       ],
	       _Ignored).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, symbolname, u_figurine))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, type, u_figurine))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, include, []))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, slot, u_potter, zlot_figurine_potter))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, keyword, kw_potter, zlot_figurine_potter))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, ordinal, 1, zlot_figurine_potter))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, name, u_potter, zlot_figurine_potter))).
:- side_effect(maybe_add_function(u_made_by,
				  [obj],
				  ['slot-value', obj, [quote, u_potter]],
				  u_made_by)).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, accessor, u_made_by, zlot_figurine_potter))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, initarg, kw_made_by, zlot_figurine_potter))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, slot, u_comes_from, zlot_figurine_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, keyword, kw_comes_from, zlot_figurine_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, ordinal, 2, zlot_figurine_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, initarg, kw_made_in, zlot_figurine_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, conc_name, "FIGURINE-"))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, setter_fn, u_setf_figurine_potter, zlot_figurine_potter))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, setter_fn, u_setf_figurine_comes_from, zlot_figurine_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, accessor, u_figurine_comes_from, zlot_figurine_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, slot, u_potter, zlot_figurine_potter))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, keyword, kw_potter, zlot_figurine_potter))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, ordinal, 1, zlot_figurine_potter))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, slot, u_comes_from, zlot_figurine_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, keyword, kw_comes_from, zlot_figurine_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, ordinal, 2, zlot_figurine_comes_from))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine, prototype, u_figurine_prototypical))).
(defclass figurine-aardvark (aardvark figurine)
  ((name :reader aardvark-name :initarg :aardvark-name)
   (diet :initform nil)))

;; ACL needs to instantiate a class before its precedence-list becomes visible
;; #+allegro 
:- cl_defclass(
	       [ u_figurine_aardvark,
		 [u_aardvark, u_figurine],
		 
		 [ 
		   [ sys_name,
		     kw_reader,
		     u_aardvark_name,
		     kw_initarg,
		     kw_aardvark_name
		   ],
		   [u_diet, kw_initform, []]
		 ]
	       ],
	       _Ignored).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, symbolname, u_figurine_aardvark))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, type, u_figurine_aardvark))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, include, [u_aardvark, u_figurine]))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, slot, sys_name, zlot_figurine_aardvark_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, keyword, kw_name, zlot_figurine_aardvark_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, ordinal, 1, zlot_figurine_aardvark_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, reader, u_aardvark_name, zlot_figurine_aardvark_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, initarg, kw_aardvark_name, zlot_figurine_aardvark_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, slot, u_diet, zlot_figurine_aardvark_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, keyword, kw_diet, zlot_figurine_aardvark_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, ordinal, 2, zlot_figurine_aardvark_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, initform, [], zlot_figurine_aardvark_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, conc_name, "FIGURINE-AARDVARK-"))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, setter_fn, u_setf_figurine_aardvark_name, zlot_figurine_aardvark_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, accessor, u_figurine_aardvark_name, zlot_figurine_aardvark_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, setter_fn, u_setf_figurine_aardvark_diet, zlot_figurine_aardvark_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, accessor, u_figurine_aardvark_diet, zlot_figurine_aardvark_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, slot, sys_name, zlot_figurine_aardvark_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, keyword, kw_name, zlot_figurine_aardvark_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, ordinal, 1, zlot_figurine_aardvark_name))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, slot, u_diet, zlot_figurine_aardvark_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, keyword, kw_diet, zlot_figurine_aardvark_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, ordinal, 2, zlot_figurine_aardvark_diet))).
:- side_effect(assertz_new(soops:struct_opv(claz_u_figurine_aardvark, prototype, u_figurine_aardvark_prototypical))).
; ACL needs to instantiate a class before its precedence-list becomes visible
; #+allegro 
(make-instance 'figurine-aardvark)

:- cl_make_instance([u_figurine_aardvark], _Ignored).
(#-allegro class-precedence-list #+allegro aclmop:class-precedence-list
             (find-class 'figurine-aardvark))

:- cl_find_class(u_figurine_aardvark, Precedence_list_Param),
   f_clos_class_precedence_list(Precedence_list_Param, _Ignored).
(setf Eric (make-instance 'figurine-aardvark
                          :legs 4
                          :made-by "Jen"
                          :made-in "Brittany"
                          :aardvark-name "Eric"))

:- cl_make_instance(
		    [ u_figurine_aardvark,
		      kw_legs,
		      4,
		      kw_made_by,
		      '$ARRAY'([*], claz_base_character, "Jen"),
		      kw_made_in,
		      '$ARRAY'([*], claz_base_character, "Brittany"),
		      kw_aardvark_name,
		      '$ARRAY'([*], claz_base_character, "Eric")
		    ],
		    Make_instance_Ret),
   set_place(TLEnv, setf, [value, u_eric], [Make_instance_Ret], Setf_R).
#+HAS_SHIFTF
(shiftf (cute-p Eric) t)

(slot-value Eric 'diet)

:- get_var(TLEnv, u_eric, Eric_Get),
   cl_slot_value(Eric_Get, u_diet, _Ignored).
#P"/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp.pro"

```

