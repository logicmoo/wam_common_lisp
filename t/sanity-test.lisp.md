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

__        ___    __  __        ____ _
\ \      / / \  |  \/  |      / ___| |
 \ \ /\ / / _ \ | |\/| |_____| |   | |
  \ V  V / ___ \| |  | |_____| |___| |___
   \_/\_/_/   \_\_|  |_|      \____|_____|

Common Lisp, written in Prolog
CL-USER> (compile-file "sanity-test")
/*
```cl
:- lisp_compiled_eval(
                      [ compile_file,
                        '$ARRAY'([*], claz_base_character, "sanity-test")
                      ]).
```
*/
% COMPILER
% 17,569 inferences, 0.003 CPU in 0.003 seconds (99% CPU, 5654482 Lips)
/*
```cl
:- cl_compile_file('$ARRAY'([*], claz_base_character, "sanity-test"),
                   [],
                   Compile_file_Ret).
```
*/
% EXEC
% Started at Thu Dec 21 20:26:41 2017
% 4.951 seconds cpu time for 19,063,005 inferences
% 39,170 atoms, 10,378 functors, 10,203 predicates, 237 modules, 1,139,508 VM-codes
%
%                        Limit    Allocated       In use
% Local  stack: 16,000,004,096      520,192       41,840 Bytes
% Global stack: 16,000,004,096      258,032      208,200 Bytes
% Trail  stack: 16,000,002,048      129,016       24,480 Bytes
%
% 49 garbage collections gained 7,614,968 bytes in 0.013 seconds.
% 1 atom garbage collections gained 9,877 atoms in 0.004 seconds.
% 45 clause garbage collections gained 2,340 clauses in 0.003 seconds.
% Stack shifts: 4 local, 2 global, 2 trail in 0.001 seconds
% 2 threads, 0 finished threads used 0.000 seconds
/*
```cl
 with_lisp_translation(file('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp'),
                         lisp_compile_to_prolog_output(<stream>(0x36de8c0))).

```
*/
/*
```cl

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
```
*/
/*
```cl
#+WAM-CL (prolog-call "cls.")

```
*/
:- lisp_compile_to_prolog(pkg_user, ['prolog-call', '$STRING'("cls.")]).
:- (   cls
   ->  _Ignored=t
   ;   _Ignored=[]
   ).
/*
```cl
(defun mapcar-visualize (func l) (if (null l) () (cons (apply func (list (first l))) (mapcar func (rest l)))))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defun,
                            'mapcar-visualize',
                            [func, l],

                            [ if,
                              [null, l],
                              [],

                              [ cons,
                                [apply, func, [list, [first, l]]],
                                [mapcar, func, [rest, l]]
                              ]
                            ]
                          ]).

##### annotating... `U::MAPCAR-VISUALIZE`
```prolog
wl:lambda_def(defun, u_mapcar_visualize, f_u_mapcar_visualize, [u_func, u_l], [[if, [null, u_l], [], [cons, [apply, u_func, [list, [first, u_l]]], [mapcar, u_func, [rest, u_l]]]]]).
```

##### annotating... `U::MAPCAR-VISUALIZE`
```prolog
wl:arglist_info(u_mapcar_visualize, [u_func, u_l], [Func_Param, L_Param], arginfo{all:[u_func, u_l], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_func, u_l], opt:0, req:[u_func, u_l], rest:0, sublists:0, whole:0}).
```

##### annotating... `U::MAPCAR-VISUALIZE`
```prolog
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
```
/*
```cl
:- set_opv(f_u_mapcar_visualize, classof, claz_function),
   set_opv(u_mapcar_visualize, compile_as, kw_function),
   set_opv(u_mapcar_visualize, function, f_u_mapcar_visualize),
   DefunResult=u_mapcar_visualize.
```
*/
/*
```cl
(load "../prolog/wam_cl/wam-cl-init")

```
*/
:- lisp_compile_to_prolog(pkg_user,
                          [load, '$STRING'("../prolog/wam_cl/wam-cl-init")]).
:- cl_load('$ARRAY'([*], claz_base_character, "../prolog/wam_cl/wam-cl-init"),
           [],
           _Ignored).
% Started at Thu Dec 21 20:26:41 2017
% 5.116 seconds cpu time for 19,901,269 inferences
% 39,655 atoms, 10,410 functors, 10,289 predicates, 239 modules, 1,143,999 VM-codes
%
%                        Limit    Allocated       In use
% Local  stack: 16,000,004,096      520,192      113,632 Bytes
% Global stack: 16,000,004,096      520,176      467,792 Bytes
% Trail  stack: 16,000,002,048      260,088       61,744 Bytes
%
% 58 garbage collections gained 11,477,600 bytes in 0.016 seconds.
% 1 atom garbage collections gained 9,877 atoms in 0.004 seconds.
% 45 clause garbage collections gained 2,340 clauses in 0.003 seconds.
% Stack shifts: 4 local, 3 global, 3 trail in 0.001 seconds
% 2 threads, 0 finished threads used 0.000 seconds
/*
```cl
(in-package #:system)

```
*/
:- lisp_compile_to_prolog(pkg_user, ['in-package', '#:system']).
:- cl_in_package(system1, _Ignored5).
/*
```cl
(defpackage "SYSTEM" (:nicknames "SYS"))
```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defpackage,
                            '$STRING'("SYSTEM"),
                            [':nicknames', '$STRING'("SYS")]
                          ]).
:- cl_defpackage('$ARRAY'([*], claz_base_character, "SYSTEM"),
                 [[kw_nicknames, '$ARRAY'([*], claz_base_character, "SYS")]],
                 _Ignored5).
/*
```cl
(defpackage "COMMON-LISP" (:nicknames "CL" "LISP")(:uses "SYSTEM"))
```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defpackage,
                            '$STRING'("COMMON-LISP"),
                            [':nicknames', '$STRING'("CL"), '$STRING'("LISP")],
                            [':uses', '$STRING'("SYSTEM")]
                          ]).
:- cl_defpackage('$ARRAY'([*], claz_base_character, "COMMON-LISP"),

                 [
                   [ kw_nicknames,
                     '$ARRAY'([*], claz_base_character, "CL"),
                     '$ARRAY'([*], claz_base_character, "LISP")
                   ],
                   [kw_uses, '$ARRAY'([*], claz_base_character, "SYSTEM")]
                 ],
                 _Ignored5).
/*
```cl
(defpackage "COMMON-LISP-USER" (:nicknames "U" "USER" "CL-USER") (:uses "COMMON-LISP"))
```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defpackage,
                            '$STRING'("COMMON-LISP-USER"),

                            [ ':nicknames',
                              '$STRING'("U"),
                              '$STRING'("USER"),
                              '$STRING'("CL-USER")
                            ],
                            [':uses', '$STRING'("COMMON-LISP")]
                          ]).
:- cl_defpackage('$ARRAY'([*], claz_base_character, "COMMON-LISP-USER"),

                 [
                   [ kw_nicknames,
                     '$ARRAY'([*], claz_base_character, "U"),
                     '$ARRAY'([*], claz_base_character, "USER"),
                     '$ARRAY'([*], claz_base_character, "CL-USER")
                   ],
                   [kw_uses, '$ARRAY'([*], claz_base_character, "COMMON-LISP")]
                 ],
                 _Ignored5).
/*
```cl
(defvar *lisp-file-type* "lisp")
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defvar, '*lisp-file-type*', '$STRING'("lisp")]).
:- set_var(TLEnv6,
           defvar,
           sys_xx_lisp_file_type_xx,
           '$ARRAY'([*], claz_base_character, "lisp")).
/*
```cl
(defvar *default-pathname-defaults* #P"")

```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defvar,
                            '*default-pathname-defaults*',
                            '$OBJ'(claz_pathname, "")
                          ]).
:- set_var(TLEnv6,
           defvar,
           xx_default_pathname_defaults_xx,
           '$OBJ'(claz_pathname, "")).
/*
```cl
(defun dd ()
 (let ((*lisp-file-type* "cl")
        (*default-pathname-defaults* (merge-pathnames "daydreamer/"))) (load "dd")))


```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defun,
                            dd,
                            [],

                            [ let,

                              [ ['*lisp-file-type*', '$STRING'("cl")],

                                [ '*default-pathname-defaults*',
                                  ['merge-pathnames', '$STRING'("daydreamer/")]
                                ]
                              ],
                              [load, '$STRING'("dd")]
                            ]
                          ]).

##### annotating... `SYS::DD`
```prolog
wl:lambda_def(defun, sys_dd, f_sys_dd, [], [[let, [[sys_xx_lisp_file_type_xx, '$ARRAY'([*], claz_base_character, "cl")], [xx_default_pathname_defaults_xx, [merge_pathnames, '$ARRAY'([*], claz_base_character, "daydreamer/")]]], [load, '$ARRAY'([*], claz_base_character, "dd")]]]).
```

##### annotating... `SYS::DD`
```prolog
wl:arglist_info(sys_dd, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::DD`
```prolog
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
```
/*
```cl
:- set_opv(f_sys_dd, classof, claz_function),
   set_opv(sys_dd, compile_as, kw_function),
   set_opv(sys_dd, function, f_sys_dd),
   _Ignored5=sys_dd.
```
*/
/*
```cl
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

```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defun,
                            'show-ascii-art',
                            [],
                            ['write-line', '$STRING'("  __________    ")],
                            ['write-line', '$STRING'(" / ___  ___ \\   ")],
                            ['write-line', '$STRING'("/ / @ \\/ @ \\ \\  ")],
                            ['write-line', '$STRING'("\\ \\___/\\___/ /\\ ")],
                            ['write-line', '$STRING'(" \\____\\/____/|| ")],
                            ['write-line', '$STRING'(" /     /\\\\\\\\\\// ")],
                            ['write-line', '$STRING'("|     |\\\\\\\\\\\\   ")],

                            [ 'write-line',
                              '$STRING'(" \\      \\\\\\\\\\\\  ")
                            ],
                            ['write-line', '$STRING'("   \\______/\\\\\\\\ ")],
                            ['write-line', '$STRING'("    _||_||_     ")],
                            ['write-line', '$STRING'("                ")]
                          ]).

##### annotating... `SYS::SHOW-ASCII-ART`
```prolog
wl:lambda_def(defun, sys_show_ascii_art, f_sys_show_ascii_art, [], [[write_line, '$ARRAY'([*], claz_base_character, "  __________    ")], [write_line, '$ARRAY'([*], claz_base_character, " / ___  ___ \\   ")], [write_line, '$ARRAY'([*], claz_base_character, "/ / @ \\/ @ \\ \\  ")], [write_line, '$ARRAY'([*], claz_base_character, "\\ \\___/\\___/ /\\ ")], [write_line, '$ARRAY'([*], claz_base_character, " \\____\\/____/|| ")], [write_line, '$ARRAY'([*], claz_base_character, " /     /\\\\\\\\\\// ")], [write_line, '$ARRAY'([*], claz_base_character, "|     |\\\\\\\\\\\\   ")], [write_line, '$ARRAY'([*], claz_base_character, " \\      \\\\\\\\\\\\  ")], [write_line, '$ARRAY'([*], claz_base_character, "   \\______/\\\\\\\\ ")], [write_line, '$ARRAY'([*], claz_base_character, "    _||_||_     ")], [write_line, '$ARRAY'([*], claz_base_character, "                ")]]).
```

##### annotating... `SYS::SHOW-ASCII-ART`
```prolog
wl:arglist_info(sys_show_ascii_art, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::SHOW-ASCII-ART`
```prolog
wl: init_args(exact_only, sys_show_ascii_art).

```

### Compiled:  `SYS::SHOW-ASCII-ART`
```prolog
f_sys_show_ascii_art(FnResult) :-
        Env=[],
        cl_write_line('$ARRAY'([*], claz_base_character, "  __________    "),
                      Write_line_Ret),
        cl_write_line('$ARRAY'([*], claz_base_character, " / ___  ___ \\   "),
                      Write_line_Ret16),
        cl_write_line('$ARRAY'([*], claz_base_character, "/ / @ \\/ @ \\ \\  "),
                      Write_line_Ret17),
        cl_write_line('$ARRAY'([*], claz_base_character, "\\ \\___/\\___/ /\\ "),
                      Write_line_Ret18),
        cl_write_line('$ARRAY'([*], claz_base_character, " \\____\\/____/|| "),
                      Write_line_Ret19),
        cl_write_line('$ARRAY'([*],
                               claz_base_character,
                               " /     /\\\\\\\\\\// "),
                      Write_line_Ret20),
        cl_write_line('$ARRAY'([*],
                               claz_base_character,
                               "|     |\\\\\\\\\\\\   "),
                      Write_line_Ret21),
        cl_write_line('$ARRAY'([*],
                               claz_base_character,
                               " \\      \\\\\\\\\\\\  "),
                      Write_line_Ret22),
        cl_write_line('$ARRAY'([*],
                               claz_base_character,
                               "   \\______/\\\\\\\\ "),
                      Write_line_Ret23),
        cl_write_line('$ARRAY'([*], claz_base_character, "    _||_||_     "),
                      Write_line_Ret24),
        cl_write_line('$ARRAY'([*], claz_base_character, "                "),
                      Write_line_Ret25),
        Write_line_Ret25=FnResult.
```
/*
```cl
:- set_opv(f_sys_show_ascii_art, classof, claz_function),
   set_opv(sys_show_ascii_art, compile_as, kw_function),
   set_opv(sys_show_ascii_art, function, f_sys_show_ascii_art),
   _Ignored5=sys_show_ascii_art.
```
*/
/*
```cl
(show-ascii-art)
```
*/
:- lisp_compile_to_prolog(pkg_sys, ['show-ascii-art']).
:- f_sys_show_ascii_art(_Ignored5).
/*
```cl
(load "wam-cl-init-1")
```
*/
:- lisp_compile_to_prolog(pkg_sys, [load, '$STRING'("wam-cl-init-1")]).
:- cl_load('$ARRAY'([*], claz_base_character, "wam-cl-init-1"), [], _Ignored5).
% Started at Thu Dec 21 20:26:41 2017
% 5.566 seconds cpu time for 22,223,194 inferences
% 39,872 atoms, 10,415 functors, 10,290 predicates, 239 modules, 1,148,408 VM-codes
%
%                        Limit    Allocated       In use
% Local  stack: 16,000,004,096    2,093,056      155,856 Bytes
% Global stack: 16,000,004,096      520,176      516,944 Bytes
% Trail  stack: 16,000,002,048      260,088       56,296 Bytes
%
% 89 garbage collections gained 24,585,144 bytes in 0.041 seconds.
% 1 atom garbage collections gained 9,877 atoms in 0.004 seconds.
% 45 clause garbage collections gained 2,340 clauses in 0.003 seconds.
% Stack shifts: 6 local, 3 global, 3 trail in 0.001 seconds
% 2 threads, 0 finished threads used 0.000 seconds
/*
```cl
;; setf.lisp
```
*/
/*
```cl
;;
```
*/
/*
```cl
;; Copyright (C) 2003-2006 Peter Graves
```
*/
/*
```cl
;; $Id$
```
*/
/*
```cl
;;
```
*/
/*
```cl
;; This program is free software; you can redistribute it and/or
```
*/
/*
```cl
;; modify it under the terms of the GNU General Public License
```
*/
/*
```cl
;; as published by the Free Software Foundation; either version 2
```
*/
/*
```cl
;; of the License, or (at your option) any later version.
```
*/
/*
```cl
;;
```
*/
/*
```cl
;; This program is distributed in the hope that it will be useful,
```
*/
/*
```cl
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
```
*/
/*
```cl
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
```
*/
/*
```cl
;; GNU General Public License for more details.
```
*/
/*
```cl
;;
```
*/
/*
```cl
;; You should have received a copy of the GNU General Public License
```
*/
/*
```cl
;; along with this program; if not, write to the Free Software
```
*/
/*
```cl
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
```
*/
/*
```cl
;;
```
*/
/*
```cl
;; As a special exception, the copyright holders of this library give you
```
*/
/*
```cl
;; permission to link this library with independent modules to produce an
```
*/
/*
```cl
;; executable, regardless of the license terms of these independent
```
*/
/*
```cl
;; modules, and to copy and distribute the resulting executable under
```
*/
/*
```cl
;; terms of your choice, provided that you also meet, for each linked
```
*/
/*
```cl
;; independent module, the terms and conditions of the license of that
```
*/
/*
```cl
;; module.  An independent module is a module which is not derived from
```
*/
/*
```cl
;; or based on this library.  If you modify this library, you may extend
```
*/
/*
```cl
;; this exception to your version of the library, but you are not
```
*/
/*
```cl
;; obligated to do so.  If you do not wish to do so, delete this
```
*/
/*
```cl
;; exception statement from your version.
```
*/
/*
```cl
(in-package "SYSTEM")

```
*/
:- lisp_compile_to_prolog(pkg_sys, ['in-package', '$STRING'("SYSTEM")]).
:- cl_in_package('$ARRAY'([*], claz_base_character, "SYSTEM"), _Ignored7).
/*
```cl
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
```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defun,
                            'get-setf-method-inverse',
                            [form, inverse, 'setf-function'],

                            [ let,
                              [['new-var', [gensym]], [vars, []], [vals, []]],

                              [ dolist,
                                [x, [cdr, form]],
                                [push, [gensym], vars],
                                [push, x, vals]
                              ],
                              [setq, vals, [nreverse, vals]],

                              [ values,
                                vars,
                                vals,
                                [list, 'new-var'],

                                [ if,
                                  'setf-function',

                                  [ '#BQ',

                                    [ ['#BQ-COMMA-ELIPSE', inverse],
                                      ['#COMMA', 'new-var'],
                                      ['#BQ-COMMA-ELIPSE', vars]
                                    ]
                                  ],

                                  [ if,
                                    [functionp, [car, inverse]],

                                    [ '#BQ',

                                      [ funcall,
                                        ['#BQ-COMMA-ELIPSE', inverse],
                                        ['#BQ-COMMA-ELIPSE', vars],
                                        ['#COMMA', 'new-var']
                                      ]
                                    ],

                                    [ '#BQ',

                                      [ ['#BQ-COMMA-ELIPSE', inverse],
                                        ['#BQ-COMMA-ELIPSE', vars],
                                        ['#COMMA', 'new-var']
                                      ]
                                    ]
                                  ]
                                ],

                                [ '#BQ',

                                  [ ['#COMMA', [car, form]],
                                    ['#BQ-COMMA-ELIPSE', vars]
                                  ]
                                ]
                              ]
                            ]
                          ]).

##### annotating... `SYS::GET-SETF-METHOD-INVERSE`
```prolog
wl:lambda_def(defun, sys_get_setf_method_inverse, f_sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], [[let, [[sys_new_var, [gensym]], [sys_vars, []], [sys_vals, []]], [dolist, [sys_x, [cdr, sys_form]], [push, [gensym], sys_vars], [push, sys_x, sys_vals]], [setq, sys_vals, [nreverse, sys_vals]], [values, sys_vars, sys_vals, [list, sys_new_var], [if, sys_setf_function, ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#COMMA', sys_new_var], ['#BQ-COMMA-ELIPSE', sys_vars]]], [if, [functionp, [car, sys_inverse]], ['#BQ', [funcall, ['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]], ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]]]], ['#BQ', [['#COMMA', [car, sys_form]], ['#BQ-COMMA-ELIPSE', sys_vars]]]]]]).
```

##### annotating... `SYS::GET-SETF-METHOD-INVERSE`
```prolog
wl:arglist_info(sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], [Form_Param, Inverse_Param, Setf_function_Param], arginfo{all:[sys_form, sys_inverse, sys_setf_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_inverse, sys_setf_function], opt:0, req:[sys_form, sys_inverse, sys_setf_function], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::GET-SETF-METHOD-INVERSE`
```prolog
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
        get_var(LEnv, sys_vals, Vals_Get32),
        cl_nreverse(Vals_Get32, Vals60),
        set_var(LEnv, sys_vals, Vals60),
        get_var(LEnv, sys_new_var, New_var_Get),
        get_var(LEnv, sys_vars, Vars_Get),
        CAR=[New_var_Get],
        (   Setf_function_Param\==[]
        ->  get_var(LEnv, sys_new_var, New_var_Get38),
            get_var(LEnv, sys_vars, Vars_Get39),
            bq_append(Inverse_Param, [New_var_Get38|Vars_Get39], TrueResult52),
            ElseResult53=TrueResult52
        ;   cl_car(Inverse_Param, PredArgResult),
            (   is_functionp(PredArgResult)
            ->  get_var(LEnv, sys_new_var, New_var_Get46),
                get_var(LEnv, sys_vars, Vars_Get45),
                bq_append(Vars_Get45, [New_var_Get46], Bq_append_Ret),
                bq_append([funcall|Inverse_Param], Bq_append_Ret, TrueResult),
                ElseResult53=TrueResult
            ;   get_var(LEnv, sys_new_var, New_var_Get49),
                get_var(LEnv, sys_vars, Vars_Get48),
                bq_append(Vars_Get48, [New_var_Get49], Bq_append_Ret62),
                bq_append(Inverse_Param, Bq_append_Ret62, ElseResult),
                ElseResult53=ElseResult
            )
        ),
        cl_car(Form_Param, Car_Ret),
        get_var(LEnv, sys_vars, Vars_Get55),
        nb_setval('$mv_return',
                  [Vars_Get, Vals_Get32, CAR, ElseResult53, [Car_Ret|Vars_Get55]]),
        Vars_Get=FnResult.
```
/*
```cl
:- set_opv(f_sys_get_setf_method_inverse, classof, claz_function),
   set_opv(sys_get_setf_method_inverse, compile_as, kw_function),
   set_opv(sys_get_setf_method_inverse, function, f_sys_get_setf_method_inverse),
   _Ignored7=sys_get_setf_method_inverse.
```
*/
/*
```cl
;; If a macro, expand one level and try again.  If not, go for the
```
*/
/*
```cl
;; SETF function.
```
*/
/*
```cl
(defun expand-or-get-setf-inverse (form environment)
  (multiple-value-bind (expansion expanded)
      (macroexpand-1 form environment)
    (if expanded
        (get-setf-expansion expansion environment)
        (get-setf-method-inverse form `(funcall #'(setf ,(car form)))
                                 t))))

```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defun,
                            'expand-or-get-setf-inverse',
                            [form, environment],

                            [ 'multiple-value-bind',
                              [expansion, expanded],
                              ['macroexpand-1', form, environment],

                              [ if,
                                expanded,
                                ['get-setf-expansion', expansion, environment],

                                [ 'get-setf-method-inverse',
                                  form,

                                  [ '#BQ',

                                    [ funcall,
                                      function([setf, ['#COMMA', [car, form]]])
                                    ]
                                  ],
                                  t
                                ]
                              ]
                            ]
                          ]).

##### annotating... `SYS::EXPAND-OR-GET-SETF-INVERSE`
```prolog
wl:lambda_def(defun, sys_expand_or_get_setf_inverse, f_sys_expand_or_get_setf_inverse, [sys_form, sys_environment], [[multiple_value_bind, [sys_expansion, sys_expanded], [macroexpand_1, sys_form, sys_environment], [if, sys_expanded, [get_setf_expansion, sys_expansion, sys_environment], [sys_get_setf_method_inverse, sys_form, ['#BQ', [funcall, function([setf, ['#COMMA', [car, sys_form]]])]], t]]]]).
```

##### annotating... `SYS::EXPAND-OR-GET-SETF-INVERSE`
```prolog
wl:arglist_info(sys_expand_or_get_setf_inverse, [sys_form, sys_environment], [Form_Param, Environment_Param], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:0, req:[sys_form, sys_environment], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::EXPAND-OR-GET-SETF-INVERSE`
```prolog
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
```
/*
```cl
:- set_opv(f_sys_expand_or_get_setf_inverse, classof, claz_function),
   set_opv(sys_expand_or_get_setf_inverse, compile_as, kw_function),
   set_opv(sys_expand_or_get_setf_inverse,
           function,
           f_sys_expand_or_get_setf_inverse),
   _Ignored7=sys_expand_or_get_setf_inverse.
```
*/
/*
```cl
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

```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defun,
                            'get-setf-expansion',
                            [form, '&optional', environment],

                            [ let,
                              [temp],

                              [ cond,

                                [ [symbolp, form],

                                  [ 'multiple-value-bind',
                                    [expansion, expanded],
                                    ['macroexpand-1', form, environment],

                                    [ if,
                                      expanded,

                                      [ 'get-setf-expansion',
                                        expansion,
                                        environment
                                      ],

                                      [ let,
                                        [['new-var', [gensym]]],

                                        [ values,
                                          [],
                                          [],
                                          [list, 'new-var'],

                                          [ '#BQ',

                                            [ setq,
                                              ['#COMMA', form],
                                              ['#COMMA', 'new-var']
                                            ]
                                          ],
                                          form
                                        ]
                                      ]
                                    ]
                                  ]
                                ],

                                [
                                  [ setq,
                                    temp,
                                    [get, [car, form], [quote, 'setf-inverse']]
                                  ],

                                  [ 'get-setf-method-inverse',
                                    form,
                                    ['#BQ', [['#COMMA', temp]]],
                                    []
                                  ]
                                ],

                                [
                                  [ setq,
                                    temp,
                                    [get, [car, form], [quote, 'setf-expander']]
                                  ],
                                  [funcall, temp, form, environment]
                                ],

                                [ t,

                                  [ 'expand-or-get-setf-inverse',
                                    form,
                                    environment
                                  ]
                                ]
                              ]
                            ]
                          ]).

##### annotating... `CL:GET-SETF-EXPANSION`
```prolog
wl:lambda_def(defun, get_setf_expansion, cl_get_setf_expansion, [sys_form, c38_optional, sys_environment], [[let, [sys_temp], [cond, [[symbolp, sys_form], [multiple_value_bind, [sys_expansion, sys_expanded], [macroexpand_1, sys_form, sys_environment], [if, sys_expanded, [get_setf_expansion, sys_expansion, sys_environment], [let, [[sys_new_var, [gensym]]], [values, [], [], [list, sys_new_var], ['#BQ', [setq, ['#COMMA', sys_form], ['#COMMA', sys_new_var]]], sys_form]]]]], [[setq, sys_temp, [get, [car, sys_form], [quote, sys_setf_inverse]]], [sys_get_setf_method_inverse, sys_form, ['#BQ', [['#COMMA', sys_temp]]], []]], [[setq, sys_temp, [get, [car, sys_form], [quote, sys_setf_expander]]], [funcall, sys_temp, sys_form, sys_environment]], [t, [sys_expand_or_get_setf_inverse, sys_form, sys_environment]]]]]).
```

##### annotating... `CL:GET-SETF-EXPANSION`
```prolog
wl:arglist_info(get_setf_expansion, [sys_form, c38_optional, sys_environment], [sys_form, sys_environment], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:[sys_environment], req:[sys_form], rest:0, sublists:0, whole:0}).
```

##### annotating... `CL:GET-SETF-EXPANSION`
```prolog
wl: init_args(1, get_setf_expansion).

```

### Compiled:  `CL:GET-SETF-EXPANSION`
```prolog
cl_get_setf_expansion(Form_Param, RestNKeys, LetResult28) :-
        Env=[bv(sys_form, Form_Param), bv(sys_environment, Environment_Param)],
        opt_var(Env, sys_environment, Environment_Param, true, [], 1, RestNKeys),
        LEnv=[[bv(sys_temp, [])]|Env],
        (   is_symbolp(Form_Param)
        ->  LEnv27=[[bv(sys_expansion, []), bv(sys_expanded, [])]|LEnv],
            get_var(LEnv27, sys_environment, Environment_Get),
            cl_macroexpand_1([Form_Param, Environment_Get], Macroexpand_1_Ret),
            setq_from_values(LEnv27, [sys_expansion, sys_expanded]),
            get_var(LEnv27, sys_expanded, IFTEST31),
            (   IFTEST31\==[]
            ->  get_var(LEnv27, sys_environment, Environment_Get35),
                get_var(LEnv27, sys_expansion, Expansion_Get),
                cl_get_setf_expansion(Expansion_Get,
                                      [Environment_Get35],
                                      TrueResult),
                LetResult28=TrueResult
            ;   cl_gensym(New_var_Init),
                LEnv36=[[bv(sys_new_var, New_var_Init)]|LEnv27],
                get_var(LEnv36, sys_new_var, New_var_Get),
                CAR=[New_var_Get],
                get_var(LEnv36, sys_new_var, New_var_Get41),
                nb_setval('$mv_return',

                          [ [],
                            [],
                            CAR,
                            [setq, Form_Param, New_var_Get41],
                            Form_Param
                          ]),
                LetResult28=[]
            )
        ;   cl_car(Form_Param, Get_Param),
            cl_get(Get_Param, sys_setf_inverse, [], IFTEST46),
            set_var(LEnv, sys_temp, IFTEST46),
            (   IFTEST46\==[]
            ->  get_var(LEnv, sys_temp, Temp_Get),
                f_sys_get_setf_method_inverse(Form_Param,
                                              [Temp_Get],
                                              [],
                                              TrueResult61),
                LetResult28=TrueResult61
            ;   cl_car(Form_Param, Get_Param68),
                cl_get(Get_Param68, sys_setf_expander, [], IFTEST52),
                set_var(LEnv, sys_temp, IFTEST52),
                (   IFTEST52\==[]
                ->  get_var(LEnv, sys_environment, Environment_Get56),
                    f_sys_temp(Form_Param, Environment_Get56, TrueResult59),
                    LetResult28=TrueResult59
                ;   get_var(LEnv, sys_environment, Environment_Get58),
                    f_sys_expand_or_get_setf_inverse(Form_Param,
                                                     Environment_Get58,
                                                     ElseResult),
                    LetResult28=ElseResult
                )
            )
        ).
```
/*
```cl
:- set_opv(cl_get_setf_expansion, classof, claz_function),
   set_opv(get_setf_expansion, compile_as, kw_function),
   set_opv(get_setf_expansion, function, cl_get_setf_expansion),
   _Ignored7=get_setf_expansion.
```
*/
/*
```cl
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
```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defmacro,
                            'abcl-setf',
                            ['&rest', args, '&environment', environment],

                            [ let,
                              [[numargs, [length, args]]],

                              [ cond,

                                [ [=, numargs, 2],

                                  [ let,

                                    [ [place, [first, args]],
                                      ['value-form', [second, args]]
                                    ],

                                    [ if,
                                      [atom, place],

                                      [ '#BQ',

                                        [ setq,
                                          ['#COMMA', place],
                                          ['#COMMA', 'value-form']
                                        ]
                                      ],

                                      [ progn,

                                        [ 'multiple-value-bind',

                                          [ dummies,
                                            vals,
                                            'store-vars',
                                            setter,
                                            getter
                                          ],

                                          [ 'get-setf-expansion',
                                            place,
                                            environment
                                          ],

                                          [ let,

                                            [
                                              [ inverse,

                                                [ get,
                                                  [car, place],
                                                  [quote, 'setf-inverse']
                                                ]
                                              ]
                                            ],

                                            [ if,

                                              [ and,
                                                inverse,
                                                [eq, inverse, [car, setter]]
                                              ],

                                              [ if,
                                                [functionp, inverse],

                                                [ '#BQ',

                                                  [ funcall,
                                                    ['#COMMA', inverse],

                                                    [ '#BQ-COMMA-ELIPSE',
                                                      [cdr, place]
                                                    ],
                                                    ['#COMMA', 'value-form']
                                                  ]
                                                ],

                                                [ '#BQ',

                                                  [ ['#COMMA', inverse],

                                                    [ '#BQ-COMMA-ELIPSE',
                                                      [cdr, place]
                                                    ],
                                                    ['#COMMA', 'value-form']
                                                  ]
                                                ]
                                              ],

                                              [ if,

                                                [ or,
                                                  [null, 'store-vars'],
                                                  [cdr, 'store-vars']
                                                ],

                                                [ '#BQ',

                                                  [ 'let*',

                                                    [
                                                      [ '#BQ-COMMA-ELIPSE',

                                                        [ mapcar,
                                                          function(list),
                                                          dummies,
                                                          vals
                                                        ]
                                                      ]
                                                    ],

                                                    [ 'multiple-value-bind',
                                                      ['#COMMA', 'store-vars'],
                                                      ['#COMMA', 'value-form'],
                                                      ['#COMMA', setter]
                                                    ]
                                                  ]
                                                ],

                                                [ '#BQ',

                                                  [ 'let*',

                                                    [
                                                      [ '#BQ-COMMA-ELIPSE',

                                                        [ mapcar,
                                                          function(list),
                                                          dummies,
                                                          vals
                                                        ]
                                                      ],

                                                      [ '#COMMA',

                                                        [ list,
                                                          [car, 'store-vars'],
                                                          'value-form'
                                                        ]
                                                      ]
                                                    ],
                                                    ['#COMMA', setter]
                                                  ]
                                                ]
                                              ]
                                            ]
                                          ]
                                        ]
                                      ]
                                    ]
                                  ]
                                ],

                                [ [oddp, numargs],

                                  [ error,
                                    '$STRING'("Odd number of arguments to SETF.")
                                  ]
                                ],

                                [ t,

                                  [ do,
                                    [[a, args, [cddr, a]], [l, []]],

                                    [ [null, a],

                                      [ '#BQ',

                                        [ progn,
                                          ['#BQ-COMMA-ELIPSE', [nreverse, l]]
                                        ]
                                      ]
                                    ],

                                    [ setq,
                                      l,

                                      [ cons,
                                        [list, [quote, setf], [car, a], [cadr, a]],
                                        l
                                      ]
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]).

##### annotating... `SYS::ABCL-SETF`
```prolog
wl:lambda_def(defmacro, sys_abcl_setf, f_sys_abcl_setf, [c38_rest, args, c38_environment, sys_environment], [progn, [let, [[sys_numargs, [length, args]]], [cond, [[=, sys_numargs, 2], [let, [[sys_place, [first, args]], [sys_value_form, [second, args]]], [if, [atom, sys_place], ['#BQ', [setq, ['#COMMA', sys_place], ['#COMMA', sys_value_form]]], [progn, [multiple_value_bind, [sys_dummies, sys_vals, sys_store_vars, sys_setter, sys_getter], [get_setf_expansion, sys_place, sys_environment], [let, [[sys_inverse, [get, [car, sys_place], [quote, sys_setf_inverse]]]], [if, [and, sys_inverse, [eq, sys_inverse, [car, sys_setter]]], [if, [functionp, sys_inverse], ['#BQ', [funcall, ['#COMMA', sys_inverse], ['#BQ-COMMA-ELIPSE', [cdr, sys_place]], ['#COMMA', sys_value_form]]], ['#BQ', [['#COMMA', sys_inverse], ['#BQ-COMMA-ELIPSE', [cdr, sys_place]], ['#COMMA', sys_value_form]]]], [if, [or, [null, sys_store_vars], [cdr, sys_store_vars]], ['#BQ', [let_xx, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]]], [multiple_value_bind, ['#COMMA', sys_store_vars], ['#COMMA', sys_value_form], ['#COMMA', sys_setter]]]], ['#BQ', [let_xx, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]], ['#COMMA', [list, [car, sys_store_vars], sys_value_form]]], ['#COMMA', sys_setter]]]]]]]]]]], [[oddp, sys_numargs], [error, '$ARRAY'([*], claz_base_character, "Odd number of arguments to SETF.")]], [t, [do, [[sys_a, args, [cddr, sys_a]], [sys_l, []]], [[null, sys_a], ['#BQ', [progn, ['#BQ-COMMA-ELIPSE', [nreverse, sys_l]]]]], [setq, sys_l, [cons, [list, [quote, setf], [car, sys_a], [cadr, sys_a]], sys_l]]]]]]]).
```

##### annotating... `SYS::ABCL-SETF`
```prolog
wl:arglist_info(sys_abcl_setf, [c38_rest, args, c38_environment, sys_environment], [args, sys_environment], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[sys_environment], key:0, names:[args, sys_environment], opt:0, req:0, rest:[args], sublists:0, whole:0}).
```

##### annotating... `SYS::ABCL-SETF`
```prolog
wl: init_args(0, sys_abcl_setf).

```

### Compiled:  `SYS::ABCL-SETF`
```prolog
f_sys_abcl_setf(Args_Param, FnResult) :-
        TLEnv8=[bv(args, Args_Param), bv(sys_environment, Environment_Param)],
        get_env(TLEnv8, sys_environment, Environment_Param),
        catch(( ( get_var(TLEnv8, args, Args_Get),
                  cl_length(Args_Get, Numargs_Init),
                  LEnv=[[bv(sys_numargs, Numargs_Init)]|TLEnv8],
                  get_var(LEnv, sys_numargs, Numargs_Get),
                  (   Numargs_Get=:=2
                  ->  get_var(LEnv, args, Args_Get30),
                      cl_car(Args_Get30, Place_Init),
                      get_var(LEnv, args, Args_Get31),
                      cl_second(Args_Get31, Value_form_Init),
                      LEnv28=[[bv(sys_place, Place_Init), bv(sys_value_form, Value_form_Init)]|LEnv],
                      get_var(LEnv28, sys_place, Place_Get),
                      (   Place_Get\=[CAR|CDR]
                      ->  get_var(LEnv28, sys_place, Place_Get38),
                          get_var(LEnv28, sys_value_form, Value_form_Get),
                          LetResult29=[setq, Place_Get38, Value_form_Get]
                      ;   LEnv40=[[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_store_vars, []), bv(sys_setter, []), bv(sys_getter, [])]|LEnv28],
                          get_var(LEnv40, sys_environment, Environment_Get),
                          get_var(LEnv40, sys_place, Place_Get42),
                          cl_get_setf_expansion(Place_Get42,
                                                [Environment_Get],
                                                Setf_expansion_Ret),
                          setq_from_values(LEnv40,

                                           [ sys_dummies,
                                             sys_vals,
                                             sys_store_vars,
                                             sys_setter,
                                             sys_getter
                                           ]),
                          get_var(LEnv40, sys_place, Place_Get46),
                          cl_car(Place_Get46, Get_Param),
                          cl_get(Get_Param, sys_setf_inverse, [], Inverse_Init),
                          LEnv44=[[bv(sys_inverse, Inverse_Init)]|LEnv40],
                          get_var(LEnv44, sys_inverse, IFTEST50),
                          (   IFTEST50\==[]
                          ->  get_var(LEnv44, sys_inverse, Inverse_Get53),
                              get_var(LEnv44, sys_setter, Setter_Get),
                              cl_car(Setter_Get, Car_Ret),
                              cl_eq(Inverse_Get53, Car_Ret, TrueResult),
                              IFTEST48=TrueResult
                          ;   IFTEST48=[]
                          ),
                          (   IFTEST48\==[]
                          ->  get_var(LEnv44, sys_inverse, Inverse_Get57),
                              (   is_functionp(Inverse_Get57)
                              ->  get_var(LEnv44, sys_inverse, Inverse_Get60),
                                  get_var(LEnv44, sys_place, Place_Get61),
                                  cl_cdr(Place_Get61, Cdr_Ret),
                                  get_var(LEnv44,
                                          sys_value_form,
                                          Value_form_Get62),
                                  bq_append([Inverse_Get60|Cdr_Ret],
                                            [Value_form_Get62],
                                            Bq_append_Ret),
                                  LetResult29=[funcall|Bq_append_Ret]
                              ;   get_var(LEnv44, sys_inverse, Inverse_Get63),
                                  get_var(LEnv44, sys_place, Place_Get64),
                                  cl_cdr(Place_Get64, Cdr_Ret147),
                                  get_var(LEnv44,
                                          sys_value_form,
                                          Value_form_Get65),
                                  bq_append([Inverse_Get63|Cdr_Ret147],
                                            [Value_form_Get65],
                                            ElseResult),
                                  LetResult29=ElseResult
                              )
                          ;   (   get_var(LEnv44,
                                          sys_store_vars,
                                          Store_vars_Get),
                                  cl_null(Store_vars_Get, FORM1_Res),
                                  FORM1_Res\==[],
                                  IFTEST67=FORM1_Res
                              ->  true
                              ;   get_var(LEnv44,
                                          sys_store_vars,
                                          Store_vars_Get70),
                                  cl_cdr(Store_vars_Get70, Cdr_Ret148),
                                  IFTEST67=Cdr_Ret148
                              ),
                              (   IFTEST67\==[]
                              ->  get_var(LEnv44, sys_dummies, Dummies_Get),
                                  get_var(LEnv44, sys_vals, Vals_Get),
                                  cl_mapcar(function(list),
                                            [Dummies_Get, Vals_Get],
                                            Mapcar_Ret),
                                  get_var(LEnv44, sys_setter, Setter_Get76),
                                  get_var(LEnv44,
                                          sys_store_vars,
                                          Store_vars_Get74),
                                  get_var(LEnv44,
                                          sys_value_form,
                                          Value_form_Get75),
                                  LetResult29=[let_xx, Mapcar_Ret, [multiple_value_bind, Store_vars_Get74, Value_form_Get75, Setter_Get76]]
                              ;   get_var(LEnv44, sys_dummies, Dummies_Get77),
                                  get_var(LEnv44, sys_vals, Vals_Get78),
                                  cl_mapcar(function(list),
                                            [Dummies_Get77, Vals_Get78],
                                            Bq_append_Param),
                                  get_var(LEnv44,
                                          sys_store_vars,
                                          Store_vars_Get79),
                                  cl_car(Store_vars_Get79, Car_Ret150),
                                  get_var(LEnv44,
                                          sys_value_form,
                                          Value_form_Get80),
                                  CAR152=[Car_Ret150, Value_form_Get80],
                                  bq_append(Bq_append_Param,
                                            [CAR152],
                                            Bq_append_Ret151),
                                  get_var(LEnv44, sys_setter, Setter_Get81),
                                  LetResult29=[let_xx, Bq_append_Ret151, Setter_Get81]
                              )
                          )
                      )
                  ;   get_var(LEnv, sys_numargs, Numargs_Get89),
                      (   mth:is_oddp(Numargs_Get89)
                      ->  cl_error(
                                   [ '$ARRAY'([*],
                                              claz_base_character,
                                              "Odd number of arguments to SETF.")
                                   ],
                                   TrueResult132),
                          LetResult29=TrueResult132
                      ;   get_var(LEnv, args, Args_Get94),
                          GoEnv=[[bv(sys_a, Args_Get94), bv(sys_l, [])]|LEnv],
                          catch(( call_addr_block(GoEnv,
                                                  (push_label(do_label_1), get_var(GoEnv, sys_a, IFTEST115), (IFTEST115==[]->cl_nreverse(L_Get108, Nreverse_Ret), throw(block_exit([], [progn|Nreverse_Ret])), _TBResult=ThrowResult119;cl_car(IFTEST97, Car_Ret154), cl_cadr(IFTEST97, Cadr_Ret), CAR156=[setf, Car_Ret154, Cadr_Ret], get_var(GoEnv, sys_l, L_Get125), L=[CAR156|L_Get125], set_var(GoEnv, sys_l, L), get_var(GoEnv, sys_a, A_Get126), cl_cddr(A_Get126, A), set_var(GoEnv, sys_a, A), goto(do_label_1, GoEnv), _TBResult=_GORES127)),

                                                  [ addr(addr_tagbody_1_do_label_1,
                                                         do_label_1,
                                                         '$unused',
                                                         AEnv,
                                                         (get_var(AEnv, sys_a, IFTEST97), (IFTEST97==[]->get_var(AEnv, sys_l, L_Get108), cl_nreverse(L_Get108, Nreverse_Ret157), throw(block_exit([], [progn|Nreverse_Ret157])), _35122=ThrowResult;cl_car(IFTEST97, Car_Ret158), cl_cadr(IFTEST97, Cadr_Ret159), CAR160=[setf, Car_Ret158, Cadr_Ret159], Set_var_Ret=[CAR160|L_Get108], set_var(AEnv, sys_l, Set_var_Ret), cl_cddr(IFTEST97, Cddr_Ret), set_var(AEnv, sys_a, Cddr_Ret), goto(do_label_1, AEnv), _35122=_GORES)))
                                                  ]),
                                  []=LetResult93
                                ),
                                block_exit([], LetResult93),
                                true),
                          LetResult29=LetResult93
                      )
                  )
                ),
                LetResult29=MFResult
              ),
              block_exit(sys_abcl_setf, MFResult),
              true),
        cl_eval(MFResult, FnResult).
```
/*
```cl
:- set_opv(f_sys_abcl_setf, classof, claz_macro),
   set_opv(sys_abcl_setf, compile_as, kw_operator),
   set_opv(sys_abcl_setf, function, f_sys_abcl_setf),
   _Ignored7=sys_abcl_setf.
```
*/
/*
```cl
;; Redefined in define-modify-macro.lisp.
```
*/
/*
```cl
(defmacro incf (place &optional (delta 1))
  `(setf ,place (+ ,place ,delta)))

;;; Redefined in define-modify-macro.lisp.
```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defmacro,
                            incf,
                            [place, '&optional', [delta, 1]],

                            [ '#BQ',

                              [ setf,
                                ['#COMMA', place],
                                [+, ['#COMMA', place], ['#COMMA', delta]]
                              ]
                            ]
                          ]).

##### annotating... `CL:INCF`
```prolog
wl:lambda_def(defmacro, incf, cl_incf, [sys_place, c38_optional, [sys_delta, 1]], [progn, ['#BQ', [setf, ['#COMMA', sys_place], [+, ['#COMMA', sys_place], ['#COMMA', sys_delta]]]]]).
```

##### annotating... `CL:INCF`
```prolog
wl:arglist_info(incf, [sys_place, c38_optional, [sys_delta, 1]], [sys_place, sys_delta], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_delta], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}).
```

##### annotating... `CL:INCF`
```prolog
wl: init_args(1, incf).

```

### Compiled:  `CL:INCF`
```prolog
cl_incf(Place_Param, RestNKeys, FnResult) :-
        TLEnv8=[bv(sys_place, Place_Param), bv(sys_delta, Delta_Param)],
        opt_var(TLEnv8, sys_delta, Delta_Param, true, 1, 1, RestNKeys),
        get_var(TLEnv8, sys_delta, Delta_Get),
        [setf, Place_Param, [+, Place_Param, Delta_Get]]=MFResult,
        cl_eval(MFResult, FnResult).
```
/*
```cl
:- set_opv(cl_incf, classof, claz_macro),
   set_opv(incf, compile_as, kw_operator),
   set_opv(incf, function, cl_incf),
   _Ignored7=incf.
```
*/
/*
```cl
;; Redefined in define-modify-macro.lisp.
```
*/
/*
```cl
(defmacro decf (place &optional (delta 1))
  `(setf ,place (- ,place ,delta)))

;; (defsetf subseq (sequence start &optional (end nil)) (v)
;;   `(progn (replace ,sequence ,v :start1 ,start :end1 ,end)
;;      ,v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defmacro,
                            decf,
                            [place, '&optional', [delta, 1]],

                            [ '#BQ',

                              [ setf,
                                ['#COMMA', place],
                                [-, ['#COMMA', place], ['#COMMA', delta]]
                              ]
                            ]
                          ]).

##### annotating... `CL:DECF`
```prolog
wl:lambda_def(defmacro, decf, cl_decf, [sys_place, c38_optional, [sys_delta, 1]], [progn, ['#BQ', [setf, ['#COMMA', sys_place], [-, ['#COMMA', sys_place], ['#COMMA', sys_delta]]]]]).
```

##### annotating... `CL:DECF`
```prolog
wl:arglist_info(decf, [sys_place, c38_optional, [sys_delta, 1]], [sys_place, sys_delta], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_delta], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}).
```

##### annotating... `CL:DECF`
```prolog
wl: init_args(1, decf).

```

### Compiled:  `CL:DECF`
```prolog
cl_decf(Place_Param, RestNKeys, FnResult) :-
        TLEnv8=[bv(sys_place, Place_Param), bv(sys_delta, Delta_Param)],
        opt_var(TLEnv8, sys_delta, Delta_Param, true, 1, 1, RestNKeys),
        get_var(TLEnv8, sys_delta, Delta_Get),
        [setf, Place_Param, [-, Place_Param, Delta_Get]]=MFResult,
        cl_eval(MFResult, FnResult).
```
/*
```cl
:- set_opv(cl_decf, classof, claz_macro),
   set_opv(decf, compile_as, kw_operator),
   set_opv(decf, function, cl_decf),
   _Ignored7=decf.
```
*/
/*
```cl
; (defsetf subseq (sequence start &optional (end nil)) (v)
```
*/
/*
```cl
;   `(progn (replace ,sequence ,v :start1 ,start :end1 ,end)
```
*/
/*
```cl
;      ,v))
```
*/
/*
```cl
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

```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defun,
                            '%set-subseq',
                            [sequence, start, '&rest', rest],

                            [ let,
                              [[end, []], v],

                              [ ecase,
                                [length, rest],
                                [1, [setq, v, [car, rest]]],
                                [2, [setq, end, [car, rest], v, [cadr, rest]]]
                              ],

                              [ progn,

                                [ replace,
                                  sequence,
                                  v,
                                  ':start1',
                                  start,
                                  ':end1',
                                  end
                                ],
                                v
                              ]
                            ]
                          ]).

##### annotating... `SYS::%SET-SUBSEQ`
```prolog
wl:lambda_def(defun, sys_pf_set_subseq, f_sys_pf_set_subseq, [sequence, start, c38_rest, rest], [[let, [[end, []], sys_v], [ecase, [length, rest], [1, [setq, sys_v, [car, rest]]], [2, [setq, end, [car, rest], sys_v, [cadr, rest]]]], [progn, [replace, sequence, sys_v, kw_start1, start, kw_end1, end], sys_v]]]).
```

##### annotating... `SYS::%SET-SUBSEQ`
```prolog
wl:arglist_info(sys_pf_set_subseq, [sequence, start, c38_rest, rest], [sequence, start, rest], arginfo{all:[sequence, start], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sequence, start, rest], opt:0, req:[sequence, start], rest:[rest], sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-SUBSEQ`
```prolog
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
        ->  get_var(LEnv, rest, Rest_Get30),
            cl_car(Rest_Get30, TrueResult37),
            set_var(LEnv, sys_v, TrueResult37),
            ElseResult38=TrueResult37
        ;   is_eq(avar(PredArg1Result, att(preserved_var, t, [])), 2)
        ->  get_var(LEnv, rest, Rest_Get33),
            cl_car(Rest_Get33, End),
            set_var(LEnv, end, End),
            get_var(LEnv, rest, Rest_Get34),
            cl_cadr(Rest_Get34, TrueResult),
            set_var(LEnv, sys_v, TrueResult),
            ElseResult38=TrueResult
        ;   cl_type_error(avar(CAR, att(preserved_var, t, [])),
                          [member, 1, 2],
                          ElseResult),
            ElseResult38=ElseResult
        ),
        get_var(LEnv, end, End_Get),
        get_var(LEnv, sys_v, V_Get),
        cl_replace(Sequence_Param,
                   V_Get,
                   [kw_start1, Start_Param, kw_end1, End_Get],
                   Replace_Ret),
        get_var(LEnv, sys_v, V_Get43),
        V_Get43=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_subseq, classof, claz_function),
   set_opv(sys_pf_set_subseq, compile_as, kw_function),
   set_opv(sys_pf_set_subseq, function, f_sys_pf_set_subseq),
   _Ignored7=sys_pf_set_subseq.
```
*/
/*
```cl
(defun %define-setf-macro (name expander inverse doc)
  (declare (ignore doc)) ; FIXME
  (when inverse
    (put name 'setf-inverse inverse))
  (when expander
    (put name 'setf-expander expander))
  name)

```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defun,
                            '%define-setf-macro',
                            [name, expander, inverse, doc],
                            [declare, [ignore, doc]],

                            [ when,
                              inverse,
                              [put, name, [quote, 'setf-inverse'], inverse]
                            ],

                            [ when,
                              expander,
                              [put, name, [quote, 'setf-expander'], expander]
                            ],
                            name
                          ]).

##### annotating... `SYS::%DEFINE-SETF-MACRO`
```prolog
wl:lambda_def(defun, sys_pf_define_setf_macro, f_sys_pf_define_setf_type_macro, [sys_name, sys_expander, sys_inverse, sys_doc], [[declare, [ignore, sys_doc]], [when, sys_inverse, [sys_put, sys_name, [quote, sys_setf_inverse], sys_inverse]], [when, sys_expander, [sys_put, sys_name, [quote, sys_setf_expander], sys_expander]], sys_name]).
```

##### annotating... `SYS::%DEFINE-SETF-MACRO`
```prolog
wl:arglist_info(sys_pf_define_setf_macro, [sys_name, sys_expander, sys_inverse, sys_doc], [Name_Param, Expander_Param, Inverse_Param, Doc_Param], arginfo{all:[sys_name, sys_expander, sys_inverse, sys_doc], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_expander, sys_inverse, sys_doc], opt:0, req:[sys_name, sys_expander, sys_inverse, sys_doc], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%DEFINE-SETF-MACRO`
```prolog
wl: init_args(exact_only, sys_pf_define_setf_macro).

```

### Compiled:  `SYS::%DEFINE-SETF-MACRO`
```prolog
f_sys_pf_define_setf_type_macro(Name_Param, Expander_Param, Inverse_Param, Doc_Param, Name_Param) :-
        Env=[bv(sys_doc, Doc_Param)],
        cl_declare([ignore, sys_doc], Declare_Ret),
        (   Inverse_Param\==[]
        ->  f_sys_put(Name_Param, sys_setf_inverse, Inverse_Param, TrueResult),
            _28386=TrueResult
        ;   _28386=[]
        ),
        (   Expander_Param\==[]
        ->  f_sys_put(Name_Param,
                      sys_setf_expander,
                      Expander_Param,
                      TrueResult34),
            _28572=TrueResult34
        ;   _28572=[]
        ).
```
/*
```cl
:- set_opv(f_sys_pf_define_setf_type_macro, classof, claz_function),
   set_opv(sys_pf_define_setf_macro, compile_as, kw_function),
   set_opv(sys_pf_define_setf_macro, function, f_sys_pf_define_setf_type_macro),
   _Ignored7=sys_pf_define_setf_macro.
```
*/
/*
```cl
 FIXME
```
*/
/*
```cl
(defmacro defsetf (access-function update-function)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (put ',access-function 'setf-inverse ',update-function)))

```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defmacro,
                            defsetf,
                            ['access-function', 'update-function'],

                            [ '#BQ',

                              [ 'eval-when',

                                [ ':load-toplevel',
                                  ':compile-toplevel',
                                  ':execute'
                                ],

                                [ put,
                                  [quote, ['#COMMA', 'access-function']],
                                  [quote, 'setf-inverse'],
                                  [quote, ['#COMMA', 'update-function']]
                                ]
                              ]
                            ]
                          ]).

##### annotating... `CL:DEFSETF`
```prolog
wl:lambda_def(defmacro, defsetf, cl_defsetf, [sys_access_function, sys_update_function], [progn, ['#BQ', [eval_when, [kw_load_toplevel, kw_compile_toplevel, kw_execute], [sys_put, [quote, ['#COMMA', sys_access_function]], [quote, sys_setf_inverse], [quote, ['#COMMA', sys_update_function]]]]]]).
```

##### annotating... `CL:DEFSETF`
```prolog
wl:arglist_info(defsetf, [sys_access_function, sys_update_function], [Access_function_Param, Update_function_Param], arginfo{all:[sys_access_function, sys_update_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_access_function, sys_update_function], opt:0, req:[sys_access_function, sys_update_function], rest:0, sublists:0, whole:0}).
```

##### annotating... `CL:DEFSETF`
```prolog
wl: init_args(exact_only, defsetf).

```

### Compiled:  `CL:DEFSETF`
```prolog
cl_defsetf(Access_function_Param, Update_function_Param, FnResult) :-
        [eval_when, [kw_load_toplevel, kw_compile_toplevel, kw_execute], [sys_put, [quote, Access_function_Param], [quote, sys_setf_inverse], [quote, Update_function_Param]]]=MFResult,
        cl_eval(MFResult, FnResult).
```
/*
```cl
:- set_opv(cl_defsetf, classof, claz_macro),
   set_opv(defsetf, compile_as, kw_operator),
   set_opv(defsetf, function, cl_defsetf),
   _Ignored7=defsetf.
```
*/
/*
```cl
(defun %set-caar (x v) (set-car (car x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-caar', [x, v], ['set-car', [car, x], v]]).

##### annotating... `SYS::%SET-CAAR`
```prolog
wl:lambda_def(defun, sys_pf_set_caar, f_sys_pf_set_caar, [sys_x, sys_v], [[sys_set_car, [car, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CAAR`
```prolog
wl:arglist_info(sys_pf_set_caar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CAAR`
```prolog
wl: init_args(exact_only, sys_pf_set_caar).

```

### Compiled:  `SYS::%SET-CAAR`
```prolog
f_sys_pf_set_caar(X_Param, V_Param, FnResult) :-
        cl_car(X_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_caar, classof, claz_function),
   set_opv(sys_pf_set_caar, compile_as, kw_function),
   set_opv(sys_pf_set_caar, function, f_sys_pf_set_caar),
   _Ignored7=sys_pf_set_caar.
```
*/
/*
```cl
(defun %set-cadr (x v) (set-car (cdr x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cadr', [x, v], ['set-car', [cdr, x], v]]).

##### annotating... `SYS::%SET-CADR`
```prolog
wl:lambda_def(defun, sys_pf_set_cadr, f_sys_pf_set_cadr, [sys_x, sys_v], [[sys_set_car, [cdr, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CADR`
```prolog
wl:arglist_info(sys_pf_set_cadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CADR`
```prolog
wl: init_args(exact_only, sys_pf_set_cadr).

```

### Compiled:  `SYS::%SET-CADR`
```prolog
f_sys_pf_set_cadr(X_Param, V_Param, FnResult) :-
        cl_cdr(X_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cadr, classof, claz_function),
   set_opv(sys_pf_set_cadr, compile_as, kw_function),
   set_opv(sys_pf_set_cadr, function, f_sys_pf_set_cadr),
   _Ignored7=sys_pf_set_cadr.
```
*/
/*
```cl
(defun %set-cdar (x v) (set-cdr (car x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cdar', [x, v], ['set-cdr', [car, x], v]]).

##### annotating... `SYS::%SET-CDAR`
```prolog
wl:lambda_def(defun, sys_pf_set_cdar, f_sys_pf_set_cdar, [sys_x, sys_v], [[sys_set_cdr, [car, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CDAR`
```prolog
wl:arglist_info(sys_pf_set_cdar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CDAR`
```prolog
wl: init_args(exact_only, sys_pf_set_cdar).

```

### Compiled:  `SYS::%SET-CDAR`
```prolog
f_sys_pf_set_cdar(X_Param, V_Param, FnResult) :-
        cl_car(X_Param, Set_cdr_Param),
        f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
        Set_cdr_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cdar, classof, claz_function),
   set_opv(sys_pf_set_cdar, compile_as, kw_function),
   set_opv(sys_pf_set_cdar, function, f_sys_pf_set_cdar),
   _Ignored7=sys_pf_set_cdar.
```
*/
/*
```cl
(defun %set-cddr (x v) (set-cdr (cdr x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cddr', [x, v], ['set-cdr', [cdr, x], v]]).

##### annotating... `SYS::%SET-CDDR`
```prolog
wl:lambda_def(defun, sys_pf_set_cddr, f_sys_pf_set_cddr, [sys_x, sys_v], [[sys_set_cdr, [cdr, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CDDR`
```prolog
wl:arglist_info(sys_pf_set_cddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CDDR`
```prolog
wl: init_args(exact_only, sys_pf_set_cddr).

```

### Compiled:  `SYS::%SET-CDDR`
```prolog
f_sys_pf_set_cddr(X_Param, V_Param, FnResult) :-
        cl_cdr(X_Param, Set_cdr_Param),
        f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
        Set_cdr_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cddr, classof, claz_function),
   set_opv(sys_pf_set_cddr, compile_as, kw_function),
   set_opv(sys_pf_set_cddr, function, f_sys_pf_set_cddr),
   _Ignored7=sys_pf_set_cddr.
```
*/
/*
```cl
(defun %set-caaar (x v) (set-car (caar x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-caaar', [x, v], ['set-car', [caar, x], v]]).

##### annotating... `SYS::%SET-CAAAR`
```prolog
wl:lambda_def(defun, sys_pf_set_caaar, f_sys_pf_set_caaar, [sys_x, sys_v], [[sys_set_car, [caar, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CAAAR`
```prolog
wl:arglist_info(sys_pf_set_caaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CAAAR`
```prolog
wl: init_args(exact_only, sys_pf_set_caaar).

```

### Compiled:  `SYS::%SET-CAAAR`
```prolog
f_sys_pf_set_caaar(X_Param, V_Param, FnResult) :-
        cl_caar(X_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_caaar, classof, claz_function),
   set_opv(sys_pf_set_caaar, compile_as, kw_function),
   set_opv(sys_pf_set_caaar, function, f_sys_pf_set_caaar),
   _Ignored7=sys_pf_set_caaar.
```
*/
/*
```cl
(defun %set-cadar (x v) (set-car (cdar x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cadar', [x, v], ['set-car', [cdar, x], v]]).

##### annotating... `SYS::%SET-CADAR`
```prolog
wl:lambda_def(defun, sys_pf_set_cadar, f_sys_pf_set_cadar, [sys_x, sys_v], [[sys_set_car, [cdar, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CADAR`
```prolog
wl:arglist_info(sys_pf_set_cadar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CADAR`
```prolog
wl: init_args(exact_only, sys_pf_set_cadar).

```

### Compiled:  `SYS::%SET-CADAR`
```prolog
f_sys_pf_set_cadar(X_Param, V_Param, FnResult) :-
        cl_cdar(X_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cadar, classof, claz_function),
   set_opv(sys_pf_set_cadar, compile_as, kw_function),
   set_opv(sys_pf_set_cadar, function, f_sys_pf_set_cadar),
   _Ignored7=sys_pf_set_cadar.
```
*/
/*
```cl
(defun %set-cdaar (x v) (set-cdr (caar x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cdaar', [x, v], ['set-cdr', [caar, x], v]]).

##### annotating... `SYS::%SET-CDAAR`
```prolog
wl:lambda_def(defun, sys_pf_set_cdaar, f_sys_pf_set_cdaar, [sys_x, sys_v], [[sys_set_cdr, [caar, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CDAAR`
```prolog
wl:arglist_info(sys_pf_set_cdaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CDAAR`
```prolog
wl: init_args(exact_only, sys_pf_set_cdaar).

```

### Compiled:  `SYS::%SET-CDAAR`
```prolog
f_sys_pf_set_cdaar(X_Param, V_Param, FnResult) :-
        cl_caar(X_Param, Set_cdr_Param),
        f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
        Set_cdr_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cdaar, classof, claz_function),
   set_opv(sys_pf_set_cdaar, compile_as, kw_function),
   set_opv(sys_pf_set_cdaar, function, f_sys_pf_set_cdaar),
   _Ignored7=sys_pf_set_cdaar.
```
*/
/*
```cl
(defun %set-cddar (x v) (set-cdr (cdar x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cddar', [x, v], ['set-cdr', [cdar, x], v]]).

##### annotating... `SYS::%SET-CDDAR`
```prolog
wl:lambda_def(defun, sys_pf_set_cddar, f_sys_pf_set_cddar, [sys_x, sys_v], [[sys_set_cdr, [cdar, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CDDAR`
```prolog
wl:arglist_info(sys_pf_set_cddar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CDDAR`
```prolog
wl: init_args(exact_only, sys_pf_set_cddar).

```

### Compiled:  `SYS::%SET-CDDAR`
```prolog
f_sys_pf_set_cddar(X_Param, V_Param, FnResult) :-
        cl_cdar(X_Param, Set_cdr_Param),
        f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
        Set_cdr_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cddar, classof, claz_function),
   set_opv(sys_pf_set_cddar, compile_as, kw_function),
   set_opv(sys_pf_set_cddar, function, f_sys_pf_set_cddar),
   _Ignored7=sys_pf_set_cddar.
```
*/
/*
```cl
(defun %set-caadr (x v) (set-car (cadr x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-caadr', [x, v], ['set-car', [cadr, x], v]]).

##### annotating... `SYS::%SET-CAADR`
```prolog
wl:lambda_def(defun, sys_pf_set_caadr, f_sys_pf_set_caadr, [sys_x, sys_v], [[sys_set_car, [cadr, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CAADR`
```prolog
wl:arglist_info(sys_pf_set_caadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CAADR`
```prolog
wl: init_args(exact_only, sys_pf_set_caadr).

```

### Compiled:  `SYS::%SET-CAADR`
```prolog
f_sys_pf_set_caadr(X_Param, V_Param, FnResult) :-
        cl_cadr(X_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_caadr, classof, claz_function),
   set_opv(sys_pf_set_caadr, compile_as, kw_function),
   set_opv(sys_pf_set_caadr, function, f_sys_pf_set_caadr),
   _Ignored7=sys_pf_set_caadr.
```
*/
/*
```cl
(defun %set-caddr (x v) (set-car (cddr x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-caddr', [x, v], ['set-car', [cddr, x], v]]).

##### annotating... `SYS::%SET-CADDR`
```prolog
wl:lambda_def(defun, sys_pf_set_caddr, f_sys_pf_set_caddr, [sys_x, sys_v], [[sys_set_car, [cddr, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CADDR`
```prolog
wl:arglist_info(sys_pf_set_caddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CADDR`
```prolog
wl: init_args(exact_only, sys_pf_set_caddr).

```

### Compiled:  `SYS::%SET-CADDR`
```prolog
f_sys_pf_set_caddr(X_Param, V_Param, FnResult) :-
        cl_cddr(X_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_caddr, classof, claz_function),
   set_opv(sys_pf_set_caddr, compile_as, kw_function),
   set_opv(sys_pf_set_caddr, function, f_sys_pf_set_caddr),
   _Ignored7=sys_pf_set_caddr.
```
*/
/*
```cl
(defun %set-cdadr (x v) (set-cdr (cadr x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cdadr', [x, v], ['set-cdr', [cadr, x], v]]).

##### annotating... `SYS::%SET-CDADR`
```prolog
wl:lambda_def(defun, sys_pf_set_cdadr, f_sys_pf_set_cdadr, [sys_x, sys_v], [[sys_set_cdr, [cadr, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CDADR`
```prolog
wl:arglist_info(sys_pf_set_cdadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CDADR`
```prolog
wl: init_args(exact_only, sys_pf_set_cdadr).

```

### Compiled:  `SYS::%SET-CDADR`
```prolog
f_sys_pf_set_cdadr(X_Param, V_Param, FnResult) :-
        cl_cadr(X_Param, Set_cdr_Param),
        f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
        Set_cdr_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cdadr, classof, claz_function),
   set_opv(sys_pf_set_cdadr, compile_as, kw_function),
   set_opv(sys_pf_set_cdadr, function, f_sys_pf_set_cdadr),
   _Ignored7=sys_pf_set_cdadr.
```
*/
/*
```cl
(defun %set-cdddr (x v) (set-cdr (cddr x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cdddr', [x, v], ['set-cdr', [cddr, x], v]]).

##### annotating... `SYS::%SET-CDDDR`
```prolog
wl:lambda_def(defun, sys_pf_set_cdddr, f_sys_pf_set_cdddr, [sys_x, sys_v], [[sys_set_cdr, [cddr, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CDDDR`
```prolog
wl:arglist_info(sys_pf_set_cdddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CDDDR`
```prolog
wl: init_args(exact_only, sys_pf_set_cdddr).

```

### Compiled:  `SYS::%SET-CDDDR`
```prolog
f_sys_pf_set_cdddr(X_Param, V_Param, FnResult) :-
        cl_cddr(X_Param, Set_cdr_Param),
        f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
        Set_cdr_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cdddr, classof, claz_function),
   set_opv(sys_pf_set_cdddr, compile_as, kw_function),
   set_opv(sys_pf_set_cdddr, function, f_sys_pf_set_cdddr),
   _Ignored7=sys_pf_set_cdddr.
```
*/
/*
```cl
(defun %set-caaaar (x v) (set-car (caaar x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-caaaar', [x, v], ['set-car', [caaar, x], v]]).

##### annotating... `SYS::%SET-CAAAAR`
```prolog
wl:lambda_def(defun, sys_pf_set_caaaar, f_sys_pf_set_caaaar, [sys_x, sys_v], [[sys_set_car, [caaar, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CAAAAR`
```prolog
wl:arglist_info(sys_pf_set_caaaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CAAAAR`
```prolog
wl: init_args(exact_only, sys_pf_set_caaaar).

```

### Compiled:  `SYS::%SET-CAAAAR`
```prolog
f_sys_pf_set_caaaar(X_Param, V_Param, FnResult) :-
        cl_caaar(X_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_caaaar, classof, claz_function),
   set_opv(sys_pf_set_caaaar, compile_as, kw_function),
   set_opv(sys_pf_set_caaaar, function, f_sys_pf_set_caaaar),
   _Ignored7=sys_pf_set_caaaar.
```
*/
/*
```cl
(defun %set-cadaar (x v) (set-car (cdaar x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cadaar', [x, v], ['set-car', [cdaar, x], v]]).

##### annotating... `SYS::%SET-CADAAR`
```prolog
wl:lambda_def(defun, sys_pf_set_cadaar, f_sys_pf_set_cadaar, [sys_x, sys_v], [[sys_set_car, [cdaar, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CADAAR`
```prolog
wl:arglist_info(sys_pf_set_cadaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CADAAR`
```prolog
wl: init_args(exact_only, sys_pf_set_cadaar).

```

### Compiled:  `SYS::%SET-CADAAR`
```prolog
f_sys_pf_set_cadaar(X_Param, V_Param, FnResult) :-
        cl_cdaar(X_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cadaar, classof, claz_function),
   set_opv(sys_pf_set_cadaar, compile_as, kw_function),
   set_opv(sys_pf_set_cadaar, function, f_sys_pf_set_cadaar),
   _Ignored7=sys_pf_set_cadaar.
```
*/
/*
```cl
(defun %set-cdaaar (x v) (set-cdr (caaar x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cdaaar', [x, v], ['set-cdr', [caaar, x], v]]).

##### annotating... `SYS::%SET-CDAAAR`
```prolog
wl:lambda_def(defun, sys_pf_set_cdaaar, f_sys_pf_set_cdaaar, [sys_x, sys_v], [[sys_set_cdr, [caaar, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CDAAAR`
```prolog
wl:arglist_info(sys_pf_set_cdaaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CDAAAR`
```prolog
wl: init_args(exact_only, sys_pf_set_cdaaar).

```

### Compiled:  `SYS::%SET-CDAAAR`
```prolog
f_sys_pf_set_cdaaar(X_Param, V_Param, FnResult) :-
        cl_caaar(X_Param, Set_cdr_Param),
        f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
        Set_cdr_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cdaaar, classof, claz_function),
   set_opv(sys_pf_set_cdaaar, compile_as, kw_function),
   set_opv(sys_pf_set_cdaaar, function, f_sys_pf_set_cdaaar),
   _Ignored7=sys_pf_set_cdaaar.
```
*/
/*
```cl
(defun %set-cddaar (x v) (set-cdr (cdaar x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cddaar', [x, v], ['set-cdr', [cdaar, x], v]]).

##### annotating... `SYS::%SET-CDDAAR`
```prolog
wl:lambda_def(defun, sys_pf_set_cddaar, f_sys_pf_set_cddaar, [sys_x, sys_v], [[sys_set_cdr, [cdaar, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CDDAAR`
```prolog
wl:arglist_info(sys_pf_set_cddaar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CDDAAR`
```prolog
wl: init_args(exact_only, sys_pf_set_cddaar).

```

### Compiled:  `SYS::%SET-CDDAAR`
```prolog
f_sys_pf_set_cddaar(X_Param, V_Param, FnResult) :-
        cl_cdaar(X_Param, Set_cdr_Param),
        f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
        Set_cdr_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cddaar, classof, claz_function),
   set_opv(sys_pf_set_cddaar, compile_as, kw_function),
   set_opv(sys_pf_set_cddaar, function, f_sys_pf_set_cddaar),
   _Ignored7=sys_pf_set_cddaar.
```
*/
/*
```cl
(defun %set-caadar (x v) (set-car (cadar x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-caadar', [x, v], ['set-car', [cadar, x], v]]).

##### annotating... `SYS::%SET-CAADAR`
```prolog
wl:lambda_def(defun, sys_pf_set_caadar, f_sys_pf_set_caadar, [sys_x, sys_v], [[sys_set_car, [cadar, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CAADAR`
```prolog
wl:arglist_info(sys_pf_set_caadar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CAADAR`
```prolog
wl: init_args(exact_only, sys_pf_set_caadar).

```

### Compiled:  `SYS::%SET-CAADAR`
```prolog
f_sys_pf_set_caadar(X_Param, V_Param, FnResult) :-
        cl_cadar(X_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_caadar, classof, claz_function),
   set_opv(sys_pf_set_caadar, compile_as, kw_function),
   set_opv(sys_pf_set_caadar, function, f_sys_pf_set_caadar),
   _Ignored7=sys_pf_set_caadar.
```
*/
/*
```cl
(defun %set-caddar (x v) (set-car (cddar x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-caddar', [x, v], ['set-car', [cddar, x], v]]).

##### annotating... `SYS::%SET-CADDAR`
```prolog
wl:lambda_def(defun, sys_pf_set_caddar, f_sys_pf_set_caddar, [sys_x, sys_v], [[sys_set_car, [cddar, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CADDAR`
```prolog
wl:arglist_info(sys_pf_set_caddar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CADDAR`
```prolog
wl: init_args(exact_only, sys_pf_set_caddar).

```

### Compiled:  `SYS::%SET-CADDAR`
```prolog
f_sys_pf_set_caddar(X_Param, V_Param, FnResult) :-
        cl_cddar(X_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_caddar, classof, claz_function),
   set_opv(sys_pf_set_caddar, compile_as, kw_function),
   set_opv(sys_pf_set_caddar, function, f_sys_pf_set_caddar),
   _Ignored7=sys_pf_set_caddar.
```
*/
/*
```cl
(defun %set-cdadar (x v) (set-cdr (cadar x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cdadar', [x, v], ['set-cdr', [cadar, x], v]]).

##### annotating... `SYS::%SET-CDADAR`
```prolog
wl:lambda_def(defun, sys_pf_set_cdadar, f_sys_pf_set_cdadar, [sys_x, sys_v], [[sys_set_cdr, [cadar, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CDADAR`
```prolog
wl:arglist_info(sys_pf_set_cdadar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CDADAR`
```prolog
wl: init_args(exact_only, sys_pf_set_cdadar).

```

### Compiled:  `SYS::%SET-CDADAR`
```prolog
f_sys_pf_set_cdadar(X_Param, V_Param, FnResult) :-
        cl_cadar(X_Param, Set_cdr_Param),
        f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
        Set_cdr_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cdadar, classof, claz_function),
   set_opv(sys_pf_set_cdadar, compile_as, kw_function),
   set_opv(sys_pf_set_cdadar, function, f_sys_pf_set_cdadar),
   _Ignored7=sys_pf_set_cdadar.
```
*/
/*
```cl
(defun %set-cdddar (x v) (set-cdr (cddar x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cdddar', [x, v], ['set-cdr', [cddar, x], v]]).

##### annotating... `SYS::%SET-CDDDAR`
```prolog
wl:lambda_def(defun, sys_pf_set_cdddar, f_sys_pf_set_cdddar, [sys_x, sys_v], [[sys_set_cdr, [cddar, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CDDDAR`
```prolog
wl:arglist_info(sys_pf_set_cdddar, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CDDDAR`
```prolog
wl: init_args(exact_only, sys_pf_set_cdddar).

```

### Compiled:  `SYS::%SET-CDDDAR`
```prolog
f_sys_pf_set_cdddar(X_Param, V_Param, FnResult) :-
        cl_cddar(X_Param, Set_cdr_Param),
        f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
        Set_cdr_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cdddar, classof, claz_function),
   set_opv(sys_pf_set_cdddar, compile_as, kw_function),
   set_opv(sys_pf_set_cdddar, function, f_sys_pf_set_cdddar),
   _Ignored7=sys_pf_set_cdddar.
```
*/
/*
```cl
(defun %set-caaadr (x v) (set-car (caadr x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-caaadr', [x, v], ['set-car', [caadr, x], v]]).

##### annotating... `SYS::%SET-CAAADR`
```prolog
wl:lambda_def(defun, sys_pf_set_caaadr, f_sys_pf_set_caaadr, [sys_x, sys_v], [[sys_set_car, [caadr, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CAAADR`
```prolog
wl:arglist_info(sys_pf_set_caaadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CAAADR`
```prolog
wl: init_args(exact_only, sys_pf_set_caaadr).

```

### Compiled:  `SYS::%SET-CAAADR`
```prolog
f_sys_pf_set_caaadr(X_Param, V_Param, FnResult) :-
        cl_caadr(X_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_caaadr, classof, claz_function),
   set_opv(sys_pf_set_caaadr, compile_as, kw_function),
   set_opv(sys_pf_set_caaadr, function, f_sys_pf_set_caaadr),
   _Ignored7=sys_pf_set_caaadr.
```
*/
/*
```cl
(defun %set-cadadr (x v) (set-car (cdadr x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cadadr', [x, v], ['set-car', [cdadr, x], v]]).

##### annotating... `SYS::%SET-CADADR`
```prolog
wl:lambda_def(defun, sys_pf_set_cadadr, f_sys_pf_set_cadadr, [sys_x, sys_v], [[sys_set_car, [cdadr, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CADADR`
```prolog
wl:arglist_info(sys_pf_set_cadadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CADADR`
```prolog
wl: init_args(exact_only, sys_pf_set_cadadr).

```

### Compiled:  `SYS::%SET-CADADR`
```prolog
f_sys_pf_set_cadadr(X_Param, V_Param, FnResult) :-
        cl_cdadr(X_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cadadr, classof, claz_function),
   set_opv(sys_pf_set_cadadr, compile_as, kw_function),
   set_opv(sys_pf_set_cadadr, function, f_sys_pf_set_cadadr),
   _Ignored7=sys_pf_set_cadadr.
```
*/
/*
```cl
(defun %set-cdaadr (x v) (set-cdr (caadr x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cdaadr', [x, v], ['set-cdr', [caadr, x], v]]).

##### annotating... `SYS::%SET-CDAADR`
```prolog
wl:lambda_def(defun, sys_pf_set_cdaadr, f_sys_pf_set_cdaadr, [sys_x, sys_v], [[sys_set_cdr, [caadr, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CDAADR`
```prolog
wl:arglist_info(sys_pf_set_cdaadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CDAADR`
```prolog
wl: init_args(exact_only, sys_pf_set_cdaadr).

```

### Compiled:  `SYS::%SET-CDAADR`
```prolog
f_sys_pf_set_cdaadr(X_Param, V_Param, FnResult) :-
        cl_caadr(X_Param, Set_cdr_Param),
        f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
        Set_cdr_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cdaadr, classof, claz_function),
   set_opv(sys_pf_set_cdaadr, compile_as, kw_function),
   set_opv(sys_pf_set_cdaadr, function, f_sys_pf_set_cdaadr),
   _Ignored7=sys_pf_set_cdaadr.
```
*/
/*
```cl
(defun %set-cddadr (x v) (set-cdr (cdadr x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cddadr', [x, v], ['set-cdr', [cdadr, x], v]]).

##### annotating... `SYS::%SET-CDDADR`
```prolog
wl:lambda_def(defun, sys_pf_set_cddadr, f_sys_pf_set_cddadr, [sys_x, sys_v], [[sys_set_cdr, [cdadr, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CDDADR`
```prolog
wl:arglist_info(sys_pf_set_cddadr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CDDADR`
```prolog
wl: init_args(exact_only, sys_pf_set_cddadr).

```

### Compiled:  `SYS::%SET-CDDADR`
```prolog
f_sys_pf_set_cddadr(X_Param, V_Param, FnResult) :-
        cl_cdadr(X_Param, Set_cdr_Param),
        f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
        Set_cdr_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cddadr, classof, claz_function),
   set_opv(sys_pf_set_cddadr, compile_as, kw_function),
   set_opv(sys_pf_set_cddadr, function, f_sys_pf_set_cddadr),
   _Ignored7=sys_pf_set_cddadr.
```
*/
/*
```cl
(defun %set-caaddr (x v) (set-car (caddr x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-caaddr', [x, v], ['set-car', [caddr, x], v]]).

##### annotating... `SYS::%SET-CAADDR`
```prolog
wl:lambda_def(defun, sys_pf_set_caaddr, f_sys_pf_set_caaddr, [sys_x, sys_v], [[sys_set_car, [caddr, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CAADDR`
```prolog
wl:arglist_info(sys_pf_set_caaddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CAADDR`
```prolog
wl: init_args(exact_only, sys_pf_set_caaddr).

```

### Compiled:  `SYS::%SET-CAADDR`
```prolog
f_sys_pf_set_caaddr(X_Param, V_Param, FnResult) :-
        cl_caddr(X_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_caaddr, classof, claz_function),
   set_opv(sys_pf_set_caaddr, compile_as, kw_function),
   set_opv(sys_pf_set_caaddr, function, f_sys_pf_set_caaddr),
   _Ignored7=sys_pf_set_caaddr.
```
*/
/*
```cl
(defun %set-cadddr (x v) (set-car (cdddr x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cadddr', [x, v], ['set-car', [cdddr, x], v]]).

##### annotating... `SYS::%SET-CADDDR`
```prolog
wl:lambda_def(defun, sys_pf_set_cadddr, f_sys_pf_set_cadddr, [sys_x, sys_v], [[sys_set_car, [cdddr, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CADDDR`
```prolog
wl:arglist_info(sys_pf_set_cadddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CADDDR`
```prolog
wl: init_args(exact_only, sys_pf_set_cadddr).

```

### Compiled:  `SYS::%SET-CADDDR`
```prolog
f_sys_pf_set_cadddr(X_Param, V_Param, FnResult) :-
        cl_cdddr(X_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cadddr, classof, claz_function),
   set_opv(sys_pf_set_cadddr, compile_as, kw_function),
   set_opv(sys_pf_set_cadddr, function, f_sys_pf_set_cadddr),
   _Ignored7=sys_pf_set_cadddr.
```
*/
/*
```cl
(defun %set-cdaddr (x v) (set-cdr (caddr x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cdaddr', [x, v], ['set-cdr', [caddr, x], v]]).

##### annotating... `SYS::%SET-CDADDR`
```prolog
wl:lambda_def(defun, sys_pf_set_cdaddr, f_sys_pf_set_cdaddr, [sys_x, sys_v], [[sys_set_cdr, [caddr, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CDADDR`
```prolog
wl:arglist_info(sys_pf_set_cdaddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CDADDR`
```prolog
wl: init_args(exact_only, sys_pf_set_cdaddr).

```

### Compiled:  `SYS::%SET-CDADDR`
```prolog
f_sys_pf_set_cdaddr(X_Param, V_Param, FnResult) :-
        cl_caddr(X_Param, Set_cdr_Param),
        f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
        Set_cdr_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cdaddr, classof, claz_function),
   set_opv(sys_pf_set_cdaddr, compile_as, kw_function),
   set_opv(sys_pf_set_cdaddr, function, f_sys_pf_set_cdaddr),
   _Ignored7=sys_pf_set_cdaddr.
```
*/
/*
```cl
(defun %set-cddddr (x v) (set-cdr (cdddr x) v))

```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-cddddr', [x, v], ['set-cdr', [cdddr, x], v]]).

##### annotating... `SYS::%SET-CDDDDR`
```prolog
wl:lambda_def(defun, sys_pf_set_cddddr, f_sys_pf_set_cddddr, [sys_x, sys_v], [[sys_set_cdr, [cdddr, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-CDDDDR`
```prolog
wl:arglist_info(sys_pf_set_cddddr, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-CDDDDR`
```prolog
wl: init_args(exact_only, sys_pf_set_cddddr).

```

### Compiled:  `SYS::%SET-CDDDDR`
```prolog
f_sys_pf_set_cddddr(X_Param, V_Param, FnResult) :-
        cl_cdddr(X_Param, Set_cdr_Param),
        f_sys_set_cdr(Set_cdr_Param, V_Param, Set_cdr_Ret),
        Set_cdr_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_cddddr, classof, claz_function),
   set_opv(sys_pf_set_cddddr, compile_as, kw_function),
   set_opv(sys_pf_set_cddddr, function, f_sys_pf_set_cddddr),
   _Ignored7=sys_pf_set_cddddr.
```
*/
/*
```cl
(defsetf car set-car)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, car, 'set-car']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(car, sys_setf_inverse, sys_set_car, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cdr set-cdr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdr, 'set-cdr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cdr, sys_setf_inverse, sys_set_cdr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf caar %set-caar)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caar, '%set-caar']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(caar, sys_setf_inverse, sys_pf_set_caar, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cadr %set-cadr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cadr, '%set-cadr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cadr, sys_setf_inverse, sys_pf_set_cadr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cdar %set-cdar)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdar, '%set-cdar']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cdar, sys_setf_inverse, sys_pf_set_cdar, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cddr %set-cddr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cddr, '%set-cddr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cddr, sys_setf_inverse, sys_pf_set_cddr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf caaar %set-caaar)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caaar, '%set-caaar']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(caaar, sys_setf_inverse, sys_pf_set_caaar, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cadar %set-cadar)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cadar, '%set-cadar']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cadar, sys_setf_inverse, sys_pf_set_cadar, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cdaar %set-cdaar)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdaar, '%set-cdaar']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cdaar, sys_setf_inverse, sys_pf_set_cdaar, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cddar %set-cddar)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cddar, '%set-cddar']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cddar, sys_setf_inverse, sys_pf_set_cddar, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf caadr %set-caadr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caadr, '%set-caadr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(caadr, sys_setf_inverse, sys_pf_set_caadr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf caddr %set-caddr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caddr, '%set-caddr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(caddr, sys_setf_inverse, sys_pf_set_caddr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cdadr %set-cdadr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdadr, '%set-cdadr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cdadr, sys_setf_inverse, sys_pf_set_cdadr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cdddr %set-cdddr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdddr, '%set-cdddr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cdddr, sys_setf_inverse, sys_pf_set_cdddr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf caaaar %set-caaaar)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caaaar, '%set-caaaar']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(caaaar, sys_setf_inverse, sys_pf_set_caaaar, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cadaar %set-cadaar)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cadaar, '%set-cadaar']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cadaar, sys_setf_inverse, sys_pf_set_cadaar, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cdaaar %set-cdaaar)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdaaar, '%set-cdaaar']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cdaaar, sys_setf_inverse, sys_pf_set_cdaaar, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cddaar %set-cddaar)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cddaar, '%set-cddaar']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cddaar, sys_setf_inverse, sys_pf_set_cddaar, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf caadar %set-caadar)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caadar, '%set-caadar']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(caadar, sys_setf_inverse, sys_pf_set_caadar, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf caddar %set-caddar)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caddar, '%set-caddar']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(caddar, sys_setf_inverse, sys_pf_set_caddar, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cdadar %set-cdadar)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdadar, '%set-cdadar']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cdadar, sys_setf_inverse, sys_pf_set_cdadar, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cdddar %set-cdddar)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdddar, '%set-cdddar']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cdddar, sys_setf_inverse, sys_pf_set_cdddar, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf caaadr %set-caaadr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caaadr, '%set-caaadr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(caaadr, sys_setf_inverse, sys_pf_set_caaadr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cadadr %set-cadadr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cadadr, '%set-cadadr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cadadr, sys_setf_inverse, sys_pf_set_cadadr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cdaadr %set-cdaadr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdaadr, '%set-cdaadr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cdaadr, sys_setf_inverse, sys_pf_set_cdaadr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cddadr %set-cddadr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cddadr, '%set-cddadr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cddadr, sys_setf_inverse, sys_pf_set_cddadr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf caaddr %set-caaddr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, caaddr, '%set-caaddr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(caaddr, sys_setf_inverse, sys_pf_set_caaddr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cadddr %set-cadddr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cadddr, '%set-cadddr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cadddr, sys_setf_inverse, sys_pf_set_cadddr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cdaddr %set-cdaddr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cdaddr, '%set-cdaddr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cdaddr, sys_setf_inverse, sys_pf_set_cdaddr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf cddddr %set-cddddr)

```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, cddddr, '%set-cddddr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(cddddr, sys_setf_inverse, sys_pf_set_cddddr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf first set-car)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, first, 'set-car']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(first, sys_setf_inverse, sys_set_car, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf second %set-cadr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, second, '%set-cadr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(second, sys_setf_inverse, sys_pf_set_cadr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf third %set-caddr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, third, '%set-caddr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(third, sys_setf_inverse, sys_pf_set_caddr, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf fourth %set-cadddr)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, fourth, '%set-cadddr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(fourth, sys_setf_inverse, sys_pf_set_cadddr, _Ignored7),
           _Ignored7).
/*
```cl
(defun %set-fifth (x v) (set-car (cddddr x) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defun, '%set-fifth', [x, v], ['set-car', [cddddr, x], v]]).

##### annotating... `SYS::%SET-FIFTH`
```prolog
wl:lambda_def(defun, sys_pf_set_fifth, f_sys_pf_set_fifth, [sys_x, sys_v], [[sys_set_car, [cddddr, sys_x], sys_v]]).
```

##### annotating... `SYS::%SET-FIFTH`
```prolog
wl:arglist_info(sys_pf_set_fifth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-FIFTH`
```prolog
wl: init_args(exact_only, sys_pf_set_fifth).

```

### Compiled:  `SYS::%SET-FIFTH`
```prolog
f_sys_pf_set_fifth(X_Param, V_Param, FnResult) :-
        cl_cddddr(X_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_fifth, classof, claz_function),
   set_opv(sys_pf_set_fifth, compile_as, kw_function),
   set_opv(sys_pf_set_fifth, function, f_sys_pf_set_fifth),
   _Ignored7=sys_pf_set_fifth.
```
*/
/*
```cl
(defsetf fifth %set-fifth)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, fifth, '%set-fifth']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(fifth, sys_setf_inverse, sys_pf_set_fifth, _Ignored7),
           _Ignored7).
/*
```cl
(defun %set-sixth (x v) (set-car (cdr (cddddr x)) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defun,
                            '%set-sixth',
                            [x, v],
                            ['set-car', [cdr, [cddddr, x]], v]
                          ]).

##### annotating... `SYS::%SET-SIXTH`
```prolog
wl:lambda_def(defun, sys_pf_set_sixth, f_sys_pf_set_sixth, [sys_x, sys_v], [[sys_set_car, [cdr, [cddddr, sys_x]], sys_v]]).
```

##### annotating... `SYS::%SET-SIXTH`
```prolog
wl:arglist_info(sys_pf_set_sixth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-SIXTH`
```prolog
wl: init_args(exact_only, sys_pf_set_sixth).

```

### Compiled:  `SYS::%SET-SIXTH`
```prolog
f_sys_pf_set_sixth(X_Param, V_Param, FnResult) :-
        cl_cddddr(X_Param, Cdr_Param),
        cl_cdr(Cdr_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_sixth, classof, claz_function),
   set_opv(sys_pf_set_sixth, compile_as, kw_function),
   set_opv(sys_pf_set_sixth, function, f_sys_pf_set_sixth),
   _Ignored7=sys_pf_set_sixth.
```
*/
/*
```cl
(defsetf sixth %set-sixth)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, sixth, '%set-sixth']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(sixth, sys_setf_inverse, sys_pf_set_sixth, _Ignored7),
           _Ignored7).
/*
```cl
(defun %set-seventh (x v) (set-car (cddr (cddddr x)) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defun,
                            '%set-seventh',
                            [x, v],
                            ['set-car', [cddr, [cddddr, x]], v]
                          ]).

##### annotating... `SYS::%SET-SEVENTH`
```prolog
wl:lambda_def(defun, sys_pf_set_seventh, f_sys_pf_set_seventh, [sys_x, sys_v], [[sys_set_car, [cddr, [cddddr, sys_x]], sys_v]]).
```

##### annotating... `SYS::%SET-SEVENTH`
```prolog
wl:arglist_info(sys_pf_set_seventh, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-SEVENTH`
```prolog
wl: init_args(exact_only, sys_pf_set_seventh).

```

### Compiled:  `SYS::%SET-SEVENTH`
```prolog
f_sys_pf_set_seventh(X_Param, V_Param, FnResult) :-
        cl_cddddr(X_Param, Cddr_Param),
        cl_cddr(Cddr_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_seventh, classof, claz_function),
   set_opv(sys_pf_set_seventh, compile_as, kw_function),
   set_opv(sys_pf_set_seventh, function, f_sys_pf_set_seventh),
   _Ignored7=sys_pf_set_seventh.
```
*/
/*
```cl
(defsetf seventh %set-seventh)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, seventh, '%set-seventh']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(seventh, sys_setf_inverse, sys_pf_set_seventh, _Ignored7),
           _Ignored7).
/*
```cl
(defun %set-eighth (x v) (set-car (cdddr (cddddr x)) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defun,
                            '%set-eighth',
                            [x, v],
                            ['set-car', [cdddr, [cddddr, x]], v]
                          ]).

##### annotating... `SYS::%SET-EIGHTH`
```prolog
wl:lambda_def(defun, sys_pf_set_eighth, f_sys_pf_set_eighth, [sys_x, sys_v], [[sys_set_car, [cdddr, [cddddr, sys_x]], sys_v]]).
```

##### annotating... `SYS::%SET-EIGHTH`
```prolog
wl:arglist_info(sys_pf_set_eighth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-EIGHTH`
```prolog
wl: init_args(exact_only, sys_pf_set_eighth).

```

### Compiled:  `SYS::%SET-EIGHTH`
```prolog
f_sys_pf_set_eighth(X_Param, V_Param, FnResult) :-
        cl_cddddr(X_Param, Cdddr_Param),
        cl_cdddr(Cdddr_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_eighth, classof, claz_function),
   set_opv(sys_pf_set_eighth, compile_as, kw_function),
   set_opv(sys_pf_set_eighth, function, f_sys_pf_set_eighth),
   _Ignored7=sys_pf_set_eighth.
```
*/
/*
```cl
(defsetf eighth %set-eighth)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, eighth, '%set-eighth']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(eighth, sys_setf_inverse, sys_pf_set_eighth, _Ignored7),
           _Ignored7).
/*
```cl
(defun %set-ninth (x v) (set-car (cddddr (cddddr x)) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defun,
                            '%set-ninth',
                            [x, v],
                            ['set-car', [cddddr, [cddddr, x]], v]
                          ]).

##### annotating... `SYS::%SET-NINTH`
```prolog
wl:lambda_def(defun, sys_pf_set_ninth, f_sys_pf_set_ninth, [sys_x, sys_v], [[sys_set_car, [cddddr, [cddddr, sys_x]], sys_v]]).
```

##### annotating... `SYS::%SET-NINTH`
```prolog
wl:arglist_info(sys_pf_set_ninth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-NINTH`
```prolog
wl: init_args(exact_only, sys_pf_set_ninth).

```

### Compiled:  `SYS::%SET-NINTH`
```prolog
f_sys_pf_set_ninth(X_Param, V_Param, FnResult) :-
        cl_cddddr(X_Param, Cddddr_Param),
        cl_cddddr(Cddddr_Param, Set_car_Param),
        f_sys_set_car(Set_car_Param, V_Param, Set_car_Ret),
        Set_car_Ret=FnResult.
```
/*
```cl
:- set_opv(f_sys_pf_set_ninth, classof, claz_function),
   set_opv(sys_pf_set_ninth, compile_as, kw_function),
   set_opv(sys_pf_set_ninth, function, f_sys_pf_set_ninth),
   _Ignored7=sys_pf_set_ninth.
```
*/
/*
```cl
(defsetf ninth %set-ninth)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, ninth, '%set-ninth']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(ninth, sys_setf_inverse, sys_pf_set_ninth, _Ignored7),
           _Ignored7).
/*
```cl
(defun %set-tenth (x v) (set-car (cdr (cddddr (cddddr x))) v))
```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defun,
                            '%set-tenth',
                            [x, v],
                            ['set-car', [cdr, [cddddr, [cddddr, x]]], v]
                          ]).

##### annotating... `SYS::%SET-TENTH`
```prolog
wl:lambda_def(defun, sys_pf_set_tenth, f_sys_pf_set_tenth, [sys_x, sys_v], [[sys_set_car, [cdr, [cddddr, [cddddr, sys_x]]], sys_v]]).
```

##### annotating... `SYS::%SET-TENTH`
```prolog
wl:arglist_info(sys_pf_set_tenth, [sys_x, sys_v], [X_Param, V_Param], arginfo{all:[sys_x, sys_v], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_x, sys_v], opt:0, req:[sys_x, sys_v], rest:0, sublists:0, whole:0}).
```

##### annotating... `SYS::%SET-TENTH`
```prolog
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
```
/*
```cl
:- set_opv(f_sys_pf_set_tenth, classof, claz_function),
   set_opv(sys_pf_set_tenth, compile_as, kw_function),
   set_opv(sys_pf_set_tenth, function, f_sys_pf_set_tenth),
   _Ignored7=sys_pf_set_tenth.
```
*/
/*
```cl
(defsetf tenth %set-tenth)

```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, tenth, '%set-tenth']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(tenth, sys_setf_inverse, sys_pf_set_tenth, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf rest set-cdr)
;;Redefined in extensible-sequences-base.lisp
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, rest, 'set-cdr']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(rest, sys_setf_inverse, sys_set_cdr, _Ignored7),
           _Ignored7).
/*
```cl
;Redefined in extensible-sequences-base.lisp
```
*/
/*
```cl
(defsetf elt %set-elt)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, elt, '%set-elt']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(elt, sys_setf_inverse, sys_pf_set_elt, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf nth %set-nth)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, nth, '%set-nth']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(nth, sys_setf_inverse, sys_pf_set_nth, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf svref svset)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, svref, svset]).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(svref, sys_setf_inverse, sys_svset, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf fill-pointer %set-fill-pointer)
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defsetf, 'fill-pointer', '%set-fill-pointer']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(fill_pointer,
                     sys_setf_inverse,
                     sys_pf_set_fill_pointer,
                     _Ignored7),
           _Ignored7).
/*
```cl
(defsetf subseq %set-subseq)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, subseq, '%set-subseq']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(subseq, sys_setf_inverse, sys_pf_set_subseq, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf symbol-value set)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, 'symbol-value', set]).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(symbol_value, sys_setf_inverse, set, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf symbol-function %set-symbol-function)
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defsetf, 'symbol-function', '%set-symbol-function']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(symbol_function,
                     sys_setf_inverse,
                     sys_pf_set_symbol_function,
                     _Ignored7),
           _Ignored7).
/*
```cl
(defsetf symbol-plist %set-symbol-plist)
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defsetf, 'symbol-plist', '%set-symbol-plist']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(symbol_plist,
                     sys_setf_inverse,
                     sys_pf_set_symbol_plist,
                     _Ignored7),
           _Ignored7).
/*
```cl
(defsetf get put)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, get, put]).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(get, sys_setf_inverse, sys_put, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf gethash puthash)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, gethash, puthash]).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(gethash, sys_setf_inverse, sys_puthash, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf char set-char)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, char, 'set-char']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(char, sys_setf_inverse, sys_set_char, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf schar set-schar)
```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, schar, 'set-schar']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(schar, sys_setf_inverse, sys_set_schar, _Ignored7),
           _Ignored7).
/*
```cl
(defsetf logical-pathname-translations %set-logical-pathname-translations)
```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defsetf,
                            'logical-pathname-translations',
                            '%set-logical-pathname-translations'
                          ]).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(logical_pathname_translations,
                     sys_setf_inverse,
                     sys_pf_set_logical_pathname_translations,
                     _Ignored7),
           _Ignored7).
/*
```cl
(defsetf readtable-case %set-readtable-case)

```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defsetf, 'readtable-case', '%set-readtable-case']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(readtable_case,
                     sys_setf_inverse,
                     sys_pf_set_readtable_case,
                     _Ignored7),
           _Ignored7).
/*
```cl
(defsetf function-info %set-function-info)

```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [defsetf, 'function-info', '%set-function-info']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(sys_function_info,
                     sys_setf_inverse,
                     sys_pf_set_function_info,
                     _Ignored7),
           _Ignored7).
/*
```cl
(defsetf stream-external-format %set-stream-external-format)

```
*/
:- lisp_compile_to_prolog(pkg_sys,

                          [ defsetf,
                            'stream-external-format',
                            '%set-stream-external-format'
                          ]).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(stream_external_format,
                     sys_setf_inverse,
                     sys_pf_set_stream_external_format,
                     _Ignored7),
           _Ignored7).
/*
```cl
(defsetf structure-ref structure-set)

```
*/
:- lisp_compile_to_prolog(pkg_sys, [defsetf, 'structure-ref', 'structure-set']).
:- do_when([kw_load_toplevel, kw_compile_toplevel, kw_execute],
           f_sys_put(sys_structure_ref,
                     sys_setf_inverse,
                     sys_structure_set,
                     _Ignored7),
           _Ignored7).
/*
```cl
'(load "wam-cl-init2")
```
*/
:- lisp_compile_to_prolog(pkg_sys, [quote, [load, '$STRING'("wam-cl-init2")]]).
/*
```cl
'(load "wam-cl-init3")
```
*/
:- lisp_compile_to_prolog(pkg_sys, [quote, [load, '$STRING'("wam-cl-init3")]]).
/*
```cl
'(write-line " WAM CommonLisp ")
```
*/
:- lisp_compile_to_prolog(pkg_sys,
                          [quote, ['write-line', '$STRING'(" WAM CommonLisp ")]]).
/*
```cl
'(read-eval-print-loop)



#|

;; (when (eq (symbol-package sym) p) (format t "fmt90_x1 fmt90_x2 fmt90_x3 "'(read-eval-print-loop)\r\n\r\n \r\n\r\n#|\r\n\r\n;; (when (eq (symbol-package sym) p) (format t \"~a ~a ~a ~a~%\" ......)) \r\n\r\n".
```
*/
:- lisp_compile_to_prolog(pkg_sys, [quote, ['read-eval-print-loop']]).
/*
```cl
; (when (eq (symbol-package sym) p) (format t "fmt90_x1 fmt90_x2 fmt90_x3 "; (when (eq (symbol-package sym) p) (format t \"~a ~a ~a ~a~%\" ......)) ".
```
*/
/*
```cl


;; (when (eq (symbol-package sym) p) (format t "fmt90_x1 fmt90_x2 fmt90_x3 "\r\n\r\n;; (when (eq (symbol-package sym) p) (format t \"~a ~a ~a ~a~%\" ......)) \r\n\r\n(defun packagesyminfo (p0)\r\n (let ((p (find-package p0)))\r\n (do-all-symbols (sym)    \r\n  (when (eq (symbol-package sym) p)\r\n   (format t \"symbolinfo('~s','~s').~%\"\r\n    sn (package-name (symbol-package sym))\r\n    (constantp sym)\r\n    (special-operator-p sym)\r\n    (symbol-plist sym)\r\n    sn (symbol-package sym)\r\n    (if (boundp sym) (symbol-value sym))\r\n    (if (fboundp sym) (type-of (symbol-function sym)))\r\n    (fboundp sym)))))))\r\n(packagesyminfo :cl)\r\n\r\n\r\n\r\n\r\n\r\n(defun packagesyminfo (p0)\r\n (let ((p (find-package p0)))\r\n (do-all-symbols (sym)    \r\n  (when (eq (symbol-package sym) p)\r\n   (format t \"symbol_package('~a','~a').~%\"\r\n    sn (package-name (symbol-package sym)))))))\r\n(packagesyminfo :cl)\r\n\r\n\r\n".
```
*/
/*
```cl
(in-package "CL-USER")



;; Test macro
```
*/
:- lisp_compile_to_prolog(pkg_user, ['in-package', '$STRING'("CL-USER")]).
:- cl_in_package('$ARRAY'([*], claz_base_character, "CL-USER"), _Ignored).
/*
```cl
; Test macro
```
*/
/*
```cl
(defmacro is (eqf expected actual)
  (let ((a (gensym "a")) (b (gensym "b")))
    `(let ((,a ,expected) (,b ,actual))
       (if (,eqf ,a ,b)
         (format t "OK: fmt90_x1 is fmt90_x2 to fmt90_x3"(defmacro is (eqf expected actual)\n  (let ((a (gensym \"a\")) (b (gensym \"b\")))\n    `(let ((,a ,expected) (,b ,actual))\n       (if (,eqf ,a ,b)\n         (format t \"OK: ~a is ~a to ~a~%\" ',expected ',eqf ',actual)\n         (progn\n           (format t \"FAILED: when matching ~a and ~a~%\" ,a ,b)\n\t   #+WAM-CL (prolog-inline \"trace\")\n\t   #+CLISP (BREAK)\n\t   #+CLISP (quit 1))\n         ))))\n\n\n".
```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defmacro,
                            (is),
                            [eqf, expected, actual],

                            [ let,

                              [ [a, [gensym, '$STRING'("a")]],
                                [b, [gensym, '$STRING'("b")]]
                              ],

                              [ '#BQ',

                                [ let,

                                  [ [['#COMMA', a], ['#COMMA', expected]],
                                    [['#COMMA', b], ['#COMMA', actual]]
                                  ],

                                  [ if,
                                    [['#COMMA', eqf], ['#COMMA', a], ['#COMMA', b]],

                                    [ format,
                                      t,
                                      '$STRING'("OK: ~a is ~a to ~a~%"),
                                      [quote, ['#COMMA', expected]],
                                      [quote, ['#COMMA', eqf]],
                                      [quote, ['#COMMA', actual]]
                                    ],

                                    [ progn,

                                      [ format,
                                        t,
                                        '$STRING'("FAILED: when matching ~a and ~a~%"),
                                        ['#COMMA', a],
                                        ['#COMMA', b]
                                      ],
                                      ['prolog-inline', '$STRING'("trace")]
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]).

##### annotating... `U::IS`
```prolog
wl:lambda_def(defmacro, u_is, f_u_is, [u_eqf, u_expected, u_actual], [progn, [let, [[u_a, [gensym, '$ARRAY'([*], claz_base_character, "a")]], [u_b, [gensym, '$ARRAY'([*], claz_base_character, "b")]]], ['#BQ', [let, [[['#COMMA', u_a], ['#COMMA', u_expected]], [['#COMMA', u_b], ['#COMMA', u_actual]]], [if, [['#COMMA', u_eqf], ['#COMMA', u_a], ['#COMMA', u_b]], [format, t, '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"), [quote, ['#COMMA', u_expected]], [quote, ['#COMMA', u_eqf]], [quote, ['#COMMA', u_actual]]], [progn, [format, t, '$ARRAY'([*], claz_base_character, "FAILED: when matching ~a and ~a~%"), ['#COMMA', u_a], ['#COMMA', u_b]], [sys_prolog_inline, '$ARRAY'([*], claz_base_character, "trace")]]]]]]]).
```

##### annotating... `U::IS`
```prolog
wl:arglist_info(u_is, [u_eqf, u_expected, u_actual], [Eqf_Param, Expected_Param, Actual_Param], arginfo{all:[u_eqf, u_expected, u_actual], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_eqf, u_expected, u_actual], opt:0, req:[u_eqf, u_expected, u_actual], rest:0, sublists:0, whole:0}).
```

##### annotating... `U::IS`
```prolog
wl: init_args(exact_only, u_is).

```

### Compiled:  `U::IS`
```prolog
f_u_is(Eqf_Param, Expected_Param, Actual_Param, FnResult) :-
        TLEnv=[bv(u_eqf, Eqf_Param), bv(u_expected, Expected_Param), bv(u_actual, Actual_Param)],
        cl_gensym('$ARRAY'([*], claz_base_character, "a"), A_Init),
        cl_gensym('$ARRAY'([*], claz_base_character, "b"), B_Init),
        LEnv=[[bv(u_a, A_Init), bv(u_b, B_Init)]|TLEnv],
        get_var(LEnv, u_a, A_Get27),
        get_var(LEnv, u_b, B_Get28),
        [let, [[A_Get27, Expected_Param], [B_Get28, Actual_Param]], [if, [Eqf_Param, A_Get27, B_Get28], [format, t, '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"), [quote, Expected_Param], [quote, Eqf_Param], [quote, Actual_Param]], [progn, [format, t, '$ARRAY'([*], claz_base_character, "FAILED: when matching ~a and ~a~%"), A_Get27, B_Get28], [sys_prolog_inline, '$ARRAY'([*], claz_base_character, "trace")]]]]=MFResult,
        cl_eval(MFResult, FnResult).
```
/*
```cl
:- set_opv(f_u_is, classof, claz_macro),
   set_opv(u_is, compile_as, kw_operator),
   set_opv(u_is, function, f_u_is),
   DefMacroResult=u_is.
```
*/
/*
```cl
(write-line "Running smoke test!")

; (progn (prolog-inline "rtrace") (is eq 1 1))
```
*/
:- lisp_compile_to_prolog(pkg_user,
                          ['write-line', '$STRING'("Running smoke test!")]).
:- cl_write_line('$ARRAY'([*], claz_base_character, "Running smoke test!"),
                 _Ignored).
/*
```cl
 (progn (prolog-inline "rtrace") (is eq 1 1))
```
*/
/*
```cl
(is eq 1 1)
```
*/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 1, 1]).
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
   ;   get_var(LEnv, a11, A11_Get13),
       get_var(LEnv, b11, B11_Get14),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A11_Get13,
                   B11_Get14
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(is equal (list 1 'a 'b) (cons 1 '(a b)))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ (is),
                            equal,
                            [list, 1, [quote, a], [quote, b]],
                            [cons, 1, [quote, [a, b]]]
                          ]).
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
   ;   get_var(LEnv, a21, A21_Get15),
       get_var(LEnv, b21, B21_Get16),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A21_Get15,
                   B21_Get16
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(is eq 2 (if nil 1 2))

```
*/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 2, [if, [], 1, 2]]).
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
   ;   get_var(LEnv, a31, A31_Get16),
       get_var(LEnv, b31, B31_Get17),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A31_Get16,
                   B31_Get17
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(is eq t (keywordp :k))

```
*/
:- lisp_compile_to_prolog(pkg_user, [is, eq, t, [keywordp, ':k']]).
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
   ;   get_var(LEnv, a41, A41_Get14),
       get_var(LEnv, b41, B41_Get15),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A41_Get14,
                   B41_Get15
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(is eq 10 (if t 10 20))

```
*/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 10, [if, t, 10, 20]]).
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
   ;   get_var(LEnv, a51, A51_Get16),
       get_var(LEnv, b51, B51_Get17),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A51_Get16,
                   B51_Get17
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(is eq t (stringp "abc"))

;;  "FAI "this has ben fix" LED: when matching fmt90_x1 and fmt90_x2"(is eq t (stringp \"abc\"))\n\n;;  \"FAI \"this has ben fix\" LED: when matching ~a and ~a~%\", ['$CHAR'(b), '$CHAR'(c)], \"bc\", t).\n".
```
*/
:- lisp_compile_to_prolog(pkg_user, [is, eq, t, [stringp, '$STRING'("abc")]]).
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
   ;   get_var(LEnv, a61, A61_Get14),
       get_var(LEnv, b61, B61_Get15),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A61_Get14,
                   B61_Get15
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
;  "FAI "this has ben fix" LED: when matching fmt90_x1 and fmt90_x2";  \"FAI \"this has ben fix\" LED: when matching ~a and ~a~%\", ['$CHAR'(b), '$CHAR'(c)], \"bc\", t).".
```
*/
/*
```cl
(is equal (subseq "abc" 1) "bc")

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ (is),
                            equal,
                            [subseq, '$STRING'("abc"), 1],
                            '$STRING'("bc")
                          ]).
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
   ;   get_var(LEnv, a71, A71_Get14),
       get_var(LEnv, b71, B71_Get15),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A71_Get14,
                   B71_Get15
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(is eq 1 (if t 1 2))
```
*/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 1, [if, t, 1, 2]]).
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
   ;   get_var(LEnv, a81, A81_Get16),
       get_var(LEnv, b81, B81_Get17),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A81_Get16,
                   B81_Get17
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(is eq 2 (if nil 1 2))

```
*/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 2, [if, [], 1, 2]]).
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
   ;   get_var(LEnv, a91, A91_Get16),
       get_var(LEnv, b91, B91_Get17),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A91_Get16,
                   B91_Get17
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(defun fib (n)
  (if (> n 1)
    (+ (fib (- n 1))
       (fib (- n 2)))
    1))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defun,
                            fib,
                            [n],
                            [if, [>, n, 1], [+, [fib, [-, n, 1]], [fib, [-, n, 2]]], 1]
                          ]).

##### annotating... `U::FIB`
```prolog
wl:lambda_def(defun, u_fib, f_u_fib, [n], [[if, [>, n, 1], [+, [u_fib, [-, n, 1]], [u_fib, [-, n, 2]]], 1]]).
```

##### annotating... `U::FIB`
```prolog
wl:arglist_info(u_fib, [n], [N_Param], arginfo{all:[n], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[n], opt:0, req:[n], rest:0, sublists:0, whole:0}).
```

##### annotating... `U::FIB`
```prolog
wl: init_args(exact_only, u_fib).

```

### Compiled:  `U::FIB`
```prolog
f_u_fib(N_Param, FnResult) :-
        (   N_Param>1
        ->  -(N_Param, 1, Fib_Param),
            f_u_fib(Fib_Param, Fib_Ret),
            -(N_Param, 2, Fib_Param23),
            f_u_fib(Fib_Param23, Fib_Ret25),
            +(Fib_Ret, Fib_Ret25, TrueResult),
            FnResult=TrueResult
        ;   FnResult=1
        ).
```
/*
```cl
:- set_opv(f_u_fib, classof, claz_function),
   set_opv(u_fib, compile_as, kw_function),
   set_opv(u_fib, function, f_u_fib),
   DefunResult=u_fib.
```
*/
/*
```cl
(disassemble #'fib)


```
*/
:- lisp_compile_to_prolog(pkg_user, [disassemble, function(fib)]).
:- cl_disassemble(function(u_fib), _Ignored).
/*
```cl
(is eql 89 (fib 10))



```
*/
:- lisp_compile_to_prolog(pkg_user, [is, eql, 89, [fib, 10]]).
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
   ;   get_var(LEnv, a101, A101_Get14),
       get_var(LEnv, b101, B101_Get15),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A101_Get14,
                   B101_Get15
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(defun accum (r) (if (= 0 r) (list 0) (cons r (accum (- r 1)))))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defun,
                            accum,
                            [r],
                            [if, [=, 0, r], [list, 0], [cons, r, [accum, [-, r, 1]]]]
                          ]).

##### annotating... `U::ACCUM`
```prolog
wl:lambda_def(defun, u_accum, f_u_accum, [u_r], [[if, [=, 0, u_r], [list, 0], [cons, u_r, [u_accum, [-, u_r, 1]]]]]).
```

##### annotating... `U::ACCUM`
```prolog
wl:arglist_info(u_accum, [u_r], [R_Param], arginfo{all:[u_r], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_r], opt:0, req:[u_r], rest:0, sublists:0, whole:0}).
```

##### annotating... `U::ACCUM`
```prolog
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
```
/*
```cl
:- set_opv(f_u_accum, classof, claz_function),
   set_opv(u_accum, compile_as, kw_function),
   set_opv(u_accum, function, f_u_accum),
   DefunResult=u_accum.
```
*/
/*
```cl
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
```
*/
:- lisp_compile_to_prolog(pkg_user, [disassemble, function(accum)]).
:- cl_disassemble(function(u_accum), _Ignored).
/*
```cl
 DISASSEMBLY FOR:f_u_accum
:- dynamic f_u_accum/2.

f_u_accum(A, G) :-
        (   0=:=A
        ->  G=[0]
        ;   C is A - 1,
            f_u_accum(C, D),
            G=[A|D]
        ).

```
*/
/*
```cl
(is equal (list 4 3 2 1 0) (accum 4))

```
*/
:- lisp_compile_to_prolog(pkg_user, [is, equal, [list, 4, 3, 2, 1, 0], [accum, 4]]).
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
   ;   get_var(LEnv, a12, A12_Get15),
       get_var(LEnv, b12, B12_Get16),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A12_Get15,
                   B12_Get16
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(defmacro defwrap (name) `(defun ,name () 1))
;;; :- ensure_loaded('sanity-test.lisp.trans.pl').
```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defmacro,
                            defwrap,
                            [name],
                            ['#BQ', [defun, ['#COMMA', name], [], 1]]
                          ]).

##### annotating... `U::DEFWRAP`
```prolog
wl:lambda_def(defmacro, u_defwrap, f_u_defwrap, [sys_name], [progn, ['#BQ', [defun, ['#COMMA', sys_name], [], 1]]]).
```

##### annotating... `U::DEFWRAP`
```prolog
wl:arglist_info(u_defwrap, [sys_name], [Name_Param], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:[sys_name], rest:0, sublists:0, whole:0}).
```

##### annotating... `U::DEFWRAP`
```prolog
wl: init_args(exact_only, u_defwrap).

```

### Compiled:  `U::DEFWRAP`
```prolog
f_u_defwrap(Name_Param, FnResult) :-
        [defun, Name_Param, [], 1]=MFResult,
        cl_eval(MFResult, FnResult).
```
/*
```cl
:- set_opv(f_u_defwrap, classof, claz_macro),
   set_opv(u_defwrap, compile_as, kw_operator),
   set_opv(u_defwrap, function, f_u_defwrap),
   DefMacroResult=u_defwrap.
```
*/
/*
```cl
;; :- ensure_loaded('sanity-test.lisp.trans.pl').
```
*/
/*
```cl
(defwrap foo)
```
*/
:- lisp_compile_to_prolog(pkg_user, [defwrap, foo]).

##### annotating... `U::FOO`
```prolog
wl:lambda_def(defun, u_foo, f_u_foo, [], [1]).
```

##### annotating... `U::FOO`
```prolog
wl:arglist_info(u_foo, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
```

##### annotating... `U::FOO`
```prolog
wl: init_args(exact_only, u_foo).

```

### Compiled:  `U::FOO`
```prolog
f_u_foo(FnResult) :-
        Env=[],
        1=FnResult.
```
/*
```cl
:- set_opv(f_u_foo, classof, claz_function),
   set_opv(u_foo, compile_as, kw_function),
   set_opv(u_foo, function, f_u_foo),
   DefunResult=u_foo.
```
*/
/*
```cl
(is eq 1 (foo))
```
*/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 1, [foo]]).
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
   ;   get_var(LEnv, a13, A13_Get14),
       get_var(LEnv, b13, B13_Get15),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A13_Get14,
                   B13_Get15
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(is equal (macroexpand-1 '(defwrap foo)) '(defun foo nil 1))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ (is),
                            equal,
                            ['macroexpand-1', [quote, [defwrap, foo]]],
                            [quote, [defun, foo, [], 1]]
                          ]).
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
   ;   get_var(LEnv, a14, A14_Get14),
       get_var(LEnv, b14, B14_Get15),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A14_Get14,
                   B14_Get15
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(write-line "PASSED")

```
*/
:- lisp_compile_to_prolog(pkg_user, ['write-line', '$STRING'("PASSED")]).
:- cl_write_line('$ARRAY'([*], claz_base_character, "PASSED"), _Ignored).
/*
```cl
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

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defun,
                            fifteen,
                            [],

                            [ let,
                              [val],

                              [ tagbody,
                                [setq, val, 1],
                                [go, 'point-a'],
                                [incf, val, 16],
                                'point-c',
                                [incf, val, 4],
                                [go, 'point-b'],
                                [incf, val, 32],
                                'point-a',
                                'point-u',
                                [incf, val, 2],
                                [go, 'point-c'],
                                [incf, val, 64],
                                'point-b',
                                [incf, val, 8]
                              ],
                              val
                            ]
                          ]).

##### annotating... `U::FIFTEEN`
```prolog
wl:lambda_def(defun, u_fifteen, f_u_fifteen, [], [[let, [u_val], [tagbody, [setq, u_val, 1], [go, u_point_a], [incf, u_val, 16], u_point_c, [incf, u_val, 4], [go, u_point_b], [incf, u_val, 32], u_point_a, u_point_u, [incf, u_val, 2], [go, u_point_c], [incf, u_val, 64], u_point_b, [incf, u_val, 8]], u_val]]).
```

##### annotating... `U::FIFTEEN`
```prolog
wl:arglist_info(u_fifteen, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
```

##### annotating... `U::FIFTEEN`
```prolog
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
                                         (get_var(Setf_Env, u_val, Get_var_Ret), +(Get_var_Ret, 4, CAR50), set_place(Setf_Env, setf, [value, u_val], [CAR50], Set_place_Ret), goto(u_point_b, Setf_Env))),
                                    addr(addr_tagbody_2_u_point_a,
                                         u_point_a,
                                         '$used',
                                         Setf_Env,
                                         (push_label(u_point_u), get_var(Setf_Env, u_val, Val_Get22), +(Val_Get22, 2, CAR26), set_place(Setf_Env, setf, [value, u_val], [CAR26], Setf_R24), goto(u_point_c, Setf_Env))),
                                    addr(addr_tagbody_2_u_point_u,
                                         u_point_u,
                                         '$unused',
                                         Setf_Env,
                                         (get_var(Setf_Env, u_val, Val_Get29), +(Val_Get29, 2, CAR33), set_place(Setf_Env, setf, [value, u_val], [CAR33], Setf_R31), goto(u_point_c, Setf_Env))),
                                    addr(addr_tagbody_2_u_point_b,
                                         u_point_b,
                                         '$used',
                                         _GEnv37,
                                         (get_var(_GEnv37, u_val, Val_Get36), +(Val_Get36, 8, CAR40), set_place(_GEnv37, setf, [value, u_val], [CAR40], _GORES27)))
                                  ]),
                  get_var(GoEnv, u_val, Val_Get44)
                ),
                Val_Get44=FnResult
              ),
              block_exit(u_fifteen, FnResult),
              true).
```
/*
```cl
:- set_opv(f_u_fifteen, classof, claz_function),
   set_opv(u_fifteen, compile_as, kw_function),
   set_opv(u_fifteen, function, f_u_fifteen),
   DefunResult=u_fifteen.
```
*/
/*
```cl
; unused
```
*/
/*
```cl
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

```
*/
:- lisp_compile_to_prolog(pkg_user, [disassemble, function(fifteen)]).
:- cl_disassemble(function(u_fifteen), _Ignored).
/*
```cl


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

```
*/
/*
```cl
(is eq 15 (fifteen))

```
*/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 15, [fifteen]]).
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
   ;   get_var(LEnv, a15, A15_Get14),
       get_var(LEnv, b15, B15_Get15),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A15_Get14,
                   B15_Get15
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(defun do-four () (DO ((temp-one 1 (1+ temp-one) )(temp-two 0 (1- temp-two) ) )((> (- temp-one temp-two) 5) temp-one)() ))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defun,
                            'do-four',
                            [],

                            [ 'DO',

                              [ ['temp-one', 1, ['1+', 'temp-one']],
                                ['temp-two', 0, ['1-', 'temp-two']]
                              ],
                              [[>, [-, 'temp-one', 'temp-two'], 5], 'temp-one'],
                              []
                            ]
                          ]).

##### annotating... `U::DO-FOUR`
```prolog
wl:lambda_def(defun, u_do_four, f_u_do_four, [], [[do, [[u_temp_one, 1, ['1+', u_temp_one]], [u_temp_two, 0, ['1-', u_temp_two]]], [[>, [-, u_temp_one, u_temp_two], 5], u_temp_one], []]]).
```

##### annotating... `U::DO-FOUR`
```prolog
wl:arglist_info(u_do_four, [], [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
```

##### annotating... `U::DO-FOUR`
```prolog
wl: init_args(exact_only, u_do_four).

```

### Compiled:  `U::DO-FOUR`
```prolog
f_u_do_four(FnResult) :-
        Env=[],
        catch(( ( GoEnv=[[bv(u_temp_one, 1), bv(u_temp_two, 0)]|Env],
                  catch(( call_addr_block(GoEnv,
                                          (push_label(do_label_2), get_var(GoEnv, u_temp_one, Temp_one_Get32), get_var(GoEnv, u_temp_two, Temp_two_Get33), -(Temp_one_Get32, Temp_two_Get33, PredArg1Result35), (PredArg1Result35>5->throw(block_exit([], Temp_one_Get22)), _TBResult=ThrowResult37;get_var(GoEnv, u_temp_one, Temp_one_Get40), '1+'(Temp_one_Get40, Temp_one), get_var(GoEnv, u_temp_two, Temp_two_Get41), '1-'(Temp_two_Get41, Temp_two), set_var(GoEnv, u_temp_one, Temp_one), set_var(GoEnv, u_temp_two, Temp_two), goto(do_label_2, GoEnv), _TBResult=_GORES42)),

                                          [ addr(addr_tagbody_3_do_label_2,
                                                 do_label_2,
                                                 '$unused',
                                                 BlockExitEnv,
                                                 (get_var(BlockExitEnv, u_temp_one, Temp_one_Get22), get_var(BlockExitEnv, u_temp_two, Temp_two_Get25), -(Temp_one_Get22, Temp_two_Get25, _18146), (_18146>5->throw(block_exit([], Temp_one_Get22)), _18148=ThrowResult;'1+'(Temp_one_Get22, Set_var_Ret), '1-'(Temp_two_Get25, Set_var_Ret52), set_var(BlockExitEnv, u_temp_one, Set_var_Ret), set_var(BlockExitEnv, u_temp_two, Set_var_Ret52), goto(do_label_2, BlockExitEnv), _18148=_GORES)))
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
```
/*
```cl
:- set_opv(f_u_do_four, classof, claz_function),
   set_opv(u_do_four, compile_as, kw_function),
   set_opv(u_do_four, function, f_u_do_four),
   DefunResult=u_do_four.
```
*/
/*
```cl
(is = 4  (do-four))

```
*/
:- lisp_compile_to_prolog(pkg_user, [is, =, 4, ['do-four']]).
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
   ;   get_var(LEnv, a16, A16_Get14),
       get_var(LEnv, b16, B16_Get15),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A16_Get14,
                   B16_Get15
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(is eq 'string_l (DEFUN string_l (x )(COND ((STRINGP x )x )((SYMBOLP x )(symbol-name x ))(T (ERROR "type error" )))))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ (is),
                            eq,
                            [quote, string_l],

                            [ 'DEFUN',
                              string_l,
                              [x],

                              [ 'COND',
                                [['STRINGP', x], x],
                                [['SYMBOLP', x], ['symbol-name', x]],
                                ['T', ['ERROR', '$STRING'("type error")]]
                              ]
                            ]
                          ]).

##### annotating... `U::STRING_L`
```prolog
wl:lambda_def(defun, u_string_l, f_u_string_l, [u_x], [[cond, [[stringp, u_x], u_x], [[symbolp, u_x], [symbol_name, u_x]], [t, [error, '$ARRAY'([*], claz_base_character, "type error")]]]]).
```

##### annotating... `U::STRING_L`
```prolog
wl:arglist_info(u_string_l, [u_x], [X_Get24], arginfo{all:[u_x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_x], opt:0, req:[u_x], rest:0, sublists:0, whole:0}).
```

##### annotating... `U::STRING_L`
```prolog
wl: init_args(exact_only, u_string_l).

```

### Compiled:  `U::STRING_L`
```prolog
f_u_string_l(X_Get24, ElseResult28) :-
        (   is_stringp(X_Get24)
        ->  ElseResult28=X_Get24
        ;   is_symbolp(X_Get24)
        ->  cl_symbol_name(X_Get24, TrueResult),
            ElseResult28=TrueResult
        ;   cl_error(['$ARRAY'([*], claz_base_character, "type error")],
                     ElseResult),
            ElseResult28=ElseResult
        ).
```
/*
```cl
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
                 TrueResult40),
       LetResult=TrueResult40
   ;   get_var(LEnv, a17, A17_Get38),
       get_var(LEnv, b17, B17_Get39),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A17_Get38,
                   B17_Get39
                 ],
                 ElseResult41),
       trace,
       LetResult=ElseResult41
   ).
```
*/
/*
```cl
(is eq () (TAGBODY 1 (PRINT "hi" )))

```
*/
:- lisp_compile_to_prolog(pkg_user,
                          [is, eq, [], ['TAGBODY', 1, ['PRINT', '$STRING'("hi")]]]).
:-call_addr_block(_16964,(push_label(1),cl_print('$ARRAY'([*],claz_base_character,"hi"),_17088)),[addr(addr_tagbody_4_1,1,'$unused',_17116,cl_print('$ARRAY'([*],claz_base_character,"hi"),_17118))]),_17060=[[bv(a18,[]),bv(b18,[])]|_16964],get_var(_17060,a18,_17340),get_var(_17060,b18,_17366),(is_eq(_17340,_17366)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[tagbody,1,[print,'$ARRAY'([*],claz_base_character,"hi")]]],_17378),_16802=_17378;get_var(_17060,a18,_17398),get_var(_17060,b18,_17438),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_17398,_17438],_17380),trace,_16802=_17380).
/*
```cl
(is eq () (TAGBODY a (PRINT "hi" )))

```
*/
:- lisp_compile_to_prolog(pkg_user,
                          [is, eq, [], ['TAGBODY', a, ['PRINT', '$STRING'("hi")]]]).
:-call_addr_block(_17076,(push_label(u_a),cl_print('$ARRAY'([*],claz_base_character,"hi"),_17200)),[addr(addr_tagbody_5_u_a,u_a,'$unused',_17228,cl_print('$ARRAY'([*],claz_base_character,"hi"),_17230))]),_17172=[[bv(a19,[]),bv(b19,[])]|_17076],get_var(_17172,a19,_17452),get_var(_17172,b19,_17478),(is_eq(_17452,_17478)->cl_format([t,'$ARRAY'([*],claz_base_character,"OK: ~a is ~a to ~a~%"),[],eq,[tagbody,u_a,[print,'$ARRAY'([*],claz_base_character,"hi")]]],_17490),_16914=_17490;get_var(_17172,a19,_17510),get_var(_17172,b19,_17550),cl_format([t,'$ARRAY'([*],claz_base_character,"FAILED: when matching ~a and ~a~%"),_17510,_17550],_17492),trace,_16914=_17492).
/*
```cl
(is eq () (LET ((val 1 ))NIL ))
```
*/
:- lisp_compile_to_prolog(pkg_user, [is, eq, [], ['LET', [[val, 1]], []]]).
:- LEnv7=[[bv(u_val, 1)]|TLEnv],
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
   ;   get_var(LEnv, a110, A110_Get16),
       get_var(LEnv, b110, B110_Get17),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A110_Get16,
                   B110_Get17
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(is eq () (LET ((val 1 )) ))

```
*/
:- lisp_compile_to_prolog(pkg_user, [is, eq, [], ['LET', [[val, 1]]]]).
:- LEnv7=[[bv(u_val, 1)]|TLEnv],
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
   ;   get_var(LEnv, a201, A201_Get16),
       get_var(LEnv, b201, B201_Get17),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A201_Get16,
                   B201_Get17
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(is eql 1 (LET ((val 1 ))val ))
```
*/
:- lisp_compile_to_prolog(pkg_user, [is, eql, 1, ['LET', [[val, 1]], val]]).
:- LEnv7=[[bv(u_val, 1)]|TLEnv],
   get_var(LEnv7, u_val, Val_Get),
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
   ;   get_var(LEnv, a22, A22_Get18),
       get_var(LEnv, b22, B22_Get19),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A22_Get18,
                   B22_Get19
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(is eql 'world (LET ((a 'b) )(LET ((a 'world) )
  (LET ((a 'hello) )(LET ((a a)(*package* (find-package :keyword) ) )(PRINT a) ) )(PRINT a) ) ))


;; 3.1. Review of defstruct

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ (is),
                            eql,
                            [quote, world],

                            [ 'LET',
                              [[a, [quote, b]]],

                              [ 'LET',
                                [[a, [quote, world]]],

                                [ 'LET',
                                  [[a, [quote, hello]]],

                                  [ 'LET',

                                    [ [a, a],

                                      [ '*package*',
                                        ['find-package', ':keyword']
                                      ]
                                    ],
                                    ['PRINT', a]
                                  ]
                                ],
                                ['PRINT', a]
                              ]
                            ]
                          ]).
:- LEnv7=[[bv(u_a, u_b)]|TLEnv],
   LEnv9=[[bv(u_a, u_world)]|LEnv7],
   LEnv11=[[bv(u_a, u_hello)]|LEnv9],
   get_var(LEnv11, u_a, A_Get),
   cl_find_package(kw_keyword, Xx_package_xx_Init),
   LEnv13=[[bv(u_a, A_Get)]|LEnv11],
   save_special(sv(xx_package_xx, Xx_package_xx_Init, value, Value)),
   get_var(LEnv13, u_a, A_Get18),
   cl_print(A_Get18, LetResult12),
   restore_special(sv(xx_package_xx, Xx_package_xx_Init, value, Value)),
   get_var(LEnv9, u_a, A_Get21),
   cl_print(A_Get21, LetResult10),
   LEnv=[[bv(a23, u_world), bv(b23, LetResult10)]|TLEnv],
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
   ;   get_var(LEnv, a23, A23_Get31),
       get_var(LEnv, b23, B23_Get32),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A23_Get31,
                   B23_Get32
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
; 3.1. Review of defstruct
```
*/
/*
```cl
(progn #+WAM-CL (prolog-inline "nop(trace)")(is eq 'point (defstruct point x y z)))
;; (defstruct point x y z)

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ progn,
                            ['prolog-inline', '$STRING'("nop(trace)")],
                            [is, eq, [quote, point], [defstruct, point, x, y, z]]
                          ]).
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
   ;   get_var(LEnv, a24, A24_Get14),
       get_var(LEnv, b24, B24_Get15),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A24_Get14,
                   B24_Get15
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
; (defstruct point x y z)
```
*/
/*
```cl
(is eq 'point4d (defstruct point4d x y z t))

```
*/
:- lisp_compile_to_prolog(pkg_user,
                          [is, eq, [quote, point4d], [defstruct, point4d, x, y, z, t]]).
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
   ;   get_var(LEnv, a25, A25_Get14),
       get_var(LEnv, b25, B25_Get15),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A25_Get14,
                   B25_Get15
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(defun distance-from-origin (point)
  (let* ((x (point-x point))
         (y (point-y point))
         (z (point-z point)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defun,
                            'distance-from-origin',
                            [point],

                            [ 'let*',

                              [ [x, ['point-x', point]],
                                [y, ['point-y', point]],
                                [z, ['point-z', point]]
                              ],
                              [sqrt, [+, [*, x, x], [*, y, y], [*, z, z]]]
                            ]
                          ]).

##### annotating... `U::DISTANCE-FROM-ORIGIN`
```prolog
wl:lambda_def(defun, u_distance_from_origin, f_u_distance_from_origin, [u_point], [[let_xx, [[u_x, [u_point_x, u_point]], [u_y, [u_point_y, u_point]], [u_z, [u_point_z, u_point]]], [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]).
```

##### annotating... `U::DISTANCE-FROM-ORIGIN`
```prolog
wl:arglist_info(u_distance_from_origin, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}).
```

##### annotating... `U::DISTANCE-FROM-ORIGIN`
```prolog
wl: init_args(exact_only, u_distance_from_origin).

```

### Compiled:  `U::DISTANCE-FROM-ORIGIN`
```prolog
f_u_distance_from_origin(Point_Param, FnResult) :-
        Env=[bv(u_point, Point_Param)],
        f_u_point_x(Point_Param, X_Init),
        LEnv=[[bv(u_x, X_Init)]|Env],
        f_u_point_y(Point_Param, Y_Init),
        LEnv17=[[bv(u_y, Y_Init)]|LEnv],
        f_u_point_z(Point_Param, Z_Init),
        LEnv21=[[bv(u_z, Z_Init)]|LEnv17],
        get_var(LEnv21, u_x, X_Get26),
        *(X_Get26, X_Get26, _19204),
        get_var(LEnv21, u_y, Y_Get28),
        *(Y_Get28, Y_Get28, _19302),
        +(_19204, _19302, _19422),
        get_var(LEnv21, u_z, Z_Get30),
        *(Z_Get30, Z_Get30, _19434),
        +(_19422, _19434, Sqrt_Param),
        cl_sqrt(Sqrt_Param, LetResult18),
        LetResult18=FnResult.
```
/*
```cl
:- set_opv(f_u_distance_from_origin, classof, claz_function),
   set_opv(u_distance_from_origin, compile_as, kw_function),
   set_opv(u_distance_from_origin, function, f_u_distance_from_origin),
   DefunResult=u_distance_from_origin.
```
*/
/*
```cl
(defun reflect-in-y-axis (point)
  (setf (point-y point)
        (- (point-y point))))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defun,
                            'reflect-in-y-axis',
                            [point],
                            [setf, ['point-y', point], [-, ['point-y', point]]]
                          ]).

##### annotating... `U::REFLECT-IN-Y-AXIS`
```prolog
wl:lambda_def(defun, u_reflect_in_y_axis, f_u_reflect_in_y_axis, [u_point], [[setf, [u_point_y, u_point], [-, [u_point_y, u_point]]]]).
```

##### annotating... `U::REFLECT-IN-Y-AXIS`
```prolog
wl:arglist_info(u_reflect_in_y_axis, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}).
```

##### annotating... `U::REFLECT-IN-Y-AXIS`
```prolog
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
```
/*
```cl
:- set_opv(f_u_reflect_in_y_axis, classof, claz_function),
   set_opv(u_reflect_in_y_axis, compile_as, kw_function),
   set_opv(u_reflect_in_y_axis, function, f_u_reflect_in_y_axis),
   DefunResult=u_reflect_in_y_axis.
```
*/
/*
```cl
(list (setf my-point (make-point :x 3 :y 4 :z 12)) (setf my-point2 (make-point :x 3 :y 4 :z 12)))
```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ list,

                            [ setf,
                              'my-point',
                              ['make-point', ':x', 3, ':y', 4, ':z', 12]
                            ],

                            [ setf,
                              'my-point2',
                              ['make-point', ':x', 3, ':y', 4, ':z', 12]
                            ]
                          ]).
:- f_u_make_point(kw_x, 3, kw_y, 4, kw_z, 12, Make_point_Ret),
   set_place(TLEnv, setf, [value, u_my_point], [Make_point_Ret], Setf_R),
   f_u_make_point(kw_x, 3, kw_y, 4, kw_z, 12, Make_point_Ret9),
   set_place(TLEnv, setf, [value, u_my_point2], [Make_point_Ret9], Setf_R7),
   _Ignored=[Setf_R, Setf_R7].
/*
```cl
(setf my-point3 #S(POINT :X 3 :Y 4 :Z 12))
```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ setf,
                            'my-point3',
                            '$S'(['POINT', ':X', 3, ':Y', 4, ':Z', 12])
                          ]).
:- create_struct([u_point, kw_x, 3, kw_y, 4, kw_z, 12], Create_struct_Ret),
   set_place(TLEnv, setf, [value, u_my_point3], [Create_struct_Ret], Setf_R).
/*
```cl
(setf my-point4d (make-point4d :x 3 :y 4 :z 12 :t 1))


```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ setf,
                            'my-point4d',
                            ['make-point4d', ':x', 3, ':y', 4, ':z', 12, ':t', 1]
                          ]).
:- f_u_make_point4d(kw_x, 3, kw_y, 4, kw_z, 12, kw_t, 1, Make_point4d_Ret),
   set_place(TLEnv, setf, [value, u_my_point4d], [Make_point4d_Ret], Setf_R).
/*
```cl
(is eq t (point-p my-point))

```
*/
:- lisp_compile_to_prolog(pkg_user, [is, eq, t, ['point-p', 'my-point']]).
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
   ;   get_var(LEnv, a26, A26_Get15),
       get_var(LEnv, b26, B26_Get16),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A26_Get15,
                   B26_Get16
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(is eq 'point (type-of my-point))

```
*/
:- lisp_compile_to_prolog(pkg_user,
                          [is, eq, [quote, point], ['type-of', 'my-point']]).
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
   ;   get_var(LEnv, a27, A27_Get15),
       get_var(LEnv, b27, B27_Get16),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A27_Get15,
                   B27_Get16
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
#+IGNORE #+WAM-CL (prolog-call "break")

```
*/
:- lisp_compile_to_prolog(pkg_user,
                          '$COMMENT'(
                                     [ flag_removed,
                                       [+, ':IGNORE'],

                                       [ #+,
                                         ':WAM-CL',
                                         ['prolog-call', '$STRING'("break")]
                                       ]
                                     ])).
/*
```cl
(is eql 13 (progn (print (distance-from-origin my-point))))

;; #+CLISP (BREAK)
;; #+WAM-CL (prolog-call "break")

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ (is),
                            eql,
                            13,

                            [ progn,
                              [print, ['distance-from-origin', 'my-point']]
                            ]
                          ]).
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
   ;   get_var(LEnv, a28, A28_Get15),
       get_var(LEnv, b28, B28_Get16),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A28_Get15,
                   B28_Get16
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
; #+CLISP (BREAK)
```
*/
/*
```cl
; #+WAM-CL (prolog-call "break")
```
*/
/*
```cl
(is = -4 (reflect-in-y-axis my-point))

```
*/
:- lisp_compile_to_prolog(pkg_user, [is, =, -4, ['reflect-in-y-axis', 'my-point']]).
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
   ;   get_var(LEnv, a29, A29_Get15),
       get_var(LEnv, b29, B29_Get16),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A29_Get15,
                   B29_Get16
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(is eq my-point my-point)

```
*/
:- lisp_compile_to_prolog(pkg_user, [is, eq, 'my-point', 'my-point']).
:- get_var(TLEnv, u_my_point, My_point_Get8),
   LEnv=[[bv(a210, My_point_Get8), bv(b210, My_point_Get8)]|TLEnv],
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
   ;   get_var(LEnv, a210, A210_Get17),
       get_var(LEnv, b210, B210_Get18),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A210_Get17,
                   B210_Get18
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(setf a-similar-point #s(point :x 3 :y -4 :z 12))

; (is eq t (equal my-point a-similar-point))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ setf,
                            'a-similar-point',
                            '$S'([point, ':x', 3, ':y', -4, ':z', 12])
                          ]).
:- create_struct([u_point, kw_x, 3, kw_y, -4, kw_z, 12], Create_struct_Ret),
   set_place(TLEnv, setf, [value, u_a_similar_point], [Create_struct_Ret], Setf_R).
/*
```cl
 (is eq t (equal my-point a-similar-point))
```
*/
/*
```cl
(is eq nil (eq my-point a-similar-point))

```
*/
:- lisp_compile_to_prolog(pkg_user,
                          [is, eq, [], [eq, 'my-point', 'a-similar-point']]).
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
   ;   get_var(LEnv, a301, A301_Get16),
       get_var(LEnv, b301, B301_Get17),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A301_Get16,
                   B301_Get17
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(equalp my-point a-similar-point)

```
*/
:- lisp_compile_to_prolog(pkg_user, [equalp, 'my-point', 'a-similar-point']).
:- get_var(TLEnv, u_a_similar_point, A_similar_point_Get),
   get_var(TLEnv, u_my_point, My_point_Get),
   cl_equalp(My_point_Get, A_similar_point_Get, _Ignored).
/*
```cl
(is eq t (equalp my-point a-similar-point) )


;; 3.2. defclass

```
*/
:- lisp_compile_to_prolog(pkg_user,
                          [is, eq, t, [equalp, 'my-point', 'a-similar-point']]).
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
   ;   get_var(LEnv, a32, A32_Get16),
       get_var(LEnv, b32, B32_Get17),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A32_Get16,
                   B32_Get17
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
; 3.2. defclass
```
*/
/*
```cl
(unintern 'point)

```
*/
:- lisp_compile_to_prolog(pkg_user, [unintern, [quote, point]]).
:- cl_unintern(u_point, _Ignored).
/*
```cl
(defclass point ()
  (x
   y
   z))

```
*/
:- lisp_compile_to_prolog(pkg_user, [defclass, point, [], [x, y, z]]).
:- cl_defclass([u_point, [], [u_x, u_y, u_z]], _Ignored).
/*
```cl
(setf my-point (make-instance 'point))

```
*/
:- lisp_compile_to_prolog(pkg_user,
                          [setf, 'my-point', ['make-instance', [quote, point]]]).
:- cl_make_instance([u_point], Make_instance_Ret),
   set_place(TLEnv, setf, [value, u_my_point], [Make_instance_Ret], Setf_R).
/*
```cl
(is eq 'point (type-of my-point))

```
*/
:- lisp_compile_to_prolog(pkg_user,
                          [is, eq, [quote, point], ['type-of', 'my-point']]).
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
   ;   get_var(LEnv, a33, A33_Get15),
       get_var(LEnv, b33, B33_Get16),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A33_Get15,
                   B33_Get16
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(defun set-point-values (point x y z)
  (setf (slot-value point 'x) x
        (slot-value point 'y) y
        (slot-value point 'z) z))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defun,
                            'set-point-values',
                            [point, x, y, z],

                            [ setf,
                              ['slot-value', point, [quote, x]],
                              x,
                              ['slot-value', point, [quote, y]],
                              y,
                              ['slot-value', point, [quote, z]],
                              z
                            ]
                          ]).

##### annotating... `U::SET-POINT-VALUES`
```prolog
wl:lambda_def(defun, u_set_point_values, f_u_set_point_values, [u_point, u_x, u_y, u_z], [[setf, [slot_value, u_point, [quote, u_x]], u_x, [slot_value, u_point, [quote, u_y]], u_y, [slot_value, u_point, [quote, u_z]], u_z]]).
```

##### annotating... `U::SET-POINT-VALUES`
```prolog
wl:arglist_info(u_set_point_values, [u_point, u_x, u_y, u_z], [Point_Param, X_Param, Y_Param, Z_Param], arginfo{all:[u_point, u_x, u_y, u_z], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point, u_x, u_y, u_z], opt:0, req:[u_point, u_x, u_y, u_z], rest:0, sublists:0, whole:0}).
```

##### annotating... `U::SET-POINT-VALUES`
```prolog
wl: init_args(exact_only, u_set_point_values).

```

### Compiled:  `U::SET-POINT-VALUES`
```prolog
f_u_set_point_values(Point_Param, X_Param, Y_Param, Z_Param, FnResult) :-
        Env=[bv(u_point, Point_Param), bv(u_x, X_Param), bv(u_y, Y_Param), bv(u_z, Z_Param)],
        set_place(Env, setf, [slot_value, Point_Param, u_x], [X_Param], Setf_R),
        set_place(Env, setf, [slot_value, Point_Param, u_y], [Y_Param], Setf_R24),
        set_place(Env, setf, [slot_value, Point_Param, u_z], [Z_Param], Setf_R27),
        Setf_R27=FnResult.
```
/*
```cl
:- set_opv(f_u_set_point_values, classof, claz_function),
   set_opv(u_set_point_values, compile_as, kw_function),
   set_opv(u_set_point_values, function, f_u_set_point_values),
   DefunResult=u_set_point_values.
```
*/
/*
```cl
(set-point-values my-point 3 4 12)

```
*/
:- lisp_compile_to_prolog(pkg_user, ['set-point-values', 'my-point', 3, 4, 12]).
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_set_point_values(My_point_Get, 3, 4, 12, _Ignored).
/*
```cl
(defun distance-from-origin (point)
  (with-slots (x y z)
      point
    (sqrt (+ (* x x) (* y y) (* z z)))))


```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defun,
                            'distance-from-origin',
                            [point],

                            [ 'with-slots',
                              [x, y, z],
                              point,
                              [sqrt, [+, [*, x, x], [*, y, y], [*, z, z]]]
                            ]
                          ]).

##### annotating... `U::DISTANCE-FROM-ORIGIN`
```prolog
wl:lambda_def(defun, u_distance_from_origin, f_u_distance_from_origin, [u_point], [[with_slots, [u_x, u_y, u_z], u_point, [sqrt, [+, [*, u_x, u_x], [*, u_y, u_y], [*, u_z, u_z]]]]]).
```

##### annotating... `U::DISTANCE-FROM-ORIGIN`
```prolog
wl:arglist_info(u_distance_from_origin, [u_point], [Point_Param], arginfo{all:[u_point], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_point], opt:0, req:[u_point], rest:0, sublists:0, whole:0}).
```

##### annotating... `U::DISTANCE-FROM-ORIGIN`
```prolog
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
        get_var(LEnv, u_x, X_Get22),
        *(X_Get22, X_Get22, _20204),
        get_var(LEnv, u_y, Y_Get24),
        *(Y_Get24, Y_Get24, _20360),
        +(_20204, _20360, _20538),
        get_var(LEnv, u_z, Z_Get26),
        *(Z_Get26, Z_Get26, _20550),
        +(_20538, _20550, Sqrt_Param),
        cl_sqrt(Sqrt_Param, LetResult),
        LetResult=FnResult.
```
/*
```cl
:- set_opv(f_u_distance_from_origin, classof, claz_function),
   set_opv(u_distance_from_origin, compile_as, kw_function),
   set_opv(u_distance_from_origin, function, f_u_distance_from_origin),
   DefunResult=u_distance_from_origin.
```
*/
/*
```cl
(DISASSEMBLE #'distance-from-origin)


```
*/
:- lisp_compile_to_prolog(pkg_user,
                          ['DISASSEMBLE', function('distance-from-origin')]).
:- cl_disassemble(function(u_distance_from_origin), _Ignored).
/*
```cl
(distance-from-origin my-point)

;; 3.3. classes are objects

```
*/
:- lisp_compile_to_prolog(pkg_user, ['distance-from-origin', 'my-point']).
:- get_var(TLEnv, u_my_point, My_point_Get),
   f_u_distance_from_origin(My_point_Get, _Ignored).
/*
```cl
; 3.3. classes are objects
```
*/
/*
```cl
(find-class 'point)

```
*/
:- lisp_compile_to_prolog(pkg_user, ['find-class', [quote, point]]).
:- cl_find_class(u_point, _Ignored).
/*
```cl
(class-name (find-class 'point))

```
*/
:- lisp_compile_to_prolog(pkg_user,
                          ['class-name', ['find-class', [quote, point]]]).
:- cl_find_class(u_point, Class_name_Param),
   cl_class_name(Class_name_Param, _Ignored).
/*
```cl
(class-of my-point)

;; #-(or cormanlisp CLISP WAM-CL)
```
*/
:- lisp_compile_to_prolog(pkg_user, ['class-of', 'my-point']).
:- get_var(TLEnv, u_my_point, My_point_Get),
   cl_class_of(My_point_Get, _Ignored).
/*
```cl
; #-(or cormanlisp CLISP WAM-CL)
```
*/
/*
```cl
(typep my-point (class-of my-point))

```
*/
:- lisp_compile_to_prolog(pkg_user, [typep, 'my-point', ['class-of', 'my-point']]).
:- get_var(TLEnv, u_my_point, My_point_Get6),
   cl_class_of(My_point_Get6, Class_of_Ret),
   cl_typep(My_point_Get6, Class_of_Ret, _Ignored).
/*
```cl
(is eq (find-class 'STANDARD-CLASS)
       (class-of (class-of my-point)))

;; 3.4. you don't need clos to use clos

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ (is),
                            eq,
                            ['find-class', [quote, 'STANDARD-CLASS']],
                            ['class-of', ['class-of', 'my-point']]
                          ]).
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
   ;   get_var(LEnv, a34, A34_Get16),
       get_var(LEnv, b34, B34_Get17),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A34_Get16,
                   B34_Get17
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
; 3.4. you don't need clos to use clos
```
*/
/*
```cl
(let ((the-symbol-class (find-class 'symbol)))
  (values the-symbol-class
          (class-name the-symbol-class)
          (eq the-symbol-class (class-of 'symbol))
          (class-of the-symbol-class)))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ let,

                            [
                              [ 'the-symbol-class',
                                ['find-class', [quote, symbol]]
                              ]
                            ],

                            [ values,
                              'the-symbol-class',
                              ['class-name', 'the-symbol-class'],

                              [ eq,
                                'the-symbol-class',
                                ['class-of', [quote, symbol]]
                              ],
                              ['class-of', 'the-symbol-class']
                            ]
                          ]).
:- cl_find_class(symbol, The_symbol_class_Init),
   LEnv=[[bv(u_the_symbol_class, The_symbol_class_Init)]|TLEnv],
   get_var(LEnv, u_the_symbol_class, The_symbol_class_Get9),
   cl_class_name(The_symbol_class_Get9, Class_name_Ret),
   get_var(LEnv, u_the_symbol_class, The_symbol_class_Get10),
   cl_class_of(symbol, Class_of_Ret),
   cl_eq(The_symbol_class_Get10, Class_of_Ret, Eq_Ret),
   get_var(LEnv, u_the_symbol_class, The_symbol_class_Get11),
   cl_class_of(The_symbol_class_Get11, Class_of_Ret15),
   nb_setval('$mv_return',
             [The_symbol_class_Get9, Class_name_Ret, Eq_Ret, Class_of_Ret15]).
/*
```cl
(find-class t)

```
*/
:- lisp_compile_to_prolog(pkg_user, ['find-class', t]).
:- cl_find_class(t, _Ignored).
/*
```cl
(is eq 'foo (defstruct foo))

```
*/
:- lisp_compile_to_prolog(pkg_user, [is, eq, [quote, foo], [defstruct, foo]]).
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
   ;   get_var(LEnv, a35, A35_Get14),
       get_var(LEnv, b35, B35_Get15),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A35_Get14,
                   B35_Get15
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
(is eq (find-class 'foo) (class-of (make-foo)))

;; 3.5 slots

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ (is),
                            eq,
                            ['find-class', [quote, foo]],
                            ['class-of', ['make-foo']]
                          ]).
:- cl_find_class(u_foo, A36_Init),
   f_u_make_foo(Class_of_Param),
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
   ;   get_var(LEnv, a36, A36_Get15),
       get_var(LEnv, b36, B36_Get16),
       cl_format(
                 [ t,
                   '$ARRAY'([*],
                            claz_base_character,
                            "FAILED: when matching ~a and ~a~%"),
                   A36_Get15,
                   B36_Get16
                 ],
                 ElseResult),
       trace,
       LetResult=ElseResult
   ).
/*
```cl
; 3.5 slots
```
*/
/*
```cl
(defclass daft-point ()
  ((x :accessor daft-x :initarg :x)
   (y :accessor daft-y :initform 3.14159)
   (z :reader daft-z :allocation :class)))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defclass,
                            'daft-point',
                            [],

                            [ [x, ':accessor', 'daft-x', ':initarg', ':x'],
                              [y, ':accessor', 'daft-y', ':initform', 3.14159],
                              [z, ':reader', 'daft-z', ':allocation', ':class']
                            ]
                          ]).
:- cl_defclass(
               [ u_daft_point,
                 [],

                 [ [u_x, kw_accessor, u_daft_x, kw_initarg, kw_x],
                   [u_y, kw_accessor, u_daft_y, kw_initform, 3.14159],
                   [u_z, kw_reader, u_daft_z, kw_allocation, kw_class]
                 ]
               ],
               _Ignored).
/*
```cl
(setf (slot-value (make-instance 'daft-point) 'z) 42)

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ setf,

                            [ 'slot-value',
                              ['make-instance', [quote, 'daft-point']],
                              [quote, z]
                            ],
                            42
                          ]).
:- cl_make_instance([u_daft_point], Make_instance_Ret),
   set_place(TLEnv, setf, [slot_value, Make_instance_Ret, u_z], [42], Setf_R).
/*
```cl
(setf my-daft-point (make-instance 'daft-point :x 19))


```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ setf,
                            'my-daft-point',
                            ['make-instance', [quote, 'daft-point'], ':x', 19]
                          ]).
:- cl_make_instance([u_daft_point, kw_x, 19], Make_instance_Ret),
   set_place(TLEnv, setf, [value, u_my_daft_point], [Make_instance_Ret], Setf_R).
/*
```cl
#+PERFECT
(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (progn #+WAM-CL (prolog-trace) (daft-z my-daft-point)))

```
*/
:- lisp_compile_to_prolog(pkg_user,
                          '$COMMENT'(
                                     [ flag_removed,
                                       [+, ':PERFECT'],

                                       [ list,
                                         ['daft-x', 'my-daft-point'],
                                         ['daft-y', 'my-daft-point'],

                                         [ progn,
                                           [#+, ':WAM-CL', ['prolog-trace']],
                                           ['daft-z', 'my-daft-point']
                                         ]
                                       ]
                                     ])).
/*
```cl
(let ((temp (make-instance 'daft-point)))
  (setf (daft-y temp) 999
        (slot-value temp 'z) 0))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ let,
                            [[temp, ['make-instance', [quote, 'daft-point']]]],

                            [ setf,
                              ['daft-y', temp],
                              999,
                              ['slot-value', temp, [quote, z]],
                              0
                            ]
                          ]).
:- cl_make_instance([u_daft_point], Temp_Init),
   LEnv=[[bv(u_temp, Temp_Init)]|TLEnv],
   get_var(LEnv, u_temp, Temp_Get),
   set_place(LEnv, setf, [u_daft_y, Temp_Get], [999], Setf_R),
   get_var(LEnv, u_temp, Temp_Get12),
   set_place(LEnv, setf, [slot_value, Temp_Get12, u_z], [0], Setf_R11).
/*
```cl
#+PERFECT
(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (daft-z my-daft-point))

;; 3.6 Subclasses and inheritance

```
*/
:- lisp_compile_to_prolog(pkg_user,
                          '$COMMENT'(
                                     [ flag_removed,
                                       [+, ':PERFECT'],

                                       [ list,
                                         ['daft-x', 'my-daft-point'],
                                         ['daft-y', 'my-daft-point'],
                                         ['daft-z', 'my-daft-point']
                                       ]
                                     ])).
/*
```cl
; 3.6 Subclasses and inheritance
```
*/
/*
```cl
(defclass animal ()
  ((legs :reader leg-count :initarg :legs)
   (comes-from :reader comes-from :initarg :comes-from)))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defclass,
                            animal,
                            [],

                            [ [legs, ':reader', 'leg-count', ':initarg', ':legs'],

                              [ 'comes-from',
                                ':reader',
                                'comes-from',
                                ':initarg',
                                ':comes-from'
                              ]
                            ]
                          ]).
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
/*
```cl
(defclass mammal (animal)
  ((diet :initform 'antelopes :initarg :diet)))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defclass,
                            mammal,
                            [animal],

                            [
                              [ diet,
                                ':initform',
                                [quote, antelopes],
                                ':initarg',
                                ':diet'
                              ]
                            ]
                          ]).
:- cl_defclass(
               [ u_mammal,
                 [u_animal],
                 [[u_diet, kw_initform, [quote, u_antelopes], kw_initarg, kw_diet]]
               ],
               _Ignored).
/*
```cl
(defclass aardvark (mammal)
  ((cute-p :accessor cute-p :initform nil)))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defclass,
                            aardvark,
                            [mammal],
                            [['cute-p', ':accessor', 'cute-p', ':initform', []]]
                          ]).
:- cl_defclass(
               [ u_aardvark,
                 [u_mammal],
                 [[u_cute_p, kw_accessor, u_cute_p, kw_initform, []]]
               ],
               _Ignored).
/*
```cl
(#-allegro class-direct-superclasses #+allegro aclmop:class-direct-superclasses
   (find-class 'aardvark))

;; ACL needs to instantiate a class before its precedence-list becomes visible
;; #+allegro
```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ 'class-direct-superclasses',
                            ['find-class', [quote, aardvark]]
                          ]).
:- cl_find_class(u_aardvark, Direct_superclasses_Param),
   f_clos_class_direct_superclasses(Direct_superclasses_Param, _Ignored).
/*
```cl
; ACL needs to instantiate a class before its precedence-list becomes visible
```
*/
/*
```cl
; #+allegro
```
*/
/*
```cl
(make-instance 'aardvark)

```
*/
:- lisp_compile_to_prolog(pkg_user, ['make-instance', [quote, aardvark]]).
:- cl_make_instance([u_aardvark], _Ignored).
/*
```cl
(#-allegro class-precedence-list #+allegro aclmop:class-precedence-list
   (find-class 'aardvark))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ 'class-precedence-list',
                            ['find-class', [quote, aardvark]]
                          ]).
:- cl_find_class(u_aardvark, Precedence_list_Param),
   f_clos_class_precedence_list(Precedence_list_Param, _Ignored).
/*
```cl
(defclass figurine ()
  ((potter :accessor made-by :initarg :made-by)
   (comes-from :initarg :made-in)))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defclass,
                            figurine,
                            [],

                            [
                              [ potter,
                                ':accessor',
                                'made-by',
                                ':initarg',
                                ':made-by'
                              ],
                              ['comes-from', ':initarg', ':made-in']
                            ]
                          ]).
:- cl_defclass(
               [ u_figurine,
                 [],

                 [ [u_potter, kw_accessor, u_made_by, kw_initarg, kw_made_by],
                   [u_comes_from, kw_initarg, kw_made_in]
                 ]
               ],
               _Ignored).
/*
```cl
(defclass figurine-aardvark (aardvark figurine)
  ((name :reader aardvark-name :initarg :aardvark-name)
   (diet :initform nil)))

;; ACL needs to instantiate a class before its precedence-list becomes visible
;; #+allegro
```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ defclass,
                            'figurine-aardvark',
                            [aardvark, figurine],

                            [
                              [ name,
                                ':reader',
                                'aardvark-name',
                                ':initarg',
                                ':aardvark-name'
                              ],
                              [diet, ':initform', []]
                            ]
                          ]).
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
/*
```cl
; ACL needs to instantiate a class before its precedence-list becomes visible
```
*/
/*
```cl
; #+allegro
```
*/
/*
```cl
(make-instance 'figurine-aardvark)

```
*/
:- lisp_compile_to_prolog(pkg_user,
                          ['make-instance', [quote, 'figurine-aardvark']]).
:- cl_make_instance([u_figurine_aardvark], _Ignored).
/*
```cl
(#-allegro class-precedence-list #+allegro aclmop:class-precedence-list
             (find-class 'figurine-aardvark))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ 'class-precedence-list',
                            ['find-class', [quote, 'figurine-aardvark']]
                          ]).
:- cl_find_class(u_figurine_aardvark, Precedence_list_Param),
   f_clos_class_precedence_list(Precedence_list_Param, _Ignored).
/*
```cl
(setf Eric (make-instance 'figurine-aardvark
                          :legs 4
                          :made-by "Jen"
                          :made-in "Brittany"
                          :aardvark-name "Eric"))

```
*/
:- lisp_compile_to_prolog(pkg_user,

                          [ setf,
                            'Eric',

                            [ 'make-instance',
                              [quote, 'figurine-aardvark'],
                              ':legs',
                              4,
                              ':made-by',
                              '$STRING'("Jen"),
                              ':made-in',
                              '$STRING'("Brittany"),
                              ':aardvark-name',
                              '$STRING'("Eric")
                            ]
                          ]).
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
/*
```cl
#+HAS_SHIFTF
(shiftf (cute-p Eric) t)

```
*/
:- lisp_compile_to_prolog(pkg_user,
                          '$COMMENT'(
                                     [ flag_removed,
                                       [+, ':HAS_SHIFTF'],
                                       [shiftf, ['cute-p', 'Eric'], t]
                                     ])).
/*
```cl
(slot-value Eric 'diet)

*/
:- lisp_compile_to_prolog(pkg_user, ['slot-value', 'Eric', [quote, diet]]).
:- get_var(TLEnv, u_eric, Eric_Get),
   cl_slot_value(Eric_Get, u_diet, _Ignored).
% 72,011,855 inferences, 13.345 CPU in 13.384 seconds (100% CPU, 5396346 Lips)
#P"/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/sanity-test.lisp"
CL-USER>
```

