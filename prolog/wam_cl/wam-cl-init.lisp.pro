#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "wam-cl-init" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Mon Jan 22 00:41:59 2018

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
(in-package #:system)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:0 **********************/
:-lisp_compile_to_prolog(pkg_user,['in-package','#:system'])
/*
% macroexpand:-[in_package,system1].
*/
/*
% into:-[eval_when,[kw_compile_toplevel,kw_load_toplevel,kw_execute],[sys_select_package,'$ARRAY'([*],claz_base_character,"SYSTEM")]].
*/
:- do_when([kw_compile_toplevel, kw_load_toplevel, kw_execute],
	   f_sys_select_package('$ARRAY'([*], claz_base_character, "SYSTEM"),
				_Ignored),
	   _Ignored).
/*
(defpackage "SYSTEM" (:nicknames "SYS"))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:24 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defpackage,'$STRING'("SYSTEM"),[':nicknames','$STRING'("SYS")]])
:- sf_defpackage('$ARRAY'([*], claz_base_character, "SYSTEM"),
		 [[kw_nicknames, '$ARRAY'([*], claz_base_character, "SYS")]],
		 _Ignored).
/*
:- side_effect(set_kind_object_slot_value(claz_package,
					  pkg_system,
					  kw_nicknames,
					  
					  [ '$ARRAY'([*],
						     claz_base_character,
						     "SYS")
					  ])).
*/
/*
(defpackage "COMMON-LISP" (:nicknames "CL" "LISP")(:uses "SYSTEM"))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:65 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defpackage,'$STRING'("COMMON-LISP"),[':nicknames','$STRING'("CL"),'$STRING'("LISP")],[':uses','$STRING'("SYSTEM")]])
:- sf_defpackage('$ARRAY'([*], claz_base_character, "COMMON-LISP"),
		 
		 [ 
		   [ kw_nicknames,
		     '$ARRAY'([*], claz_base_character, "CL"),
		     '$ARRAY'([*], claz_base_character, "LISP")
		   ],
		   [kw_uses, '$ARRAY'([*], claz_base_character, "SYSTEM")]
		 ],
		 _Ignored).
/*
:- side_effect(set_kind_object_slot_value(claz_package,
					  pkg_common_lisp,
					  kw_nicknames,
					  
					  [ '$ARRAY'([*],
						     claz_base_character,
						     "CL"),
					    '$ARRAY'([*],
						     claz_base_character,
						     "LISP")
					  ])).
*/
/*
:- side_effect(set_kind_object_slot_value(claz_package,
					  pkg_common_lisp,
					  kw_uses,
					  
					  [ '$ARRAY'([*],
						     claz_base_character,
						     "SYSTEM")
					  ])).
*/
/*
(defpackage "COMMON-LISP-USER" (:nicknames "U" "USER" "CL-USER") (:uses "COMMON-LISP"))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:133 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defpackage,'$STRING'("COMMON-LISP-USER"),[':nicknames','$STRING'("U"),'$STRING'("USER"),'$STRING'("CL-USER")],[':uses','$STRING'("COMMON-LISP")]])
:- sf_defpackage('$ARRAY'([*], claz_base_character, "COMMON-LISP-USER"),
		 
		 [ 
		   [ kw_nicknames,
		     '$ARRAY'([*], claz_base_character, "U"),
		     '$ARRAY'([*], claz_base_character, "USER"),
		     '$ARRAY'([*], claz_base_character, "CL-USER")
		   ],
		   [kw_uses, '$ARRAY'([*], claz_base_character, "COMMON-LISP")]
		 ],
		 _Ignored).
/*
:- side_effect(set_kind_object_slot_value(claz_package,
					  pkg_common_lisp_user,
					  kw_nicknames,
					  
					  [ '$ARRAY'([*],
						     claz_base_character,
						     "U"),
					    '$ARRAY'([*],
						     claz_base_character,
						     "USER"),
					    '$ARRAY'([*],
						     claz_base_character,
						     "CL-USER")
					  ])).
*/
/*
:- side_effect(set_kind_object_slot_value(claz_package,
					  pkg_common_lisp_user,
					  kw_uses,
					  
					  [ '$ARRAY'([*],
						     claz_base_character,
						     "COMMON-LISP")
					  ])).
*/
/*
(defvar *lisp-file-type* "lisp") 
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:221 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*lisp-file-type*','$STRING'("lisp")])
:- set_var(AEnv,
	   sys_xx_lisp_file_type_xx,
	   '$ARRAY'([*], claz_base_character, "lisp")).
/*
(defvar *default-pathname-defaults* #P"")
 
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:255 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defvar,'*default-pathname-defaults*','$OBJ'(claz_pathname,"")])
:- set_var(AEnv, xx_default_pathname_defaults_xx, '$OBJ'(claz_pathname, "")).
/*
(defun dd () 
 (let ((*lisp-file-type* "cl") 
        (*default-pathname-defaults* (merge-pathnames "daydreamer/"))) (load "dd")))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:299 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,dd,[],[let,[['*lisp-file-type*','$STRING'("cl")],['*default-pathname-defaults*',['merge-pathnames','$STRING'("daydreamer/")]]],[load,'$STRING'("dd")]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_dd,
					       kw_function,
					       f_sys_dd)).
*/
wl:lambda_def(defun, sys_dd, f_sys_dd, [], [[let, [[sys_xx_lisp_file_type_xx, '$ARRAY'([*], claz_base_character, "cl")], [xx_default_pathname_defaults_xx, [merge_pathnames, '$ARRAY'([*], claz_base_character, "daydreamer/")]]], [load, '$ARRAY'([*], claz_base_character, "dd")]]]).
wl:arglist_info(sys_dd, f_sys_dd, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_dd).

/*

### Compiled Function: `SYS::DD` 
*/
f_sys_dd(FnResult) :-
	CDR=[],
	catch(( ( f_merge_pathnames('$ARRAY'([*],
					     claz_base_character,
					     "daydreamer/"),
				    Xx_default_pathname_defaults_xx_Init),
		  LEnv=[bv(sys_xx_lisp_file_type_xx, '$ARRAY'([*], claz_base_character, "cl"))|CDR],
		  save_special(sv(xx_default_pathname_defaults_xx,
				  Xx_default_pathname_defaults_xx_Init,
				  symbol_value,
				  Symbol_value)),
		  f_load('$ARRAY'([*], claz_base_character, "dd"), [], LetResult),
		  restore_special(sv(xx_default_pathname_defaults_xx,
				     Xx_default_pathname_defaults_xx_Init,
				     symbol_value,
				     Symbol_value))
		),
		LetResult=FnResult
	      ),
	      block_exit(sys_dd, FnResult),
	      true).
:- set_opv(sys_dd, symbol_function, f_sys_dd),
   DefunResult=sys_dd.
/*
:- side_effect(assert_lsp(sys_dd,
			  lambda_def(defun,
				     sys_dd,
				     f_sys_dd,
				     [],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_xx_lisp_file_type_xx,
					     '$ARRAY'([*],
						      claz_base_character,
						      "cl")
					   ],
					   
					   [ xx_default_pathname_defaults_xx,
					     
					     [ merge_pathnames,
					       '$ARRAY'([*],
							claz_base_character,
							"daydreamer/")
					     ]
					   ]
					 ],
					 
					 [ load,
					   '$ARRAY'([*],
						    claz_base_character,
						    "dd")
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_dd,
			  arglist_info(sys_dd,
				       f_sys_dd,
				       [],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[],
						opt:0,
						req:0,
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_dd, init_args(x, f_sys_dd))).
*/
/*
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

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:432 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'show-ascii-art',[],['write-line','$STRING'("  __________    ")],['write-line','$STRING'(" / ___  ___ \\   ")],['write-line','$STRING'("/ / @ \\/ @ \\ \\  ")],['write-line','$STRING'("\\ \\___/\\___/ /\\ ")],['write-line','$STRING'(" \\____\\/____/|| ")],['write-line','$STRING'(" /     /\\\\\\\\\\// ")],['write-line','$STRING'("|     |\\\\\\\\\\\\   ")],['write-line','$STRING'(" \\      \\\\\\\\\\\\  ")],['write-line','$STRING'("   \\______/\\\\\\\\ ")],['write-line','$STRING'("    _||_||_     ")],['write-line','$STRING'("                ")]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_show_ascii_art,
					       kw_function,
					       f_sys_show_ascii_art)).
*/
wl:lambda_def(defun, sys_show_ascii_art, f_sys_show_ascii_art, [], [[write_line, '$ARRAY'([*], claz_base_character, "  __________    ")], [write_line, '$ARRAY'([*], claz_base_character, " / ___  ___ \\   ")], [write_line, '$ARRAY'([*], claz_base_character, "/ / @ \\/ @ \\ \\  ")], [write_line, '$ARRAY'([*], claz_base_character, "\\ \\___/\\___/ /\\ ")], [write_line, '$ARRAY'([*], claz_base_character, " \\____\\/____/|| ")], [write_line, '$ARRAY'([*], claz_base_character, " /     /\\\\\\\\\\// ")], [write_line, '$ARRAY'([*], claz_base_character, "|     |\\\\\\\\\\\\   ")], [write_line, '$ARRAY'([*], claz_base_character, " \\      \\\\\\\\\\\\  ")], [write_line, '$ARRAY'([*], claz_base_character, "   \\______/\\\\\\\\ ")], [write_line, '$ARRAY'([*], claz_base_character, "    _||_||_     ")], [write_line, '$ARRAY'([*], claz_base_character, "                ")]]).
wl:arglist_info(sys_show_ascii_art, f_sys_show_ascii_art, [], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[], opt:0, req:0, rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_show_ascii_art).

/*

### Compiled Function: `SYS::SHOW-ASCII-ART` 
*/
f_sys_show_ascii_art(FnResult) :-
	_8208=[],
	catch(( ( f_write_line('$ARRAY'([*],
					claz_base_character,
					"  __________    "),
			       Write_line_Ret),
		  f_write_line('$ARRAY'([*],
					claz_base_character,
					" / ___  ___ \\   "),
			       Write_line_Ret7),
		  f_write_line('$ARRAY'([*],
					claz_base_character,
					"/ / @ \\/ @ \\ \\  "),
			       Write_line_Ret8),
		  f_write_line('$ARRAY'([*],
					claz_base_character,
					"\\ \\___/\\___/ /\\ "),
			       Write_line_Ret9),
		  f_write_line('$ARRAY'([*],
					claz_base_character,
					" \\____\\/____/|| "),
			       Write_line_Ret10),
		  f_write_line('$ARRAY'([*],
					claz_base_character,
					" /     /\\\\\\\\\\// "),
			       Write_line_Ret11),
		  f_write_line('$ARRAY'([*],
					claz_base_character,
					"|     |\\\\\\\\\\\\   "),
			       Write_line_Ret12),
		  f_write_line('$ARRAY'([*],
					claz_base_character,
					" \\      \\\\\\\\\\\\  "),
			       Write_line_Ret13),
		  f_write_line('$ARRAY'([*],
					claz_base_character,
					"   \\______/\\\\\\\\ "),
			       Write_line_Ret14),
		  f_write_line('$ARRAY'([*],
					claz_base_character,
					"    _||_||_     "),
			       Write_line_Ret15),
		  f_write_line('$ARRAY'([*],
					claz_base_character,
					"                "),
			       Write_line_Ret16)
		),
		Write_line_Ret16=FnResult
	      ),
	      block_exit(sys_show_ascii_art, FnResult),
	      true).
:- set_opv(sys_show_ascii_art, symbol_function, f_sys_show_ascii_art),
   DefunResult=sys_show_ascii_art.
/*
:- side_effect(assert_lsp(sys_show_ascii_art,
			  lambda_def(defun,
				     sys_show_ascii_art,
				     f_sys_show_ascii_art,
				     [],
				     
				     [ 
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  "  __________    ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  " / ___  ___ \\   ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  "/ / @ \\/ @ \\ \\  ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  "\\ \\___/\\___/ /\\ ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  " \\____\\/____/|| ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  " /     /\\\\\\\\\\// ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  "|     |\\\\\\\\\\\\   ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  " \\      \\\\\\\\\\\\  ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  "   \\______/\\\\\\\\ ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  "    _||_||_     ")
				       ],
				       
				       [ write_line,
					 '$ARRAY'([*],
						  claz_base_character,
						  "                ")
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_show_ascii_art,
			  arglist_info(sys_show_ascii_art,
				       f_sys_show_ascii_art,
				       [],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[],
						opt:0,
						req:0,
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_show_ascii_art,
			  init_args(x, f_sys_show_ascii_art))).
*/
/*
(show-ascii-art)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:853 **********************/
:-lisp_compile_to_prolog(pkg_sys,['show-ascii-art'])
:- f_sys_show_ascii_art(_Ignored).
/*
(compile-file "wam-cl-init-00")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:871 **********************/
:-lisp_compile_to_prolog(pkg_sys,['compile-file','$STRING'("wam-cl-init-00")])
:- f_compile_file('$ARRAY'([*], claz_base_character, "wam-cl-init-00"),
		  [],
		  _Ignored).
/*
(load "wam-cl-init-00")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:903 **********************/
:-lisp_compile_to_prolog(pkg_sys,[load,'$STRING'("wam-cl-init-00")])
:- f_load('$ARRAY'([*], claz_base_character, "wam-cl-init-00"), [], _Ignored).
/*
(compile-file "wam-cl-init-10")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:927 **********************/
:-lisp_compile_to_prolog(pkg_sys,['compile-file','$STRING'("wam-cl-init-10")])
:- f_compile_file('$ARRAY'([*], claz_base_character, "wam-cl-init-10"),
		  [],
		  _Ignored).
/*
(compile-file "wam-cl-init-20")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:959 **********************/
:-lisp_compile_to_prolog(pkg_sys,['compile-file','$STRING'("wam-cl-init-20")])
:- f_compile_file('$ARRAY'([*], claz_base_character, "wam-cl-init-20"),
		  [],
		  _Ignored).
/*
(compile-file "wam-cl-init-30")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:991 **********************/
:-lisp_compile_to_prolog(pkg_sys,['compile-file','$STRING'("wam-cl-init-30")])
:- f_compile_file('$ARRAY'([*], claz_base_character, "wam-cl-init-30"),
		  [],
		  _Ignored).
/*
'(load "wam-cl-init2")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:1023 **********************/
:-lisp_compile_to_prolog(pkg_sys,[quote,[load,'$STRING'("wam-cl-init2")]])
/*
'(load "wam-cl-init3")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:1046 **********************/
:-lisp_compile_to_prolog(pkg_sys,[quote,[load,'$STRING'("wam-cl-init3")]])
/*
'(write-line " WAM CommonLisp ")
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:1069 **********************/
:-lisp_compile_to_prolog(pkg_sys,[quote,['write-line','$STRING'(" WAM CommonLisp ")]])
/*
'(read-eval-print-loop)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:1102 **********************/
:-lisp_compile_to_prolog(pkg_sys,[quote,['read-eval-print-loop']])

%; Total compilation time: 1.115 seconds

