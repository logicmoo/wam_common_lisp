#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "wam-cl-init-00" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Tue Jan 23 17:36:40 2018

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
;; #+BUILTIN Means to ignore since it should already be defined
*/
/*
;; #+WAM-CL Means we want it
*/
/*
;; #+LISP500 Means probably we dont want it
*/
/*
;; #+ALT Alternative definition
*/
/*
;; #+ABCL From ABCL
*/
/*
;; #+SBCL From SBCL
*/
/*
;; #+ECL From ECL
*/
/*
;; #+SICL From SICL
*/
/*
(in-package "SYSTEM")


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:262 **********************/
:-lisp_compile_to_prolog(pkg_sys,['in-package','#:system'])
/*
% macroexpand:-[in_package,system2].
*/
/*
% into:-[eval_when,[kw_compile_toplevel,kw_load_toplevel,kw_execute],[sys_select_package,'$ARRAY'([*],claz_base_character,"SYSTEM")]].
*/
:- do_when([kw_compile_toplevel, kw_load_toplevel, kw_execute],
	   f_sys_select_package('$ARRAY'([*], claz_base_character, "SYSTEM"),
				_Ignored),
	   _Ignored).
/*
#-WAM-CL (defmacro put-sysprop (s p v) `(setf (get ,s ,p) ,v ))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:294 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[-,':WAM-CL'],[defmacro,'put-sysprop',[s,p,v],['#BQ',[setf,[get,['#COMMA',s],['#COMMA',p]],['#COMMA',v]]]]]))
/*
(defclass pathname ()
  ((host      :accessor pathname-host
              :initarg :host
              :initform nil)
   (device    :accessor pathname-device
              :initarg :device
              :initform :unspecific)
   (directory :accessor pathname-directory
              :initarg :directory
              :initform nil)
   (name      :accessor pathname-name
              :initarg :name
              :initform nil)
   (type      :accessor pathname-type
              :initarg :type
              :initform nil)
   (version   :accessor pathname-version
              :initarg :version
              :initform nil))
  (:documentation "A physical pathname."))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:363 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defclass,pathname,[],[[host,':accessor','pathname-host',':initarg',':host',':initform',[]],[device,':accessor','pathname-device',':initarg',':device',':initform',':unspecific'],[directory,':accessor','pathname-directory',':initarg',':directory',':initform',[]],[name,':accessor','pathname-name',':initarg',':name',':initform',[]],[type,':accessor','pathname-type',':initarg',':type',':initform',[]],[version,':accessor','pathname-version',':initarg',':version',':initform',[]]],[':documentation','$STRING'("A physical pathname.")]])
:- sf_defclass(Sf_defclass_Param,
	       
	       [ pathname,
		 [],
		 
		 [ 
		   [ sys_host,
		     kw_accessor,
		     pathname_host,
		     kw_initarg,
		     kw_host,
		     kw_initform,
		     []
		   ],
		   
		   [ sys_device,
		     kw_accessor,
		     pathname_device,
		     kw_initarg,
		     kw_device,
		     kw_initform,
		     kw_unspecific
		   ],
		   
		   [ directory,
		     kw_accessor,
		     pathname_directory,
		     kw_initarg,
		     kw_directory,
		     kw_initform,
		     []
		   ],
		   
		   [ sys_name,
		     kw_accessor,
		     pathname_name,
		     kw_initarg,
		     kw_name,
		     kw_initform,
		     []
		   ],
		   
		   [ type,
		     kw_accessor,
		     pathname_type,
		     kw_initarg,
		     kw_type,
		     kw_initform,
		     []
		   ],
		   
		   [ sys_version,
		     kw_accessor,
		     pathname_version,
		     kw_initarg,
		     kw_version,
		     kw_initform,
		     []
		   ]
		 ],
		 
		 [ kw_documentation,
		   '$ARRAY'([*], claz_base_character, "A physical pathname.")
		 ]
	       ],
	       _Ignored).
/*
(defmethod print-object ((self pathname) stream)
  (format stream ""(defmethod print-object ((self pathname) stream)\r\n  (format stream \"~:[~;#P\\\"~]~A~0@*~:[~;\\\"~]\" *print-escape* (namestring self))\r\n  self)\r\n\r\n\r\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:1055 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmethod,'print-object',[[self,pathname],stream],[format,stream,'$STRING'("~:[~;#P\"~]~A~0@*~:[~;\"~]"),'*print-escape*',[namestring,self]],self])
 defmethod(print_object,
	     [[sys_self, pathname], stream],
	     
	     [ 
	       [ format,
		 stream,
		 '$ARRAY'([*],
			  claz_base_character,
			  "~:[~;#P\"~]~A~0@*~:[~;\"~]"),
		 xx_print_escape_xx,
		 [namestring, sys_self]
	       ],
	       sys_self
	     ]).

/*
(defmacro defun=sourceinfo (name ll &rest body)
  "Used to show what was already compiled"
   `(put-sysprop ',name 'defun=sourceinfo `(defun ,',name ,',ll ,',@body)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:1199 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'defun=sourceinfo',[name,ll,'&rest',body],'$STRING'("Used to show what was already compiled"),['#BQ',['put-sysprop',[quote,['#COMMA',name]],[quote,'defun=sourceinfo'],['#BQ',[defun,['#COMMA',[quote,['#COMMA',name]]],['#COMMA',[quote,['#COMMA',ll]]],['#COMMA',[quote,['#BQ-COMMA-ELIPSE',body]]]]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_defun_c61_sourceinfo,
					       kw_macro,
					       mf_sys_defun_c61_sourceinfo)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_defun_c61_sourceinfo,
					       kw_special,
					       sf_sys_defun_c61_sourceinfo)).
*/
doc: doc_string(sys_defun_c61_sourceinfo,
	      _7912,
	      function,
	      "Used to show what was already compiled").

wl:lambda_def(defmacro, sys_defun_c61_sourceinfo, mf_sys_defun_c61_sourceinfo, [sys_name, sys_ll, c38_rest, sys_body], [['#BQ', [sys_put_sysprop, [quote, ['#COMMA', sys_name]], [quote, sys_defun_c61_sourceinfo], ['#BQ', [defun, ['#COMMA', [quote, ['#COMMA', sys_name]]], ['#COMMA', [quote, ['#COMMA', sys_ll]]], ['#COMMA', [quote, ['#BQ-COMMA-ELIPSE', sys_body]]]]]]]]).
wl:arglist_info(sys_defun_c61_sourceinfo, mf_sys_defun_c61_sourceinfo, [sys_name, sys_ll, c38_rest, sys_body], arginfo{all:[sys_name, sys_ll], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_name, sys_ll, sys_body], opt:0, req:[sys_name, sys_ll], rest:[sys_body], sublists:0, whole:0}).
wl: init_args(2, mf_sys_defun_c61_sourceinfo).

/*

### Compiled Macro Operator: `SYS::DEFUN=SOURCEINFO` 
*/
sf_sys_defun_c61_sourceinfo(MacroEnv, Name_In, Ll_In, RestNKeys, FResult) :-
	mf_sys_defun_c61_sourceinfo(
				    [ sys_defun_c61_sourceinfo,
				      Name_In,
				      Ll_In
				    | RestNKeys
				    ],
				    MacroEnv,
				    MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `SYS::DEFUN=SOURCEINFO` 
*/
mf_sys_defun_c61_sourceinfo([sys_defun_c61_sourceinfo, Name_In, Ll_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_name, Name_In), bv(sys_ll, Ll_In), bv(sys_body, RestNKeys)],
	catch(( ( ( get_var(GEnv, sys_body, Body_Get),
		    get_var(GEnv, sys_ll, Ll_Get)
		  ),
		  get_var(GEnv, sys_name, Name_Get8)
		),
		[sys_put_sysprop, [quote, Name_Get8], [quote, sys_defun_c61_sourceinfo], ['#BQ', [defun, ['#COMMA', [quote, Name_Get8]], ['#COMMA', [quote, Ll_Get]], ['#COMMA', [quote|Body_Get]]]]]=MFResult
	      ),
	      block_exit(sys_defun_c61_sourceinfo, MFResult),
	      true).
:- set_opv(mf_sys_defun_c61_sourceinfo, type_of, sys_macro),
   set_opv(sys_defun_c61_sourceinfo,
	   symbol_function,
	   mf_sys_defun_c61_sourceinfo),
   DefMacroResult=sys_defun_c61_sourceinfo.
/*
:- side_effect(assert_lsp(sys_defun_c61_sourceinfo,
			  doc_string(sys_defun_c61_sourceinfo,
				     _7910,
				     function,
				     "Used to show what was already compiled"))).
*/
/*
:- side_effect(assert_lsp(sys_defun_c61_sourceinfo,
			  lambda_def(defmacro,
				     sys_defun_c61_sourceinfo,
				     mf_sys_defun_c61_sourceinfo,
				     [sys_name, sys_ll, c38_rest, sys_body],
				     
				     [ 
				       [ '#BQ',
					 
					 [ sys_put_sysprop,
					   [quote, ['#COMMA', sys_name]],
					   [quote, sys_defun_c61_sourceinfo],
					   
					   [ '#BQ',
					     
					     [ defun,
					       
					       [ '#COMMA',
						 [quote, ['#COMMA', sys_name]]
					       ],
					       
					       [ '#COMMA',
						 [quote, ['#COMMA', sys_ll]]
					       ],
					       
					       [ '#COMMA',
						 
						 [ quote,
						   
						   [ '#BQ-COMMA-ELIPSE',
						     sys_body
						   ]
						 ]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_defun_c61_sourceinfo,
			  arglist_info(sys_defun_c61_sourceinfo,
				       mf_sys_defun_c61_sourceinfo,
				       [sys_name, sys_ll, c38_rest, sys_body],
				       arginfo{ all:[sys_name, sys_ll],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_name,
							sys_ll,
							sys_body
						      ],
						opt:0,
						req:[sys_name, sys_ll],
						rest:[sys_body],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_defun_c61_sourceinfo,
			  init_args(2, mf_sys_defun_c61_sourceinfo))).
*/
/*
(defmacro defmacro=sourceinfo (name ll &rest body)
   "Used to show what was already compiled"
   `(put-sysprop ',name 'defmacro=sourceinfo '(defmacro ,name ,ll ,@body)))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:1371 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'defmacro=sourceinfo',[name,ll,'&rest',body],'$STRING'("Used to show what was already compiled"),['#BQ',['put-sysprop',[quote,['#COMMA',name]],[quote,'defmacro=sourceinfo'],[quote,[defmacro,['#COMMA',name],['#COMMA',ll],['#BQ-COMMA-ELIPSE',body]]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_defmacro_c61_sourceinfo,
					       kw_macro,
					       mf_sys_defmacro_c61_sourceinfo)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_defmacro_c61_sourceinfo,
					       kw_special,
					       sf_sys_defmacro_c61_sourceinfo)).
*/
doc: doc_string(sys_defmacro_c61_sourceinfo,
	      _7626,
	      function,
	      "Used to show what was already compiled").

wl:lambda_def(defmacro, sys_defmacro_c61_sourceinfo, mf_sys_defmacro_c61_sourceinfo, [sys_name, sys_ll, c38_rest, sys_body], [['#BQ', [sys_put_sysprop, [quote, ['#COMMA', sys_name]], [quote, sys_defmacro_c61_sourceinfo], [quote, [defmacro, ['#COMMA', sys_name], ['#COMMA', sys_ll], ['#BQ-COMMA-ELIPSE', sys_body]]]]]]).
wl:arglist_info(sys_defmacro_c61_sourceinfo, mf_sys_defmacro_c61_sourceinfo, [sys_name, sys_ll, c38_rest, sys_body], arginfo{all:[sys_name, sys_ll], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_name, sys_ll, sys_body], opt:0, req:[sys_name, sys_ll], rest:[sys_body], sublists:0, whole:0}).
wl: init_args(2, mf_sys_defmacro_c61_sourceinfo).

/*

### Compiled Macro Operator: `SYS::DEFMACRO=SOURCEINFO` 
*/
sf_sys_defmacro_c61_sourceinfo(MacroEnv, Name_In, Ll_In, RestNKeys, FResult) :-
	mf_sys_defmacro_c61_sourceinfo(
				       [ sys_defmacro_c61_sourceinfo,
					 Name_In,
					 Ll_In
				       | RestNKeys
				       ],
				       MacroEnv,
				       MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `SYS::DEFMACRO=SOURCEINFO` 
*/
mf_sys_defmacro_c61_sourceinfo([sys_defmacro_c61_sourceinfo, Name_In, Ll_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_name, Name_In), bv(sys_ll, Ll_In), bv(sys_body, RestNKeys)],
	catch(( ( ( get_var(GEnv, sys_body, Body_Get),
		    get_var(GEnv, sys_ll, Ll_Get)
		  ),
		  get_var(GEnv, sys_name, Name_Get8)
		),
		[sys_put_sysprop, [quote, Name_Get8], [quote, sys_defmacro_c61_sourceinfo], [quote, [defmacro, Name_Get8, Ll_Get|Body_Get]]]=MFResult
	      ),
	      block_exit(sys_defmacro_c61_sourceinfo, MFResult),
	      true).
:- set_opv(mf_sys_defmacro_c61_sourceinfo, type_of, sys_macro),
   set_opv(sys_defmacro_c61_sourceinfo,
	   symbol_function,
	   mf_sys_defmacro_c61_sourceinfo),
   DefMacroResult=sys_defmacro_c61_sourceinfo.
/*
:- side_effect(assert_lsp(sys_defmacro_c61_sourceinfo,
			  doc_string(sys_defmacro_c61_sourceinfo,
				     _7626,
				     function,
				     "Used to show what was already compiled"))).
*/
/*
:- side_effect(assert_lsp(sys_defmacro_c61_sourceinfo,
			  lambda_def(defmacro,
				     sys_defmacro_c61_sourceinfo,
				     mf_sys_defmacro_c61_sourceinfo,
				     [sys_name, sys_ll, c38_rest, sys_body],
				     
				     [ 
				       [ '#BQ',
					 
					 [ sys_put_sysprop,
					   [quote, ['#COMMA', sys_name]],
					   [quote, sys_defmacro_c61_sourceinfo],
					   
					   [ quote,
					     
					     [ defmacro,
					       ['#COMMA', sys_name],
					       ['#COMMA', sys_ll],
					       ['#BQ-COMMA-ELIPSE', sys_body]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_defmacro_c61_sourceinfo,
			  arglist_info(sys_defmacro_c61_sourceinfo,
				       mf_sys_defmacro_c61_sourceinfo,
				       [sys_name, sys_ll, c38_rest, sys_body],
				       arginfo{ all:[sys_name, sys_ll],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_name,
							sys_ll,
							sys_body
						      ],
						opt:0,
						req:[sys_name, sys_ll],
						rest:[sys_body],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_defmacro_c61_sourceinfo,
			  init_args(2, mf_sys_defmacro_c61_sourceinfo))).
*/
/*
(defmacro assert (test-form &optional places string &rest args)
  (declare (dynamic-extent args))
  `(do nil (,test-form nil)
     (multiple-value-setq
	 ,places
       (apply 'assert-places ',places (list ,@places)
	      ,@(if string `(,string (list ,@args)) `("The assertion "(defmacro assert (test-form &optional places string &rest args)\r\n  (declare (dynamic-extent args))\r\n  `(do nil (,test-form nil)\r\n     (multiple-value-setq\r\n\t ,places\r\n       (apply 'assert-places ',places (list ,@places)\r\n\t      ,@(if string `(,string (list ,@args)) `(\"The assertion ~:@(~S~) failed.\" ',test-form nil))))))\r\n\r\n\r\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:1549 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,assert,['test-form','&optional',places,string,'&rest',args],[declare,['dynamic-extent',args]],['#BQ',[do,[],[['#COMMA','test-form'],[]],['multiple-value-setq',['#COMMA',places],[apply,[quote,'assert-places'],[quote,['#COMMA',places]],[list,['#BQ-COMMA-ELIPSE',places]],['#BQ-COMMA-ELIPSE',[if,string,['#BQ',[['#COMMA',string],[list,['#BQ-COMMA-ELIPSE',args]]]],['#BQ',['$STRING'("The assertion ~:@(~S~) failed."),[quote,['#COMMA','test-form']],[]]]]]]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       assert,
					       kw_special,
					       sf_assert)).
*/
wl:lambda_def(defmacro, assert, mf_assert, [sys_test_form, c38_optional, sys_places, string, c38_rest, sys_args], [[declare, [dynamic_extent, sys_args]], ['#BQ', [do, [], [['#COMMA', sys_test_form], []], [multiple_value_setq, ['#COMMA', sys_places], [apply, [quote, sys_assert_places], [quote, ['#COMMA', sys_places]], [list, ['#BQ-COMMA-ELIPSE', sys_places]], ['#BQ-COMMA-ELIPSE', [if, string, ['#BQ', [['#COMMA', string], [list, ['#BQ-COMMA-ELIPSE', sys_args]]]], ['#BQ', ['$ARRAY'([*], claz_base_character, "The assertion ~:@(~S~) failed."), [quote, ['#COMMA', sys_test_form]], []]]]]]]]]]).
wl:arglist_info(assert, mf_assert, [sys_test_form, c38_optional, sys_places, string, c38_rest, sys_args], arginfo{all:[sys_test_form, sys_places, string], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_test_form, sys_places, string, sys_args], opt:[sys_places, string], req:[sys_test_form], rest:[sys_args], sublists:0, whole:0}).
wl: init_args(1, mf_assert).

/*

### Compiled Macro Operator: `CL:ASSERT` 
*/
sf_assert(MacroEnv, Test_form_In, Optionals, FResult) :-
	mf_assert([assert, Test_form_In|Optionals], MacroEnv, MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:ASSERT` 
*/
mf_assert([assert, Test_form_In|Optionals], MacroEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_test_form, Test_form_In), bv(sys_places, Places_In), bv(string, String_In), bv(sys_args, Optionals)],
	opt_var(MacroEnv, sys_places, Places_In, true, [], 1, Optionals),
	opt_var(MacroEnv, string, String_In, true, [], 2, Optionals),
	catch(( ( sf_declare(GEnv, [dynamic_extent, sys_args], Sf_declare_Ret),
		  get_var(GEnv, sys_places, Places_Get),
		  ( get_var(GEnv, string, IFTEST),
		    get_var(GEnv, sys_test_form, Test_form_Get)
		  ),
		  get_var(GEnv, sys_places, Places_Get11),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, string, String_Get16),
		      get_var(GEnv, sys_args, Args_Get),
		      CDR=[String_Get16, [list|Args_Get]]
		  ;   get_var(GEnv, sys_test_form, Test_form_Get18),
		      CDR=['$ARRAY'([*], claz_base_character, "The assertion ~:@(~S~) failed."), [quote, Test_form_Get18], []]
		  )
		),
		[do, [], [Test_form_Get, []], [multiple_value_setq, Places_Get, [apply, [quote, sys_assert_places], [quote, Places_Get11], [list|Places_Get11]|CDR]]]=MFResult
	      ),
	      block_exit(assert, MFResult),
	      true).
:- set_opv(mf_assert, type_of, sys_macro),
   set_opv(assert, symbol_function, mf_assert),
   DefMacroResult=assert.
/*
:- side_effect(assert_lsp(assert,
			  lambda_def(defmacro,
				     assert,
				     mf_assert,
				     
				     [ sys_test_form,
				       c38_optional,
				       sys_places,
				       string,
				       c38_rest,
				       sys_args
				     ],
				     
				     [ [declare, [dynamic_extent, sys_args]],
				       
				       [ '#BQ',
					 
					 [ do,
					   [],
					   [['#COMMA', sys_test_form], []],
					   
					   [ multiple_value_setq,
					     ['#COMMA', sys_places],
					     
					     [ apply,
					       [quote, sys_assert_places],
					       [quote, ['#COMMA', sys_places]],
					       
					       [ list,
						 
						 [ '#BQ-COMMA-ELIPSE',
						   sys_places
						 ]
					       ],
					       
					       [ '#BQ-COMMA-ELIPSE',
						 
						 [ if,
						   string,
						   
						   [ '#BQ',
						     
						     [ ['#COMMA', string],
						       
						       [ list,
							 
							 [ '#BQ-COMMA-ELIPSE',
							   sys_args
							 ]
						       ]
						     ]
						   ],
						   
						   [ '#BQ',
						     
						     [ '$ARRAY'([*],
								claz_base_character,
								"The assertion ~:@(~S~) failed."),
						       
						       [ quote,
							 
							 [ '#COMMA',
							   sys_test_form
							 ]
						       ],
						       []
						     ]
						   ]
						 ]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(assert,
			  arglist_info(assert,
				       mf_assert,
				       
				       [ sys_test_form,
					 c38_optional,
					 sys_places,
					 string,
					 c38_rest,
					 sys_args
				       ],
				       arginfo{ all:
						    [ sys_test_form,
						      sys_places,
						      string
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_test_form,
							sys_places,
							string,
							sys_args
						      ],
						opt:[sys_places, string],
						req:[sys_test_form],
						rest:[sys_args],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(assert, init_args(1, mf_assert))).
*/
/*
(defmacro eval-when-tl ((&rest when) &body body) (if (or (member 'eval when) (member ':execute when)) `(progn ,@body) nil)) 

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:1878 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'eval-when-tl',[['&rest',when],'&body',body],[if,[or,[member,[quote,eval],when],[member,[quote,':execute'],when]],['#BQ',[progn,['#BQ-COMMA-ELIPSE',body]]],[]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_eval_when_tl,
					       kw_macro,
					       mf_sys_eval_when_tl)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_eval_when_tl,
					       kw_special,
					       sf_sys_eval_when_tl)).
*/
wl:lambda_def(defmacro, sys_eval_when_tl, mf_sys_eval_when_tl, [[c38_rest, when], c38_body, sys_body], [[if, [or, [member, [quote, eval], when], [member, [quote, kw_execute], when]], ['#BQ', [progn, ['#BQ-COMMA-ELIPSE', sys_body]]], []]]).
wl:arglist_info(sys_eval_when_tl, mf_sys_eval_when_tl, [[c38_rest, when], c38_body, sys_body], arginfo{all:0, allow_other_keys:0, aux:0, body:[sys_body], complex:[body], env:0, key:0, names:[sys_body, when], opt:0, req:0, rest:[sys_body], sublists:0, whole:0}).
wl: init_args(1, mf_sys_eval_when_tl).

/*

### Compiled Macro Operator: `SYS::EVAL-WHEN-TL` 
*/
sf_sys_eval_when_tl(SubEnv, When_In, RestNKeys, FResult) :-
	mf_sys_eval_when_tl([sys_eval_when_tl, When_In|RestNKeys],
			    SubEnv,
			    MFResult),
	f_sys_env_eval(SubEnv, MFResult, FResult).
/*

### Compiled Macro Function: `SYS::EVAL-WHEN-TL` 
*/
mf_sys_eval_when_tl([sys_eval_when_tl, When_In|RestNKeys], SubEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_body, Body_In), bv(when, When_In)],
	as_body(sys_body, Body_In, RestNKeys),
	catch(( ( (   get_var(GEnv, when, When_Get),
		      f_member(eval, When_Get, [], FORM1_Res),
		      FORM1_Res\==[],
		      IFTEST=FORM1_Res
		  ->  true
		  ;   get_var(GEnv, when, When_Get10),
		      f_member(kw_execute, When_Get10, [], Member_Ret),
		      IFTEST=Member_Ret
		  ),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, sys_body, Body_Get),
		      _7212=[progn|Body_Get]
		  ;   _7212=[]
		  )
		),
		_7212=MFResult
	      ),
	      block_exit(sys_eval_when_tl, MFResult),
	      true).
:- set_opv(mf_sys_eval_when_tl, type_of, sys_macro),
   set_opv(sys_eval_when_tl, symbol_function, mf_sys_eval_when_tl),
   DefMacroResult=sys_eval_when_tl.
/*
:- side_effect(assert_lsp(sys_eval_when_tl,
			  lambda_def(defmacro,
				     sys_eval_when_tl,
				     mf_sys_eval_when_tl,
				     [[c38_rest, when], c38_body, sys_body],
				     
				     [ 
				       [ if,
					 
					 [ or,
					   [member, [quote, eval], when],
					   [member, [quote, kw_execute], when]
					 ],
					 
					 [ '#BQ',
					   
					   [ progn,
					     ['#BQ-COMMA-ELIPSE', sys_body]
					   ]
					 ],
					 []
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_eval_when_tl,
			  arglist_info(sys_eval_when_tl,
				       mf_sys_eval_when_tl,
				       [[c38_rest, when], c38_body, sys_body],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:[sys_body],
						complex:[body],
						env:0,
						key:0,
						names:[sys_body, when],
						opt:0,
						req:0,
						rest:[sys_body],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_eval_when_tl, init_args(1, mf_sys_eval_when_tl))).
*/
/*
(defun list* (arg &rest others)
  "Return a list of the arguments with last cons a dotted pair"
  (cond ((null others) arg)
	((null (cdr others)) (cons arg (car others)))
	(t (do ((x others (cdr x)))
	       ((null (cddr x)) (rplacd x (cadr x))))
	   (cons arg others))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:2006 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'list*',[arg,'&rest',others],'$STRING'("Return a list of the arguments with last cons a dotted pair"),[cond,[[null,others],arg],[[null,[cdr,others]],[cons,arg,[car,others]]],[t,[do,[[x,others,[cdr,x]]],[[null,[cddr,x]],[rplacd,x,[cadr,x]]]],[cons,arg,others]]]])
doc: doc_string(list_xx,
	      _8328,
	      function,
	      "Return a list of the arguments with last cons a dotted pair").

wl:lambda_def(defun, list_xx, f_list_xx, [sys_arg, c38_rest, sys_others], [[cond, [[null, sys_others], sys_arg], [[null, [cdr, sys_others]], [cons, sys_arg, [car, sys_others]]], [t, [do, [[sys_x, sys_others, [cdr, sys_x]]], [[null, [cddr, sys_x]], [rplacd, sys_x, [cadr, sys_x]]]], [cons, sys_arg, sys_others]]]]).
wl:arglist_info(list_xx, f_list_xx, [sys_arg, c38_rest, sys_others], arginfo{all:[sys_arg], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_arg, sys_others], opt:0, req:[sys_arg], rest:[sys_others], sublists:0, whole:0}).
wl: init_args(1, f_list_xx).

/*

### Compiled Function: `CL:LIST*` 
*/
f_list_xx(Arg_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_arg, Arg_In), bv(sys_others, RestNKeys)],
	catch(( ( get_var(GEnv, sys_others, IFTEST),
		  (   IFTEST==[]
		  ->  get_var(GEnv, sys_arg, Arg_Get),
		      _8408=Arg_Get
		  ;   get_var(GEnv, sys_others, Others_Get12),
		      f_cdr(Others_Get12, IFTEST10),
		      (   IFTEST10==[]
		      ->  get_var(GEnv, sys_arg, Arg_Get13),
			  get_var(GEnv, sys_others, Others_Get14),
			  f_car(Others_Get14, Car_Ret),
			  TrueResult52=[Arg_Get13|Car_Ret],
			  ElseResult55=TrueResult52
		      ;   get_var(GEnv, sys_others, Others_Get18),
			  AEnv=[bv(sys_x, Others_Get18)|GEnv],
			  catch(( call_addr_block(AEnv,
						  (push_label(do_label_1), get_var(AEnv, sys_x, X_Get38), f_cddr(X_Get38, IFTEST36), (IFTEST36==[]->get_var(AEnv, sys_x, X_Get41), f_cadr(X_Get41, Cadr_Ret), f_rplacd(X_Get41, Cadr_Ret, RetResult39), throw(block_exit([], RetResult39)), _TBResult=ThrowResult40;get_var(AEnv, sys_x, X_Get45), f_cdr(X_Get45, X), set_var(AEnv, sys_x, X), goto(do_label_1, AEnv), _TBResult=_GORES46)),
						  
						  [ addr(addr_tagbody_1_do_label_1,
							 do_label_1,
							 '$unused',
							 AEnv,
							 (get_var(AEnv, sys_x, Cddr_Param), f_cddr(Cddr_Param, IFTEST21), (IFTEST21==[]->get_var(AEnv, sys_x, X_Get26), f_cadr(X_Get26, Cadr_Ret63), f_rplacd(X_Get26, Cadr_Ret63, Rplacd_Ret), throw(block_exit([], Rplacd_Ret)), _9396=ThrowResult;get_var(AEnv, sys_x, X_Get30), f_cdr(X_Get30, Cdr_Ret), set_var(AEnv, sys_x, Cdr_Ret), goto(do_label_1, AEnv), _9396=_GORES)))
						  ]),
				  []=LetResult
				),
				block_exit([], LetResult),
				true),
			  get_var(GEnv, sys_arg, Arg_Get50),
			  get_var(GEnv, sys_others, Others_Get51),
			  ElseResult53=[Arg_Get50|Others_Get51],
			  ElseResult55=ElseResult53
		      ),
		      _8408=ElseResult55
		  )
		),
		_8408=FnResult
	      ),
	      block_exit(list_xx, FnResult),
	      true).
:- set_opv(list_xx, symbol_function, f_list_xx),
   DefunResult=list_xx.
/*
:- side_effect(assert_lsp(list_xx,
			  doc_string(list_xx,
				     _8328,
				     function,
				     "Return a list of the arguments with last cons a dotted pair"))).
*/
/*
:- side_effect(assert_lsp(list_xx,
			  lambda_def(defun,
				     list_xx,
				     f_list_xx,
				     [sys_arg, c38_rest, sys_others],
				     
				     [ 
				       [ cond,
					 [[null, sys_others], sys_arg],
					 
					 [ [null, [cdr, sys_others]],
					   [cons, sys_arg, [car, sys_others]]
					 ],
					 
					 [ t,
					   
					   [ do,
					     [[sys_x, sys_others, [cdr, sys_x]]],
					     
					     [ [null, [cddr, sys_x]],
					       [rplacd, sys_x, [cadr, sys_x]]
					     ]
					   ],
					   [cons, sys_arg, sys_others]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(list_xx,
			  arglist_info(list_xx,
				       f_list_xx,
				       [sys_arg, c38_rest, sys_others],
				       arginfo{ all:[sys_arg],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_arg, sys_others],
						opt:0,
						req:[sys_arg],
						rest:[sys_others],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(list_xx, init_args(1, f_list_xx))).
*/
/*
(defmacro psetq (&environment env &rest args)
  (do ((l args (cddr l))
       (forms nil)
       (bindings nil))
    ((endp l) (list* 'let* (reverse bindings) (reverse (cons nil forms))))
    (if (and (symbolp (car l))
             (eq (car l) (macroexpand-1 (car l) env)))
        (let ((sym (gensym)))
          (push (list sym (cadr l)) bindings)
          (push (list 'setq (car l) sym) forms))
        (multiple-value-bind
              (dummies vals newval setter getter)
            (get-setf-expansion (macroexpand-1 (car l) env) env)
          (declare (ignore getter))
          (do ((d dummies (cdr d))
               (v vals (cdr v)))
              ((null d))
            (push (list (car d) (car v)) bindings))
          (push (list (car newval) (cadr l)) bindings)
          (push setter forms)))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:2289 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,psetq,['&environment',env,'&rest',args],[do,[[l,args,[cddr,l]],[forms,[]],[bindings,[]]],[[endp,l],['list*',[quote,'let*'],[reverse,bindings],[reverse,[cons,[],forms]]]],[if,[and,[symbolp,[car,l]],[eq,[car,l],['macroexpand-1',[car,l],env]]],[let,[[sym,[gensym]]],[push,[list,sym,[cadr,l]],bindings],[push,[list,[quote,setq],[car,l],sym],forms]],['multiple-value-bind',[dummies,vals,newval,setter,getter],['get-setf-expansion',['macroexpand-1',[car,l],env],env],[declare,[ignore,getter]],[do,[[d,dummies,[cdr,d]],[v,vals,[cdr,v]]],[[null,d]],[push,[list,[car,d],[car,v]],bindings]],[push,[list,[car,newval],[cadr,l]],bindings],[push,setter,forms]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       psetq,
					       kw_special,
					       sf_psetq)).
*/
wl:lambda_def(defmacro, psetq, mf_psetq, [c38_environment, sys_env, c38_rest, sys_args], [[do, [[sys_l, sys_args, [cddr, sys_l]], [sys_forms, []], [sys_bindings, []]], [[endp, sys_l], [list_xx, [quote, let_xx], [reverse, sys_bindings], [reverse, [cons, [], sys_forms]]]], [if, [and, [symbolp, [car, sys_l]], [eq, [car, sys_l], [macroexpand_1, [car, sys_l], sys_env]]], [let, [[sys_sym, [gensym]]], [push, [list, sys_sym, [cadr, sys_l]], sys_bindings], [push, [list, [quote, setq], [car, sys_l], sys_sym], sys_forms]], [multiple_value_bind, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter], [get_setf_expansion, [macroexpand_1, [car, sys_l], sys_env], sys_env], [declare, [ignore, sys_getter]], [do, [[sys_d, sys_dummies, [cdr, sys_d]], [sys_v, sys_vals, [cdr, sys_v]]], [[null, sys_d]], [push, [list, [car, sys_d], [car, sys_v]], sys_bindings]], [push, [list, [car, sys_newval], [cadr, sys_l]], sys_bindings], [push, sys_setter, sys_forms]]]]]).
wl:arglist_info(psetq, mf_psetq, [c38_environment, sys_env, c38_rest, sys_args], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[environment, rest], env:[sys_env], key:0, names:[sys_env, sys_args], opt:0, req:0, rest:[sys_args], sublists:0, whole:0}).
wl: init_args(0, mf_psetq).

/*

### Compiled Macro Operator: `CL:PSETQ` 
*/
sf_psetq(Env_In, RestNKeys, FResult) :-
	mf_psetq([psetq|RestNKeys], Env_In, MFResult),
	f_sys_env_eval(Env_In, MFResult, FResult).
/*

### Compiled Macro Function: `CL:PSETQ` 
*/
mf_psetq([psetq|RestNKeys], Env_In, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_env, Env_In), bv(sys_args, RestNKeys)],
	catch(( ( get_var(GEnv, sys_args, Args_Get),
		  AEnv=[bv(sys_l, Args_Get), bv(sys_forms, []), bv(sys_bindings, [])|GEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_2), get_var(AEnv, sys_l, L_Get84), (s3q:is_endp(L_Get84)->get_var(AEnv, sys_bindings, Bindings_Get89), f_reverse(Bindings_Get89, Reverse_Ret), get_var(AEnv, sys_forms, Forms_Get90), Reverse_Param=[[]|Forms_Get90], f_reverse(Reverse_Param, Reverse_Ret171), f_list_xx(let_xx, [Reverse_Ret, Reverse_Ret171], RetResult87), throw(block_exit([], RetResult87)), _TBResult=ThrowResult88;get_var(AEnv, sys_l, L_Get95), f_car(L_Get95, PredArgResult97), (is_symbolp(PredArgResult97)->get_var(AEnv, sys_l, L_Get98), f_car(L_Get98, Eq_Param), get_var(AEnv, sys_l, L_Get100), f_car(L_Get100, Car_Ret), get_var(AEnv, sys_env, Env_Get99), f_macroexpand_1([Car_Ret, Env_Get99], Macroexpand_1_Ret), f_eq(Eq_Param, Macroexpand_1_Ret, TrueResult101), IFTEST92=TrueResult101;IFTEST92=[]), (IFTEST92\==[]->f_gensym(Sym_Init105), LEnv104=[bv(sys_sym, Sym_Init105)|AEnv], sf_push(LEnv104, [list, sys_sym, [cadr, sys_l]], sys_bindings, Bindings), sf_push(LEnv104, [list, [quote, setq], [car, sys_l], sys_sym], sys_forms, LetResult103), _10376=LetResult103;LEnv108=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_newval, []), bv(sys_setter, []), bv(sys_getter, [])|AEnv], get_var(LEnv108, sys_l, L_Get110), f_car(L_Get110, Car_Ret174), get_var(LEnv108, sys_env, Env_Get109), f_macroexpand_1([Car_Ret174, Env_Get109], Setf_expansion_Param), get_var(LEnv108, sys_env, Env_Get111), f_get_setf_expansion(Setf_expansion_Param, [Env_Get111], Setf_expansion_Ret), setq_from_values(LEnv108, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter]), sf_declare(LEnv108, [ignore, sys_getter], Sf_declare_Ret), get_var(LEnv108, sys_dummies, Dummies_Get115), get_var(LEnv108, sys_vals, Vals_Get116), BlockExitEnv=[bv(sys_d, Dummies_Get115), bv(sys_v, Vals_Get116)|LEnv108], catch((call_addr_block(BlockExitEnv,  (push_label(do_label_4), get_var(BlockExitEnv, sys_d, IFTEST133), (IFTEST133==[]->throw(block_exit([], [])), _TBResult119=ThrowResult137;sf_push(BlockExitEnv, [list, [car, sys_d], [car, sys_v]], sys_bindings, Bindings157), get_var(BlockExitEnv, sys_d, D_Get139), f_cdr(D_Get139, D), get_var(BlockExitEnv, sys_v, V_Get140), f_cdr(V_Get140, V), set_var(BlockExitEnv, sys_d, D), set_var(BlockExitEnv, sys_v, V), goto(do_label_4, BlockExitEnv), _TBResult119=_GORES141)), [addr(addr_tagbody_4_do_label_4, do_label_4, '$unused', BlockExitEnv125,  (get_var(BlockExitEnv125, sys_d, IFTEST120), (IFTEST120==[]->throw(block_exit([], [])), _TBResult119=ThrowResult124;sf_push(BlockExitEnv125, [list, [car, sys_d], [car, sys_v]], sys_bindings, Sf_push_Ret), get_var(BlockExitEnv125, sys_d, D_Get126), f_cdr(D_Get126, Cdr_Ret), get_var(BlockExitEnv125, sys_v, V_Get127), f_cdr(V_Get127, Cdr_Ret179), set_var(BlockExitEnv125, sys_d, Cdr_Ret), set_var(BlockExitEnv125, sys_v, Cdr_Ret179), goto(do_label_4, BlockExitEnv125), _TBResult119=_GORES128)))]), []=LetResult113), block_exit([], LetResult113), true), sf_push(LEnv108, [list, [car, sys_newval], [cadr, sys_l]], sys_bindings, Bindings160), sf_push(LEnv108, sys_setter, sys_forms, LetResult107), _10376=LetResult107), get_var(AEnv, sys_l, L_Get148), f_cddr(L_Get148, L), set_var(AEnv, sys_l, L), goto(do_label_2, AEnv), _TBResult=_GORES149)),
					  
					  [ addr(addr_tagbody_2_do_label_2,
						 do_label_2,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_l, L_Get), (s3q:is_endp(L_Get)->get_var(AEnv, sys_bindings, Reverse_Param165), f_reverse(Reverse_Param165, Reverse_Ret180), get_var(AEnv, sys_forms, Get_var_Ret), Reverse_Param166=[[]|Get_var_Ret], f_reverse(Reverse_Param166, Reverse_Ret182), f_list_xx(let_xx, [Reverse_Ret180, Reverse_Ret182], List_xx_Ret), throw(block_exit([], List_xx_Ret)), _11992=ThrowResult;get_var(AEnv, sys_l, L_Get24), f_car(L_Get24, PredArgResult26), (is_symbolp(PredArgResult26)->get_var(AEnv, sys_l, L_Get27), f_car(L_Get27, Eq_Param167), get_var(AEnv, sys_l, L_Get29), f_car(L_Get29, Car_Ret184), get_var(AEnv, sys_env, Get_var_Ret185), f_macroexpand_1([Car_Ret184, Get_var_Ret185], Macroexpand_1_Ret186), f_eq(Eq_Param167, Macroexpand_1_Ret186, Eq_Ret), IFTEST21=Eq_Ret;IFTEST21=[]), (IFTEST21\==[]->f_gensym(Gensym_Ret), LEnv33=[bv(sys_sym, Gensym_Ret)|AEnv], sf_push(LEnv33, [list, sys_sym, [cadr, sys_l]], sys_bindings, Sf_push_Ret189), sf_push(LEnv33, [list, [quote, setq], [car, sys_l], sys_sym], sys_forms, LetResult32), _12120=LetResult32;LEnv37=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_newval, []), bv(sys_setter, []), bv(sys_getter, [])|AEnv], get_var(LEnv37, sys_l, L_Get39), f_car(L_Get39, Car_Ret190), get_var(LEnv37, sys_env, Env_Get38), f_macroexpand_1([Car_Ret190, Env_Get38], Setf_expansion_Param168), get_var(LEnv37, sys_env, Env_Get40), f_get_setf_expansion(Setf_expansion_Param168, [Env_Get40], Setf_expansion_Ret191), setq_from_values(LEnv37, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter]), sf_declare(LEnv37, [ignore, sys_getter], Sf_declare_Ret192), get_var(LEnv37, sys_dummies, Dummies_Get), get_var(LEnv37, sys_vals, Vals_Get), BlockExitEnv=[bv(sys_d, Dummies_Get), bv(sys_v, Vals_Get)|LEnv37], catch((call_addr_block(BlockExitEnv,  (push_label(do_label_3), get_var(BlockExitEnv, sys_d, IFTEST62), (IFTEST62==[]->throw(block_exit([], [])), _TBResult48=ThrowResult66;sf_push(BlockExitEnv, [list, [car, sys_d], [car, sys_v]], sys_bindings, Sf_push_Ret193), get_var(BlockExitEnv, sys_d, D_Get68), f_cdr(D_Get68, Cdr_Ret194), get_var(BlockExitEnv, sys_v, V_Get69), f_cdr(V_Get69, Cdr_Ret195), set_var(BlockExitEnv, sys_d, Cdr_Ret194), set_var(BlockExitEnv, sys_v, Cdr_Ret195), goto(do_label_3, BlockExitEnv), _TBResult48=_GORES70)), [addr(addr_tagbody_3_do_label_3, do_label_3, '$unused', BlockExitEnv54,  (get_var(BlockExitEnv54, sys_d, IFTEST49), (IFTEST49==[]->throw(block_exit([], [])), _TBResult48=ThrowResult53;sf_push(BlockExitEnv54, [list, [car, sys_d], [car, sys_v]], sys_bindings, Sf_push_Ret196), get_var(BlockExitEnv54, sys_d, D_Get55), f_cdr(D_Get55, Cdr_Ret197), get_var(BlockExitEnv54, sys_v, Cdr_Param), f_cdr(Cdr_Param, Cdr_Ret198), set_var(BlockExitEnv54, sys_d, Cdr_Ret197), set_var(BlockExitEnv54, sys_v, Cdr_Ret198), goto(do_label_3, BlockExitEnv54), _TBResult48=_GORES)))]), []=LetResult42), block_exit([], LetResult42), true), sf_push(LEnv37, [list, [car, sys_newval], [cadr, sys_l]], sys_bindings, Sf_push_Ret199), sf_push(LEnv37, sys_setter, sys_forms, LetResult36), _12120=LetResult36), get_var(AEnv, sys_l, L_Get77), f_cddr(L_Get77, Cddr_Ret), set_var(AEnv, sys_l, Cddr_Ret), goto(do_label_2, AEnv), _11992=_GORES78)))
					  ]),
			  []=LetResult
			),
			block_exit([], LetResult),
			true)
		),
		LetResult=MFResult
	      ),
	      block_exit(psetq, MFResult),
	      true).
:- set_opv(mf_psetq, type_of, sys_macro),
   set_opv(psetq, symbol_function, mf_psetq),
   DefMacroResult=psetq.
/*
:- side_effect(assert_lsp(psetq,
			  lambda_def(defmacro,
				     psetq,
				     mf_psetq,
				     
				     [ c38_environment,
				       sys_env,
				       c38_rest,
				       sys_args
				     ],
				     
				     [ 
				       [ do,
					 
					 [ [sys_l, sys_args, [cddr, sys_l]],
					   [sys_forms, []],
					   [sys_bindings, []]
					 ],
					 
					 [ [endp, sys_l],
					   
					   [ list_xx,
					     [quote, let_xx],
					     [reverse, sys_bindings],
					     [reverse, [cons, [], sys_forms]]
					   ]
					 ],
					 
					 [ if,
					   
					   [ and,
					     [symbolp, [car, sys_l]],
					     
					     [ eq,
					       [car, sys_l],
					       
					       [ macroexpand_1,
						 [car, sys_l],
						 sys_env
					       ]
					     ]
					   ],
					   
					   [ let,
					     [[sys_sym, [gensym]]],
					     
					     [ push,
					       [list, sys_sym, [cadr, sys_l]],
					       sys_bindings
					     ],
					     
					     [ push,
					       
					       [ list,
						 [quote, setq],
						 [car, sys_l],
						 sys_sym
					       ],
					       sys_forms
					     ]
					   ],
					   
					   [ multiple_value_bind,
					     
					     [ sys_dummies,
					       sys_vals,
					       sys_newval,
					       sys_setter,
					       sys_getter
					     ],
					     
					     [ get_setf_expansion,
					       
					       [ macroexpand_1,
						 [car, sys_l],
						 sys_env
					       ],
					       sys_env
					     ],
					     [declare, [ignore, sys_getter]],
					     
					     [ do,
					       
					       [ 
						 [ sys_d,
						   sys_dummies,
						   [cdr, sys_d]
						 ],
						 [sys_v, sys_vals, [cdr, sys_v]]
					       ],
					       [[null, sys_d]],
					       
					       [ push,
						 
						 [ list,
						   [car, sys_d],
						   [car, sys_v]
						 ],
						 sys_bindings
					       ]
					     ],
					     
					     [ push,
					       
					       [ list,
						 [car, sys_newval],
						 [cadr, sys_l]
					       ],
					       sys_bindings
					     ],
					     [push, sys_setter, sys_forms]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(psetq,
			  arglist_info(psetq,
				       mf_psetq,
				       
				       [ c38_environment,
					 sys_env,
					 c38_rest,
					 sys_args
				       ],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[environment, rest],
						env:[sys_env],
						key:0,
						names:[sys_env, sys_args],
						opt:0,
						req:0,
						rest:[sys_args],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(psetq, init_args(0, mf_psetq))).
*/
/*
(defmacro setf (&rest pairs &environment env)
             (let ((nargs (length pairs)))
               (assert (evenp nargs))
               (cond
                ((zerop nargs) nil)
                ((= nargs 2)
                 (let ((place (car pairs))
                       (value-form (cadr pairs)))
                   (cond
                    ((symbolp place)
                     `(setq ,place ,value-form))
                    ((consp place)
                     (if (eq (car place) 'the)
                         `(setf ,(caddr place) (the ,(cadr place) ,value-form))
                       (multiple-value-bind (temps vars newvals setter getter)
                           (get-setf-expansion place env)
                         (declare (ignore getter))
                         `(let (,@(mapcar #'list temps vars))
                            (multiple-value-bind ,newvals ,value-form
                              ,setter))))))))
                (t
                 (do* ((pairs pairs (cddr pairs))
                       (setfs (list 'progn))
                       (splice setfs))
                      ((endp pairs) setfs)
                   (setq splice (cdr (rplacd splice
                                             `((setf ,(car pairs) ,(cadr pairs)))))))))))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:3126 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,setf,['&rest',pairs,'&environment',env],[let,[[nargs,[length,pairs]]],[assert,[evenp,nargs]],[cond,[[zerop,nargs],[]],[[=,nargs,2],[let,[[place,[car,pairs]],['value-form',[cadr,pairs]]],[cond,[[symbolp,place],['#BQ',[setq,['#COMMA',place],['#COMMA','value-form']]]],[[consp,place],[if,[eq,[car,place],[quote,the]],['#BQ',[setf,['#COMMA',[caddr,place]],[the,['#COMMA',[cadr,place]],['#COMMA','value-form']]]],['multiple-value-bind',[temps,vars,newvals,setter,getter],['get-setf-expansion',place,env],[declare,[ignore,getter]],['#BQ',[let,[['#BQ-COMMA-ELIPSE',[mapcar,function(list),temps,vars]]],['multiple-value-bind',['#COMMA',newvals],['#COMMA','value-form'],['#COMMA',setter]]]]]]]]]],[t,['do*',[[pairs,pairs,[cddr,pairs]],[setfs,[list,[quote,progn]]],[splice,setfs]],[[endp,pairs],setfs],[setq,splice,[cdr,[rplacd,splice,['#BQ',[[setf,['#COMMA',[car,pairs]],['#COMMA',[cadr,pairs]]]]]]]]]]]]])
/*
% macroexpand:-[assert,[evenp,sys_nargs]].
*/
/*
% into:-[do,[],[[evenp,sys_nargs],[]],[multiple_value_setq,[],[apply,[quote,sys_assert_places],[quote,[]],[list],'$ARRAY'([*],claz_base_character,"The assertion ~:@(~S~) failed."),[quote,[evenp,sys_nargs]],[]]]].
*/
/*
% macroexpand:-[psetq].
*/
/*
% into:-[let_xx,[],[]].
*/
/*
% macroexpand:-[psetq].
*/
/*
% into:-[let_xx,[],[]].
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       setf,
					       kw_special,
					       sf_setf)).
*/
wl:lambda_def(defmacro, setf, mf_setf, [c38_rest, sys_pairs, c38_environment, sys_env], [[let, [[sys_nargs, [length, sys_pairs]]], [assert, [evenp, sys_nargs]], [cond, [[zerop, sys_nargs], []], [[=, sys_nargs, 2], [let, [[sys_place, [car, sys_pairs]], [sys_value_form, [cadr, sys_pairs]]], [cond, [[symbolp, sys_place], ['#BQ', [setq, ['#COMMA', sys_place], ['#COMMA', sys_value_form]]]], [[consp, sys_place], [if, [eq, [car, sys_place], [quote, the]], ['#BQ', [setf, ['#COMMA', [caddr, sys_place]], [the, ['#COMMA', [cadr, sys_place]], ['#COMMA', sys_value_form]]]], [multiple_value_bind, [sys_temps, sys_vars, sys_newvals, sys_setter, sys_getter], [get_setf_expansion, sys_place, sys_env], [declare, [ignore, sys_getter]], ['#BQ', [let, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_temps, sys_vars]]], [multiple_value_bind, ['#COMMA', sys_newvals], ['#COMMA', sys_value_form], ['#COMMA', sys_setter]]]]]]]]]], [t, [do_xx, [[sys_pairs, sys_pairs, [cddr, sys_pairs]], [sys_setfs, [list, [quote, progn]]], [sys_splice, sys_setfs]], [[endp, sys_pairs], sys_setfs], [setq, sys_splice, [cdr, [rplacd, sys_splice, ['#BQ', [[setf, ['#COMMA', [car, sys_pairs]], ['#COMMA', [cadr, sys_pairs]]]]]]]]]]]]]).
wl:arglist_info(setf, mf_setf, [c38_rest, sys_pairs, c38_environment, sys_env], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[sys_env], key:0, names:[sys_pairs, sys_env], opt:0, req:0, rest:[sys_pairs], sublists:0, whole:0}).
wl: init_args(0, mf_setf).

/*

### Compiled Macro Operator: `CL:SETF` 
*/
sf_setf(Env_In, RestNKeys, FResult) :-
	mf_setf([setf|RestNKeys], Env_In, MFResult),
	f_sys_env_eval(Env_In, MFResult, FResult).
/*

### Compiled Macro Function: `CL:SETF` 
*/
mf_setf([setf|RestNKeys], Env_In, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_pairs, RestNKeys), bv(sys_env, Env_In)],
	catch(( ( get_var(GEnv, sys_pairs, Pairs_Get),
		  f_length(Pairs_Get, Nargs_Init),
		  BlockExitEnv=[bv(sys_nargs, Nargs_Init)|GEnv],
		  catch(( call_addr_block(BlockExitEnv,
					  (push_label(do_label_5), get_var(BlockExitEnv, sys_nargs, Nargs_Get25), (mth:is_evenp(Nargs_Get25)->throw(block_exit([], [])), _TBResult=ThrowResult29;CAR=[], f_apply(sys_assert_places, [[], CAR, '$ARRAY'([*], claz_base_character, "The assertion ~:@(~S~) failed."), [evenp, sys_nargs], []], Apply_Ret), setq_from_values(BlockExitEnv, []), goto(do_label_5, BlockExitEnv), _TBResult=_GORES31)),
					  
					  [ addr(addr_tagbody_5_do_label_5,
						 do_label_5,
						 '$unused',
						 BlockExitEnv,
						 (get_var(BlockExitEnv, sys_nargs, Nargs_Get), (mth:is_evenp(Nargs_Get)->throw(block_exit([], [])), _9542=ThrowResult;CAR144=[], f_apply(sys_assert_places, [[], CAR144, '$ARRAY'([*], claz_base_character, "The assertion ~:@(~S~) failed."), [evenp, sys_nargs], []], Apply_Ret143), setq_from_values(BlockExitEnv, []), goto(do_label_5, BlockExitEnv), _9542=_GORES)))
					  ]),
			  []=Block_exit_Ret
			),
			block_exit([], Block_exit_Ret),
			true),
		  get_var(BlockExitEnv, sys_nargs, Nargs_Get36),
		  (   mth:is_zerop(Nargs_Get36)
		  ->  LetResult=[]
		  ;   get_var(BlockExitEnv, sys_nargs, Nargs_Get40),
		      (   Nargs_Get40=:=2
		      ->  get_var(BlockExitEnv, sys_pairs, Pairs_Get46),
			  f_car(Pairs_Get46, Place_Init),
			  get_var(BlockExitEnv, sys_pairs, Pairs_Get47),
			  f_cadr(Pairs_Get47, Value_form_Init),
			  LEnv45=[bv(sys_place, Place_Init), bv(sys_value_form, Value_form_Init)|BlockExitEnv],
			  get_var(LEnv45, sys_place, Place_Get),
			  (   is_symbolp(Place_Get)
			  ->  get_var(LEnv45, sys_place, Place_Get54),
			      get_var(LEnv45, sys_value_form, Value_form_Get),
			      LetResult44=[setq, Place_Get54, Value_form_Get]
			  ;   get_var(LEnv45, sys_place, Place_Get57),
			      (   c0nz:is_consp(Place_Get57)
			      ->  get_var(LEnv45, sys_place, Place_Get61),
				  f_car(Place_Get61, PredArg1Result63),
				  (   is_eq(PredArg1Result63, the)
				  ->  get_var(LEnv45, sys_place, Place_Get64),
				      f_caddr(Place_Get64, Caddr_Ret),
				      get_var(LEnv45, sys_place, Place_Get65),
				      f_cadr(Place_Get65, Cadr_Ret),
				      get_var(LEnv45,
					      sys_value_form,
					      Value_form_Get66),
				      TrueResult77=[setf, Caddr_Ret, [the, Cadr_Ret, Value_form_Get66]]
				  ;   LEnv69=[bv(sys_temps, []), bv(sys_vars, []), bv(sys_newvals, []), bv(sys_setter, []), bv(sys_getter, [])|LEnv45],
				      get_var(LEnv69, sys_env, Env_Get),
				      get_var(LEnv69, sys_place, Place_Get70),
				      f_get_setf_expansion(Place_Get70,
							   [Env_Get],
							   Setf_expansion_Ret),
				      setq_from_values(LEnv69,
						       
						       [ sys_temps,
							 sys_vars,
							 sys_newvals,
							 sys_setter,
							 sys_getter
						       ]),
				      sf_declare(LEnv69,
						 [ignore, sys_getter],
						 Sf_declare_Ret),
				      get_var(LEnv69, sys_temps, Temps_Get),
				      get_var(LEnv69, sys_vars, Vars_Get),
				      f_mapcar(f_list,
					       [Temps_Get, Vars_Get],
					       Mapcar_Ret),
				      get_var(LEnv69, sys_newvals, Newvals_Get),
				      get_var(LEnv69, sys_setter, Setter_Get),
				      get_var(LEnv69,
					      sys_value_form,
					      Value_form_Get75),
				      TrueResult77=[let, Mapcar_Ret, [multiple_value_bind, Newvals_Get, Value_form_Get75, Setter_Get]]
				  ),
				  ElseResult79=TrueResult77
			      ;   ElseResult78=[],
				  ElseResult79=ElseResult78
			      ),
			      LetResult44=ElseResult79
			  ),
			  ElseResult132=LetResult44
		      ;   get_var(BlockExitEnv, sys_pairs, Pairs_Get83),
			  LEnv82=[bv(sys_pairs, Pairs_Get83)|BlockExitEnv],
			  Setfs_Init=[progn],
			  LEnv87=[bv(sys_setfs, Setfs_Init)|LEnv82],
			  get_var(LEnv87, sys_setfs, Setfs_Get),
			  AEnv=[bv(sys_splice, Setfs_Get)|LEnv87],
			  catch(( call_addr_block(AEnv,
						  (push_label(do_label_6), get_var(AEnv, sys_pairs, Pairs_Get114), (s3q:is_endp(Pairs_Get114)->get_var(AEnv, sys_setfs, RetResult117), throw(block_exit([], RetResult117)), _TBResult94=ThrowResult118;get_var(AEnv, sys_pairs, Pairs_Get123), get_var(AEnv, sys_splice, Splice_Get122), f_car(Pairs_Get123, Car_Ret), get_var(AEnv, sys_pairs, Pairs_Get124), f_cadr(Pairs_Get124, Cadr_Ret152), f_rplacd(Splice_Get122, [[setf, Car_Ret, Cadr_Ret152]], Cdr_Param), f_cdr(Cdr_Param, Splice), set_var(AEnv, sys_splice, Splice), get_var(AEnv, sys_pairs, Pairs_Get125), f_cddr(Pairs_Get125, Pairs), set_var(AEnv, sys_pairs, Pairs), goto(do_label_6, AEnv), _TBResult94=_GORES126)),
						  
						  [ addr(addr_tagbody_6_do_label_6,
							 do_label_6,
							 '$unused',
							 AEnv,
							 (get_var(AEnv, sys_pairs, Pairs_Get96), (s3q:is_endp(Pairs_Get96)->get_var(AEnv, sys_setfs, RetResult99), throw(block_exit([], RetResult99)), _TBResult94=ThrowResult100;get_var(AEnv, sys_pairs, Pairs_Get105), get_var(AEnv, sys_splice, Rplacd_Param), f_car(Pairs_Get105, Car_Ret153), get_var(AEnv, sys_pairs, Pairs_Get106), f_cadr(Pairs_Get106, Cadr_Ret154), f_rplacd(Rplacd_Param, [[setf, Car_Ret153, Cadr_Ret154]], Cdr_Param140), f_cdr(Cdr_Param140, Cdr_Ret), set_var(AEnv, sys_splice, Cdr_Ret), get_var(AEnv, sys_pairs, Pairs_Get107), f_cddr(Pairs_Get107, Cddr_Ret), set_var(AEnv, sys_pairs, Cddr_Ret), goto(do_label_6, AEnv), _TBResult94=_GORES108)))
						  ]),
				  []=LetResult90
				),
				block_exit([], LetResult90),
				true),
			  ElseResult132=LetResult90
		      ),
		      LetResult=ElseResult132
		  )
		),
		LetResult=MFResult
	      ),
	      block_exit(setf, MFResult),
	      true).
:- set_opv(mf_setf, type_of, sys_macro),
   set_opv(setf, symbol_function, mf_setf),
   DefMacroResult=setf.
/*
:- side_effect(assert_lsp(setf,
			  lambda_def(defmacro,
				     setf,
				     mf_setf,
				     
				     [ c38_rest,
				       sys_pairs,
				       c38_environment,
				       sys_env
				     ],
				     
				     [ 
				       [ let,
					 [[sys_nargs, [length, sys_pairs]]],
					 [assert, [evenp, sys_nargs]],
					 
					 [ cond,
					   [[zerop, sys_nargs], []],
					   
					   [ [=, sys_nargs, 2],
					     
					     [ let,
					       
					       [ [sys_place, [car, sys_pairs]],
						 
						 [ sys_value_form,
						   [cadr, sys_pairs]
						 ]
					       ],
					       
					       [ cond,
						 
						 [ [symbolp, sys_place],
						   
						   [ '#BQ',
						     
						     [ setq,
						       ['#COMMA', sys_place],
						       
						       [ '#COMMA',
							 sys_value_form
						       ]
						     ]
						   ]
						 ],
						 
						 [ [consp, sys_place],
						   
						   [ if,
						     
						     [ eq,
						       [car, sys_place],
						       [quote, the]
						     ],
						     
						     [ '#BQ',
						       
						       [ setf,
							 
							 [ '#COMMA',
							   [caddr, sys_place]
							 ],
							 
							 [ the,
							   
							   [ '#COMMA',
							     [cadr, sys_place]
							   ],
							   
							   [ '#COMMA',
							     sys_value_form
							   ]
							 ]
						       ]
						     ],
						     
						     [ multiple_value_bind,
						       
						       [ sys_temps,
							 sys_vars,
							 sys_newvals,
							 sys_setter,
							 sys_getter
						       ],
						       
						       [ get_setf_expansion,
							 sys_place,
							 sys_env
						       ],
						       
						       [ declare,
							 [ignore, sys_getter]
						       ],
						       
						       [ '#BQ',
							 
							 [ let,
							   
							   [ 
							     [ '#BQ-COMMA-ELIPSE',
							       
							       [ mapcar,
								 function(list),
								 sys_temps,
								 sys_vars
							       ]
							     ]
							   ],
							   
							   [ multiple_value_bind,
							     
							     [ '#COMMA',
							       sys_newvals
							     ],
							     
							     [ '#COMMA',
							       sys_value_form
							     ],
							     
							     [ '#COMMA',
							       sys_setter
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
					   
					   [ t,
					     
					     [ do_xx,
					       
					       [ 
						 [ sys_pairs,
						   sys_pairs,
						   [cddr, sys_pairs]
						 ],
						 
						 [ sys_setfs,
						   [list, [quote, progn]]
						 ],
						 [sys_splice, sys_setfs]
					       ],
					       [[endp, sys_pairs], sys_setfs],
					       
					       [ setq,
						 sys_splice,
						 
						 [ cdr,
						   
						   [ rplacd,
						     sys_splice,
						     
						     [ '#BQ',
						       
						       [ 
							 [ setf,
							   
							   [ '#COMMA',
							     [car, sys_pairs]
							   ],
							   
							   [ '#COMMA',
							     [cadr, sys_pairs]
							   ]
							 ]
						       ]
						     ]
						   ]
						 ]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(setf,
			  arglist_info(setf,
				       mf_setf,
				       
				       [ c38_rest,
					 sys_pairs,
					 c38_environment,
					 sys_env
				       ],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest, environment],
						env:[sys_env],
						key:0,
						names:[sys_pairs, sys_env],
						opt:0,
						req:0,
						rest:[sys_pairs],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(setf, init_args(0, mf_setf))).
*/
/*
(defmacro psetf (&rest pairs &environment env)
             (let ((nargs (length pairs)))
               (assert (evenp nargs))
               (if (< nargs 4)
                   `(progn (setf ,@pairs) nil)
                 (let ((setters nil))
                   (labels ((expand (pairs)
                                    (if pairs
                                        (multiple-value-bind (temps vars newvals setter getter)
                                            (get-setf-expansion (car pairs) env)
                                          (declare (ignore getter))
                                          (setq setters (cons setter setters))
                                          `(let (,@(mapcar #'list temps vars))
                                             (multiple-value-bind ,newvals ,(cadr pairs)
                                               ,(expand (cddr pairs)))))
                                      `(progn ,@setters nil))))
                     (expand pairs))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:4436 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,psetf,['&rest',pairs,'&environment',env],[let,[[nargs,[length,pairs]]],[assert,[evenp,nargs]],[if,[<,nargs,4],['#BQ',[progn,[setf,['#BQ-COMMA-ELIPSE',pairs]],[]]],[let,[[setters,[]]],[labels,[[expand,[pairs],[if,pairs,['multiple-value-bind',[temps,vars,newvals,setter,getter],['get-setf-expansion',[car,pairs],env],[declare,[ignore,getter]],[setq,setters,[cons,setter,setters]],['#BQ',[let,[['#BQ-COMMA-ELIPSE',[mapcar,function(list),temps,vars]]],['multiple-value-bind',['#COMMA',newvals],['#COMMA',[cadr,pairs]],['#COMMA',[expand,[cddr,pairs]]]]]]],['#BQ',[progn,['#BQ-COMMA-ELIPSE',setters],[]]]]]],[expand,pairs]]]]]])
/*
% macroexpand:-[assert,[evenp,sys_nargs]].
*/
/*
% into:-[do,[],[[evenp,sys_nargs],[]],[multiple_value_setq,[],[apply,[quote,sys_assert_places],[quote,[]],[list],'$ARRAY'([*],claz_base_character,"The assertion ~:@(~S~) failed."),[quote,[evenp,sys_nargs]],[]]]].
*/
/*
% macroexpand:-[psetq].
*/
/*
% into:-[let_xx,[],[]].
*/
/*
% macroexpand:-[psetq].
*/
/*
% into:-[let_xx,[],[]].
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_expand,
					       kw_function,
					       f_sys_expand)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_expand,
					       kw_function,
					       f_sys_expand)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand1),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       psetf,
					       kw_special,
					       sf_psetf)).
*/
wl:lambda_def(defmacro, psetf, mf_psetf, [c38_rest, sys_pairs, c38_environment, sys_env], [[let, [[sys_nargs, [length, sys_pairs]]], [assert, [evenp, sys_nargs]], [if, [<, sys_nargs, 4], ['#BQ', [progn, [setf, ['#BQ-COMMA-ELIPSE', sys_pairs]], []]], [let, [[sys_setters, []]], [labels, [[sys_expand, [sys_pairs], [if, sys_pairs, [multiple_value_bind, [sys_temps, sys_vars, sys_newvals, sys_setter, sys_getter], [get_setf_expansion, [car, sys_pairs], sys_env], [declare, [ignore, sys_getter]], [setq, sys_setters, [cons, sys_setter, sys_setters]], ['#BQ', [let, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_temps, sys_vars]]], [multiple_value_bind, ['#COMMA', sys_newvals], ['#COMMA', [cadr, sys_pairs]], ['#COMMA', [sys_expand, [cddr, sys_pairs]]]]]]], ['#BQ', [progn, ['#BQ-COMMA-ELIPSE', sys_setters], []]]]]], [sys_expand, sys_pairs]]]]]]).
wl:arglist_info(psetf, mf_psetf, [c38_rest, sys_pairs, c38_environment, sys_env], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[sys_env], key:0, names:[sys_pairs, sys_env], opt:0, req:0, rest:[sys_pairs], sublists:0, whole:0}).
wl: init_args(0, mf_psetf).

/*

### Compiled Macro Operator: `CL:PSETF` 
*/
sf_psetf(Env_In, RestNKeys, FResult) :-
	mf_psetf([psetf|RestNKeys], Env_In, MFResult),
	f_sys_env_eval(Env_In, MFResult, FResult).
/*

### Compiled Macro Function: `CL:PSETF` 
*/
mf_psetf([psetf|RestNKeys], Env_In, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_pairs, RestNKeys), bv(sys_env, Env_In)],
	catch(( ( get_var(GEnv, sys_pairs, Pairs_Get),
		  f_length(Pairs_Get, Nargs_Init),
		  BlockExitEnv=[bv(sys_nargs, Nargs_Init)|GEnv],
		  catch(( call_addr_block(BlockExitEnv,
					  (push_label(do_label_7), get_var(BlockExitEnv, sys_nargs, Nargs_Get25), (mth:is_evenp(Nargs_Get25)->throw(block_exit([], [])), _TBResult=ThrowResult29;CAR=[], f_apply(sys_assert_places, [[], CAR, '$ARRAY'([*], claz_base_character, "The assertion ~:@(~S~) failed."), [evenp, sys_nargs], []], Apply_Ret), setq_from_values(BlockExitEnv, []), goto(do_label_7, BlockExitEnv), _TBResult=_GORES31)),
					  
					  [ addr(addr_tagbody_7_do_label_7,
						 do_label_7,
						 '$unused',
						 BlockExitEnv,
						 (get_var(BlockExitEnv, sys_nargs, Nargs_Get), (mth:is_evenp(Nargs_Get)->throw(block_exit([], [])), _8674=ThrowResult;CAR78=[], f_apply(sys_assert_places, [[], CAR78, '$ARRAY'([*], claz_base_character, "The assertion ~:@(~S~) failed."), [evenp, sys_nargs], []], Apply_Ret77), setq_from_values(BlockExitEnv, []), goto(do_label_7, BlockExitEnv), _8674=_GORES)))
					  ]),
			  []=Block_exit_Ret
			),
			block_exit([], Block_exit_Ret),
			true),
		  get_var(BlockExitEnv, sys_nargs, Nargs_Get36),
		  (   Nargs_Get36<4
		  ->  get_var(BlockExitEnv, sys_pairs, Pairs_Get39),
		      LetResult=[progn, [setf|Pairs_Get39], []]
		  ;   LEnv42=[bv(sys_setters, [])|BlockExitEnv],
		      assert_lsp(sys_expand,
				 wl:lambda_def(defun, sys_expand, f_sys_expand1, [sys_pairs], [[if, sys_pairs, [multiple_value_bind, [sys_temps, sys_vars, sys_newvals, sys_setter, sys_getter], [get_setf_expansion, [car, sys_pairs], sys_env], [declare, [ignore, sys_getter]], [setq, sys_setters, [cons, sys_setter, sys_setters]], ['#BQ', [let, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_temps, sys_vars]]], [multiple_value_bind, ['#COMMA', sys_newvals], ['#COMMA', [cadr, sys_pairs]], ['#COMMA', [sys_expand, [cddr, sys_pairs]]]]]]], ['#BQ', [progn, ['#BQ-COMMA-ELIPSE', sys_setters], []]]]])),
		      assert_lsp(sys_expand,
				 wl:arglist_info(sys_expand, f_sys_expand1, [sys_pairs], arginfo{all:[sys_pairs], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_pairs], opt:0, req:[sys_pairs], rest:0, sublists:0, whole:0})),
		      assert_lsp(sys_expand, wl:init_args(1, f_sys_expand1)),
		      assert_lsp(sys_expand,
				 (f_sys_expand1(Pairs_In46, RestNKeys45, FnResult):-GEnv71=[bv(sys_pairs, Pairs_In46)], catch(((get_var(GEnv71, sys_pairs, IFTEST47), (IFTEST47\==[]->LEnv52=[bv(sys_temps, []), bv(sys_vars, []), bv(sys_newvals, []), bv(sys_setter, []), bv(sys_getter, [])|GEnv71], get_var(LEnv52, sys_pairs, Pairs_Get53), f_car(Pairs_Get53, Setf_expansion_Param), get_var(LEnv52, sys_env, Env_Get), f_get_setf_expansion(Setf_expansion_Param, [Env_Get], Setf_expansion_Ret), setq_from_values(LEnv52, [sys_temps, sys_vars, sys_newvals, sys_setter, sys_getter]), sf_declare(LEnv52, [ignore, sys_getter], Sf_declare_Ret), get_var(LEnv52, sys_setter, Setter_Get), get_var(LEnv52, sys_setters, Setters_Get), Setters=[Setter_Get|Setters_Get], set_var(LEnv52, sys_setters, Setters), get_var(LEnv52, sys_temps, Temps_Get), get_var(LEnv52, sys_vars, Vars_Get), f_mapcar(f_list, [Temps_Get, Vars_Get], Mapcar_Ret), get_var(LEnv52, sys_newvals, Newvals_Get), get_var(LEnv52, sys_pairs, Pairs_Get61), f_cadr(Pairs_Get61, Cadr_Ret), get_var(LEnv52, sys_pairs, Pairs_Get62), f_cddr(Pairs_Get62, Expand_Param), f_sys_expand(Expand_Param, Expand_Ret), _8990=[let, Mapcar_Ret, [multiple_value_bind, Newvals_Get, Cadr_Ret, Expand_Ret]];get_var(GEnv71, sys_setters, Setters_Get63), bq_append([progn|Setters_Get63], [[]], ElseResult64), _8990=ElseResult64)), _8990=FnResult), block_exit(sys_expand, FnResult), true))),
		      get_var(LEnv42, sys_pairs, Pairs_Get66),
		      f_sys_expand1(Pairs_Get66, LetResult41),
		      LetResult=LetResult41
		  )
		),
		LetResult=MFResult
	      ),
	      block_exit(psetf, MFResult),
	      true).
:- set_opv(mf_psetf, type_of, sys_macro),
   set_opv(psetf, symbol_function, mf_psetf),
   DefMacroResult=psetf.
/*
:- side_effect(assert_lsp(psetf,
			  lambda_def(defmacro,
				     psetf,
				     mf_psetf,
				     
				     [ c38_rest,
				       sys_pairs,
				       c38_environment,
				       sys_env
				     ],
				     
				     [ 
				       [ let,
					 [[sys_nargs, [length, sys_pairs]]],
					 [assert, [evenp, sys_nargs]],
					 
					 [ if,
					   [<, sys_nargs, 4],
					   
					   [ '#BQ',
					     
					     [ progn,
					       
					       [ setf,
						 
						 [ '#BQ-COMMA-ELIPSE',
						   sys_pairs
						 ]
					       ],
					       []
					     ]
					   ],
					   
					   [ let,
					     [[sys_setters, []]],
					     
					     [ labels,
					       
					       [ 
						 [ sys_expand,
						   [sys_pairs],
						   
						   [ if,
						     sys_pairs,
						     
						     [ multiple_value_bind,
						       
						       [ sys_temps,
							 sys_vars,
							 sys_newvals,
							 sys_setter,
							 sys_getter
						       ],
						       
						       [ get_setf_expansion,
							 [car, sys_pairs],
							 sys_env
						       ],
						       
						       [ declare,
							 [ignore, sys_getter]
						       ],
						       
						       [ setq,
							 sys_setters,
							 
							 [ cons,
							   sys_setter,
							   sys_setters
							 ]
						       ],
						       
						       [ '#BQ',
							 
							 [ let,
							   
							   [ 
							     [ '#BQ-COMMA-ELIPSE',
							       
							       [ mapcar,
								 function(list),
								 sys_temps,
								 sys_vars
							       ]
							     ]
							   ],
							   
							   [ multiple_value_bind,
							     
							     [ '#COMMA',
							       sys_newvals
							     ],
							     
							     [ '#COMMA',
							       [cadr, sys_pairs]
							     ],
							     
							     [ '#COMMA',
							       
							       [ sys_expand,
								 [cddr, sys_pairs]
							       ]
							     ]
							   ]
							 ]
						       ]
						     ],
						     
						     [ '#BQ',
						       
						       [ progn,
							 
							 [ '#BQ-COMMA-ELIPSE',
							   sys_setters
							 ],
							 []
						       ]
						     ]
						   ]
						 ]
					       ],
					       [sys_expand, sys_pairs]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(psetf,
			  arglist_info(psetf,
				       mf_psetf,
				       
				       [ c38_rest,
					 sys_pairs,
					 c38_environment,
					 sys_env
				       ],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest, environment],
						env:[sys_env],
						key:0,
						names:[sys_pairs, sys_env],
						opt:0,
						req:0,
						rest:[sys_pairs],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(psetf, init_args(0, mf_psetf))).
*/
/*
(defmacro shiftf (&rest places-and-newvalue &environment env)
             (let ((nargs (length places-and-newvalue)))
               (assert (>= nargs 2))
               (let ((place (car places-and-newvalue)))
                 (multiple-value-bind (temps vars newvals setter getter)
                     (get-setf-expansion place env)
                   `(let (,@(mapcar #'list temps vars))
                      (multiple-value-prog1 ,getter
                        (multiple-value-bind ,newvals
                            ,(if (= nargs 2)
                                 (cadr places-and-newvalue)
                               `(shiftf ,@(cdr places-and-newvalue)))
                          ,setter)))))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:5459 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,shiftf,['&rest','places-and-newvalue','&environment',env],[let,[[nargs,[length,'places-and-newvalue']]],[assert,[>=,nargs,2]],[let,[[place,[car,'places-and-newvalue']]],['multiple-value-bind',[temps,vars,newvals,setter,getter],['get-setf-expansion',place,env],['#BQ',[let,[['#BQ-COMMA-ELIPSE',[mapcar,function(list),temps,vars]]],['multiple-value-prog1',['#COMMA',getter],['multiple-value-bind',['#COMMA',newvals],['#COMMA',[if,[=,nargs,2],[cadr,'places-and-newvalue'],['#BQ',[shiftf,['#BQ-COMMA-ELIPSE',[cdr,'places-and-newvalue']]]]]],['#COMMA',setter]]]]]]]]])
/*
% macroexpand:-[assert,[>=,sys_nargs,2]].
*/
/*
% into:-[do,[],[[>=,sys_nargs,2],[]],[multiple_value_setq,[],[apply,[quote,sys_assert_places],[quote,[]],[list],'$ARRAY'([*],claz_base_character,"The assertion ~:@(~S~) failed."),[quote,[>=,sys_nargs,2]],[]]]].
*/
/*
% macroexpand:-[psetq].
*/
/*
% into:-[let_xx,[],[]].
*/
/*
% macroexpand:-[psetq].
*/
/*
% into:-[let_xx,[],[]].
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand1),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       shiftf,
					       kw_special,
					       sf_shiftf)).
*/
wl:lambda_def(defmacro, shiftf, mf_shiftf, [c38_rest, sys_places_and_newvalue, c38_environment, sys_env], [[let, [[sys_nargs, [length, sys_places_and_newvalue]]], [assert, [>=, sys_nargs, 2]], [let, [[sys_place, [car, sys_places_and_newvalue]]], [multiple_value_bind, [sys_temps, sys_vars, sys_newvals, sys_setter, sys_getter], [get_setf_expansion, sys_place, sys_env], ['#BQ', [let, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_temps, sys_vars]]], [multiple_value_prog1, ['#COMMA', sys_getter], [multiple_value_bind, ['#COMMA', sys_newvals], ['#COMMA', [if, [=, sys_nargs, 2], [cadr, sys_places_and_newvalue], ['#BQ', [shiftf, ['#BQ-COMMA-ELIPSE', [cdr, sys_places_and_newvalue]]]]]], ['#COMMA', sys_setter]]]]]]]]]).
wl:arglist_info(shiftf, mf_shiftf, [c38_rest, sys_places_and_newvalue, c38_environment, sys_env], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[sys_env], key:0, names:[sys_places_and_newvalue, sys_env], opt:0, req:0, rest:[sys_places_and_newvalue], sublists:0, whole:0}).
wl: init_args(0, mf_shiftf).

/*

### Compiled Macro Operator: `CL:SHIFTF` 
*/
sf_shiftf(Env_In, RestNKeys, FResult) :-
	mf_shiftf([shiftf|RestNKeys], Env_In, MFResult),
	f_sys_env_eval(Env_In, MFResult, FResult).
/*

### Compiled Macro Function: `CL:SHIFTF` 
*/
mf_shiftf([shiftf|RestNKeys], Env_In, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_places_and_newvalue, RestNKeys), bv(sys_env, Env_In)],
	catch(( ( get_var(GEnv,
			  sys_places_and_newvalue,
			  Places_and_newvalue_Get),
		  f_length(Places_and_newvalue_Get, Nargs_Init),
		  BlockExitEnv=[bv(sys_nargs, Nargs_Init)|GEnv],
		  catch(( call_addr_block(BlockExitEnv,
					  (push_label(do_label_8), get_var(BlockExitEnv, sys_nargs, Nargs_Get25), (Nargs_Get25>=2->throw(block_exit([], [])), _TBResult=ThrowResult29;CAR=[], f_apply(sys_assert_places, [[], CAR, '$ARRAY'([*], claz_base_character, "The assertion ~:@(~S~) failed."), [>=, sys_nargs, 2], []], Apply_Ret), setq_from_values(BlockExitEnv, []), goto(do_label_8, BlockExitEnv), _TBResult=_GORES31)),
					  
					  [ addr(addr_tagbody_8_do_label_8,
						 do_label_8,
						 '$unused',
						 BlockExitEnv,
						 (get_var(BlockExitEnv, sys_nargs, Nargs_Get), (Nargs_Get>=2->throw(block_exit([], [])), _8250=ThrowResult;CAR63=[], f_apply(sys_assert_places, [[], CAR63, '$ARRAY'([*], claz_base_character, "The assertion ~:@(~S~) failed."), [>=, sys_nargs, 2], []], Apply_Ret62), setq_from_values(BlockExitEnv, []), goto(do_label_8, BlockExitEnv), _8250=_GORES)))
					  ]),
			  []=Block_exit_Ret
			),
			block_exit([], Block_exit_Ret),
			true),
		  get_var(BlockExitEnv,
			  sys_places_and_newvalue,
			  Places_and_newvalue_Get38),
		  f_car(Places_and_newvalue_Get38, Place_Init),
		  LEnv37=[bv(sys_place, Place_Init)|BlockExitEnv],
		  LEnv42=[bv(sys_temps, []), bv(sys_vars, []), bv(sys_newvals, []), bv(sys_setter, []), bv(sys_getter, [])|LEnv37],
		  get_var(LEnv42, sys_env, Env_Get),
		  get_var(LEnv42, sys_place, Place_Get),
		  f_get_setf_expansion(Place_Get, [Env_Get], Setf_expansion_Ret),
		  setq_from_values(LEnv42,
				   
				   [ sys_temps,
				     sys_vars,
				     sys_newvals,
				     sys_setter,
				     sys_getter
				   ]),
		  get_var(LEnv42, sys_temps, Temps_Get),
		  get_var(LEnv42, sys_vars, Vars_Get),
		  f_mapcar(f_list, [Temps_Get, Vars_Get], Mapcar_Ret),
		  get_var(LEnv42, sys_getter, Getter_Get),
		  get_var(LEnv42, sys_nargs, Nargs_Get50),
		  get_var(LEnv42, sys_newvals, Newvals_Get),
		  (   Nargs_Get50=:=2
		  ->  get_var(LEnv42,
			      sys_places_and_newvalue,
			      Places_and_newvalue_Get53),
		      f_cadr(Places_and_newvalue_Get53, TrueResult55),
		      CAR68=TrueResult55
		  ;   get_var(LEnv42,
			      sys_places_and_newvalue,
			      Places_and_newvalue_Get54),
		      f_cdr(Places_and_newvalue_Get54, Cdr_Ret),
		      CAR68=[shiftf|Cdr_Ret]
		  ),
		  get_var(LEnv42, sys_setter, Setter_Get)
		),
		[let, Mapcar_Ret, [multiple_value_prog1, Getter_Get, [multiple_value_bind, Newvals_Get, CAR68, Setter_Get]]]=MFResult
	      ),
	      block_exit(shiftf, MFResult),
	      true).
:- set_opv(mf_shiftf, type_of, sys_macro),
   set_opv(shiftf, symbol_function, mf_shiftf),
   DefMacroResult=shiftf.
/*
:- side_effect(assert_lsp(shiftf,
			  lambda_def(defmacro,
				     shiftf,
				     mf_shiftf,
				     
				     [ c38_rest,
				       sys_places_and_newvalue,
				       c38_environment,
				       sys_env
				     ],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_nargs,
					     [length, sys_places_and_newvalue]
					   ]
					 ],
					 [assert, [>=, sys_nargs, 2]],
					 
					 [ let,
					   
					   [ 
					     [ sys_place,
					       [car, sys_places_and_newvalue]
					     ]
					   ],
					   
					   [ multiple_value_bind,
					     
					     [ sys_temps,
					       sys_vars,
					       sys_newvals,
					       sys_setter,
					       sys_getter
					     ],
					     
					     [ get_setf_expansion,
					       sys_place,
					       sys_env
					     ],
					     
					     [ '#BQ',
					       
					       [ let,
						 
						 [ 
						   [ '#BQ-COMMA-ELIPSE',
						     
						     [ mapcar,
						       function(list),
						       sys_temps,
						       sys_vars
						     ]
						   ]
						 ],
						 
						 [ multiple_value_prog1,
						   ['#COMMA', sys_getter],
						   
						   [ multiple_value_bind,
						     ['#COMMA', sys_newvals],
						     
						     [ '#COMMA',
						       
						       [ if,
							 [=, sys_nargs, 2],
							 
							 [ cadr,
							   sys_places_and_newvalue
							 ],
							 
							 [ '#BQ',
							   
							   [ shiftf,
							     
							     [ '#BQ-COMMA-ELIPSE',
							       
							       [ cdr,
								 sys_places_and_newvalue
							       ]
							     ]
							   ]
							 ]
						       ]
						     ],
						     ['#COMMA', sys_setter]
						   ]
						 ]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(shiftf,
			  arglist_info(shiftf,
				       mf_shiftf,
				       
				       [ c38_rest,
					 sys_places_and_newvalue,
					 c38_environment,
					 sys_env
				       ],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest, environment],
						env:[sys_env],
						key:0,
						names:
						      [ sys_places_and_newvalue,
							sys_env
						      ],
						opt:0,
						req:0,
						rest:[sys_places_and_newvalue],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(shiftf, init_args(0, mf_shiftf))).
*/
/*
(defmacro rotatef (&rest places &environment env)
             (if (< (length places) 2)
                 nil
               (multiple-value-bind (temps vars newvals setter getter)
                   (get-setf-expansion (car places) env)
                 `(let (,@(mapcar #'list temps vars))
                    (multiple-value-bind ,newvals (shiftf ,@(cdr places) ,getter)
                      ,setter)
                    nil))))


;; Adapted from SBCL.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:6191 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,rotatef,['&rest',places,'&environment',env],[if,[<,[length,places],2],[],['multiple-value-bind',[temps,vars,newvals,setter,getter],['get-setf-expansion',[car,places],env],['#BQ',[let,[['#BQ-COMMA-ELIPSE',[mapcar,function(list),temps,vars]]],['multiple-value-bind',['#COMMA',newvals],[shiftf,['#BQ-COMMA-ELIPSE',[cdr,places]],['#COMMA',getter]],['#COMMA',setter]],[]]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand1),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       rotatef,
					       kw_special,
					       sf_rotatef)).
*/
wl:lambda_def(defmacro, rotatef, mf_rotatef, [c38_rest, sys_places, c38_environment, sys_env], [[if, [<, [length, sys_places], 2], [], [multiple_value_bind, [sys_temps, sys_vars, sys_newvals, sys_setter, sys_getter], [get_setf_expansion, [car, sys_places], sys_env], ['#BQ', [let, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_temps, sys_vars]]], [multiple_value_bind, ['#COMMA', sys_newvals], [shiftf, ['#BQ-COMMA-ELIPSE', [cdr, sys_places]], ['#COMMA', sys_getter]], ['#COMMA', sys_setter]], []]]]]]).
wl:arglist_info(rotatef, mf_rotatef, [c38_rest, sys_places, c38_environment, sys_env], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[sys_env], key:0, names:[sys_places, sys_env], opt:0, req:0, rest:[sys_places], sublists:0, whole:0}).
wl: init_args(0, mf_rotatef).

/*

### Compiled Macro Operator: `CL:ROTATEF` 
*/
sf_rotatef(Env_In, RestNKeys, FResult) :-
	mf_rotatef([rotatef|RestNKeys], Env_In, MFResult),
	f_sys_env_eval(Env_In, MFResult, FResult).
/*

### Compiled Macro Function: `CL:ROTATEF` 
*/
mf_rotatef([rotatef|RestNKeys], Env_In, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_places, RestNKeys), bv(sys_env, Env_In)],
	catch(( ( get_var(GEnv, sys_places, Places_Get),
		  f_length(Places_Get, PredArg1Result),
		  (   PredArg1Result<2
		  ->  _7280=[]
		  ;   LEnv=[bv(sys_temps, []), bv(sys_vars, []), bv(sys_newvals, []), bv(sys_setter, []), bv(sys_getter, [])|GEnv],
		      get_var(LEnv, sys_places, Places_Get13),
		      f_car(Places_Get13, Setf_expansion_Param),
		      get_var(LEnv, sys_env, Env_Get),
		      f_get_setf_expansion(Setf_expansion_Param,
					   [Env_Get],
					   Setf_expansion_Ret),
		      setq_from_values(LEnv,
				       
				       [ sys_temps,
					 sys_vars,
					 sys_newvals,
					 sys_setter,
					 sys_getter
				       ]),
		      get_var(LEnv, sys_temps, Temps_Get),
		      get_var(LEnv, sys_vars, Vars_Get),
		      f_mapcar(f_list, [Temps_Get, Vars_Get], Mapcar_Ret),
		      get_var(LEnv, sys_newvals, Newvals_Get),
		      get_var(LEnv, sys_places, Places_Get18),
		      f_cdr(Places_Get18, Cdr_Ret),
		      get_var(LEnv, sys_getter, Getter_Get),
		      bq_append([shiftf|Cdr_Ret], [Getter_Get], Bq_append_Ret),
		      get_var(LEnv, sys_setter, Setter_Get),
		      _7280=[let, Mapcar_Ret, [multiple_value_bind, Newvals_Get, Bq_append_Ret, Setter_Get], []]
		  )
		),
		_7280=MFResult
	      ),
	      block_exit(rotatef, MFResult),
	      true).
:- set_opv(mf_rotatef, type_of, sys_macro),
   set_opv(rotatef, symbol_function, mf_rotatef),
   DefMacroResult=rotatef.
/*
:- side_effect(assert_lsp(rotatef,
			  lambda_def(defmacro,
				     rotatef,
				     mf_rotatef,
				     
				     [ c38_rest,
				       sys_places,
				       c38_environment,
				       sys_env
				     ],
				     
				     [ 
				       [ if,
					 [<, [length, sys_places], 2],
					 [],
					 
					 [ multiple_value_bind,
					   
					   [ sys_temps,
					     sys_vars,
					     sys_newvals,
					     sys_setter,
					     sys_getter
					   ],
					   
					   [ get_setf_expansion,
					     [car, sys_places],
					     sys_env
					   ],
					   
					   [ '#BQ',
					     
					     [ let,
					       
					       [ 
						 [ '#BQ-COMMA-ELIPSE',
						   
						   [ mapcar,
						     function(list),
						     sys_temps,
						     sys_vars
						   ]
						 ]
					       ],
					       
					       [ multiple_value_bind,
						 ['#COMMA', sys_newvals],
						 
						 [ shiftf,
						   
						   [ '#BQ-COMMA-ELIPSE',
						     [cdr, sys_places]
						   ],
						   ['#COMMA', sys_getter]
						 ],
						 ['#COMMA', sys_setter]
					       ],
					       []
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(rotatef,
			  arglist_info(rotatef,
				       mf_rotatef,
				       
				       [ c38_rest,
					 sys_places,
					 c38_environment,
					 sys_env
				       ],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest, environment],
						env:[sys_env],
						key:0,
						names:[sys_places, sys_env],
						opt:0,
						req:0,
						rest:[sys_places],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(rotatef, init_args(0, mf_rotatef))).
*/
/*
; Adapted from SBCL.
*/
/*
(defmacro push (&environment env item place)
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(setq ,place (cons ,item ,place))
      (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
        (let ((g (gensym)))
          `(let* ((,g ,item)
                  ,@(mapcar #'list dummies vals)
                  (,(car newval) (cons ,g ,getter)))
             ,setter)))))

;; Adapted from SBCL.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:6660 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,push,['&environment',env,item,place],[if,[and,[symbolp,place],[eq,place,[macroexpand,place,env]]],['#BQ',[setq,['#COMMA',place],[cons,['#COMMA',item],['#COMMA',place]]]],['multiple-value-bind',[dummies,vals,newval,setter,getter],['get-setf-expansion',place,env],[let,[[g,[gensym]]],['#BQ',['let*',[[['#COMMA',g],['#COMMA',item]],['#BQ-COMMA-ELIPSE',[mapcar,function(list),dummies,vals]],[['#COMMA',[car,newval]],[cons,['#COMMA',g],['#COMMA',getter]]]],['#COMMA',setter]]]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand1),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       push,
					       kw_macro,
					       mf_push)).
*/
wl:lambda_def(defmacro, push, mf_push, [c38_environment, sys_env, sys_item, sys_place], [[if, [and, [symbolp, sys_place], [eq, sys_place, [macroexpand, sys_place, sys_env]]], ['#BQ', [setq, ['#COMMA', sys_place], [cons, ['#COMMA', sys_item], ['#COMMA', sys_place]]]], [multiple_value_bind, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter], [get_setf_expansion, sys_place, sys_env], [let, [[sys_g, [gensym]]], ['#BQ', [let_xx, [[['#COMMA', sys_g], ['#COMMA', sys_item]], ['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]], [['#COMMA', [car, sys_newval]], [cons, ['#COMMA', sys_g], ['#COMMA', sys_getter]]]], ['#COMMA', sys_setter]]]]]]]).
wl:arglist_info(push, mf_push, [c38_environment, sys_env, sys_item, sys_place], arginfo{all:[sys_item, sys_place], allow_other_keys:0, aux:0, body:0, complex:[environment], env:[sys_env], key:0, names:[sys_env, sys_item, sys_place], opt:0, req:[sys_item, sys_place], rest:0, sublists:0, whole:0}).
wl: init_args(2, mf_push).

/*

### Compiled Macro Operator: `CL:PUSH` 
*/
sf_push(Env_In, Item_In, Place_In, RestNKeys, FResult) :-
	mf_push([push, Item_In, Place_In|RestNKeys], Env_In, MFResult),
	f_sys_env_eval(Env_In, MFResult, FResult).
/*

### Compiled Macro Function: `CL:PUSH` 
*/
mf_push([push, Item_In, Place_In|RestNKeys], Env_In, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_env, Env_In), bv(sys_item, Item_In), bv(sys_place, Place_In)],
	catch(( ( get_var(GEnv, sys_place, Place_Get),
		  (   is_symbolp(Place_Get)
		  ->  get_var(GEnv, sys_env, Env_Get),
		      get_var(GEnv, sys_place, Place_Get13),
		      f_macroexpand([Place_Get13, Env_Get], Macroexpand_Ret),
		      f_eq(Place_Get13, Macroexpand_Ret, TrueResult),
		      IFTEST=TrueResult
		  ;   IFTEST=[]
		  ),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, sys_item, Item_Get),
		      get_var(GEnv, sys_place, Place_Get17),
		      _8750=[setq, Place_Get17, [cons, Item_Get, Place_Get17]]
		  ;   LEnv=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_newval, []), bv(sys_setter, []), bv(sys_getter, [])|GEnv],
		      get_var(LEnv, sys_env, Env_Get24),
		      get_var(LEnv, sys_place, Place_Get23),
		      f_get_setf_expansion(Place_Get23,
					   [Env_Get24],
					   Setf_expansion_Ret),
		      setq_from_values(LEnv,
				       
				       [ sys_dummies,
					 sys_vals,
					 sys_newval,
					 sys_setter,
					 sys_getter
				       ]),
		      f_gensym(G_Init),
		      LEnv27=[bv(sys_g, G_Init)|LEnv],
		      get_var(LEnv27, sys_dummies, Dummies_Get),
		      get_var(LEnv27, sys_g, G_Get),
		      get_var(LEnv27, sys_item, Item_Get30),
		      get_var(LEnv27, sys_vals, Vals_Get),
		      f_mapcar(f_list, [Dummies_Get, Vals_Get], Mapcar_Ret),
		      get_var(LEnv27, sys_newval, Newval_Get),
		      f_car(Newval_Get, Car_Ret),
		      get_var(LEnv27, sys_g, G_Get34),
		      get_var(LEnv27, sys_getter, Getter_Get),
		      bq_append([[G_Get, Item_Get30]|Mapcar_Ret],
				[[Car_Ret, [cons, G_Get34, Getter_Get]]],
				Bq_append_Ret),
		      get_var(LEnv27, sys_setter, Setter_Get),
		      _8750=[let_xx, Bq_append_Ret, Setter_Get]
		  )
		),
		_8750=MFResult
	      ),
	      block_exit(push, MFResult),
	      true).
:- set_opv(mf_push, type_of, sys_macro),
   set_opv(push, symbol_function, mf_push),
   DefMacroResult=push.
/*
:- side_effect(assert_lsp(push,
			  lambda_def(defmacro,
				     push,
				     mf_push,
				     
				     [ c38_environment,
				       sys_env,
				       sys_item,
				       sys_place
				     ],
				     
				     [ 
				       [ if,
					 
					 [ and,
					   [symbolp, sys_place],
					   
					   [ eq,
					     sys_place,
					     [macroexpand, sys_place, sys_env]
					   ]
					 ],
					 
					 [ '#BQ',
					   
					   [ setq,
					     ['#COMMA', sys_place],
					     
					     [ cons,
					       ['#COMMA', sys_item],
					       ['#COMMA', sys_place]
					     ]
					   ]
					 ],
					 
					 [ multiple_value_bind,
					   
					   [ sys_dummies,
					     sys_vals,
					     sys_newval,
					     sys_setter,
					     sys_getter
					   ],
					   
					   [ get_setf_expansion,
					     sys_place,
					     sys_env
					   ],
					   
					   [ let,
					     [[sys_g, [gensym]]],
					     
					     [ '#BQ',
					       
					       [ let_xx,
						 
						 [ 
						   [ ['#COMMA', sys_g],
						     ['#COMMA', sys_item]
						   ],
						   
						   [ '#BQ-COMMA-ELIPSE',
						     
						     [ mapcar,
						       function(list),
						       sys_dummies,
						       sys_vals
						     ]
						   ],
						   
						   [ 
						     [ '#COMMA',
						       [car, sys_newval]
						     ],
						     
						     [ cons,
						       ['#COMMA', sys_g],
						       ['#COMMA', sys_getter]
						     ]
						   ]
						 ],
						 ['#COMMA', sys_setter]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(push,
			  arglist_info(push,
				       mf_push,
				       
				       [ c38_environment,
					 sys_env,
					 sys_item,
					 sys_place
				       ],
				       arginfo{ all:[sys_item, sys_place],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[environment],
						env:[sys_env],
						key:0,
						names:
						      [ sys_env,
							sys_item,
							sys_place
						      ],
						opt:0,
						req:[sys_item, sys_place],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(push, init_args(2, mf_push))).
*/
/*
; Adapted from SBCL.
*/
/*
(defmacro pushnew (&environment env item place &rest keys)
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(setq ,place (adjoin ,item ,place ,@keys))
      (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
        (let ((g (gensym)))
          `(let* ((,g ,item)
                  ,@(mapcar #'list dummies vals)
                  (,(car newval) (adjoin ,g ,getter ,@keys)))
             ,setter)))))

;; Adapted from SBCL.
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:7136 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,pushnew,['&environment',env,item,place,'&rest',keys],[if,[and,[symbolp,place],[eq,place,[macroexpand,place,env]]],['#BQ',[setq,['#COMMA',place],[adjoin,['#COMMA',item],['#COMMA',place],['#BQ-COMMA-ELIPSE',keys]]]],['multiple-value-bind',[dummies,vals,newval,setter,getter],['get-setf-expansion',place,env],[let,[[g,[gensym]]],['#BQ',['let*',[[['#COMMA',g],['#COMMA',item]],['#BQ-COMMA-ELIPSE',[mapcar,function(list),dummies,vals]],[['#COMMA',[car,newval]],[adjoin,['#COMMA',g],['#COMMA',getter],['#BQ-COMMA-ELIPSE',keys]]]],['#COMMA',setter]]]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand1),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       pushnew,
					       kw_macro,
					       mf_pushnew)).
*/
wl:lambda_def(defmacro, pushnew, mf_pushnew, [c38_environment, sys_env, sys_item, sys_place, c38_rest, sys_keys], [[if, [and, [symbolp, sys_place], [eq, sys_place, [macroexpand, sys_place, sys_env]]], ['#BQ', [setq, ['#COMMA', sys_place], [adjoin, ['#COMMA', sys_item], ['#COMMA', sys_place], ['#BQ-COMMA-ELIPSE', sys_keys]]]], [multiple_value_bind, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter], [get_setf_expansion, sys_place, sys_env], [let, [[sys_g, [gensym]]], ['#BQ', [let_xx, [[['#COMMA', sys_g], ['#COMMA', sys_item]], ['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]], [['#COMMA', [car, sys_newval]], [adjoin, ['#COMMA', sys_g], ['#COMMA', sys_getter], ['#BQ-COMMA-ELIPSE', sys_keys]]]], ['#COMMA', sys_setter]]]]]]]).
wl:arglist_info(pushnew, mf_pushnew, [c38_environment, sys_env, sys_item, sys_place, c38_rest, sys_keys], arginfo{all:[sys_item, sys_place], allow_other_keys:0, aux:0, body:0, complex:[environment, rest], env:[sys_env], key:0, names:[sys_env, sys_item, sys_place, sys_keys], opt:0, req:[sys_item, sys_place], rest:[sys_keys], sublists:0, whole:0}).
wl: init_args(2, mf_pushnew).

/*

### Compiled Macro Operator: `CL:PUSHNEW` 
*/
sf_pushnew(Env_In, Item_In, Place_In, RestNKeys, FResult) :-
	mf_pushnew([pushnew, Item_In, Place_In|RestNKeys], Env_In, MFResult),
	f_sys_env_eval(Env_In, MFResult, FResult).
/*

### Compiled Macro Function: `CL:PUSHNEW` 
*/
mf_pushnew([pushnew, Item_In, Place_In|RestNKeys], Env_In, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_env, Env_In), bv(sys_item, Item_In), bv(sys_place, Place_In), bv(sys_keys, RestNKeys)],
	catch(( ( get_var(GEnv, sys_place, Place_Get),
		  (   is_symbolp(Place_Get)
		  ->  get_var(GEnv, sys_env, Env_Get),
		      get_var(GEnv, sys_place, Place_Get14),
		      f_macroexpand([Place_Get14, Env_Get], Macroexpand_Ret),
		      f_eq(Place_Get14, Macroexpand_Ret, TrueResult),
		      IFTEST=TrueResult
		  ;   IFTEST=[]
		  ),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, sys_item, Item_Get),
		      ( get_var(GEnv, sys_keys, Keys_Get),
			get_var(GEnv, sys_place, Place_Get18)
		      ),
		      get_var(GEnv, sys_place, Place_Get20),
		      _8976=[setq, Place_Get18, [adjoin, Item_Get, Place_Get20|Keys_Get]]
		  ;   LEnv=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_newval, []), bv(sys_setter, []), bv(sys_getter, [])|GEnv],
		      get_var(LEnv, sys_env, Env_Get26),
		      get_var(LEnv, sys_place, Place_Get25),
		      f_get_setf_expansion(Place_Get25,
					   [Env_Get26],
					   Setf_expansion_Ret),
		      setq_from_values(LEnv,
				       
				       [ sys_dummies,
					 sys_vals,
					 sys_newval,
					 sys_setter,
					 sys_getter
				       ]),
		      f_gensym(G_Init),
		      LEnv29=[bv(sys_g, G_Init)|LEnv],
		      get_var(LEnv29, sys_dummies, Dummies_Get),
		      get_var(LEnv29, sys_g, G_Get),
		      get_var(LEnv29, sys_item, Item_Get32),
		      get_var(LEnv29, sys_vals, Vals_Get),
		      f_mapcar(f_list, [Dummies_Get, Vals_Get], Mapcar_Ret),
		      get_var(LEnv29, sys_newval, Newval_Get),
		      f_car(Newval_Get, Car_Ret),
		      get_var(LEnv29, sys_g, G_Get36),
		      get_var(LEnv29, sys_getter, Getter_Get),
		      get_var(LEnv29, sys_keys, Keys_Get38),
		      bq_append([[G_Get, Item_Get32]|Mapcar_Ret],
				
				[ 
				  [ Car_Ret,
				    [adjoin, G_Get36, Getter_Get|Keys_Get38]
				  ]
				],
				Bq_append_Ret),
		      get_var(LEnv29, sys_setter, Setter_Get),
		      _8976=[let_xx, Bq_append_Ret, Setter_Get]
		  )
		),
		_8976=MFResult
	      ),
	      block_exit(pushnew, MFResult),
	      true).
:- set_opv(mf_pushnew, type_of, sys_macro),
   set_opv(pushnew, symbol_function, mf_pushnew),
   DefMacroResult=pushnew.
/*
:- side_effect(assert_lsp(pushnew,
			  lambda_def(defmacro,
				     pushnew,
				     mf_pushnew,
				     
				     [ c38_environment,
				       sys_env,
				       sys_item,
				       sys_place,
				       c38_rest,
				       sys_keys
				     ],
				     
				     [ 
				       [ if,
					 
					 [ and,
					   [symbolp, sys_place],
					   
					   [ eq,
					     sys_place,
					     [macroexpand, sys_place, sys_env]
					   ]
					 ],
					 
					 [ '#BQ',
					   
					   [ setq,
					     ['#COMMA', sys_place],
					     
					     [ adjoin,
					       ['#COMMA', sys_item],
					       ['#COMMA', sys_place],
					       ['#BQ-COMMA-ELIPSE', sys_keys]
					     ]
					   ]
					 ],
					 
					 [ multiple_value_bind,
					   
					   [ sys_dummies,
					     sys_vals,
					     sys_newval,
					     sys_setter,
					     sys_getter
					   ],
					   
					   [ get_setf_expansion,
					     sys_place,
					     sys_env
					   ],
					   
					   [ let,
					     [[sys_g, [gensym]]],
					     
					     [ '#BQ',
					       
					       [ let_xx,
						 
						 [ 
						   [ ['#COMMA', sys_g],
						     ['#COMMA', sys_item]
						   ],
						   
						   [ '#BQ-COMMA-ELIPSE',
						     
						     [ mapcar,
						       function(list),
						       sys_dummies,
						       sys_vals
						     ]
						   ],
						   
						   [ 
						     [ '#COMMA',
						       [car, sys_newval]
						     ],
						     
						     [ adjoin,
						       ['#COMMA', sys_g],
						       ['#COMMA', sys_getter],
						       
						       [ '#BQ-COMMA-ELIPSE',
							 sys_keys
						       ]
						     ]
						   ]
						 ],
						 ['#COMMA', sys_setter]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(pushnew,
			  arglist_info(pushnew,
				       mf_pushnew,
				       
				       [ c38_environment,
					 sys_env,
					 sys_item,
					 sys_place,
					 c38_rest,
					 sys_keys
				       ],
				       arginfo{ all:[sys_item, sys_place],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[environment, rest],
						env:[sys_env],
						key:0,
						names:
						      [ sys_env,
							sys_item,
							sys_place,
							sys_keys
						      ],
						opt:0,
						req:[sys_item, sys_place],
						rest:[sys_keys],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(pushnew, init_args(2, mf_pushnew))).
*/
/*
; Adapted from SBCL.
*/
/*
(defmacro pop (&environment env place)
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(prog1 (car ,place)
	      (setq ,place (cdr ,place)))
      (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
        (do* ((d dummies (cdr d))
              (v vals (cdr v))
              (let-list nil))
             ((null d)
              (push (list (car newval) getter) let-list)
              `(let* ,(nreverse let-list)
                 (prog1 (car ,(car newval))
                        (setq ,(car newval) (cdr ,(car newval)))
                        ,setter)))
          (push (list (car d) (car v)) let-list)))))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-00.lisp:7644 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,pop,['&environment',env,place],[if,[and,[symbolp,place],[eq,place,[macroexpand,place,env]]],['#BQ',[prog1,[car,['#COMMA',place]],[setq,['#COMMA',place],[cdr,['#COMMA',place]]]]],['multiple-value-bind',[dummies,vals,newval,setter,getter],['get-setf-expansion',place,env],['do*',[[d,dummies,[cdr,d]],[v,vals,[cdr,v]],['let-list',[]]],[[null,d],[push,[list,[car,newval],getter],'let-list'],['#BQ',['let*',['#COMMA',[nreverse,'let-list']],[prog1,[car,['#COMMA',[car,newval]]],[setq,['#COMMA',[car,newval]],[cdr,['#COMMA',[car,newval]]]],['#COMMA',setter]]]]],[push,[list,[car,d],[car,v]],'let-list']]]]])
/*
% macroexpand:-[push,[list,[car,sys_newval],sys_getter],sys_let_list].
*/
/*
% into:-[setq,sys_let_list,[cons,[list,[car,sys_newval],sys_getter],sys_let_list]].
*/
/*
% macroexpand:-[push,[list,[car,sys_d],[car,sys_v]],sys_let_list].
*/
/*
% into:-[setq,sys_let_list,[cons,[list,[car,sys_d],[car,sys_v]],sys_let_list]].
*/
/*
% macroexpand:-[push,[list,[car,sys_newval],sys_getter],sys_let_list].
*/
/*
% into:-[setq,sys_let_list,[cons,[list,[car,sys_newval],sys_getter],sys_let_list]].
*/
/*
% macroexpand:-[push,[list,[car,sys_d],[car,sys_v]],sys_let_list].
*/
/*
% into:-[setq,sys_let_list,[cons,[list,[car,sys_d],[car,sys_v]],sys_let_list]].
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand1),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       pop,
					       kw_special,
					       sf_pop)).
*/
wl:lambda_def(defmacro, pop, mf_pop, [c38_environment, sys_env, sys_place], [[if, [and, [symbolp, sys_place], [eq, sys_place, [macroexpand, sys_place, sys_env]]], ['#BQ', [prog1, [car, ['#COMMA', sys_place]], [setq, ['#COMMA', sys_place], [cdr, ['#COMMA', sys_place]]]]], [multiple_value_bind, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter], [get_setf_expansion, sys_place, sys_env], [do_xx, [[sys_d, sys_dummies, [cdr, sys_d]], [sys_v, sys_vals, [cdr, sys_v]], [sys_let_list, []]], [[null, sys_d], [push, [list, [car, sys_newval], sys_getter], sys_let_list], ['#BQ', [let_xx, ['#COMMA', [nreverse, sys_let_list]], [prog1, [car, ['#COMMA', [car, sys_newval]]], [setq, ['#COMMA', [car, sys_newval]], [cdr, ['#COMMA', [car, sys_newval]]]], ['#COMMA', sys_setter]]]]], [push, [list, [car, sys_d], [car, sys_v]], sys_let_list]]]]]).
wl:arglist_info(pop, mf_pop, [c38_environment, sys_env, sys_place], arginfo{all:[sys_place], allow_other_keys:0, aux:0, body:0, complex:[environment], env:[sys_env], key:0, names:[sys_env, sys_place], opt:0, req:[sys_place], rest:0, sublists:0, whole:0}).
wl: init_args(1, mf_pop).

/*

### Compiled Macro Operator: `CL:POP` 
*/
sf_pop(Env_In, Place_In, RestNKeys, FResult) :-
	mf_pop([pop, Place_In|RestNKeys], Env_In, MFResult),
	f_sys_env_eval(Env_In, MFResult, FResult).
/*

### Compiled Macro Function: `CL:POP` 
*/
mf_pop([pop, Place_In|RestNKeys], Env_In, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_env, Env_In), bv(sys_place, Place_In)],
	catch(( ( get_var(GEnv, sys_place, Place_Get),
		  (   is_symbolp(Place_Get)
		  ->  get_var(GEnv, sys_env, Env_Get),
		      get_var(GEnv, sys_place, Place_Get12),
		      f_macroexpand([Place_Get12, Env_Get], Macroexpand_Ret),
		      f_eq(Place_Get12, Macroexpand_Ret, TrueResult),
		      IFTEST=TrueResult
		  ;   IFTEST=[]
		  ),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, sys_place, Place_Get16),
		      _8118=[prog1, [car, Place_Get16], [setq, Place_Get16, [cdr, Place_Get16]]]
		  ;   LEnv=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_newval, []), bv(sys_setter, []), bv(sys_getter, [])|GEnv],
		      get_var(LEnv, sys_env, Env_Get23),
		      get_var(LEnv, sys_place, Place_Get22),
		      f_get_setf_expansion(Place_Get22,
					   [Env_Get23],
					   Setf_expansion_Ret),
		      setq_from_values(LEnv,
				       
				       [ sys_dummies,
					 sys_vals,
					 sys_newval,
					 sys_setter,
					 sys_getter
				       ]),
		      get_var(LEnv, sys_dummies, Dummies_Get),
		      LEnv26=[bv(sys_d, Dummies_Get)|LEnv],
		      get_var(LEnv26, sys_vals, Vals_Get),
		      LEnv31=[bv(sys_v, Vals_Get)|LEnv26],
		      BlockExitEnv=[bv(sys_let_list, [])|LEnv31],
		      catch(( call_addr_block(BlockExitEnv,
					      (push_label(do_label_9), get_var(BlockExitEnv, sys_d, IFTEST63), (IFTEST63==[]->get_var(BlockExitEnv, sys_newval, Newval_Get69), f_car(Newval_Get69, Car_Ret), get_var(BlockExitEnv, sys_getter, Getter_Get70), CAR=[Car_Ret, Getter_Get70], get_var(BlockExitEnv, sys_let_list, Let_list_Get71), Let_list=[CAR|Let_list_Get71], set_var(BlockExitEnv, sys_let_list, Let_list), get_var(BlockExitEnv, sys_let_list, Let_list_Get72), f_nreverse(Let_list_Get72, Nreverse_Ret), get_var(BlockExitEnv, sys_newval, Newval_Get73), f_car(Newval_Get73, Car_Ret102), get_var(BlockExitEnv, sys_newval, Newval_Get74), f_car(Newval_Get74, Car_Ret103), get_var(BlockExitEnv, sys_newval, Newval_Get75), f_car(Newval_Get75, Car_Ret104), get_var(BlockExitEnv, sys_setter, Setter_Get76), throw(block_exit([], [let_xx, Nreverse_Ret, [prog1, [car, Car_Ret102], [setq, Car_Ret103, [cdr, Car_Ret104]], Setter_Get76]])), _TBResult=ThrowResult67;get_var(BlockExitEnv, sys_d, D_Get78), f_car(D_Get78, Car_Ret105), get_var(BlockExitEnv, sys_v, V_Get79), f_car(V_Get79, Car_Ret106), CAR107=[Car_Ret105, Car_Ret106], get_var(BlockExitEnv, sys_let_list, Let_list_Get80), Let_list92=[CAR107|Let_list_Get80], set_var(BlockExitEnv, sys_let_list, Let_list92), get_var(BlockExitEnv, sys_d, D_Get81), f_cdr(D_Get81, D), get_var(BlockExitEnv, sys_v, V_Get82), f_cdr(V_Get82, V), set_var(BlockExitEnv, sys_d, D), set_var(BlockExitEnv, sys_v, V), goto(do_label_9, BlockExitEnv), _TBResult=_GORES83)),
					      
					      [ addr(addr_tagbody_9_do_label_9,
						     do_label_9,
						     '$unused',
						     BlockExitEnv,
						     (get_var(BlockExitEnv, sys_d, IFTEST38), (IFTEST38==[]->get_var(BlockExitEnv, sys_newval, Car_Param), f_car(Car_Param, Car_Ret108), get_var(BlockExitEnv, sys_getter, Get_var_Ret), CAR111=[Car_Ret108, Get_var_Ret], get_var(BlockExitEnv, sys_let_list, Get_var_Ret110), Set_var_Ret=[CAR111|Get_var_Ret110], set_var(BlockExitEnv, sys_let_list, Set_var_Ret), get_var(BlockExitEnv, sys_let_list, Let_list_Get47), f_nreverse(Let_list_Get47, Nreverse_Ret113), get_var(BlockExitEnv, sys_newval, Newval_Get48), f_car(Newval_Get48, Car_Ret114), get_var(BlockExitEnv, sys_newval, Newval_Get49), f_car(Newval_Get49, Car_Ret115), get_var(BlockExitEnv, sys_newval, Newval_Get50), f_car(Newval_Get50, Car_Ret116), get_var(BlockExitEnv, sys_setter, Get_var_Ret117), throw(block_exit([], [let_xx, Nreverse_Ret113, [prog1, [car, Car_Ret114], [setq, Car_Ret115, [cdr, Car_Ret116]], Get_var_Ret117]])), _10278=ThrowResult;get_var(BlockExitEnv, sys_d, D_Get53), f_car(D_Get53, Car_Ret118), get_var(BlockExitEnv, sys_v, Car_Param96), f_car(Car_Param96, Car_Ret119), CAR120=[Car_Ret118, Car_Ret119], get_var(BlockExitEnv, sys_let_list, Let_list_Get55), Set_var_Ret121=[CAR120|Let_list_Get55], set_var(BlockExitEnv, sys_let_list, Set_var_Ret121), get_var(BlockExitEnv, sys_d, D_Get56), f_cdr(D_Get56, Cdr_Ret), get_var(BlockExitEnv, sys_v, V_Get57), f_cdr(V_Get57, Cdr_Ret123), set_var(BlockExitEnv, sys_d, Cdr_Ret), set_var(BlockExitEnv, sys_v, Cdr_Ret123), goto(do_label_9, BlockExitEnv), _10278=_GORES)))
					      ]),
			      []=LetResult35
			    ),
			    block_exit([], LetResult35),
			    true),
		      _8118=LetResult35
		  )
		),
		_8118=MFResult
	      ),
	      block_exit(pop, MFResult),
	      true).
:- set_opv(mf_pop, type_of, sys_macro),
   set_opv(pop, symbol_function, mf_pop),
   DefMacroResult=pop.
/*
:- side_effect(assert_lsp(pop,
			  lambda_def(defmacro,
				     pop,
				     mf_pop,
				     [c38_environment, sys_env, sys_place],
				     
				     [ 
				       [ if,
					 
					 [ and,
					   [symbolp, sys_place],
					   
					   [ eq,
					     sys_place,
					     [macroexpand, sys_place, sys_env]
					   ]
					 ],
					 
					 [ '#BQ',
					   
					   [ prog1,
					     [car, ['#COMMA', sys_place]],
					     
					     [ setq,
					       ['#COMMA', sys_place],
					       [cdr, ['#COMMA', sys_place]]
					     ]
					   ]
					 ],
					 
					 [ multiple_value_bind,
					   
					   [ sys_dummies,
					     sys_vals,
					     sys_newval,
					     sys_setter,
					     sys_getter
					   ],
					   
					   [ get_setf_expansion,
					     sys_place,
					     sys_env
					   ],
					   
					   [ do_xx,
					     
					     [ [sys_d, sys_dummies, [cdr, sys_d]],
					       [sys_v, sys_vals, [cdr, sys_v]],
					       [sys_let_list, []]
					     ],
					     
					     [ [null, sys_d],
					       
					       [ push,
						 
						 [ list,
						   [car, sys_newval],
						   sys_getter
						 ],
						 sys_let_list
					       ],
					       
					       [ '#BQ',
						 
						 [ let_xx,
						   
						   [ '#COMMA',
						     [nreverse, sys_let_list]
						   ],
						   
						   [ prog1,
						     
						     [ car,
						       
						       [ '#COMMA',
							 [car, sys_newval]
						       ]
						     ],
						     
						     [ setq,
						       
						       [ '#COMMA',
							 [car, sys_newval]
						       ],
						       
						       [ cdr,
							 
							 [ '#COMMA',
							   [car, sys_newval]
							 ]
						       ]
						     ],
						     ['#COMMA', sys_setter]
						   ]
						 ]
					       ]
					     ],
					     
					     [ push,
					       [list, [car, sys_d], [car, sys_v]],
					       sys_let_list
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(pop,
			  arglist_info(pop,
				       mf_pop,
				       [c38_environment, sys_env, sys_place],
				       arginfo{ all:[sys_place],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[environment],
						env:[sys_env],
						key:0,
						names:[sys_env, sys_place],
						opt:0,
						req:[sys_place],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(pop, init_args(1, mf_pop))).
*/


%; Total compilation time: 12.695 seconds

