#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "wam-cl-init-20" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Tue Jan 23 00:54:20 2018

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
(in-package #:system)



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:262 **********************/
:-lisp_compile_to_prolog(pkg_sys,['in-package','#:system'])
/*
% macroexpand:-[in_package,system6].
*/
/*
% into:-[eval_when,[kw_compile_toplevel,kw_load_toplevel,kw_execute],[sys_select_package,'$ARRAY'([*],claz_base_character,"SYSTEM")]].
*/
:- do_when([kw_compile_toplevel, kw_load_toplevel, kw_execute],
	   f_sys_select_package('$ARRAY'([*], claz_base_character, "SYSTEM"),
				_Ignored),
	   _Ignored).
/*
(defmacro=sourceinfo psetq (&rest rest)
  (let ((inits nil)
	(sets nil)
	(list rest))
    (tagbody
     start
       (when (cddr list)
	 (push (list (gensym) (cadr list)) inits)
	 (setq list (cddr list))
	 (go start)))
    (setq list inits)
    (tagbody
     start
       (when (cddr rest)
	 (push (caar list) sets)
	 (push (car rest) sets)
	 (setq list (cdr list))
	 (setq rest (cddr rest))
	 (go start)))
    `(let ,(reverse inits)
      (setq ,@sets ,@rest))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:296 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo',psetq,['&rest',rest],[let,[[inits,[]],[sets,[]],[list,rest]],[tagbody,start,[when,[cddr,list],[push,[list,[gensym],[cadr,list]],inits],[setq,list,[cddr,list]],[go,start]]],[setq,list,inits],[tagbody,start,[when,[cddr,rest],[push,[caar,list],sets],[push,[car,rest],sets],[setq,list,[cdr,list]],[setq,rest,[cddr,rest]],[go,start]]],['#BQ',[let,['#COMMA',[reverse,inits]],[setq,['#BQ-COMMA-ELIPSE',sets],['#BQ-COMMA-ELIPSE',rest]]]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,psetq,[c38_rest,rest],[let,[[sys_inits,[]],[sys_sets,[]],[list,rest]],[tagbody,sys_start,[when,[cddr,list],[push,[list,[gensym],[cadr,list]],sys_inits],[setq,list,[cddr,list]],[go,sys_start]]],[setq,list,sys_inits],[tagbody,sys_start,[when,[cddr,rest],[push,[caar,list],sys_sets],[push,[car,rest],sys_sets],[setq,list,[cdr,list]],[setq,rest,[cddr,rest]],[go,sys_start]]],['#BQ',[let,['#COMMA',[reverse,sys_inits]],[setq,['#BQ-COMMA-ELIPSE',sys_sets],['#BQ-COMMA-ELIPSE',rest]]]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,psetq],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,psetq,[c38_rest,rest],[let,[[sys_inits,[]],[sys_sets,[]],[list,rest]],[tagbody,sys_start,[when,[cddr,list],[push,[list,[gensym],[cadr,list]],sys_inits],[setq,list,[cddr,list]],[go,sys_start]]],[setq,list,sys_inits],[tagbody,sys_start,[when,[cddr,rest],[push,[caar,list],sys_sets],[push,[car,rest],sys_sets],[setq,list,[cdr,list]],[setq,rest,[cddr,rest]],[go,sys_start]]],['#BQ',[let,['#COMMA',[reverse,sys_inits]],[setq,['#BQ-COMMA-ELIPSE',sys_sets],['#BQ-COMMA-ELIPSE',rest]]]]]]]].
*/
:- f_sys_put_sysprop(psetq,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       psetq,
		       [c38_rest, rest],
		       
		       [ let,
			 [[sys_inits, []], [sys_sets, []], [list, rest]],
			 
			 [ tagbody,
			   sys_start,
			   
			   [ when,
			     [cddr, list],
			     [push, [list, [gensym], [cadr, list]], sys_inits],
			     [setq, list, [cddr, list]],
			     [go, sys_start]
			   ]
			 ],
			 [setq, list, sys_inits],
			 
			 [ tagbody,
			   sys_start,
			   
			   [ when,
			     [cddr, rest],
			     [push, [caar, list], sys_sets],
			     [push, [car, rest], sys_sets],
			     [setq, list, [cdr, list]],
			     [setq, rest, [cddr, rest]],
			     [go, sys_start]
			   ]
			 ],
			 
			 [ '#BQ',
			   
			   [ let,
			     ['#COMMA', [reverse, sys_inits]],
			     
			     [ setq,
			       ['#BQ-COMMA-ELIPSE', sys_sets],
			       ['#BQ-COMMA-ELIPSE', rest]
			     ]
			   ]
			 ]
		       ]
		     ],
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo return (&optional result)
  `(return-from nil ,result))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:785 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo',return,['&optional',result],['#BQ',['return-from',[],['#COMMA',result]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,return,[c38_optional,sys_result],['#BQ',[return_from,[],['#COMMA',sys_result]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,return],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,return,[c38_optional,sys_result],['#BQ',[return_from,[],['#COMMA',sys_result]]]]]].
*/
:- f_sys_put_sysprop(return,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       return,
		       [c38_optional, sys_result],
		       ['#BQ', [return_from, [], ['#COMMA', sys_result]]]
		     ],
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo when (test-form &rest forms)
  `(if ,test-form (progn ,@forms)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:866 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo',when,['test-form','&rest',forms],['#BQ',[if,['#COMMA','test-form'],[progn,['#BQ-COMMA-ELIPSE',forms]]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,when,[sys_test_form,c38_rest,sys_forms],['#BQ',[if,['#COMMA',sys_test_form],[progn,['#BQ-COMMA-ELIPSE',sys_forms]]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,when],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,when,[sys_test_form,c38_rest,sys_forms],['#BQ',[if,['#COMMA',sys_test_form],[progn,['#BQ-COMMA-ELIPSE',sys_forms]]]]]]].
*/
:- f_sys_put_sysprop(when,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       when,
		       [sys_test_form, c38_rest, sys_forms],
		       
		       [ '#BQ',
			 
			 [ if,
			   ['#COMMA', sys_test_form],
			   [progn, ['#BQ-COMMA-ELIPSE', sys_forms]]
			 ]
		       ]
		     ],
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo unless (test-form &rest forms)
  `(if (not ,test-form) (progn ,@forms)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:956 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo',unless,['test-form','&rest',forms],['#BQ',[if,[not,['#COMMA','test-form']],[progn,['#BQ-COMMA-ELIPSE',forms]]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,unless,[sys_test_form,c38_rest,sys_forms],['#BQ',[if,[not,['#COMMA',sys_test_form]],[progn,['#BQ-COMMA-ELIPSE',sys_forms]]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,unless],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,unless,[sys_test_form,c38_rest,sys_forms],['#BQ',[if,[not,['#COMMA',sys_test_form]],[progn,['#BQ-COMMA-ELIPSE',sys_forms]]]]]]].
*/
:- f_sys_put_sysprop(unless,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       unless,
		       [sys_test_form, c38_rest, sys_forms],
		       
		       [ '#BQ',
			 
			 [ if,
			   [not, ['#COMMA', sys_test_form]],
			   [progn, ['#BQ-COMMA-ELIPSE', sys_forms]]
			 ]
		       ]
		     ],
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo and (&rest forms)
  (if forms
      (if (cdr forms)
	  `(when ,(car forms) (and ,@(cdr forms)))
	(car forms))
    `t))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:1054 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo',and,['&rest',forms],[if,forms,[if,[cdr,forms],['#BQ',[when,['#COMMA',[car,forms]],[and,['#BQ-COMMA-ELIPSE',[cdr,forms]]]]],[car,forms]],['#BQ',t]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,and,[c38_rest,sys_forms],[if,sys_forms,[if,[cdr,sys_forms],['#BQ',[when,['#COMMA',[car,sys_forms]],[and,['#BQ-COMMA-ELIPSE',[cdr,sys_forms]]]]],[car,sys_forms]],['#BQ',t]]].
*/
/*
% into:-[sys_put_sysprop,[quote,and],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,and,[c38_rest,sys_forms],[if,sys_forms,[if,[cdr,sys_forms],['#BQ',[when,['#COMMA',[car,sys_forms]],[and,['#BQ-COMMA-ELIPSE',[cdr,sys_forms]]]]],[car,sys_forms]],['#BQ',t]]]]].
*/
:- f_sys_put_sysprop(and,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       and,
		       [c38_rest, sys_forms],
		       
		       [ if,
			 sys_forms,
			 
			 [ if,
			   [cdr, sys_forms],
			   
			   [ '#BQ',
			     
			     [ when,
			       ['#COMMA', [car, sys_forms]],
			       [and, ['#BQ-COMMA-ELIPSE', [cdr, sys_forms]]]
			     ]
			   ],
			   [car, sys_forms]
			 ],
			 ['#BQ', t]
		       ]
		     ],
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo or (&rest forms)
  (if forms
      (if (cdr forms)
	  (let ((temp (gensym)))
	    `(let ((,temp ,(car forms)))
	      (if ,temp
		  ,temp
		(or ,@(cdr forms)))))
	(car forms))
    `nil))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:1202 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo',or,['&rest',forms],[if,forms,[if,[cdr,forms],[let,[[temp,[gensym]]],['#BQ',[let,[[['#COMMA',temp],['#COMMA',[car,forms]]]],[if,['#COMMA',temp],['#COMMA',temp],[or,['#BQ-COMMA-ELIPSE',[cdr,forms]]]]]]],[car,forms]],['#BQ',[]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,or,[c38_rest,sys_forms],[if,sys_forms,[if,[cdr,sys_forms],[let,[[sys_temp,[gensym]]],['#BQ',[let,[[['#COMMA',sys_temp],['#COMMA',[car,sys_forms]]]],[if,['#COMMA',sys_temp],['#COMMA',sys_temp],[or,['#BQ-COMMA-ELIPSE',[cdr,sys_forms]]]]]]],[car,sys_forms]],['#BQ',[]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,or],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,or,[c38_rest,sys_forms],[if,sys_forms,[if,[cdr,sys_forms],[let,[[sys_temp,[gensym]]],['#BQ',[let,[[['#COMMA',sys_temp],['#COMMA',[car,sys_forms]]]],[if,['#COMMA',sys_temp],['#COMMA',sys_temp],[or,['#BQ-COMMA-ELIPSE',[cdr,sys_forms]]]]]]],[car,sys_forms]],['#BQ',[]]]]]].
*/
:- f_sys_put_sysprop(or,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       or,
		       [c38_rest, sys_forms],
		       
		       [ if,
			 sys_forms,
			 
			 [ if,
			   [cdr, sys_forms],
			   
			   [ let,
			     [[sys_temp, [gensym]]],
			     
			     [ '#BQ',
			       
			       [ let,
				 
				 [ 
				   [ ['#COMMA', sys_temp],
				     ['#COMMA', [car, sys_forms]]
				   ]
				 ],
				 
				 [ if,
				   ['#COMMA', sys_temp],
				   ['#COMMA', sys_temp],
				   [or, ['#BQ-COMMA-ELIPSE', [cdr, sys_forms]]]
				 ]
			       ]
			     ]
			   ],
			   [car, sys_forms]
			 ],
			 ['#BQ', []]
		       ]
		     ],
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo cond (&rest clauses)
  (when clauses
    (if (cdar clauses)
	`(if ,(caar clauses)
	     (progn ,@(cdar clauses))
	     (cond ,@(cdr clauses)))
	`(or ,(caar clauses)
	     (cond ,@(cdr clauses))))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:1422 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo',cond,['&rest',clauses],[when,clauses,[if,[cdar,clauses],['#BQ',[if,['#COMMA',[caar,clauses]],[progn,['#BQ-COMMA-ELIPSE',[cdar,clauses]]],[cond,['#BQ-COMMA-ELIPSE',[cdr,clauses]]]]],['#BQ',[or,['#COMMA',[caar,clauses]],[cond,['#BQ-COMMA-ELIPSE',[cdr,clauses]]]]]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,cond,[c38_rest,sys_clauses],[when,sys_clauses,[if,[cdar,sys_clauses],['#BQ',[if,['#COMMA',[caar,sys_clauses]],[progn,['#BQ-COMMA-ELIPSE',[cdar,sys_clauses]]],[cond,['#BQ-COMMA-ELIPSE',[cdr,sys_clauses]]]]],['#BQ',[or,['#COMMA',[caar,sys_clauses]],[cond,['#BQ-COMMA-ELIPSE',[cdr,sys_clauses]]]]]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,cond],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,cond,[c38_rest,sys_clauses],[when,sys_clauses,[if,[cdar,sys_clauses],['#BQ',[if,['#COMMA',[caar,sys_clauses]],[progn,['#BQ-COMMA-ELIPSE',[cdar,sys_clauses]]],[cond,['#BQ-COMMA-ELIPSE',[cdr,sys_clauses]]]]],['#BQ',[or,['#COMMA',[caar,sys_clauses]],[cond,['#BQ-COMMA-ELIPSE',[cdr,sys_clauses]]]]]]]]]].
*/
:- f_sys_put_sysprop(cond,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       cond,
		       [c38_rest, sys_clauses],
		       
		       [ when,
			 sys_clauses,
			 
			 [ if,
			   [cdar, sys_clauses],
			   
			   [ '#BQ',
			     
			     [ if,
			       ['#COMMA', [caar, sys_clauses]],
			       [progn, ['#BQ-COMMA-ELIPSE', [cdar, sys_clauses]]],
			       [cond, ['#BQ-COMMA-ELIPSE', [cdr, sys_clauses]]]
			     ]
			   ],
			   
			   [ '#BQ',
			     
			     [ or,
			       ['#COMMA', [caar, sys_clauses]],
			       [cond, ['#BQ-COMMA-ELIPSE', [cdr, sys_clauses]]]
			     ]
			   ]
			 ]
		       ]
		     ],
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo case (keyform &rest clauses)
  (let ((temp (gensym)))
    (labels ((recur (clauses)
	       (when clauses
		 (if (member (caar clauses) '(otherwise t))
		     `(progn ,@(cdar clauses))
		     `(if ,(if (listp (caar clauses))
			       `(member ,temp ',(caar clauses))
			       `(eql ,temp ',(caar clauses)))
		          (progn ,@(cdar clauses))
		          ,(recur (cdr clauses)))))))
      `(let ((,temp ,keyform))
	,(recur clauses)))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:1653 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo',case,[keyform,'&rest',clauses],[let,[[temp,[gensym]]],[labels,[[recur,[clauses],[when,clauses,[if,[member,[caar,clauses],[quote,[otherwise,t]]],['#BQ',[progn,['#BQ-COMMA-ELIPSE',[cdar,clauses]]]],['#BQ',[if,['#COMMA',[if,[listp,[caar,clauses]],['#BQ',[member,['#COMMA',temp],[quote,['#COMMA',[caar,clauses]]]]],['#BQ',[eql,['#COMMA',temp],[quote,['#COMMA',[caar,clauses]]]]]]],[progn,['#BQ-COMMA-ELIPSE',[cdar,clauses]]],['#COMMA',[recur,[cdr,clauses]]]]]]]]],['#BQ',[let,[[['#COMMA',temp],['#COMMA',keyform]]],['#COMMA',[recur,clauses]]]]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,case,[sys_keyform,c38_rest,sys_clauses],[let,[[sys_temp,[gensym]]],[labels,[[sys_recur,[sys_clauses],[when,sys_clauses,[if,[member,[caar,sys_clauses],[quote,[otherwise,t]]],['#BQ',[progn,['#BQ-COMMA-ELIPSE',[cdar,sys_clauses]]]],['#BQ',[if,['#COMMA',[if,[listp,[caar,sys_clauses]],['#BQ',[member,['#COMMA',sys_temp],[quote,['#COMMA',[caar,sys_clauses]]]]],['#BQ',[eql,['#COMMA',sys_temp],[quote,['#COMMA',[caar,sys_clauses]]]]]]],[progn,['#BQ-COMMA-ELIPSE',[cdar,sys_clauses]]],['#COMMA',[sys_recur,[cdr,sys_clauses]]]]]]]]],['#BQ',[let,[[['#COMMA',sys_temp],['#COMMA',sys_keyform]]],['#COMMA',[sys_recur,sys_clauses]]]]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,case],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,case,[sys_keyform,c38_rest,sys_clauses],[let,[[sys_temp,[gensym]]],[labels,[[sys_recur,[sys_clauses],[when,sys_clauses,[if,[member,[caar,sys_clauses],[quote,[otherwise,t]]],['#BQ',[progn,['#BQ-COMMA-ELIPSE',[cdar,sys_clauses]]]],['#BQ',[if,['#COMMA',[if,[listp,[caar,sys_clauses]],['#BQ',[member,['#COMMA',sys_temp],[quote,['#COMMA',[caar,sys_clauses]]]]],['#BQ',[eql,['#COMMA',sys_temp],[quote,['#COMMA',[caar,sys_clauses]]]]]]],[progn,['#BQ-COMMA-ELIPSE',[cdar,sys_clauses]]],['#COMMA',[sys_recur,[cdr,sys_clauses]]]]]]]]],['#BQ',[let,[[['#COMMA',sys_temp],['#COMMA',sys_keyform]]],['#COMMA',[sys_recur,sys_clauses]]]]]]]]].
*/
:- f_sys_put_sysprop(case,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       case,
		       [sys_keyform, c38_rest, sys_clauses],
		       
		       [ let,
			 [[sys_temp, [gensym]]],
			 
			 [ labels,
			   
			   [ 
			     [ sys_recur,
			       [sys_clauses],
			       
			       [ when,
				 sys_clauses,
				 
				 [ if,
				   
				   [ member,
				     [caar, sys_clauses],
				     [quote, [otherwise, t]]
				   ],
				   
				   [ '#BQ',
				     
				     [ progn,
				       ['#BQ-COMMA-ELIPSE', [cdar, sys_clauses]]
				     ]
				   ],
				   
				   [ '#BQ',
				     
				     [ if,
				       
				       [ '#COMMA',
					 
					 [ if,
					   [listp, [caar, sys_clauses]],
					   
					   [ '#BQ',
					     
					     [ member,
					       ['#COMMA', sys_temp],
					       
					       [ quote,
						 ['#COMMA', [caar, sys_clauses]]
					       ]
					     ]
					   ],
					   
					   [ '#BQ',
					     
					     [ eql,
					       ['#COMMA', sys_temp],
					       
					       [ quote,
						 ['#COMMA', [caar, sys_clauses]]
					       ]
					     ]
					   ]
					 ]
				       ],
				       
				       [ progn,
					 
					 [ '#BQ-COMMA-ELIPSE',
					   [cdar, sys_clauses]
					 ]
				       ],
				       
				       [ '#COMMA',
					 [sys_recur, [cdr, sys_clauses]]
				       ]
				     ]
				   ]
				 ]
			       ]
			     ]
			   ],
			   
			   [ '#BQ',
			     
			     [ let,
			       [[['#COMMA', sys_temp], ['#COMMA', sys_keyform]]],
			       ['#COMMA', [sys_recur, sys_clauses]]
			     ]
			   ]
			 ]
		       ]
		     ],
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo ecase (keyform &rest clauses)
  (let ((temp (gensym)))
    `(let ((,temp ,keyform))
      (case ,temp ,@clauses
	    (error 'type-error :datum ,temp
		   :expected-type `(member ,@(mapcan #'(lambda (x)
							 (if (listp (car x))
							     (car x)
							     (list (car x))))
						     clauses)))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:2130 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo',ecase,[keyform,'&rest',clauses],[let,[[temp,[gensym]]],['#BQ',[let,[[['#COMMA',temp],['#COMMA',keyform]]],[case,['#COMMA',temp],['#BQ-COMMA-ELIPSE',clauses],[error,[quote,'type-error'],':datum',['#COMMA',temp],':expected-type',['#BQ',[member,['#BQ-COMMA-ELIPSE',[mapcan,function([lambda,[x],[if,[listp,[car,x]],[car,x],[list,[car,x]]]]),clauses]]]]]]]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,ecase,[sys_keyform,c38_rest,sys_clauses],[let,[[sys_temp,[gensym]]],['#BQ',[let,[[['#COMMA',sys_temp],['#COMMA',sys_keyform]]],[case,['#COMMA',sys_temp],['#BQ-COMMA-ELIPSE',sys_clauses],[error,[quote,type_error],kw_datum,['#COMMA',sys_temp],kw_expected_type,['#BQ',[member,['#BQ-COMMA-ELIPSE',[mapcan,function([lambda,[sys_x],[if,[listp,[car,sys_x]],[car,sys_x],[list,[car,sys_x]]]]),sys_clauses]]]]]]]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,ecase],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,ecase,[sys_keyform,c38_rest,sys_clauses],[let,[[sys_temp,[gensym]]],['#BQ',[let,[[['#COMMA',sys_temp],['#COMMA',sys_keyform]]],[case,['#COMMA',sys_temp],['#BQ-COMMA-ELIPSE',sys_clauses],[error,[quote,type_error],kw_datum,['#COMMA',sys_temp],kw_expected_type,['#BQ',[member,['#BQ-COMMA-ELIPSE',[mapcan,function([lambda,[sys_x],[if,[listp,[car,sys_x]],[car,sys_x],[list,[car,sys_x]]]]),sys_clauses]]]]]]]]]]]].
*/
:- f_sys_put_sysprop(ecase,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       ecase,
		       [sys_keyform, c38_rest, sys_clauses],
		       
		       [ let,
			 [[sys_temp, [gensym]]],
			 
			 [ '#BQ',
			   
			   [ let,
			     [[['#COMMA', sys_temp], ['#COMMA', sys_keyform]]],
			     
			     [ case,
			       ['#COMMA', sys_temp],
			       ['#BQ-COMMA-ELIPSE', sys_clauses],
			       
			       [ error,
				 [quote, type_error],
				 kw_datum,
				 ['#COMMA', sys_temp],
				 kw_expected_type,
				 
				 [ '#BQ',
				   
				   [ member,
				     
				     [ '#BQ-COMMA-ELIPSE',
				       
				       [ mapcan,
					 function(
						  [ lambda,
						    [sys_x],
						    
						    [ if,
						      [listp, [car, sys_x]],
						      [car, sys_x],
						      [list, [car, sys_x]]
						    ]
						  ]),
					 sys_clauses
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
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo multiple-value-bind (vars values-form &rest forms)
  `(multiple-value-call #'(lambda (&optional ,@vars &rest ,(gensym))
			    ,@forms)
                        ,values-form))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:2468 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo','multiple-value-bind',[vars,'values-form','&rest',forms],['#BQ',['multiple-value-call',function([lambda,['&optional',['#BQ-COMMA-ELIPSE',vars],'&rest',['#COMMA',[gensym]]],['#BQ-COMMA-ELIPSE',forms]]),['#COMMA','values-form']]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,multiple_value_bind,[sys_vars,sys_values_form,c38_rest,sys_forms],['#BQ',[multiple_value_call,function([lambda,[c38_optional,['#BQ-COMMA-ELIPSE',sys_vars],c38_rest,['#COMMA',[gensym]]],['#BQ-COMMA-ELIPSE',sys_forms]]),['#COMMA',sys_values_form]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,multiple_value_bind],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,multiple_value_bind,[sys_vars,sys_values_form,c38_rest,sys_forms],['#BQ',[multiple_value_call,function([lambda,[c38_optional,['#BQ-COMMA-ELIPSE',sys_vars],c38_rest,['#COMMA',[gensym]]],['#BQ-COMMA-ELIPSE',sys_forms]]),['#COMMA',sys_values_form]]]]]].
*/
:- f_sys_put_sysprop(multiple_value_bind,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       multiple_value_bind,
		       [sys_vars, sys_values_form, c38_rest, sys_forms],
		       
		       [ '#BQ',
			 
			 [ multiple_value_call,
			   function(
				    [ lambda,
				      
				      [ c38_optional,
					['#BQ-COMMA-ELIPSE', sys_vars],
					c38_rest,
					['#COMMA', [gensym]]
				      ],
				      ['#BQ-COMMA-ELIPSE', sys_forms]
				    ]),
			   ['#COMMA', sys_values_form]
			 ]
		       ]
		     ],
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo multiple-value-list (form)
  `(multiple-value-call #'list ,form))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:2670 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo','multiple-value-list',[form],['#BQ',['multiple-value-call',function(list),['#COMMA',form]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,multiple_value_list,[sys_form],['#BQ',[multiple_value_call,function(list),['#COMMA',sys_form]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,multiple_value_list],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,multiple_value_list,[sys_form],['#BQ',[multiple_value_call,function(list),['#COMMA',sys_form]]]]]].
*/
:- f_sys_put_sysprop(multiple_value_list,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       multiple_value_list,
		       [sys_form],
		       
		       [ '#BQ',
			 
			 [ multiple_value_call,
			   function(list),
			   ['#COMMA', sys_form]
			 ]
		       ]
		     ],
		     [],
		     _Ignored).
/*
(defun=sourceinfo values-list (list) (apply #'values list))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:2761 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defun=sourceinfo','values-list',[list],[apply,function(values),list]])
/*
% macroexpand:-[sys_defun_c61_sourceinfo,values_list,[list],[apply,function(values),list]].
*/
/*
% into:-[sys_put_sysprop,[quote,values_list],[quote,sys_defun_c61_sourceinfo],['#BQ',[defun,['#COMMA',[quote,values_list]],['#COMMA',[quote,[list]]],['#COMMA',[quote,[apply,function(values),list]]]]]].
*/
:- f_sys_put_sysprop(values_list,
		     sys_defun_c61_sourceinfo,
		     [defun, values_list, [list], [apply, function(values), list]],
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo prog (inits &rest forms)
  `(block nil
    (let ,inits
      (tagbody ,@forms))))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:2824 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo',prog,[inits,'&rest',forms],['#BQ',[block,[],[let,['#COMMA',inits],[tagbody,['#BQ-COMMA-ELIPSE',forms]]]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,prog,[sys_inits,c38_rest,sys_forms],['#BQ',[block,[],[let,['#COMMA',sys_inits],[tagbody,['#BQ-COMMA-ELIPSE',sys_forms]]]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,prog],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,prog,[sys_inits,c38_rest,sys_forms],['#BQ',[block,[],[let,['#COMMA',sys_inits],[tagbody,['#BQ-COMMA-ELIPSE',sys_forms]]]]]]]].
*/
:- f_sys_put_sysprop(prog,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       prog,
		       [sys_inits, c38_rest, sys_forms],
		       
		       [ '#BQ',
			 
			 [ block,
			   [],
			   
			   [ let,
			     ['#COMMA', sys_inits],
			     [tagbody, ['#BQ-COMMA-ELIPSE', sys_forms]]
			   ]
			 ]
		       ]
		     ],
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo prog* (inits &rest forms)
  `(block nil
    (let* ,inits
      (tagbody ,@forms))))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:2931 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo','prog*',[inits,'&rest',forms],['#BQ',[block,[],['let*',['#COMMA',inits],[tagbody,['#BQ-COMMA-ELIPSE',forms]]]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,prog_xx,[sys_inits,c38_rest,sys_forms],['#BQ',[block,[],[let_xx,['#COMMA',sys_inits],[tagbody,['#BQ-COMMA-ELIPSE',sys_forms]]]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,prog_xx],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,prog_xx,[sys_inits,c38_rest,sys_forms],['#BQ',[block,[],[let_xx,['#COMMA',sys_inits],[tagbody,['#BQ-COMMA-ELIPSE',sys_forms]]]]]]]].
*/
:- f_sys_put_sysprop(prog_xx,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       prog_xx,
		       [sys_inits, c38_rest, sys_forms],
		       
		       [ '#BQ',
			 
			 [ block,
			   [],
			   
			   [ let_xx,
			     ['#COMMA', sys_inits],
			     [tagbody, ['#BQ-COMMA-ELIPSE', sys_forms]]
			   ]
			 ]
		       ]
		     ],
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo prog1 (first-form &rest forms)
  (let ((temp (gensym)))
    `(let ((,temp ,first-form))
      ,@forms
      ,temp)))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:3040 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo',prog1,['first-form','&rest',forms],[let,[[temp,[gensym]]],['#BQ',[let,[[['#COMMA',temp],['#COMMA','first-form']]],['#BQ-COMMA-ELIPSE',forms],['#COMMA',temp]]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,prog1,[sys_first_form,c38_rest,sys_forms],[let,[[sys_temp,[gensym]]],['#BQ',[let,[[['#COMMA',sys_temp],['#COMMA',sys_first_form]]],['#BQ-COMMA-ELIPSE',sys_forms],['#COMMA',sys_temp]]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,prog1],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,prog1,[sys_first_form,c38_rest,sys_forms],[let,[[sys_temp,[gensym]]],['#BQ',[let,[[['#COMMA',sys_temp],['#COMMA',sys_first_form]]],['#BQ-COMMA-ELIPSE',sys_forms],['#COMMA',sys_temp]]]]]]].
*/
:- f_sys_put_sysprop(prog1,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       prog1,
		       [sys_first_form, c38_rest, sys_forms],
		       
		       [ let,
			 [[sys_temp, [gensym]]],
			 
			 [ '#BQ',
			   
			   [ let,
			     [[['#COMMA', sys_temp], ['#COMMA', sys_first_form]]],
			     ['#BQ-COMMA-ELIPSE', sys_forms],
			     ['#COMMA', sys_temp]
			   ]
			 ]
		       ]
		     ],
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo prog2 (first-form second-form &rest forms)
  (let ((temp (gensym)))
    `(progn
      ,first-form
      (let ((,temp ,second-form))
	,@forms
	,temp))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:3183 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo',prog2,['first-form','second-form','&rest',forms],[let,[[temp,[gensym]]],['#BQ',[progn,['#COMMA','first-form'],[let,[[['#COMMA',temp],['#COMMA','second-form']]],['#BQ-COMMA-ELIPSE',forms],['#COMMA',temp]]]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,prog2,[sys_first_form,sys_second_form,c38_rest,sys_forms],[let,[[sys_temp,[gensym]]],['#BQ',[progn,['#COMMA',sys_first_form],[let,[[['#COMMA',sys_temp],['#COMMA',sys_second_form]]],['#BQ-COMMA-ELIPSE',sys_forms],['#COMMA',sys_temp]]]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,prog2],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,prog2,[sys_first_form,sys_second_form,c38_rest,sys_forms],[let,[[sys_temp,[gensym]]],['#BQ',[progn,['#COMMA',sys_first_form],[let,[[['#COMMA',sys_temp],['#COMMA',sys_second_form]]],['#BQ-COMMA-ELIPSE',sys_forms],['#COMMA',sys_temp]]]]]]]].
*/
:- f_sys_put_sysprop(prog2,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       prog2,
		       [sys_first_form, sys_second_form, c38_rest, sys_forms],
		       
		       [ let,
			 [[sys_temp, [gensym]]],
			 
			 [ '#BQ',
			   
			   [ progn,
			     ['#COMMA', sys_first_form],
			     
			     [ let,
			       
			       [ 
				 [ ['#COMMA', sys_temp],
				   ['#COMMA', sys_second_form]
				 ]
			       ],
			       ['#BQ-COMMA-ELIPSE', sys_forms],
			       ['#COMMA', sys_temp]
			     ]
			   ]
			 ]
		       ]
		     ],
		     [],
		     _Ignored).
/*
#+BUILTIN
#+(or WAM-CL LISP500)
(defun equal (a b)
  (or (eql a b)
      (cond
	((not a) nil)
	((consp a) (and (consp b)
			(equal (car a) (car b))
			(equal (cdr a) (cdr b))))
	((stringp a) (and (stringp b)
			  (string= a b)))
	((bit-vector-p a) (and (bit-vector-p b)
			       (= (length a) (length b))
			       (dotimes (i (length a) t)
				 (when (/= (aref a i) (aref b i))
				   (return))))))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:3365 **********************/
:-lisp_compile_to_prolog(pkg_sys,'$COMMENT'([flag_removed,[+,':BUILTIN'],[#+,[':or',':WAM-CL',':LISP500'],[defun,equal,[a,b],[or,[eql,a,b],[cond,[[not,a],[]],[[consp,a],[and,[consp,b],[equal,[car,a],[car,b]],[equal,[cdr,a],[cdr,b]]]],[[stringp,a],[and,[stringp,b],['string=',a,b]]],[['bit-vector-p',a],[and,['bit-vector-p',b],[=,[length,a],[length,b]],[dotimes,[i,[length,a],t],[when,[/=,[aref,a,i],[aref,b,i]],[return]]]]]]]]]]))
/*
#+(or WAM-CL LISP500)
(defun identity (object) object)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:3786 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,identity,[object],object])
wl:lambda_def(defun, identity, f_identity, [sys_object], [sys_object]).
wl:arglist_info(identity, f_identity, [sys_object], arginfo{all:[sys_object], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_object], opt:0, req:[sys_object], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_identity).

/*

### Compiled Function: `CL:IDENTITY` 
*/
f_identity(Object_In, FnResult) :-
	GEnv=[bv(sys_object, Object_In)],
	catch(( get_var(GEnv, sys_object, Object_Get),
		Object_Get=FnResult
	      ),
	      block_exit(identity, FnResult),
	      true).
:- set_opv(identity, symbol_function, f_identity),
   DefunResult=identity.
/*
:- side_effect(assert_lsp(identity,
			  lambda_def(defun,
				     identity,
				     f_identity,
				     [sys_object],
				     [sys_object]))).
*/
/*
:- side_effect(assert_lsp(identity,
			  arglist_info(identity,
				       f_identity,
				       [sys_object],
				       arginfo{ all:[sys_object],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_object],
						opt:0,
						req:[sys_object],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(identity, init_args(x, f_identity))).
*/
/*
#+(or WAM-CL LISP500)
(defun complement (function)
  #'(lambda (&rest rest) (not (apply function rest))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:3845 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,complement,[function],function([lambda,['&rest',rest],[not,[apply,function,rest]]])])
wl:lambda_def(defun, complement, f_complement, [function], [function([lambda, [c38_rest, rest], [not, [apply, function, rest]]])]).
wl:arglist_info(complement, f_complement, [function], arginfo{all:[function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[function], opt:0, req:[function], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_complement).

/*

### Compiled Function: `CL:COMPLEMENT` 
*/
f_complement(Function_In, FnResult) :-
	CDR=[bv(function, Function_In)],
	catch(( true,
		closure(kw_function, [ClosureEnvironment|CDR], Whole, LResult, [c38_rest, rest],  (get_var(ClosureEnvironment, function, Function_Get), get_var(ClosureEnvironment, rest, Rest_Get), f_apply(Function_Get, Rest_Get, Not_Param), f_not(Not_Param, LResult)), [lambda, [c38_rest, rest], [not, [apply, function, rest]]])=FnResult
	      ),
	      block_exit(complement, FnResult),
	      true).
:- set_opv(complement, symbol_function, f_complement),
   DefunResult=complement.
/*
:- side_effect(assert_lsp(complement,
			  lambda_def(defun,
				     complement,
				     f_complement,
				     [function],
				     
				     [ function(
						[ lambda,
						  [c38_rest, rest],
						  [not, [apply, function, rest]]
						])
				     ]))).
*/
/*
:- side_effect(assert_lsp(complement,
			  arglist_info(complement,
				       f_complement,
				       [function],
				       arginfo{ all:[function],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[function],
						opt:0,
						req:[function],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(complement, init_args(x, f_complement))).
*/
/*
#+(or WAM-CL LISP500)
(defun constantly (value) #'(lambda (&rest rest) value))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:3956 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,constantly,[value],function([lambda,['&rest',rest],value])])
wl:lambda_def(defun, constantly, f_constantly, [sys_value], [function([lambda, [c38_rest, rest], sys_value])]).
wl:arglist_info(constantly, f_constantly, [sys_value], arginfo{all:[sys_value], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_value], opt:0, req:[sys_value], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_constantly).

/*

### Compiled Function: `CL:CONSTANTLY` 
*/
f_constantly(Value_In, FnResult) :-
	CDR=[bv(sys_value, Value_In)],
	catch(( true,
		closure(kw_function, [ClosureEnvironment|CDR], Whole, Value_Get, [c38_rest, rest], get_var(ClosureEnvironment, sys_value, Value_Get), [lambda, [c38_rest, rest], sys_value])=FnResult
	      ),
	      block_exit(constantly, FnResult),
	      true).
:- set_opv(constantly, symbol_function, f_constantly),
   DefunResult=constantly.
/*
:- side_effect(assert_lsp(constantly,
			  lambda_def(defun,
				     constantly,
				     f_constantly,
				     [sys_value],
				     
				     [ function(
						[ lambda,
						  [c38_rest, rest],
						  sys_value
						])
				     ]))).
*/
/*
:- side_effect(assert_lsp(constantly,
			  arglist_info(constantly,
				       f_constantly,
				       [sys_value],
				       arginfo{ all:[sys_value],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_value],
						opt:0,
						req:[sys_value],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(constantly, init_args(x, f_constantly))).
*/
/*
(defmacro=sourceinfo dotimes ((var count-form &optional result-form) &rest forms)
  (let ((start (gensym))
	(count (gensym)))
    `(block nil
      (let ((,var 0)
	    (,count ,count-form))
	(tagbody
	   ,start
	   (when (< ,var ,count)
	     ,@forms
	     (incf ,var)
	     (go ,start)))
	,result-form))))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:4043 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo',dotimes,[[var,'count-form','&optional','result-form'],'&rest',forms],[let,[[start,[gensym]],[count,[gensym]]],['#BQ',[block,[],[let,[[['#COMMA',var],0],[['#COMMA',count],['#COMMA','count-form']]],[tagbody,['#COMMA',start],[when,[<,['#COMMA',var],['#COMMA',count]],['#BQ-COMMA-ELIPSE',forms],[incf,['#COMMA',var]],[go,['#COMMA',start]]]],['#COMMA','result-form']]]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,dotimes,[[sys_var,sys_count_form,c38_optional,sys_result_form],c38_rest,sys_forms],[let,[[sys_start,[gensym]],[count,[gensym]]],['#BQ',[block,[],[let,[[['#COMMA',sys_var],0],[['#COMMA',count],['#COMMA',sys_count_form]]],[tagbody,['#COMMA',sys_start],[when,[<,['#COMMA',sys_var],['#COMMA',count]],['#BQ-COMMA-ELIPSE',sys_forms],[incf,['#COMMA',sys_var]],[go,['#COMMA',sys_start]]]],['#COMMA',sys_result_form]]]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,dotimes],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,dotimes,[[sys_var,sys_count_form,c38_optional,sys_result_form],c38_rest,sys_forms],[let,[[sys_start,[gensym]],[count,[gensym]]],['#BQ',[block,[],[let,[[['#COMMA',sys_var],0],[['#COMMA',count],['#COMMA',sys_count_form]]],[tagbody,['#COMMA',sys_start],[when,[<,['#COMMA',sys_var],['#COMMA',count]],['#BQ-COMMA-ELIPSE',sys_forms],[incf,['#COMMA',sys_var]],[go,['#COMMA',sys_start]]]],['#COMMA',sys_result_form]]]]]]]].
*/
:- f_sys_put_sysprop(dotimes,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       dotimes,
		       
		       [ [sys_var, sys_count_form, c38_optional, sys_result_form],
			 c38_rest,
			 sys_forms
		       ],
		       
		       [ let,
			 [[sys_start, [gensym]], [count, [gensym]]],
			 
			 [ '#BQ',
			   
			   [ block,
			     [],
			     
			     [ let,
			       
			       [ [['#COMMA', sys_var], 0],
				 [['#COMMA', count], ['#COMMA', sys_count_form]]
			       ],
			       
			       [ tagbody,
				 ['#COMMA', sys_start],
				 
				 [ when,
				   [<, ['#COMMA', sys_var], ['#COMMA', count]],
				   ['#BQ-COMMA-ELIPSE', sys_forms],
				   [incf, ['#COMMA', sys_var]],
				   [go, ['#COMMA', sys_start]]
				 ]
			       ],
			       ['#COMMA', sys_result_form]
			     ]
			   ]
			 ]
		       ]
		     ],
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo do (vars (end-test-form &rest result-forms) &rest forms)
  (let ((start (gensym))
	(inits nil)
	(steps nil))
  `(block nil
    (let ,(dolist (var vars (reverse inits))
	    (push (if (consp var)
		      (list (car var) (cadr var))
		      (list var)) inits))
      (tagbody
	 ,start
	 (if ,end-test-form (return (progn ,@result-forms)))
	 ,@forms
	 ,@(dolist (var vars (when steps `((psetq ,@(reverse steps)))))
	     (when (and (consp var) (cddr var))
	       (push (car var) steps)
	       (push (caddr var) steps)))
	 (go ,start))))))
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:4369 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo',do,[vars,['end-test-form','&rest','result-forms'],'&rest',forms],[let,[[start,[gensym]],[inits,[]],[steps,[]]],['#BQ',[block,[],[let,['#COMMA',[dolist,[var,vars,[reverse,inits]],[push,[if,[consp,var],[list,[car,var],[cadr,var]],[list,var]],inits]]],[tagbody,['#COMMA',start],[if,['#COMMA','end-test-form'],[return,[progn,['#BQ-COMMA-ELIPSE','result-forms']]]],['#BQ-COMMA-ELIPSE',forms],['#BQ-COMMA-ELIPSE',[dolist,[var,vars,[when,steps,['#BQ',[[psetq,['#BQ-COMMA-ELIPSE',[reverse,steps]]]]]]],[when,[and,[consp,var],[cddr,var]],[push,[car,var],steps],[push,[caddr,var],steps]]]],[go,['#COMMA',start]]]]]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,do,[sys_vars,[sys_end_test_form,c38_rest,sys_result_forms],c38_rest,sys_forms],[let,[[sys_start,[gensym]],[sys_inits,[]],[sys_steps,[]]],['#BQ',[block,[],[let,['#COMMA',[dolist,[sys_var,sys_vars,[reverse,sys_inits]],[push,[if,[consp,sys_var],[list,[car,sys_var],[cadr,sys_var]],[list,sys_var]],sys_inits]]],[tagbody,['#COMMA',sys_start],[if,['#COMMA',sys_end_test_form],[return,[progn,['#BQ-COMMA-ELIPSE',sys_result_forms]]]],['#BQ-COMMA-ELIPSE',sys_forms],['#BQ-COMMA-ELIPSE',[dolist,[sys_var,sys_vars,[when,sys_steps,['#BQ',[[psetq,['#BQ-COMMA-ELIPSE',[reverse,sys_steps]]]]]]],[when,[and,[consp,sys_var],[cddr,sys_var]],[push,[car,sys_var],sys_steps],[push,[caddr,sys_var],sys_steps]]]],[go,['#COMMA',sys_start]]]]]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,do],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,do,[sys_vars,[sys_end_test_form,c38_rest,sys_result_forms],c38_rest,sys_forms],[let,[[sys_start,[gensym]],[sys_inits,[]],[sys_steps,[]]],['#BQ',[block,[],[let,['#COMMA',[dolist,[sys_var,sys_vars,[reverse,sys_inits]],[push,[if,[consp,sys_var],[list,[car,sys_var],[cadr,sys_var]],[list,sys_var]],sys_inits]]],[tagbody,['#COMMA',sys_start],[if,['#COMMA',sys_end_test_form],[return,[progn,['#BQ-COMMA-ELIPSE',sys_result_forms]]]],['#BQ-COMMA-ELIPSE',sys_forms],['#BQ-COMMA-ELIPSE',[dolist,[sys_var,sys_vars,[when,sys_steps,['#BQ',[[psetq,['#BQ-COMMA-ELIPSE',[reverse,sys_steps]]]]]]],[when,[and,[consp,sys_var],[cddr,sys_var]],[push,[car,sys_var],sys_steps],[push,[caddr,sys_var],sys_steps]]]],[go,['#COMMA',sys_start]]]]]]]]]].
*/
:- f_sys_put_sysprop(do,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       do,
		       
		       [ sys_vars,
			 [sys_end_test_form, c38_rest, sys_result_forms],
			 c38_rest,
			 sys_forms
		       ],
		       
		       [ let,
			 [[sys_start, [gensym]], [sys_inits, []], [sys_steps, []]],
			 
			 [ '#BQ',
			   
			   [ block,
			     [],
			     
			     [ let,
			       
			       [ '#COMMA',
				 
				 [ dolist,
				   [sys_var, sys_vars, [reverse, sys_inits]],
				   
				   [ push,
				     
				     [ if,
				       [consp, sys_var],
				       [list, [car, sys_var], [cadr, sys_var]],
				       [list, sys_var]
				     ],
				     sys_inits
				   ]
				 ]
			       ],
			       
			       [ tagbody,
				 ['#COMMA', sys_start],
				 
				 [ if,
				   ['#COMMA', sys_end_test_form],
				   
				   [ return,
				     
				     [ progn,
				       ['#BQ-COMMA-ELIPSE', sys_result_forms]
				     ]
				   ]
				 ],
				 ['#BQ-COMMA-ELIPSE', sys_forms],
				 
				 [ '#BQ-COMMA-ELIPSE',
				   
				   [ dolist,
				     
				     [ sys_var,
				       sys_vars,
				       
				       [ when,
					 sys_steps,
					 
					 [ '#BQ',
					   
					   [ 
					     [ psetq,
					       
					       [ '#BQ-COMMA-ELIPSE',
						 [reverse, sys_steps]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ],
				     
				     [ when,
				       [and, [consp, sys_var], [cddr, sys_var]],
				       [push, [car, sys_var], sys_steps],
				       [push, [caddr, sys_var], sys_steps]
				     ]
				   ]
				 ],
				 [go, ['#COMMA', sys_start]]
			       ]
			     ]
			   ]
			 ]
		       ]
		     ],
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo do* (vars (end-test-form &rest result-forms) &rest forms)
  (let ((start (gensym))
	(inits nil)
	(steps nil))
  `(block nil
    (let* ,(dolist (var vars (reverse inits))
	     (push (if (consp var)
		       (list (car var) (cadr var))
		       (list var)) inits))
      (tagbody
	 ,start
	 (if ,end-test-form (return (progn ,@result-forms)))
	 ,@forms
	 ,@(dolist (var vars (when steps `((setq ,@(reverse steps)))))
	     (when (and (consp var) (cddr var))
	       (push (car var) steps)
	       (push (caddr var) steps)))
	 (go ,start))))))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:4946 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo','do*',[vars,['end-test-form','&rest','result-forms'],'&rest',forms],[let,[[start,[gensym]],[inits,[]],[steps,[]]],['#BQ',[block,[],['let*',['#COMMA',[dolist,[var,vars,[reverse,inits]],[push,[if,[consp,var],[list,[car,var],[cadr,var]],[list,var]],inits]]],[tagbody,['#COMMA',start],[if,['#COMMA','end-test-form'],[return,[progn,['#BQ-COMMA-ELIPSE','result-forms']]]],['#BQ-COMMA-ELIPSE',forms],['#BQ-COMMA-ELIPSE',[dolist,[var,vars,[when,steps,['#BQ',[[setq,['#BQ-COMMA-ELIPSE',[reverse,steps]]]]]]],[when,[and,[consp,var],[cddr,var]],[push,[car,var],steps],[push,[caddr,var],steps]]]],[go,['#COMMA',start]]]]]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,do_xx,[sys_vars,[sys_end_test_form,c38_rest,sys_result_forms],c38_rest,sys_forms],[let,[[sys_start,[gensym]],[sys_inits,[]],[sys_steps,[]]],['#BQ',[block,[],[let_xx,['#COMMA',[dolist,[sys_var,sys_vars,[reverse,sys_inits]],[push,[if,[consp,sys_var],[list,[car,sys_var],[cadr,sys_var]],[list,sys_var]],sys_inits]]],[tagbody,['#COMMA',sys_start],[if,['#COMMA',sys_end_test_form],[return,[progn,['#BQ-COMMA-ELIPSE',sys_result_forms]]]],['#BQ-COMMA-ELIPSE',sys_forms],['#BQ-COMMA-ELIPSE',[dolist,[sys_var,sys_vars,[when,sys_steps,['#BQ',[[setq,['#BQ-COMMA-ELIPSE',[reverse,sys_steps]]]]]]],[when,[and,[consp,sys_var],[cddr,sys_var]],[push,[car,sys_var],sys_steps],[push,[caddr,sys_var],sys_steps]]]],[go,['#COMMA',sys_start]]]]]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,do_xx],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,do_xx,[sys_vars,[sys_end_test_form,c38_rest,sys_result_forms],c38_rest,sys_forms],[let,[[sys_start,[gensym]],[sys_inits,[]],[sys_steps,[]]],['#BQ',[block,[],[let_xx,['#COMMA',[dolist,[sys_var,sys_vars,[reverse,sys_inits]],[push,[if,[consp,sys_var],[list,[car,sys_var],[cadr,sys_var]],[list,sys_var]],sys_inits]]],[tagbody,['#COMMA',sys_start],[if,['#COMMA',sys_end_test_form],[return,[progn,['#BQ-COMMA-ELIPSE',sys_result_forms]]]],['#BQ-COMMA-ELIPSE',sys_forms],['#BQ-COMMA-ELIPSE',[dolist,[sys_var,sys_vars,[when,sys_steps,['#BQ',[[setq,['#BQ-COMMA-ELIPSE',[reverse,sys_steps]]]]]]],[when,[and,[consp,sys_var],[cddr,sys_var]],[push,[car,sys_var],sys_steps],[push,[caddr,sys_var],sys_steps]]]],[go,['#COMMA',sys_start]]]]]]]]]].
*/
:- f_sys_put_sysprop(do_xx,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       do_xx,
		       
		       [ sys_vars,
			 [sys_end_test_form, c38_rest, sys_result_forms],
			 c38_rest,
			 sys_forms
		       ],
		       
		       [ let,
			 [[sys_start, [gensym]], [sys_inits, []], [sys_steps, []]],
			 
			 [ '#BQ',
			   
			   [ block,
			     [],
			     
			     [ let_xx,
			       
			       [ '#COMMA',
				 
				 [ dolist,
				   [sys_var, sys_vars, [reverse, sys_inits]],
				   
				   [ push,
				     
				     [ if,
				       [consp, sys_var],
				       [list, [car, sys_var], [cadr, sys_var]],
				       [list, sys_var]
				     ],
				     sys_inits
				   ]
				 ]
			       ],
			       
			       [ tagbody,
				 ['#COMMA', sys_start],
				 
				 [ if,
				   ['#COMMA', sys_end_test_form],
				   
				   [ return,
				     
				     [ progn,
				       ['#BQ-COMMA-ELIPSE', sys_result_forms]
				     ]
				   ]
				 ],
				 ['#BQ-COMMA-ELIPSE', sys_forms],
				 
				 [ '#BQ-COMMA-ELIPSE',
				   
				   [ dolist,
				     
				     [ sys_var,
				       sys_vars,
				       
				       [ when,
					 sys_steps,
					 
					 [ '#BQ',
					   
					   [ 
					     [ setq,
					       
					       [ '#BQ-COMMA-ELIPSE',
						 [reverse, sys_steps]
					       ]
					     ]
					   ]
					 ]
				       ]
				     ],
				     
				     [ when,
				       [and, [consp, sys_var], [cddr, sys_var]],
				       [push, [car, sys_var], sys_steps],
				       [push, [caddr, sys_var], sys_steps]
				     ]
				   ]
				 ],
				 [go, ['#COMMA', sys_start]]
			       ]
			     ]
			   ]
			 ]
		       ]
		     ],
		     [],
		     _Ignored).
/*
(defmacro=sourceinfo dolist ((var list-form &optional result-form) &rest forms)
  (let ((start (gensym))
	(list (gensym)))
    `(block nil
      (let ((,list ,list-form)
	    (,var nil))
	(tagbody
	   ,start
	   (unless ,list
	     (setf ,var nil)
	     (return-from nil ,result-form))
	   (setf ,var (car ,list))
	   (setf ,list (cdr ,list))
	   ,@forms
	   (go ,start))))))




*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:5533 **********************/
:-lisp_compile_to_prolog(pkg_sys,['defmacro=sourceinfo',dolist,[[var,'list-form','&optional','result-form'],'&rest',forms],[let,[[start,[gensym]],[list,[gensym]]],['#BQ',[block,[],[let,[[['#COMMA',list],['#COMMA','list-form']],[['#COMMA',var],[]]],[tagbody,['#COMMA',start],[unless,['#COMMA',list],[setf,['#COMMA',var],[]],['return-from',[],['#COMMA','result-form']]],[setf,['#COMMA',var],[car,['#COMMA',list]]],[setf,['#COMMA',list],[cdr,['#COMMA',list]]],['#BQ-COMMA-ELIPSE',forms],[go,['#COMMA',start]]]]]]]])
/*
% macroexpand:-[sys_defmacro_c61_sourceinfo,dolist,[[sys_var,sys_list_form,c38_optional,sys_result_form],c38_rest,sys_forms],[let,[[sys_start,[gensym]],[list,[gensym]]],['#BQ',[block,[],[let,[[['#COMMA',list],['#COMMA',sys_list_form]],[['#COMMA',sys_var],[]]],[tagbody,['#COMMA',sys_start],[unless,['#COMMA',list],[setf,['#COMMA',sys_var],[]],[return_from,[],['#COMMA',sys_result_form]]],[setf,['#COMMA',sys_var],[car,['#COMMA',list]]],[setf,['#COMMA',list],[cdr,['#COMMA',list]]],['#BQ-COMMA-ELIPSE',sys_forms],[go,['#COMMA',sys_start]]]]]]]].
*/
/*
% into:-[sys_put_sysprop,[quote,dolist],[quote,sys_defmacro_c61_sourceinfo],[quote,[defmacro,dolist,[[sys_var,sys_list_form,c38_optional,sys_result_form],c38_rest,sys_forms],[let,[[sys_start,[gensym]],[list,[gensym]]],['#BQ',[block,[],[let,[[['#COMMA',list],['#COMMA',sys_list_form]],[['#COMMA',sys_var],[]]],[tagbody,['#COMMA',sys_start],[unless,['#COMMA',list],[setf,['#COMMA',sys_var],[]],[return_from,[],['#COMMA',sys_result_form]]],[setf,['#COMMA',sys_var],[car,['#COMMA',list]]],[setf,['#COMMA',list],[cdr,['#COMMA',list]]],['#BQ-COMMA-ELIPSE',sys_forms],[go,['#COMMA',sys_start]]]]]]]]]].
*/
:- f_sys_put_sysprop(dolist,
		     sys_defmacro_c61_sourceinfo,
		     
		     [ defmacro,
		       dolist,
		       
		       [ [sys_var, sys_list_form, c38_optional, sys_result_form],
			 c38_rest,
			 sys_forms
		       ],
		       
		       [ let,
			 [[sys_start, [gensym]], [list, [gensym]]],
			 
			 [ '#BQ',
			   
			   [ block,
			     [],
			     
			     [ let,
			       
			       [ [['#COMMA', list], ['#COMMA', sys_list_form]],
				 [['#COMMA', sys_var], []]
			       ],
			       
			       [ tagbody,
				 ['#COMMA', sys_start],
				 
				 [ unless,
				   ['#COMMA', list],
				   [setf, ['#COMMA', sys_var], []],
				   [return_from, [], ['#COMMA', sys_result_form]]
				 ],
				 
				 [ setf,
				   ['#COMMA', sys_var],
				   [car, ['#COMMA', list]]
				 ],
				 [setf, ['#COMMA', list], [cdr, ['#COMMA', list]]],
				 ['#BQ-COMMA-ELIPSE', sys_forms],
				 [go, ['#COMMA', sys_start]]
			       ]
			     ]
			   ]
			 ]
		       ]
		     ],
		     [],
		     _Ignored).
/*
#+(or WAM-CL LISP500)
(defun designator-condition (default-type datum arguments)
  (if (symbolp datum)
      (apply #'make-condition datum arguments)
      (if (or (stringp datum) (functionp datum))
	  (make-condition default-type
			  :format-control datum
			  :format-arguments arguments)
	  datum)))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:5932 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'designator-condition',['default-type',datum,arguments],[if,[symbolp,datum],[apply,function('make-condition'),datum,arguments],[if,[or,[stringp,datum],[functionp,datum]],['make-condition','default-type',':format-control',datum,':format-arguments',arguments],datum]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_designator_condition,
					       kw_function,
					       f_sys_designator_condition)).
*/
wl:lambda_def(defun, sys_designator_condition, f_sys_designator_condition, [sys_default_type, sys_datum, sys_arguments], [[if, [symbolp, sys_datum], [apply, function(make_condition), sys_datum, sys_arguments], [if, [or, [stringp, sys_datum], [functionp, sys_datum]], [make_condition, sys_default_type, kw_format_control, sys_datum, kw_format_arguments, sys_arguments], sys_datum]]]).
wl:arglist_info(sys_designator_condition, f_sys_designator_condition, [sys_default_type, sys_datum, sys_arguments], arginfo{all:[sys_default_type, sys_datum, sys_arguments], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_default_type, sys_datum, sys_arguments], opt:0, req:[sys_default_type, sys_datum, sys_arguments], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_designator_condition).

/*

### Compiled Function: `SYS::DESIGNATOR-CONDITION` 
*/
f_sys_designator_condition(Default_type_In, Datum_In, Arguments_In, FnResult) :-
	GEnv=[bv(sys_default_type, Default_type_In), bv(sys_datum, Datum_In), bv(sys_arguments, Arguments_In)],
	catch(( ( get_var(GEnv, sys_datum, Datum_Get),
		  (   is_symbolp(Datum_Get)
		  ->  get_var(GEnv, sys_arguments, Arguments_Get),
		      get_var(GEnv, sys_datum, Datum_Get11),
		      f_apply(f_make_condition,
			      [Datum_Get11, Arguments_Get],
			      TrueResult24),
		      _7386=TrueResult24
		  ;   (   get_var(GEnv, sys_datum, Datum_Get15),
			  f_stringp(Datum_Get15, FORM1_Res),
			  FORM1_Res\==[],
			  IFTEST13=FORM1_Res
		      ->  true
		      ;   get_var(GEnv, sys_datum, Datum_Get16),
			  f_functionp(Datum_Get16, Functionp_Ret),
			  IFTEST13=Functionp_Ret
		      ),
		      (   IFTEST13\==[]
		      ->  get_var(GEnv, sys_datum, Datum_Get19),
			  ( get_var(GEnv, sys_arguments, Arguments_Get20),
			    get_var(GEnv, sys_default_type, Default_type_Get)
			  ),
			  f_make_condition(Default_type_Get,
					   kw_format_control,
					   Datum_Get19,
					   kw_format_arguments,
					   Arguments_Get20,
					   TrueResult),
			  ElseResult25=TrueResult
		      ;   get_var(GEnv, sys_datum, Datum_Get21),
			  ElseResult25=Datum_Get21
		      ),
		      _7386=ElseResult25
		  )
		),
		_7386=FnResult
	      ),
	      block_exit(sys_designator_condition, FnResult),
	      true).
:- set_opv(sys_designator_condition,
	   symbol_function,
	   f_sys_designator_condition),
   DefunResult=sys_designator_condition.
/*
:- side_effect(assert_lsp(sys_designator_condition,
			  lambda_def(defun,
				     sys_designator_condition,
				     f_sys_designator_condition,
				     
				     [ sys_default_type,
				       sys_datum,
				       sys_arguments
				     ],
				     
				     [ 
				       [ if,
					 [symbolp, sys_datum],
					 
					 [ apply,
					   function(make_condition),
					   sys_datum,
					   sys_arguments
					 ],
					 
					 [ if,
					   
					   [ or,
					     [stringp, sys_datum],
					     [functionp, sys_datum]
					   ],
					   
					   [ make_condition,
					     sys_default_type,
					     kw_format_control,
					     sys_datum,
					     kw_format_arguments,
					     sys_arguments
					   ],
					   sys_datum
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_designator_condition,
			  arglist_info(sys_designator_condition,
				       f_sys_designator_condition,
				       
				       [ sys_default_type,
					 sys_datum,
					 sys_arguments
				       ],
				       arginfo{ all:
						    [ sys_default_type,
						      sys_datum,
						      sys_arguments
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_default_type,
							sys_datum,
							sys_arguments
						      ],
						opt:0,
						req:
						    [ sys_default_type,
						      sys_datum,
						      sys_arguments
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_designator_condition,
			  init_args(x, f_sys_designator_condition))).
*/
/*
#+(or WAM-CL LISP500) 
(defun invoke-debugger (condition)
  (let ((debugger-hook *debugger-hook*)
	(*debugger-hook* nil))
    (when debugger-hook
      (funcall debugger-hook condition debugger-hook))
    (format *debug-io* "Entering debugger."#+(or WAM-CL LISP500) \r\n(defun invoke-debugger (condition)\r\n  (let ((debugger-hook *debugger-hook*)\r\n\t(*debugger-hook* nil))\r\n    (when debugger-hook\r\n      (funcall debugger-hook condition debugger-hook))\r\n    (format *debug-io* \"Entering debugger.~%\")\r\n    (princ condition *debug-io*)\r\n    (terpri *debug-io*)\r\n    (let ((restarts (compute-restarts condition))\r\n\t  (stack (makef))\r\n\t  (frame-depth 0)\r\n\t  (active-frame nil))\r\n      (let ((count 0))\r\n\t(dolist (restart restarts)\r\n\t  (format *debug-io* \"~A: \" count)\r\n\t  (princ restart *debug-io*)\r\n\t  (terpri *debug-io*)\r\n\t  (incf count)))\r\n      (setq active-frame (next-function-frame (- stack 20)))\r\n      (show-frame active-frame 0)\r\n      (tagbody\r\n       start\r\n\t (format *debug-io* \";~A> \" frame-depth)\r\n\t (let ((form (read)))\r\n\t   (case form\r\n\t     (:help (format *debug-io* \"Type :help to get help.~%\")\r\n\t\t    (format *debug-io* \"Type :continue <index> to invoke the indexed restart.~%\"))\r\n\t     (:back (do ((frame (next-function-frame (- stack 20))\r\n\t\t\t\t(next-function-frame frame))\r\n\t\t\t (index 0 (+ 1 index)))\r\n\t\t\t((not frame))\r\n\t\t      (show-frame frame index)))\r\n\t     (:up (if (plusp frame-depth)\r\n\t\t      (progn\r\n\t\t\t(decf frame-depth)\r\n\t\t\t(do ((frame (next-function-frame (- stack 20))\r\n\t\t\t\t    (next-function-frame frame))\r\n\t\t\t     (index 0 (+ 1 index)))\r\n\t\t\t    ((= index frame-depth) (setq active-frame frame)))\r\n\t\t\t(show-frame active-frame frame-depth))\r\n\t\t      (format *debug-io* \"Top of stack.~%\")))\r\n\t     (:down (let ((frame (next-function-frame active-frame)))\r\n\t\t      (if frame\r\n\t\t\t  (progn\r\n\t\t\t    (incf frame-depth)\r\n\t\t\t    (setq active-frame frame)\r\n\t\t\t    (show-frame active-frame frame-depth))\r\n\t\t\t  (format *debug-io* \"Bottom of stack.~%\"))))\r\n\t     (:locals (do ((env (fref (- active-frame 1)) (cdr env)))\r\n\t\t\t  ((not env))\r\n\t\t\t(when (symbolp (caar env))\r\n\t\t\t  (format *debug-io* \"~A~%\" (caar env)))))\r\n\t     (:continue (let ((index (read)))\r\n\t\t\t  (invoke-restart-interactively (nth index restarts))))\r\n\t     (t (let ((values (multiple-value-list\r\n\t\t\t       (eval form (fref (- active-frame 1)))))\r\n\t\t      (count 0))\r\n\t\t  (if values\r\n\t\t      (dolist (value values)\r\n\t\t\t(format *debug-io* \";~A: ~S~%\" count value)\r\n\t\t\t(incf count))\r\n\t\t      (format *debug-io* \";No values.~%\")))))\r\n\t   (go start))))))\r\n\r\n\r\n\r\n\r\n\r\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:6249 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'invoke-debugger',[condition],[let,[['debugger-hook','*debugger-hook*'],['*debugger-hook*',[]]],[when,'debugger-hook',[funcall,'debugger-hook',condition,'debugger-hook']],[format,'*debug-io*','$STRING'("Entering debugger.~%")],[princ,condition,'*debug-io*'],[terpri,'*debug-io*'],[let,[[restarts,['compute-restarts',condition]],[stack,[makef]],['frame-depth',0],['active-frame',[]]],[let,[[count,0]],[dolist,[restart,restarts],[format,'*debug-io*','$STRING'("~A: "),count],[princ,restart,'*debug-io*'],[terpri,'*debug-io*'],[incf,count]]],[setq,'active-frame',['next-function-frame',[-,stack,20]]],['show-frame','active-frame',0],[tagbody,start,[format,'*debug-io*','$STRING'(";~A> "),'frame-depth'],[let,[[form,[read]]],[case,form,[':help',[format,'*debug-io*','$STRING'("Type :help to get help.~%")],[format,'*debug-io*','$STRING'("Type :continue <index> to invoke the indexed restart.~%")]],[':back',[do,[[frame,['next-function-frame',[-,stack,20]],['next-function-frame',frame]],[index,0,[+,1,index]]],[[not,frame]],['show-frame',frame,index]]],[':up',[if,[plusp,'frame-depth'],[progn,[decf,'frame-depth'],[do,[[frame,['next-function-frame',[-,stack,20]],['next-function-frame',frame]],[index,0,[+,1,index]]],[[=,index,'frame-depth'],[setq,'active-frame',frame]]],['show-frame','active-frame','frame-depth']],[format,'*debug-io*','$STRING'("Top of stack.~%")]]],[':down',[let,[[frame,['next-function-frame','active-frame']]],[if,frame,[progn,[incf,'frame-depth'],[setq,'active-frame',frame],['show-frame','active-frame','frame-depth']],[format,'*debug-io*','$STRING'("Bottom of stack.~%")]]]],[':locals',[do,[[env,[fref,[-,'active-frame',1]],[cdr,env]]],[[not,env]],[when,[symbolp,[caar,env]],[format,'*debug-io*','$STRING'("~A~%"),[caar,env]]]]],[':continue',[let,[[index,[read]]],['invoke-restart-interactively',[nth,index,restarts]]]],[t,[let,[[values,['multiple-value-list',[eval,form,[fref,[-,'active-frame',1]]]]],[count,0]],[if,values,[dolist,[value,values],[format,'*debug-io*','$STRING'(";~A: ~S~%"),count,value],[incf,count]],[format,'*debug-io*','$STRING'(";No values.~%")]]]]],[go,start]]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_makef,
					       kw_function,
					       f_sys_makef)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_next_function_frame,
					       kw_function,
					       f_sys_next_function_frame)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_show_frame,
					       kw_function,
					       f_sys_show_frame)).
*/
/*
% case:-[[kw_help,[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Type :help to get help.~%")],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Type :continue <index> to invoke the indexed restart.~%")]],[kw_back,[do,[[sys_frame,[sys_next_function_frame,[-,sys_stack,20]],[sys_next_function_frame,sys_frame]],[sys_index,0,[+,1,sys_index]]],[[not,sys_frame]],[sys_show_frame,sys_frame,sys_index]]],[kw_up,[if,[plusp,sys_frame_depth],[progn,[decf,sys_frame_depth],[do,[[sys_frame,[sys_next_function_frame,[-,sys_stack,20]],[sys_next_function_frame,sys_frame]],[sys_index,0,[+,1,sys_index]]],[[=,sys_index,sys_frame_depth],[setq,sys_active_frame,sys_frame]]],[sys_show_frame,sys_active_frame,sys_frame_depth]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Top of stack.~%")]]],[kw_down,[let,[[sys_frame,[sys_next_function_frame,sys_active_frame]]],[if,sys_frame,[progn,[incf,sys_frame_depth],[setq,sys_active_frame,sys_frame],[sys_show_frame,sys_active_frame,sys_frame_depth]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Bottom of stack.~%")]]]],[kw_locals,[do,[[sys_env,[sys_fref,[-,sys_active_frame,1]],[cdr,sys_env]]],[[not,sys_env]],[when,[symbolp,[caar,sys_env]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"~A~%"),[caar,sys_env]]]]],[kw_continue,[let,[[sys_index,[read]]],[invoke_restart_interactively,[nth,sys_index,sys_restarts]]]],[t,[let,[[values,[multiple_value_list,[eval,sys_form,[sys_fref,[-,sys_active_frame,1]]]]],[count,0]],[if,values,[dolist,[sys_value,values],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,";~A: ~S~%"),count,sys_value],[incf,count]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,";No values.~%")]]]]].
*/
/*
% conds:-[[[eq,_135670,[quote,kw_help]],[progn,[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Type :help to get help.~%")],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Type :continue <index> to invoke the indexed restart.~%")]]],[[eq,_135670,[quote,kw_back]],[progn,[do,[[sys_frame,[sys_next_function_frame,[-,sys_stack,20]],[sys_next_function_frame,sys_frame]],[sys_index,0,[+,1,sys_index]]],[[not,sys_frame]],[sys_show_frame,sys_frame,sys_index]]]],[[eq,_135670,[quote,kw_up]],[progn,[if,[plusp,sys_frame_depth],[progn,[decf,sys_frame_depth],[do,[[sys_frame,[sys_next_function_frame,[-,sys_stack,20]],[sys_next_function_frame,sys_frame]],[sys_index,0,[+,1,sys_index]]],[[=,sys_index,sys_frame_depth],[setq,sys_active_frame,sys_frame]]],[sys_show_frame,sys_active_frame,sys_frame_depth]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Top of stack.~%")]]]],[[eq,_135670,[quote,kw_down]],[progn,[let,[[sys_frame,[sys_next_function_frame,sys_active_frame]]],[if,sys_frame,[progn,[incf,sys_frame_depth],[setq,sys_active_frame,sys_frame],[sys_show_frame,sys_active_frame,sys_frame_depth]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Bottom of stack.~%")]]]]],[[eq,_135670,[quote,kw_locals]],[progn,[do,[[sys_env,[sys_fref,[-,sys_active_frame,1]],[cdr,sys_env]]],[[not,sys_env]],[when,[symbolp,[caar,sys_env]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"~A~%"),[caar,sys_env]]]]]],[[eq,_135670,[quote,kw_continue]],[progn,[let,[[sys_index,[read]]],[invoke_restart_interactively,[nth,sys_index,sys_restarts]]]]],[t,[progn,[let,[[values,[multiple_value_list,[eval,sys_form,[sys_fref,[-,sys_active_frame,1]]]]],[count,0]],[if,values,[dolist,[sys_value,values],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,";~A: ~S~%"),count,sys_value],[incf,count]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,";No values.~%")]]]]]].
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_next_function_frame,kw_function,f_sys_next_function_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_show_frame,kw_function,f_sys_show_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_next_function_frame,kw_function,f_sys_next_function_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_show_frame,kw_function,f_sys_show_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_next_function_frame,kw_function,f_sys_next_function_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_next_function_frame,kw_function,f_sys_next_function_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_next_function_frame,kw_function,f_sys_next_function_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_next_function_frame,kw_function,f_sys_next_function_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_show_frame,kw_function,f_sys_show_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_next_function_frame,kw_function,f_sys_next_function_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_show_frame,kw_function,f_sys_show_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_fref,kw_function,f_sys_fref)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_fref,kw_function,f_sys_fref)).
*/
/*
% case:-[[kw_help,[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Type :help to get help.~%")],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Type :continue <index> to invoke the indexed restart.~%")]],[kw_back,[do,[[sys_frame,[sys_next_function_frame,[-,sys_stack,20]],[sys_next_function_frame,sys_frame]],[sys_index,0,[+,1,sys_index]]],[[not,sys_frame]],[sys_show_frame,sys_frame,sys_index]]],[kw_up,[if,[plusp,sys_frame_depth],[progn,[decf,sys_frame_depth],[do,[[sys_frame,[sys_next_function_frame,[-,sys_stack,20]],[sys_next_function_frame,sys_frame]],[sys_index,0,[+,1,sys_index]]],[[=,sys_index,sys_frame_depth],[setq,sys_active_frame,sys_frame]]],[sys_show_frame,sys_active_frame,sys_frame_depth]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Top of stack.~%")]]],[kw_down,[let,[[sys_frame,[sys_next_function_frame,sys_active_frame]]],[if,sys_frame,[progn,[incf,sys_frame_depth],[setq,sys_active_frame,sys_frame],[sys_show_frame,sys_active_frame,sys_frame_depth]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Bottom of stack.~%")]]]],[kw_locals,[do,[[sys_env,[sys_fref,[-,sys_active_frame,1]],[cdr,sys_env]]],[[not,sys_env]],[when,[symbolp,[caar,sys_env]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"~A~%"),[caar,sys_env]]]]],[kw_continue,[let,[[sys_index,[read]]],[invoke_restart_interactively,[nth,sys_index,sys_restarts]]]],[t,[let,[[values,[multiple_value_list,[eval,sys_form,[sys_fref,[-,sys_active_frame,1]]]]],[count,0]],[if,values,[dolist,[sys_value,values],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,";~A: ~S~%"),count,sys_value],[incf,count]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,";No values.~%")]]]]].
*/
/*
% conds:-[[[eq,_107400,[quote,kw_help]],[progn,[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Type :help to get help.~%")],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Type :continue <index> to invoke the indexed restart.~%")]]],[[eq,_107400,[quote,kw_back]],[progn,[do,[[sys_frame,[sys_next_function_frame,[-,sys_stack,20]],[sys_next_function_frame,sys_frame]],[sys_index,0,[+,1,sys_index]]],[[not,sys_frame]],[sys_show_frame,sys_frame,sys_index]]]],[[eq,_107400,[quote,kw_up]],[progn,[if,[plusp,sys_frame_depth],[progn,[decf,sys_frame_depth],[do,[[sys_frame,[sys_next_function_frame,[-,sys_stack,20]],[sys_next_function_frame,sys_frame]],[sys_index,0,[+,1,sys_index]]],[[=,sys_index,sys_frame_depth],[setq,sys_active_frame,sys_frame]]],[sys_show_frame,sys_active_frame,sys_frame_depth]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Top of stack.~%")]]]],[[eq,_107400,[quote,kw_down]],[progn,[let,[[sys_frame,[sys_next_function_frame,sys_active_frame]]],[if,sys_frame,[progn,[incf,sys_frame_depth],[setq,sys_active_frame,sys_frame],[sys_show_frame,sys_active_frame,sys_frame_depth]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Bottom of stack.~%")]]]]],[[eq,_107400,[quote,kw_locals]],[progn,[do,[[sys_env,[sys_fref,[-,sys_active_frame,1]],[cdr,sys_env]]],[[not,sys_env]],[when,[symbolp,[caar,sys_env]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"~A~%"),[caar,sys_env]]]]]],[[eq,_107400,[quote,kw_continue]],[progn,[let,[[sys_index,[read]]],[invoke_restart_interactively,[nth,sys_index,sys_restarts]]]]],[t,[progn,[let,[[values,[multiple_value_list,[eval,sys_form,[sys_fref,[-,sys_active_frame,1]]]]],[count,0]],[if,values,[dolist,[sys_value,values],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,";~A: ~S~%"),count,sys_value],[incf,count]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,";No values.~%")]]]]]].
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_next_function_frame,kw_function,f_sys_next_function_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_show_frame,kw_function,f_sys_show_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_next_function_frame,kw_function,f_sys_next_function_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_show_frame,kw_function,f_sys_show_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_next_function_frame,kw_function,f_sys_next_function_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_next_function_frame,kw_function,f_sys_next_function_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_next_function_frame,kw_function,f_sys_next_function_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_next_function_frame,kw_function,f_sys_next_function_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_show_frame,kw_function,f_sys_show_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_next_function_frame,kw_function,f_sys_next_function_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_show_frame,kw_function,f_sys_show_frame)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_fref,kw_function,f_sys_fref)).
*/
/*
:-side_effect(generate_function_or_macro_name([fbound(sys_expand,kw_function)=function(f_sys_expand11),name='GLOBAL',environ=env_1],sys_fref,kw_function,f_sys_fref)).
*/
wl:lambda_def(defun,invoke_debugger,f_invoke_debugger,[condition],[[let,[[sys_debugger_hook,xx_debugger_hook_xx],[xx_debugger_hook_xx,[]]],[when,sys_debugger_hook,[funcall,sys_debugger_hook,condition,sys_debugger_hook]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Entering debugger.~%")],[princ,condition,xx_debug_io_xx],[terpri,xx_debug_io_xx],[let,[[sys_restarts,[compute_restarts,condition]],[sys_stack,[sys_makef]],[sys_frame_depth,0],[sys_active_frame,[]]],[let,[[count,0]],[dolist,[restart,sys_restarts],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"~A: "),count],[princ,restart,xx_debug_io_xx],[terpri,xx_debug_io_xx],[incf,count]]],[setq,sys_active_frame,[sys_next_function_frame,[-,sys_stack,20]]],[sys_show_frame,sys_active_frame,0],[tagbody,sys_start,[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,";~A> "),sys_frame_depth],[let,[[sys_form,[read]]],[case,sys_form,[kw_help,[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Type :help to get help.~%")],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Type :continue <index> to invoke the indexed restart.~%")]],[kw_back,[do,[[sys_frame,[sys_next_function_frame,[-,sys_stack,20]],[sys_next_function_frame,sys_frame]],[sys_index,0,[+,1,sys_index]]],[[not,sys_frame]],[sys_show_frame,sys_frame,sys_index]]],[kw_up,[if,[plusp,sys_frame_depth],[progn,[decf,sys_frame_depth],[do,[[sys_frame,[sys_next_function_frame,[-,sys_stack,20]],[sys_next_function_frame,sys_frame]],[sys_index,0,[+,1,sys_index]]],[[=,sys_index,sys_frame_depth],[setq,sys_active_frame,sys_frame]]],[sys_show_frame,sys_active_frame,sys_frame_depth]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Top of stack.~%")]]],[kw_down,[let,[[sys_frame,[sys_next_function_frame,sys_active_frame]]],[if,sys_frame,[progn,[incf,sys_frame_depth],[setq,sys_active_frame,sys_frame],[sys_show_frame,sys_active_frame,sys_frame_depth]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Bottom of stack.~%")]]]],[kw_locals,[do,[[sys_env,[sys_fref,[-,sys_active_frame,1]],[cdr,sys_env]]],[[not,sys_env]],[when,[symbolp,[caar,sys_env]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"~A~%"),[caar,sys_env]]]]],[kw_continue,[let,[[sys_index,[read]]],[invoke_restart_interactively,[nth,sys_index,sys_restarts]]]],[t,[let,[[values,[multiple_value_list,[eval,sys_form,[sys_fref,[-,sys_active_frame,1]]]]],[count,0]],[if,values,[dolist,[sys_value,values],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,";~A: ~S~%"),count,sys_value],[incf,count]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,";No values.~%")]]]]],[go,sys_start]]]]]]).
wl:arglist_info(invoke_debugger,f_invoke_debugger,[condition],arginfo{all:[condition],allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[condition],opt:0,req:[condition],rest:0,sublists:0,whole:0}).
wl:init_args(x,f_invoke_debugger).

/*

### Compiled Function: `CL:INVOKE-DEBUGGER` 
*/
f_invoke_debugger(_16770,_29940):-_16730=[bv(condition,_16770)],catch(((get_var(_16730,xx_debugger_hook_xx,_16860),_16788=[bv(sys_debugger_hook,_16860)|_16730],save_special(sv(xx_debugger_hook_xx,[],symbol_value,_16872)),get_var(_16788,sys_debugger_hook,_16904),(_16904\==[]->get_var(_16788,condition,_16960),get_var(_16788,sys_debugger_hook,_16948),f_apply(_16948,[_16960,_16948],_16930),_16874=_16930;_16874=[]),get_var(_16788,xx_debug_io_xx,_17018),f_format([_17018,'$ARRAY'([*],claz_base_character,"Entering debugger.~%")],_17016),get_var(_16788,condition,_17052),get_var(_16788,xx_debug_io_xx,_17080),f_princ(_17052,_17080,_17034),get_var(_16788,xx_debug_io_xx,_17110),f_terpri(_17110,_17092),get_var(_16788,condition,_17224),f_compute_restarts(_17224,_17206),f_sys_makef(_17236),_17194=[bv(sys_restarts,_17206),bv(sys_stack,_17236),bv(sys_frame_depth,0),bv(sys_active_frame,[])|_16788],_17334=[bv(count,0)|_17194],get_var(_17334,sys_restarts,_17538),_17484=bv(restart,_17510),_17360=[_17484|_17334],forall(member(_17510,_17538),(nb_setarg(2,_17484,_17510),get_var(_17360,count,_17392),get_var(_17360,xx_debug_io_xx,_17380),f_format([_17380,'$ARRAY'([*],claz_base_character,"~A: "),_17392],_54062),get_var(_17360,restart,_17410),get_var(_17360,xx_debug_io_xx,_17442),f_princ(_17410,_17442,_54088),get_var(_17360,xx_debug_io_xx,_17472),f_terpri(_17472,_54114),place_op(_17360,incf,count,symbol_value,[],_17308))),get_var(_17194,sys_stack,_17568),'f_-'(_17568,20,_17566),f_sys_next_function_frame(_17566,_17550),set_var(_17194,sys_active_frame,_17550),get_var(_17194,sys_active_frame,_17586),f_sys_show_frame(_17586,0,_17584),call_addr_block(_17194,(push_label(sys_start),get_var(_17194,sys_frame_depth,_22696),get_var(_17194,xx_debug_io_xx,_22668),f_format([_22668,'$ARRAY'([*],claz_base_character,";~A> "),_22696],_54152),f_read(_22808),_22780=[bv(sys_form,_22808)|_17194],get_var(_22780,sys_form,_22838),(is_eq(_22838,kw_help)->get_var(_22780,xx_debug_io_xx,_22940),f_format([_22940,'$ARRAY'([*],claz_base_character,"Type :help to get help.~%")],_54178),get_var(_22780,xx_debug_io_xx,_22968),f_format([_22968,'$ARRAY'([*],claz_base_character,"Type :continue <index> to invoke the indexed restart.~%")],_28198),_22820=_28198;(is_eq(_22838,kw_back)->get_var(_22780,sys_stack,_23128),'f_-'(_23128,20,_53794),f_sys_next_function_frame(_53794,_23154),_23096=[bv(sys_frame,_23154),bv(sys_index,0)|_22780],catch((call_addr_block(_23096,(push_label(do_label_31),get_var(_23096,sys_frame,_23618),(_23618==[]->throw(block_exit([],[])),_23182=_23702;get_var(_23096,sys_frame,_23748),get_var(_23096,sys_index,_23776),f_sys_show_frame(_23748,_23776,_54204),get_var(_23096,sys_frame,_23808),f_sys_next_function_frame(_23808,_23790),get_var(_23096,sys_index,_23836),'f_+'(1,_23836,_23788),set_var(_23096,sys_frame,_23790),set_var(_23096,sys_index,_23788),goto(do_label_31,_23096),_23182=_23864)),[addr(addr_tagbody_32_do_label_31,do_label_31,'$unused',_23918,(get_var(_23918,sys_frame,_23932),(_23932==[]->throw(block_exit([],[])),_23946=_23960;get_var(_23918,sys_frame,_23974),get_var(_23918,sys_index,_23988),f_sys_show_frame(_23974,_23988,_54230),get_var(_23918,sys_frame,_24004),f_sys_next_function_frame(_24004,_24016),get_var(_23918,sys_index,_24020),'f_+'(1,_24020,_24032),set_var(_23918,sys_frame,_24016),set_var(_23918,sys_index,_24032),goto(do_label_31,_23918),_23946=_24036)))]),[]=_23070),block_exit([],_23070),true),_28224=_23070;(is_eq(_22838,kw_up)->get_var(_22780,sys_frame_depth,_24138),(mth:is_plusp(_24138)->set_place(_22780,decf,[value,sys_frame_depth],[],_24196),get_var(_22780,sys_stack,_24326),'f_-'(_24326,20,_53820),f_sys_next_function_frame(_53820,_24352),_24294=[bv(sys_frame,_24352),bv(sys_index,0)|_22780],catch((call_addr_block(_24294,(push_label(do_label_32),get_var(_24294,sys_frame_depth,_24870),get_var(_24294,sys_index,_24842),(_24842=:=_24870->get_var(_24294,sys_frame,_24942),set_var(_24294,sys_active_frame,_24942),throw(block_exit([],_24942)),_24380=_24968;get_var(_24294,sys_frame,_25056),f_sys_next_function_frame(_25056,_53692),get_var(_24294,sys_index,_25084),'f_+'(1,_25084,_53718),set_var(_24294,sys_frame,_53692),set_var(_24294,sys_index,_53718),goto(do_label_32,_24294),_24380=_25112)),[addr(addr_tagbody_33_do_label_32,do_label_32,'$unused',_25166,(get_var(_25166,sys_frame_depth,_25180),get_var(_25166,sys_index,_25194),(_25194=:=_25180->get_var(_25166,sys_frame,_25208),set_var(_25166,sys_active_frame,_25208),throw(block_exit([],_25208)),_25222=_25236;get_var(_25166,sys_frame,_25250),f_sys_next_function_frame(_25250,_54280),get_var(_25166,sys_index,_25266),'f_+'(1,_25266,_54306),set_var(_25166,sys_frame,_54280),set_var(_25166,sys_index,_54306),goto(do_label_32,_25166),_25222=_25282)))]),[]=_24268),block_exit([],_24268),true),get_var(_22780,sys_active_frame,_25310),get_var(_22780,sys_frame_depth,_25338),f_sys_show_frame(_25310,_25338,_25394),_28106=_25394;get_var(_22780,xx_debug_io_xx,_25368),f_format([_25368,'$ARRAY'([*],claz_base_character,"Top of stack.~%")],_25420),_28106=_25420),_28172=_28106;(is_eq(_22838,kw_down)->get_var(_22780,sys_active_frame,_25578),f_sys_next_function_frame(_25578,_25604),_25548=[bv(sys_frame,_25604)|_22780],get_var(_25548,sys_frame,_25632),(_25632\==[]->place_op(_25548,incf,sys_frame_depth,symbol_value,[],_25690),get_var(_25548,sys_frame,_25722),set_var(_25548,sys_active_frame,_25722),get_var(_25548,sys_active_frame,_25750),get_var(_25548,sys_frame_depth,_25778),f_sys_show_frame(_25750,_25778,_25834),_25522=_25834;get_var(_25548,xx_debug_io_xx,_25808),f_format([_25808,'$ARRAY'([*],claz_base_character,"Bottom of stack.~%")],_25860),_25522=_25860),_28132=_25522;(is_eq(_22838,kw_locals)->get_var(_22780,sys_active_frame,_26020),'f_-'(_26020,1,_26002),f_sys_fref(_26002,_26046),_25988=[bv(sys_env,_26046)|_22780],catch((call_addr_block(_25988,(push_label(do_label_33),get_var(_25988,sys_env,_26620),(_26620==[]->throw(block_exit([],[])),_26074=_26704;get_var(_25988,sys_env,_26780),f_caar(_26780,_26820),(is_symbolp(_26820)->get_var(_25988,sys_env,_26880),get_var(_25988,xx_debug_io_xx,_26850),f_caar(_26880,_26862),f_format([_26850,'$ARRAY'([*],claz_base_character,"~A~%"),_26862],_26906),_26732=_26906;_26732=[]),get_var(_25988,sys_env,_26948),f_cdr(_26948,_53744),set_var(_25988,sys_env,_53744),goto(do_label_33,_25988),_26074=_26976)),[addr(addr_tagbody_34_do_label_33,do_label_33,'$unused',_27030,(get_var(_27030,sys_env,_27044),(_27044==[]->throw(block_exit([],[])),_27058=_27072;get_var(_27030,sys_env,_27086),f_caar(_27086,_27100),(is_symbolp(_27100)->get_var(_27030,sys_env,_27114),get_var(_27030,xx_debug_io_xx,_27128),f_caar(_27114,_54356),f_format([_27128,'$ARRAY'([*],claz_base_character,"~A~%"),_54356],_27144),_27156=_27144;_27156=[]),get_var(_27030,sys_env,_27160),f_cdr(_27160,_27172),set_var(_27030,sys_env,_27172),goto(do_label_33,_27030),_27058=_27176)))]),[]=_25962),block_exit([],_25962),true),_28080=_25962;(is_eq(_22838,kw_continue)->f_read(_27332),_27304=[bv(sys_index,_27332)|_22780],get_var(_27304,sys_index,_27362),get_var(_27304,sys_restarts,_27390),f_nth(_27362,_27390,_27344),f_invoke_restart_interactively(_27344,_27278),_28040=_27278;get_var(_22780,sys_active_frame,_27590),get_var(_22780,sys_form,_27558),'f_-'(_27590,1,_53870),f_sys_fref(_53870,_27570),f_eval(_27558,_27570,_27530),nb_current('$mv_return',_27502),_27474=[bv(values,_27502),bv(count,0)|_22780],get_var(_27474,values,_27632),(_27632\==[]->get_var(_27474,values,_27706),_27822=bv(sys_value,_27876),_27848=[_27822|_27474],forall(member(_27876,_27706),(nb_setarg(2,_27822,_27876),get_var(_27848,count,_27766),(get_var(_27848,sys_value,_27794),get_var(_27848,xx_debug_io_xx,_27738)),f_format([_27738,'$ARRAY'([*],claz_base_character,";~A: ~S~%"),_27766,_27794],_54406),place_op(_27848,incf,count,symbol_value,[],_27946))),_27448=_27946;get_var(_27474,xx_debug_io_xx,_27920),f_format([_27920,'$ARRAY'([*],claz_base_character,";No values.~%")],_27972),_27448=_27972),_28040=_27448),_28080=_28040),_28132=_28080),_28172=_28132),_28224=_28172),_22820=_28224),goto(sys_start,_22780)),[addr(addr_tagbody_28_sys_start,sys_start,'$unused',_28280,(get_var(_28280,sys_frame_depth,_28282),get_var(_28280,xx_debug_io_xx,_28286),f_format([_28286,'$ARRAY'([*],claz_base_character,";~A> "),_28282],_54444),f_read(_28300),_28304=[bv(sys_form,_28300)|_28280],get_var(_28304,sys_form,_28318),(is_eq(_28318,kw_help)->get_var(_28304,xx_debug_io_xx,_28332),f_format([_28332,'$ARRAY'([*],claz_base_character,"Type :help to get help.~%")],_54482),get_var(_28304,xx_debug_io_xx,_28348),f_format([_28348,'$ARRAY'([*],claz_base_character,"Type :continue <index> to invoke the indexed restart.~%")],_28362),_28374=_28362;(is_eq(_28318,kw_back)->get_var(_28304,sys_stack,_28378),'f_-'(_28378,20,_53896),f_sys_next_function_frame(_53896,_54508),_28396=[bv(sys_frame,_54508),bv(sys_index,0)|_28304],catch((call_addr_block(_28396,(push_label(do_label_28),get_var(_28396,sys_frame,_28410),(_28410==[]->throw(block_exit([],[])),_28424=_28438;get_var(_28396,sys_frame,_28452),get_var(_28396,sys_index,_28466),f_sys_show_frame(_28452,_28466,_54534),get_var(_28396,sys_frame,_28482),f_sys_next_function_frame(_28482,_54560),get_var(_28396,sys_index,_28498),'f_+'(1,_28498,_54586),set_var(_28396,sys_frame,_54560),set_var(_28396,sys_index,_54586),goto(do_label_28,_28396),_28424=_28514)),[addr(addr_tagbody_29_do_label_28,do_label_28,'$unused',_28528,(get_var(_28528,sys_frame,_28542),(_28542==[]->throw(block_exit([],[])),_28556=_28570;get_var(_28528,sys_frame,_28584),get_var(_28528,sys_index,_54612),f_sys_show_frame(_28584,_54612,_54638),get_var(_28528,sys_frame,_28602),f_sys_next_function_frame(_28602,_54664),get_var(_28528,sys_index,_28618),'f_+'(1,_28618,_54690),set_var(_28528,sys_frame,_54664),set_var(_28528,sys_index,_54690),goto(do_label_28,_28528),_28556=_28634)))]),[]=_28648),block_exit([],_28648),true),_28662=_28648;(is_eq(_28318,kw_up)->get_var(_28304,sys_frame_depth,_28676),(mth:is_plusp(_28676)->set_place(_28304,decf,[value,sys_frame_depth],[],_28688),get_var(_28304,sys_stack,_28692),'f_-'(_28692,20,_53922),f_sys_next_function_frame(_53922,_28708),_28722=[bv(sys_frame,_28708),bv(sys_index,0)|_28304],catch((call_addr_block(_28722,(push_label(do_label_29),get_var(_28722,sys_frame_depth,_28736),get_var(_28722,sys_index,_28750),(_28750=:=_28736->get_var(_28722,sys_frame,_28764),set_var(_28722,sys_active_frame,_28764),throw(block_exit([],_28764)),_28778=_28792;get_var(_28722,sys_frame,_28806),f_sys_next_function_frame(_28806,_54728),get_var(_28722,sys_index,_28822),'f_+'(1,_28822,_54754),set_var(_28722,sys_frame,_54728),set_var(_28722,sys_index,_54754),goto(do_label_29,_28722),_28778=_28838)),[addr(addr_tagbody_30_do_label_29,do_label_29,'$unused',_28852,(get_var(_28852,sys_frame_depth,_28866),get_var(_28852,sys_index,_28880),(_28880=:=_28866->get_var(_28852,sys_frame,_28894),set_var(_28852,sys_active_frame,_28894),throw(block_exit([],_28894)),_28908=_28922;get_var(_28852,sys_frame,_28936),f_sys_next_function_frame(_28936,_54780),get_var(_28852,sys_index,_28952),'f_+'(1,_28952,_54806),set_var(_28852,sys_frame,_54780),set_var(_28852,sys_index,_54806),goto(do_label_29,_28852),_28908=_28968)))]),[]=_28982),block_exit([],_28982),true),get_var(_28304,sys_active_frame,_28996),get_var(_28304,sys_frame_depth,_29010),f_sys_show_frame(_28996,_29010,_29024),_29038=_29024;get_var(_28304,xx_debug_io_xx,_29052),f_format([_29052,'$ARRAY'([*],claz_base_character,"Top of stack.~%")],_29066),_29038=_29066),_29080=_29038;(is_eq(_28318,kw_down)->get_var(_28304,sys_active_frame,_29094),f_sys_next_function_frame(_29094,_29108),_29122=[bv(sys_frame,_29108)|_28304],get_var(_29122,sys_frame,_29136),(_29136\==[]->place_op(_29122,incf,sys_frame_depth,symbol_value,[],_54832),get_var(_29122,sys_frame,_29152),set_var(_29122,sys_active_frame,_29152),get_var(_29122,sys_active_frame,_29166),get_var(_29122,sys_frame_depth,_29180),f_sys_show_frame(_29166,_29180,_29194),_29208=_29194;get_var(_29122,xx_debug_io_xx,_29222),f_format([_29222,'$ARRAY'([*],claz_base_character,"Bottom of stack.~%")],_29236),_29208=_29236),_29250=_29208;(is_eq(_28318,kw_locals)->get_var(_28304,sys_active_frame,_29264),'f_-'(_29264,1,_53948),f_sys_fref(_53948,_54858),_29282=[bv(sys_env,_54858)|_28304],catch((call_addr_block(_29282,(push_label(do_label_30),get_var(_29282,sys_env,_29296),(_29296==[]->throw(block_exit([],[])),_29310=_29324;get_var(_29282,sys_env,_29338),f_caar(_29338,_29352),(is_symbolp(_29352)->get_var(_29282,sys_env,_29366),get_var(_29282,xx_debug_io_xx,_29380),f_caar(_29366,_54884),f_format([_29380,'$ARRAY'([*],claz_base_character,"~A~%"),_54884],_29396),_29408=_29396;_29408=[]),get_var(_29282,sys_env,_29412),f_cdr(_29412,_54910),set_var(_29282,sys_env,_54910),goto(do_label_30,_29282),_29310=_29428)),[addr(addr_tagbody_31_do_label_30,do_label_30,'$unused',_29442,(get_var(_29442,sys_env,_29456),(_29456==[]->throw(block_exit([],[])),_29470=_29484;get_var(_29442,sys_env,_29498),f_caar(_29498,_29512),(is_symbolp(_29512)->get_var(_29442,sys_env,_29526),get_var(_29442,xx_debug_io_xx,_29540),f_caar(_29526,_54936),f_format([_29540,'$ARRAY'([*],claz_base_character,"~A~%"),_54936],_29556),_29568=_29556;_29568=[]),get_var(_29442,sys_env,_29572),f_cdr(_29572,_54962),set_var(_29442,sys_env,_54962),goto(do_label_30,_29442),_29470=_29588)))]),[]=_29602),block_exit([],_29602),true),_29616=_29602;(is_eq(_28318,kw_continue)->f_read(_54988),_29632=[bv(sys_index,_54988)|_28304],get_var(_29632,sys_index,_29646),get_var(_29632,sys_restarts,_29660),f_nth(_29646,_29660,_53974),f_invoke_restart_interactively(_53974,_29676),_29690=_29676;get_var(_28304,sys_active_frame,_29704),get_var(_28304,sys_form,_29718),'f_-'(_29704,1,_54000),f_sys_fref(_54000,_55014),f_eval(_29718,_55014,_29734),nb_current('$mv_return',_29738),_29752=[bv(values,_29738),bv(count,0)|_28304],get_var(_29752,values,_29766),(_29766\==[]->get_var(_29752,values,_29780),_29794=bv(sys_value,_29808),_29822=[_29794|_29752],forall(member(_29808,_29780),(nb_setarg(2,_29794,_29808),get_var(_29822,count,_29836),(get_var(_29822,sys_value,_55052),get_var(_29822,xx_debug_io_xx,_29852)),f_format([_29852,'$ARRAY'([*],claz_base_character,";~A: ~S~%"),_29836,_55052],_55078),place_op(_29822,incf,count,symbol_value,[],_29868))),_29882=_29868;get_var(_29752,xx_debug_io_xx,_29896),f_format([_29896,'$ARRAY'([*],claz_base_character,";No values.~%")],_29910),_29882=_29910),_29690=_29882),_29616=_29690),_29250=_29616),_29080=_29250),_28662=_29080),_28374=_28662),goto(sys_start,_28304)))]),restore_special(sv(xx_debugger_hook_xx,[],symbol_value,_16872))),[]=_29940),block_exit(invoke_debugger,_29940),true).
:-set_opv(invoke_debugger,symbol_function,f_invoke_debugger),_10982=invoke_debugger.
/*
:-side_effect(assert_lsp(invoke_debugger,lambda_def(defun,invoke_debugger,f_invoke_debugger,[condition],[[let,[[sys_debugger_hook,xx_debugger_hook_xx],[xx_debugger_hook_xx,[]]],[when,sys_debugger_hook,[funcall,sys_debugger_hook,condition,sys_debugger_hook]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Entering debugger.~%")],[princ,condition,xx_debug_io_xx],[terpri,xx_debug_io_xx],[let,[[sys_restarts,[compute_restarts,condition]],[sys_stack,[sys_makef]],[sys_frame_depth,0],[sys_active_frame,[]]],[let,[[count,0]],[dolist,[restart,sys_restarts],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"~A: "),count],[princ,restart,xx_debug_io_xx],[terpri,xx_debug_io_xx],[incf,count]]],[setq,sys_active_frame,[sys_next_function_frame,[-,sys_stack,20]]],[sys_show_frame,sys_active_frame,0],[tagbody,sys_start,[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,";~A> "),sys_frame_depth],[let,[[sys_form,[read]]],[case,sys_form,[kw_help,[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Type :help to get help.~%")],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Type :continue <index> to invoke the indexed restart.~%")]],[kw_back,[do,[[sys_frame,[sys_next_function_frame,[-,sys_stack,20]],[sys_next_function_frame,sys_frame]],[sys_index,0,[+,1,sys_index]]],[[not,sys_frame]],[sys_show_frame,sys_frame,sys_index]]],[kw_up,[if,[plusp,sys_frame_depth],[progn,[decf,sys_frame_depth],[do,[[sys_frame,[sys_next_function_frame,[-,sys_stack,20]],[sys_next_function_frame,sys_frame]],[sys_index,0,[+,1,sys_index]]],[[=,sys_index,sys_frame_depth],[setq,sys_active_frame,sys_frame]]],[sys_show_frame,sys_active_frame,sys_frame_depth]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Top of stack.~%")]]],[kw_down,[let,[[sys_frame,[sys_next_function_frame,sys_active_frame]]],[if,sys_frame,[progn,[incf,sys_frame_depth],[setq,sys_active_frame,sys_frame],[sys_show_frame,sys_active_frame,sys_frame_depth]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"Bottom of stack.~%")]]]],[kw_locals,[do,[[sys_env,[sys_fref,[-,sys_active_frame,1]],[cdr,sys_env]]],[[not,sys_env]],[when,[symbolp,[caar,sys_env]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,"~A~%"),[caar,sys_env]]]]],[kw_continue,[let,[[sys_index,[read]]],[invoke_restart_interactively,[nth,sys_index,sys_restarts]]]],[t,[let,[[values,[multiple_value_list,[eval,sys_form,[sys_fref,[-,sys_active_frame,1]]]]],[count,0]],[if,values,[dolist,[sys_value,values],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,";~A: ~S~%"),count,sys_value],[incf,count]],[format,xx_debug_io_xx,'$ARRAY'([*],claz_base_character,";No values.~%")]]]]],[go,sys_start]]]]]]))).
*/
/*
:-side_effect(assert_lsp(invoke_debugger,arglist_info(invoke_debugger,f_invoke_debugger,[condition],arginfo{all:[condition],allow_other_keys:0,aux:0,body:0,complex:0,env:0,key:0,names:[condition],opt:0,req:[condition],rest:0,sublists:0,whole:0}))).
*/
/*
:-side_effect(assert_lsp(invoke_debugger,init_args(x,f_invoke_debugger))).
*/
/*
#+(or WAM-CL LISP500) 
(defparameter *debugger-hook* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:8548 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defparameter,'*debugger-hook*',[]])
:- set_var(AEnv, xx_debugger_hook_xx, []).
/*
#+(or WAM-CL LISP500) 
(defparameter *break-on-signals* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:8610 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defparameter,'*break-on-signals*',[]])
:- set_var(AEnv, xx_break_on_signals_xx, []).
/*
#+(or WAM-CL LISP500) 
(defparameter *handlers* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:8675 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defparameter,'*handlers*',[]])
:- set_var(AEnv, sys_xx_handlers_xx, []).
/*
#+(or WAM-CL LISP500) 
(defun invoke-handler (condition)
  (dolist (handler *handlers*)
    (when (typep condition (car handler))
      (setq *handlers* (caddr handler))
      (funcall (cadr handler) condition))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:8732 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'invoke-handler',[condition],[dolist,[handler,'*handlers*'],[when,[typep,condition,[car,handler]],[setq,'*handlers*',[caddr,handler]],[funcall,[cadr,handler],condition]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_invoke_handler,
					       kw_function,
					       f_sys_invoke_handler)).
*/
wl:lambda_def(defun, sys_invoke_handler, f_sys_invoke_handler, [condition], [[dolist, [sys_handler, sys_xx_handlers_xx], [when, [typep, condition, [car, sys_handler]], [setq, sys_xx_handlers_xx, [caddr, sys_handler]], [funcall, [cadr, sys_handler], condition]]]]).
wl:arglist_info(sys_invoke_handler, f_sys_invoke_handler, [condition], arginfo{all:[condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[condition], opt:0, req:[condition], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_invoke_handler).

/*

### Compiled Function: `SYS::INVOKE-HANDLER` 
*/
f_sys_invoke_handler(Condition_In, FnResult) :-
	GEnv=[bv(condition, Condition_In)],
	catch(( ( get_var(GEnv, sys_xx_handlers_xx, Xx_handlers_xx_Get),
		  BV=bv(sys_handler, Ele),
		  AEnv=[BV|GEnv],
		  forall(member(Ele, Xx_handlers_xx_Get),
			 ( nb_setarg(2, BV, Ele),
			   get_var(AEnv, condition, Condition_Get),
			   get_var(AEnv, sys_handler, Handler_Get),
			   f_car(Handler_Get, Car_Ret),
			   f_typep(Condition_Get, Car_Ret, IFTEST),
			   (   IFTEST\==[]
			   ->  get_var(AEnv, sys_handler, Handler_Get11),
			       f_caddr(Handler_Get11, Xx_handlers_xx),
			       set_var(AEnv, sys_xx_handlers_xx, Xx_handlers_xx),
			       get_var(AEnv, sys_handler, Handler_Get12),
			       f_cadr(Handler_Get12, Apply_Param),
			       get_var(AEnv, condition, Condition_Get13),
			       f_apply(Apply_Param,
				       [Condition_Get13],
				       TrueResult),
			       _7198=TrueResult
			   ;   _7198=[]
			   )
			 ))
		),
		_7198=FnResult
	      ),
	      block_exit(sys_invoke_handler, FnResult),
	      true).
:- set_opv(sys_invoke_handler, symbol_function, f_sys_invoke_handler),
   DefunResult=sys_invoke_handler.
/*
:- side_effect(assert_lsp(sys_invoke_handler,
			  lambda_def(defun,
				     sys_invoke_handler,
				     f_sys_invoke_handler,
				     [condition],
				     
				     [ 
				       [ dolist,
					 [sys_handler, sys_xx_handlers_xx],
					 
					 [ when,
					   [typep, condition, [car, sys_handler]],
					   
					   [ setq,
					     sys_xx_handlers_xx,
					     [caddr, sys_handler]
					   ],
					   
					   [ funcall,
					     [cadr, sys_handler],
					     condition
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_invoke_handler,
			  arglist_info(sys_invoke_handler,
				       f_sys_invoke_handler,
				       [condition],
				       arginfo{ all:[condition],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[condition],
						opt:0,
						req:[condition],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_invoke_handler,
			  init_args(x, f_sys_invoke_handler))).
*/
/*
#+(or WAM-CL LISP500) 
(defmacro handler-bind (bindings &rest forms)
  (let ((form '*handlers*)
	(handlers (gensym)))
    (dolist (binding (reverse bindings))
      (setq form
	    `(cons (list ',(car binding) ,(cadr binding) ',handlers) ,form)))
    `(let ((handlers *handlers*)
	   (*handlers* ,form))
      ,@forms)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:8956 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'handler-bind',[bindings,'&rest',forms],[let,[[form,[quote,'*handlers*']],[handlers,[gensym]]],[dolist,[binding,[reverse,bindings]],[setq,form,['#BQ',[cons,[list,[quote,['#COMMA',[car,binding]]],['#COMMA',[cadr,binding]],[quote,['#COMMA',handlers]]],['#COMMA',form]]]]],['#BQ',[let,[[handlers,'*handlers*'],['*handlers*',['#COMMA',form]]],['#BQ-COMMA-ELIPSE',forms]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       handler_bind,
					       kw_special,
					       sf_handler_bind)).
*/
wl:lambda_def(defmacro, handler_bind, mf_handler_bind, [sys_bindings, c38_rest, sys_forms], [[let, [[sys_form, [quote, sys_xx_handlers_xx]], [sys_handlers, [gensym]]], [dolist, [sys_binding, [reverse, sys_bindings]], [setq, sys_form, ['#BQ', [cons, [list, [quote, ['#COMMA', [car, sys_binding]]], ['#COMMA', [cadr, sys_binding]], [quote, ['#COMMA', sys_handlers]]], ['#COMMA', sys_form]]]]], ['#BQ', [let, [[sys_handlers, sys_xx_handlers_xx], [sys_xx_handlers_xx, ['#COMMA', sys_form]]], ['#BQ-COMMA-ELIPSE', sys_forms]]]]]).
wl:arglist_info(handler_bind, mf_handler_bind, [sys_bindings, c38_rest, sys_forms], arginfo{all:[sys_bindings], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_bindings, sys_forms], opt:0, req:[sys_bindings], rest:[sys_forms], sublists:0, whole:0}).
wl: init_args(1, mf_handler_bind).

/*

### Compiled Macro Operator: `CL:HANDLER-BIND` 
*/
sf_handler_bind(MacroEnv, Bindings_In, RestNKeys, FResult) :-
	mf_handler_bind([handler_bind, Bindings_In|RestNKeys],
			MacroEnv,
			MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:HANDLER-BIND` 
*/
mf_handler_bind([handler_bind, Bindings_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	CDR=[bv(sys_bindings, Bindings_In), bv(sys_forms, RestNKeys)],
	catch(( ( f_gensym(Handlers_Init),
		  LEnv=[bv(sys_form, sys_xx_handlers_xx), bv(sys_handlers, Handlers_Init)|CDR],
		  get_var(LEnv, sys_bindings, Bindings_Get),
		  f_reverse(Bindings_Get, List),
		  BV=bv(sys_binding, Ele),
		  AEnv=[BV|LEnv],
		  forall(member(Ele, List),
			 ( nb_setarg(2, BV, Ele),
			   get_var(AEnv, sys_binding, Binding_Get),
			   f_car(Binding_Get, Car_Ret),
			   get_var(AEnv, sys_binding, Binding_Get13),
			   f_cadr(Binding_Get13, Cadr_Ret),
			   get_var(AEnv, sys_form, Form_Get),
			   get_var(AEnv, sys_handlers, Handlers_Get),
			   set_var(AEnv,
				   sys_form,
				   
				   [ cons,
				     
				     [ list,
				       [quote, Car_Ret],
				       Cadr_Ret,
				       [quote, Handlers_Get]
				     ],
				     Form_Get
				   ])
			 )),
		  get_var(LEnv, sys_form, Form_Get20),
		  get_var(LEnv, sys_forms, Forms_Get)
		),
		[let, [[sys_handlers, sys_xx_handlers_xx], [sys_xx_handlers_xx, Form_Get20]]|Forms_Get]=MFResult
	      ),
	      block_exit(handler_bind, MFResult),
	      true).
:- set_opv(mf_handler_bind, type_of, sys_macro),
   set_opv(handler_bind, symbol_function, mf_handler_bind),
   DefMacroResult=handler_bind.
/*
:- side_effect(assert_lsp(handler_bind,
			  lambda_def(defmacro,
				     handler_bind,
				     mf_handler_bind,
				     [sys_bindings, c38_rest, sys_forms],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_form,
					     [quote, sys_xx_handlers_xx]
					   ],
					   [sys_handlers, [gensym]]
					 ],
					 
					 [ dolist,
					   
					   [ sys_binding,
					     [reverse, sys_bindings]
					   ],
					   
					   [ setq,
					     sys_form,
					     
					     [ '#BQ',
					       
					       [ cons,
						 
						 [ list,
						   
						   [ quote,
						     
						     [ '#COMMA',
						       [car, sys_binding]
						     ]
						   ],
						   
						   [ '#COMMA',
						     [cadr, sys_binding]
						   ],
						   
						   [ quote,
						     ['#COMMA', sys_handlers]
						   ]
						 ],
						 ['#COMMA', sys_form]
					       ]
					     ]
					   ]
					 ],
					 
					 [ '#BQ',
					   
					   [ let,
					     
					     [ 
					       [ sys_handlers,
						 sys_xx_handlers_xx
					       ],
					       
					       [ sys_xx_handlers_xx,
						 ['#COMMA', sys_form]
					       ]
					     ],
					     ['#BQ-COMMA-ELIPSE', sys_forms]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(handler_bind,
			  arglist_info(handler_bind,
				       mf_handler_bind,
				       [sys_bindings, c38_rest, sys_forms],
				       arginfo{ all:[sys_bindings],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_bindings, sys_forms],
						opt:0,
						req:[sys_bindings],
						rest:[sys_forms],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(handler_bind, init_args(1, mf_handler_bind))).
*/
/*
#+(or WAM-CL LISP500) 
(defmacro handler-case (expression &rest clauses)
  (let ((tag (gensym))
	(bindings nil))
    `(handler-bind
      ,(dolist (clause clauses (reverse bindings))
	 (let ((typespec (car clause))
	       (var-list (cadr clause))
	       (forms (cddr clauses)))
	   (push `(typespec #'(lambda (,(if var-list (car var-list) (gensym)))
				(return-from tag (progn ,@forms))))
		 bindings)))
      ,expression)))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:9289 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'handler-case',[expression,'&rest',clauses],[let,[[tag,[gensym]],[bindings,[]]],['#BQ',['handler-bind',['#COMMA',[dolist,[clause,clauses,[reverse,bindings]],[let,[[typespec,[car,clause]],['var-list',[cadr,clause]],[forms,[cddr,clauses]]],[push,['#BQ',[typespec,function([lambda,[['#COMMA',[if,'var-list',[car,'var-list'],[gensym]]]],['return-from',tag,[progn,['#BQ-COMMA-ELIPSE',forms]]]])]],bindings]]]],['#COMMA',expression]]]]])
/*
% macroexpand:-[push,['#BQ',[sys_typespec,function([lambda,[['#COMMA',[if,sys_var_list,[car,sys_var_list],[gensym]]]],[return_from,sys_tag,[progn,['#BQ-COMMA-ELIPSE',sys_forms]]]])]],sys_bindings].
*/
/*
% into:-[setq,sys_bindings,[cons,['#BQ',[sys_typespec,function([lambda,[['#COMMA',[if,sys_var_list,[car,sys_var_list],[gensym]]]],[return_from,sys_tag,[progn,['#BQ-COMMA-ELIPSE',sys_forms]]]])]],sys_bindings]].
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       handler_case,
					       kw_special,
					       sf_handler_case)).
*/
wl:lambda_def(defmacro, handler_case, mf_handler_case, [sys_expression, c38_rest, sys_clauses], [[let, [[sys_tag, [gensym]], [sys_bindings, []]], ['#BQ', [handler_bind, ['#COMMA', [dolist, [sys_clause, sys_clauses, [reverse, sys_bindings]], [let, [[sys_typespec, [car, sys_clause]], [sys_var_list, [cadr, sys_clause]], [sys_forms, [cddr, sys_clauses]]], [push, ['#BQ', [sys_typespec, function([lambda, [['#COMMA', [if, sys_var_list, [car, sys_var_list], [gensym]]]], [return_from, sys_tag, [progn, ['#BQ-COMMA-ELIPSE', sys_forms]]]])]], sys_bindings]]]], ['#COMMA', sys_expression]]]]]).
wl:arglist_info(handler_case, mf_handler_case, [sys_expression, c38_rest, sys_clauses], arginfo{all:[sys_expression], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_expression, sys_clauses], opt:0, req:[sys_expression], rest:[sys_clauses], sublists:0, whole:0}).
wl: init_args(1, mf_handler_case).

/*

### Compiled Macro Operator: `CL:HANDLER-CASE` 
*/
sf_handler_case(MacroEnv, Expression_In, RestNKeys, FResult) :-
	mf_handler_case([handler_case, Expression_In|RestNKeys],
			MacroEnv,
			MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:HANDLER-CASE` 
*/
mf_handler_case([handler_case, Expression_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	CDR=[bv(sys_expression, Expression_In), bv(sys_clauses, RestNKeys)],
	catch(( ( f_gensym(Tag_Init),
		  LEnv=[bv(sys_tag, Tag_Init), bv(sys_bindings, [])|CDR],
		  get_var(LEnv, sys_bindings, Bindings_Get),
		  LEnv12=[bv(reverse, Bindings_Get)|LEnv],
		  get_var(LEnv12, sys_clauses, Clauses_Get),
		  BV=bv(sys_clause, Ele),
		  Env2=[BV|LEnv12],
		  forall(member(Ele, Clauses_Get),
			 ( nb_setarg(2, BV, Ele),
			   get_var(Env2, sys_clause, Clause_Get),
			   f_car(Clause_Get, Typespec_Init),
			   get_var(Env2, sys_clause, Clause_Get20),
			   f_cadr(Clause_Get20, Var_list_Init),
			   get_var(Env2, sys_clauses, Clauses_Get21),
			   f_cddr(Clauses_Get21, Forms_Init),
			   LEnv18=[bv(sys_typespec, Typespec_Init), bv(sys_var_list, Var_list_Init), bv(sys_forms, Forms_Init)|Env2],
			   get_var(LEnv18, sys_bindings, Bindings_Get26),
			   LetResult17=[[sys_typespec, function([lambda, [['#COMMA', [if, sys_var_list, [car, sys_var_list], [gensym]]]], [return_from, sys_tag, [progn, ['#BQ-COMMA-ELIPSE', sys_forms]]]])]|Bindings_Get26],
			   set_var(LEnv18, sys_bindings, LetResult17)
			 )),
		  get_var(LEnv12, sys_bindings, Bindings_Get31),
		  f_reverse(Bindings_Get31, LetResult11),
		  get_var(LEnv, sys_expression, Expression_Get)
		),
		[handler_bind, LetResult11, Expression_Get]=MFResult
	      ),
	      block_exit(handler_case, MFResult),
	      true).
:- set_opv(mf_handler_case, type_of, sys_macro),
   set_opv(handler_case, symbol_function, mf_handler_case),
   DefMacroResult=handler_case.
/*
:- side_effect(assert_lsp(handler_case,
			  lambda_def(defmacro,
				     handler_case,
				     mf_handler_case,
				     [sys_expression, c38_rest, sys_clauses],
				     
				     [ 
				       [ let,
					 
					 [ [sys_tag, [gensym]],
					   [sys_bindings, []]
					 ],
					 
					 [ '#BQ',
					   
					   [ handler_bind,
					     
					     [ '#COMMA',
					       
					       [ dolist,
						 
						 [ sys_clause,
						   sys_clauses,
						   [reverse, sys_bindings]
						 ],
						 
						 [ let,
						   
						   [ 
						     [ sys_typespec,
						       [car, sys_clause]
						     ],
						     
						     [ sys_var_list,
						       [cadr, sys_clause]
						     ],
						     
						     [ sys_forms,
						       [cddr, sys_clauses]
						     ]
						   ],
						   
						   [ push,
						     
						     [ '#BQ',
						       
						       [ sys_typespec,
							 function(
								  [ lambda,
								    
								    [ 
								      [ '#COMMA',
									
									[ if,
									  sys_var_list,
									  [car, sys_var_list],
									  [gensym]
									]
								      ]
								    ],
								    
								    [ return_from,
								      sys_tag,
								      
								      [ progn,
									
									[ '#BQ-COMMA-ELIPSE',
									  sys_forms
									]
								      ]
								    ]
								  ])
						       ]
						     ],
						     sys_bindings
						   ]
						 ]
					       ]
					     ],
					     ['#COMMA', sys_expression]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(handler_case,
			  arglist_info(handler_case,
				       mf_handler_case,
				       [sys_expression, c38_rest, sys_clauses],
				       arginfo{ all:[sys_expression],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_expression,
							sys_clauses
						      ],
						opt:0,
						req:[sys_expression],
						rest:[sys_clauses],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(handler_case, init_args(1, mf_handler_case))).
*/
/*
#+(or WAM-CL LISP500) 
(defmacro ignore-errors (&rest forms)
  `(handler-case (progn ,@forms)
    (error (condition) (values nil condition))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:9732 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'ignore-errors',['&rest',forms],['#BQ',['handler-case',[progn,['#BQ-COMMA-ELIPSE',forms]],[error,[condition],[values,[],condition]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       ignore_errors,
					       kw_special,
					       sf_ignore_errors)).
*/
wl:lambda_def(defmacro, ignore_errors, mf_ignore_errors, [c38_rest, sys_forms], [['#BQ', [handler_case, [progn, ['#BQ-COMMA-ELIPSE', sys_forms]], [error, [condition], [values, [], condition]]]]]).
wl:arglist_info(ignore_errors, mf_ignore_errors, [c38_rest, sys_forms], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_forms], opt:0, req:0, rest:[sys_forms], sublists:0, whole:0}).
wl: init_args(0, mf_ignore_errors).

/*

### Compiled Macro Operator: `CL:IGNORE-ERRORS` 
*/
sf_ignore_errors(MacroEnv, RestNKeys, FResult) :-
	mf_ignore_errors([ignore_errors|RestNKeys], MacroEnv, MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:IGNORE-ERRORS` 
*/
mf_ignore_errors([ignore_errors|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_forms, RestNKeys)],
	catch(( get_var(GEnv, sys_forms, Forms_Get),
		[handler_case, [progn|Forms_Get], [error, [condition], [values, [], condition]]]=MFResult
	      ),
	      block_exit(ignore_errors, MFResult),
	      true).
:- set_opv(mf_ignore_errors, type_of, sys_macro),
   set_opv(ignore_errors, symbol_function, mf_ignore_errors),
   DefMacroResult=ignore_errors.
/*
:- side_effect(assert_lsp(ignore_errors,
			  lambda_def(defmacro,
				     ignore_errors,
				     mf_ignore_errors,
				     [c38_rest, sys_forms],
				     
				     [ 
				       [ '#BQ',
					 
					 [ handler_case,
					   
					   [ progn,
					     ['#BQ-COMMA-ELIPSE', sys_forms]
					   ],
					   
					   [ error,
					     [condition],
					     [values, [], condition]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(ignore_errors,
			  arglist_info(ignore_errors,
				       mf_ignore_errors,
				       [c38_rest, sys_forms],
				       arginfo{ all:0,
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_forms],
						opt:0,
						req:0,
						rest:[sys_forms],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(ignore_errors, init_args(0, mf_ignore_errors))).
*/
/*
#+(or WAM-CL LISP500) 
(defparameter *restarts* nil)

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:9881 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defparameter,'*restarts*',[]])
:- set_var(AEnv, sys_xx_restarts_xx, []).
/*
#+(or WAM-CL LISP500) 
(defun compute-restarts (&optional condition)
  "FIXME restarts associated with conditions"
  (if condition
      *restarts*
      *restarts*))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:9938 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'compute-restarts',['&optional',condition],'$STRING'("FIXME restarts associated with conditions"),[if,condition,'*restarts*','*restarts*']])
doc: doc_string(compute_restarts,
	      _8326,
	      function,
	      "FIXME restarts associated with conditions").

wl:lambda_def(defun, compute_restarts, f_compute_restarts, [c38_optional, condition], [[if, condition, sys_xx_restarts_xx, sys_xx_restarts_xx]]).
wl:arglist_info(compute_restarts, f_compute_restarts, [c38_optional, condition], arginfo{all:[condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[condition], opt:[condition], req:0, rest:0, sublists:0, whole:0}).
wl: init_args(0, f_compute_restarts).

/*

### Compiled Function: `CL:COMPUTE-RESTARTS` 
*/
f_compute_restarts(RestNKeys, FnResult) :-
	GEnv=[bv(condition, Condition_In)],
	opt_var(Env, condition, Condition_In, true, [], 1, RestNKeys),
	catch(( ( get_var(GEnv, condition, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, sys_xx_restarts_xx, Xx_restarts_xx_Get),
		      _10132=Xx_restarts_xx_Get
		  ;   get_var(GEnv, sys_xx_restarts_xx, Xx_restarts_xx_Get10),
		      _10132=Xx_restarts_xx_Get10
		  )
		),
		_10132=FnResult
	      ),
	      block_exit(compute_restarts, FnResult),
	      true).
:- set_opv(compute_restarts, symbol_function, f_compute_restarts),
   DefunResult=compute_restarts.
/*
:- side_effect(assert_lsp(compute_restarts,
			  doc_string(compute_restarts,
				     _8326,
				     function,
				     "FIXME restarts associated with conditions"))).
*/
/*
:- side_effect(assert_lsp(compute_restarts,
			  lambda_def(defun,
				     compute_restarts,
				     f_compute_restarts,
				     [c38_optional, condition],
				     
				     [ 
				       [ if,
					 condition,
					 sys_xx_restarts_xx,
					 sys_xx_restarts_xx
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(compute_restarts,
			  arglist_info(compute_restarts,
				       f_compute_restarts,
				       [c38_optional, condition],
				       arginfo{ all:[condition],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[condition],
						opt:[condition],
						req:0,
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(compute_restarts, init_args(0, f_compute_restarts))).
*/
/*
#+(or WAM-CL LISP500) 
(defun find-restart (identifier &optional condition)
  (dolist (restart *restarts*)
    (when (eq restart identifier)
      (return restart))
    (when (eq (restart-name restart) identifier)
      (return restart))))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:10113 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'find-restart',[identifier,'&optional',condition],[dolist,[restart,'*restarts*'],[when,[eq,restart,identifier],[return,restart]],[when,[eq,['restart-name',restart],identifier],[return,restart]]]])
wl:lambda_def(defun, find_restart, f_find_restart, [sys_identifier, c38_optional, condition], [[dolist, [restart, sys_xx_restarts_xx], [when, [eq, restart, sys_identifier], [return, restart]], [when, [eq, [restart_name, restart], sys_identifier], [return, restart]]]]).
wl:arglist_info(find_restart, f_find_restart, [sys_identifier, c38_optional, condition], arginfo{all:[sys_identifier, condition], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_identifier, condition], opt:[condition], req:[sys_identifier], rest:0, sublists:0, whole:0}).
wl: init_args(1, f_find_restart).

/*

### Compiled Function: `CL:FIND-RESTART` 
*/
f_find_restart(Identifier_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_identifier, Identifier_In), bv(condition, Condition_In)],
	opt_var(Env, condition, Condition_In, true, [], 1, RestNKeys),
	catch(( ( get_var(GEnv, sys_xx_restarts_xx, Xx_restarts_xx_Get),
		  BV=bv(restart, Ele),
		  BlockExitEnv=[BV|GEnv],
		  forall(member(Ele, Xx_restarts_xx_Get),
			 ( nb_setarg(2, BV, Ele),
			   get_var(BlockExitEnv, restart, Restart_Get),
			   get_var(BlockExitEnv, sys_identifier, Identifier_Get),
			   (   is_eq(Restart_Get, Identifier_Get)
			   ->  get_var(BlockExitEnv, restart, Restart_Get16),
			       throw(block_exit([], Restart_Get16)),
			       _9718=ThrowResult
			   ;   _9718=[]
			   ),
			   get_var(BlockExitEnv, restart, Restart_Get20),
			   f_restart_name(Restart_Get20, PredArg1Result23),
			   get_var(BlockExitEnv,
				   sys_identifier,
				   Identifier_Get21),
			   (   is_eq(PredArg1Result23, Identifier_Get21)
			   ->  get_var(BlockExitEnv, restart, RetResult25),
			       throw(block_exit([], RetResult25)),
			       _8792=ThrowResult26
			   ;   _8792=[]
			   )
			 ))
		),
		_8792=FnResult
	      ),
	      block_exit(find_restart, FnResult),
	      true).
:- set_opv(find_restart, symbol_function, f_find_restart),
   DefunResult=find_restart.
/*
:- side_effect(assert_lsp(find_restart,
			  lambda_def(defun,
				     find_restart,
				     f_find_restart,
				     [sys_identifier, c38_optional, condition],
				     
				     [ 
				       [ dolist,
					 [restart, sys_xx_restarts_xx],
					 
					 [ when,
					   [eq, restart, sys_identifier],
					   [return, restart]
					 ],
					 
					 [ when,
					   
					   [ eq,
					     [restart_name, restart],
					     sys_identifier
					   ],
					   [return, restart]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(find_restart,
			  arglist_info(find_restart,
				       f_find_restart,
				       [sys_identifier, c38_optional, condition],
				       arginfo{ all:[sys_identifier, condition],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_identifier,
							condition
						      ],
						opt:[condition],
						req:[sys_identifier],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(find_restart, init_args(1, f_find_restart))).
*/
/*
#+(or WAM-CL LISP500) 
(defun designator-restart (designator)
  (if (restartp designator)
      designator
      (dolist (restart *restarts* (error 'type-error :datum designator
					 :expected-type 'restart))
	(when (eq (restart-name restart) designator)
	  (return restart)))))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:10366 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'designator-restart',[designator],[if,[restartp,designator],designator,[dolist,[restart,'*restarts*',[error,[quote,'type-error'],':datum',designator,':expected-type',[quote,restart]]],[when,[eq,['restart-name',restart],designator],[return,restart]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_designator_restart,
					       kw_function,
					       f_sys_designator_restart)).
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       sys_restartp,
					       kw_function,
					       f_sys_restartp)).
*/
wl:lambda_def(defun, sys_designator_restart, f_sys_designator_restart, [sys_designator], [[if, [sys_restartp, sys_designator], sys_designator, [dolist, [restart, sys_xx_restarts_xx, [error, [quote, type_error], kw_datum, sys_designator, kw_expected_type, [quote, restart]]], [when, [eq, [restart_name, restart], sys_designator], [return, restart]]]]]).
wl:arglist_info(sys_designator_restart, f_sys_designator_restart, [sys_designator], arginfo{all:[sys_designator], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_designator], opt:0, req:[sys_designator], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_designator_restart).

/*

### Compiled Function: `SYS::DESIGNATOR-RESTART` 
*/
f_sys_designator_restart(Designator_In, FnResult) :-
	GEnv=[bv(sys_designator, Designator_In)],
	catch(( ( get_var(GEnv, sys_designator, Designator_Get),
		  f_sys_restartp(Designator_Get, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, sys_designator, Designator_Get8),
		      _8990=Designator_Get8
		  ;   LEnv=[bv([error, [quote, type_error], kw_datum, sys_designator, kw_expected_type, [quote, restart]], [])|GEnv],
		      get_var(LEnv, sys_xx_restarts_xx, Xx_restarts_xx_Get),
		      BV=bv(restart, Ele),
		      BlockExitEnv=[BV|LEnv],
		      forall(member(Ele, Xx_restarts_xx_Get),
			     ( nb_setarg(2, BV, Ele),
			       get_var(BlockExitEnv, restart, Restart_Get),
			       f_restart_name(Restart_Get, PredArg1Result),
			       get_var(BlockExitEnv,
				       sys_designator,
				       Designator_Get15),
			       (   is_eq(PredArg1Result, Designator_Get15)
			       ->  get_var(BlockExitEnv, restart, Restart_Get21),
				   throw(block_exit([], Restart_Get21)),
				   _11208=ThrowResult
			       ;   _11208=[]
			       )
			     )),
		      get_var(LEnv, sys_designator, Designator_Get28),
		      f_error(
			      [ type_error,
				kw_datum,
				Designator_Get28,
				kw_expected_type,
				restart
			      ],
			      LetResult),
		      _8990=LetResult
		  )
		),
		_8990=FnResult
	      ),
	      block_exit(sys_designator_restart, FnResult),
	      true).
:- set_opv(sys_designator_restart, symbol_function, f_sys_designator_restart),
   DefunResult=sys_designator_restart.
/*
:- side_effect(assert_lsp(sys_designator_restart,
			  lambda_def(defun,
				     sys_designator_restart,
				     f_sys_designator_restart,
				     [sys_designator],
				     
				     [ 
				       [ if,
					 [sys_restartp, sys_designator],
					 sys_designator,
					 
					 [ dolist,
					   
					   [ restart,
					     sys_xx_restarts_xx,
					     
					     [ error,
					       [quote, type_error],
					       kw_datum,
					       sys_designator,
					       kw_expected_type,
					       [quote, restart]
					     ]
					   ],
					   
					   [ when,
					     
					     [ eq,
					       [restart_name, restart],
					       sys_designator
					     ],
					     [return, restart]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_designator_restart,
			  arglist_info(sys_designator_restart,
				       f_sys_designator_restart,
				       [sys_designator],
				       arginfo{ all:[sys_designator],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[sys_designator],
						opt:0,
						req:[sys_designator],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_designator_restart,
			  init_args(x, f_sys_designator_restart))).
*/
/*
#+(or WAM-CL LISP500) 
(defun invoke-restart (restart &rest arguments)
  (setq restart (designator-restart restart))
  (apply (restart-function restart) arguments))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:10658 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'invoke-restart',[restart,'&rest',arguments],[setq,restart,['designator-restart',restart]],[apply,['restart-function',restart],arguments]])
wl:lambda_def(defun, invoke_restart, f_invoke_restart, [restart, c38_rest, sys_arguments], [[setq, restart, [sys_designator_restart, restart]], [apply, [sys_restart_function, restart], sys_arguments]]).
wl:arglist_info(invoke_restart, f_invoke_restart, [restart, c38_rest, sys_arguments], arginfo{all:[restart], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[restart, sys_arguments], opt:0, req:[restart], rest:[sys_arguments], sublists:0, whole:0}).
wl: init_args(1, f_invoke_restart).

/*

### Compiled Function: `CL:INVOKE-RESTART` 
*/
f_invoke_restart(Restart_In, RestNKeys, FnResult) :-
	AEnv=[bv(restart, Restart_In), bv(sys_arguments, RestNKeys)],
	catch(( ( get_var(AEnv, restart, Restart_Get),
		  f_sys_designator_restart(Restart_Get, Restart),
		  set_var(AEnv, restart, Restart),
		  get_var(AEnv, restart, Restart_Get8),
		  f_sys_restart_function(Restart_Get8, Apply_Param),
		  get_var(AEnv, sys_arguments, Arguments_Get),
		  f_apply(Apply_Param, Arguments_Get, Apply_Ret)
		),
		Apply_Ret=FnResult
	      ),
	      block_exit(invoke_restart, FnResult),
	      true).
:- set_opv(invoke_restart, symbol_function, f_invoke_restart),
   DefunResult=invoke_restart.
/*
:- side_effect(assert_lsp(invoke_restart,
			  lambda_def(defun,
				     invoke_restart,
				     f_invoke_restart,
				     [restart, c38_rest, sys_arguments],
				     
				     [ 
				       [ setq,
					 restart,
					 [sys_designator_restart, restart]
				       ],
				       
				       [ apply,
					 [sys_restart_function, restart],
					 sys_arguments
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(invoke_restart,
			  arglist_info(invoke_restart,
				       f_invoke_restart,
				       [restart, c38_rest, sys_arguments],
				       arginfo{ all:[restart],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[restart, sys_arguments],
						opt:0,
						req:[restart],
						rest:[sys_arguments],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(invoke_restart, init_args(1, f_invoke_restart))).
*/
/*
#+(or WAM-CL LISP500) 
(defun invoke-restart-interactively (restart)
  (setq restart (designator-restart restart))
  (apply (restart-function restart)
	 (funcall (restart-interactive-function restart))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:10829 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'invoke-restart-interactively',[restart],[setq,restart,['designator-restart',restart]],[apply,['restart-function',restart],[funcall,['restart-interactive-function',restart]]]])
wl:lambda_def(defun, invoke_restart_interactively, f_invoke_restart_interactively, [restart], [[setq, restart, [sys_designator_restart, restart]], [apply, [sys_restart_function, restart], [funcall, [sys_restart_interactive_function, restart]]]]).
wl:arglist_info(invoke_restart_interactively, f_invoke_restart_interactively, [restart], arginfo{all:[restart], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[restart], opt:0, req:[restart], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_invoke_restart_interactively).

/*

### Compiled Function: `CL:INVOKE-RESTART-INTERACTIVELY` 
*/
f_invoke_restart_interactively(Restart_In, FnResult) :-
	AEnv=[bv(restart, Restart_In)],
	catch(( ( get_var(AEnv, restart, Restart_Get),
		  f_sys_designator_restart(Restart_Get, Restart),
		  set_var(AEnv, restart, Restart),
		  get_var(AEnv, restart, Restart_Get7),
		  f_sys_restart_function(Restart_Get7, Apply_Param13),
		  get_var(AEnv, restart, Restart_Get8),
		  f_sys_restart_interactive_function(Restart_Get8, Apply_Param),
		  f_apply(Apply_Param, [], Apply_Ret),
		  f_apply(Apply_Param13, Apply_Ret, Apply_Ret15)
		),
		Apply_Ret15=FnResult
	      ),
	      block_exit(invoke_restart_interactively, FnResult),
	      true).
:- set_opv(invoke_restart_interactively,
	   symbol_function,
	   f_invoke_restart_interactively),
   DefunResult=invoke_restart_interactively.
/*
:- side_effect(assert_lsp(invoke_restart_interactively,
			  lambda_def(defun,
				     invoke_restart_interactively,
				     f_invoke_restart_interactively,
				     [restart],
				     
				     [ 
				       [ setq,
					 restart,
					 [sys_designator_restart, restart]
				       ],
				       
				       [ apply,
					 [sys_restart_function, restart],
					 
					 [ funcall,
					   
					   [ sys_restart_interactive_function,
					     restart
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(invoke_restart_interactively,
			  arglist_info(invoke_restart_interactively,
				       f_invoke_restart_interactively,
				       [restart],
				       arginfo{ all:[restart],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[restart],
						opt:0,
						req:[restart],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(invoke_restart_interactively,
			  init_args(x, f_invoke_restart_interactively))).
*/
/*
#+(or WAM-CL LISP500) 
(defmacro restart-bind (restart-bindings &rest forms)
  (let ((form '*restarts*))
    (dolist (binding (reverse restart-bindings))
      (setq form
	    `(cons (make-restart ',(car binding) ,@(cdr binding)) ,form)))
    `(let ((*restarts* ,form))
      ,@forms)))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:11040 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'restart-bind',['restart-bindings','&rest',forms],[let,[[form,[quote,'*restarts*']]],[dolist,[binding,[reverse,'restart-bindings']],[setq,form,['#BQ',[cons,['make-restart',[quote,['#COMMA',[car,binding]]],['#BQ-COMMA-ELIPSE',[cdr,binding]]],['#COMMA',form]]]]],['#BQ',[let,[['*restarts*',['#COMMA',form]]],['#BQ-COMMA-ELIPSE',forms]]]]])
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       restart_bind,
					       kw_special,
					       sf_restart_bind)).
*/
wl:lambda_def(defmacro, restart_bind, mf_restart_bind, [sys_restart_bindings, c38_rest, sys_forms], [[let, [[sys_form, [quote, sys_xx_restarts_xx]]], [dolist, [sys_binding, [reverse, sys_restart_bindings]], [setq, sys_form, ['#BQ', [cons, [sys_make_restart, [quote, ['#COMMA', [car, sys_binding]]], ['#BQ-COMMA-ELIPSE', [cdr, sys_binding]]], ['#COMMA', sys_form]]]]], ['#BQ', [let, [[sys_xx_restarts_xx, ['#COMMA', sys_form]]], ['#BQ-COMMA-ELIPSE', sys_forms]]]]]).
wl:arglist_info(restart_bind, mf_restart_bind, [sys_restart_bindings, c38_rest, sys_forms], arginfo{all:[sys_restart_bindings], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_restart_bindings, sys_forms], opt:0, req:[sys_restart_bindings], rest:[sys_forms], sublists:0, whole:0}).
wl: init_args(1, mf_restart_bind).

/*

### Compiled Macro Operator: `CL:RESTART-BIND` 
*/
sf_restart_bind(MacroEnv, Restart_bindings_In, RestNKeys, FResult) :-
	mf_restart_bind([restart_bind, Restart_bindings_In|RestNKeys],
			MacroEnv,
			MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:RESTART-BIND` 
*/
mf_restart_bind([restart_bind, Restart_bindings_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	CDR=[bv(sys_restart_bindings, Restart_bindings_In), bv(sys_forms, RestNKeys)],
	catch(( ( LEnv=[bv(sys_form, sys_xx_restarts_xx)|CDR],
		  get_var(LEnv, sys_restart_bindings, Restart_bindings_Get),
		  f_reverse(Restart_bindings_Get, List),
		  BV=bv(sys_binding, Ele),
		  AEnv=[BV|LEnv],
		  forall(member(Ele, List),
			 ( nb_setarg(2, BV, Ele),
			   get_var(AEnv, sys_binding, Binding_Get),
			   f_car(Binding_Get, Car_Ret),
			   get_var(AEnv, sys_binding, Binding_Get12),
			   f_cdr(Binding_Get12, Cdr_Ret),
			   get_var(AEnv, sys_form, Form_Get),
			   set_var(AEnv,
				   sys_form,
				   
				   [ cons,
				     
				     [ sys_make_restart,
				       [quote, Car_Ret]
				     | Cdr_Ret
				     ],
				     Form_Get
				   ])
			 )),
		  get_var(LEnv, sys_form, Form_Get18),
		  get_var(LEnv, sys_forms, Forms_Get)
		),
		[let, [[sys_xx_restarts_xx, Form_Get18]]|Forms_Get]=MFResult
	      ),
	      block_exit(restart_bind, MFResult),
	      true).
:- set_opv(mf_restart_bind, type_of, sys_macro),
   set_opv(restart_bind, symbol_function, mf_restart_bind),
   DefMacroResult=restart_bind.
/*
:- side_effect(assert_lsp(restart_bind,
			  lambda_def(defmacro,
				     restart_bind,
				     mf_restart_bind,
				     [sys_restart_bindings, c38_rest, sys_forms],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ sys_form,
					     [quote, sys_xx_restarts_xx]
					   ]
					 ],
					 
					 [ dolist,
					   
					   [ sys_binding,
					     [reverse, sys_restart_bindings]
					   ],
					   
					   [ setq,
					     sys_form,
					     
					     [ '#BQ',
					       
					       [ cons,
						 
						 [ sys_make_restart,
						   
						   [ quote,
						     
						     [ '#COMMA',
						       [car, sys_binding]
						     ]
						   ],
						   
						   [ '#BQ-COMMA-ELIPSE',
						     [cdr, sys_binding]
						   ]
						 ],
						 ['#COMMA', sys_form]
					       ]
					     ]
					   ]
					 ],
					 
					 [ '#BQ',
					   
					   [ let,
					     
					     [ 
					       [ sys_xx_restarts_xx,
						 ['#COMMA', sys_form]
					       ]
					     ],
					     ['#BQ-COMMA-ELIPSE', sys_forms]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(restart_bind,
			  arglist_info(restart_bind,
				       mf_restart_bind,
				       
				       [ sys_restart_bindings,
					 c38_rest,
					 sys_forms
				       ],
				       arginfo{ all:[sys_restart_bindings],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_restart_bindings,
							sys_forms
						      ],
						opt:0,
						req:[sys_restart_bindings],
						rest:[sys_forms],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(restart_bind, init_args(1, mf_restart_bind))).
*/
/*
#+(or WAM-CL LISP500) 
(defmacro restart-case (restartable-form &rest clauses)
  (let ((catch-tag (gensym))
	(bindings nil))
    `(catch ',catch-tag
      (restart-bind
	  ,(dolist (clause clauses (reverse bindings))
	     (let ((name (car clause))
		   (lambda-list (cadr clause))
		   (rest (cddr clause))
		   (interactive '#'(lambda () nil))
		   (report '#'(lambda (stream)
				(format stream ""#+(or WAM-CL LISP500) \r\n(defmacro restart-case (restartable-form &rest clauses)\r\n  (let ((catch-tag (gensym))\r\n\t(bindings nil))\r\n    `(catch ',catch-tag\r\n      (restart-bind\r\n\t  ,(dolist (clause clauses (reverse bindings))\r\n\t     (let ((name (car clause))\r\n\t\t   (lambda-list (cadr clause))\r\n\t\t   (rest (cddr clause))\r\n\t\t   (interactive '#'(lambda () nil))\r\n\t\t   (report '#'(lambda (stream)\r\n\t\t\t\t(format stream \"~A\" (car clause))))\r\n\t\t   (test '#'(lambda (condition) t)))\r\n\t       (tagbody\r\n\t\tstart\r\n\t\t  (when (member (car rest) '(:interactive :report :test))\r\n\t\t    (let ((value (cadr rest)))\r\n\t\t      (case (car rest)\r\n\t\t\t(:interactive (setq interactive `(function ,value)))\r\n\t\t\t(:report (setq report\r\n\t\t\t\t       (if (stringp value)\r\n\t\t\t\t\t   `#'(lambda (stream)\r\n\t\t\t\t\t\t(write-string ,value stream))\r\n\t\t\t\t\t   `(function ,value))))\r\n\t\t\t(:test (setq test `(function ,value)))))\r\n\t\t    (setq rest (cddr rest))\r\n\t\t    (go start)))\r\n\t       (push `(,(car clause)\r\n\t\t       #'(lambda ,(cadr clause)\r\n\t\t\t   (throw ',catch-tag (progn ,@rest)))\r\n\t\t       :interactive-function ,interactive\r\n\t\t       :report-function ,report\r\n\t\t       :test-function ,test)\r\n\t\t     bindings)))\r\n\t,restartable-form))))\r\n\r\n\r\n\r\n\r\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:11339 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,'restart-case',['restartable-form','&rest',clauses],[let,[['catch-tag',[gensym]],[bindings,[]]],['#BQ',[catch,[quote,['#COMMA','catch-tag']],['restart-bind',['#COMMA',[dolist,[clause,clauses,[reverse,bindings]],[let,[[name,[car,clause]],['lambda-list',[cadr,clause]],[rest,[cddr,clause]],[interactive,[quote,function([lambda,[],[]])]],[report,[quote,function([lambda,[stream],[format,stream,'$STRING'("~A"),[car,clause]]])]],[test,[quote,function([lambda,[condition],t])]]],[tagbody,start,[when,[member,[car,rest],[quote,[':interactive',':report',':test']]],[let,[[value,[cadr,rest]]],[case,[car,rest],[':interactive',[setq,interactive,['#BQ',[function,['#COMMA',value]]]]],[':report',[setq,report,[if,[stringp,value],['#BQ',function([lambda,[stream],['write-string',['#COMMA',value],stream]])],['#BQ',[function,['#COMMA',value]]]]]],[':test',[setq,test,['#BQ',[function,['#COMMA',value]]]]]]],[setq,rest,[cddr,rest]],[go,start]]],[push,['#BQ',[['#COMMA',[car,clause]],function([lambda,['#COMMA',[cadr,clause]],[throw,[quote,['#COMMA','catch-tag']],[progn,['#BQ-COMMA-ELIPSE',rest]]]]),':interactive-function',['#COMMA',interactive],':report-function',['#COMMA',report],':test-function',['#COMMA',test]]],bindings]]]],['#COMMA','restartable-form']]]]]])
/*
% case:-[[kw_interactive,[setq,sys_interactive,['#BQ',[function,['#COMMA',sys_value]]]]],[kw_report,[setq,sys_report,[if,[stringp,sys_value],['#BQ',function([lambda,[stream],[write_string,['#COMMA',sys_value],stream]])],['#BQ',[function,['#COMMA',sys_value]]]]]],[kw_test,[setq,sys_test,['#BQ',[function,['#COMMA',sys_value]]]]]].
*/
/*
% conds:-[[[eq,_71020,[quote,kw_interactive]],[progn,[setq,sys_interactive,['#BQ',[function,['#COMMA',sys_value]]]]]],[[eq,_71020,[quote,kw_report]],[progn,[setq,sys_report,[if,[stringp,sys_value],['#BQ',function([lambda,[stream],[write_string,['#COMMA',sys_value],stream]])],['#BQ',[function,['#COMMA',sys_value]]]]]]],[[eq,_71020,[quote,kw_test]],[progn,[setq,sys_test,['#BQ',[function,['#COMMA',sys_value]]]]]]].
*/
/*
% case:-[[kw_interactive,[setq,sys_interactive,['#BQ',[function,['#COMMA',sys_value]]]]],[kw_report,[setq,sys_report,[if,[stringp,sys_value],['#BQ',function([lambda,[stream],[write_string,['#COMMA',sys_value],stream]])],['#BQ',[function,['#COMMA',sys_value]]]]]],[kw_test,[setq,sys_test,['#BQ',[function,['#COMMA',sys_value]]]]]].
*/
/*
% conds:-[[[eq,_77308,[quote,kw_interactive]],[progn,[setq,sys_interactive,['#BQ',[function,['#COMMA',sys_value]]]]]],[[eq,_77308,[quote,kw_report]],[progn,[setq,sys_report,[if,[stringp,sys_value],['#BQ',function([lambda,[stream],[write_string,['#COMMA',sys_value],stream]])],['#BQ',[function,['#COMMA',sys_value]]]]]]],[[eq,_77308,[quote,kw_test]],[progn,[setq,sys_test,['#BQ',[function,['#COMMA',sys_value]]]]]]].
*/
/*
% macroexpand:-[push,['#BQ',[['#COMMA',[car,sys_clause]],function([lambda,['#COMMA',[cadr,sys_clause]],[throw,[quote,['#COMMA',sys_catch_tag]],[progn,['#BQ-COMMA-ELIPSE',rest]]]]),kw_interactive_function,['#COMMA',sys_interactive],kw_report_function,['#COMMA',sys_report],kw_test_function,['#COMMA',sys_test]]],sys_bindings].
*/
/*
% into:-[setq,sys_bindings,[cons,['#BQ',[['#COMMA',[car,sys_clause]],function([lambda,['#COMMA',[cadr,sys_clause]],[throw,[quote,['#COMMA',sys_catch_tag]],[progn,['#BQ-COMMA-ELIPSE',rest]]]]),kw_interactive_function,['#COMMA',sys_interactive],kw_report_function,['#COMMA',sys_report],kw_test_function,['#COMMA',sys_test]]],sys_bindings]].
*/
/*
:- side_effect(generate_function_or_macro_name(
					       [ fbound(sys_expand, kw_function)=function(f_sys_expand11),
						 name='GLOBAL',
						 environ=env_1
					       ],
					       restart_case,
					       kw_special,
					       sf_restart_case)).
*/
wl:lambda_def(defmacro, restart_case, mf_restart_case, [sys_restartable_form, c38_rest, sys_clauses], [[let, [[sys_catch_tag, [gensym]], [sys_bindings, []]], ['#BQ', [catch, [quote, ['#COMMA', sys_catch_tag]], [restart_bind, ['#COMMA', [dolist, [sys_clause, sys_clauses, [reverse, sys_bindings]], [let, [[sys_name, [car, sys_clause]], [sys_lambda_list, [cadr, sys_clause]], [rest, [cddr, sys_clause]], [sys_interactive, [quote, function([lambda, [], []])]], [sys_report, [quote, function([lambda, [stream], [format, stream, '$ARRAY'([*], claz_base_character, "~A"), [car, sys_clause]]])]], [sys_test, [quote, function([lambda, [condition], t])]]], [tagbody, sys_start, [when, [member, [car, rest], [quote, [kw_interactive, kw_report, kw_test]]], [let, [[sys_value, [cadr, rest]]], [case, [car, rest], [kw_interactive, [setq, sys_interactive, ['#BQ', [function, ['#COMMA', sys_value]]]]], [kw_report, [setq, sys_report, [if, [stringp, sys_value], ['#BQ', function([lambda, [stream], [write_string, ['#COMMA', sys_value], stream]])], ['#BQ', [function, ['#COMMA', sys_value]]]]]], [kw_test, [setq, sys_test, ['#BQ', [function, ['#COMMA', sys_value]]]]]]], [setq, rest, [cddr, rest]], [go, sys_start]]], [push, ['#BQ', [['#COMMA', [car, sys_clause]], function([lambda, ['#COMMA', [cadr, sys_clause]], [throw, [quote, ['#COMMA', sys_catch_tag]], [progn, ['#BQ-COMMA-ELIPSE', rest]]]]), kw_interactive_function, ['#COMMA', sys_interactive], kw_report_function, ['#COMMA', sys_report], kw_test_function, ['#COMMA', sys_test]]], sys_bindings]]]], ['#COMMA', sys_restartable_form]]]]]]).
wl:arglist_info(restart_case, mf_restart_case, [sys_restartable_form, c38_rest, sys_clauses], arginfo{all:[sys_restartable_form], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_restartable_form, sys_clauses], opt:0, req:[sys_restartable_form], rest:[sys_clauses], sublists:0, whole:0}).
wl: init_args(1, mf_restart_case).

/*

### Compiled Macro Operator: `CL:RESTART-CASE` 
*/
sf_restart_case(MacroEnv, Restartable_form_In, RestNKeys, FResult) :-
	mf_restart_case([restart_case, Restartable_form_In|RestNKeys],
			MacroEnv,
			MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:RESTART-CASE` 
*/
mf_restart_case([restart_case, Restartable_form_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	CDR=[bv(sys_restartable_form, Restartable_form_In), bv(sys_clauses, RestNKeys)],
	catch(( ( f_gensym(Catch_tag_Init),
		  LEnv=[bv(sys_catch_tag, Catch_tag_Init), bv(sys_bindings, [])|CDR],
		  get_var(LEnv, sys_bindings, Bindings_Get),
		  get_var(LEnv, sys_catch_tag, Catch_tag_Get),
		  LEnv13=[bv(reverse, Bindings_Get)|LEnv],
		  get_var(LEnv13, sys_clauses, Clauses_Get),
		  BV=bv(sys_clause, Ele),
		  Env2=[BV|LEnv13],
		  forall(member(Ele, Clauses_Get),
			 ( nb_setarg(2, BV, Ele),
			   get_var(Env2, sys_clause, Clause_Get),
			   f_car(Clause_Get, Name_Init),
			   get_var(Env2, sys_clause, Clause_Get21),
			   f_cadr(Clause_Get21, Lambda_list_Init),
			   get_var(Env2, sys_clause, Clause_Get22),
			   f_cddr(Clause_Get22, Rest_Init),
			   AEnv=[bv(sys_name, Name_Init), bv(sys_lambda_list, Lambda_list_Init), bv(rest, Rest_Init), bv(sys_interactive, function([lambda, [], []])), bv(sys_report, function([lambda, [stream], [format, stream, '$ARRAY'([*], claz_base_character, "~A"), [car, sys_clause]]])), bv(sys_test, function([lambda, [condition], t]))|Env2],
			   call_addr_block(AEnv,
					   (push_label(sys_start), get_var(AEnv, rest, Rest_Get64), f_car(Rest_Get64, Member_Param), f_member(Member_Param, [kw_interactive, kw_report, kw_test], [], IFTEST62), (IFTEST62\==[]->get_var(AEnv, rest, Rest_Get68), f_cadr(Rest_Get68, Value_Init69), LEnv67=[bv(sys_value, Value_Init69)|AEnv], get_var(LEnv67, rest, Rest_Get70), f_car(Rest_Get70, Key71), (is_eq(Key71, kw_interactive)->get_var(LEnv67, sys_value, Value_Get76), set_var(LEnv67, sys_interactive, [function, Value_Get76]), LetResult66=[function, Value_Get76];(is_eq(Key71, kw_report)->get_var(LEnv67, sys_value, Value_Get80), (is_stringp(Value_Get80)->TrueResult88=function([lambda, [stream], [write_string, ['#COMMA', sys_value], stream]]);get_var(LEnv67, sys_value, Value_Get83), TrueResult88=[function, Value_Get83]), set_var(LEnv67, sys_report, TrueResult88), ElseResult90=TrueResult88;(is_eq(Key71, kw_test)->get_var(LEnv67, sys_value, Value_Get86), set_var(LEnv67, sys_test, [function, Value_Get86]), ElseResult89=[function, Value_Get86];ElseResult87=[], ElseResult89=ElseResult87), ElseResult90=ElseResult89), LetResult66=ElseResult90), get_var(AEnv, rest, Rest_Get92), f_cddr(Rest_Get92, Rest), set_var(AEnv, rest, Rest), goto(sys_start, AEnv), _TBResult=_GORES93;_TBResult=[])),
					   
					   [ addr(addr_tagbody_35_sys_start,
						  sys_start,
						  '$unused',
						  AEnv56,
						  (get_var(AEnv56, rest, Car_Param), f_car(Car_Param, Member_Param112), f_member(Member_Param112, [kw_interactive, kw_report, kw_test], [], IFTEST), (IFTEST\==[]->get_var(AEnv56, rest, Rest_Get33), f_cadr(Rest_Get33, Cadr_Ret), LEnv32=[bv(sys_value, Cadr_Ret)|AEnv56], get_var(LEnv32, rest, Rest_Get35), f_car(Rest_Get35, Key), (is_eq(Key, kw_interactive)->get_var(LEnv32, sys_value, Get_var_Ret), set_var(LEnv32, sys_interactive, [function, Get_var_Ret]), LetResult31=[function, Get_var_Ret];(is_eq(Key, kw_report)->get_var(LEnv32, sys_value, Value_Get45), (is_stringp(Value_Get45)->Set_var_Ret=function([lambda, [stream], [write_string, ['#COMMA', sys_value], stream]]);get_var(LEnv32, sys_value, Value_Get48), Set_var_Ret=[function, Value_Get48]), set_var(LEnv32, sys_report, Set_var_Ret), ElseResult55=Set_var_Ret;(is_eq(Key, kw_test)->get_var(LEnv32, sys_value, Value_Get51), set_var(LEnv32, sys_test, [function, Value_Get51]), ElseResult54=[function, Value_Get51];_11712=[], ElseResult54=_11712), ElseResult55=ElseResult54), LetResult31=ElseResult55), get_var(AEnv56, rest, Rest_Get57), f_cddr(Rest_Get57, Cddr_Ret), set_var(AEnv56, rest, Cddr_Ret), goto(sys_start, AEnv56), _11730=_GORES;_11730=[])))
					   ]),
			   get_var(AEnv, sys_clause, Clause_Get96),
			   f_car(Clause_Get96, Car_Ret),
			   ( get_var(AEnv, sys_bindings, Bindings_Get100),
			     get_var(AEnv, sys_interactive, Interactive_Get)
			   ),
			   get_var(AEnv, sys_report, Report_Get),
			   get_var(AEnv, sys_test, Test_Get),
			   LetResult18=[[Car_Ret, function([lambda, ['#COMMA', [cadr, sys_clause]], [throw, [quote, ['#COMMA', sys_catch_tag]], [progn, ['#BQ-COMMA-ELIPSE', rest]]]]), kw_interactive_function, Interactive_Get, kw_report_function, Report_Get, kw_test_function, Test_Get]|Bindings_Get100],
			   set_var(AEnv, sys_bindings, LetResult18)
			 )),
		  get_var(LEnv13, sys_bindings, Bindings_Get105),
		  f_reverse(Bindings_Get105, LetResult12),
		  get_var(LEnv, sys_restartable_form, Restartable_form_Get)
		),
		[catch, [quote, Catch_tag_Get], [restart_bind, LetResult12, Restartable_form_Get]]=MFResult
	      ),
	      block_exit(restart_case, MFResult),
	      true).
:- set_opv(mf_restart_case, type_of, sys_macro),
   set_opv(restart_case, symbol_function, mf_restart_case),
   DefMacroResult=restart_case.
/*
:- side_effect(assert_lsp(restart_case,
			  lambda_def(defmacro,
				     restart_case,
				     mf_restart_case,
				     
				     [ sys_restartable_form,
				       c38_rest,
				       sys_clauses
				     ],
				     
				     [ 
				       [ let,
					 
					 [ [sys_catch_tag, [gensym]],
					   [sys_bindings, []]
					 ],
					 
					 [ '#BQ',
					   
					   [ catch,
					     [quote, ['#COMMA', sys_catch_tag]],
					     
					     [ restart_bind,
					       
					       [ '#COMMA',
						 
						 [ dolist,
						   
						   [ sys_clause,
						     sys_clauses,
						     [reverse, sys_bindings]
						   ],
						   
						   [ let,
						     
						     [ 
						       [ sys_name,
							 [car, sys_clause]
						       ],
						       
						       [ sys_lambda_list,
							 [cadr, sys_clause]
						       ],
						       
						       [ rest,
							 [cddr, sys_clause]
						       ],
						       
						       [ sys_interactive,
							 
							 [ quote,
							   function([lambda, [], []])
							 ]
						       ],
						       
						       [ sys_report,
							 
							 [ quote,
							   function(
								    [ lambda,
								      [stream],
								      
								      [ format,
									stream,
									'$ARRAY'([*],
										 claz_base_character,
										 "~A"),
									[car, sys_clause]
								      ]
								    ])
							 ]
						       ],
						       
						       [ sys_test,
							 
							 [ quote,
							   function(
								    [ lambda,
								      [condition],
								      t
								    ])
							 ]
						       ]
						     ],
						     
						     [ tagbody,
						       sys_start,
						       
						       [ when,
							 
							 [ member,
							   [car, rest],
							   
							   [ quote,
							     
							     [ kw_interactive,
							       kw_report,
							       kw_test
							     ]
							   ]
							 ],
							 
							 [ let,
							   
							   [ 
							     [ sys_value,
							       [cadr, rest]
							     ]
							   ],
							   
							   [ case,
							     [car, rest],
							     
							     [ kw_interactive,
							       
							       [ setq,
								 sys_interactive,
								 
								 [ '#BQ',
								   
								   [ function,
								     
								     [ '#COMMA',
								       sys_value
								     ]
								   ]
								 ]
							       ]
							     ],
							     
							     [ kw_report,
							       
							       [ setq,
								 sys_report,
								 
								 [ if,
								   [stringp, sys_value],
								   
								   [ '#BQ',
								     function(
									      [ lambda,
										[stream],
										
										[ write_string,
										  
										  [ '#COMMA',
										    sys_value
										  ],
										  stream
										]
									      ])
								   ],
								   
								   [ '#BQ',
								     
								     [ function,
								       
								       [ '#COMMA',
									 sys_value
								       ]
								     ]
								   ]
								 ]
							       ]
							     ],
							     
							     [ kw_test,
							       
							       [ setq,
								 sys_test,
								 
								 [ '#BQ',
								   
								   [ function,
								     
								     [ '#COMMA',
								       sys_value
								     ]
								   ]
								 ]
							       ]
							     ]
							   ]
							 ],
							 
							 [ setq,
							   rest,
							   [cddr, rest]
							 ],
							 [go, sys_start]
						       ]
						     ],
						     
						     [ push,
						       
						       [ '#BQ',
							 
							 [ 
							   [ '#COMMA',
							     [car, sys_clause]
							   ],
							   function(
								    [ lambda,
								      
								      [ '#COMMA',
									[cadr, sys_clause]
								      ],
								      
								      [ throw,
									
									[ quote,
									  
									  [ '#COMMA',
									    sys_catch_tag
									  ]
									],
									
									[ progn,
									  
									  [ '#BQ-COMMA-ELIPSE',
									    rest
									  ]
									]
								      ]
								    ]),
							   kw_interactive_function,
							   
							   [ '#COMMA',
							     sys_interactive
							   ],
							   kw_report_function,
							   
							   [ '#COMMA',
							     sys_report
							   ],
							   kw_test_function,
							   ['#COMMA', sys_test]
							 ]
						       ],
						       sys_bindings
						     ]
						   ]
						 ]
					       ],
					       ['#COMMA', sys_restartable_form]
					     ]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(restart_case,
			  arglist_info(restart_case,
				       mf_restart_case,
				       
				       [ sys_restartable_form,
					 c38_rest,
					 sys_clauses
				       ],
				       arginfo{ all:[sys_restartable_form],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_restartable_form,
							sys_clauses
						      ],
						opt:0,
						req:[sys_restartable_form],
						rest:[sys_clauses],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(restart_case, init_args(1, mf_restart_case))).
*/
/*
#+(or WAM-CL LISP500) 
(defun warn (datum &rest arguments)
  (restart-case
      (let ((warning (if (symbolp datum)
			 (apply #'make-condition 'warning datum arguments)
			 datum)))
	(signal warning)
	(print-object warning *error-output*))
    (muffle-warning () nil))
  nil)


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:12540 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,warn,[datum,'&rest',arguments],['restart-case',[let,[[warning,[if,[symbolp,datum],[apply,function('make-condition'),[quote,warning],datum,arguments],datum]]],[signal,warning],['print-object',warning,'*error-output*']],['muffle-warning',[],[]]],[]])
/*
% macroexpand:-[restart_case,[let,[[warning,[if,[symbolp,sys_datum],[apply,function(make_condition),[quote,warning],sys_datum,sys_arguments],sys_datum]]],[signal,warning],[print_object,warning,xx_error_output_xx]],[muffle_warning,[],[]]].
*/
/*
% into:-[catch,[quote,g11],[restart_bind,[[muffle_warning,function([lambda,['#COMMA',[cadr,sys_clause]],[throw,[quote,['#COMMA',sys_catch_tag]],[progn,['#BQ-COMMA-ELIPSE',rest]]]]),kw_interactive_function,function([lambda,[],[]]),kw_report_function,function([lambda,[stream],[format,stream,'$ARRAY'([*],claz_base_character,"~A"),[car,sys_clause]]]),kw_test_function,function([lambda,[condition],t])]],[let,[[warning,[if,[symbolp,sys_datum],[apply,function(make_condition),[quote,warning],sys_datum,sys_arguments],sys_datum]]],[signal,warning],[print_object,warning,xx_error_output_xx]]]].
*/
wl:lambda_def(defun, warn, f_warn, [sys_datum, c38_rest, sys_arguments], [[restart_case, [let, [[warning, [if, [symbolp, sys_datum], [apply, function(make_condition), [quote, warning], sys_datum, sys_arguments], sys_datum]]], [signal, warning], [print_object, warning, xx_error_output_xx]], [muffle_warning, [], []]], []]).
wl:arglist_info(warn, f_warn, [sys_datum, c38_rest, sys_arguments], arginfo{all:[sys_datum], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_datum, sys_arguments], opt:0, req:[sys_datum], rest:[sys_arguments], sublists:0, whole:0}).
wl: init_args(1, f_warn).

/*

### Compiled Function: `CL:WARN` 
*/
f_warn(Datum_In, RestNKeys, FnResult) :-
	_7416=[bv(sys_datum, Datum_In), bv(sys_arguments, RestNKeys)],
	catch(( sf_catch([quote, g11],
			 
			 [ restart_bind,
			   
			   [ 
			     [ muffle_warning,
			       function(
					[ lambda,
					  ['#COMMA', [cadr, sys_clause]],
					  
					  [ throw,
					    [quote, ['#COMMA', sys_catch_tag]],
					    [progn, ['#BQ-COMMA-ELIPSE', rest]]
					  ]
					]),
			       kw_interactive_function,
			       function([lambda, [], []]),
			       kw_report_function,
			       function(
					[ lambda,
					  [stream],
					  
					  [ format,
					    stream,
					    '$ARRAY'([*],
						     claz_base_character,
						     "~A"),
					    [car, sys_clause]
					  ]
					]),
			       kw_test_function,
			       function([lambda, [condition], t])
			     ]
			   ],
			   
			   [ let,
			     
			     [ 
			       [ warning,
				 
				 [ if,
				   [symbolp, sys_datum],
				   
				   [ apply,
				     function(make_condition),
				     [quote, warning],
				     sys_datum,
				     sys_arguments
				   ],
				   sys_datum
				 ]
			       ]
			     ],
			     [signal, warning],
			     [print_object, warning, xx_error_output_xx]
			   ]
			 ],
			 Sf_catch_Ret),
		[]=FnResult
	      ),
	      block_exit(warn, FnResult),
	      true).
:- set_opv(warn, symbol_function, f_warn),
   DefunResult=warn.
/*
:- side_effect(assert_lsp(warn,
			  lambda_def(defun,
				     warn,
				     f_warn,
				     [sys_datum, c38_rest, sys_arguments],
				     
				     [ 
				       [ restart_case,
					 
					 [ let,
					   
					   [ 
					     [ warning,
					       
					       [ if,
						 [symbolp, sys_datum],
						 
						 [ apply,
						   function(make_condition),
						   [quote, warning],
						   sys_datum,
						   sys_arguments
						 ],
						 sys_datum
					       ]
					     ]
					   ],
					   [signal, warning],
					   
					   [ print_object,
					     warning,
					     xx_error_output_xx
					   ]
					 ],
					 [muffle_warning, [], []]
				       ],
				       []
				     ]))).
*/
/*
:- side_effect(assert_lsp(warn,
			  arglist_info(warn,
				       f_warn,
				       [sys_datum, c38_rest, sys_arguments],
				       arginfo{ all:[sys_datum],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_datum,
							sys_arguments
						      ],
						opt:0,
						req:[sys_datum],
						rest:[sys_arguments],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(warn, init_args(1, f_warn))).
*/
/*
#+(or WAM-CL LISP500) 
(defun error (datum &rest arguments)
  (let ((condition (designator-condition 'simple-error datum arguments)))
    (when (typep condition *break-on-signals*)
      (invoke-debugger condition))
    (invoke-handler condition)
    (invoke-debugger condition)))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:12831 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,error,[datum,'&rest',arguments],[let,[[condition,['designator-condition',[quote,'simple-error'],datum,arguments]]],[when,[typep,condition,'*break-on-signals*'],['invoke-debugger',condition]],['invoke-handler',condition],['invoke-debugger',condition]]])
wl:lambda_def(defun, error, f_error, [sys_datum, c38_rest, sys_arguments], [[let, [[condition, [sys_designator_condition, [quote, simple_error], sys_datum, sys_arguments]]], [when, [typep, condition, xx_break_on_signals_xx], [invoke_debugger, condition]], [sys_invoke_handler, condition], [invoke_debugger, condition]]]).
wl:arglist_info(error, f_error, [sys_datum, c38_rest, sys_arguments], arginfo{all:[sys_datum], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_datum, sys_arguments], opt:0, req:[sys_datum], rest:[sys_arguments], sublists:0, whole:0}).
wl: init_args(0, f_error).

/*

### Compiled Function: `CL:ERROR` 
*/
f_error(Datum_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_datum, Datum_In), bv(sys_arguments, RestNKeys)],
	catch(( ( get_var(GEnv, sys_arguments, Arguments_Get),
		  get_var(GEnv, sys_datum, Datum_Get),
		  f_sys_designator_condition(simple_error,
					     Datum_Get,
					     Arguments_Get,
					     Condition_Init),
		  LEnv=[bv(condition, Condition_Init)|GEnv],
		  get_var(LEnv, condition, Condition_Get),
		  get_var(LEnv,
			  xx_break_on_signals_xx,
			  Xx_break_on_signals_xx_Get),
		  f_typep(Condition_Get, Xx_break_on_signals_xx_Get, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(LEnv, condition, Condition_Get16),
		      f_invoke_debugger(Condition_Get16, TrueResult),
		      _7438=TrueResult
		  ;   _7438=[]
		  ),
		  get_var(LEnv, condition, Condition_Get18),
		  f_sys_invoke_handler(Condition_Get18, Invoke_handler_Ret),
		  get_var(LEnv, condition, Condition_Get19),
		  f_invoke_debugger(Condition_Get19, LetResult)
		),
		LetResult=FnResult
	      ),
	      block_exit(error, FnResult),
	      true).
:- set_opv(error, symbol_function, f_error),
   DefunResult=error.
/*
:- side_effect(assert_lsp(error,
			  lambda_def(defun,
				     error,
				     f_error,
				     [sys_datum, c38_rest, sys_arguments],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ condition,
					     
					     [ sys_designator_condition,
					       [quote, simple_error],
					       sys_datum,
					       sys_arguments
					     ]
					   ]
					 ],
					 
					 [ when,
					   
					   [ typep,
					     condition,
					     xx_break_on_signals_xx
					   ],
					   [invoke_debugger, condition]
					 ],
					 [sys_invoke_handler, condition],
					 [invoke_debugger, condition]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(error,
			  arglist_info(error,
				       f_error,
				       [sys_datum, c38_rest, sys_arguments],
				       arginfo{ all:[sys_datum],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_datum,
							sys_arguments
						      ],
						opt:0,
						req:[sys_datum],
						rest:[sys_arguments],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(error, init_args(0, f_error))).
*/
/*
#+(or WAM-CL LISP500) 
(defun cerror (continue-format-control datum &rest arguments)
  (with-simple-restart (continue continue-format-control)
    (apply #'error datum arguments)))


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:13123 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,cerror,['continue-format-control',datum,'&rest',arguments],['with-simple-restart',[continue,'continue-format-control'],[apply,function(error),datum,arguments]]])
wl:lambda_def(defun, cerror, f_cerror, [sys_continue_format_control, sys_datum, c38_rest, sys_arguments], [[with_simple_restart, [continue, sys_continue_format_control], [apply, function(error), sys_datum, sys_arguments]]]).
wl:arglist_info(cerror, f_cerror, [sys_continue_format_control, sys_datum, c38_rest, sys_arguments], arginfo{all:[sys_continue_format_control, sys_datum], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_continue_format_control, sys_datum, sys_arguments], opt:0, req:[sys_continue_format_control, sys_datum], rest:[sys_arguments], sublists:0, whole:0}).
wl: init_args(2, f_cerror).

/*

### Compiled Function: `CL:CERROR` 
*/
f_cerror(Continue_format_control_In, Datum_In, RestNKeys, FnResult) :-
	Simple_restart_Param=[bv(sys_continue_format_control, Continue_format_control_In), bv(sys_datum, Datum_In), bv(sys_arguments, RestNKeys)],
	catch(( sf_with_simple_restart(Simple_restart_Param,
				       [continue, sys_continue_format_control],
				       
				       [ apply,
					 function(error),
					 sys_datum,
					 sys_arguments
				       ],
				       Simple_restart_Ret),
		Simple_restart_Ret=FnResult
	      ),
	      block_exit(cerror, FnResult),
	      true).
:- set_opv(cerror, symbol_function, f_cerror),
   DefunResult=cerror.
/*
:- side_effect(assert_lsp(cerror,
			  lambda_def(defun,
				     cerror,
				     f_cerror,
				     
				     [ sys_continue_format_control,
				       sys_datum,
				       c38_rest,
				       sys_arguments
				     ],
				     
				     [ 
				       [ with_simple_restart,
					 
					 [ continue,
					   sys_continue_format_control
					 ],
					 
					 [ apply,
					   function(error),
					   sys_datum,
					   sys_arguments
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(cerror,
			  arglist_info(cerror,
				       f_cerror,
				       
				       [ sys_continue_format_control,
					 sys_datum,
					 c38_rest,
					 sys_arguments
				       ],
				       arginfo{ all:
						    [ sys_continue_format_control,
						      sys_datum
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_continue_format_control,
							sys_datum,
							sys_arguments
						      ],
						opt:0,
						req:
						    [ sys_continue_format_control,
						      sys_datum
						    ],
						rest:[sys_arguments],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(cerror, init_args(2, f_cerror))).
*/
/*
#+(or WAM-CL LISP500) 
(defun signal (datum &rest arguments)
  (let ((condition (designator-condition 'simple-condition datum arguments)))
    (when (typep condition *break-on-signals*)
      (invoke-debugger condition))
    (invoke-handler condition)
    nil))



*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-20.lisp:13312 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,signal,[datum,'&rest',arguments],[let,[[condition,['designator-condition',[quote,'simple-condition'],datum,arguments]]],[when,[typep,condition,'*break-on-signals*'],['invoke-debugger',condition]],['invoke-handler',condition],[]]])
wl:lambda_def(defun, signal, f_signal, [sys_datum, c38_rest, sys_arguments], [[let, [[condition, [sys_designator_condition, [quote, simple_condition], sys_datum, sys_arguments]]], [when, [typep, condition, xx_break_on_signals_xx], [invoke_debugger, condition]], [sys_invoke_handler, condition], []]]).
wl:arglist_info(signal, f_signal, [sys_datum, c38_rest, sys_arguments], arginfo{all:[sys_datum], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_datum, sys_arguments], opt:0, req:[sys_datum], rest:[sys_arguments], sublists:0, whole:0}).
wl: init_args(1, f_signal).

/*

### Compiled Function: `CL:SIGNAL` 
*/
f_signal(Datum_In, RestNKeys, FnResult) :-
	GEnv=[bv(sys_datum, Datum_In), bv(sys_arguments, RestNKeys)],
	catch(( ( get_var(GEnv, sys_arguments, Arguments_Get),
		  get_var(GEnv, sys_datum, Datum_Get),
		  f_sys_designator_condition(simple_condition,
					     Datum_Get,
					     Arguments_Get,
					     Condition_Init),
		  LEnv=[bv(condition, Condition_Init)|GEnv],
		  get_var(LEnv, condition, Condition_Get),
		  get_var(LEnv,
			  xx_break_on_signals_xx,
			  Xx_break_on_signals_xx_Get),
		  f_typep(Condition_Get, Xx_break_on_signals_xx_Get, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(LEnv, condition, Condition_Get16),
		      f_invoke_debugger(Condition_Get16, TrueResult),
		      _7382=TrueResult
		  ;   _7382=[]
		  ),
		  get_var(LEnv, condition, Condition_Get18),
		  f_sys_invoke_handler(Condition_Get18, Invoke_handler_Ret)
		),
		[]=FnResult
	      ),
	      block_exit(signal, FnResult),
	      true).
:- set_opv(signal, symbol_function, f_signal),
   DefunResult=signal.
/*
:- side_effect(assert_lsp(signal,
			  lambda_def(defun,
				     signal,
				     f_signal,
				     [sys_datum, c38_rest, sys_arguments],
				     
				     [ 
				       [ let,
					 
					 [ 
					   [ condition,
					     
					     [ sys_designator_condition,
					       [quote, simple_condition],
					       sys_datum,
					       sys_arguments
					     ]
					   ]
					 ],
					 
					 [ when,
					   
					   [ typep,
					     condition,
					     xx_break_on_signals_xx
					   ],
					   [invoke_debugger, condition]
					 ],
					 [sys_invoke_handler, condition],
					 []
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(signal,
			  arglist_info(signal,
				       f_signal,
				       [sys_datum, c38_rest, sys_arguments],
				       arginfo{ all:[sys_datum],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:
						      [ sys_datum,
							sys_arguments
						      ],
						opt:0,
						req:[sys_datum],
						rest:[sys_arguments],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(signal, init_args(1, f_signal))).
*/


%; Total compilation time: 17.252 seconds

