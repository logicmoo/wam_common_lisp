#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "lib/lsp/export" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/export.lsp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Sun Jan 28 04:47:26 2018

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
*/
/*
;;;  Copyright (c) 1990, Giuseppe Attardi.
*/
/*
;;;
*/
/*
;;;    This program is free software; you can redistribute it and/or
*/
/*
;;;    modify it under the terms of the GNU Library General Public
*/
/*
;;;    License as published by the Free Software Foundation; either
*/
/*
;;;    version 2 of the License, or (at your option) any later version.
*/
/*
;;;
*/
/*
;;;    See file '../Copyright' for full details.
*/
/*
;;;                    Exporting external symbols of LISP package
*/
/*
(si::select-package "CL")

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/export.lsp:510 **********************/
:-lisp_compile_to_prolog(pkg_user,['si::select-package','$STRING'("CL")])
:- f_sys_select_package('$ARRAY'([*], claz_base_character, "CL"), _Ignored).
/*
(export '(
	  &whole
	  &environment
	  &body
	  *
	  **
	  ***
	  *break-enable*
	  *break-on-warnings*
	  *features*
	  *modules*
	  +
	  ++
	  +++
	  -
	  /
	  //
	  ///
	  COMMON
	  KYOTO
	  KCL
	  ECL
	  abs
	  acos
	  acosh
	  adjust-array
	  adjustable-array-p
	  apropos
	  apropos-list
	  arglist
	  array-dimension
	  array-dimensions
	  array-element-type
	  array-has-fill-pointer-p
	  array-in-bounds-p
	  array-rank
	  array-row-major-index
	  asin
	  asinh
	  assert
	  atanh
	  bit
	  bit-and
	  bit-andc1
	  bit-andc2
	  bit-eqv
	  bit-ior
	  bit-nand
	  bit-nor
	  bit-not
	  bit-orc1
	  bit-orc2
	  bit-xor
	  break
	  byte
	  byte-position
	  byte-size
	  ccase
	  cerror
	  check-type
	  cis
	  coerce
	  compile
	  compile-file
	  concatenate
	  cosh
	  count
	  count-if
	  count-if-not
	  ctypecase
	  decf
	  decode-universal-time
	  defconstant
	  define-modify-macro
	  define-setf-method
	  defparameter
	  defsetf
	  defstruct
	  deftype
	  defvar
	  delete
	  delete-duplicates
	  delete-if
	  delete-if-not
	  deposit-field
	  describe
	  disassemble
	  do*
	  do-all-symbols
	  do-external-symbols
	  do-symbols
	  documentation
	  dolist
	  dotimes
	  dpb
	  dribble
	  ecase
	  ed
	  eighth
	  encode-universal-time
	  error
	  etypecase
	  eval-when
	  every
	  fceiling
	  ffloor
	  fifth
	  fill
	  fill-pointer
	  find
	  find-all-symbols
	  find-if
	  find-if-not
	  first
	  format
	  fourth
	  fround
	  ftruncate
	  get-decoded-time
	  get-setf-method
	  get-setf-method-multiple-value
	  get-universal-time
	  getf
	  ignore
	  incf
	  inspect
	  intersection
	  isqrt
	  ldb
	  ldb-test
	  lisp-implementation-type
	  logandc1
	  logandc2
	  lognand
	  lognor
	  lognot
	  logorc1
	  logorc2
	  logtest
	  long-site-name
	  loop
	  machine-instance
	  machine-type
	  machine-version
	  make-array
	  make-sequence
	  map
	  mask-field
	  merge
	  mismatch
	  mod
	  multiple-value-setq
	  nintersection
	  ninth
	  notany
	  notevery
	  nset-difference
	  nset-exclusive-or
	  nsubstitute
	  nsubstitute-if
	  nsubstitute-if-not
	  nunion
	  phase
	  pop
	  position
	  position-if
	  position-if-not
	  prin1-to-string
	  princ-to-string
	  prog*
	  provide
	  psetf
	  push
	  pushnew
	  rational
	  rationalize
	  read-from-string
	  reduce
	  rem
	  remf
	  remove
	  remove-duplicates
	  remove-if
	  remove-if-not
	  replace
	  require
	  rotatef
	  room
	  sbit
	  search
	  second
	  set-difference
	  set-exclusive-or
	  setf
	  seventh
	  shiftf
	  short-site-name
	  signum
	  sinh
	  sixth
	  software-type
	  software-version
	  some
	  sort
	  stable-sort
	  step
	  structure
	  subsetp
	  substitute
	  substitute-if
	  substitute-if-not
	  subtypep
	  tanh
	  tenth
	  third
	  time
	  trace
	  type
	  typecase
	  typep
	  union
	  untrace
	  variable
	  vector
	  vector-pop
	  vector-push
	  vector-push-extend
	  warn
	  with-input-from-string
	  with-open-file
	  with-open-stream
	  with-output-to-string
	  write-to-string
	  y-or-n-p
	  yes-or-no-p

	  proclaim
	  proclamation
	  special
	  type
	  ftype
	  function
	  inline
	  notinline
	  ignore
	  optimize
	  speed
	  space
	  safety
	  compilation-speed
	  declaration

	  *eval-when-compile*

	  clines
	  defcfun
	  defentry
	  defla
	  defcbody			; Beppe
	  definline			; Beppe
	  defunC			; Beppe
	  void
	  object
	  char*				; Beppe
	  char
	  int
	  float
	  double
	  ))

;;; ----------------------------------------------------------------------
;;;
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/export.lsp:538 **********************/
:-lisp_compile_to_prolog(pkg_cl,[export,[quote,['&whole','&environment','&body',*,**,***,'*break-enable*','*break-on-warnings*','*features*','*modules*',+,++,+++,-,/,//,///,'COMMON','KYOTO','KCL','ECL',abs,acos,acosh,'adjust-array','adjustable-array-p',apropos,'apropos-list',arglist,'array-dimension','array-dimensions','array-element-type','array-has-fill-pointer-p','array-in-bounds-p','array-rank','array-row-major-index',asin,asinh,assert,atanh,bit,'bit-and','bit-andc1','bit-andc2','bit-eqv','bit-ior','bit-nand','bit-nor','bit-not','bit-orc1','bit-orc2','bit-xor',break,byte,'byte-position','byte-size',ccase,cerror,'check-type',cis,coerce,compile,'compile-file',concatenate,cosh,count,'count-if','count-if-not',ctypecase,decf,'decode-universal-time',defconstant,'define-modify-macro','define-setf-method',defparameter,defsetf,defstruct,deftype,defvar,delete,'delete-duplicates','delete-if','delete-if-not','deposit-field',describe,disassemble,'do*','do-all-symbols','do-external-symbols','do-symbols',documentation,dolist,dotimes,dpb,dribble,ecase,ed,eighth,'encode-universal-time',error,etypecase,'eval-when',every,fceiling,ffloor,fifth,fill,'fill-pointer',find,'find-all-symbols','find-if','find-if-not',first,format,fourth,fround,ftruncate,'get-decoded-time','get-setf-method','get-setf-method-multiple-value','get-universal-time',getf,ignore,incf,inspect,intersection,isqrt,ldb,'ldb-test','lisp-implementation-type',logandc1,logandc2,lognand,lognor,lognot,logorc1,logorc2,logtest,'long-site-name',loop,'machine-instance','machine-type','machine-version','make-array','make-sequence',map,'mask-field',merge,mismatch,mod,'multiple-value-setq',nintersection,ninth,notany,notevery,'nset-difference','nset-exclusive-or',nsubstitute,'nsubstitute-if','nsubstitute-if-not',nunion,phase,pop,position,'position-if','position-if-not','prin1-to-string','princ-to-string','prog*',provide,psetf,push,pushnew,rational,rationalize,'read-from-string',reduce,rem,remf,remove,'remove-duplicates','remove-if','remove-if-not',replace,require,rotatef,room,sbit,search,second,'set-difference','set-exclusive-or',setf,seventh,shiftf,'short-site-name',signum,sinh,sixth,'software-type','software-version',some,sort,'stable-sort',step,structure,subsetp,substitute,'substitute-if','substitute-if-not',subtypep,tanh,tenth,third,time,trace,type,typecase,typep,union,untrace,variable,vector,'vector-pop','vector-push','vector-push-extend',warn,'with-input-from-string','with-open-file','with-open-stream','with-output-to-string','write-to-string','y-or-n-p','yes-or-no-p',proclaim,proclamation,special,type,ftype,function,inline,notinline,ignore,optimize,speed,space,safety,'compilation-speed',declaration,'*eval-when-compile*',clines,defcfun,defentry,defla,defcbody,definline,defunC,void,object,'char*',char,int,float,double]]])
:- f_export(
	    [ c38_whole,
	      c38_environment,
	      c38_body,
	      (*),
	      (**),
	      ***,
	      xx_break_enable_xx,
	      sys_xx_break_on_warnings_xx,
	      xx_features_xx,
	      xx_modules_xx,
	      (+),
	      ++,
	      +++,
	      (-),
	      (/),
	      (//),
	      ///,
	      common,
	      kyoto,
	      kcl,
	      ecl,
	      abs,
	      acos,
	      acosh,
	      adjust_array,
	      adjustable_array_p,
	      apropos,
	      apropos_list,
	      sys_arglist,
	      array_dimension,
	      array_dimensions,
	      array_element_type,
	      array_has_fill_pointer_p,
	      array_in_bounds_p,
	      array_rank,
	      array_row_major_index,
	      asin,
	      asinh,
	      assert,
	      atanh,
	      bit,
	      bit_and,
	      bit_andc1,
	      bit_andc2,
	      bit_eqv,
	      bit_ior,
	      bit_nand,
	      bit_nor,
	      bit_not,
	      bit_orc1,
	      bit_orc2,
	      bit_xor,
	      break,
	      byte,
	      byte_position,
	      byte_size,
	      ccase,
	      cerror,
	      check_type,
	      cis,
	      coerce,
	      compile,
	      compile_file,
	      concatenate,
	      cosh,
	      count,
	      count_if,
	      count_if_not,
	      ctypecase,
	      decf,
	      decode_universal_time,
	      defconstant,
	      define_modify_macro,
	      sys_define_setf_method,
	      defparameter,
	      defsetf,
	      defstruct,
	      deftype,
	      defvar,
	      delete,
	      delete_duplicates,
	      delete_if,
	      delete_if_not,
	      deposit_field,
	      describe,
	      disassemble,
	      do_xx,
	      do_all_symbols,
	      do_external_symbols,
	      do_symbols,
	      documentation,
	      dolist,
	      dotimes,
	      dpb,
	      dribble,
	      ecase,
	      ed,
	      eighth,
	      encode_universal_time,
	      error,
	      etypecase,
	      eval_when,
	      every,
	      fceiling,
	      ffloor,
	      fifth,
	      fill,
	      fill_pointer,
	      find,
	      find_all_symbols,
	      find_if,
	      find_if_not,
	      first,
	      format,
	      fourth,
	      fround,
	      ftruncate,
	      get_decoded_time,
	      sys_get_setf_method,
	      sys_get_setf_method_multiple_value,
	      get_universal_time,
	      getf,
	      ignore,
	      incf,
	      inspect,
	      intersection,
	      isqrt,
	      ldb,
	      ldb_test,
	      lisp_implementation_type,
	      logandc1,
	      logandc2,
	      lognand,
	      lognor,
	      lognot,
	      logorc1,
	      logorc2,
	      logtest,
	      long_site_name,
	      loop,
	      machine_instance,
	      machine_type,
	      machine_version,
	      make_array,
	      make_sequence,
	      map,
	      mask_field,
	      merge,
	      mismatch,
	      (mod),
	      multiple_value_setq,
	      nintersection,
	      ninth,
	      notany,
	      notevery,
	      nset_difference,
	      nset_exclusive_or,
	      nsubstitute,
	      nsubstitute_if,
	      nsubstitute_if_not,
	      nunion,
	      phase,
	      pop,
	      position,
	      position_if,
	      position_if_not,
	      prin1_to_string,
	      princ_to_string,
	      prog_xx,
	      provide,
	      psetf,
	      push,
	      pushnew,
	      rational,
	      rationalize,
	      read_from_string,
	      reduce,
	      (rem),
	      remf,
	      remove,
	      remove_duplicates,
	      remove_if,
	      remove_if_not,
	      replace,
	      require,
	      rotatef,
	      room,
	      sbit,
	      search,
	      second,
	      set_difference,
	      set_exclusive_or,
	      setf,
	      seventh,
	      shiftf,
	      short_site_name,
	      signum,
	      sinh,
	      sixth,
	      software_type,
	      software_version,
	      some,
	      sort,
	      stable_sort,
	      step,
	      structure,
	      subsetp,
	      substitute,
	      substitute_if,
	      substitute_if_not,
	      subtypep,
	      tanh,
	      tenth,
	      third,
	      time,
	      trace,
	      type,
	      typecase,
	      typep,
	      union,
	      untrace,
	      variable,
	      vector,
	      vector_pop,
	      vector_push,
	      vector_push_extend,
	      warn,
	      with_input_from_string,
	      with_open_file,
	      with_open_stream,
	      with_output_to_string,
	      write_to_string,
	      y_or_n_p,
	      yes_or_no_p,
	      proclaim,
	      sys_proclamation,
	      special,
	      type,
	      ftype,
	      function,
	      inline,
	      notinline,
	      ignore,
	      optimize,
	      speed,
	      space,
	      safety,
	      compilation_speed,
	      declaration,
	      xx_eval_when_compile_xx,
	      clines,
	      defcfun,
	      defentry,
	      defla,
	      defcbody,
	      definline,
	      defunc,
	      void,
	      object,
	      char_xx,
	      char,
	      int,
	      float,
	      double
	    ],
	    _Ignored).
/*
 Beppe
*/
/*
 Beppe
*/
/*
 Beppe
*/
/*
 Beppe
*/
/*
;; ----------------------------------------------------------------------
*/
/*
;;
*/
/*
(defun eval-feature (x)
  (cond ((symbolp x)
         (member x *features*
                 :test #'(lambda (a b)
                           (or (eql a b)
			       (and (symbolp a) (symbolp b)
				    (string-equal (symbol-name a)
						  (symbol-name b)))))))
	((atom x) (error ""(defun eval-feature (x)\n  (cond ((symbolp x)\n         (member x *features*\n                 :test #'(lambda (a b)\n                           (or (eql a b)\n\t\t\t       (and (symbolp a) (symbolp b)\n\t\t\t\t    (string-equal (symbol-name a)\n\t\t\t\t\t\t  (symbol-name b)))))))\n\t((atom x) (error \"~ is not allowed as a feature\" x))\n        ((eq (car x) 'AND)\n         (dolist (x (cdr x) t) (unless (eval-feature x) (return nil))))\n        ((eq (car x) 'OR)\n         (dolist (x (cdr x) nil) (when (eval-feature x) (return t))))\n        ((eq (car x) 'NOT)\n\t (not (eval-feature (second x))))\n\t(t (error \"~S is not a feature expression.\" x))))\n\n;;; Revised by G. Attardi\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/export.lsp:4025 **********************/
:-lisp_compile_to_prolog(pkg_cl,[defun,'eval-feature',[x],[cond,[[symbolp,x],[member,x,'*features*',':test',function([lambda,[a,b],[or,[eql,a,b],[and,[symbolp,a],[symbolp,b],['string-equal',['symbol-name',a],['symbol-name',b]]]]])]],[[atom,x],[error,'$STRING'("~ is not allowed as a feature"),x]],[[eq,[car,x],[quote,'AND']],[dolist,[x,[cdr,x],t],[unless,['eval-feature',x],[return,[]]]]],[[eq,[car,x],[quote,'OR']],[dolist,[x,[cdr,x],[]],[when,['eval-feature',x],[return,t]]]],[[eq,[car,x],[quote,'NOT']],[not,['eval-feature',[second,x]]]],[t,[error,'$STRING'("~S is not a feature expression."),x]]]])
wl:lambda_def(defun, eval_feature, f_eval_feature, [x], [[cond, [[symbolp, x], [member, x, xx_features_xx, kw_test, function([lambda, [a, b], [or, [eql, a, b], [and, [symbolp, a], [symbolp, b], [string_equal, [symbol_name, a], [symbol_name, b]]]]])]], [[atom, x], [error, '$ARRAY'([*], claz_base_character, "~ is not allowed as a feature"), x]], [[eq, [car, x], [quote, and]], [dolist, [x, [cdr, x], t], [unless, [eval_feature, x], [return, []]]]], [[eq, [car, x], [quote, or]], [dolist, [x, [cdr, x], []], [when, [eval_feature, x], [return, t]]]], [[eq, [car, x], [quote, not]], [not, [eval_feature, [second, x]]]], [t, [error, '$ARRAY'([*], claz_base_character, "~S is not a feature expression."), x]]]]).
wl:arglist_info(eval_feature, f_eval_feature, [x], arginfo{all:[x], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[x], opt:0, req:[x], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_eval_feature).

/*

### Compiled Function: `CL::EVAL-FEATURE` 
*/
f_eval_feature(X_In, FnResult) :-
	GEnv=[bv(x, X_In)],
	catch(( ( get_var(GEnv, x, X_Get),
		  (   is_symbolp(X_Get)
		  ->  get_var(GEnv, x, X_Get9),
		      get_var(GEnv, xx_features_xx, Xx_features_xx_Get),
		      f_member(X_Get9,
			       Xx_features_xx_Get,
			       
			       [ kw_test,
				 closure(kw_function,
					 [ClosureEnvironment|GEnv],
					 Whole,
					 LResult,
					 [a, b],
					 (get_var(ClosureEnvironment, a, A_Get), get_var(ClosureEnvironment, b, B_Get), f_eql(A_Get, B_Get, FORM1_Res), FORM1_Res\==[], LResult=FORM1_Res->true;get_var(ClosureEnvironment, a, A_Get14), (is_symbolp(A_Get14)->get_var(ClosureEnvironment, b, B_Get18), (is_symbolp(B_Get18)->get_var(ClosureEnvironment, a, A_Get21), f_symbol_name(A_Get21, String_equal_Param), get_var(ClosureEnvironment, b, B_Get22), f_symbol_name(B_Get22, Symbol_name_Ret), f_string_equal(String_equal_Param, Symbol_name_Ret, TrueResult), TrueResult24=TrueResult;TrueResult24=[]), _5202=TrueResult24;_5202=[]), LResult=_5202),
					 
					 [ lambda,
					   [a, b],
					   
					   [ or,
					     [eql, a, b],
					     
					     [ and,
					       [symbolp, a],
					       [symbolp, b],
					       
					       [ string_equal,
						 [symbol_name, a],
						 [symbol_name, b]
					       ]
					     ]
					   ]
					 ])
			       ],
			       TrueResult85),
		      _5022=TrueResult85
		  ;   get_var(GEnv, x, X_Get31),
		      (   X_Get31\=[CAR|CDR]
		      ->  get_var(GEnv, x, X_Get34),
			  f_error(
				  [ '$ARRAY'([*],
					     claz_base_character,
					     "~ is not allowed as a feature"),
				    X_Get34
				  ],
				  TrueResult83),
			  ElseResult86=TrueResult83
		      ;   get_var(GEnv, x, X_Get36),
			  f_car(X_Get36, PredArg1Result),
			  (   is_eq(PredArg1Result, and)
			  ->  LEnv=GEnv,
			      save_special(sv(t, [], symbol_value, Symbol_value)),
			      get_var(LEnv, x, X_Get42),
			      f_cdr(X_Get42, List),
			      BV=bv(x, Ele),
			      BlockExitEnv=[BV|LEnv],
			      forall(member(Ele, List),
				     ( nb_setarg(2, BV, Ele),
				       get_var(BlockExitEnv, x, X_Get45),
				       f_eval_feature(X_Get45, IFTEST43),
				       (   IFTEST43\==[]
				       ->  _6052=[]
				       ;   throw(block_exit([], [])),
					   _6052=ThrowResult
				       )
				     )),
			      restore_special(sv(t,
						 [],
						 symbol_value,
						 Symbol_value)),
			      ElseResult84=t
			  ;   get_var(GEnv, x, X_Get55),
			      f_car(X_Get55, PredArg1Result57),
			      (   is_eq(PredArg1Result57, or)
			      ->  LEnv60=[bv([], [])|GEnv],
				  get_var(LEnv60, x, X_Get61),
				  f_cdr(X_Get61, List72),
				  BV69=bv(x, Ele71),
				  BlockExitEnv67=[BV69|LEnv60],
				  forall(member(Ele71, List72),
					 ( nb_setarg(2, BV69, Ele71),
					   get_var(BlockExitEnv67, x, X_Get64),
					   f_eval_feature(X_Get64, IFTEST62),
					   (   IFTEST62\==[]
					   ->  throw(block_exit([], t)),
					       _6474=ThrowResult66
					   ;   _6474=[]
					   )
					 )),
				  ElseResult82=[]
			      ;   get_var(GEnv, x, X_Get74),
				  f_car(X_Get74, PredArg1Result76),
				  (   is_eq(PredArg1Result76, not)
				  ->  get_var(GEnv, x, X_Get77),
				      f_second(X_Get77, Eval_feature_Param),
				      f_eval_feature(Eval_feature_Param,
						     Not_Param),
				      f_not(Not_Param, TrueResult79),
				      ElseResult81=TrueResult79
				  ;   get_var(GEnv, x, X_Get78),
				      f_error(
					      [ '$ARRAY'([*],
							 claz_base_character,
							 "~S is not a feature expression."),
						X_Get78
					      ],
					      ElseResult80),
				      ElseResult81=ElseResult80
				  ),
				  ElseResult82=ElseResult81
			      ),
			      ElseResult84=ElseResult82
			  ),
			  ElseResult86=ElseResult84
		      ),
		      _5022=ElseResult86
		  )
		),
		_5022=FnResult
	      ),
	      block_exit(eval_feature, FnResult),
	      true).
:- set_opv(eval_feature, symbol_function, f_eval_feature),
   DefunResult=eval_feature.
/*
:- side_effect(assert_lsp(eval_feature,
			  lambda_def(defun,
				     eval_feature,
				     f_eval_feature,
				     [x],
				     
				     [ 
				       [ cond,
					 
					 [ [symbolp, x],
					   
					   [ member,
					     x,
					     xx_features_xx,
					     kw_test,
					     function(
						      [ lambda,
							[a, b],
							
							[ or,
							  [eql, a, b],
							  
							  [ and,
							    [symbolp, a],
							    [symbolp, b],
							    
							    [ string_equal,
							      [symbol_name, a],
							      [symbol_name, b]
							    ]
							  ]
							]
						      ])
					   ]
					 ],
					 
					 [ [atom, x],
					   
					   [ error,
					     '$ARRAY'([*],
						      claz_base_character,
						      "~ is not allowed as a feature"),
					     x
					   ]
					 ],
					 
					 [ [eq, [car, x], [quote, and]],
					   
					   [ dolist,
					     [x, [cdr, x], t],
					     
					     [ unless,
					       [eval_feature, x],
					       [return, []]
					     ]
					   ]
					 ],
					 
					 [ [eq, [car, x], [quote, or]],
					   
					   [ dolist,
					     [x, [cdr, x], []],
					     
					     [ when,
					       [eval_feature, x],
					       [return, t]
					     ]
					   ]
					 ],
					 
					 [ [eq, [car, x], [quote, not]],
					   [not, [eval_feature, [second, x]]]
					 ],
					 
					 [ t,
					   
					   [ error,
					     '$ARRAY'([*],
						      claz_base_character,
						      "~S is not a feature expression."),
					     x
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(eval_feature,
			  arglist_info(eval_feature,
				       f_eval_feature,
				       [x],
				       arginfo{ all:[x],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[x],
						opt:0,
						req:[x],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(eval_feature, init_args(x, f_eval_feature))).
*/
/*
;; Revised by G. Attardi
*/
/*
(defun check-no-infix (stream subchar arg)
  (when arg
    (error "Reading from "(defun check-no-infix (stream subchar arg)\n  (when arg\n    (error \"Reading from ~S: no number should appear between # and ~A\"\n\t   stream subchar)))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/export.lsp:4676 **********************/
:-lisp_compile_to_prolog(pkg_cl,[defun,'check-no-infix',[stream,subchar,arg],[when,arg,[error,'$STRING'("Reading from ~S: no number should appear between # and ~A"),stream,subchar]]])
wl:lambda_def(defun, check_no_infix, f_check_no_infix, [stream, subchar, arg], [[when, arg, [error, '$ARRAY'([*], claz_base_character, "Reading from ~S: no number should appear between # and ~A"), stream, subchar]]]).
wl:arglist_info(check_no_infix, f_check_no_infix, [stream, subchar, arg], arginfo{all:[stream, subchar, arg], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, subchar, arg], opt:0, req:[stream, subchar, arg], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_check_no_infix).

/*

### Compiled Function: `CL::CHECK-NO-INFIX` 
*/
f_check_no_infix(Stream_In, Subchar_In, Arg_In, FnResult) :-
	GEnv=[bv(stream, Stream_In), bv(subchar, Subchar_In), bv(arg, Arg_In)],
	catch(( ( get_var(GEnv, arg, IFTEST),
		  (   IFTEST\==[]
		  ->  get_var(GEnv, stream, Stream_Get),
		      get_var(GEnv, subchar, Subchar_Get),
		      f_error(
			      [ '$ARRAY'([*],
					 claz_base_character,
					 "Reading from ~S: no number should appear between # and ~A"),
				Stream_Get,
				Subchar_Get
			      ],
			      TrueResult),
		      _5788=TrueResult
		  ;   _5788=[]
		  )
		),
		_5788=FnResult
	      ),
	      block_exit(check_no_infix, FnResult),
	      true).
:- set_opv(check_no_infix, symbol_function, f_check_no_infix),
   DefunResult=check_no_infix.
/*
:- side_effect(assert_lsp(check_no_infix,
			  lambda_def(defun,
				     check_no_infix,
				     f_check_no_infix,
				     [stream, subchar, arg],
				     
				     [ 
				       [ when,
					 arg,
					 
					 [ error,
					   '$ARRAY'([*],
						    claz_base_character,
						    "Reading from ~S: no number should appear between # and ~A"),
					   stream,
					   subchar
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(check_no_infix,
			  arglist_info(check_no_infix,
				       f_check_no_infix,
				       [stream, subchar, arg],
				       arginfo{ all:[stream, subchar, arg],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[stream, subchar, arg],
						opt:0,
						req:[stream, subchar, arg],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(check_no_infix, init_args(x, f_check_no_infix))).
*/
/*
(defun sharp-+-reader (stream subchar arg)
  (check-no-infix stream subchar arg)
  (let ((feature (read stream t nil t)))
    (if (and (not *read-suppress*) (eval-feature feature))
	(read stream t nil t)
	(let ((*read-suppress* t)) (read stream t nil t) (values)))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/export.lsp:4825 **********************/
:-lisp_compile_to_prolog(pkg_cl,[defun,'sharp-+-reader',[stream,subchar,arg],['check-no-infix',stream,subchar,arg],[let,[[feature,[read,stream,t,[],t]]],[if,[and,[not,'*read-suppress*'],['eval-feature',feature]],[read,stream,t,[],t],[let,[['*read-suppress*',t]],[read,stream,t,[],t],[values]]]]])
wl:lambda_def(defun, sharp_c43_reader, f_sharp_c43_reader, [stream, subchar, arg], [[check_no_infix, stream, subchar, arg], [let, [[feature, [read, stream, t, [], t]]], [if, [and, [not, xx_read_suppress_xx], [eval_feature, feature]], [read, stream, t, [], t], [let, [[xx_read_suppress_xx, t]], [read, stream, t, [], t], [values]]]]]).
wl:arglist_info(sharp_c43_reader, f_sharp_c43_reader, [stream, subchar, arg], arginfo{all:[stream, subchar, arg], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, subchar, arg], opt:0, req:[stream, subchar, arg], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sharp_c43_reader).

/*

### Compiled Function: `CL::SHARP-+-READER` 
*/
f_sharp_c43_reader(Stream_In, Subchar_In, Arg_In, FnResult) :-
	GEnv=[bv(stream, Stream_In), bv(subchar, Subchar_In), bv(arg, Arg_In)],
	catch(( ( ( get_var(GEnv, arg, Arg_Get),
		    get_var(GEnv, stream, Stream_Get)
		  ),
		  get_var(GEnv, subchar, Subchar_Get),
		  f_check_no_infix(Stream_Get,
				   Subchar_Get,
				   Arg_Get,
				   No_infix_Ret),
		  get_var(GEnv, stream, Stream_Get13),
		  f_read(Stream_Get13, t, [], t, Feature_Init),
		  LEnv=[bv(feature, Feature_Init)|GEnv],
		  get_var(LEnv, xx_read_suppress_xx, Xx_read_suppress_xx_Get),
		  (   Xx_read_suppress_xx_Get==[]
		  ->  get_var(LEnv, feature, Feature_Get),
		      f_eval_feature(Feature_Get, TrueResult),
		      IFTEST=TrueResult
		  ;   IFTEST=[]
		  ),
		  (   IFTEST\==[]
		  ->  get_var(LEnv, stream, Stream_Get23),
		      f_read(Stream_Get23, t, [], t, TrueResult25),
		      LetResult=TrueResult25
		  ;   locally_set(xx_read_suppress_xx,
				  t,
				  (get_var(LEnv, stream, Stream_Get24), f_read(Stream_Get24, t, [], t, T), nb_setval('$mv_return', []))),
		      LetResult=[]
		  )
		),
		LetResult=FnResult
	      ),
	      block_exit(sharp_c43_reader, FnResult),
	      true).
:- set_opv(sharp_c43_reader, symbol_function, f_sharp_c43_reader),
   DefunResult=sharp_c43_reader.
/*
:- side_effect(assert_lsp(sharp_c43_reader,
			  lambda_def(defun,
				     sharp_c43_reader,
				     f_sharp_c43_reader,
				     [stream, subchar, arg],
				     
				     [ [check_no_infix, stream, subchar, arg],
				       
				       [ let,
					 [[feature, [read, stream, t, [], t]]],
					 
					 [ if,
					   
					   [ and,
					     [not, xx_read_suppress_xx],
					     [eval_feature, feature]
					   ],
					   [read, stream, t, [], t],
					   
					   [ let,
					     [[xx_read_suppress_xx, t]],
					     [read, stream, t, [], t],
					     [values]
					   ]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sharp_c43_reader,
			  arglist_info(sharp_c43_reader,
				       f_sharp_c43_reader,
				       [stream, subchar, arg],
				       arginfo{ all:[stream, subchar, arg],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[stream, subchar, arg],
						opt:0,
						req:[stream, subchar, arg],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sharp_c43_reader, init_args(x, f_sharp_c43_reader))).
*/
/*
(set-dispatch-macro-character #\# #\+ 'sharp-+-reader)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/export.lsp:5093 **********************/
:-lisp_compile_to_prolog(pkg_cl,['set-dispatch-macro-character',#\(#),#\(+),[quote,'sharp-+-reader']])
:- f_set_dispatch_macro_character(#\(#), #\(+), sharp_c43_reader, _Ignored).
/*
(set-dispatch-macro-character #\# #\+ 'sharp-+-reader
                              (sys::standard-readtable))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/export.lsp:5148 **********************/
:-lisp_compile_to_prolog(pkg_cl,['set-dispatch-macro-character',#\(#),#\(+),[quote,'sharp-+-reader'],['sys::standard-readtable']])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_standard_readtable,
					       kw_function,
					       f_sys_standard_readtable)).
*/
:- f_sys_standard_readtable(Sharp_c43_reader),
   f_set_dispatch_macro_character(#\(#),
				  #\(+),
				  sharp_c43_reader,
				  Sharp_c43_reader,
				  _Ignored).
/*
(defun sharp---reader (stream subchar arg)
  (check-no-infix stream subchar arg)
  (let ((feature (read stream t nil t)))
    (if (or *read-suppress* (eval-feature feature))
	(let ((*read-suppress* t)) (read stream t nil t) (values))
	(read stream t nil t))))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/export.lsp:5260 **********************/
:-lisp_compile_to_prolog(pkg_cl,[defun,'sharp---reader',[stream,subchar,arg],['check-no-infix',stream,subchar,arg],[let,[[feature,[read,stream,t,[],t]]],[if,[or,'*read-suppress*',['eval-feature',feature]],[let,[['*read-suppress*',t]],[read,stream,t,[],t],[values]],[read,stream,t,[],t]]]])
wl:lambda_def(defun, sharp_reader, f_sharp_reader, [stream, subchar, arg], [[check_no_infix, stream, subchar, arg], [let, [[feature, [read, stream, t, [], t]]], [if, [or, xx_read_suppress_xx, [eval_feature, feature]], [let, [[xx_read_suppress_xx, t]], [read, stream, t, [], t], [values]], [read, stream, t, [], t]]]]).
wl:arglist_info(sharp_reader, f_sharp_reader, [stream, subchar, arg], arginfo{all:[stream, subchar, arg], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, subchar, arg], opt:0, req:[stream, subchar, arg], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sharp_reader).

/*

### Compiled Function: `CL::SHARP---READER` 
*/
f_sharp_reader(Stream_In, Subchar_In, Arg_In, FnResult) :-
	GEnv=[bv(stream, Stream_In), bv(subchar, Subchar_In), bv(arg, Arg_In)],
	catch(( ( ( get_var(GEnv, arg, Arg_Get),
		    get_var(GEnv, stream, Stream_Get)
		  ),
		  get_var(GEnv, subchar, Subchar_Get),
		  f_check_no_infix(Stream_Get,
				   Subchar_Get,
				   Arg_Get,
				   No_infix_Ret),
		  get_var(GEnv, stream, Stream_Get13),
		  f_read(Stream_Get13, t, [], t, Feature_Init),
		  LEnv=[bv(feature, Feature_Init)|GEnv],
		  (   get_var(LEnv,
			      xx_read_suppress_xx,
			      Xx_read_suppress_xx_Get),
		      Xx_read_suppress_xx_Get\==[],
		      IFTEST=Xx_read_suppress_xx_Get
		  ->  true
		  ;   get_var(LEnv, feature, Feature_Get),
		      f_eval_feature(Feature_Get, Eval_feature_Ret),
		      IFTEST=Eval_feature_Ret
		  ),
		  (   IFTEST\==[]
		  ->  locally_set(xx_read_suppress_xx,
				  t,
				  (get_var(LEnv, stream, Stream_Get20), f_read(Stream_Get20, t, [], t, T), nb_setval('$mv_return', []))),
		      LetResult=[]
		  ;   get_var(LEnv, stream, Stream_Get21),
		      f_read(Stream_Get21, t, [], t, ElseResult),
		      LetResult=ElseResult
		  )
		),
		LetResult=FnResult
	      ),
	      block_exit(sharp_reader, FnResult),
	      true).
:- set_opv(sharp_reader, symbol_function, f_sharp_reader),
   DefunResult=sharp_reader.
/*
:- side_effect(assert_lsp(sharp_reader,
			  lambda_def(defun,
				     sharp_reader,
				     f_sharp_reader,
				     [stream, subchar, arg],
				     
				     [ [check_no_infix, stream, subchar, arg],
				       
				       [ let,
					 [[feature, [read, stream, t, [], t]]],
					 
					 [ if,
					   
					   [ or,
					     xx_read_suppress_xx,
					     [eval_feature, feature]
					   ],
					   
					   [ let,
					     [[xx_read_suppress_xx, t]],
					     [read, stream, t, [], t],
					     [values]
					   ],
					   [read, stream, t, [], t]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(sharp_reader,
			  arglist_info(sharp_reader,
				       f_sharp_reader,
				       [stream, subchar, arg],
				       arginfo{ all:[stream, subchar, arg],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:[stream, subchar, arg],
						opt:0,
						req:[stream, subchar, arg],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sharp_reader, init_args(x, f_sharp_reader))).
*/
/*
(set-dispatch-macro-character #\# #\- 'sharp---reader)
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/export.lsp:5521 **********************/
:-lisp_compile_to_prolog(pkg_cl,['set-dispatch-macro-character',#\(#),#\(-),[quote,'sharp---reader']])
:- f_set_dispatch_macro_character(#\(#), #\(-), sharp_reader, _Ignored).
/*
(set-dispatch-macro-character #\# #\- 'sharp---reader
                              (sys::standard-readtable))

;;; ----------------------------------------------------------------------

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/export.lsp:5576 **********************/
:-lisp_compile_to_prolog(pkg_cl,['set-dispatch-macro-character',#\(#),#\(-),[quote,'sharp---reader'],['sys::standard-readtable']])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_standard_readtable,
					       kw_function,
					       f_sys_standard_readtable)).
*/
:- f_sys_standard_readtable(Sharp_reader),
   f_set_dispatch_macro_character(#\(#),
				  #\(-),
				  sharp_reader,
				  Sharp_reader,
				  _Ignored).
/*
;; ----------------------------------------------------------------------
*/
/*
#+CLOS
(export '(
	  add-method
	  call-next-method
	  change-class
	  class-changed
	  class-name
	  class-of
	  defclass
	  defgeneric
	  define-method-combination
	  defmethod
	  ensure-generic-function
	  find-class
	  generic-flet
	  generic-function
	  generic-labels
	  get-method
	  initialize-instance
	  invalid-method-error
	  make-instance
	  make-instance-obsolete
	  make-method-call
	  method
	  method-combination-error
	  method-qualifiers
	  next-method-p
	  no-applicable-method
	  print-object
	  remove-method
	  slot-boundp
	  slot-exists-p
	  slot-makunbound
	  slot-missing
	  slot-unbound
	  slot-value
	  symbol-macrolet
	  update-instance-structure
	  with-accessors
	  with-added-methods
	  with-slots

	  class
	  built-in
	  standard-class
	  standard-generic-function
	  standard-method
	  standard-object
	  structure-class
	  structure-object
	  ))

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/export.lsp:5764 **********************/
:-lisp_compile_to_prolog(pkg_cl,'$COMMENT'([flag_removed,[+,':CLOS'],[export,[quote,['add-method','call-next-method','change-class','class-changed','class-name','class-of',defclass,defgeneric,'define-method-combination',defmethod,'ensure-generic-function','find-class','generic-flet','generic-function','generic-labels','get-method','initialize-instance','invalid-method-error','make-instance','make-instance-obsolete','make-method-call',method,'method-combination-error','method-qualifiers','next-method-p','no-applicable-method','print-object','remove-method','slot-boundp','slot-exists-p','slot-makunbound','slot-missing','slot-unbound','slot-value','symbol-macrolet','update-instance-structure','with-accessors','with-added-methods','with-slots',class,'built-in','standard-class','standard-generic-function','standard-method','standard-object','structure-class','structure-object']]]]))

%; Total compilation time: 2.459 seconds

