
:- style_check(-singleton).

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8170 **********************/
%:-lisp_compile_to_prolog(pkg_sys,['in-package','$STRING'("SYSTEM")]).
%:- cl_in_package('$ARRAY'([*], claz_base_character, "SYSTEM"), _Ignored).
wl:interned_eval_devel("
#+(or ABCL WAM-CL)
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
").

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8195 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'get-setf-method-inverse',[form,inverse,'setf-function'],[let,[['new-var',[gensym]],[vars,[]],[vals,[]]],[dolist,[x,[cdr,form]],[push,[gensym],vars],[push,x,vals]],[setq,vals,[nreverse,vals]],[values,vars,vals,[list,'new-var'],[if,'setf-function',['#BQ',[['#BQ-COMMA-ELIPSE',inverse],['#COMMA','new-var'],['#BQ-COMMA-ELIPSE',vars]]],[if,[functionp,[car,inverse]],['#BQ',[funcall,['#BQ-COMMA-ELIPSE',inverse],['#BQ-COMMA-ELIPSE',vars],['#COMMA','new-var']]],['#BQ',[['#BQ-COMMA-ELIPSE',inverse],['#BQ-COMMA-ELIPSE',vars],['#COMMA','new-var']]]]],['#BQ',[['#COMMA',[car,form]],['#BQ-COMMA-ELIPSE',vars]]]]]]).
wl:lambda_def(defun, sys_get_setf_method_inverse, f_sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], [[let, [[sys_new_var, [gensym]], [sys_vars, []], [sys_vals, []]], [dolist, [sys_x, [cdr, sys_form]], [push, [gensym], sys_vars], [push, sys_x, sys_vals]], [setq, sys_vals, [nreverse, sys_vals]], [values, sys_vars, sys_vals, [list, sys_new_var], [if, sys_setf_function, ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#COMMA', sys_new_var], ['#BQ-COMMA-ELIPSE', sys_vars]]], [if, [functionp, [car, sys_inverse]], ['#BQ', [funcall, ['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]], ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]]]], ['#BQ', [['#COMMA', [car, sys_form]], ['#BQ-COMMA-ELIPSE', sys_vars]]]]]]).
wl:arglist_info(sys_get_setf_method_inverse, f_sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], arginfo{all:[sys_form, sys_inverse, sys_setf_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_inverse, sys_setf_function], opt:0, req:[sys_form, sys_inverse, sys_setf_function], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_get_setf_method_inverse).

/*

### Compiled:  `SYS::GET-SETF-METHOD-INVERSE` 
*/
f_sys_get_setf_method_inverse(Form_In, Inverse_In, Setf_function_In, FnResult) :-
	Env43=[bv(sys_form, Form_In), bv(sys_inverse, Inverse_In), bv(sys_setf_function, Setf_function_In)|Env],
	global_env(Env),
	catch(( cl_gensym(New_var_Init),
		LEnv=[bv(sys_new_var, New_var_Init), bv(sys_vars, []), bv(sys_vals, [])|Env43],
		get_var(LEnv, sys_form, Form_Get),
		cl_cdr(Form_Get, List),
		BV=bv(sys_x, Ele),
		Env2=[BV|LEnv],
		forall(member(Ele, List),
		       ( nb_setarg(2, BV, Ele),
			 cl_push([gensym], sys_vars, Vars),
			 cl_push(sys_x, sys_vals, Vals)
		       )),
		get_var(LEnv, sys_vals, Vals_Get),
		cl_nreverse(Vals_Get, Vals46),
		set_var(LEnv, sys_vals, Vals46),
		get_var(LEnv, sys_new_var, New_var_Get),
		get_var(LEnv, sys_vals, Vals_Get17),
		CAR51=New_var_Get,
		get_var(LEnv, sys_setf_function, IFTEST),
		(   IFTEST\==[]
		->  get_var(LEnv, sys_inverse, Inverse_Get),
		    get_var(LEnv, sys_new_var, New_var_Get23),
		    get_var(LEnv, sys_vars, Vars_Get),
		    bq_append(Inverse_Get,
			      [New_var_Get23|Vars_Get],
			      TrueResult37),
		    CAR=TrueResult37
		;   get_var(LEnv, sys_inverse, Inverse_Get26),
		    cl_car(Inverse_Get26, PredArgResult),
		    (   is_functionp(PredArgResult)
		    ->  get_var(LEnv, sys_inverse, Inverse_Get29),
			get_var(LEnv, sys_new_var, New_var_Get31),
			get_var(LEnv, sys_vars, Vars_Get30),
			bq_append(Vars_Get30, [New_var_Get31], Bq_append_Ret),
			bq_append([funcall|Inverse_Get29],
				  Bq_append_Ret,
				  TrueResult),
			ElseResult38=TrueResult
		    ;   get_var(LEnv, sys_inverse, Inverse_Get32),
			get_var(LEnv, sys_new_var, New_var_Get34),
			get_var(LEnv, sys_vars, Vars_Get33),
			bq_append(Vars_Get33, [New_var_Get34], Bq_append_Ret48),
			bq_append(Inverse_Get32, Bq_append_Ret48, ElseResult),
			ElseResult38=ElseResult
		    ),
		    CAR=ElseResult38
		),
		get_var(LEnv, sys_form, Form_Get39),
		cl_car(Form_Get39, Car_Ret),
		get_var(LEnv, sys_vars, Vars_Get40),
		nb_setval('$mv_return',
			  [sys_vars, Vals_Get17, CAR51, CAR, [Car_Ret|Vars_Get40]]),
		sys_vars=FnResult
	      ),
	      block_exit(sys_get_setf_method_inverse, FnResult),
	      true).
:- set_opv(f_sys_get_setf_method_inverse, classof, claz_function),
   set_opv(sys_get_setf_method_inverse, compile_as, kw_function),
   set_opv(sys_get_setf_method_inverse, function, f_sys_get_setf_method_inverse),
   DefunResult=sys_get_setf_method_inverse.

wl:interned_eval_devel("
;; If a macro, expand one level and try again.  If not, go for the
;; SETF function.
#+(or ABCL WAM-CL)
(defun expand-or-get-setf-inverse (form environment)
  (multiple-value-bind (expansion expanded)
      (macroexpand-1 form environment)
    (if expanded
        (get-setf-expansion expansion environment)
        (get-setf-method-inverse form `(funcall #'(setf ,(car form)))
                                 t))))

").

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8857 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'expand-or-get-setf-inverse',[form,environment],['multiple-value-bind',[expansion,expanded],['macroexpand-1',form,environment],[if,expanded,['get-setf-expansion',expansion,environment],['get-setf-method-inverse',form,['#BQ',[funcall,function([setf,['#COMMA',[car,form]]])]],t]]]]).
wl:lambda_def(defun, sys_expand_or_get_setf_inverse, f_sys_expand_or_get_setf_inverse, [sys_form, sys_environment], [[multiple_value_bind, [sys_expansion, sys_expanded], [macroexpand_1, sys_form, sys_environment], [if, sys_expanded, [get_setf_expansion, sys_expansion, sys_environment], [sys_get_setf_method_inverse, sys_form, ['#BQ', [funcall, function([setf, ['#COMMA', [car, sys_form]]])]], t]]]]).
wl:arglist_info(sys_expand_or_get_setf_inverse, f_sys_expand_or_get_setf_inverse, [sys_form, sys_environment], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:0, req:[sys_form, sys_environment], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_expand_or_get_setf_inverse).

/*

### Compiled:  `SYS::EXPAND-OR-GET-SETF-INVERSE` 
*/
f_sys_expand_or_get_setf_inverse(Form_In, Environment_In, FnResult) :-
	Env20=[bv(sys_form, Form_In), bv(sys_environment, Environment_In)|Env],
	global_env(Env),
	catch(( LEnv=[bv(sys_expansion, []), bv(sys_expanded, [])|Env20],
		get_var(LEnv, sys_environment, Environment_Get),
		get_var(LEnv, sys_form, Form_Get),
		cl_macroexpand_1([Form_Get, Environment_Get], Macroexpand_1_Ret),
		setq_from_values(LEnv, [sys_expansion, sys_expanded]),
		get_var(LEnv, sys_expanded, IFTEST),
		(   IFTEST\==[]
		->  get_var(LEnv, sys_environment, Environment_Get14),
		    get_var(LEnv, sys_expansion, Expansion_Get),
		    cl_get_setf_expansion(Expansion_Get,
					  Environment_Get14,
					  TrueResult),
		    LetResult=TrueResult
		;   get_var(LEnv, sys_form, Form_Get15),
		    f_sys_get_setf_method_inverse(Form_Get15,
						  
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
		    LetResult=ElseResult
		),
		LetResult=FnResult
	      ),
	      block_exit(sys_expand_or_get_setf_inverse, FnResult),
	      true).
:- set_opv(f_sys_expand_or_get_setf_inverse, classof, claz_function),
   set_opv(sys_expand_or_get_setf_inverse, compile_as, kw_function),
   set_opv(sys_expand_or_get_setf_inverse,
	   function,
	   f_sys_expand_or_get_setf_inverse),
   DefunResult=sys_expand_or_get_setf_inverse.

wl:interned_eval_devel("
#+(or ABCL WAM-CL)
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
").

wl:interned_eval_orig_alt("
(defun get-setf-expansion (form &optional env)
  (declare (optimize (safety 2)))
  (get-setf-method form env))

").

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:9199 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'get-setf-expansion',[form,'&optional',environment],[let,[temp],[cond,[[symbolp,form],['multiple-value-bind',[expansion,expanded],['macroexpand-1',form,environment],[if,expanded,['get-setf-expansion',expansion,environment],[let,[['new-var',[gensym]]],[values,[],[],[list,'new-var'],['#BQ',[setq,['#COMMA',form],['#COMMA','new-var']]],form]]]]],[[setq,temp,[get,[car,form],[quote,'setf-inverse']]],['get-setf-method-inverse',form,['#BQ',[['#COMMA',temp]]],[]]],[[setq,temp,[get,[car,form],[quote,'setf-expander']]],[funcall,temp,form,environment]],[t,['expand-or-get-setf-inverse',form,environment]]]]]).
wl:lambda_def(defun, get_setf_expansion, cl_get_setf_expansion, [sys_form, c38_optional, sys_environment], [[let, [sys_temp], [cond, [[symbolp, sys_form], [multiple_value_bind, [sys_expansion, sys_expanded], [macroexpand_1, sys_form, sys_environment], [if, sys_expanded, [get_setf_expansion, sys_expansion, sys_environment], [let, [[sys_new_var, [gensym]]], [values, [], [], [list, sys_new_var], ['#BQ', [setq, ['#COMMA', sys_form], ['#COMMA', sys_new_var]]], sys_form]]]]], [[setq, sys_temp, [get, [car, sys_form], [quote, sys_setf_inverse]]], [sys_get_setf_method_inverse, sys_form, ['#BQ', [['#COMMA', sys_temp]]], []]], [[setq, sys_temp, [get, [car, sys_form], [quote, sys_setf_expander]]], [funcall, sys_temp, sys_form, sys_environment]], [t, [sys_expand_or_get_setf_inverse, sys_form, sys_environment]]]]]).
wl:arglist_info(get_setf_expansion, cl_get_setf_expansion, [sys_form, c38_optional, sys_environment], arginfo{all:[sys_form, sys_environment], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_environment], opt:[sys_environment], req:[sys_form], rest:0, sublists:0, whole:0}).
wl: init_args(1,get_setf_expansion).

/*

### Compiled:  `CL:GET-SETF-EXPANSION` 
*/
cl_get_setf_expansion(Form_In, RestNKeys, FnResult) :-
	Env55=[bv(sys_form, Form_In), bv(sys_environment, Environment_In)|Env],
	global_env(Env),
	opt_var(Env, sys_environment, Environment_In, true, [], 1, RestNKeys),
	catch(( LEnv=[bv(sys_temp, [])|Env55],
		get_var(LEnv, sys_form, Form_Get),
		(   is_symbolp(Form_Get)
		->  LEnv16=[bv(sys_expansion, []), bv(sys_expanded, [])|LEnv],
		    get_var(LEnv16, sys_environment, Environment_Get),
		    get_var(LEnv16, sys_form, Form_Get17),
		    cl_macroexpand_1([Form_Get17, Environment_Get],
				     Macroexpand_1_Ret),
		    setq_from_values(LEnv16, [sys_expansion, sys_expanded]),
		    get_var(LEnv16, sys_expanded, IFTEST19),
		    (   IFTEST19\==[]
		    ->  get_var(LEnv16, sys_environment, Environment_Get23),
			get_var(LEnv16, sys_expansion, Expansion_Get),
			cl_get_setf_expansion(Expansion_Get,
					      Environment_Get23,
					      TrueResult),
			LetResult15=TrueResult
		    ;   cl_gensym(New_var_Init),
			LEnv26=[bv(sys_new_var, New_var_Init)|LEnv16],
			get_var(LEnv26, sys_new_var, New_var_Get),
			CAR=New_var_Get,
			get_var(LEnv26, sys_form, Form_Get29),
			get_var(LEnv26, sys_form, Form_Get31),
			get_var(LEnv26, sys_new_var, New_var_Get30),
			nb_setval('$mv_return',
				  
				  [ [],
				    [],
				    CAR,
				    [setq, Form_Get29, New_var_Get30],
				    Form_Get31
				  ]),
			LetResult15=[]
		    ),
		    LetResult=LetResult15
		;   get_var(LEnv, sys_form, Form_Get36),
		    cl_car(Form_Get36, Get_Param),
		    cl_get(Get_Param, sys_setf_inverse, [], IFTEST33),
		    set_var(LEnv, sys_temp, IFTEST33),
		    (   IFTEST33\==[]
		    ->  get_var(LEnv, sys_form, Form_Get37),
			get_var(LEnv, sys_temp, Temp_Get),
			f_sys_get_setf_method_inverse(Form_Get37,
						      [Temp_Get],
						      [],
						      TrueResult49),
			ElseResult52=TrueResult49
		    ;   get_var(LEnv, sys_form, Form_Get41),
			cl_car(Form_Get41, Get_Param57),
			cl_get(Get_Param57, sys_setf_expander, [], IFTEST39),
			set_var(LEnv, sys_temp, IFTEST39),
			(   IFTEST39\==[]
			->  get_var(LEnv, sys_form, Form_Get43),
			    get_var(LEnv, sys_temp, Temp_Get42),
			    get_var(LEnv, sys_environment, Environment_Get44),
			    cl_apply(Temp_Get42,
				     [Form_Get43, Environment_Get44],
				     TrueResult47),
			    ElseResult50=TrueResult47
			;   get_var(LEnv, sys_environment, Environment_Get46),
			    get_var(LEnv, sys_form, Form_Get45),
			    f_sys_expand_or_get_setf_inverse(Form_Get45,
							     Environment_Get46,
							     ElseResult),
			    ElseResult50=ElseResult
			),
			ElseResult52=ElseResult50
		    ),
		    LetResult=ElseResult52
		),
		LetResult=FnResult
	      ),
	      block_exit(get_setf_expansion, FnResult),
	      true).
:- set_opv(cl_get_setf_expansion, classof, claz_function),
   set_opv(get_setf_expansion, compile_as, kw_function),
   set_opv(get_setf_expansion, function, cl_get_setf_expansion),
   DefunResult=get_setf_expansion.

wl:interned_eval_devel("
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
").

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:3312 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,shiftf,['&rest','places-and-newvalue','&environment',env],[let,[[nargs,[length,'places-and-newvalue']]],[assert,[>=,nargs,2]],[let,[[place,[car,'places-and-newvalue']]],['multiple-value-bind',[temps,vars,newvals,setter,getter],['get-setf-expansion',place,env],['#BQ',[let,[['#BQ-COMMA-ELIPSE',[mapcar,function(list),temps,vars]]],['multiple-value-prog1',['#COMMA',getter],['multiple-value-bind',['#COMMA',newvals],['#COMMA',[if,[=,nargs,2],[cadr,'places-and-newvalue'],['#BQ',[shiftf,['#BQ-COMMA-ELIPSE',[cdr,'places-and-newvalue']]]]]],['#COMMA',setter]]]]]]]]]).
wl:lambda_def(defmacro, shiftf, cl_shiftf, [c38_rest, sys_places_and_newvalue, c38_environment, env], [progn, [let, [[sys_nargs, [length, sys_places_and_newvalue]]], [assert, [>=, sys_nargs, 2]], [let, [[sys_place, [car, sys_places_and_newvalue]]], [multiple_value_bind, [sys_temps, sys_vars, sys_newvals, sys_setter, sys_getter], [get_setf_expansion, sys_place, env], ['#BQ', [let, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_temps, sys_vars]]], [multiple_value_prog1, ['#COMMA', sys_getter], [multiple_value_bind, ['#COMMA', sys_newvals], ['#COMMA', [if, [=, sys_nargs, 2], [cadr, sys_places_and_newvalue], ['#BQ', [shiftf, ['#BQ-COMMA-ELIPSE', [cdr, sys_places_and_newvalue]]]]]], ['#COMMA', sys_setter]]]]]]]]]).
wl: declared(cl_shiftf, env_arg1).

wl:arglist_info(shiftf, cl_shiftf, [c38_rest, sys_places_and_newvalue, c38_environment, env], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[env], key:0, names:[sys_places_and_newvalue, env], opt:0, req:0, rest:[sys_places_and_newvalue], sublists:0, whole:0}).
wl: init_args(0,shiftf).

/*

### Compiled:  `CL:SHIFTF` 
*/
cl_shiftf(RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_places_and_newvalue, RestNKeys), bv(env, Env_In)|CDR],
	global_env(CDR),
	parent_env(Env_In),
	get_var(Env, sys_places_and_newvalue, Places_and_newvalue_Get),
	cl_length(Places_and_newvalue_Get, Nargs_Init),
	LEnv=[bv(sys_nargs, Nargs_Init)|Env],
	cl_assert([>=, sys_nargs, 2], Assert_Ret),
	get_var(LEnv, sys_places_and_newvalue, Places_and_newvalue_Get17),
	cl_car(Places_and_newvalue_Get17, Place_Init),
	LEnv16=[bv(sys_place, Place_Init)|LEnv],
	LEnv21=[bv(sys_temps, []), bv(sys_vars, []), bv(sys_newvals, []), bv(sys_setter, []), bv(sys_getter, [])|LEnv16],
	get_var(LEnv21, env, Env_Get),
	get_var(LEnv21, sys_place, Place_Get),
	cl_get_setf_expansion(Place_Get, Env_Get, Setf_expansion_Ret),
	setq_from_values(LEnv21,
			 
			 [ sys_temps,
			   sys_vars,
			   sys_newvals,
			   sys_setter,
			   sys_getter
			 ]),
	get_var(LEnv21, sys_temps, Temps_Get),
	get_var(LEnv21, sys_vars, Vars_Get),
	cl_mapcar(cl_list, [Temps_Get, Vars_Get], Mapcar_Ret),
	get_var(LEnv21, sys_getter, Getter_Get),
	get_var(LEnv21, sys_nargs, Nargs_Get),
	get_var(LEnv21, sys_newvals, Newvals_Get),
	(   Nargs_Get=:=2
	->  get_var(LEnv21, sys_places_and_newvalue, Places_and_newvalue_Get32),
	    cl_cadr(Places_and_newvalue_Get32, TrueResult),
	    CAR=TrueResult
	;   get_var(LEnv21, sys_places_and_newvalue, Places_and_newvalue_Get33),
	    cl_cdr(Places_and_newvalue_Get33, Cdr_Ret),
	    CAR=[shiftf|Cdr_Ret]
	),
	get_var(LEnv21, sys_setter, Setter_Get),
	[let, Mapcar_Ret, [multiple_value_prog1, Getter_Get, [multiple_value_bind, Newvals_Get, CAR, Setter_Get]]]=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_shiftf, classof, claz_macro),
   set_opv(shiftf, compile_as, kw_operator),
   set_opv(shiftf, function, cl_shiftf),
   DefMacroResult=shiftf.

wl:interned_eval_devel("
(defmacro rotatef (&rest places &environment env)
             (if (< (length places) 2)
                 nil
               (multiple-value-bind (temps vars newvals setter getter)
                   (get-setf-expansion (car places) env)
                 `(let (,@(mapcar #'list temps vars))
                    (multiple-value-bind ,newvals (shiftf ,@(cdr places) ,getter)
                      ,setter)
                    nil))))
").

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:4028 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,rotatef,['&rest',places,'&environment',env],[if,[<,[length,places],2],[],['multiple-value-bind',[temps,vars,newvals,setter,getter],['get-setf-expansion',[car,places],env],['#BQ',[let,[['#BQ-COMMA-ELIPSE',[mapcar,function(list),temps,vars]]],['multiple-value-bind',['#COMMA',newvals],[shiftf,['#BQ-COMMA-ELIPSE',[cdr,places]],['#COMMA',getter]],['#COMMA',setter]],[]]]]]]).
wl:lambda_def(defmacro, rotatef, cl_rotatef, [c38_rest, sys_places, c38_environment, env], [progn, [if, [<, [length, sys_places], 2], [], [multiple_value_bind, [sys_temps, sys_vars, sys_newvals, sys_setter, sys_getter], [get_setf_expansion, [car, sys_places], env], ['#BQ', [let, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_temps, sys_vars]]], [multiple_value_bind, ['#COMMA', sys_newvals], [shiftf, ['#BQ-COMMA-ELIPSE', [cdr, sys_places]], ['#COMMA', sys_getter]], ['#COMMA', sys_setter]], []]]]]]).
wl: declared(cl_rotatef, env_arg1).

wl:arglist_info(rotatef, cl_rotatef, [c38_rest, sys_places, c38_environment, env], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[env], key:0, names:[sys_places, env], opt:0, req:0, rest:[sys_places], sublists:0, whole:0}).
wl: init_args(0,rotatef).

/*

### Compiled:  `CL:ROTATEF` 
*/
cl_rotatef(RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_places, RestNKeys), bv(env, Env_In)|CDR],
	global_env(CDR),
	parent_env(Env_In),
	get_var(Env, sys_places, Places_Get),
	cl_length(Places_Get, PredArg1Result),
	(   PredArg1Result<2
	->  _20912602=[]
	;   LEnv=[bv(sys_temps, []), bv(sys_vars, []), bv(sys_newvals, []), bv(sys_setter, []), bv(sys_getter, [])|Env],
	    get_var(LEnv, sys_places, Places_Get16),
	    cl_car(Places_Get16, Setf_expansion_Param),
	    get_var(LEnv, env, Env_Get),
	    cl_get_setf_expansion(Setf_expansion_Param,
				  Env_Get,
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
	    cl_mapcar(cl_list, [Temps_Get, Vars_Get], Mapcar_Ret),
	    get_var(LEnv, sys_newvals, Newvals_Get),
	    get_var(LEnv, sys_places, Places_Get21),
	    cl_cdr(Places_Get21, Cdr_Ret),
	    get_var(LEnv, sys_getter, Getter_Get),
	    bq_append([shiftf|Cdr_Ret], [Getter_Get], Bq_append_Ret),
	    get_var(LEnv, sys_setter, Setter_Get),
	    _20912602=[let, Mapcar_Ret, [multiple_value_bind, Newvals_Get, Bq_append_Ret, Setter_Get], []]
	),
	_20912602=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_rotatef, classof, claz_macro),
   set_opv(rotatef, compile_as, kw_operator),
   set_opv(rotatef, function, cl_rotatef),
   DefMacroResult=rotatef.



wl:interned_eval_devel("
; Adapted from SBCL.
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


").

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:4485 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,push,['&environment',env,item,place],[if,[and,[symbolp,place],[eq,place,[macroexpand,place,env]]],['#BQ',[setq,['#COMMA',place],[cons,['#COMMA',item],['#COMMA',place]]]],['multiple-value-bind',[dummies,vals,newval,setter,getter],['get-setf-expansion',place,env],[let,[[g,[gensym]]],['#BQ',['let*',[[['#COMMA',g],['#COMMA',item]],['#BQ-COMMA-ELIPSE',[mapcar,function(list),dummies,vals]],[['#COMMA',[car,newval]],[cons,['#COMMA',g],['#COMMA',getter]]]],['#COMMA',setter]]]]]]]).
wl:lambda_def(defmacro, push, cl_push, [c38_environment, env, item, sys_place], [progn, [if, [and, [symbolp, sys_place], [eq, sys_place, [macroexpand, sys_place, env]]], ['#BQ', [setq, ['#COMMA', sys_place], [cons, ['#COMMA', item], ['#COMMA', sys_place]]]], [multiple_value_bind, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter], [get_setf_expansion, sys_place, env], [let, [[sys_g, [gensym]]], ['#BQ', [let_xx, [[['#COMMA', sys_g], ['#COMMA', item]], ['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]], [['#COMMA', [car, sys_newval]], [cons, ['#COMMA', sys_g], ['#COMMA', sys_getter]]]], ['#COMMA', sys_setter]]]]]]]).
wl: declared(cl_push, env_arg1).

wl:arglist_info(push, cl_push, [c38_environment, env, item, sys_place], arginfo{all:[item, sys_place], allow_other_keys:0, aux:0, body:0, complex:[environment], env:[env], key:0, names:[env, item, sys_place], opt:0, req:[item, sys_place], rest:0, sublists:0, whole:0}).
wl: init_args(2,push).

/*

### Compiled:  `CL:PUSH` 
*/
cl_push(Item_In, Place_In, RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(env, Env_In), bv(item, Item_In), bv(sys_place, Place_In)|CDR],
	global_env(CDR),
	parent_env(Env_In),
	get_var(Env, sys_place, Place_Get),
	(   is_symbolp(Place_Get)
	->  get_var(Env, env, Env_Get),
	    get_var(Env, sys_place, Place_Get16),
	    cl_macroexpand([Place_Get16, Env_Get], Macroexpand_Ret),
	    cl_eq(Place_Get16, Macroexpand_Ret, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(Env, item, Item_Get),
	    get_var(Env, sys_place, Place_Get20),
	    _22831648=[setq, Place_Get20, [cons, Item_Get, Place_Get20]]
	;   LEnv=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_newval, []), bv(sys_setter, []), bv(sys_getter, [])|Env],
	    get_var(LEnv, env, Env_Get27),
	    get_var(LEnv, sys_place, Place_Get26),
	    cl_get_setf_expansion(Place_Get26, Env_Get27, Setf_expansion_Ret),
	    setq_from_values(LEnv,
			     
			     [ sys_dummies,
			       sys_vals,
			       sys_newval,
			       sys_setter,
			       sys_getter
			     ]),
	    cl_gensym(G_Init),
	    LEnv30=[bv(sys_g, G_Init)|LEnv],
	    get_var(LEnv30, item, Item_Get33),
	    get_var(LEnv30, sys_dummies, Dummies_Get),
	    get_var(LEnv30, sys_g, G_Get),
	    get_var(LEnv30, sys_vals, Vals_Get),
	    cl_mapcar(cl_list, [Dummies_Get, Vals_Get], Mapcar_Ret),
	    get_var(LEnv30, sys_newval, Newval_Get),
	    cl_car(Newval_Get, Car_Ret),
	    get_var(LEnv30, sys_g, G_Get37),
	    get_var(LEnv30, sys_getter, Getter_Get),
	    bq_append([[G_Get, Item_Get33]|Mapcar_Ret],
		      [[Car_Ret, [cons, G_Get37, Getter_Get]]],
		      Bq_append_Ret),
	    get_var(LEnv30, sys_setter, Setter_Get),
	    _22831648=[let_xx, Bq_append_Ret, Setter_Get]
	),
	_22831648=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_push, classof, claz_macro),
   set_opv(push, compile_as, kw_operator),
   set_opv(push, function, cl_push),
   DefMacroResult=push.

wl:interned_eval_devel("
; Adapted from SBCL.
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

").

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:4948 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,pushnew,['&environment',env,item,place,'&rest',keys],[if,[and,[symbolp,place],[eq,place,[macroexpand,place,env]]],['#BQ',[setq,['#COMMA',place],[adjoin,['#COMMA',item],['#COMMA',place],['#BQ-COMMA-ELIPSE',keys]]]],['multiple-value-bind',[dummies,vals,newval,setter,getter],['get-setf-expansion',place,env],[let,[[g,[gensym]]],['#BQ',['let*',[[['#COMMA',g],['#COMMA',item]],['#BQ-COMMA-ELIPSE',[mapcar,function(list),dummies,vals]],[['#COMMA',[car,newval]],[adjoin,['#COMMA',g],['#COMMA',getter],['#BQ-COMMA-ELIPSE',keys]]]],['#COMMA',setter]]]]]]]).
wl:lambda_def(defmacro, pushnew, cl_pushnew, [c38_environment, env, item, sys_place, c38_rest, sys_keys], [progn, [if, [and, [symbolp, sys_place], [eq, sys_place, [macroexpand, sys_place, env]]], ['#BQ', [setq, ['#COMMA', sys_place], [adjoin, ['#COMMA', item], ['#COMMA', sys_place], ['#BQ-COMMA-ELIPSE', sys_keys]]]], [multiple_value_bind, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter], [get_setf_expansion, sys_place, env], [let, [[sys_g, [gensym]]], ['#BQ', [let_xx, [[['#COMMA', sys_g], ['#COMMA', item]], ['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]], [['#COMMA', [car, sys_newval]], [adjoin, ['#COMMA', sys_g], ['#COMMA', sys_getter], ['#BQ-COMMA-ELIPSE', sys_keys]]]], ['#COMMA', sys_setter]]]]]]]).
wl: declared(cl_pushnew, env_arg1).

wl:arglist_info(pushnew, cl_pushnew, [c38_environment, env, item, sys_place, c38_rest, sys_keys], arginfo{all:[item, sys_place], allow_other_keys:0, aux:0, body:0, complex:[environment, rest], env:[env], key:0, names:[env, item, sys_place, sys_keys], opt:0, req:[item, sys_place], rest:[sys_keys], sublists:0, whole:0}).
wl: init_args(2,pushnew).

/*

### Compiled:  `CL:PUSHNEW` 
*/
cl_pushnew(Item_In, Place_In, RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(env, Env_In), bv(item, Item_In), bv(sys_place, Place_In), bv(sys_keys, RestNKeys)|CDR],
	global_env(CDR),
	parent_env(Env_In),
	get_var(Env, sys_place, Place_Get),
	(   is_symbolp(Place_Get)
	->  get_var(Env, env, Env_Get),
	    get_var(Env, sys_place, Place_Get17),
	    cl_macroexpand([Place_Get17, Env_Get], Macroexpand_Ret),
	    cl_eq(Place_Get17, Macroexpand_Ret, TrueResult),
	    IFTEST=TrueResult
	;   IFTEST=[]
	),
	(   IFTEST\==[]
	->  get_var(Env, item, Item_Get),
	    get_var(Env, sys_keys, Keys_Get),
	    get_var(Env, sys_place, Place_Get21),
	    _25518664=[setq, Place_Get21, [adjoin, Item_Get, Place_Get21|Keys_Get]]
	;   LEnv=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_newval, []), bv(sys_setter, []), bv(sys_getter, [])|Env],
	    get_var(LEnv, env, Env_Get29),
	    get_var(LEnv, sys_place, Place_Get28),
	    cl_get_setf_expansion(Place_Get28, Env_Get29, Setf_expansion_Ret),
	    setq_from_values(LEnv,
			     
			     [ sys_dummies,
			       sys_vals,
			       sys_newval,
			       sys_setter,
			       sys_getter
			     ]),
	    cl_gensym(G_Init),
	    LEnv32=[bv(sys_g, G_Init)|LEnv],
	    get_var(LEnv32, item, Item_Get35),
	    get_var(LEnv32, sys_dummies, Dummies_Get),
	    get_var(LEnv32, sys_g, G_Get),
	    get_var(LEnv32, sys_vals, Vals_Get),
	    cl_mapcar(cl_list, [Dummies_Get, Vals_Get], Mapcar_Ret),
	    get_var(LEnv32, sys_newval, Newval_Get),
	    cl_car(Newval_Get, Car_Ret),
	    get_var(LEnv32, sys_g, G_Get39),
	    get_var(LEnv32, sys_getter, Getter_Get),
	    get_var(LEnv32, sys_keys, Keys_Get41),
	    bq_append([[G_Get, Item_Get35]|Mapcar_Ret],
		      [[Car_Ret, [adjoin, G_Get39, Getter_Get|Keys_Get41]]],
		      Bq_append_Ret),
	    get_var(LEnv32, sys_setter, Setter_Get),
	    _25518664=[let_xx, Bq_append_Ret, Setter_Get]
	),
	_25518664=MFResult,
	cl_eval(MFResult, FnResult).
:- set_opv(cl_pushnew, classof, claz_macro),
   set_opv(pushnew, compile_as, kw_operator),
   set_opv(pushnew, function, cl_pushnew),
   DefMacroResult=pushnew.

wl:interned_eval_devel("
; Adapted from SBCL.
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
").

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:5443 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,pop,['&environment',env,place],[if,[and,[symbolp,place],[eq,place,[macroexpand,place,env]]],['#BQ',[prog1,[car,['#COMMA',place]],[setq,['#COMMA',place],[cdr,['#COMMA',place]]]]],['multiple-value-bind',[dummies,vals,newval,setter,getter],['get-setf-expansion',place,env],['do*',[[d,dummies,[cdr,d]],[v,vals,[cdr,v]],['let-list',[]]],[[null,d],[push,[list,[car,newval],getter],'let-list'],['#BQ',['let*',['#COMMA',[nreverse,'let-list']],[prog1,[car,['#COMMA',[car,newval]]],[setq,['#COMMA',[car,newval]],[cdr,['#COMMA',[car,newval]]]],['#COMMA',setter]]]]],[push,[list,[car,d],[car,v]],'let-list']]]]]).
wl:lambda_def(defmacro, pop, cl_pop, [c38_environment, env, sys_place], [progn, [if, [and, [symbolp, sys_place], [eq, sys_place, [macroexpand, sys_place, env]]], ['#BQ', [prog1, [car, ['#COMMA', sys_place]], [setq, ['#COMMA', sys_place], [cdr, ['#COMMA', sys_place]]]]], [multiple_value_bind, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter], [get_setf_expansion, sys_place, env], [do_xx, [[sys_d, sys_dummies, [cdr, sys_d]], [sys_v, sys_vals, [cdr, sys_v]], [sys_let_list, []]], [[null, sys_d], [push, [list, [car, sys_newval], sys_getter], sys_let_list], ['#BQ', [let_xx, ['#COMMA', [nreverse, sys_let_list]], [prog1, [car, ['#COMMA', [car, sys_newval]]], [setq, ['#COMMA', [car, sys_newval]], [cdr, ['#COMMA', [car, sys_newval]]]], ['#COMMA', sys_setter]]]]], [push, [list, [car, sys_d], [car, sys_v]], sys_let_list]]]]]).
wl: declared(cl_pop, env_arg1).

wl:arglist_info(pop, cl_pop, [c38_environment, env, sys_place], arginfo{all:[sys_place], allow_other_keys:0, aux:0, body:0, complex:[environment], env:[env], key:0, names:[env, sys_place], opt:0, req:[sys_place], rest:0, sublists:0, whole:0}).
wl: init_args(1,pop).

/*

### Compiled:  `CL:POP` 
*/
cl_pop(Place_In, RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(env, Env_In), bv(sys_place, Place_In)|CDR],
	global_env(CDR),
	parent_env(Env_In),
	catch(( ( get_var(Env, sys_place, Place_Get),
		  (   is_symbolp(Place_Get)
		  ->  get_var(Env, env, Env_Get),
		      get_var(Env, sys_place, Place_Get15),
		      cl_macroexpand([Place_Get15, Env_Get], Macroexpand_Ret),
		      cl_eq(Place_Get15, Macroexpand_Ret, TrueResult),
		      IFTEST=TrueResult
		  ;   IFTEST=[]
		  ),
		  (   IFTEST\==[]
		  ->  get_var(Env, sys_place, Place_Get19),
		      _28373318=[prog1, [car, Place_Get19], [setq, Place_Get19, [cdr, Place_Get19]]]
		  ;   LEnv=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_newval, []), bv(sys_setter, []), bv(sys_getter, [])|Env],
		      get_var(LEnv, env, Env_Get26),
		      get_var(LEnv, sys_place, Place_Get25),
		      cl_get_setf_expansion(Place_Get25,
					    Env_Get26,
					    Setf_expansion_Ret),
		      setq_from_values(LEnv,
				       
				       [ sys_dummies,
					 sys_vals,
					 sys_newval,
					 sys_setter,
					 sys_getter
				       ]),
		      get_var(LEnv, sys_dummies, Dummies_Get),
		      get_var(LEnv, sys_vals, Vals_Get),
		      BlockExitEnv=[bv(sys_d, Dummies_Get), bv(sys_v, Vals_Get), bv(sys_let_list, [])|LEnv],
		      catch(( call_addr_block(BlockExitEnv,
					      (push_label(do_label_2), get_var(BlockExitEnv, sys_d, IFTEST53), (IFTEST53==[]->cl_push([list, [car, sys_newval], sys_getter], sys_let_list, [], Push_Ret), get_var(BlockExitEnv, sys_let_list, Let_list_Get58), cl_nreverse(Let_list_Get58, Nreverse_Ret), get_var(BlockExitEnv, sys_newval, Newval_Get59), cl_car(Newval_Get59, Car_Ret), get_var(BlockExitEnv, sys_newval, Newval_Get60), cl_car(Newval_Get60, Car_Ret84), get_var(BlockExitEnv, sys_newval, Newval_Get61), cl_car(Newval_Get61, Car_Ret85), get_var(BlockExitEnv, sys_setter, Setter_Get62), throw(block_exit([], [let_xx, Nreverse_Ret, [prog1, [car, Car_Ret], [setq, Car_Ret84, [cdr, Car_Ret85]], Setter_Get62]])), _TBResult=ThrowResult57;cl_push([list, [car, sys_d], [car, sys_v]], sys_let_list, [], Push_Ret86), get_var(BlockExitEnv, sys_d, D_Get64), cl_cdr(D_Get64, D), get_var(BlockExitEnv, sys_v, V_Get65), cl_cdr(V_Get65, V), set_var(BlockExitEnv, sys_d, D), set_var(BlockExitEnv, sys_v, V), goto(do_label_2, BlockExitEnv), _TBResult=_GORES66)),
					      
					      [ addr(addr_tagbody_2_do_label_2,
						     do_label_2,
						     '$unused',
						     BlockExitEnv,
						     (get_var(BlockExitEnv, sys_d, IFTEST35), (IFTEST35==[]->cl_push([list, [car, sys_newval], sys_getter], sys_let_list, [], Push_Ret87), get_var(BlockExitEnv, sys_let_list, Nreverse_Param), cl_nreverse(Nreverse_Param, Nreverse_Ret88), get_var(BlockExitEnv, sys_newval, Car_Param), cl_car(Car_Param, Car_Ret89), get_var(BlockExitEnv, sys_newval, Newval_Get42), cl_car(Newval_Get42, Car_Ret90), get_var(BlockExitEnv, sys_newval, Newval_Get43), cl_car(Newval_Get43, Car_Ret91), get_var(BlockExitEnv, sys_setter, Get_var_Ret), throw(block_exit([], [let_xx, Nreverse_Ret88, [prog1, [car, Car_Ret89], [setq, Car_Ret90, [cdr, Car_Ret91]], Get_var_Ret]])), _29138900=ThrowResult;cl_push([list, [car, sys_d], [car, sys_v]], sys_let_list, [], Push_Ret93), get_var(BlockExitEnv, sys_d, D_Get46), cl_cdr(D_Get46, Cdr_Ret), get_var(BlockExitEnv, sys_v, Cdr_Param), cl_cdr(Cdr_Param, Cdr_Ret95), set_var(BlockExitEnv, sys_d, Cdr_Ret), set_var(BlockExitEnv, sys_v, Cdr_Ret95), goto(do_label_2, BlockExitEnv), _29138900=_GORES)))
					      ]),
			      []=LetResult28
			    ),
			    block_exit([], LetResult28),
			    true),
		      _28373318=LetResult28
		  )
		),
		_28373318=MFResult
	      ),
	      block_exit(pop, MFResult),
	      true),
	cl_eval(MFResult, FnResult).
:- set_opv(cl_pop, classof, claz_macro),
   set_opv(pop, compile_as, kw_operator),
   set_opv(pop, function, cl_pop),
   DefMacroResult=pop.  


wl:interned_eval_devel('
; See section 5.1.3.
#+WAM-CL
(defmacro incf-is (place &optional (delta 1) &environment env)
  "The first argument is some location holding a number.  This number is
incremented by the second argument, DELTA, which defaults to 1."
  (if (and (symbolp (setq place (%symbol-macroexpand place env)))
           (or (constantp delta)
               (and (symbolp delta)
                    (not (nth-value 1 (%symbol-macroexpand delta env))))))
    `(setq ,place (+ ,place ,delta))
    (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-method place env)
      (let ((d (gensym)))
        `(let* (,@(mapcar (function list) dummies vals)
                (,d ,delta)
                (,(car newval) (+ ,getter ,d)))
           ,setter)))))

').

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5125 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,incf,[place,'&optional',[delta,1],'&environment',env],'$STRING'("The first argument is some location holding a number.  This number is\r\nincremented by the second argument, DELTA, which defaults to 1."),[if,[and,[symbolp,[setq,place,['%symbol-macroexpand',place,env]]],[or,[constantp,delta],[and,[symbolp,delta],[not,['nth-value',1,['%symbol-macroexpand',delta,env]]]]]],['#BQ',[setq,['#COMMA',place],[+,['#COMMA',place],['#COMMA',delta]]]],['multiple-value-bind',[dummies,vals,newval,setter,getter],['get-setf-method',place,env],[let,[[d,[gensym]]],['#BQ',['let*',[['#BQ-COMMA-ELIPSE',[mapcar,function(list),dummies,vals]],[['#COMMA',d],['#COMMA',delta]],[['#COMMA',[car,newval]],[+,['#COMMA',getter],['#COMMA',d]]]],['#COMMA',setter]]]]]]]).
doc: doc_string(incf,
	      pkg_sys,
	      function,
	      "The first argument is some location holding a number.  This number is\r\nincremented by the second argument, DELTA, which defaults to 1.").

wl:lambda_def(defmacro, incf, cl_incf, [sys_place, c38_optional, [sys_delta, 1], c38_environment, env], [progn, [if, [and, [symbolp, [setq, sys_place, [sys_pf_symbol_macroexpand, sys_place, env]]], [or, [constantp, sys_delta], [and, [symbolp, sys_delta], [not, [nth_value, 1, [sys_pf_symbol_macroexpand, sys_delta, env]]]]]], ['#BQ', [setq, ['#COMMA', sys_place], [+, ['#COMMA', sys_place], ['#COMMA', sys_delta]]]], [multiple_value_bind, [sys_dummies, sys_vals, sys_newval, sys_setter, sys_getter], [ext_get_setf_method, sys_place, env], [let, [[sys_d, [gensym]]], ['#BQ', [let_xx, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_dummies, sys_vals]], [['#COMMA', sys_d], ['#COMMA', sys_delta]], [['#COMMA', [car, sys_newval]], [+, ['#COMMA', sys_getter], ['#COMMA', sys_d]]]], ['#COMMA', sys_setter]]]]]]]).
wl: declared(cl_incf, env_arg1).

wl:arglist_info(incf, cl_incf, [sys_place, c38_optional, [sys_delta, 1], c38_environment, env], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:[environment], env:[env], key:0, names:[sys_place, sys_delta, env], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}).
wl: init_args(1,incf).

/*

### Compiled:  `CL:INCF` 
*/
incf_macroexpand1_(Place_In, RestNKeys, MFResult) :-
	nop(defmacro),
	AEnv=[bv(sys_place, Place_In), bv(sys_delta, Delta_In), bv(env, Env_In)|Opt_var_Param],
	global_env(Opt_var_Param),
	opt_var(Opt_var_Param, sys_delta, Delta_In, true, 1, 1, RestNKeys),
	parent_env(Env_In),
	catch(( get_var(AEnv, env, Env_Get),
		get_var(AEnv, sys_place, Place_Get),
		f_sys_pf_symbol_macroexpand(Place_Get, Env_Get, PredArgResult),
		set_var(AEnv, sys_place, PredArgResult),
		(   is_symbolp(PredArgResult)
		->  (   get_var(AEnv, sys_delta, Delta_Get),
			cl_constantp(Delta_Get, FORM1_Res),
			FORM1_Res\==[],
			TrueResult23=FORM1_Res
		    ->  true
		    ;   get_var(AEnv, sys_delta, Delta_Get18),
			(   is_symbolp(Delta_Get18)
			->  cl_nth_value(1,
					 
					 [ sys_pf_symbol_macroexpand,
					   sys_delta,
					   env
					 ],
					 Not_Param),
			    cl_not(Not_Param, TrueResult),
			    _15651516=TrueResult
			;   _15651516=[]
			),
			TrueResult23=_15651516
		    ),
		    IFTEST=TrueResult23
		;   IFTEST=[]
		),
		(   IFTEST\==[]
		->  get_var(AEnv, sys_delta, Delta_Get26),
		    get_var(AEnv, sys_place, Place_Get24),
		    _15634562=[setq, Place_Get24, [+, Place_Get24, Delta_Get26]]
		;   LEnv=[bv(sys_dummies, []), bv(sys_vals, []), bv(sys_newval, []), bv(sys_setter, []), bv(sys_getter, [])|AEnv],
		    get_var(LEnv, env, Env_Get31),
		    get_var(LEnv, sys_place, Place_Get30),
		    f_ext_get_setf_method(Place_Get30,
					  Env_Get31,
					  Setf_method_Ret),
		    setq_from_values(LEnv,
				     
				     [ sys_dummies,
				       sys_vals,
				       sys_newval,
				       sys_setter,
				       sys_getter
				     ]),
		    cl_gensym(D_Init),
		    LEnv34=[bv(sys_d, D_Init)|LEnv],
		    get_var(LEnv34, sys_dummies, Dummies_Get),
		    get_var(LEnv34, sys_vals, Vals_Get),
		    cl_mapcar(cl_list, [Dummies_Get, Vals_Get], Bq_append_Param),
		    get_var(LEnv34, sys_d, D_Get),
		    get_var(LEnv34, sys_delta, Delta_Get39),
		    get_var(LEnv34, sys_newval, Newval_Get),
		    cl_car(Newval_Get, Car_Ret),
		    get_var(LEnv34, sys_d, D_Get42),
		    get_var(LEnv34, sys_getter, Getter_Get),
		    bq_append(Bq_append_Param,
			      
			      [ [D_Get, Delta_Get39],
				[Car_Ret, [+, Getter_Get, D_Get42]]
			      ],
			      Bq_append_Ret),
		    get_var(LEnv34, sys_setter, Setter_Get),
		    _15634562=[let_xx, Bq_append_Ret, Setter_Get]
		),
		_15634562=MFResult
	      ),
	      block_exit(incf, MFResult),
	      true).
:- set_opv(cl_incf, classof, claz_macro),
   set_opv(incf, compile_as, kw_operator),
   set_opv(incf, function, cl_incf),
   DefMacroResult=incf.

/*`
wl:interned_eval_devel(
"#+WAM-CL
(defmacro decf (place &optional (delta 1))
  `(incf ,place (- 0 ,delta)))
"). % "
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:5869 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,decf,[place,'&optional',[delta,1]],['#BQ',[incf,['#COMMA',place],[-,0,['#COMMA',delta]]]]]).
wl:lambda_def(defmacro, decf, cl_decf, [sys_place, c38_optional, [sys_delta, 1]], [progn, ['#BQ', [incf, ['#COMMA', sys_place], [-, 0, ['#COMMA', sys_delta]]]]]).
wl:arglist_info(decf, cl_decf, [sys_place, c38_optional, [sys_delta, 1]], arginfo{all:[sys_place, sys_delta], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_place, sys_delta], opt:[sys_delta], req:[sys_place], rest:0, sublists:0, whole:0}).
wl: init_args(1,decf).

/*

### Compiled:  `CL:DECF` 
*/
cl_decf(Place_In, RestNKeys, FnResult) :-
	nop(defmacro),
	Env=[bv(sys_place, Place_In), bv(sys_delta, Delta_In)|Opt_var_Param],
	global_env(Opt_var_Param),
	opt_var(Opt_var_Param, sys_delta, Delta_In, true, 1, 1, RestNKeys),
	catch(( get_var(Env, sys_delta, Delta_Get),
		get_var(Env, sys_place, Place_Get),
		[incf, Place_Get, [-, 0, Delta_Get]]=MFResult
	      ),
	      block_exit(decf, MFResult),
	      true),
	cl_eval(MFResult, FnResult).
:- set_opv(cl_decf, classof, claz_macro),
   set_opv(decf, compile_as, kw_operator),
   set_opv(decf, function, cl_decf),
   DefMacroResult=decf.


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init-1.lisp:8195 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'get-setf-method-inverse',[form,inverse,'setf-function'],[let,[['new-var',[gensym]],[vars,[]],[vals,[]]],[dolist,[x,[cdr,form]],[push,[gensym],vars],[push,x,vals]],[setq,vals,[nreverse,vals]],[values,vars,vals,[list,'new-var'],[if,'setf-function',['#BQ',[['#BQ-COMMA-ELIPSE',inverse],['#COMMA','new-var'],['#BQ-COMMA-ELIPSE',vars]]],[if,[functionp,[car,inverse]],['#BQ',[funcall,['#BQ-COMMA-ELIPSE',inverse],['#BQ-COMMA-ELIPSE',vars],['#COMMA','new-var']]],['#BQ',[['#BQ-COMMA-ELIPSE',inverse],['#BQ-COMMA-ELIPSE',vars],['#COMMA','new-var']]]]],['#BQ',[['#COMMA',[car,form]],['#BQ-COMMA-ELIPSE',vars]]]]]]).
wl:lambda_def(defun, sys_get_setf_method_inverse, f_sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], [[let, [[sys_new_var, [gensym]], [sys_vars, []], [sys_vals, []]], [dolist, [sys_x, [cdr, sys_form]], [push, [gensym], sys_vars], [push, sys_x, sys_vals]], [setq, sys_vals, [nreverse, sys_vals]], [values, sys_vars, sys_vals, [list, sys_new_var], [if, sys_setf_function, ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#COMMA', sys_new_var], ['#BQ-COMMA-ELIPSE', sys_vars]]], [if, [functionp, [car, sys_inverse]], ['#BQ', [funcall, ['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]], ['#BQ', [['#BQ-COMMA-ELIPSE', sys_inverse], ['#BQ-COMMA-ELIPSE', sys_vars], ['#COMMA', sys_new_var]]]]], ['#BQ', [['#COMMA', [car, sys_form]], ['#BQ-COMMA-ELIPSE', sys_vars]]]]]]).
wl:arglist_info(sys_get_setf_method_inverse, f_sys_get_setf_method_inverse, [sys_form, sys_inverse, sys_setf_function], arginfo{all:[sys_form, sys_inverse, sys_setf_function], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_form, sys_inverse, sys_setf_function], opt:0, req:[sys_form, sys_inverse, sys_setf_function], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_get_setf_method_inverse).

wl:interned_eval_devel("

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

").


/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/wam-cl-init.lisp:1023 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,setf,['&rest',pairs,'&environment',env],[let,[[nargs,[length,pairs]]],[assert,[evenp,nargs]],[cond,[[zerop,nargs],[]],[[=,nargs,2],[let,[[place,[car,pairs]],['value-form',[cadr,pairs]]],[cond,[[symbolp,place],['#BQ',[setq,['#COMMA',place],['#COMMA','value-form']]]],[[consp,place],[if,[eq,[car,place],[quote,the]],['#BQ',[setf,['#COMMA',[caddr,place]],[the,['#COMMA',[cadr,place]],['#COMMA','value-form']]]],['multiple-value-bind',[temps,vars,newvals,setter,getter],['get-setf-expansion',place,env],[declare,[ignore,getter]],['#BQ',[let,[['#BQ-COMMA-ELIPSE',[mapcar,function(list),temps,vars]]],['multiple-value-bind',['#COMMA',newvals],['#COMMA','value-form'],['#COMMA',setter]]]]]]]]]],[t,['do*',[[pairs,pairs,[cddr,pairs]],[setfs,[list,[quote,progn]]],[splice,setfs]],[[endp,pairs],setfs],[setq,splice,[cdr,[rplacd,splice,['#BQ',[[setf,['#COMMA',[car,pairs]],['#COMMA',[cadr,pairs]]]]]]]]]]]]]).
wl:lambda_def(defmacro, setf, cl_setf, [c38_rest, sys_pairs, c38_environment, env], [progn, [let, [[sys_nargs, [length, sys_pairs]]], [assert, [evenp, sys_nargs]], [cond, [[zerop, sys_nargs], []], [[=, sys_nargs, 2], [let, [[sys_place, [car, sys_pairs]], [sys_value_form, [cadr, sys_pairs]]], [cond, [[symbolp, sys_place], ['#BQ', [setq, ['#COMMA', sys_place], ['#COMMA', sys_value_form]]]], [[consp, sys_place], [if, [eq, [car, sys_place], [quote, the]], ['#BQ', [setf, ['#COMMA', [caddr, sys_place]], [the, ['#COMMA', [cadr, sys_place]], ['#COMMA', sys_value_form]]]], [multiple_value_bind, [sys_temps, sys_vars, sys_newvals, sys_setter, sys_getter], [get_setf_expansion, sys_place, env], [declare, [ignore, sys_getter]], ['#BQ', [let, [['#BQ-COMMA-ELIPSE', [mapcar, function(list), sys_temps, sys_vars]]], [multiple_value_bind, ['#COMMA', sys_newvals], ['#COMMA', sys_value_form], ['#COMMA', sys_setter]]]]]]]]]], [t, [do_xx, [[sys_pairs, sys_pairs, [cddr, sys_pairs]], [sys_setfs, [list, [quote, progn]]], [sys_splice, sys_setfs]], [[endp, sys_pairs], sys_setfs], [setq, sys_splice, [cdr, [rplacd, sys_splice, ['#BQ', [[setf, ['#COMMA', [car, sys_pairs]], ['#COMMA', [cadr, sys_pairs]]]]]]]]]]]]]).
wl: declared(cl_setf, env_arg1).

wl:arglist_info(setf, cl_setf, [c38_rest, sys_pairs, c38_environment, env], arginfo{all:0, allow_other_keys:0, aux:0, body:0, complex:[rest, environment], env:[env], key:0, names:[sys_pairs, env], opt:0, req:0, rest:[sys_pairs], sublists:0, whole:0}).
wl: init_args(0,setf).

/*

### Compiled:  `CL:SETF` 
*/
cl_setf(RestNKeys, FnResult) :- slow_trace,
	nop(defmacro),
	Env=[bv(sys_pairs, RestNKeys), bv(env, Env_In)|CDR],
	global_env(CDR),
	parent_env(Env_In),
	catch(( ( get_var(Env, sys_pairs, Pairs_Get),
		  cl_length(Pairs_Get, Nargs_Init),
		  LEnv=[bv(sys_nargs, Nargs_Init)|Env],
		  cl_assert([evenp, sys_nargs], Assert_Ret),
		  get_var(LEnv, sys_nargs, Nargs_Get),
		  (   mth:is_zerop(Nargs_Get)
		  ->  LetResult23=[]
		  ;   get_var(LEnv, sys_nargs, Nargs_Get19),
		      (   Nargs_Get19=:=2
		      ->  get_var(LEnv, sys_pairs, Pairs_Get25),
			  cl_car(Pairs_Get25, Place_Init),
			  get_var(LEnv, sys_pairs, Pairs_Get26),
			  cl_cadr(Pairs_Get26, Value_form_Init),
			  LEnv24=[bv(sys_place, Place_Init), bv(sys_value_form, Value_form_Init)|LEnv],
			  get_var(LEnv24, sys_place, Place_Get),
			  (   is_symbolp(Place_Get)
			  ->  get_var(LEnv24, sys_place, Place_Get33),
			      get_var(LEnv24, sys_value_form, Value_form_Get),
			      LetResult23=[setq, Place_Get33, Value_form_Get]
			  ;   get_var(LEnv24, sys_place, Place_Get36),
			      (   is_consp(Place_Get36)
			      ->  get_var(LEnv24, sys_place, Place_Get40),
				  cl_car(Place_Get40, PredArg1Result42),
				  (   is_eq(PredArg1Result42, the)
				  ->  get_var(LEnv24, sys_place, Place_Get43),
				      cl_caddr(Place_Get43, Caddr_Ret),
				      get_var(LEnv24, sys_place, Place_Get44),
				      cl_cadr(Place_Get44, Cadr_Ret),
				      get_var(LEnv24,
					      sys_value_form,
					      Value_form_Get45),
				      LetResult23=[setf, Caddr_Ret, [the, Cadr_Ret, Value_form_Get45]]
				  ;   LEnv48=[bv(sys_temps, []), bv(sys_vars, []), bv(sys_newvals, []), bv(sys_setter, []), bv(sys_getter, [])|LEnv24],
				      get_var(LEnv48, env, Env_Get),
				      get_var(LEnv48, sys_place, Place_Get49),
				      cl_get_setf_expansion(Place_Get49,
							    Env_Get,
							    Setf_expansion_Ret),
				      setq_from_values(LEnv48,
						       
						       [ sys_temps,
							 sys_vars,
							 sys_newvals,
							 sys_setter,
							 sys_getter
						       ]),
				      cl_declare([ignore, sys_getter],
						 Declare_Ret),
				      get_var(LEnv48, sys_temps, Temps_Get),
				      get_var(LEnv48, sys_vars, Vars_Get),
				      cl_mapcar(cl_list,
						[Temps_Get, Vars_Get],
						Mapcar_Ret),
				      get_var(LEnv48, sys_newvals, Newvals_Get),
				      get_var(LEnv48, sys_setter, Setter_Get),
				      get_var(LEnv48,
					      sys_value_form,
					      Value_form_Get54),
				      LetResult23=[let, Mapcar_Ret, [multiple_value_bind, Newvals_Get, Value_form_Get54, Setter_Get]]
				  )
			      ;   ElseResult=[],
				  LetResult23=ElseResult
			      )
			  )
		      ;   get_var(LEnv, sys_pairs, Pairs_Get62),
			  Setfs_Init=progn,
			  get_var(LEnv, sys_setfs, Setfs_Get),
			  AEnv=[bv(sys_pairs, Pairs_Get62), bv(sys_setfs, Setfs_Init), bv(sys_splice, Setfs_Get)|LEnv],
			  catch(( call_addr_block(AEnv,
						  (push_label(do_label_1), get_var(AEnv, sys_pairs, Pairs_Get87), (is_endp(Pairs_Get87)->get_var(AEnv, sys_setfs, RetResult90), throw(block_exit([], RetResult90)), _TBResult=ThrowResult91;get_var(AEnv, sys_pairs, Pairs_Get96), get_var(AEnv, sys_splice, Splice_Get95), cl_car(Pairs_Get96, Car_Ret), get_var(AEnv, sys_pairs, Pairs_Get97), cl_cadr(Pairs_Get97, Cadr_Ret121), cl_rplacd(Splice_Get95, [[setf, Car_Ret, Cadr_Ret121]], Cdr_Param), cl_cdr(Cdr_Param, Splice), set_var(AEnv, sys_splice, Splice), get_var(AEnv, sys_pairs, Pairs_Get98), cl_cddr(Pairs_Get98, Pairs), set_var(AEnv, sys_pairs, Pairs), goto(do_label_1, AEnv), _TBResult=_GORES99)),
						  
						  [ addr(addr_tagbody_1_do_label_1,
							 do_label_1,
							 '$unused',
							 AEnv,
							 (get_var(AEnv, sys_pairs, Pairs_Get69), (is_endp(Pairs_Get69)->get_var(AEnv, sys_setfs, Setfs_Get74), throw(block_exit([], Setfs_Get74)), _7039978=ThrowResult;get_var(AEnv, sys_pairs, Pairs_Get78), get_var(AEnv, sys_splice, Rplacd_Param), cl_car(Pairs_Get78, Car_Ret122), get_var(AEnv, sys_pairs, Pairs_Get79), cl_cadr(Pairs_Get79, Cadr_Ret123), cl_rplacd(Rplacd_Param, [[setf, Car_Ret122, Cadr_Ret123]], Cdr_Param112), cl_cdr(Cdr_Param112, Cdr_Ret), set_var(AEnv, sys_splice, Cdr_Ret), get_var(AEnv, sys_pairs, Pairs_Get80), cl_cddr(Pairs_Get80, Cddr_Ret), set_var(AEnv, sys_pairs, Cddr_Ret), goto(do_label_1, AEnv), _7039978=_GORES)))
						  ]),
				  []=LetResult60
				),
				block_exit([], LetResult60),
				true),
			  LetResult23=LetResult60
		      )
		  )
		),
		LetResult23=MFResult
	      ),
	      block_exit(setf, MFResult),
	      true),
	cl_eval(MFResult, FnResult).
:- set_opv(cl_setf, classof, claz_macro),
   set_opv(setf, compile_as, kw_operator),
   set_opv(setf, function, cl_setf),
   DefMacroResult=setf.



wl:lambda_def(defun, ext_get_setf_method_multiple_value, f_ext_get_setf_method_multiple_value, [u_form, c38_optional, env, c38_aux, u_tem], [[cond, [[symbolp, u_form], [let, [[u_store, [gensym]]], [values, [], [], [list, u_store], ['#BQ', [setq, ['#COMMA', u_form], ['#COMMA', u_store]]], u_form]]], [[or, [not, [consp, u_form]], [not, [symbolp, [car, u_form]]]], [error, '$ARRAY'([*], claz_base_character, "Cannot get the setf-method of ~S."), u_form]], [[multiple_value_bind, [u_t1, exp], [macroexpand, u_form, env], [when, exp, [setq, u_tem, u_t1]]], [ext_get_setf_method_multiple_value, u_tem, env]], [[get, [car, u_form], [quote, u_setf_method]], [apply, [get, [car, u_form], [quote, u_setf_method]], env, [cdr, u_form]]], [[or, [get, [car, u_form], [quote, u_setf_update_fn]], [setq, u_tem, [get, [car, u_form], [quote, sys_structure_access]]]], [let, [[u_vars, [u_to_gensyms, [cdr, u_form]]], [u_store, [gensym]]], [values, u_vars, [cdr, u_form], [list, u_store], [cond, [u_tem, [u_setf_structure_access, [car, u_vars], [car, u_tem], [cdr, u_tem], u_store]], [[let, [[u_f, [get, [car, u_form], [quote, u_setf_update_fn]]]], ['#BQ', [['#COMMA', u_f], ['#BQ-COMMA-ELIPSE', u_vars], ['#COMMA', u_store]]]]]], [cons, [car, u_form], u_vars]]]], [[get, [car, u_form], [quote, u_setf_lambda]], [let_xx, [[u_vars, [u_to_gensyms, [cdr, u_form]]], [u_store, [gensym]], [u_f, [get, [car, u_form], [quote, u_setf_lambda]]]], [values, u_vars, [cdr, u_form], [list, u_store], [funcall, [apply, u_f, u_vars], u_store], [cons, [car, u_form], u_vars]]]], [[macro_function, [car, u_form]], [ext_get_setf_method_multiple_value, [macroexpand, u_form, env]]], [t, [let, [[u_vars, [u_to_gensyms, [cdr, u_form]]], [u_store, [gensym]]], [values, u_vars, [cdr, u_form], [list, u_store], ['#BQ', [funcall, function([setf, ['#COMMA', [car, u_form]]]), ['#COMMA', u_store], ['#BQ-COMMA-ELIPSE', u_vars]]], [cons, [car, u_form], u_vars]]]]]]).
wl:arglist_info(ext_get_setf_method_multiple_value, f_ext_get_setf_method_multiple_value, [u_form, c38_optional, env, c38_aux, u_tem], arginfo{all:[u_form, env], allow_other_keys:0, aux:[u_tem], body:0, complex:0, env:0, key:0, names:[u_form, env, u_tem], opt:[env], req:[u_form], rest:0, sublists:0, whole:0}).
 wl:init_args(1,ext_get_setf_method_multiple_value).

 % ### Compiled:  `EXT:GET-SETF-METHOD-MULTIPLE-VALUE`
% ```prolog
f_ext_get_setf_method_multiple_value(Form_In, RestNKeys, FnResult) :-
        AEnv51=[bv(u_form, Form_In), bv(env, Env_In), bv(u_tem, Tem_In)|ReplEnv],
        global_env(ReplEnv),
        opt_var(ReplEnv, env, Env_In, true, [], 1, RestNKeys),
        aux_var(ReplEnv, u_tem, Tem_In, true, []),
        catch(( ( get_var(AEnv51, u_form, Form_Get),
                  (   is_symbolp(Form_Get)
                  ->  cl_gensym(Store_Init),
                      LEnv=[bv(u_store, Store_Init)|AEnv51],
                      get_var(LEnv, u_store, Store_Get),
                      CAR=[Store_Get],
                      get_var(LEnv, u_form, Form_Get18),
                      get_var(LEnv, u_store, Store_Get19),
                      nb_setval('$mv_return',

                                [ [],
                                  [],
                                  CAR,
                                  [setq, Form_Get18, Store_Get19],
                                  Form_Get18
                                ]),
                      _9544=[]
                  ;   (   get_var(AEnv51, u_form, Form_Get23),
                          cl_consp(Form_Get23, Not_Param),
                          cl_not(Not_Param, FORM1_Res),
                          FORM1_Res\==[],
                          IFTEST21=FORM1_Res
                      ->  true
                      ;   get_var(AEnv51, u_form, Form_Get24),
                          cl_car(Form_Get24, Symbolp_Param),
                          cl_symbolp(Symbolp_Param, Not_Param134),
                          cl_not(Not_Param134, Not_Ret),
                          IFTEST21=Not_Ret
                      ),
                      (   IFTEST21\==[]
                      ->  get_var(AEnv51, u_form, Form_Get26),
                          cl_error(
                                   [ '$ARRAY'([*],
                                              claz_base_character,
                                              "Cannot get the setf-method of ~S."),
                                     Form_Get26
                                   ],
                                   TrueResult126),
                          ElseResult128=TrueResult126
                      ;   LEnv31=[bv(u_t1, []), bv(exp, [])|AEnv51],
                          get_var(LEnv31, env, Env_Get),
                          get_var(LEnv31, u_form, Form_Get32),
                          cl_macroexpand([Form_Get32, Env_Get], Macroexpand_Ret),
                          setq_from_values(LEnv31, [u_t1, exp]),
                          get_var(LEnv31, exp, IFTEST34),
                          (   IFTEST34\==[]
                          ->  get_var(LEnv31, u_t1, T1_Get),
                              set_var(LEnv31, u_tem, T1_Get),
                              IFTEST27=T1_Get
                          ;   IFTEST27=[]
                          ),
                          (   IFTEST27\==[]
                          ->  get_var(AEnv51, env, Env_Get41),
                              get_var(AEnv51, u_tem, Tem_Get),
                              f_ext_get_setf_method_multiple_value(Tem_Get,
                                                                   [Env_Get41],
                                                                   TrueResult124),
                              ElseResult127=TrueResult124
                          ;   get_var(AEnv51, u_form, Form_Get44),
                              cl_car(Form_Get44, Get_Param),
                              cl_get(Get_Param, u_setf_method, [], IFTEST42),
                              (   IFTEST42\==[]
                              ->  get_var(AEnv51, u_form, Form_Get45),
                                  cl_car(Form_Get45, Get_Param136),
                                  cl_get(Get_Param136,
                                         u_setf_method,
                                         [],
                                         Apply_Param),
                                  get_var(AEnv51, env, Env_Get46),
                                  get_var(AEnv51, u_form, Form_Get47),
                                  cl_cdr(Form_Get47, Cdr_Ret),
                                  cl_apply(Apply_Param,
                                           [Env_Get46, Cdr_Ret],
                                           TrueResult122),
                                  ElseResult125=TrueResult122
                              ;   (   get_var(AEnv51, u_form, Form_Get50),
                                      cl_car(Form_Get50, Get_Param138),
                                      cl_get(Get_Param138,
                                             u_setf_update_fn,
                                             [],
                                             FORM1_Res53),
                                      FORM1_Res53\==[],
                                      IFTEST48=FORM1_Res53
                                  ->  true
                                  ;   get_var(AEnv51, u_form, Form_Get52),
                                      cl_car(Form_Get52, Get_Param139),
                                      cl_get(Get_Param139,
                                             sys_structure_access,
                                             [],
                                             Tem),
                                      set_var(AEnv51, u_tem, Tem),
                                      IFTEST48=Tem
                                  ),
                                  (   IFTEST48\==[]
                                  ->  get_var(AEnv51, u_form, Form_Get57),
                                      cl_cdr(Form_Get57, To_gensyms_Param),
                                      f_u_to_gensyms(To_gensyms_Param,
                                                     Vars_Init),
                                      cl_gensym(Store_Init59),
                                      LEnv56=[bv(u_vars, Vars_Init), bv(u_store, Store_Init59)|AEnv51],
                                      get_var(LEnv56, u_form, Form_Get60),
                                      cl_cdr(Form_Get60, Cdr_Ret154),
                                      get_var(LEnv56, u_store, Store_Get61),
                                      CAR160=[Store_Get61],
                                      get_var(LEnv56, u_tem, IFTEST62),
                                      (   IFTEST62\==[]
                                      ->  get_var(LEnv56, u_vars, Vars_Get),
                                          cl_car(Vars_Get,
                                                 Structure_access_Param),
                                          get_var(LEnv56, u_tem, Tem_Get66),
                                          cl_car(Tem_Get66, Car_Ret),
                                          get_var(LEnv56, u_tem, Tem_Get67),
                                          cl_cdr(Tem_Get67, Cdr_Ret156),
                                          get_var(LEnv56, u_store, Store_Get68),
                                          f_u_setf_structure_access(Structure_access_Param,
                                                                    Car_Ret,
                                                                    Cdr_Ret156,
                                                                    Store_Get68,
                                                                    TrueResult80),
                                          CAR159=TrueResult80
                                      ;   get_var(LEnv56, u_form, Form_Get74),
                                          cl_car(Form_Get74, Get_Param142),
                                          cl_get(Get_Param142,
                                                 u_setf_update_fn,
                                                 [],
                                                 Init),
                                          LEnv73=[bv(u_f, Init)|LEnv56],
                                          get_var(LEnv73, u_f, Get),
                                          get_var(LEnv73, u_store, Store_Get78),
                                          get_var(LEnv73, u_vars, Vars_Get77),
                                          bq_append([Get|Vars_Get77],
                                                    [Store_Get78],
                                                    IFTEST69),
                                          (   IFTEST69\==[]
                                          ->  ElseResult81=[]
                                          ;   ElseResult=[],
                                              ElseResult81=ElseResult
                                          ),
                                          CAR159=ElseResult81
                                      ),
                                      get_var(LEnv56, u_form, Form_Get82),
                                      cl_car(Form_Get82, Car_Ret157),
                                      get_var(LEnv56, u_vars, Vars_Get83),
                                      CAR158=[Car_Ret157|Vars_Get83],
                                      nb_setval('$mv_return',

                                                [ u_vars,
                                                  Cdr_Ret154,
                                                  CAR160,
                                                  CAR159,
                                                  CAR158
                                                ]),
                                      ElseResult123=u_vars
                                  ;   get_var(AEnv51, u_form, Form_Get86),
                                      cl_car(Form_Get86, Get_Param143),
                                      cl_get(Get_Param143,
                                             u_setf_lambda,
                                             [],
                                             IFTEST84),
                                      (   IFTEST84\==[]
                                      ->  get_var(AEnv51, u_form, Form_Get90),
                                          cl_cdr(Form_Get90,
                                                 To_gensyms_Param144),
                                          f_u_to_gensyms(To_gensyms_Param144,
                                                         Vars_Init92),
                                          cl_gensym(Store_Init93),
                                          get_var(AEnv51, u_form, Form_Get91),
                                          cl_car(Form_Get91, Get_Param145),
                                          cl_get(Get_Param145,
                                                 u_setf_lambda,
                                                 [],
                                                 Init94),
                                          LEnv89=[bv(u_vars, Vars_Init92), bv(u_store, Store_Init93), bv(u_f, Init94)|AEnv51],
                                          get_var(LEnv89, u_form, Form_Get95),
                                          cl_cdr(Form_Get95, Cdr_Ret161),
                                          get_var(LEnv89, u_store, Store_Get96),
                                          CAR165=[Store_Get96],
                                          get_var(LEnv89, u_f, Get97),
                                          get_var(LEnv89, u_vars, Vars_Get98),
                                          cl_apply(Get97,
                                                   Vars_Get98,
                                                   Apply_Param146),
                                          get_var(LEnv89, u_store, Store_Get99),
                                          cl_apply(Apply_Param146,
                                                   [Store_Get99],
                                                   Apply_Ret),
                                          get_var(LEnv89, u_form, Form_Get100),
                                          cl_car(Form_Get100, Car_Ret163),
                                          get_var(LEnv89, u_vars, Vars_Get101),
                                          CAR164=[Car_Ret163|Vars_Get101],
                                          nb_setval('$mv_return',

                                                    [ u_vars,
                                                      Cdr_Ret161,
                                                      CAR165,
                                                      Apply_Ret,
                                                      CAR164
                                                    ]),
                                          ElseResult121=u_vars
                                      ;   get_var(AEnv51, u_form, Form_Get104),
                                          cl_car(Form_Get104,
                                                 Macro_function_Param),
                                          cl_macro_function(Macro_function_Param,
                                                            IFTEST102),
                                          (   IFTEST102\==[]
                                          ->  get_var(AEnv51, env, Env_Get106),
                                              get_var(AEnv51,
                                                      u_form,
                                                      Form_Get105),
                                              cl_macroexpand(
                                                             [ Form_Get105,
                                                               Env_Get106
                                                             ],
                                                             Multiple_value_Param),
                                              f_ext_get_setf_method_multiple_value(Multiple_value_Param,
                                                                                   [],
                                                                                   TrueResult119),
                                              ElseResult120=TrueResult119
                                          ;   get_var(AEnv51,
                                                      u_form,
                                                      Form_Get110),
                                              cl_cdr(Form_Get110,
                                                     To_gensyms_Param149),
                                              f_u_to_gensyms(To_gensyms_Param149,
                                                             Vars_Init111),
                                              cl_gensym(Store_Init112),
                                              LEnv109=[bv(u_vars, Vars_Init111), bv(u_store, Store_Init112)|AEnv51],
                                              get_var(LEnv109,
                                                      u_form,
                                                      Form_Get113),
                                              cl_cdr(Form_Get113, Cdr_Ret166),
                                              get_var(LEnv109,
                                                      u_store,
                                                      Store_Get114),
                                              CAR169=[Store_Get114],
                                              get_var(LEnv109,
                                                      u_form,
                                                      Form_Get117),
                                              get_var(LEnv109,
                                                      u_store,
                                                      Store_Get115),
                                              get_var(LEnv109,
                                                      u_vars,
                                                      Vars_Get116),
                                              cl_car(Form_Get117, Car_Ret167),
                                              get_var(LEnv109,
                                                      u_vars,
                                                      Vars_Get118),
                                              CAR168=[Car_Ret167|Vars_Get118],
                                              nb_setval('$mv_return',

                                                        [ u_vars,
                                                          Cdr_Ret166,
                                                          CAR169,

                                                          [ funcall,
                                                            function(
                                                                     [ setf,

                                                                       [ '#COMMA',
                                                                         [car, u_form]
                                                                       ]
                                                                     ]),
                                                            Store_Get115
                                                          | Vars_Get116
                                                          ],
                                                          CAR168
                                                        ]),
                                              ElseResult120=u_vars
                                          ),
                                          ElseResult121=ElseResult120
                                      ),
                                      ElseResult123=ElseResult121
                                  ),
                                  ElseResult125=ElseResult123
                              ),
                              ElseResult127=ElseResult125
                          ),
                          ElseResult128=ElseResult127
                      ),
                      _9544=ElseResult128
                  )
                ),
                _9544=FnResult
              ),
              block_exit(ext_get_setf_method_multiple_value, FnResult),
              true).
:- set_opv(f_ext_get_setf_method_multiple_value, classof, claz_function),
   set_opv(ext_get_setf_method_multiple_value, compile_as, kw_function),
   set_opv(ext_get_setf_method_multiple_value,
           function,
           f_ext_get_setf_method_multiple_value),
   DefunResult=ext_get_setf_method_multiple_value.

% ## EXEC
% 971 inferences, 0.001 CPU in 0.001 seconds (97% CPU, 1350006 Lips)
/*
Warning: cl_macro_function/2, which is referenced by
Warning:        1-st clause of f_ext_get_setf_method_multiple_value/3: 1-st clause of f_ext_get_setf_method_multiple_value/3
Warning: f_u_setf_structure_access/5, which is referenced by
Warning:        1-st clause of f_ext_get_setf_method_multiple_value/3: 1-st clause of f_ext_get_setf_method_multiple_value/3
Warning: f_u_to_gensyms/2, which is referenced by
Warning:        1-st clause of f_ext_get_setf_method_multiple_value/3: 1-st clause of f_ext_get_setf_method_multiple_value/3

*/
wl:lambda_def(defun, u_setf_structure_access, f_u_setf_structure_access, [u_struct, type, index, u_newvalue], [[case, type, [list, ['#BQ', [setf, [nth, ['#COMMA', index], ['#COMMA', u_struct]], ['#COMMA', u_newvalue]]]], [vector, ['#BQ', [sys_elt_set, ['#COMMA', u_struct], ['#COMMA', index], ['#COMMA', u_newvalue]]]], [t, ['#BQ', [sys_structure_set, ['#COMMA', u_struct], [quote, ['#COMMA', type]], ['#COMMA', index], ['#COMMA', u_newvalue]]]]]]).
wl:arglist_info(u_setf_structure_access, f_u_setf_structure_access, [u_struct, type, index, u_newvalue], arginfo{all:[u_struct, type, index, u_newvalue], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_struct, type, index, u_newvalue], opt:0, req:[u_struct, type, index, u_newvalue], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_u_setf_structure_access).

f_u_setf_structure_access(Struct_In, Type_In, Index_In, Newvalue_In, FnResult) :-
        Env28=[bv(u_struct, Struct_In), bv(type, Type_In), bv(index, Index_In), bv(u_newvalue, Newvalue_In)|ReplEnv],
        global_env(ReplEnv),
        catch(( ( get_var(Env28, type, Key),
                  (   is_eq(Key, list)
                  ->  get_var(Env28, index, Index_Get),
                      get_var(Env28, u_newvalue, Newvalue_Get),
                      get_var(Env28, u_struct, Struct_Get),
                      _5704=[setf, [nth, Index_Get, Struct_Get], Newvalue_Get]
                  ;   (   is_eq(Key, vector)
                      ->  get_var(Env28, index, Index_Get19),
                          get_var(Env28, u_newvalue, Newvalue_Get20),
                          get_var(Env28, u_struct, Struct_Get18),
                          ElseResult=[sys_elt_set, Struct_Get18, Index_Get19, Newvalue_Get20]
                      ;   get_var(Env28, index, Index_Get23),
                          get_var(Env28, type, Type_Get22),
                          get_var(Env28, u_newvalue, Newvalue_Get24),
                          get_var(Env28, u_struct, Struct_Get21),
                          ElseResult=[sys_structure_set, Struct_Get21, [quote, Type_Get22], Index_Get23, Newvalue_Get24]
                      ),
                      _5704=ElseResult
                  )
                ),
                _5704=FnResult
              ),
              block_exit(u_setf_structure_access, FnResult),
              true).
:- set_opv(f_u_setf_structure_access, classof, claz_function),
   set_opv(u_setf_structure_access, compile_as, kw_function),
   set_opv(u_setf_structure_access, function, f_u_setf_structure_access),
   DefunResult=u_setf_structure_access.

/*
(defmacro define-setf-expander (access-fn &rest rest)
  (declare (optimize (safety 2)))
  `(define-setf-method ,access-fn ,@rest))

*/

wl:lambda_def(defun, ext_get_setf_method, f_ext_get_setf_method, [u_form, c38_optional, env], [[multiple_value_bind, [u_vars, u_vals, u_stores, u_store_form, u_access_form], [ext_get_setf_method_multiple_value, u_form, env], [unless, [=, [list_length, u_stores], 1], [error, '$ARRAY'([*], claz_base_character, "Multiple store-variables are not allowed.")]], [values, u_vars, u_vals, u_stores, u_store_form, u_access_form]]]).
wl:arglist_info(ext_get_setf_method, f_ext_get_setf_method, [u_form, c38_optional, env], arginfo{all:[u_form, env], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_form, env], opt:[env], req:[u_form], rest:0, sublists:0, whole:0}).
wl: init_args(1,ext_get_setf_method).


wl:interned_eval_devel('
;;; GET-SETF-METHOD.
;;; It just calls GET-SETF-METHOD-MULTIPLE-VALUE
;;;  and checks the number of the store variable.
(defun get-setf-method (form &optional env)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method-multiple-value form env)
    (unless (= (list-length stores) 1)
	    (error "Multiple store-variables are not allowed."))
    (values vars vals stores store-form access-form)))

').

%### Compiled:  `EXT:GET-SETF-METHOD`
%```prolog

f_ext_get_setf_method(Form_In, RestNKeys, FnResult):- !,
  cl_get_setf_expansion(Form_In, RestNKeys, FnResult).

f_ext_get_setf_method(Form_In, RestNKeys, FnResult) :-
        Env24=[bv(u_form, Form_In), bv(env, Env_In)|ReplEnv],
        global_env(ReplEnv),
        opt_var(ReplEnv, env, Env_In, true, [], 1, RestNKeys),
        catch(( ( LEnv=[bv(u_vars, []), bv(u_vals, []), bv(u_stores, []), bv(u_store_form, []), bv(u_access_form, [])|Env24],
                  get_var(LEnv, env, Env_Get),
                  get_var(LEnv, u_form, Form_Get),
                  f_ext_get_setf_method_multiple_value(Form_Get,
                                                       [Env_Get],
                                                       Multiple_value_Ret),
                  setq_from_values(LEnv,

                                   [ u_vars,
                                     u_vals,
                                     u_stores,
                                     u_store_form,
                                     u_access_form
                                   ]),
                  get_var(LEnv, u_stores, Stores_Get),
                  cl_list_length(Stores_Get, PredArg1Result),
                  (   PredArg1Result=:=1
                  ->  _6520=[]
                  ;   cl_error(
                               [ '$ARRAY'([*],
                                          claz_base_character,
                                          "Multiple store-variables are not allowed.")
                               ],
                               ElseResult),
                      _6520=ElseResult
                  ),
                  get_var(LEnv, u_access_form, Access_form_Get),
                  ( get_var(LEnv, u_store_form, Store_form_Get),
                    get_var(LEnv, u_stores, Stores_Get19)
                  ),
                  get_var(LEnv, u_vals, Vals_Get),
                  nb_setval('$mv_return',

                            [ u_vars,
                              Vals_Get,
                              Stores_Get19,
                              Store_form_Get,
                              Access_form_Get
                            ])
                ),
                u_vars=FnResult
              ),
              block_exit(ext_get_setf_method, FnResult),
              true).
:- set_opv(f_ext_get_setf_method, classof, claz_function),
   set_opv(ext_get_setf_method, compile_as, kw_function),
   set_opv(ext_get_setf_method, function, f_ext_get_setf_method),
   DefunResult=ext_get_setf_method.

