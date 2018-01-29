;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPLAM  Lambda expression.

(in-package 'compiler)

(eval-when (compile eval)
  (defmacro ck-spec (condition)
    `(unless ,condition
      (cmperr "The parameter specification ~s is illegal." spec)))

  (defmacro ck-vl (condition)
    `(unless ,condition
      (cmperr "The lambda list ~s is illegal." vl)))
  )

;;; During Pass1, a lambda-list
;;;
;;; (	{ var }*
;;; 	[ &optional { var | ( var [ initform [ svar ] ] ) }* ]
;;; 	[ &rest var ]
;;; 	[ &key { var | ( { var | ( kwd var ) } [initform [ svar ]])}*
;;; 		[&allow-other-keys]]
;;; 	[ &aux {var | (var [initform])}*]
;;; )
;;;
;;; is transformed into
;;;
;;; (	( { var }* )				; required
;;; 	( { (var initform svar) }* )		; optional
;;; 	{ var | nil }				; rest
;;; 	key-flag
;;; 	( { ( kwd-vv-index var initform svar ) }* )	; key
;;; 	allow-other-keys-flag
;;; )
;;;
;;; where
;;; 	svar:	(make-var :kind 'DUMMY)	; means svar is not supplied
;;;	        | var
;;;
;;; &aux parameters will be embedded into LET*.
;;;
;;; c1lambda-expr receives
;;;	( lambda-list { doc | decl }* . body )
;;; and returns
;;;	( lambda info-object lambda-list' doc body' )
;;;
;;; Doc is NIL if no doc string is supplied.
;;; Body' is body possibly surrounded by a LET* (if &aux parameters are
;;; supplied) and an implicit block.

(defun c1lambda-expr (lambda-expr
                      &optional (block-name nil block-it)
                      &aux (requireds nil) (optionals nil) (rest nil)
                           (keywords nil) (key-flag nil)
                           (allow-other-keys nil) (aux-vars nil)
                           (aux-inits nil) doc spec body ss is ts
                           other-decls vnames
                           (*vars* *vars*)
                           (info (make-info))
                           (aux-info nil)
                      )
  (cmpck (endp lambda-expr)
         "The lambda expression ~s is illegal." (cons 'LAMBDA lambda-expr))

  (multiple-value-setq (body ss ts is other-decls doc)
                       (c1body (cdr lambda-expr) t))

  (when block-it (setq body (list (cons 'BLOCK (cons block-name body)))))

  (c1add-globals ss)

  (multiple-value-setq (requireds optionals rest
				  key-flag keywords allow-other-keys
				  aux-vars)
    (parse-lambda-list (car lambda-expr)))

  (do ((specs requireds (cdr specs)))
      ((null specs))
    (setq spec (car specs))
    (let ((v (c1make-var spec ss is ts)))
      (push spec vnames)
      (push v *vars*)
      (setf (car specs) v)))

  (do ((specs optionals (cdr specs)))
      ((null specs))
    (setq spec (car specs))
    (cond ((symbolp spec)
	   (let ((v (c1make-var spec ss is ts)))
	     (push spec vnames)
	     (setf (car specs) (list v (default-init (var-type v)) nil))
	     (push v *vars*)))
	  ((symbolp (cdr spec))
	   (ck-spec (null (cdr spec)))
	   (let ((v (c1make-var (car spec) ss is ts)))
	     (push (car spec) vnames)
	     (setf (car specs) (list v (default-init (var-type v)) nil))
	     (push v *vars*)))
	  ((symbolp (cddr spec))
	   (ck-spec (null (cddr spec)))
	   (let ((init (c1expr* (second spec) info))
		 (v (c1make-var (car spec) ss is ts)))
	     (push (car spec) vnames)
	     (setf (car specs)
		   (list v (and-form-type (var-type v) init (second spec)) nil))
	     (push v *vars*)))
	  (t
	   (ck-spec (null (cdddr spec)))
	   (let ((init (c1expr* (second spec) info))
		 (v (c1make-var (car spec) ss is ts))
		 (sv (c1make-var (third spec) ss is ts))
		 )
	     (push (car spec) vnames)
	     (push (third spec) vnames)
	     (setf (car specs)
		   (list v (and-form-type (var-type v) init (second spec)) sv))
	     (push v *vars*)
	     (push sv *vars*)))))

  (when rest
    (push rest vnames)
    (setq rest (c1make-var rest ss is ts))
    (push rest *vars*))

  (do ((specs keywords (cdr specs)))
      ((null specs))
    (setq spec (car specs))
    ;; spec is:
    ;; 	( kwd-vv-index var [ initform [ svar ]])
    (cond ((null (cddr spec))
	   (ck-spec (null (cddr spec)))
	   (let ((v (c1make-var (second spec) ss is ts)))
	     (push (second spec) vnames)
	     (setf (car specs)
	      (list (car spec) v (default-init (var-type v))
		    (make-var :kind 'DUMMY)))
	     (push v *vars*)))
	  ((null (cdddr spec))
	   (ck-spec (null (cdddr spec)))
	   (let ((init (c1expr* (third spec) info))
		 (v (c1make-var (second spec) ss is ts)))
	     (push (second spec) vnames)
	     (setf (car specs)
		   (list (car spec) v
			 (and-form-type (var-type v) init (third spec))
			 (make-var :kind 'DUMMY)))
	     (push v *vars*)))
	  (t
	   (ck-spec (null (cddddr spec)))
	   (let ((init (c1expr* (third spec) info))
		 (v (c1make-var (second spec) ss is ts))
		 (sv (c1make-var (fourth spec) ss is ts)))
	     (push (second spec) vnames)
	     (push (fourth spec) vnames)
	     (setf (car specs)
		   (list (car spec) v
			 (and-form-type (var-type v) init (third spec))
			 sv))
	     (push v *vars*)
	     (push sv *vars*)))))

  (when aux-vars
    (setq aux-info (make-info))
    (do ((specs aux-vars (cdr specs)))
	((null specs))
      (setq spec (car specs))
      (if (consp spec)
	  (cond ((symbolp (cdr spec))
		 (ck-spec (null (cdr spec)))
		 (let ((v (c1make-var (car spec) ss is ts)))
		   (push (car spec) vnames)
		   (push (default-init (var-type v)) aux-inits)
		   (setf (car specs) v)
		   (push v *vars*)))
		(t
		 (ck-spec (null (cddr spec)))
		 (let ((init (c1expr* (second spec) aux-info))
		       (v (c1make-var (car spec) ss is ts)))
		   (push (car spec) vnames)
		   (push (and-form-type (var-type v) init (second spec))
			 aux-inits)
		   (setf (car specs) v)
		   (push v *vars*))))
	  (let ((v (c1make-var spec ss is ts)))
	    (push spec vnames)
	    (push (default-init (var-type v)) aux-inits)
	    (setf (car specs) v)
	    (push v *vars*))))
    (setq aux-inits (nreverse aux-inits))
    (add-info info aux-info))

  (check-vdecl vnames ts is)

  (setq body (c1decl-body other-decls body))

  (add-info info (second body))

  (dolist (var requireds) (check-vref var))
  (dolist (opt optionals)
            (check-vref (car opt))
            (when (third opt) (check-vref (third opt))))
  (when rest (check-vref rest))
  (dolist (kwd keywords)
            (check-vref (second kwd))
            (when (fourth kwd) (check-vref (fourth kwd))))
  (dolist (var aux-vars) (check-vref var))

  (when aux-vars
        (add-info aux-info (second body))
        (setq body (list 'LET* aux-info aux-vars aux-inits body)))

  (list 'LAMBDA
        info
        (list requireds optionals rest key-flag keywords allow-other-keys)
        doc
        body)
  )

(defun parse-lambda-list (vl &aux spec requireds optionals rest
			     key-flag keywords allow-other-keys aux-vars)
  (block parse
   (tagbody
    Lreq
      (when (null vl) (return-from parse))
      (ck-vl (consp vl))
      (case (setq spec (pop vl))
            (&optional (go Lopt))
            (&rest (go Lrest))
            (&key (go Lkey))
            (&aux (go Laux)))
      (push spec requireds)
      (go Lreq)

    Lopt
      (when (null vl) (return-from parse))
      (ck-vl (consp vl))
      (case (setq spec (pop vl))
            (&rest (go Lrest))
            (&key (go Lkey))
            (&aux (go Laux)))
      (push spec optionals)
      (go Lopt)

    Lrest
      (ck-vl (consp vl))
      (setq rest (pop vl))
      (when (null vl) (return-from parse))
      (ck-vl (consp vl))
      (case (setq spec (pop vl))
            (&key (go Lkey))
            (&aux (go Laux)))
      (cmperr "Either &key or &aux is missing before ~s." spec)

    Lkey
      (setq key-flag t)
      (when (null vl) (return-from parse))
      (ck-vl (consp vl))
      (case (setq spec (pop vl))
            (&aux (go Laux))
            (&allow-other-keys (setq allow-other-keys t)
                               (when (null vl) (return-from parse))
                               (ck-vl (consp vl))
                               (case (setq spec (pop vl))
                                     (&aux (go Laux)))
                               (cmperr "&aux is missing before ~s." spec)))
      ;; { var || ( { var || ( kwd var ) } [initform [ svar ]] ) }
      (unless (consp spec) (setq spec (list spec)))
      (cond ((consp (car spec))
             (ck-spec (and (keywordp (caar spec))
                           (consp (cdar spec))
                           (null (cddar spec))))
             (setq spec (cons (caar spec) (cons (cadar spec) (cdr spec)))))
            (t
             (ck-spec (symbolp (car spec)))
             (setq spec (cons (intern (string (car spec)) 'KEYWORD)
                              (cons (car spec) (cdr spec))))))
      (push spec keywords)
      (go Lkey)

    Laux
      (when (null vl) (return-from parse))
      (ck-vl (consp vl))
      (push (pop vl) aux-vars)
      (go Laux)
      )
   )
  (values (nreverse requireds)
	  (nreverse optionals)
	  rest
	  key-flag
	  (nreverse keywords)
	  allow-other-keys
	  (nreverse aux-vars))
  )

(defun c2lambda-expr (lambda-list body cfun fname
				  &optional closure-p call-lambda)
  (let ((*tail-recursion-info*		;;; Tail recursion possible if
         (and fname (symbolp fname)	;;; named function (a list is used for
	      				;;; lambda-block's),
				        ;;; no required appears in closure,
              (dolist (var (car lambda-list) t)
                (declare (type var var))
                (when (var-ref-ccb var) (return nil)))
              (null (second lambda-list))	;;; no optionals,
              (null (third lambda-list))	;;; no rest parameter, and
              (null (fourth lambda-list))	;;; no keywords.
              (cons fname (car lambda-list)))))
    (if (fourth lambda-list)			;;; key-flag
      (c2lambda-expr-with-key lambda-list body closure-p call-lambda cfun)
      (c2lambda-expr-without-key lambda-list body closure-p call-lambda)))
  )

#| Steps:
 1. defun creates declarations for requireds + va_alist
 2. c2lambda-expr adds declarations for:
	unboxed requireds
	lexical optionals (+ supplied-p), rest, keywords (+ supplied-p)
    Lexical optionals and keywords can be unboxed if:
	a. there is more then one reference in the body
	b. they are not referenced in closures
 3. binding is performed for:
	special or unboxed requireds
	optionals, rest, keywords
|#

(defun c2lambda-expr-without-key (lambda-list
                                  body
				  closure-p
				  kind
                                  &aux
				  (requireds (first lambda-list))
                                  (optionals (second lambda-list))
                                  (rest (third lambda-list)) rest-loc
                                  (nreq (length requireds))
                                  (labels nil)
                                  (*unwind-exit* *unwind-exit*)
                                  (*env* *env*)
                                  (block-p nil))
  (declare (fixnum nreq))

  ;; kind is either:
  ;; 1. CALL-LAMBDA, for a lambda expression
  ;; 2. LOCAL-ENTRY, for the local entry of a proclaimed function
  ;; 3. NIL, otherwise

  (if
   (eq 'LOCAL-ENTRY kind)

   ;; for local entry functions arguments are processed by t3defun
   (do ((reqs requireds (cdr reqs))
	(reqi (1+ *lcl*) (1+ reqi))
	(LCL_i (list 'LCL 0)))		; to allow concurrent compilations
       ((endp reqs) (setq *lcl* reqi))
     (declare (fixnum reqi) (type cons reqs))
     (if (eq (var-kind (first reqs)) 'SPECIAL)
	 (progn
	   (setf (second LCL_i) reqi)
	   (bind LCL_i (first reqs)))
	 (setf (var-loc (first reqs)) reqi)))

   ;; For each variable, set its var-loc.
   ;; For optional parameters, and lexical variables which can be unboxed,
   ;; this will be a new LCL.
   ;; The bind step later will assign to such variable.
   (let* ((req0 (if (eq 'CALL-LAMBDA kind) (- *lcl* nreq) *lcl*))
	  (lcl (+ req0 nreq)))
     (declare (fixnum lcl))
     (labels ((wt-decl (var)
		(wt-nl)
		(unless block-p
		  (wt "{") (setq block-p t))
		(wt *volatile* (register var) (rep-type (var-kind var)))
		(wt-lcl (incf lcl)) (wt ";")
		lcl)
	      (do-decl (var)
		(when (local var)	; no LCL needed for SPECIAL or LEX
		  (setf (var-loc var) (wt-decl var)))))
       (do ((reqs requireds (cdr reqs))
	    (reqi (1+ req0) (1+ reqi)) (var))
	   ((endp reqs))
	 (declare (fixnum reqi) (type cons reqs) (type var var))
	 (setq var (first reqs))
	 (when (unboxed var)
	   (setf (var-loc var) (wt-decl var)))) ; create unboxed variable
       (when (and rest (< (var-ref rest) 1)) ; dont create rest if not used
	 (setq rest nil))
       (when optionals
	 (unless block-p
	   (wt-nl "{") (setq block-p t))
	 (wt "int i=" (if (eq 'CALL-LAMBDA kind) 0 nreq) ";"))
       (dolist (opt optionals)
	 (do-decl (car opt))
	 (when (third opt) (do-decl (third opt))))
       (when rest (setq rest-loc `(LCL ,(wt-decl rest)))))

     (unless (eq 'CALL-LAMBDA kind)
       (when (or optionals rest)	; (not (null requireds))
	 (unless block-p
	   (wt-nl "{") (setq block-p t))
	 (wt-nl "va_list args; va_start(args, ")
	 (if (plusp nreq)
	     (wt-lcl (+ req0 nreq))
	     (if closure-p (wt "env0") (wt "narg")))
	 (wt ");")))

     ;; check arguments
     (when (or *safe-compile* *compiler-check-args*)
       (cond ((or (third lambda-list) ; rest=NIL if not used
		  optionals)
	      (when requireds
		(wt-nl "if(narg<" nreq ") FEtoo_few_arguments(&narg);"))
	      (unless (third lambda-list)
		(wt-nl "if(narg>" (+ nreq (length optionals))
		       ") FEtoo_many_arguments(&narg);")))
	     (t (wt-nl "check_arg(" nreq ");"))))

     ;; Bind required parameters.
     (do ((reqs requireds (cdr reqs))
	  (reqi (1+ req0) (1+ reqi))
	  (LCL_i (list 'LCL 0)))	; to allow concurrent compilations
	 ((endp reqs))
       (declare (fixnum reqi) (type cons reqs))
       (setf (second LCL_i) reqi)
       (bind LCL_i (first reqs)))

     (setq *lcl* lcl))
   )
  ;; Bind optional parameters as long as there remain arguments.
  (when optionals
    (let ((va-arg-loc `(VA-ARG ,(eq kind 'CALL-LAMBDA))))
      (dolist (opt optionals)
	(push (next-label) labels)
	(wt-nl "if (i==narg) ") (wt-go (car labels))
	(bind va-arg-loc (car opt))
	(when (third opt) (bind t (third opt)))
	(wt-nl "i++;")
	))
    (let ((label (next-label)))
      (wt-nl) (wt-go label)
      (setq labels (nreverse labels))
      ;;; Bind unspecified optional parameters.
      (dolist (opt optionals)
        (wt-label (car labels))
        (pop labels)
	(bind-init (first opt) (second opt) T)
        (when (third opt) (bind nil (third opt) t)))
      (wt-label label))
    )
  (when rest
    (wt-nl "narg -=" (+ nreq (length optionals)) ";")
    (wt-nl rest-loc)
    (if (eq (var-type rest) :DYNAMIC-EXTENT)
	(wt "=(ALLOCA_CONS(narg),ON_STACK_MAKE_LIST(narg));")
	(wt "=(Lmake_list(1,MAKE_FIXNUM(narg)), VALUES(0));"))
    (wt-nl "{ object p=" rest-loc ";")
    (wt-nl " for(;narg-->0;p=CDR(p))")
    (wt-nl "   CAR(p)=") (wt-va_arg (eq 'CALL-LAMBDA kind)) (wt ";}")
    (bind rest-loc rest))

  (when *tail-recursion-info*
        (push 'TAIL-RECURSION-MARK *unwind-exit*) (wt-nl1 "TTL:"))

  ;;; Now the parameters are ready!
  (c2expr body)

  (when block-p (wt-nl "}"))
  )

(defun c2lambda-expr-with-key
    (lambda-list body closure-p call-lambda cfun
		 &aux (requireds (first lambda-list))
		 (optionals (second lambda-list))
		 (rest (third lambda-list)) rest-loc
		 (key-flag (fourth lambda-list))
		 (keywords (fifth lambda-list))
		 (allow-other-keys (sixth lambda-list))
		 (nreq (length requireds))
		 (nkey (length keywords))
		 (labels nil)
		 (*unwind-exit* *unwind-exit*)
		 (*env* *env*)
		 (block-p nil)
		 (last-arg))
  (declare (fixnum nreq nkey))

  ;; For each variable, set its var-loc.
  ;; For optional and keyword parameters, and lexical variables which
  ;; can be unboxed, this will be a new LCL.
  ;; The bind step later will assign to such variable.
  (let* ((req0 (if call-lambda (- *lcl* nreq) *lcl*))
	 (lcl (+ req0 nreq)))
    (declare (fixnum lcl))
    (labels ((wt-decl (var)
               (wt-nl)
               (unless block-p
                 (wt "{") (setq block-p t))
               (wt *volatile* (register var) (rep-type (var-kind var)))
               (wt-lcl (incf lcl)) (wt ";")
               lcl)
             (do-decl (var)
	       (when (local var) ; no LCL needed for SPECIAL or LEX
		 (setf (var-loc var) (wt-decl var)))))
      (do ((reqs requireds (cdr reqs))
	   (reqi (1+ req0) (1+ reqi)) (var))
	  ((endp reqs))
	(declare (fixnum reqi) (type cons reqs) (type var var))
	(setq var (first reqs))
	(if (unboxed var)
	    (setf (var-loc var) (wt-decl var)))) ; create unboxed variable
      (when (and rest (< (var-ref rest) 1)) ; dont create rest if not used
	(setq rest nil))
      (when optionals
        (unless block-p
          (wt-nl "{") (setq block-p t))
        (wt "int i=" (if call-lambda 0 nreq) ";"))
      (dolist (opt optionals)
        (do-decl (car opt))
        (when (third opt) (do-decl (third opt))))
      (when rest (setq rest-loc `(LCL ,(wt-decl rest))))
      (dolist (key keywords)
        (do-decl (second key))
        (when (fourth key) (do-decl (fourth key)))))

    (unless call-lambda
      (unless block-p
	(wt-nl "{") (setq block-p t))
      (wt-nl "va_list args; va_start(args, ")
      (wt (setq last-arg (if (plusp nreq)
			     (format nil "V~d" (+ req0 nreq))
			     (if closure-p "env0" "narg"))))
      (wt ");"))

    ;; check arguments
    (when (and (or *safe-compile* *compiler-check-args*) requireds)
      (wt-nl "if(narg<" nreq ") FEtoo_few_arguments(&narg);"))

    ;; Bind required parameters.
    (do ((reqs requireds (cdr reqs))
	 (reqi (1+ req0) (1+ reqi))
	 (LCL_i (list 'LCL 0)))		; to allow concurrent compilations
	((endp reqs))
      (declare (fixnum reqi) (type cons reqs))
      (setf (second LCL_i) reqi)
      (bind LCL_i (first reqs)))

    (setq *lcl* lcl)
    )
  ;; Bind optional parameters as long as there remain arguments.
  (when optionals
    (let ((va-arg-loc `(VA-ARG ,call-lambda)))
      (dolist (opt optionals)
	(push (next-label) labels)
	(wt-nl "if (i==narg) ") (wt-go (car labels))
	(bind va-arg-loc (car opt))
	(when (third opt) (bind t (third opt)))
	(wt-nl "i++;")
	))
    (let ((label (next-label)))
      (wt-nl) (wt-go label)
      (setq labels (nreverse labels))
      ;;; Bind unspecified optional parameters.
      (dolist (opt optionals)
        (wt-label (car labels))
        (pop labels)
	(bind-init (first opt) (second opt) T)
        (when (third opt) (bind nil (third opt) T)))
      (wt-label label))
    )

  (wt-nl "narg -=" (+ nreq (length optionals)) ";")
  (when rest
    (wt-nl rest-loc)
    (if (eq (var-type rest) :DYNAMIC-EXTENT)
	(wt "=(ALLOCA_CONS(narg), ON_STACK_MAKE_LIST(narg));")
	(wt "=(Lmake_list(1, MAKE_FIXNUM(narg)), VALUES(0));"))
    ;; don't clobber narg, needed by parse_key
    (wt-nl "{ int n=narg; object p=" rest-loc ";")
    (wt-nl " for(;n-->0;p=CDR(p))")
    (wt-nl "   CAR(p)=") (wt-va_arg call-lambda) (wt ";}")
    (bind rest-loc rest))

  (wt-h "static intUobject L" cfun "keys[" (length keywords) "]={")
  (do ((kwds keywords (cdr kwds)))
      ((null kwds) (princ-h "};"))
      (princ-h (add-symbol (caar kwds)))
      (when (cdr kwds) (princ-h ",")))

  (wt-nl "{ object keyvars[" (* 2 nkey) "];")
  (when rest
    (if call-lambda
	(wt-nl "args -= narg;")
	(wt-nl "va_start(args," last-arg ");")))
  (wt-nl "parse_key(narg,args," (length keywords) ",L" cfun "keys,keyvars,")
  (if rest
      (wt rest-loc)
    (wt "OBJNULL"))
  (wt (if allow-other-keys ",TRUE);" ",FALSE);"))

  ;;; Bind keywords.
  (let ((KEYVARS[i] `(KEYVARS 0))	; create, since we clobber it
	(i 0))
    (declare (fixnum i))
    (dolist (kwd keywords)
      (cond ((and (eq (car (third kwd)) 'LOCATION)
		  (null (third (third kwd))))
	     ;; no initform
	     ;; Cnil has been set in keyvars if keyword parameter is not supplied.
	     (setf (second KEYVARS[i]) i)
	     (bind KEYVARS[i] (second kwd)))
	    (t
	     ;; with initform
	     (setf (second KEYVARS[i]) (+ nkey i))
	     (wt-nl "if(") (wt-loc KEYVARS[i]) (wt "==Cnil){")
	     (bind-init (second kwd) (third kwd) T)
	     (wt-nl "}else{")
	     (setf (second KEYVARS[i]) i)
	     (bind KEYVARS[i] (second kwd))
	     (wt "}")))
      (unless (eq (var-kind (fourth kwd)) 'DUMMY)
	(setf (second KEYVARS[i]) (+ nkey i))
	(bind KEYVARS[i] (fourth kwd)))
      (incf i))
    )
  (wt-nl "}")

  ;;; Now the parameters are ready, after all!
  (c2expr body)

  (when block-p (wt-nl "}"))
  )

(defun need-to-set-vs-pointers (lambda-list)
				;;; On entry to in-line lambda expression,
				;;; vs_base and vs_top must be set iff,
   (or *safe-compile*
       *compiler-check-args*
       (nth 1 lambda-list)	;;; optional,
       (nth 2 lambda-list)	;;; rest, or
       (nth 3 lambda-list)	;;; key-flag.
       ))


;;; The DEFMACRO compiler.

;;; valid lambda-list to DEFMACRO is:
;;;
;;;	( [ &whole sym ]
;;;	  [ &environment sym ]
;;;	  { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )
;;;
;;; where v is short for { defmacro-lambda-list | sym }.
;;; Defmacro-lambda-list is defined as:
;;;
;;;	( { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )

;(defvar *vnames*) -> vnames
;(defvar *dm-info*)-> dm-info
;(defvar *dm-vars*)-> dm-vars

(defun c1dm (macro-name vl body
                        &aux (whole nil) (env nil)
                        (vnames nil) (dm-info (make-info)) (dm-vars nil)
			(setjmps *setjmps*) ; Beppe
                        doc ss is ts other-decls ppn)

  (multiple-value-setq (body ss ts is other-decls doc) (c1body body t))
  (setq body (list (list* 'BLOCK macro-name body)))

  (c1add-globals ss)

  (when (and (listp vl) (eq (car vl) '&WHOLE))
        (push (second vl) vnames)
        (setq whole (c1make-var (second vl) ss is ts))
        (push whole dm-vars)
        (push whole *vars*)
        (setq vl (cddr vl))
        )
  (do ((x vl (cdr x)))
      ((atom x))
    (when (eq (car x) '&ENVIRONMENT)
      (push (second x) vnames)
      (setq env (c1make-var (second x) ss is ts))
      (push env dm-vars)
      (push env *vars*)
      (setq vl (nconc (ldiff vl x) (cddr x)))))

  (labels ((c1dm-vl (vl ss is ts)
	     (do ((optionalp nil) (restp nil) (keyp nil)
		  (allow-other-keys-p nil) (auxp nil)
		  (requireds nil) (optionals nil) (rest nil) (key-flag nil)
		  (keywords nil) (auxs nil) (allow-other-keys nil)
		  (n 0) (ppn nil))
		 ((not (consp vl))
		  (when vl
		    (when restp (dm-bad-key '&REST))
		    (setq rest (c1dm-v vl ss is ts)))
		  (values (list (nreverse requireds) (nreverse optionals) rest key-flag
				(nreverse keywords) allow-other-keys (nreverse auxs))
			  ppn)
		  )
	       (let ((v (car vl)))
		 (declare (object v))
		 (cond
		   ((eq v '&OPTIONAL)
		    (when optionalp (dm-bad-key '&OPTIONAL))
		    (setq optionalp t)
		    (pop vl))
		   ((or (eq v '&REST) (eq v '&BODY))
		    (when restp (dm-bad-key v))
		    (setq rest (c1dm-v (second vl) ss is ts))
		    (setq restp t optionalp t)
		    (setq vl (cddr vl))
		    (when (eq v '&BODY) (setq ppn n)))
		   ((eq v '&KEY)
		    (when keyp (dm-bad-key '&KEY))
		    (setq keyp t restp t optionalp t key-flag t)
		    (pop vl))
		   ((eq v '&ALLOW-OTHER-KEYS)
		    (when (or (not keyp) allow-other-keys-p)
		      (dm-bad-key '&ALLOW-OTHER-KEYS))
		    (setq allow-other-keys-p t allow-other-keys t)
		    (pop vl))
		   ((eq v '&AUX)
		    (when auxp (dm-bad-key '&AUX))
		    (setq auxp t allow-other-keys-p t keyp t restp t optionalp t)
		    (pop vl))
		   (auxp
		    (let (x init)
		      (cond ((symbolp v) (setq x v init (c1nil)))
			    (t (setq x (car v))
			       (if (endp (cdr v))
				   (setq init (c1nil))
				   (setq init (c1expr* (second v) dm-info)))))
		      (push (list (c1dm-v x ss is ts) init) auxs))
		    (pop vl))
		   (keyp
		    (let (x k init (sv nil))
		      (cond ((symbolp v)
			     (setq x v
				   k (intern (string v) 'KEYWORD)
				   init (c1nil)))
			    (t (if (symbolp (car v))
				   (setq x (car v)
					 k (intern (string (car v)) 'KEYWORD))
				   (setq x (cadar v) k (caar v)))
			       (cond ((endp (cdr v)) (setq init (c1nil)))
				     (t (setq init (c1expr* (second v) dm-info))
					(unless (endp (cddr v))
					  (setq sv (third v)))))))
		      (push (list k (c1dm-v x ss is ts) init
				  (if sv (c1dm-v sv ss is ts) nil))
			    keywords)
		      )
		    (pop vl))
		   (optionalp
		    (let (x init (sv nil))
		      (cond ((symbolp v) (setq x v init (c1nil)))
			    (t (setq x (car v))
			       (cond ((endp (cdr v))
				      (setq init (c1nil)))
				     (t (setq init (c1expr* (second v) dm-info))
					(unless (endp (cddr v))
					  (setq sv (third v)))))))
		      (push (list (c1dm-v x ss is ts) init
				  (if sv (c1dm-v sv ss is ts) nil))
			    optionals))
		    (pop vl)
		    (incf n)
		    )
		   (t (push (c1dm-v v ss is ts) requireds)
		      (pop vl)
		      (incf n))
		   )))
	     )

	   (c1dm-v (v ss is ts)
	     (cond ((symbolp v)
		    (push v vnames)
		    (setq v (c1make-var v ss is ts))
		    (push v *vars*)
		    (push v dm-vars)
		    v)
		   (t (c1dm-vl v ss is ts))))
	   )
    (multiple-value-setq (vl ppn) (c1dm-vl vl ss is ts)))

  (check-vdecl vnames ts is)
  (setq body (c1decl-body other-decls body))
  (add-info dm-info (second body))
  (unless (eql setjmps *setjmps*)
    (setf (info-volatile dm-info) t)
    (setf (get macro-name 'CONTAINS-SETJMP) t))
  (dolist (v dm-vars) (check-vref v))

  (list doc ppn whole env vl body)
  )

(defun c1dm-bad-key (key)
       (cmperr "Defmacro-lambda-list contains illegal use of ~s." key))

(defun c2dm (name whole env vl body &aux lcl)
  (when (or *safe-compile* *compiler-check-args*)
    (wt-nl "check_arg(2);"))
  (setq lcl (next-lcl))
  (when whole
    (check-vref whole)
;    (setf (var-loc whole) lcl)
    (bind (list 'LCL lcl) whole))
  (setq lcl (next-lcl))
  (when env
    (check-vref env)
;    (setf (var-loc env) lcl)
    (bind (list 'LCL lcl) env))
  (labels ((reserve-v (v)
	     (if (consp v)
		 (reserve-vl v)
		 (when (local v)
		   (let ((lcl (next-lcl)))
		     (setf (var-loc v) lcl)
		     (setf (var-kind v) 'OBJECT)
		     (wt ",") (wt-lcl lcl)))))

	   (reserve-vl (vl)
	     (dolist (var (car vl)) (reserve-v var))
	     (dolist (opt (second vl))
	       (reserve-v (car opt))
	       (when (third opt) (reserve-v (third opt))))
	     (when (third vl) (reserve-v (third vl)))
	     (dolist (kwd (fifth vl))
	       (reserve-v (second kwd))
	       (when (fourth kwd) (reserve-v (fourth kwd))))
	     (dolist (aux (seventh vl))
	       (reserve-v (car aux))))

	   (dm-bind-loc (v loc)
	     (if (consp v)
		 (let ((lcl (next-lcl)))
		   (wt-nl "{object ") (wt-lcl lcl) (wt "= " loc ";")
		   (dm-bind-vl v lcl)
		   (wt "}"))
		 (bind loc v)))

	   (dm-bind-init (para &aux (v (first para)) (init (second para)))
	     (if (consp v)
		 (let* ((*inline-blocks* 0) ; used by inline-args
			(lcl (next-lcl))
			(loc (second (first (inline-args (list init))))))
		   (wt-nl) (wt-lcl lcl) (wt "= " loc ";")
		   (dm-bind-vl v lcl)
		   (close-inline-blocks))
		 (bind-init v init)))

	   (dm-bind-vl (vl lcl &aux
			   (requireds (car vl)) (optionals (second vl))
			   (rest (third vl)) (key-flag (fourth vl))
			   (keywords (fifth vl))
			   (allow-other-keys (sixth vl))
			   (auxs (seventh vl))
			   )
	     (declare (object requireds optionals rest key-flag keywords
			      allow-other-keys auxs))
	     (do ((reqs requireds (cdr reqs)))
		 ((endp reqs))
	       (declare (object reqs))
	       (when (or *safe-compile* *compiler-check-args*)
		 (wt-nl "if(endp(") (wt-lcl lcl)
		 (wt "))FEinvalid_macro_call(VV[" (add-symbol name) "]);"))
	       (dm-bind-loc (car reqs) `(CAR ,lcl))
	       (when (or (cdr reqs) optionals rest key-flag
			 *safe-compile* *compiler-check-args*)
		 (wt-nl) (wt-lcl lcl) (wt "=CDR(") (wt-lcl lcl) (wt ");")))
	     (do ((opts optionals (cdr opts))
		  (opt))
		 ((endp opts))
	       (declare (object opts opt))
	       (setq opt (car opts))
	       (wt-nl "if(endp(") (wt-lcl lcl) (wt ")){")
	       (let ((*env* *env*)
		     (*unwind-exit* *unwind-exit*))
		 (dm-bind-init opt)
		 (when (third opt) (dm-bind-loc (third opt) nil))
		 )
	       (wt-nl "} else {")
	       (dm-bind-loc (car opt) `(CAR ,lcl))
	       (when (third opt) (dm-bind-loc (third opt) t))
	       (when (or (cdr opts) rest key-flag
			 *safe-compile* *compiler-check-args*)
		 (wt-nl) (wt-lcl lcl) (wt "=CDR(") (wt-lcl lcl) (wt ");"))
	       (wt "}"))
	     (when rest (dm-bind-loc rest `(LCL ,lcl)))
	     (when keywords
	       (let* ((lcl1 (next-lcl))
		      (loc1 `(LCL ,lcl1)))
		 (wt-nl "{object " loc1 ";")
		 (dolist (kwd keywords)
		   (wt-nl loc1 "=getf(") (wt-lcl lcl)
		   (wt ",VV[" (add-symbol (car kwd)) "],OBJNULL);")
		   (wt-nl "if(" loc1 "==OBJNULL){")
		   (let ((*env* *env*)
			 (*unwind-exit* *unwind-exit*))
		     (dm-bind-init (cdr kwd))
		     (when (fourth kwd) (dm-bind-loc (fourth kwd) nil))
		     (wt-nl "} else {"))
		   (dm-bind-loc (second kwd) loc1)
		   (when (fourth kwd) (dm-bind-loc (fourth kwd) t))
		   (wt "}"))
		 (wt "}")))
	     (when (and (or *safe-compile* *compiler-check-args*)
			(null rest)
			(null key-flag))
	       (wt-nl "if(!endp(") (wt-lcl lcl)
	       (wt "))FEinvalid_macro_call(VV[" (add-symbol name) "]);"))
	     (when (and (or *safe-compile* *compiler-check-args*)
			key-flag
			(not allow-other-keys))
	       (wt-nl "check_other_key(") (wt-lcl lcl) (wt "," (length keywords))
	       (dolist (kwd keywords)
		 (wt ",VV[" (add-symbol (car kwd)) "]"))
	       (wt ");"))
	     (dolist (aux auxs)
	       (dm-bind-init aux)))
	   )

    (setq lcl (next-lcl))
    (wt-nl "{object ") (wt-lcl lcl) (wt "=CDR(V1)")
    (reserve-vl vl)			; declare variables for pattern
    (wt ";")
    (dm-bind-vl vl lcl)
    )
  (c2expr body)
  (wt "}")
  )
