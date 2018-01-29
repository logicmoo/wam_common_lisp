;;;; CMPEVAL --  The Expression Dispatcher.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package 'compiler)

(defun c1expr* (form info)
  (setq form (c1expr form))
  (add-info info (second form) (eq (car form) 'FUNCTION))
  form)

(defun c1expr (form)
  (setq form (catch *cmperr-tag*
    (cond ((symbolp form)
	   (cond ((eq form nil) (c1nil))
		 ((eq form t) (c1t))
		 ((keywordp form)
		  (list 'LOCATION (make-info :type (object-type form))
			(list 'VV (add-object form))))
		 ((constantp form)
		  (let ((val (symbol-value form)))
		    (or (c1constant-value val nil)
			(list 'LOCATION (make-info :type (object-type val))
			      (list 'VV (add-constant form))))))
		 (t (c1var form))))
	  ((consp form)
	   (let ((fun (car form))
		 ;; #+cltl2
		 setf-symbol)
	     (cond ((symbolp fun)
		    (c1call-symbol fun (cdr form)))
		   ;; #+cltl2
		   ((setq setf-symbol (setf-namep fun))
		    (c1call-symbol setf-symbol (cdr form)))
		   ((and (consp fun) (eq (car fun) 'LAMBDA))
		    (c1call-lambda (cdr fun) (cdr form)))
		   ((and (consp fun) (eq (car fun) 'sys:|#,|))
		    (cmperr "Sharp-comma-macro was found in a bad place."))
		   (t (cmperr "The function ~s is illegal." fun)))))
	  (t (c1constant-value form t)))))
  (if (eq form '*cmperr-tag*) (c1nil) form))

(defun c1sharp-comma (arg)
  (c1constant-value (cons 'sys:|#,| arg) t))

(defvar *c1nil* (list 'LOCATION (make-info :type (object-type nil)) nil))
(defun c1nil () *c1nil*)
(defvar *c1t* (list 'LOCATION (make-info :type (object-type t)) t))
(defun c1t () *c1t*)

(defun c1call-symbol (fname args &aux fd)
  (cond ((setq fd (get fname 'c1special)) (funcall fd args))
	((setq fd (c1call-local fname))
	 (if (eq (car fd) 'CALL-LOCAL)
	     (let* ((info (make-info :sp-change t
				     :local-referred
				     (info-local-referred (second fd))
				     ))
		    (forms (c1args args info)))
		  (let ((return-type (get-local-return-type (third fd))))
		       (when return-type (setf (info-type info) return-type)))
		  (let ((arg-types (get-local-arg-types (third fd))))
		       ;;; Add type information to the arguments.
		    (when arg-types
		      (let ((fl nil))
			(dolist (form forms)
			  (cond ((endp arg-types) (push form fl))
				(t (push (and-form-type
					  (car arg-types) form
					  (car args))
					 fl)
				   (pop arg-types)
				   (pop args))))
			(setq forms (nreverse fl)))))
		  (list 'CALL-LOCAL info (cddr fd) forms))
	     (c1expr (cmp-expand-macro fd fname args))))
	((and (setq fd (get fname 'C1))
	      (inline-possible fname))
	 (funcall fd args))
	((and (setq fd (get fname 'C1CONDITIONAL))
	      (inline-possible fname)
	      (funcall fd args)))
	((setq fd (macro-function fname))
	 (c1expr (cmp-expand-macro fd fname args)))
	((setq fd (get fname 'COMPILER-MACRO))
	 (c1expr (cmp-eval `(funcall ',fd ',(cons fname args) nil))))
	((and (setq fd (get fname 'SYS::STRUCTURE-ACCESS))
	      (inline-possible fname)
	      ;;; Structure hack.
	      (consp fd)
	      (sys::fixnump (cdr fd))
	      (not (endp args))
	      (endp (cdr args)))
	 (case (car fd)
	   (VECTOR (c1expr `(svref ,(car args) ,(cdr fd)))) ; Beppe
	   (LIST (c1expr `(sys:list-nth ,(cdr fd) ,(car args))))
	   (t (c1structure-ref1 (car args) (car fd) (cdr fd)))
	   )
	 )
	((eq fname 'SYS:|#,|)
	 (cmperr "Sharp-comma-macro was found in a bad place."))
	(t (let* ((info (make-info
			 :sp-change (null (get fname 'NO-SP-CHANGE))))
		  (forms (c1args args info)))
	     (let ((return-type (get-return-type fname)))
	       (when return-type (setf (info-type info) return-type)))
	     (let ((arg-types (get-arg-types fname)))
	       ;; Add type information to the arguments.
	       (when arg-types
		 (do ((fl forms (cdr fl))
		      (fl1 nil)
		      (al args (cdr al)))
		     ((endp fl)
		      (setq forms (nreverse fl1)))
		   (cond ((endp arg-types) (push (car fl) fl1))
			 (t (push (and-form-type (car arg-types)
						 (car fl)
						 (car al))
				  fl1)
			    (pop arg-types))))))
	     (let ((arg-types (get fname 'ARG-TYPES)))
	       ;; Check argument types.
	       (when arg-types
		 (do ((fl forms (cdr fl))
		      (al args (cdr al)))
		     ((or (endp arg-types) (endp fl)))
		   (check-form-type (car arg-types)
				    (car fl) (car al))
		   (pop arg-types))))
#|
	;; Do this in c2call-global, when we know types of variables rebound
	;; by let
	     (case fname
	       (AREF
		(let ((etype (info-type (cadar forms))))
		  (when (or (and (eq etype 'STRING)
				 (setq etype 'CHARACTER))
			    (and (consp etype)
				 (or (eq (car etype) 'ARRAY)
				     (eq (car etype) 'VECTOR))
				 (setq etype (second etype))))
		    (setq etype
			  (type-and (info-type info) etype))
		    (unless etype
		      (cmpwarn "Type mismatch was found in ~s."
			       (cons fname args))
		      (setq etype T))	; assume no information
		    (setf (info-type info) etype))))
	       (SYS:ASET		; (sys:aset value array i0 ... in)
		(let ((etype (info-type (cadr (second forms)))))
		  (when (or (and (eq etype 'STRING)
				 (setq etype 'CHARACTER))
			    (and (consp etype)
				 (or (eq (car etype) 'ARRAY)
				     (eq (car etype) 'VECTOR))
				 (setq etype (second etype))))
		    (setq etype
			  (type-and (info-type info)
				    (type-and (info-type (cadr (first forms)))
					      etype)))
		    (unless etype
		      (cmpwarn "Type mismatch was found in ~s."
			       (cons fname args))
		      (setq etype T))
		    (setf (info-type info) etype)
		    (setf (info-type (cadr (first forms))) etype)
		    ))))
|#
	     (list 'CALL-GLOBAL info fname forms)))
	)
  )

(defun c1call-lambda (lambda-expr args &aux (info (make-info :sp-change t)))
  (setq args (c1args args info))
  (setq lambda-expr (c1lambda-expr lambda-expr))
  (add-info info (second lambda-expr))
  (list 'CALL-LAMBDA info lambda-expr args (gen-init-keywords lambda-expr)))

(defun c2expr (form)
  (if (eq (car form) 'CALL-GLOBAL)
;;; ----------------------------------------------------------------------
;;; Added optimization for proclaimed functions: Beppe
;;; ----------------------------------------------------------------------
    (let ((fname (car *tail-recursion-info*)))
      (c2call-global (third form) (fourth form) nil
		     (if (and
			  (last-call-p)
			  (symbolp fname) ; locally defined function are
					; represented as variables
			  (get fname 'PROCLAIMED-FUNCTION))
			 (get fname 'PROCLAIMED-RETURN-TYPE)
			 (info-type (second form)))))
    (if (or (eq (car form) 'LET)
	    (eq (car form) 'LET*))
      (let ((*volatile* (volatile (second form))))
	(declare (special *volatile*))
	(apply (get (car form) 'C2) (cddr form)))
      (apply (get (car form) 'C2) (cddr form)))))

(defun c2expr* (form)
  (let* ((*exit* (next-label))
	 (*unwind-exit* (cons *exit* *unwind-exit*))
	 (*lcl* *lcl*)
	 (*temp* *temp*))
    (c2expr form)
    (wt-label *exit*))
  )

(defun c1progn (forms &aux (fl nil))
  (cond ((endp forms) (c1nil))
	((endp (cdr forms)) (c1expr (car forms)))
	(t (let ((info (make-info)))
		(dolist (form forms)
			(setq form (c1expr form))
			(push form fl)
			(add-info info (second form)))
		(setf (info-type info) (info-type (cadar fl)))
		(list 'PROGN info (nreverse fl))
		)))
  )

(defun c2progn (forms)
  ;; c1progn ensures that the length of forms is not less than 1.
  (do ((l forms (cdr l)))
      ((endp (cdr l))
       (c2expr (car l)))
    (let ((*destination* 'TRASH)) (c2expr* (car l))))
  )

(defun c1args (forms info)
  (mapcar #'(lambda (form) (c1expr* form info)) forms))

;;; Structures

(defun c1structure-ref (args)
  (if (and (not *safe-compile*)         ; Beppe
	   (not (endp args))
	   (not (endp (cdr args)))
	   (consp (second args))
	   (eq (caadr args) 'QUOTE)
	   (not (endp (cdadr args)))
	   (symbolp (cadadr args))
	   (endp (cddadr args))
	   (not (endp (cddr args)))
	   (sys::fixnump (third args))
	   (endp (cdddr args)))
      (c1structure-ref1 (car args) (cadadr args) (third args))
      (let ((info (make-info)))
	(list 'CALL-GLOBAL info 'SYS:STRUCTURE-REF (c1args args info)))))

(defun c1structure-ref1 (form name index
			      &aux (info (make-info
					  :type (get-slot-type name index))))
  ;;; Explicitly called from c1expr and c1structure-ref.
  (list 'SYS:STRUCTURE-REF info (c1expr* form info) (add-symbol name) index))

(defun get-slot-type (name index)
  ;; default is t
  (type-filter
   (or (third (nth index (get name 'SYS::STRUCTURE-SLOT-DESCRIPTIONS))) 'T)))

(defun c2structure-ref (form name-vv index
			     &aux (*inline-blocks* 0))
  (let ((loc (second (car (inline-args (list form))))))
       (unwind-exit (list 'SYS:STRUCTURE-REF loc name-vv index)))
  (close-inline-blocks)
  )

(defun wt-structure-ref (loc name-vv index)
  (if *safe-compile*
      (wt "structure_ref(" loc ",VV[" name-vv "]," index ")")
      #+clos
      (wt "(" loc ")->in.in_slots[" index "]")
      #-clos
      (wt "(" loc ")->str.str_self[" index "]")))

(defun c1structure-set (args &aux (info (make-info)))
  (if (and (not *safe-compile*)         ; Beppe
	   (not (endp args))
	   (not (endp (cdr args)))
	   (consp (second args))
	   (eq (caadr args) 'QUOTE)
	   (not (endp (cdadr args)))
	   (symbolp (cadadr args))
	   (endp (cddadr args))
	   (not (endp (cddr args)))
	   (sys::fixnump (third args))
	   (not (endp (cdddr args)))
	   (endp (cddddr args)))
      (let ((x (c1expr (car args)))
	    (y (c1expr (fourth args)))
	    (name (cadadr args)))       ; remove QUOTE.
	(add-info info (second x))
	(add-info info (second y))
	;; Beppe. Type check added:
	(let* ((slot-type (get-slot-type name (third args)))
	       (new-type (type-and slot-type (info-type (second y)))))
	  (if new-type                  ; compatible
	      (if (eq 'VAR (car y))     ; it's a variable
		(setf (var-type (car (third y))) new-type)   ; propagate type
		(setf (info-type (second y)) new-type))
	      (cmpwarn "The type of the form ~s is not ~s."
		       (fourth args) slot-type)))
	(setf (info-type info) (info-type (second y)))
	(list 'SYS:STRUCTURE-SET info x
	      (add-symbol name)
	      (third args) y))
      (list 'CALL-GLOBAL info 'SYS:STRUCTURE-SET (c1args args info))))

(defun c2structure-set (x name-vv index y
			  &aux locs (*inline-blocks* 0))
  ;; the third argument here *c1t* is just a hack to ensure that
  ;; a variable is introduced for y if it is an expression with side effects
  (setq locs (inline-args (list x y *c1t*)))
  (setq x (second (first locs)))
  (setq y (second (second locs)))
  (if *safe-compile*
      (wt-nl "structure_set(" x ",VV[" name-vv "]," index "," y ");")
      #+clos
      (wt-nl "(" x ")->in.in_slots[" index "]= " y ";")
      #-clos
      (wt-nl "(" x ")->str.str_self[" index "]= " y ";"))
  (unwind-exit y)
  (close-inline-blocks)
  )

;;; ----------------------------------------------------------------------
;;; Instances

#+clos
(defun c1instance-ref (args &aux (info (make-info)))
  (let ((form (first args))
	(index (second args)))
    (if (sys::fixnump index)
	(list 'SYS:INSTANCE-REF info (c1expr* form info) index)
      (list 'CALL-GLOBAL info 'SYS:INSTANCE-REF (c1args args info)))))

#+clos
(defun c2instance-ref (form index
			    &aux (*inline-blocks* 0))
  (let ((loc (second (car (inline-args (list form))))))
    (unwind-exit (list 'SYS:INSTANCE-REF loc index)))
  (close-inline-blocks)
  )

#+clos
(defun wt-instance-ref (loc index)
  (if *safe-compile*
      (wt "instance_ref(" loc "," index ")")
      (wt "(" loc ")->in.in_slots[" index "]")))

;;; ----------------------------------------------------------------------

(defun c1constant-value (val always-p)
  (cond
   ((eq val nil) (c1nil))
   ((eq val t) (c1t))
   ((sys::fixnump val)
    (list 'LOCATION (make-info :type 'FIXNUM)
	  (list 'FIXNUM-VALUE val)))
   ((characterp val)
    (list 'LOCATION (make-info :type 'CHARACTER)
	  (list 'CHARACTER-VALUE (char-code val))))
   ((typep val 'LONG-FLOAT)
    (list 'LOCATION (make-info :type 'LONG-FLOAT)
	  (list 'LONG-FLOAT-VALUE val (add-object val))))
   ((typep val 'SHORT-FLOAT)
    (list 'LOCATION (make-info :type 'SHORT-FLOAT)
	  (list 'SHORT-FLOAT-VALUE val (add-object val))))
   (always-p
    (list 'LOCATION (make-info :type (object-type val))
	  (list 'VV (add-object val))))
   (t nil)))

(defmacro sys::define-compiler-macro (name vl &rest body)
  `(progn (setf (get ',name 'COMPILER-MACRO)
		(sys::expand-defmacro ',name ',vl ',body))
	  ',name))  

(defun sys::undef-compiler-macro (name)
  (remprop name 'COMPILER-MACRO))

(defvar *compiler-temps*
	'(tmp0 tmp1 tmp2 tmp3 tmp4 tmp5 tmp6 tmp7 tmp8 tmp9))

(defmacro sys::define-inline-function (name vars &body body)
  (let ((temps nil)
	(*compiler-temps* *compiler-temps*))
    (dolist (var vars)
      (if (and (symbolp var)
	       (not (member var '(&OPTIONAL &REST &KEY &AUX) :test #'eq)))
	(push (or (pop *compiler-temps*)
		  (gentemp "TMP" (find-package 'COMPILER)))
	      temps)
	(error "The parameter ~s for the inline function ~s is illegal."
	       var name)))
    (let ((binding (cons 'LIST (mapcar
				#'(lambda (var temp) `(list ',var ,temp))
				vars temps))))
      `(progn
	 (defun ,name ,vars ,@body)
	 (sys::define-compiler-macro ,name ,temps
				    (list* 'LET ,binding ',body))))))

;;; ----------------------------------------------------------------------

(setf (get 'PROGN 'C1SPECIAL) 'c1progn)
(setf (get 'PROGN 'C2) 'c2progn)

(setf (get 'sys:|#,| 'c1 ) 'c1sharp-comma)

(setf (get 'SYS:STRUCTURE-REF 'C1) 'c1structure-ref)
(setf (get 'SYS:STRUCTURE-REF 'C2) 'c2structure-ref)
(setf (get 'SYS:STRUCTURE-REF 'WT-LOC) 'wt-structure-ref)
(setf (get 'SYS:STRUCTURE-SET 'C1) 'c1structure-set)
(setf (get 'SYS:STRUCTURE-SET 'C2) 'c2structure-set)

#+clos
(setf (get 'SYS:INSTANCE-REF 'C1) 'c1instance-ref)
#+clos
(setf (get 'SYS:INSTANCE-REF 'C2) 'c2instance-ref)
#+clos
(setf (get 'SYS:INSTANCE-REF 'WT-LOC) 'wt-instance-ref)
