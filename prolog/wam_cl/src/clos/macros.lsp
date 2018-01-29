;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package 'CLOS :use (list (or (find-package 'WALKER)
				 (make-package 'WALKER :use '(LISP)))
			     'LISP))

;;; ----------------------------------------------------------------------

;(proclaim '(DECLARATION VARIABLE-REBINDING))
;;; Make this stable:
(defvar compiler::*alien-declarations* '(VARIABLE-REBINDING))

(defvar *keyword-package* (find-package 'KEYWORD))

(defun make-keyword (symbol)
  (intern (symbol-name symbol) *keyword-package*))

(defmacro doplist ((key val) plist &body body)
  `(let ((.plist-tail. ,plist) ,key ,val)
     (loop (when (null .plist-tail.) (return nil))
	   (setq ,key (pop .plist-tail.))
	   (when (null .plist-tail.)
	     (error "Malformed plist in doplist, odd number of elements."))
	   (setq ,val (pop .plist-tail.))
	   (progn ,@body))))
  ;;
;;;;;; FIND-CLASS  naming classes.
  ;;
;;;
;;; (FIND-CLASS <name>) returns the class named <name>.  setf can be used
;;; with find-class to set the class named <name>.  These are "extrinsic"
;;; names.  Neither find-class nor setf of find-class do anything with the
;;; name slot of the class, they only lookup and change the association from
;;; name to class.
;;; 

;(defvar *class-name-hash-table* (make-hash-table :test #'eq)
; "The hash table containing all classes")

(defun find-class (name &optional (errorp t) environment)
  (declare (ignore environment))
  (let ((class (gethash name si:*class-name-hash-table*)))
    (cond (class class)
	  (errorp (error "No class named ~S." name))
	  (t nil))))

;;; This is only used during boot. The real one is in built-in.
(defun setf-find-class (name new-value)
  (if (classp new-value)
      (setf (gethash name si:*class-name-hash-table*) new-value)
    (error "~A is not a class." new-value)))

(defsetf find-class setf-find-class)

(defun legal-class-name-p (x)
  (and (symbolp x)
       (not (keywordp x))))

;;; ----------------------------------------------------------------------

(defmacro keyword-bind (keywords form &body body)
  `(apply (function (lambda (&key . ,keywords) . ,body)) ,form))

;;;----------------------------------------------------------------------
;;; Implementation of Instance

;;; ECL implementation:

(proclaim '(function si:instance-ref (t fixnum) t))
(proclaim '(function si:instance-set (t fixnum t) t))

;;; ----------------------------------------------------------------------
;;; Class CLASS
;;;
;;; The slots in Class Class are:
;;; name superiors inferiors slots methods

(defmacro class-name		(class) `(si:instance-ref ,class 0))
(defmacro class-superiors	(class) `(si:instance-ref ,class 1))
(defmacro class-inferiors	(class) `(si:instance-ref ,class 2))
(defmacro class-slots		(class) `(si:instance-ref ,class 3))

;;; ----------------------------------------------------------------------
;;; STANDARD-CLASS

;;; for compiler optimization to work, a-standard-class should be
;;; a variable declared of type standard-class.
(defmacro slot-index-table (a-standard-class)
  `(the hash-table (si:instance-ref ,a-standard-class 5)))

;;; ----------------------------------------------------------------------
;;; Low level printing
;;;

(defmacro print-unreadable-object ((thing stream) &body body)
  (let ((var (gensym)))
    `(let* ((,var ,stream)
	    (*print-level* (when (numberp *print-level*) (- *print-level* 1))))
      (princ "#<" ,var)
      ,@body
      (princ " " ,var)
      (format ,var "~O" (si:pointer ,thing))
      (princ ">" ,var))))

;;; ----------------------------------------------------------------------
;;; symbol-macrolet

(defmacro symbol-macrolet (symbol-expansions &body body &environment env)
  (labels ((translate (form context env &aux symbol-expansion)
	    (cond ((and (symbolp form)
			(eq context ':EVAL)
			(null (variable-lexical-p form env))
			(null (variable-special-p form env))
			(setq symbol-expansion (assoc form symbol-expansions)))
		   (second symbol-expansion))
		  ((not (listp form)) form)
		  ((eq 'SETQ (car form))
		   (if (cdddr form)	; multiple setq
		       `(progn (setq ,(cadr form) ,(caddr form))
			       (setq . ,(cdddr form)))
		     (if (and (symbolp (second form))
			      (null (variable-lexical-p (second form) env))
			      (null (variable-special-p (second form) env))
			      (setq symbol-expansion
				    (assoc (second form) symbol-expansions)))
			 (list 'SETF (second symbol-expansion) (third form))
		       form)))
		  ((eq (car form) 'MULTIPLE-VALUE-SETQ)
		   (let* ((vars (cadr form))
			  (gensyms (mapcar #'(lambda (x) x (gensym)) vars)))
		     `(multiple-value-bind
			  ,gensyms 
			,(caddr form)
			.,(mapcar #'(lambda (v g) `(setf ,v ,g))
				  vars
				  gensyms))))
		  ((eq (car form) 'SETF)
		   ;; avoid anticipated expansion of setf into funcall
		   (values (cons 'SETF
				 (mapcar #'(lambda (form)
					     (walk-form form env #'translate))
					 (cdr form)))
			   t))
		  (t form))))
    `(progn
       ,@(mapcar
	  #'(lambda (form)
	      (walk-form form env #'translate))
	  body))))

;(symbol-macrolet ((y new-y)) (let ((a 1)) #'(lambda (x) (list a y (setq y x)))))

;;; ----------------------------------------------------------------------
;;;
(defCbody set-compiled-function-name (object object) object
 "((type_of(#0) == t_cfun) || (type_of(#0) == t_cclosure))
     ? ((#0)->cf.cf_name = (#1))
     : (object)FEerror(\"~S is not a compiled-function.\", 1, (#0))")

(defun set-function-name (fn new-name)
  (cond ((typep fn 'COMPILED-FUNCTION)
	 (set-compiled-function-name fn new-name))
	((and (listp fn)
	      (eq (car fn) 'LAMBDA-BLOCK))
	 (setf (cadr fn) new-name))
	((and (listp fn)
	      (eq (car fn) 'LAMBDA))
	 (setf (car fn) 'LAMBDA-BLOCK
	       (cdr fn) (cons new-name (cdr fn)))))
  fn)

