;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package 'CLOS)

(defun legal-generic-function-p (name)
  (cond ((not (fboundp name)))
	; a generic function already exists
	((si:gfunp (symbol-function name)))
	((special-form-p name)
	 (error "~A is a special form" name))
	((macro-function name)
	 (error "~A is a macro" name))
	(t 
	 (error "~A is a lisp function" name))))

(defmacro defgeneric (&rest args)
  (multiple-value-bind (function-specifier lambda-list options)
    (parse-defgeneric args)
    (when (legal-generic-function-p function-specifier)
      (parse-lambda-list lambda-list)
      
      ;; process options
      (multiple-value-bind (argument-precedence-order 
			    declaration documentation method-combination
			    generic-function-class method-class method-list) 
	(parse-generic-options options lambda-list)
	(unless generic-function-class
	  (setq generic-function-class 'STANDARD-GENERIC-FUNCTION))
	(unless method-class (setq method-class 'STANDARD-METHOD))
	(unless argument-precedence-order
	  (setq argument-precedence-order ':DEFAULT))
	#|
	       (if (fboundp function-specifier)
		   ;; remove methods defined by previous defgeneric
		   (setf (methods 
			  (si:gfun-instance 
			   (symbol-function function-specifier)) nil)))
|#
	`(progn
	   (ensure-generic-function ',function-specifier
				    :lambda-list ',lambda-list
				    :argument-precedence-order 
				    ',argument-precedence-order
				    :declare ',declaration
				    :documentation ',documentation
				    :generic-function-class 
				    ',generic-function-class
				    :method-combination
				    ',method-combination
				    :method-class ',method-class)
	   ;,(dolist (method method-list)
	   ;; add methods specified by defgeneric
	   ;)
	   )))))

(defmacro generic-function (&rest args)
  (multiple-value-bind (lambda-list options)
    (parse--generic-function args)
    
    (parse-lambda-list lambda-list)
    ;; process options
    (multiple-value-bind (argument-precedence-order 
			  declaration documentation method-combination
			  generic-function-class method-class method-list) 
      (parse-generic-options options lambda-list)
      (unless generic-function-class
	(setq generic-function-class 'STANDARD-GENERIC-FUNCTION))
    (unless method-class
      (setq method-class 'STANDARD-METHOD))
    (unless argument-precedence-order
      (setq argument-precedence-order ':DEFAULT))
    `(progn
       (let* ((dispatcher (make-gfun nil ',lambda-list))
	      (gf-object (make-instance 
			  ',generic-function-class
			  :lambda-list ',lambda-list
			  :argument-precedence-order ',argument-precedence-order
			  :method-combination ',method-combination
			  :method-class ',method-class
			  :documentation ',documentation
			  :gfun dispatcher)))
	 (setf (si:gfun-instance dispatcher) gf-object)
	 ;,(dolist (method method-list)
	 ;; add methods specified by defgeneric
	 ;)
	 gf-object)))))
		
;;; ----------------------------------------------------------------------
;;;                                                                parsing

(defun parse-defgeneric (args)
  ;; (values function-specifier lambda-list options)
  (let (function-specifier)
    (unless args
      (error "Illegal defgeneric form: missing generic function name"))
    (setq function-specifier (pop args))
    (unless (legal-generic-function-name-p function-specifier)
      (error "~A cannot be a generic function specifier.~%~
             It must be either a non-nil symbol or ~%~
             a list whose car is SETF and whose cadr is a non-nil symbol."
	     function-specifier))
    (unless args
      (error "Illegal defgeneric  form: missing lambda-list"))
    (values function-specifier (first args) (rest args))))

(defun parse-generic-function (args)
  ;; (values lambda-list options)
  (unless args
    (error "Illegal generic-function form: missing lambda-list"))
  (values (first args) (rest args)))
	
(defun parse-generic-options (options lambda-list)
  (let (argument-precedence-order
	declaration
	documentation
	method-combination
	generic-function-class 
	method-class
	method-list)
    (dolist (option options)
	    (case (first option)
		  (:argument-precedence-order
		   (if argument-precedence-order
		       (error "Option :argument-precedence-order specified more than once")
		     (setq argument-precedence-order
			   (parse-parameter-names
			    (second option) lambda-list))))
		  (declare
		   (if declaration
		       (error "Option declare specified more than once")
		     (setq declaration
			   (parse-legal-declaration
			    (second option)))))
		  (:documentation
		   (if documentation
		       (error "Option :documentation specified more than once")
		     (setq documentation
			   (parse-legal-documentation
			    (second option)))))
		  (:method-combination
		   (if method-combination
		       (error "Option :method-combination specified more than \
once")
		     (setq method-combination
			   ;(parse-legal-method-combination
			    ;(second option))
			   ; until method-combination is implemented
			   (second option))))
		  (:generic-function-class
		   (if generic-function-class
		       (error "Option :generic-function-class specified more \
than once")
		     (setq generic-function-class
			   (legal-generic-function-classp (second option)))))
		  (:method-class
		   (if method-class
		       (error "Option :method-class specified more than once")
		     (setq method-class
			   (parse-legal-method-class (second option)))))
		  (:method
		   (push (parse-legal-method-list (second option))
			  method-list))
		  (otherwise 
		   (error "~S is not a legal defgeneric option" 
			  (first option)))))
    (values argument-precedence-order declaration documentation 
	    method-combination generic-function-class  method-class 
	    method-list)))

(defun ensure-generic-function (function-specifier 
				&key lambda-list
				     argument-precedence-order
				     declare
				     documentation
				     (generic-function-class
				      'STANDARD-GENERIC-FUNCTION)
				     (method-combination 'STANDARD)
				     (method-class 'STANDARD-METHOD)
				     environment)
  (unless (LEGAL-GENERIC-FUNCTION-NAME-P function-specifier)
	  (error "Generic function ~A has incorrect function specifier
                  (a non-nil symbol, a list whose car is SETF)"
		 function-specifier))
  (when (LEGAL-GENERIC-FUNCTION-P function-specifier)
	(unless (classp method-class)
		(setq method-class 
		      (find-class method-class)))

	(let (dispatcher gf-object)
	  (if (and (fboundp function-specifier)
		   (si:gfunp 
		    (setq dispatcher (symbol-function function-specifier))))

	      ;; modify the existing object
	      (progn
		(setf gf-object
		      (si:gfun-instance dispatcher)
		      (slot-value gf-object 'ARGUMENT-PRECEDENCE-ORDER)
		      argument-precedence-order
		      (slot-value gf-object 'DOCUMENTATION)
		      documentation
		      (generic-function-method-combination gf-object)
		      method-combination
		      (slot-value gf-object 'METHOD-CLASS)
		      method-class)
		;;(if (or
		;;     (not (method-exist-p function-specifier))
		;;     (congruent-lambda-list-p lambda-list 
		;;		    (lambda-list gf-object)))
		;;   (setf (lambda-list generic-function) lambda-list))
		;;(when (and generic-function-class
		;;           (compatible-p generic-function-class
		;;                         (generic-function-class gf-object)))
		;;     (change-class gf-object generic-function-class))
		)
	    ;; else create a new generic function object
	    (setf dispatcher (make-gfun function-specifier lambda-list)
		  gf-object (make-instance 
			     generic-function-class
			     :lambda-list lambda-list
			     :argument-precedence-order argument-precedence-order
			     :method-combination method-combination
			     :method-class method-class
			     :documentation documentation
			     :gfun dispatcher)
		  (si:gfun-instance dispatcher) gf-object
		  (symbol-function function-specifier) dispatcher))
	  gf-object)))

;;; ----------------------------------------------------------------------
;;;                                                             congruence

(defun congruent-lambda-list-p (l1 l2)
  (let (post-keyword)
    (do ((scan1 l1 (cdr scan1))
	 (scan2 l2 (cdr scan2)))
	((and (null scan1) (null scan2)))
	(cond ((and (not post-keyword)
		    (or
		     (and 
		      (member (first scan1) 
			      '(&REST &KEY &ALLOW-OTHER-KEYS &AUX))
		      (not (member (first scan2) 
			      '(&REST &KEY &ALLOW-OTHER-KEYS &AUX))))
		     (and 
		      (member (first scan2) 
			      '(&REST &KEY &ALLOW-OTHER-KEYS &AUX))
		      (not (member (first scan1) 
			      '(&REST &KEY &ALLOW-OTHER-KEYS &AUX))))))
	       (error "the two lambda lists ~S ~S have not the same number
                       of required parameters" l1 l2))
	      ((eq (first scan1) '&OPTIONAL)
	       (unless (eq (first scan2) '&OPTIONAL)
		       (error "the two lambda lists ~S ~S have not the same 
                        number of optional parameters" l1 l2))
	       (do ((scan-op1 (cdr scan1) (cdr scan-op1))
		    (scan-op2 (cdr scan2) (cdr scan-op2)))
		    ((and (null scan-op1) (null scan-op2)))
		    (cond ((or (null scan-op2)
			      (member (car scan-op2) 
				      '(&REST &KEY &ALLOW-OTHER-KEYS &AUX)))
			  (error "the two lambda lists ~S ~S have not the same 
                        number of optional parameters" l1 l2)))))))))

;;; ----------------------------------------------------------------------
;;;                                                                parsing

(defun parse-lambda-list (lambda-list &optional post-keyword)
  (let ((arg (car lambda-list)))
    (cond ((null lambda-list))
	  ((eq arg '&AUX)
	   (error "&aux is not allowed in a generic function lambda-list"))
	  ((member arg lambda-list-keywords)
	   (parse-lambda-list (cdr lambda-list) t))
	  (post-keyword
	   ;; After a lambda-list-keyword there can be no specializers.
	   (parse-lambda-list (cdr lambda-list) t))
	  (t
	   (if (listp arg)
	       (error "the parameters cannot be specialized in generic function lambda-list")
	       (parse-lambda-list (cdr lambda-list)))))))

(defun parse-parameter-names (parameter-list lambda-list)
  (let (required-list count)
    (setf required-list
	  (do ((l lambda-list (cdr l)))
	      ((or (null l)
		   (member (car l) '(&REST &KEY &ALLOW-OTHER-KEYS)))
	       (nreverse required-list))
	      (push l required-list)))
    (dolist (l required-list parameter-list)
	    (setf count (count l parameter-list))
	    (when (not (= count 1))
		  (error "the required parameter ~S must be included
                                exactly once in the argument-precedence-order 
                                option" l)))))

(defun parse-legal-declaration (decl)
  (unless (eq (car decl) 'OPTIMIZE)
	  (error "The only declaration allowed is optimize"))
  (do* ((d (cdr decl) (cdr d))
	(first (car d) (car d)))
       ((null d) decl)
       (unless (member (car first) '(SPEED SPACE))
	       (error "The only qualities allowed are speed and space"))))

(defun parse-legal-documentation (doc)
  (unless (stringp doc)
	  (error "The documentation must be a string"))
  doc)

(defun parse-legal-method-combination (name args)
  (unless (method-combination-p name)
	  (error "~A is not the name of a method-combination type" name))
  (unless (legal-method-combination-args name args)
	  (error "~S are not legal args for the method combination type ~A"
		 args name))
  (values name args))

(defun legal-generic-function-classp (class-name)
  ; until we don't know when a class can be the class of a generic function
  (unless (subtypep (find-class class-name) 
		    (find-class 'GENERIC-FUNCTION))
	  (error "The class ~S cannnot be the class of a generic function" 
		 class-name))
  class-name)

(defun parse-legal-method-class (class-name)
  ; until we don't know when a class can be the class of a method
  (error "At the moment the class of a method can be only standard-method"))

(defun parse-legal-method-list (methods-list)
  methods-list)

