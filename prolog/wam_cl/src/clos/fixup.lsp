;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package 'CLOS)

;;; ----------------------------------------------------------------------
;;; Fixup

(defun fix-early-methods ()
  (dolist (method-info *early-methods*)
    (let* ((method-name (car method-info))
	   (gfun (symbol-function method-name))
	   (gf-object (si:gfun-instance gfun)))

      (when (eq 'T (class-name (si:instance-class gf-object)))
	;; complete the generic function object
	(si:instance-class-set gf-object
			       (find-class 'STANDARD-GENERIC-FUNCTION))
	(si:instance-set gf-object 4
			 (find-class 'STANDARD-METHOD))	; method-class
	)

      (dolist (method (cdr method-info))
	;; we must use CxR here since method accessors have been redefined
	(let* (;;(class-name (car method))
	       (qualifiers (cadr method))
	       (specializers (caddr method))
	       (lambda-list (cadddr method))
	       (function (nth 4 method))
	       (plist (nth 5 method))
	       (options (nth 6 method)))

	  ;; create the method instance according
	  ;; to the information previously saved
	  (add-method gf-object
		      (make-method qualifiers specializers lambda-list
				   function plist options gf-object
				   'STANDARD-METHOD))))))

      (fmakunbound 'FIX-EARLY-METHODS)
      (makunbound '*EARLY-METHODS*))


;;; ----------------------------------------------------------------------
;;;                                                              redefined

(defun make-method (qualifiers specializers arglist
			       function plist options gfun method-class)
  (declare (ignore options))
  (make-instance method-class
		 :generic-function gfun
		 :qualifiers qualifiers
		 :lambda-list arglist
		 :specializers specializers
		 :method-function function
		 :plist plist
		 :allow-other-keys t))

(defun get-method-qualifiers (method)
  (if (typep method 'STANDARD-METHOD)
      (si:instance-ref method 3)
    (error "~A is not a standard method" method)))

(defun add-method (gf method)
  (declare (notinline method-qualifiers)) ; during boot it's a structure accessor
  (let ((method-qualifiers (method-qualifiers method)) 
	(specializers (specializers method))
	found)
    (when (setq found (find-method gf method-qualifiers specializers nil))
	  (remove-method gf found))
  (push method (methods gf))
  (clrhash (si:gfun-method-ht (generic-function-dispatcher gf)))
  method))

(defun remove-method (gf method)
  (setf (methods gf) (delete method (methods gf))))

;;; ----------------------------------------------------------------------
;;;                                                           do the fixup

(fix-early-methods)

;;; ----------------------------------------------------------------------
;;;                                                  continue redefinition

(defun method-needs-next-methods-p (method)
  (getf (si:instance-ref method 7) :needs-next-methods-p))

(defun method-p (method) (typep method 'METHOD))

(defun get-method-function (method)
  (if (typep method 'STANDARD-METHOD)
      (si:instance-ref method 4)
    (error "~A is not a standard method" method)))

(defun si:compute-applicable-methods (gf args)
  (let ((methods (methods gf))
	applicable-list
	args-specializers)
    ;; first compute the applicable method list
    (dolist (method methods)
      ;; for each method in the list
      (do* ((scan-args args (cdr scan-args))
	    (scan-specializers (specializers method)
			       (cdr scan-specializers))
	    (arg)
	    (spec))
	  ;; check if the method is applicable verifying 
	  ;; if each argument satisfies the corresponding
	  ;; parameter specializers
	  ((null scan-args) (push method applicable-list))
	(setq arg (first scan-args)
	      spec (first scan-specializers))
	(unless (or (null spec)
		    (and (consp spec) (eql arg (cadr spec)))
		    (typep arg spec)
		    (and (eq 'INVALID spec)
			 (si:instancep arg)
			 (eq 'INVALID (class-name (class-of arg)))))
	  (return))))
    (dolist (arg args) 
      (push (type-of arg) args-specializers))
    (setq args-specializers (nreverse args-specializers))
    ;; then order the list
    (do* ((scan applicable-list)
	  (most-specific (first scan) (first scan))
	  (ordered-list))
	 ((null (cdr scan)) (when most-specific
			      ;; at least one method
			      (nreverse
			       (push most-specific ordered-list))))
      (dolist (meth (cdr scan))
	(when (eq (compare-methods most-specific
				   meth args-specializers) 2)
	  (setq most-specific meth)))
      (setq scan (delete most-specific scan))
      (push most-specific ordered-list))))

;;; ----------------------------------------------------------------------
;;;                                                      method comparison

(defun compare-methods (method-1 method-2 args-specializers)
  (let ((specializers-list-1 (specializers method-1))
	(specializers-list-2 (specializers method-2)))
    (compare-specializers-lists specializers-list-1 
				specializers-list-2 args-specializers)))

(defun compare-specializers-lists (spec-list-1 spec-list-2 args-specializers)
  (when (or spec-list-1 spec-list-2)
    (ecase (compare-specializers (first spec-list-1)
				 (first spec-list-2)
				 (first args-specializers))
      (1 '1)
      (2 '2)
      (= 
       (compare-specializers-lists (cdr spec-list-1)
				   (cdr spec-list-2)
				   (cdr args-specializers)))
      ((nil)
       (error "The type specifiers ~S and ~S can not be disambiguated~
                  with respect to the argument specializer: ~S"
	      (or (car spec-list-1) t)
	      (or (car spec-list-2) t)
	      (car args-specializers)))))
  )

(defun compare-specializers (spec-1 spec-2 arg-spec)
  (let* ((arg-class (closest-class arg-spec))
	 (cpl (cons arg-class
		    (if (typep arg-class 'STANDARD-CLASS)
			(slot-value arg-class 'CLASS-PRECEDENCE-LIST)
			(compute-class-precedence-list
			 arg-spec
			 (class-superiors arg-class)))))
	 (cpl-names))
    (setq cpl-names (dolist (e cpl (nreverse cpl-names))
		      (push (class-name e) cpl-names)))
    (cond ((equal spec-1 spec-2) '=)
	  ((null spec-1) '2)
	  ((null spec-2) '1)
	  ((subtypep spec-1 spec-2) '1)
	  ((subtypep spec-2 spec-1) '2)
	  ((and (listp spec-1) (eq (car spec-1) 'eql)) '1) ; is this engough?
	  ((and (listp spec-2) (eq (car spec-2) 'eql)) '2) ; Beppe
	  ((member spec-1 (member spec-2 cpl-names)) '2)
	  ((member spec-2 (member spec-1 cpl-names)) '1)
	  (t (compare-complex-specializers spec-1 spec-2 arg-spec)))))

(defun compare-complex-specializers (spec-1 spec-2 arg-spec)
  (declare (ignore spec-1 spec-2 arg-spec))
  (error "Complex type specifiers are not yet supported."))

(defun si:compute-effective-method (gf applicable-methods
				    method-combination-type
				    method-combination-args)
  (declare (ignore method-combination-type method-combination-args))
  (if applicable-methods
      (compute-combined-method gf applicable-methods)
    (no-applicable-method gf)))

(defun no-applicable-method (gf)
  (error "No applicable method for ~S" 
	 (si:gfun-name (generic-function-dispatcher gf))))

(defun no-next-method (gf method)
  (declare (ignore gf))
  (error "No next method for method ~A" method))


;;; ----------------------------------------------------------------------
;;; Redefinition Protocol

(defun redefine-class (class new-class superclasses-names inferiors)
  (unless (typep class 'STANDARD-CLASS)
    (error "Class ~A cannot be redefined: it is not a standard class" class))

  ;; remove previous defined accessor methods
  (remove-optional-slot-accessors class)
  (let ((superclasses (mapcar #'find-class 
			      (or superclasses-names
				  (list 'STANDARD-OBJECT)))))
    ;; update subclasses
    (dolist (subclass (nreverse inferiors))
      (let ((superclasses
	     (mapcar #'(lambda (x) (class-name x)) (class-superiors subclass)))
	    (subclass-name (class-name subclass)))
	(pushnew
	 (define-a-class
	     (si:instance-class subclass)
	     subclass-name
	   superclasses
	   (slot-value subclass 'LOCAL-SLOTS)
	   (class-class-slots subclass)
	   (collect-slotds
	    (compute-class-precedence-list subclass-name
					   (mapcar #'find-class superclasses))
	    (slot-value subclass 'LOCAL-SLOTS))
	   (default-initargs-of subclass)
	   (documentation-of subclass))
	 (class-inferiors new-class))
	(eval
	 (cons 'PROGN
	       (generate-methods
		subclass-name
		:metaclass-name (class-name (class-of subclass))
		:superiors superclasses
		:slots (class-slots subclass)
		:class-slots (slot-value subclass 'LOCAL-SLOTS)
		:slots-of-the-class
		(slot-value subclass 'CLASS-CLASS-SLOTS))))))

    ;; invalidate the class
    (setf (class-name class) 'INVALID)
    (setf (slot-value class 'FORWARD) new-class))

  new-class)

;;;----------------------------------------------------------------------
