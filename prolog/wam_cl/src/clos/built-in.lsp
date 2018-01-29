;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package 'clos)

;;; ----------------------------------------------------------------------
;;; Built-in classes
;;; ----------------------------------------------------------------------

;;; ----------------------------------------------------------------------
;;; Predefined Common Lisp Classes

;(defclass t (object) () (:metaclass built-in))

(eval-when (compile load eval)
   (defclass array (t) () (:metaclass built-in))
   (defclass sequence (t) () (:metaclass built-in))
      (defclass list (sequence) () (:metaclass built-in))
         (defclass cons (list) () (:metaclass built-in))
      (defclass string (array sequence) () (:metaclass built-in))
      (defclass vector (array sequence) () (:metaclass built-in))
         (defclass bit-vector (vector) () (:metaclass built-in))

   (defclass character (t) () (:metaclass built-in))

   (defclass number (t) () (:metaclass built-in))
      (defclass complex (number) () (:metaclass built-in))
      (defclass float (number) () (:metaclass built-in))
      (defclass rational (number) () (:metaclass built-in))
         (defclass integer (rational) () (:metaclass built-in))
         (defclass ratio (rational) () (:metaclass built-in))

   (defclass symbol (t) () (:metaclass built-in))
      (defclass null (symbol list) () (:metaclass built-in))
      (defclass keyword (symbol) () (:metaclass built-in))
)

;;; Now we protect classes from redefinition:
(defun setf-find-class (name new-value)
  (cond
   ((member name '(t nil null symbol keyword atom cons list sequence
		     number integer bignum rational ratio float
		     short-float single-float double-float long-float complex
		     character standard-char string-char
		     package stream pathname readtable hash-table random-state
		     structure array simple-array function compiled-function))
    (error "The class associated to the CL specifier ~S cannot be changed."
	   name))
   ((member name '(class built-in))
    (error "The kernel CLOS class ~S cannot be changed." name))
   ((classp new-value)
    (setf (gethash name si:*class-name-hash-table*) new-value))
   ((null new-value) (remhash name si:*class-name-hash-table*))
   (t (error "~A is not a class." new-value))))

;;; ----------------------------------------------------------------------
;;; Methods

(defmethod make-instance ((class-name symbol) &rest initargs)
  (apply #'make-instance (find-class class-name) initargs))

(defmethod change-class ((instance t) (new-class symbol))
  (funcall #'change-class instance (find-class new-class)))

;;; ----------------------------------------------------------------------
;;; Structures
;;; ----------------------------------------------------------------------

;;; ----------------------------------------------------------------------
;;; Structure-metaclass
;;;
;;; This metaclass is necessary to override the behavior of BUILT-IN-CLASS,
;;; by providing specific method for dealing with defining classes as instances
;;; of STANDARD-CLASS

(defclass structure-metaclass (class)
  ()
  (:metaclass class))

;;; the method is invoked with structure-class as first argument in order 
;;; to create classes that are instances of structure-class 
;;; (classes created by defstruct)

(defmethod collect-all-slots ((metaclass structure-metaclass)
			      slots name superclasses-names)
  (let* ((superclasses (mapcar #'find-class superclasses-names))
	 (cpl (compute-class-precedence-list name superclasses)))
    (collect-slotds cpl slots)))

(defmethod define-a-class ((metaclass structure-metaclass) name
			   superclasses-names local-slots class-slots all-slots
			   default-initargs documentation)
  (declare (ignore class-slots default-initargs documentation))
  (when class-slots
    (error "The structure class ~S can't have class-slots" name))
  (let* ((existing (find-class name nil))
	 (superclasses (mapcar #'find-class superclasses-names))
	 (cpl (compute-class-precedence-list name superclasses)))

    (flet ((unchanged-class ()
	     (and existing
		  (eq metaclass (si:instance-class existing))
		  (equal (or superclasses-names '(STRUCTURE-OBJECT))
			 ;; i.e. class-default-direct-superclasses
			 (mapcar #'(lambda (x) (class-name x))
				 (class-superiors existing)))
		  (equal all-slots (slot-value existing 'SLOTS))
		  (prog2 (setf (slot-value existing 'DOCUMENTATION)
			       documentation)
		      t))))

      (if (unchanged-class)
	  existing
	  (make-instance metaclass
			 :name name
			 :direct-superclasses superclasses
			 :slots all-slots
			 :class-precedence-list cpl)))))

;;; the method to make instances of structure-class

(defmethod make-instance ((class structure-metaclass) &rest initargs)
  (let ((instance (allocate-instance class)))
    (apply #'initialize-instance instance initargs)
    instance))

;;; -----------------------------------------------------------------------
;;; Structure-class

(defclass structure-class (class)
  ;; class-precedence-list must be in the same position as in standard-class
  ((class-precedence-list :initarg :class-precedence-list)
   slot-descriptions initial-offset defstruct-form constructors documentation
		     copier predicate print-function)
  (:metaclass structure-metaclass))

;;; structure-classes cannot be instantiated
(defmethod make-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (error "The structure-class (~A) cannot be instantiated" class))

;;; the method to initialize the instances of structure-class
(defmethod initialize-instance ((class structure-class)
				&rest initargs &key &allow-other-keys)
    (call-next-method)				; from class T
    
    ;; if the class has a name register it in hash table
    (when (system:sl-boundp (class-name class))
      (setf (find-class (class-name class)) class))

  (dolist (s (class-superiors class))	; inheritance lattice
	  (push class (class-inferiors s)))
  (push class (slot-value class 'class-precedence-list)) ;; add itself in cpl
  class)

;;; ----------------------------------------------------------------------
;;; Structure-object
;;; ----------------------------------------------------------------------

;;; Structure-object has no slots and inherits only from t:
;;; (defclass structure-object (t) ())

(eval-when
 (compile load eval)
 (make-instance (find-class 'standard-class)
		:name 'structure-object
		:direct-superclasses (list (find-class 't))
		:slots ()
		:class-precedence-list ()
		:slot-index-table ()
		:local-slots ()
		:default-initargs ()
		:documentation "The root of inheritance for structures"))

(defmethod print-object ((obj structure-object) stream)
  (let* ((class (si:instance-class obj))
	 (slotds (class-slots class)))
    (princ "#S(" stream)
    (prin1 (class-name class) stream)
    (do ((scan slotds (cdr scan))
	 (i 0 (1+ i))
	 (sv))
	((null scan))
	(declare (fixnum i))
	(setq sv (si:instance-ref obj i))
	(princ " " stream)
	(prin1 (slotd-name (car scan)) stream)
	(princ " " stream)
	(prin1 sv stream)
	)
    (princ ")" stream)
    obj))

;;; ----------------------------------------------------------------------

