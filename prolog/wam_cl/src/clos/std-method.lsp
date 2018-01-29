;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package 'clos)

;;;----------------------------------------------------------------------
;;; Method
;;; ----------------------------------------------------------------------

(defclass method () ())

;;; ----------------------------------------------------------------------
;;; Standard Method
;;; ----------------------------------------------------------------------


(defclass standard-method (method)
  ((generic-function :initarg :generic-function)
   (lambda-list :initarg :lambda-list)
   (specializers :initarg :specializers :accessor specializers)
   (qualifiers :initform nil :initarg :qualifiers :reader method-qualifiers)
   (method-function :initarg :method-function)
   (documentation :initform nil :initarg documentation)
   (declarations :initform nil)
   (plist :initform nil :initarg :plist :accessor method-plist)))


(defmethod function-keywords ((method standard-method))
  (let ((lambda-list (slot-value method 'lambda-list))
	arg
	key-list
	keyp
	allowed)
    (dolist (arg lambda-list (setq key-list (nreverse key-list)))
      (cond 
       ((eq arg '&key)
	(setf keyp t))
       ((and keyp 
	     (not (member arg '(&allow-other-keys &aux))))
	(push (if (listp arg)
		  (let ((key-par (first arg)))
		    (if (listp key-par) (first key-par)
		      (make-keyword key-par)))
		(make-keyword arg))
	      key-list))
       ((eq arg '&allow-other-keys)
	(setf allowed t)
	(return))
       ((eq arg '&aux) 
	(return))
       (t ())))
    (values key-list allowed)))
