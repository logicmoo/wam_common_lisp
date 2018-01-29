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
;;; BOOT

(defun boot ()
  (let ((class (find-class 'class))
	(built-in (find-class 'built-in)))

    ;; class CLASS	--------
    (setf (class-slots class)
	  (parse-class-slots '((name :initarg :name :initform nil)
			       (superiors :initarg :direct-superclasses)
			       (inferiors :initform nil)
			       (slots :initarg :slots))))

#|    ;; the compiler needs it! Chicca
    (defmethod slot-value ((self class) slot)
      (let ((position (position slot (class-slots (si:instance-class self))
				:key #'slotd-name)))
	(if position
	    (si:instance-ref self position)
	    (slot-missing (si:instance-class self) self slot 'slot-value))))
|#

    (defmethod OPTIMIZE-SLOT-VALUE ((class class) form) form)

    (defmethod OPTIMIZE-SET-SLOT-VALUE ((class class) form) form)

    (defmethod make-instance ((class class) &rest initargs)
      (let ((instance (allocate-instance class)))
	(apply #'initialize-instance instance initargs)
	instance))

    (defmethod initialize-instance ((class class) 
				    &rest initargs 
				    &key direct-superclasses
				    &allow-other-keys)

	(call-next-method)			; from class T
	
	;; default inheritance
	(unless direct-superclasses 
	  (setf (class-superiors class)
		(class-default-direct-superclasses class direct-superclasses)))
	  
	;; if the class has a name register it in hash table
	(when (si:sl-boundp (class-name class))
	  (setf (find-class (class-name class)) class))
 
	(dolist (s (class-superiors class)) ; inheritance lattice
	  (push class (class-inferiors s)))
	class)

    (defmethod class-default-direct-superclasses ((class class)
						  supplied-superclasses)
      (or supplied-superclasses
	  (list (find-class 't))))

    ;; class BUILT-IN	--------
    (setf (class-slots built-in)
	  (parse-class-slots '((name :initarg :name :initform nil)
			       (superiors :initarg :direct-superclasses)
			       (inferiors :initform nil)
			       (slots :initarg :slots))))

    (defmethod slot-value ((self built-in) slot)
      (let ((position (position slot (class-slots (si:instance-class self))
				:key #'slotd-name)))
	(if position
	    (si:instance-ref self position)
	    (slot-missing (si:instance-class self) self slot 'slot-value))))

    (defmethod make-instance ((class built-in) &rest initargs)
      (declare (ignore initargs))
      (error "The built-in class (~A) cannot be instantiated" class))

    (defmethod initialize-instance ((class built-in)
				    &rest initargs &key &allow-other-keys)

	(call-next-method)		; from class T
	
	;; if the class has a name register it in hash table
	(when (si:sl-boundp (class-name class))
	  (setf (find-class (class-name class)) class)) 

	(dolist (s (class-superiors class)) ; inheritance lattice
	  (push class (class-inferiors s)))
	class)

    (defmethod print-object ((class built-in) stream)
      (print-unreadable-object
       (class stream)
       (format stream "The ~A ~A" (class-name (si:instance-class class))
	       (class-name class)))
      class)

    ;; class T	--------
    (defmethod initialize-instance ((instance T)
				    &rest initargs &key &allow-other-keys)
      (initialize-from-initargs instance initargs)
      (initialize-from-initforms instance)
      instance)

    (defmethod slot-missing ((class t) object slot-name operation 
			     &optional new-value)
      (declare (ignore operation new-value))
      (error "~A is not a slot of ~A" slot-name object))

    (defmethod slot-unbound ((class t) object slot-name)
      (error "the slot ~A of ~A is unbound" slot-name object))
    ))

(boot)

;;; ----------------------------------------------------------------------
