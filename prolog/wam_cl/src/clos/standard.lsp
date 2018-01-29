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
;;; STANDARD-METACLASS
;;;
;;; This metaclass is necessary to override the behavior of CLASS,
;;; by providing specific methods for dealing with defining classes, etc.

(defclass standard-metaclass (class)
  ()
  (:metaclass class))


(defmethod collect-all-slots ((metaclass standard-metaclass)
			      slots name superclasses-names)
    (declare (ignore metaclass))
  (let* ((superclasses (mapcar #'find-class superclasses-names))
	 (cpl (compute-class-precedence-list name superclasses)))
    (collect-slotds cpl slots)))

(defmethod define-a-class
    ((metaclass standard-metaclass) name superclasses-names
     local-slots class-slots all-slots default-initargs documentation)
  (let* ((existing (find-class name nil))
	 (superclasses (mapcar #'find-class superclasses-names))
	 (cpl (compute-class-precedence-list name superclasses)))

    (flet ((unchanged-class ()
	     (and existing
		  (eq metaclass (si:instance-class existing))
		  (equal (or superclasses-names '(STANDARD-OBJECT))
			 ; i.e. class-default-direct-superclasses
			 (mapcar #'(lambda (x) (class-name x))
				 (class-superiors existing)))
		  (equal local-slots (slot-value existing 'LOCAL-SLOTS))
		  (equal class-slots (class-class-slots existing))
		  (equal all-slots (slot-value existing 'SLOTS))
		  (equal default-initargs (default-initargs-of existing))
		  (prog2 (setf (slot-value existing 'DOCUMENTATION)
			       documentation)
		      t))))

      (if (unchanged-class)
	  existing
	  (let ((new-class
		 (make-instance
		  metaclass
		  :name name
		  :direct-superclasses superclasses
		  :slots all-slots

		  ;; The following slots are defined in standard-class.
		  ;; initialize-instance takes care of them
		  :slot-index-table (build-slot-index-table
				     (append all-slots class-slots))
		  :local-slots local-slots
		  :class-class-slots class-slots
		  ;; :class-class-slots-values class-class-slots-values
		  :class-precedence-list cpl
		  :default-initargs
		  (collect-default-initargs cpl default-initargs)
		  :documentation documentation)))
	    (when existing
	      (redefine-class existing new-class superclasses-names
			      (class-inferiors existing))) ; Beppe
	    new-class)))))


;;; Bootstrap versions.
(defun redefine-class (class new-class superclasses-names inferiors)
  (declare (ignore superclasses-names inferiors))
  (format t "~%Redefinition of class ~A." (class-name class))
  new-class)

;;; ----------------------------------------------------------------------
;;; STANDARD-CLASS
;;; ----------------------------------------------------------------------

(defclass standard-class (class)
  ;; class-precedence-list must be in the same position as in structure-class
  ((class-precedence-list :initarg :class-precedence-list)
   (slot-index-table :initarg :slot-index-table :initform nil) 
   ;; associates names to slot index. Be careful when changing the position of this slot!
   (local-slots :initarg :local-slots)
   (class-class-slots :initarg :class-class-slots
                      :initform nil :reader class-class-slots)
   (class-class-slots-values :initarg :class-class-slots-values
                             :initform nil :accessor class-class-slots-values)
   (default-initargs :initarg :default-initargs :reader default-initargs-of)
   (documentation :initarg :documentation :accessor documentation-of)
   (forward)				; forwarding pointer to redefined class
   )
  (:metaclass standard-metaclass))

;;; ----------------------------------------------------------------------
;;; Standard-object
;;; ----------------------------------------------------------------------

;;; Standard-object has no slots and inherits only from t:
;;; (defclass standard-object (t) ())

(eval-when
 (compile load eval)
 (make-instance (find-class 'STANDARD-CLASS)
		:name 'STANDARD-OBJECT
		:direct-superclasses (list (find-class 'T))
		:slots ()
		:class-precedence-list (list (find-class 'T))
		:slot-index-table ()
		:local-slots ()
		:default-initargs ()
		:documentation "The root of inheritance for objects"))

#+PDE
(si:record-source-pathname 'STANDARD-OBJECT 'DEFCLASS)

(defmethod slot-value ((object standard-object) slot-name)
  (multiple-value-bind (val condition)
    (standard-instance-get object slot-name)
    (ecase condition
      (:VALUE val)
      (:UNBOUND	(slot-unbound (si:instance-class object) object slot-name))
      (:MISSING (slot-missing (si:instance-class object) object slot-name
			      'SLOT-VALUE))
      )))

(defmethod (setf slot-value) (val (instance standard-object) slot-name)
  (standard-instance-set val instance slot-name))

(defmethod slot-boundp ((instance standard-object) slot-name)
  (multiple-value-bind (val condition)
    (standard-instance-get instance slot-name)
    (ecase condition
      (:VALUE t)
      (:UNBOUND nil)
      (:MISSING (slot-missing (si:instance-class instance) instance slot-name
			      'SLOT-BOUNDP))
      )))
      
(defmethod slot-exists-p ((instance standard-object) slot-name)
  (let ((class (si:instance-class instance)))
    (declare (type standard-class class))
    (slot-index slot-name (slot-index-table class))))

(defmethod slot-makunbound ((instance standard-object) slot-name)
  (let* ((class (si:instance-class instance))
	 (index (slot-index slot-name (slot-index-table class))))
    (declare (type standard-class class) (fixnum index))
    (if index
	(if (>= index 0)
	    (si:sl-makunbound instance index)
	    ;; else it is a class-slot
	    (let ((entry (assoc slot-name (class-class-slots-values class) :test #'eq)))
	      (setf (cdr entry) *class-slot-unbound*)))
	(slot-missing (si:instance-class instance) instance slot-name
		      'SLOT-MAKUNBOUND))))

(defmethod shared-initialize ((instance standard-object) 
			      slot-names &rest initargs)
  ;;
  ;; initialize the instance's slots in a two step process
  ;;   1 A slot for which one of the initargs in initargs can set
  ;;      the slot, should be set by that initarg.  If more than
  ;;      one initarg in initargs can set the slot, the leftmost
  ;;      one should set it.
  ;;
  ;;   2 Any slot not set by step 1, may be set from its initform
  ;;      by step 2.  Only those slots specified by the slot-names
  ;;      argument are set.  If slot-names is:
  ;;       T
  ;;            any slot not set in step 1 is set from its
  ;;            initform
  ;;       <list of slot names>
  ;;            any slot in the list, and not set in step 1
  ;;            is set from its initform
  ;;
  ;;       ()
  ;;            no slots are set from initforms
  ;;
  (let* ((class (si:instance-class instance)))
    ;; initialize-instance slots
    (do* ((slotds (class-slots class) (cdr slotds))
	  (slotd (car slotds) (car slotds))
	  (i 0 (1+ i)))
	 ((null slotds))
	 (let ((slot-name (slotd-name slotd))
	       (slot-initargs (slotd-initargs slotd)))
	   (flet ((from-initargs ()
		 ;; Try to initialize the slot from one of the initargs.
		 ;; If we succeed return T, otherwise return nil.
		 (doplist (initarg val)
			  initargs
		   (when (member initarg slot-initargs :test #'eq)
		     (setf (si:instance-ref instance i) val)
		     (return 't))))
	       (from-initforms ()
		 ;; Try to initialize the slot from its initform.  This
		 ;; returns no meaningful value.
		 (if (and slot-names
			  (or (eq slot-names 't)
			      (member slot-name slot-names :test #'eq))
			  (not (si:sl-boundp 
				(si:instance-ref instance i))))
		     (let ((initform (slotd-initform slotd)))
		       (when (not (eq initform '*INITFORM-UNSUPPLIED*))
			 (si:instance-set instance i
			       (eval initform)))))))
	  
	  (or (from-initargs)
	      (from-initforms)))))
    (when (typep class 'STANDARD-CLASS)
      ;; initialize class-slots
      (let ((class-class-slots (class-class-slots class))
	    (class-class-slots-values 
	     (slot-value class 'CLASS-CLASS-SLOTS-VALUES)))
	(dolist (slotd class-class-slots)
	  (let* ((slot-name (slotd-name slotd))
		 (slot-initargs (slotd-initargs slotd))
		 (found)
		 (value))
	    (if (setq found (assoc slot-name class-class-slots-values
				   :test #'eq))
		(progn
		  (setq value (cdr found))
		  (setf class-class-slots-values 
			(delete found class-class-slots-values)))
		; else
		(setq value '*CLASS-SLOT-UNBOUND*))
	    (flet ((from-initargs ()
		 ;; Try to initialize the slot from one of the initargs.
		 ;; If we succeed return T, otherwise return nil.
		 (doplist (initarg val)
			  initargs
		   (when (member initarg slot-initargs :test #'eq)
		     (setf value val)
		     (return 't))))
	       (from-initforms ()
		 ;; Try to initialize the slot from its initform.  This
		 ;; returns no meaningful value.
		 (if (eql value '*CLASS-SLOT-UNBOUND*)
		     (let ((initform (slotd-initform slotd)))
		       (when (not (eq initform '*INITFORM-UNSUPPLIED*))
			 (setf value
			       (eval initform)))))))
	  
	  (or (from-initargs)
	      (from-initforms))
	  (push (cons slot-name value) class-class-slots-values))))
	(setf (slot-value class 'CLASS-CLASS-SLOTS-VALUES) 
	      class-class-slots-values))))
  instance)

(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defmethod reinitialize-instance ((instance standard-object) &rest initargs)
  (let* ((class (si:instance-class instance)))
    (flet ((search-allow-other-keys 
	    (initargs-list)
	    (do ((arg-list initargs-list (cddr arg-list)))
		((null arg-list) nil)
		(when (eq (first arg-list) ':ALLOW-OTHER-KEYS)
		  (return (second arg-list))))))
      (if (not (search-allow-other-keys initargs))
	  (check-initargs class initargs)))
    (apply #'shared-initialize instance nil initargs)
    instance))

(defmethod change-class ((instance standard-object) (new-class standard-class))
  (let* ((old-class (si:instance-class instance))
	 (copy (allocate-copy instance))
	 (old-slotds (append (class-slots old-class) 
			     (class-class-slots old-class)))
	 (old-slotds-names (mapcar #'slotd-name old-slotds))	 
	 (new-slotds (class-slots new-class)))
    (si:change-instance instance new-class (length new-slotds))

    ;; "The values of local slots specified by both the class Cto and
    ;; Cfrom are retained.  If such a local slot was unbound, it remains
    ;; unbound."
    ;; "The values of slots specified as shared in the class Cfrom and
    ;; as local in the class Cto are retained."
    (do* ((slotds new-slotds (cdr slotds))
	  (slotd (car slotds) (car slotds))
	  (i 0 (1+ i)))
	 ((null slotds))
	 (let ((slotd-name (slotd-name slotd)))
	   (if (member slotd-name old-slotds-names :test #'eq)
	       (when (slot-boundp copy slotd-name)
		 (si:instance-set instance i
				  (slot-value copy slotd-name))))))
    #| this is done by SHARED-INITIALIZE:
    ;; initialize newly added class slots
    (let ((class-class-slots (class-class-slots new-class))
	  (class-class-slots-values))
      (dolist (slotd class-class-slots)
	(let ((slot-name (slotd-name slotd))
	      (val (let ((initform (slotd-initform slotd)))
		     (if (not (eq initform '*INITFORM-UNSUPPLIED*))
			 (eval initform)
			 ;; else
			 '*CLASS-SLOT-UNBOUND*))))
	  (push (cons slot-name val) class-class-slots-values)))
      (setf (slot-value new-class 'CLASS-CLASS-SLOTS-VALUES)
	    class-class-slots-values))
    |#
    (update-instance-for-different-class copy instance))
    instance)

(defun allocate-copy (instance)
  (let* ((class (si:instance-class instance))
	 (copy (allocate-instance class))
	 (l (length (class-slots class))))
    (dotimes (i l)
      (si:instance-set copy i (si:instance-ref instance i)))
    copy))

(defmethod update-instance-for-different-class ((previous standard-object)
						(current standard-object)
						&rest initargs)
  (let ((class (si:instance-class current)))
    (flet ((search-allow-other-keys 
	    (initargs-list)
	    (do ((arg-list initargs-list (cddr arg-list)))
		((null arg-list) nil)
		(when (eq (first arg-list) ':ALLOW-OTHER-KEYS)
		  (return (second arg-list))))))
      (if (not (search-allow-other-keys initargs))
	  (check-initargs class initargs)))
  ;;
  ;; First we must compute the newly added slots.  The spec defines
  ;; newly added slots as "those local slots for which no slot of
  ;; the same name exists in the previous class."
  (let ((added-slots nil)
	(current-slotds (class-slots (si:instance-class current)))
	(previous-slot-names
	 (nconc (mapcar #'slotd-name
			(class-class-slots (si:instance-class previous)))
		(mapcar #'slotd-name
			(class-slots (si:instance-class previous))))))
    (dolist (slotd current-slotds)
      (if (and (not (member (slotd-name slotd) previous-slot-names :test #'eq))
	       (eq (slotd-allocation slotd) ':INSTANCE))
	  (push (slotd-name slotd) added-slots)))
    (apply #'shared-initialize current added-slots initargs))))

(defmethod update-instance-for-redefined-class ((instance standard-object)
						added-slots
						discarded-slots
						property-list
						&rest initargs)
  (declare (ignore discarded-slots property-list))
  ;; ***
  ;; *** Later we need to do initarg checking here.
  ;; ***
  (apply #'shared-initialize instance added-slots initargs))

(defmethod describe-object ((obj standard-object))
  (let* ((class (si:instance-class obj))
	 (slotds (class-slots class)))
    (format t "~%~A is an instance of class ~A"
	    obj (class-name class))
    (when slotds
      ;; print instance slots
      (terpri)
      (format t "it has the following instance slots")
      (do ((scan slotds (cdr scan))
	   (i 0 (1+ i))
	   (sv))
	  ((null scan))
	(declare (fixnum i))
	(setq sv (si:instance-ref obj i))
	(format t "~%~A:~24,8T~A"
		(slotd-name (car scan))
		(if (si:sl-boundp sv) sv "Unbound"))))
    (when (typep class 'STANDARD-CLASS)
      (let ((class-class-slots (class-class-slots class)))
	(when class-class-slots
	  ;; print class slots
	  (terpri)
	  (format t "it has the following class slots")
	  (dolist (scan class-class-slots)
	    (let* ((slotd-name (slotd-name scan))
		   (sv (cdr (assoc slotd-name (class-class-slots-values class)
				   :test #'eq))))
	      (format t "~%~A:~24,8T~A"
		      slotd-name
		      (if (si:sl-boundp sv) sv "Unbound"))))))))
  obj)

;;; ----------------------------------------------------------------------
;;; Now we can fix inheritance for standard-class:

(eval-when (compile load eval)
	   (setf (class-superiors (find-class 'STANDARD-CLASS))
		 (list (find-class 'CLASS)
		       (find-class 'STANDARD-OBJECT))))

;;; ----------------------------------------------------------------------
;;; default-initargs

(defmethod default-initargs ((class t) initargs)
  initargs)

(defmethod default-initargs ((class standard-class) initargs)
  (do ((scan (reverse (default-initargs-of class)) (cddr scan))
       (defaults))
      ((null scan) (nconc initargs (nreverse defaults)))
    (unless (do ((iscan initargs (cddr iscan)))
		((null iscan) nil)
	      (when (eq (first iscan) (second scan)) (return t)))
      (setq defaults (nconc defaults (list (first scan) (second scan)))))))

;;; ----------------------------------------------------------------------
;;; check-initargs

(defun check-initargs (class initargs)
  ;; scan initarg list 
  (do* ((name-loc initargs (cddr name-loc))
	 (name (first name-loc) (first name-loc)))
	((null name-loc) class)
	(cond 
	  ((null (cdr name-loc))
	   (error "No value supplied for the init-name ~S." name))
	  (:else
	    (unless
	      (or 
 		;; check if the arguments is associated with an instance slot
		(do ((scan-slot (class-slots class) (cdr scan-slot)))
		    ((null scan-slot) ())
		    (when (member name (slotd-initargs (first scan-slot)))
			  (return t)))
		;; check if the arguments is associated with a class slot
		(do ((scan-slot (class-class-slots class) (cdr scan-slot)))
		    ((null scan-slot) ())
		    (when (member name (slotd-initargs (first scan-slot)))
			  (return t)))
#| modify this
		;; check if the argument is associated with a 
		;; initialize-instance or allocate-instance method
		;; inspecting all the keywords of the applicable methods
		(let ((cpl (cons class (slot-value class
                                        'CLASS-PRECEDENCE-LIST)))
		      cpl-names)
		  ; convert cpl in the list of the names of the classes
		  (do ((scan-cpl cpl (cdr scan-cpl)))
		      ((null scan-cpl) (setq cpl-names (nreverse cpl-names)))
		      (push (class-name (first scan-cpl)) cpl-names))
		  (dolist (scan cpl-names)
			  (when (member name
					(gethash scan *method-key-hash-table*))
				(return t))))
|#
		 )
	      ;; signal error
	      (error "Unknown initialization option ~A for class ~A"
		     name class))
	   ))))

;;; ----------------------------------------------------------------------
;;; Basic access to instances

(defun standard-instance-get (instance slot-name)
  (let* ((class (si:instance-class instance))
	(index
	 (the fixnum (gethash slot-name (slot-index-table class)
			      most-positive-fixnum))))
    (declare (type standard-class class) (fixnum index))
    (if (= index most-positive-fixnum)
	(values nil :missing)
	(if (>= index 0)
	    (let ((val (si:instance-ref instance index)))
	      (if (si:sl-boundp val)
		  (values val :value)
		  ;; else
		  (values nil :unbound)))
	    ;; else it is a class-slot
	    (let ((val (cdr (assoc slot-name (class-class-slots-values class)
				   :test #'eq))))
	      (if (eql val '*CLASS-SLOT-UNBOUND*)
		  (values nil :unbound)
		  (values val :value)))))))

(defun standard-instance-set (val instance slot-name)
  (let* ((class (si:instance-class instance))
	 (index (the fixnum (gethash slot-name (slot-index-table class)))))
    (declare (type standard-class class))
    (if index
	(if (>= index 0)
	    (si:instance-set instance index val)
          ;; else it is a class-slot
	  (let ((entry (assoc slot-name (class-class-slots-values class)
			      :test #'eq)))
	    (setf (cdr entry) val)))
      (slot-missing (si:instance-class instance) instance slot-name
		    'SLOT-VALUE))))

(defun general-instance-get (instance slot-name)
  (let* ((class (si:instance-class instance))
         (index (position slot-name (class-slots class)
                          :key #'slotd-name  :test #'eq)))
        (if index
            (let ((val (si:instance-ref instance index)))
                 (if (si:sl-boundp val)
                     (values val :value)
                     (values nil :unbound)))
            (values nil :missing))))

;;; ----------------------------------------------------------------------
;;;                                                             optimizers

(defmethod OPTIMIZE-SLOT-VALUE ((class standard-class) form)
  (let ((instance (second form))
	(slot-name (third form)))
    `(standard-instance-access ,instance
			       ',(reduce-constant slot-name) . ,(cdddr form))))

(defmethod OPTIMIZE-SET-SLOT-VALUE ((class standard-class) form)
  (let ((instance (cadadr form))
	(slot-name (caddr (second form)))
	(new-value (third form)))
    `(standard-instance-access ,instance
			       ',(reduce-constant slot-name) ,new-value)))

;;; ----------------------------------------------------------------------
;;; Methods

(defmethod initialize-instance ((class standard-class)
;;;				&rest initargs 
				&key name direct-superclasses 
				&allow-other-keys)
  (call-next-method)				; from class T

  (let* ((superclasses 
	  (class-default-direct-superclasses class direct-superclasses))
	 (cpl (if (and (cdr superclasses)
		       (eq (class-name (car superclasses)) 'STANDARD-OBJECT))
		  ;; it is a class inheriting from a structure
		  ;; so standard-object must be the first in cpl
		  (cons (car superclasses)
			(remove (find-class 'STANDARD-OBJECT)
				(compute-class-precedence-list 
				 name
				 (cdr superclasses))))
		;; else
		(compute-class-precedence-list name superclasses))))
    (setf (slot-value class 'SUPERIORS) superclasses)
    (setf (slot-value class 'CLASS-PRECEDENCE-LIST) cpl))
  class)


(defmethod class-default-direct-superclasses ((class standard-class)
					      supplied-superclasses)
  (let ((default-superclasses supplied-superclasses))
    (if supplied-superclasses
	(progn
	  ;; check if the class inherits from a structure. 
	  ;; A structure can be the first one in the list of superclasses.
	  (dolist (super (cdr supplied-superclasses))
	    (when (typep super 'STRUCTURE)
	      (error
	       "The standard class ~A can have the structure class ~A only  as first superclass in the list" class super)))
	  
	  ;; default inheritance for CLOS classes that are instances
	  ;; of standard-class is standard-object.
	  (if (eq (class-name (class-of (first supplied-superclasses)))
		  'STRUCTURE-CLASS)
	      (push (find-class 'STANDARD-OBJECT) default-superclasses)
	  
	  ;; else
	  (unless (or (eq (class-name class) 'STANDARD-OBJECT)
		      (eq (class-name class) 'STRUCTURE-OBJECT)
		      (some #'(lambda (x) (subtypep (class-name x)
						    'STANDARD-OBJECT))
			    supplied-superclasses))
	    (setf default-superclasses
		  (nreverse (cons (find-class 'STANDARD-OBJECT)
				  (nreverse supplied-superclasses)))))))
    ;; else
      (setf default-superclasses
	    (list (find-class 'STANDARD-OBJECT))))
    default-superclasses))

(defmethod make-instance ((class standard-class) &rest initargs)
  (setq initargs (default-initargs class initargs))
  (flet ((search-allow-other-keys (initargs-list)
	    (do ((arg-list initargs-list (cddr arg-list)))
		((null arg-list))
		(when (eq (first arg-list) ':ALLOW-OTHER-KEYS)
		      (return (second arg-list))))))
	(if (not (search-allow-other-keys initargs))
	    (check-initargs class initargs)))
  (let ((instance (allocate-instance class)))
    (apply #'initialize-instance instance initargs)
    instance))

(defmethod describe-object ((obj standard-class))
  (let ((slotds (class-slots (si:instance-class obj))))
    (format t "~%~A is an instance of class ~A"
	    obj (class-name (si:instance-class obj)))
    (do ((scan slotds (cdr scan))
	 (i 0 (1+ i)))
	((null scan))
      (declare (fixnum i))
      (print (slotd-name (car scan))) (princ ":	")
      (case (slotd-name (car scan))
	    ((superiors inferiors class-precedence-list)
	     (princ "(")
	     (do* ((scan (si:instance-ref obj i) (cdr scan))
		   (e (car scan) (car scan)))
		  ((null scan))
		  (prin1 (class-name e))
		  (when (cdr scan) (princ " ")))
	     (princ ")"))
	    (otherwise (prin1 (si:instance-ref obj i))))))
  obj)


;;; ----------------------------------------------------------------------
;;;                                                          documentation

#|
(defmethod documentation ((obj standard-class) &optional doc-type)
  (declare (ignore doc-type))
  (documentation-of obj))

(defmethod (setf documentation)
  ((s string) (obj standard-class) &optional doc-type)
  (declare (ignore doc-type))
  (setf (documentation-of obj) s))
|#
;;; ----------------------------------------------------------------------
;;; Generic Function
;;; ----------------------------------------------------------------------

(defclass generic-function () ())


;;; ----------------------------------------------------------------------
;;; Standard Generic Function
;;; ----------------------------------------------------------------------

(defclass standard-generic-function (generic-function)
  ((lambda-list :initarg :lambda-list :accessor lambda-list)
   (argument-precedence-order 
    :initarg :argument-precedence-order
    :accessor generic-function-argument-precedence-order)
   (method-combination 
    :initarg :method-combination 
    :accessor generic-function-method-combination
    )
   (method-combination-arguments
    :initarg :method-combination-arguments
    :accessor generic-function-method-combination-arguments
    )
   (method-class :initarg :method-class)
   (documentation :initarg :documentation 
;                 :accessor documentation
		  )
   (gfun :initarg :gfun :accessor gfun :initform nil)
   (methods :initform nil :accessor methods))) ; 7th slot as in kernel.lsp

;;;----------------------------------------------------------------------
