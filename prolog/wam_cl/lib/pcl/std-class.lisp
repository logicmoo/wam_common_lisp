;;;-*-Mode:LISP; Package:PCL; Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;;

(in-package 'pcl)

(defmethod slot-accessor-function ((slotd effective-slot-definition) type)
  (ecase type
    (reader (slot-definition-reader-function slotd))
    (writer (slot-definition-writer-function slotd))
    (boundp (slot-definition-boundp-function slotd))))

(defvar *dfuns-needing-update* NIL)
(defvar *classes-being-updated* NIL)

(defmethod update-slot-accessor-function ((slotd effective-slot-definition)
                                          type function
                                          &optional (update-accessors-p T))
  (ecase type
    (reader (setf (slot-definition-reader-function slotd) function)
            (when (and update-accessors-p (eq *boot-state* 'complete))
              (dolist (reader (slot-definition-readers slotd))
                (pushnew reader *dfuns-needing-update* :test #'eq))))
    (writer (setf (slot-definition-writer-function slotd) function)
            (when (and update-accessors-p (eq *boot-state* 'complete))
              (dolist (writer (slot-definition-writers slotd))
                (pushnew writer *dfuns-needing-update*))))
    (boundp (setf (slot-definition-boundp-function slotd) function))))

(defconstant *slotd-reader-function-std-p* 1)
(defconstant *slotd-writer-function-std-p* 2)
(defconstant *slotd-boundp-function-std-p* 4)
(defconstant *slotd-all-function-std-p*    7)

(defmethod slot-accessor-std-p ((slotd effective-slot-definition) type)
  (let ((flags (slot-value slotd 'accessor-flags)))
    (declare (type index flags))
    (if (eq type 'all)
	(= *slotd-all-function-std-p* flags)
	(let ((mask (ecase type
		      (reader *slotd-reader-function-std-p*)
		      (writer *slotd-writer-function-std-p*)
		      (boundp *slotd-boundp-function-std-p*))))
	  (declare (type index mask))
	  (not (zerop (the index (logand mask flags))))))))

(defmethod (setf slot-accessor-std-p) (value (slotd effective-slot-definition) type)
  (let ((mask (ecase type
		(reader *slotd-reader-function-std-p*)
		(writer *slotd-writer-function-std-p*)
		(boundp *slotd-boundp-function-std-p*)))
	(flags (slot-value slotd 'accessor-flags)))
    (declare (type index mask flags))
    (setf (slot-value slotd 'accessor-flags)
	  (if value
	      (the index (logior mask flags))
	      (the index (logand (the index (lognot mask)) flags)))))
  value)

(defvar *name->class->slotd-table* (make-hash-table))

(defmethod initialize-internal-slot-functions ((slotd effective-slot-definition))
  (let* ((name (slot-value slotd 'name))
	 (class (slot-value slotd 'class))
	 (old-slotd (and (slot-boundp class 'slots)
                         (find-slot-definition class name))))
    (let ((table (or (gethash name *name->class->slotd-table*)
		     (setf (gethash name *name->class->slotd-table*)
			   (make-hash-table :test 'eq :size 5)))))
      (setf (gethash class table) slotd))
    (dolist (type '(reader writer boundp))
      (multiple-value-bind (function std-p)
	  (if (eq *boot-state* 'complete)
	      (let* ((gf-name (ecase type
				(reader 'slot-value-using-class)
				(writer '(setf slot-value-using-class))
				(boundp 'slot-boundp-using-class)))
		     (gf (gdefinition gf-name)))
		(get-accessor-method-function gf type class slotd))
	      (get-optimized-std-accessor-method-function class slotd type))
	(setf (slot-accessor-std-p slotd type) std-p)
        (let ((old-function
                (if old-slotd (slot-accessor-function old-slotd type))))
	  (update-slot-accessor-function slotd type function
            (and old-function (neq function old-function))))))))

(defmethod initialize-internal-slot-functions :after
          ((slotd standard-effective-slot-definition))
  (let ((name (slot-definition-name slotd)))
    (unless *safe-to-use-slot-value-wrapper-optimizations-p*
      (initialize-internal-slot-reader-gfs name))
    (unless *safe-to-use-set-slot-value-wrapper-optimizations-p*
      (initialize-internal-slot-writer-gfs name))
    (unless *safe-to-use-slot-boundp-wrapper-optimizations-p*
      (initialize-internal-slot-boundp-gfs name))))

(defmethod (setf slot-definition-reader-function) :after 
    (new-value (slotd effective-slot-definition))
  (setf (internal-slotd-reader-function (slot-value slotd 'internal-slotd))
        new-value))

(defmethod (setf slot-definition-writer-function) :after 
    (new-value (slotd effective-slot-definition))
  (setf (internal-slotd-writer-function (slot-value slotd 'internal-slotd))
        new-value))

(defmethod (setf slot-definition-boundp-function) :after 
    (new-value (slotd effective-slot-definition))
  (setf (internal-slotd-boundp-function (slot-value slotd 'internal-slotd))
        new-value))

(defmethod (setf slot-definition-location) :after 
    (location (slotd standard-effective-slot-definition))
  (setf (internal-slotd-location (slot-value slotd 'internal-slotd))
        location)
  (initialize-internal-slot-functions slotd))



(defmethod documentation (object &optional doc-type)
  (lisp:documentation object doc-type))

(defmethod (setf documentation) (new-value object &optional doc-type)
  (declare (ignore new-value doc-type))
  (error "Can't change the documentation of ~S." object))


(defmethod documentation ((object documentation-mixin)
                          &optional (doc-type NIL doc-type-p))
  (if doc-type-p
      (error
        "Doc-type parameter (~S) supplied to documentation called
         on PCL object ~S"
        doc-type object)
     (slot-value object 'documentation)))

(defmethod (setf documentation) (new-value (object documentation-mixin)
                                 &optional (doc-type NIL doc-type-p))
  (if doc-type-p
      (error
        "Doc-type parameter (~S) supplied to (setf documentation) called
         on PCL object ~S"
        doc-type object)
      (setf (slot-value object 'documentation) new-value)))

(defmethod shared-initialize :before ((object documentation-mixin)
                                      slot-names
                                      &key documentation)
  (declare (ignore slot-names))
  (unless (legal-documentation-p object documentation)
    (error "When initializing the ~A ~S:~%~
            The ~S initialization argument was: ~A.~%~
            It must be a string or nil."
       (string-downcase (symbol-name (class-name (class-of object))))
       object :documentation documentation)))

(defmethod legal-documentation-p ((object documentation-mixin) x)
  (or (null x) (stringp x)))




;;;
;;; Various class accessors that are a little more complicated than can be
;;; done with automatically generated reader methods.
;;;


(defmethod class-prototype ((class std-class))
  (with-slots (prototype) class
    (or prototype
        (if (memq class *classes-being-updated*)
            (allocate-instance class)
            (setq prototype (make-class-prototype class))))))

(defmethod make-class-prototype ((class std-class))
  (let ((proto (allocate-instance class))
        (normal-slots nil))
    (dolist (slotd (class-slots class))
      (when (or (typep (slot-definition-location slotd) 'fixnum)
                (consp (slot-definition-location slotd)))
        (push slotd normal-slots)))
    (shared-initialize proto normal-slots :check-initargs-legality-p NIL)
    proto))

(defmethod finalized-class-prototype ((class std-class))
  "Same as class-prototype, but finalizes the class if necessary."
  (with-slots (prototype finalized-p) class
    (unless finalized-p
      (finalize-inheritance class))
    (or prototype
        (if (memq class *classes-being-updated*)
            (allocate-instance class)
            (setq prototype (make-class-prototype class))))))

(defmethod class-constructors ((class slot-class))
  (plist-value class 'constructors))

(defmethod class-slot-cells ((class std-class))
  (plist-value class 'class-slot-cells))

(defmethod (setf class-name) (new-value (class std-class))
  (reinitialize-instance class :name new-value))

(defmethod slot-unbound :around ((class class) class-instance slot-name)
  (let ((documented-reader
          (case slot-name
            (default-initargs      'class-default-initargs)
            (class-precedence-list 'class-precedence-list)
            (prototype             'class-prototype)
            (slots                 'class-slots))))
    (if documented-reader
        (if (class-finalized-p class-instance)
            (error "Huh? -- ~S slot unbound in ~S when finalized."
                   slot-name class-instance)
            (error "~S called on ~S before it is finalized."
                   documented-reader class-instance))
        (call-next-method))))


;;;
;;; Class accessors that are even a little bit more complicated than those
;;; above.  These have a protocol for updating them, we must implement that
;;; protocol.
;;; 

;;;
;;; Maintaining the direct subclasses backpointers.  The update methods are
;;; here, the values are read by an automatically generated reader method.
;;; 
(defmethod add-direct-subclass ((class class) (subclass class))
  (with-slots (direct-subclasses) class
    (pushnew subclass direct-subclasses)
    subclass))

(defmethod remove-direct-subclass ((class class) (subclass class))
  (with-slots (direct-subclasses) class
    (setq direct-subclasses (remove subclass direct-subclasses))
    subclass))

;;;
;;; Maintaining the direct-methods and direct-generic-functions backpointers.
;;;
;;; There are four generic functions involved, each has one method for the
;;; class case and another method for the damned EQL specializers. All of
;;; these are specified methods and appear in their specified place in the
;;; class graph.
;;;
;;;   ADD-DIRECT-METHOD
;;;   REMOVE-DIRECT-METHOD
;;;   SPECIALIZER-DIRECT-METHODS
;;;   SPECIALIZER-DIRECT-GENERIC-FUNCTIONS
;;;
;;; In each case, we maintain one value which is a cons.  The car is the list
;;; methods.  The cdr is a list of the generic functions.  The cdr is always
;;; computed lazily.
;;;

(defmethod add-direct-method ((specializer class) (method method))
  (with-slots (direct-methods) specializer
    (setf (car direct-methods) (adjoin method (car direct-methods))	;PUSH
	  (cdr direct-methods) ()))
  method)

(defmethod remove-direct-method ((specializer class) (method method))
  (with-slots (direct-methods) specializer
    (setf (car direct-methods) (remove method (car direct-methods))
	  (cdr direct-methods) ()))
  method)

(defmethod specializer-direct-methods ((specializer class))
  (with-slots (direct-methods) specializer
    (car direct-methods)))

(defmethod specializer-direct-generic-functions ((specializer class))
  (with-slots (direct-methods) specializer
    (or (cdr direct-methods)
	(setf (cdr direct-methods)
	      (gathering1 (collecting-once)
		(dolist (m (car direct-methods))
		  (gather1 (method-generic-function m))))))))



;;;
;;; This hash table is used to store the direct methods and direct generic
;;; functions of EQL specializers.  Each value in the table is the cons.
;;; 
(defvar *eql-specializer-methods* (make-hash-table :test #'eql))
(defvar *class-eq-specializer-methods* (make-hash-table :test #'eq))

(defmethod specializer-method-table ((specializer eql-specializer))
  *eql-specializer-methods*)

(defmethod specializer-method-table ((specializer class-eq-specializer))
  *class-eq-specializer-methods*)

(defmethod add-direct-method ((specializer specializer-with-object) (method method))
  (let* ((object (specializer-object specializer))
	 (table (specializer-method-table specializer))
	 (entry (gethash object table)))
    (unless entry
      (setq entry
	    (setf (gethash object table)
		  (cons nil nil))))
    (setf (car entry) (adjoin method (car entry))
	  (cdr entry) ())
    method))

(defmethod remove-direct-method ((specializer specializer-with-object) (method method))
  (let* ((object (specializer-object specializer))
	 (entry (gethash object (specializer-method-table specializer))))
    (when entry
      (setf (car entry) (remove method (car entry))
	    (cdr entry) ()))
    method))

(defmethod specializer-direct-methods ((specializer specializer-with-object))  
  (car (gethash (specializer-object specializer)
		(specializer-method-table specializer))))

(defmethod specializer-direct-generic-functions ((specializer specializer-with-object))
  (let* ((object (specializer-object specializer))
	 (entry (gethash object (specializer-method-table specializer))))
    (when entry
      (or (cdr entry)
	  (setf (cdr entry)
		(gathering1 (collecting-once)
		  (dolist (m (car entry))
		    (gather1 (method-generic-function m)))))))))

(defun map-all-classes (function &optional (root-name 't))
  (declare (type real-function function))
  (labels ((do-class (class)
	     (mapc #'do-class (class-direct-subclasses class))
	     (funcall function class)))
    (do-class (find-class root-name))))

(defun map-specializers (function)
  (declare (type real-function function))
  (map-all-classes #'(lambda (class)
		       (funcall function (class-eq-specializer class))
		       (funcall function class)))
  (maphash #'(lambda (object methods)
	       (declare (ignore methods))
	       (intern-eql-specializer object))
	   *eql-specializer-methods*)
  (maphash #'(lambda (object specl)
	       (declare (ignore object))
	       (funcall function specl))
	   *eql-specializer-table*)
  nil)

(defun map-all-generic-functions (function)
  (declare (type real-function function))
  (let ((all-generic-functions (make-hash-table :test 'eq)))
    (map-specializers #'(lambda (specl)
			  (dolist (gf (specializer-direct-generic-functions specl))
			    (unless (gethash gf all-generic-functions)
			      (setf (gethash gf all-generic-functions) t)
			      (funcall function gf))))))
  nil)

(defmethod shared-initialize :after ((specl class-eq-specializer) slot-names &key)
  (declare (ignore slot-names))
  (setf (slot-value specl 'type) `(class-eq ,(specializer-class specl))))

(defmethod shared-initialize :after ((specl eql-specializer) slot-names &key)
  (declare (ignore slot-names))
  (setf (slot-value specl 'type) `(eql ,(specializer-object specl))))



(defun real-load-defclass (name metaclass-name supers slots other accessors)
  (do-standard-defsetfs-for-defclass accessors)	                ;***
  (apply #'ensure-class name :metaclass metaclass-name
			     :direct-superclasses supers
			     :direct-slots slots
			     :definition-source `((defclass ,name)
						  ,(load-truename))
			     other))

(declaim (ftype (function (T T) (values T list)) ensure-class-values))
(defun ensure-class-values (class args)
  (let* ((initargs (copy-list args))
	 (unsupplied (list 1))
	 (supplied-meta   (getf initargs :metaclass unsupplied))
	 (supplied-supers (getf initargs :direct-superclasses unsupplied))
	 (supplied-slots  (getf initargs :direct-slots unsupplied))
	 (meta
	   (cond ((neq supplied-meta unsupplied)
		  (find-class supplied-meta))
		 ((or (null class)
		      (forward-referenced-class-p class))
		  *the-class-standard-class*)
		 (t
		  (class-of class)))))  
    (flet ((fix-super (s)
	     (cond ((classp s) s)
		   ((not (legal-class-name-p s))
		    (error "~S is not a class or a legal class name." s))
		   (t
		    (or (find-class s nil)
			(setf (find-class s)
			      (make-instance 'forward-referenced-class
					     :name s)))))))      
      (loop (unless (remf initargs :metaclass) (return)))
      (loop (unless (remf initargs :direct-superclasses) (return)))
      (loop (unless (remf initargs :direct-slots) (return)))
      (values meta
	      (list* :direct-superclasses
		     (and (neq supplied-supers unsupplied)
			  (mapcar #'fix-super supplied-supers))
		     :direct-slots
		     (and (neq supplied-slots unsupplied) supplied-slots)
		     initargs)))))

(defun ensure-class (name &rest all)
  (apply #'ensure-class-using-class name (find-class name nil) all))

(defmethod ensure-class-using-class (name (class null) &rest args &key)
  (multiple-value-bind (meta initargs)
      (ensure-class-values class args)
    (setf class (apply #'make-instance meta :name name initargs))
    class))

(defmethod ensure-class-using-class (name (class pcl-class) &rest args &key)
  (declare (ignore name))
  (multiple-value-bind (meta initargs)
      (ensure-class-values class args)
    (unless (eq (class-of class) meta) (change-class class meta))
    (apply #'reinitialize-instance class initargs)
    class))


;;;
;;;
;;;

(defmethod shared-initialize :before ((class std-class)
				      slot-names
				      &key (name nil name-p))
  (declare (ignore slot-names))
  (when name-p
    (unless (legal-class-name-p name)
      (error "~S is not a legal class name." name))
    (when (slot-boundp class 'name)
      (setf (find-class (slot-value class 'name)) nil))))

(defmethod make-direct-slotd ((class std-class) &rest initargs)
  (let ((initargs (list* :class class initargs)))
    (apply #'make-instance (direct-slot-definition-class class initargs) initargs)))

(defmethod shared-initialize :after
	   ((class std-class)
	    slot-names
	    &key (direct-superclasses nil direct-superclasses-p)
		 (direct-slots nil direct-slots-p)
		 (direct-default-initargs nil direct-default-initargs-p)
	         (predicate-name nil predicate-name-p))
  (declare (ignore slot-names))
  (if direct-superclasses-p
      (progn
        (setq direct-superclasses (or direct-superclasses
                                      (list *the-class-standard-object*)))
        (dolist (superclass direct-superclasses)
	  (unless (validate-superclass class superclass)
	    (error "The class ~S was specified as a~%super-class of the class ~S;~%~
                    but the meta-classes ~S and~%~S are incompatible."
		   superclass class (class-of superclass) (class-of class))))
        (setf (slot-value class 'direct-superclasses) direct-superclasses))
      (setq direct-superclasses (slot-value class 'direct-superclasses)))
  (setq direct-slots
	(if direct-slots-p
	    (setf (slot-value class 'direct-slots)
		  (mapcar #'(lambda (pl) (apply #'make-direct-slotd class pl))
                          direct-slots))
	    (slot-value class 'direct-slots)))
  (if direct-default-initargs-p
      (setf (slot-value class 'direct-default-initargs) direct-default-initargs)
      (setq direct-default-initargs (class-direct-default-initargs class)))
  (setf (plist-value class 'class-slot-cells)
	(gathering1 (collecting)
	  (dolist (dslotd direct-slots)
	    (when (eq (slot-definition-allocation dslotd) class)
	      (let ((initfunction (slot-definition-initfunction dslotd)))
		(gather1 (cons (slot-definition-name dslotd)
			       (if initfunction 
				   (slot-initfunction-funcall initfunction)
				   *slot-unbound*))))))))
  (let ((name (class-name class)))
    (when name
      (if predicate-name-p
          (progn
            (setf (slot-value class 'predicate-name) (car predicate-name))
            (setf (find-class-predicate name) (make-class-predicate class)))
          (unless (slot-value class 'predicate-name)
	    (setf (slot-value class 'predicate-name)
	          (make-class-predicate-name name))))
      (setf (find-class name) class)
      (inform-type-system-about-class class name)))
  (add-direct-subclasses class direct-superclasses)
  (add-slot-accessors    class direct-slots))

(defmethod shared-initialize :before ((class class) slot-names &key)
  (declare (ignore slot-names))
  (setf (slot-value class 'type) `(class ,class))
  (setf (slot-value class 'class-eq-specializer)
	(make-instance 'class-eq-specializer :class class)))

(defmethod reinitialize-instance :before ((class slot-class) &key)
  (remove-direct-subclasses class (class-direct-superclasses class))
  (remove-slot-accessors    class (class-direct-slots class)))

(defmethod reinitialize-instance :after ((class std-class)
					 &rest initargs
					 &key)
  (update-class class nil)
  (map-dependents class
		  #'(lambda (dependent)
		      (apply #'update-dependent class dependent initargs))))


 
(defun make-class-predicate (class)
  (let* ((name (class-predicate-name class))
         (gf (ensure-generic-function name))
         (method-class (find-class 'standard-method nil))
         (method-proto
           (if (and method-class (class-finalized-p method-class))
               (class-prototype method-class)))
         (store-method-function-p
          (call-store-method-function-p gf method-proto nil))
         (store-method-optimized-function-p
          (call-store-method-optimized-function-p gf method-proto nil))
	 (mlist (if (eq *boot-state* 'complete)
		    (generic-function-methods gf)
		    (early-gf-methods gf))))
    (unless mlist
      (unless (eq class *the-class-t*)
	(let ((default-method
                (make-a-method
                  'standard-method
	          ()
	          (list 'object)
	          (list *the-class-t*)
                  (when store-method-function-p
                    #'documented-function-returning-nil)
                  (when store-method-optimized-function-p
                    #'function-returning-nil)
	          NIL
	          "class predicate default method"
                  NIL
                  `(:constant-value NIL))))
	  (add-method gf default-method)))
      (let ((class-method
              (make-a-method
                'standard-method
	        ()
	        (list 'object)
		(list class)
                (when store-method-function-p
                  #'documented-function-returning-t)
                (when store-method-optimized-function-p
                  #'function-returning-t)
	        NIL
		"class predicate class method"
                NIL
                `(:constant-value T))))
	(add-method gf class-method)))
    gf))

(defun fix-dfuns-needing-update ()
  (loop (unless *dfuns-needing-update* (return T))
        (let ((name (pop *dfuns-needing-update*)))
          (when (gboundp name)
            (update-dfun (gdefinition name))))))

(defun add-slot-accessors (class dslotds)
  (fix-slot-accessors class dslotds 'add))

(defun remove-slot-accessors (class dslotds)
  (fix-slot-accessors class dslotds 'remove))

(defun fix-slot-accessors (class dslotds add/remove)  
  (flet ((fix (gfspec name dslot r/w)
	   (let ((gf (ensure-generic-function gfspec)))
	     (case r/w
	       (r (if (eq add/remove 'add)
		      (add-reader-method class gf name dslot)
		      (remove-reader-method class gf)))
	       (w (if (eq add/remove 'add)
		      (add-writer-method class gf name dslot)
		      (remove-writer-method class gf)))))))
    (dolist (dslotd dslotds)
      (let ((name (slot-definition-name dslotd)))
        (dolist (r (slot-definition-readers dslotd)) (fix r name dslotd 'r))
        (dolist (w (slot-definition-writers dslotd)) (fix w name dslotd 'w)))))
  (fix-dfuns-needing-update))


(defun add-direct-subclasses (class new)
  (dolist (n new)
    (unless (memq class (class-direct-subclasses class))
      (add-direct-subclass n class))))

(defun remove-direct-subclasses (class new)
  (let ((old (class-direct-superclasses class)))
    (dolist (o (set-difference old new))
      (remove-direct-subclass o class))))


;;;
;;;
;;;

(defvar *notify-finalize* NIL)

(defmethod finalize-inheritance ((class std-class))
  (when *notify-finalize*
    (warn "Finalizing ~S" class))
  (update-class class t))

(defmacro assure-finalized (class)
  (once-only (class)
    `(unless (class-finalized-p ,class) (finalize-inheritance ,class))))



      
;;;
;;; Called by :after shared-initialize whenever a class is initialized or 
;;; reinitialized.  The class may or may not be finalized.
;;; 
(defun update-class (class finalizep)  
  (when (or finalizep (class-finalized-p class))
    (push class *classes-being-updated*)
    (update-cpl class (compute-class-precedence-list class))
    (update-slots class)
    (update-gfs-of-class class)
    (update-inits class (compute-default-initargs class))
    (update-constructors class)
    (setf *classes-being-updated* (delete class *classes-being-updated*)))
  (unless finalizep
    (dolist (sub (class-direct-subclasses class)) (update-class sub nil))))

(defun update-cpl (class cpl)
  (when (class-finalized-p class)
    (unless (equal (class-precedence-list class) cpl)
      (force-cache-flushes class)))
  (fast-set-slot-value class 'class-precedence-list cpl slow-slot-value)
  (let ((wrapper (class-wrapper class)))
    (when wrapper
      (setf (wrapper-class-precedence-list wrapper) cpl)))
  (update-class-can-precede-p cpl))

(defun update-class-can-precede-p (cpl)
  (when cpl
    (let* ((first (car cpl))
           (orig-precede-list
             (fast-slot-value first 'can-precede-list slow-slot-value))
           (precede-list orig-precede-list))
      (dolist (c (cdr cpl))
        (unless (memq c precede-list)
          (setf precede-list (cons c precede-list))))
      (unless (eq precede-list orig-precede-list)
        (fast-set-slot-value first 'can-precede-list precede-list
                             slow-slot-value)))
    (update-class-can-precede-p (cdr cpl))))

(defun class-can-precede-p (class1 class2)
  (memq class2 (class-can-precede-list class1)))


(declaim (ftype (function (T T) (values list list)) compute-storage-info))
(defmethod compute-storage-info ((class std-class) eslotds)
  (let ((instance-slots ())
	(class-slots    ()))
    (dolist (eslotd eslotds)
      (let ((alloc (slot-definition-allocation eslotd)))
	(cond ((eq alloc :instance) (push eslotd instance-slots))
	      ((classp alloc)       (push eslotd class-slots)))))
    (values (compute-instance-layout class instance-slots)
	    (compute-class-slots class class-slots))))

(defmethod compute-instance-layout ((class std-class) instance-eslotds)
  (mapcar #'slot-definition-name
          (sort instance-eslotds #'< :key #'slot-definition-location)))

(defmethod compute-class-slots ((class std-class) eslotds)
  (gathering1 (collecting)
    (dolist (eslotd eslotds)
      (gather1
	(assq (slot-definition-name eslotd)
	      (class-slot-cells (slot-definition-allocation eslotd)))))))

(defmethod compute-layout ((class std-class) cpl instance-eslotds)
  (let* ((names
	   (gathering1 (collecting)
	     (dolist (eslotd instance-eslotds)
	       (gather1 (slot-definition-name eslotd)))))
	 (order ()))
    (labels ((rwalk (tail)
	       (when tail
		 (rwalk (cdr tail))
		 (dolist (ss (class-direct-slots (car tail)))
		   (let ((n (slot-definition-name ss)))
		     (when (memq n names)
		       (setq order (cons n order)
			     names (remove n names))))))))
      (rwalk cpl)
      (nreverse order))))

(defmethod all-standard-slots-p ((class std-class) eslotds)
  (dolist (slotd eslotds T)
    (unless (memq (slot-definition-allocation slotd) '(:instance :class))
      (return nil))))

(defvar *old-update-slots-wrapper* NIL)

(defun update-slots (class)
  (let ((owrapper (class-wrapper class))
        (nwrapper (make-wrapper  class)))
    (setf (fast-slot-value class 'wrapper) nwrapper)
    (unless owrapper
      ;; The class isn't really totally finalized, but the
      ;; class finalization functions have to think it is.
      (fast-set-slot-value class 'finalized-p T slow-slot-value)
      (setf (fast-slot-value class 'prototype) nil))
    (let ((eslotds (compute-slots class)))
      (multiple-value-bind (nlayout nwrapper-class-slots)
          (compute-storage-info class eslotds)
        (declare (type list nlayout nwrapper-class-slots))
        ;;
        ;; If there is a change in the shape of the instances then the
        ;; old class is now obsolete.
        ;;
        (let* ((olayout (and owrapper (wrapper-instance-slots-layout owrapper)))
  	       (owrapper-class-slots
                  (and owrapper (wrapper-class-slots owrapper)))
               (make-instances-obsolete-p
                 (not (or (null owrapper)
  		          (and (all-standard-slots-p class eslotds)
                               (equal nlayout olayout)
  			       (not
  			        (iterate
                                  ((o (list-elements owrapper-class-slots))
  				   (n (list-elements nwrapper-class-slots)))
  				  (unless (eq (car o) (car n))
                                     (return t)))))))))
          (declare (type list    olayout owrapper-class-slots)
                   (type boolean make-instances-obsolete-p))
          (when (or (null owrapper) make-instances-obsolete-p)
            (let ((internal-slotds ())
                  (side-effect-internal-slotds ()))
              (dolist (eslotd eslotds)
                (let ((internal-slotd (slot-definition-internal-slotd eslotd)))
                  (push internal-slotd internal-slotds)
                  (unless (and (slot-definition-initfunction-side-effect-free-p
                                 eslotd)
                               (typep (slot-definition-location eslotd)
                                      '(or cons fixnum)))
                    (push internal-slotd side-effect-internal-slotds))))
              (setf (slow-slot-value class 'slots) eslotds
                    (slow-slot-value class 'internal-slotds)
                      (nreverse internal-slotds)
                    (slow-slot-value class 'side-effect-internal-slotds)
                      (nreverse side-effect-internal-slotds))))
          (setf (wrapper-instance-slots-layout nwrapper) nlayout
                (wrapper-allocate-static-slot-storage-copy nwrapper)
                  (%allocate-origional-static-slot-storage-copy
                     (length nlayout))
  	        (wrapper-class-slots nwrapper) nwrapper-class-slots
                (wrapper-class-precedence-list nwrapper)
                  (fast-slot-value class 'class-precedence-list)
                (wrapper-unreserved-field nwrapper) NIL)
          (when make-instances-obsolete-p
            (let ((*old-update-slots-wrapper* owrapper))
              (make-instances-obsolete class)))
          (initialize-allocate-static-slot-storage-copy class))))))

(defmethod initialize-allocate-static-slot-storage-copy ((class std-class))
  (let* ((dummy-instance (allocate-instance class))
         (wrapper (class-wrapper class))
         (old-allocate-static-slot-storage-copy
          (wrapper-allocate-static-slot-storage-copy wrapper)))
    (dolist (slot (class-slots class))
      (when (and (slot-definition-initfunction-side-effect-free-p slot)
                 (typep (slot-definition-location slot)
                        '(or fixnum cons)))
        (let ((initfn (slot-definition-initfunction slot)))
          (when initfn
            (method-function-funcall
              (slot-definition-writer-function slot)
              (slot-initfunction-funcall initfn)
              dummy-instance)))))
    (setf (wrapper-allocate-static-slot-storage-copy wrapper)
          (or (get-slots-or-nil dummy-instance)
              old-allocate-static-slot-storage-copy))))

(defun update-gfs-of-class (class)
  (when (let ((cpl (class-precedence-list class)))
	  (or (memq *the-class-slot-class* cpl)
	      (memq *the-class-standard-effective-slot-definition* cpl)))
    (let ((gf-table (make-hash-table :test 'eq)))
      (labels ((collect-gfs (class)
		 (dolist (gf (specializer-direct-generic-functions class))
		   (setf (gethash gf gf-table) t))
		 (mapc #'collect-gfs (class-direct-superclasses class))))
	(collect-gfs class)
	(maphash #'(lambda (gf ignore)
		     (declare (ignore ignore))
		     (update-gf-dfun class gf))
		 gf-table)))))

(defun update-inits (class inits)
  (setf (slot-value class 'default-initargs) inits))


;;;
;;;
;;;
(defmethod compute-default-initargs ((class slot-class))
  (let ((cpl (class-precedence-list class))
	(direct (class-direct-default-initargs class)))
    (labels ((walk (tail)
	       (if (null tail)
		   nil
		   (let ((c (pop tail)))
		     (append (if (eq c class)
				 direct 
				 (class-direct-default-initargs c))
			     (walk tail))))))
      (let ((initargs (walk cpl)))
	(delete-duplicates initargs :test #'eq :key #'car :from-end t)))))


;;;
;;; Protocols for constructing direct and effective slot definitions.
;;;
;;; 
;;;
;;;
(defmethod direct-slot-definition-class ((class std-class) initargs)
  (declare (ignore initargs))
  (find-class 'standard-direct-slot-definition))

;;;
;;;
;;;
(defmethod compute-slots ((class std-class))
  ;;
  ;; As specified, we must call COMPUTE-EFFECTIVE-SLOT-DEFINITION once
  ;; for each different slot name we find in our superclasses.  Each
  ;; call receives the class and a list of the dslotds with that name.
  ;; The list is in most-specific-first order.
  ;;
  (let ((name-dslotds-alist ()))
    (dolist (c (slot-value class 'class-precedence-list))
      (let ((dslotds (class-direct-slots c)))
	(dolist (d dslotds)
	  (let* ((name (slot-definition-name d))
		 (entry (assq name name-dslotds-alist)))
	    (if entry
		(push d (cdr entry))
		(push (list name d) name-dslotds-alist))))))
    (mapcar #'(lambda (direct)
		(compute-effective-slot-definition class
                                                   (car direct)
						   (nreverse (cdr direct))))
	    name-dslotds-alist)))

(defmethod compute-slots :around ((class std-class))
  (let ((eslotds (call-next-method))
	(cpl (slot-value class 'class-precedence-list))
	(instance-slots ())
	(class-slots    ())
        (other-slots    ()))
    (dolist (eslotd eslotds)
      (let ((alloc (slot-definition-allocation eslotd)))
	(cond ((eq alloc :instance) (push eslotd instance-slots))
	      ((classp alloc)       (push eslotd class-slots))
              (T                    (push eslotd other-slots)))))
    (let ((nlayout (compute-layout class cpl instance-slots)))
      (declare (type list nlayout))
      (dolist (eslotd instance-slots)
	(setf (slot-definition-location eslotd) 
	      (posq (slot-definition-name eslotd) nlayout))))
    (dolist (eslotd class-slots)
      (setf (slot-definition-location eslotd) 
	    (assq (slot-definition-name eslotd)
		  (class-slot-cells (slot-definition-allocation eslotd)))))
    (dolist (eslotd other-slots)
      (initialize-internal-slot-functions eslotd))
    eslotds))

(defmethod compute-effective-slot-definition ((class standard-class) name dslotds)
  (let* ((initargs (compute-effective-slot-definition-initargs class dslotds))
	 (class (effective-slot-definition-class class initargs))
         (slot-definition (apply #'make-instance class initargs))
         (internal-slotd
           (make-internal-slotd
             :name name
             :slot-definition slot-definition
             :location        (slot-definition-location     slot-definition)
             :initargs        (slot-definition-initargs     slot-definition)
             :initfunction    (slot-definition-initfunction slot-definition))))
    (setf (fast-slot-value slot-definition 'internal-slotd) internal-slotd)
    slot-definition))

(defmethod compute-effective-slot-definition ((class funcallable-standard-class)
                                              name dslotds)
  (let* ((initargs (compute-effective-slot-definition-initargs class dslotds))
	 (class (effective-slot-definition-class class initargs))
         (slot-definition (apply #'make-instance class initargs))
         (internal-slotd
           (make-internal-slotd
             :name name
             :slot-definition slot-definition
             :location        (slot-definition-location     slot-definition)
             :initargs        (slot-definition-initargs     slot-definition)
             :initfunction    (slot-definition-initfunction slot-definition))))
    (setf (fast-slot-value slot-definition 'internal-slotd) internal-slotd)
    slot-definition))

(defmethod effective-slot-definition-class ((class std-class) initargs)
  (declare (ignore initargs))
  *the-class-standard-effective-slot-definition*)

(defmethod compute-effective-slot-definition-initargs 
    ((class slot-class) direct-slotds)
  (let* ((name nil)
	 (initfunction nil)
	 (initform nil)
	 (initargs nil)
	 (allocation nil)
         (documentation nil)
	 (type t)
	 (namep  nil)
	 (initp  nil)
	 (allocp nil)
         (readers nil)
         (writers nil)
         (initfunction-side-effect-free-p nil))
       (declare (type boolean namep initp allocp
                              initfunction-side-effect-free-p))

    (dolist (slotd direct-slotds)
      (when slotd
	(unless namep
	  (setq name (slot-definition-name slotd)
		namep t))
	(unless initp
	  (setq initform (slot-definition-initform slotd))
	  (when (slot-definition-initfunction slotd)
	    (setq initfunction (slot-definition-initfunction slotd)
                  initfunction-side-effect-free-p
                    (slot-definition-initfunction-side-effect-free-p slotd)
		  initp t)))
	(unless allocp
	  (setq allocation (slot-definition-allocation slotd)
		allocp t))
	(setq initargs (append (slot-definition-initargs slotd) initargs))
	(let ((slotd-type (slot-definition-type slotd)))
	  (setq type (cond ((eq type 't) slotd-type)
			   ((*subtypep type slotd-type) type)
                           ((*subtypep slotd-type type) slotd-type)
			   (t `(and ,type ,slotd-type)))))
        (unless documentation
          (setq documentation
                (fast-slot-value slotd 'documentation slow-slot-value)))
        (dolist (reader (slot-definition-readers slotd))
          (pushnew reader readers :test #'eq))
        (dolist (writer (slot-definition-writers slotd))
          (pushnew writer writers :test #'eq))))
    (list :name name
	  :initform initform
	  :initfunction initfunction
          :initfunction-side-effect-free-p initfunction-side-effect-free-p
	  :initargs initargs
	  :allocation allocation
	  :type type
	  :class class
          :documentation documentation
          :readers readers
          :writers writers)))



;;;
;;; NOTE: For bootstrapping considerations, these can't use make-instance
;;;       to make the method object.  They have to use make-a-method which
;;;       is a specially bootstrapped mechanism for making standard methods.
;;;

(defmethod add-reader-method ((class slot-class)
                              generic-function 
                              slot-name
                              &optional
                              direct-slot)
  (let*
    ((reader-class
       (reader-method-class class direct-slot))
     (reader-prototype
       (when reader-class
         (assure-finalized reader-class)
         (class-prototype reader-class)))
     (method
       (make-a-method
         (if reader-class (class-name reader-class) 'standard-reader-method)
         ()
         (list (or (class-name class) 'standard-object))
         (list class)
         (when (call-store-method-function-p generic-function reader-prototype nil)
           (make-documented-reader-method-function
             class generic-function reader-prototype slot-name))
         (when (call-store-method-optimized-function-p
                 generic-function reader-prototype nil)
           (if (eq *boot-state* 'complete)
               (make-optimized-reader-method-function
                 class generic-function reader-prototype slot-name)
               (make-std-reader-method-function slot-name)))
         NIL
         "automatically generated reader method"
         slot-name
         `(,@(when direct-slot (list :slot-definition direct-slot))
           :needs-next-methods-p NIL))))
    (add-method generic-function method)))

(defmethod add-writer-method ((class slot-class)
                              generic-function 
                              slot-name
                              &optional
                              direct-slot)
  (let*
    ((writer-class
       (writer-method-class class direct-slot))
     (writer-prototype
       (when writer-class
         (assure-finalized writer-class)
         (class-prototype writer-class)))
     (method
       (make-a-method
         (if writer-class (class-name writer-class) 'standard-writer-method)
         ()
         (list 'new-value (or (class-name class) 'standard-object))
         (list *the-class-t* class)
         (when (call-store-method-function-p generic-function writer-prototype nil)
           (make-documented-writer-method-function
             class generic-function writer-prototype slot-name))
         (when (call-store-method-optimized-function-p
                 generic-function writer-prototype nil)
           (if (eq *boot-state* 'complete)
               (make-optimized-writer-method-function
                 class generic-function writer-prototype slot-name)
               (make-std-writer-method-function slot-name)))
         NIL
         "automatically generated writer method"
         slot-name
         `(,@(when direct-slot (list :slot-definition direct-slot))
           :needs-next-methods-p NIL))))
   (add-method generic-function method)))

(defmethod add-boundp-method ((class slot-class)
                              generic-function
                              slot-name
                              &optional
                              direct-slot)
  (let*
    ((boundp-class
       (boundp-method-class class direct-slot))
     (boundp-prototype
       (when boundp-class
         (assure-finalized boundp-class)
         (class-prototype boundp-class)))
     (method
       (make-a-method
         (if boundp-class (class-name boundp-class) 'standard-boundp-method)
         ()
         (list (or (class-name class) 'standard-object))
         (list class)
         (when (call-store-method-function-p generic-function boundp-prototype nil)
           (make-documented-boundp-method-function
             class generic-function boundp-prototype slot-name))
         (when (call-store-method-optimized-function-p
                 generic-function boundp-prototype nil)
           (if (eq *boot-state* 'complete)
               (make-optimized-boundp-method-function
                 class generic-function boundp-prototype slot-name)
               (make-std-boundp-method-function slot-name)))
         NIL
         "automatically generated boundp method"
         slot-name
         `(,@(when direct-slot (list :slot-definition direct-slot))
           :needs-next-methods-p NIL))))
    (add-method generic-function method)))


(defmethod reader-method-class ((class T)
                                direct-slot
                                &rest initargs)
  ;; To handle the case when a reader method is added before
  ;; standard-reader-method is defined.
  (declare (ignore direct-slot initargs))
  NIL)

(defmethod writer-method-class ((class T)
                                direct-slot
                                &rest initargs)
  ;; To handle the case when a writer method is added before
  ;; standard-writer-method is defined.
  (declare (ignore direct-slot initargs))
  NIL)

(defmethod boundp-method-class ((class T)
                                direct-slot
                                &rest initargs)
  ;; To handle the case when a boundp method is added before
  ;; standard-boundp-method is defined.
  (declare (ignore direct-slot initargs))
  NIL)


(defmethod remove-reader-method ((class slot-class) generic-function)
  (let ((method (get-method generic-function () (list class) nil)))
    (when method (remove-method generic-function method))))

(defmethod remove-writer-method ((class slot-class) generic-function)
  (let ((method
	  (get-method generic-function () (list *the-class-t* class) nil)))
    (when method (remove-method generic-function method))))

(defmethod remove-boundp-method ((class slot-class) generic-function)
  (let ((method (get-method generic-function () (list class) nil)))
    (when method (remove-method generic-function method))))


;;;
;;; make-reader-method-function and make-write-method function are NOT part of
;;; the standard protocol.  They are however useful, PCL makes uses makes use
;;; of them internally and documents them for PCL users.
;;;
;;; *** This needs work to make type testing by the writer functions which
;;; *** do type testing faster.  The idea would be to have one constructor
;;; *** for each possible type test.  In order to do this it would be nice
;;; *** to have help from inform-type-system-about-class and friends.
;;;
;;; *** There is a subtle bug here which is going to have to be fixed.
;;; *** Namely, the simplistic use of the template has to be fixed.  We
;;; *** have to give the optimize-slot-value method the user might have
;;; *** defined for this metclass a chance to run.
;;;

(defvar *documented-reader-method-function-makers* NIL)
(defvar *documented-writer-method-function-makers* NIL)
(defvar *documented-boundp-method-function-makers* NIL)

(defmethod make-documented-reader-method-function ((class slot-class)
                                                   generic-function
                                                   reader-method-prototype
                                                   slot-name)
  ;;   Make the documented reader-method-function.  To do this correctly for
  ;; all cases, we must build the reader method function by passing it
  ;; through make-method-lambda in case the user does something funky
  ;; with their method lambdas.
  ;;   Since there usually won't be many different kinds of method
  ;; lambdas for readers, this method actually dynamically builds a function
  ;; to make the documented-reader-method-function from each particular
  ;; method lambda returned and stores it in
  ;; *documented-reader-method-functions-makers* to be re-used each time
  ;; the reader lambda is the same.
  (multiple-value-bind (method-lambda initargs)
    (call-make-method-lambda generic-function
                             reader-method-prototype
                             '(lambda (instance)
                                (funcall #'slot-value instance slot-name))
                             ())
    (declare (ignore initargs))
    (funcall-compiled
       (or (cdr (assoc method-lambda
                       *documented-reader-method-function-makers*
                       :test #'equal))
           (let ((reader-function-maker-name
                   (gensym "MAKE-DOCUMENTED-READER-METHOD-FUNCTION"))
                 (compiled-lambda
                   (compile-lambda
                     `(lambda (slot-name) (function ,method-lambda)))))
             (declare (type compiled-function compiled-lambda))
             (setf (symbol-function reader-function-maker-name)
                   compiled-lambda)
             (let ((func (eval `(function ,reader-function-maker-name))))
               (push (cons method-lambda func)
                     *documented-reader-method-function-makers*)
               func)))
      slot-name)))

(defmethod make-documented-writer-method-function ((class slot-class)
                                                   generic-function
                                                   writer-method-prototype
                                                   slot-name)
  ;;   Make the documented writer-method-function.  To do this correctly for
  ;; all cases, we must build the writer method function by passing it
  ;; through make-method-lambda in case the user does something funky
  ;; with their method lambdas.
  ;;   Since there usually won't be many different kinds of method
  ;; lambdas for writers, this method actually dynamically builds a function
  ;; to make the documented-writer-method-function from each particular
  ;; method lambda returned and stores it in
  ;; *documented-writer-method-functions-makers* to be re-used each time
  ;; the writer lambda is the same.
  (multiple-value-bind (method-lambda initargs)
    (call-make-method-lambda generic-function
                             writer-method-prototype
                             '(lambda (nv instance)
                               (funcall #'set-slot-value instance slot-name nv))
                             ())
    (declare (ignore initargs))
    (funcall-compiled
       (or (cdr (assoc method-lambda
                       *documented-writer-method-function-makers*
                       :test #'equal))
           (let ((writer-function-maker-name
                   (gensym "MAKE-DOCUMENTED-WRITER-METHOD-FUNCTION"))
                 (compiled-lambda
                   (compile-lambda
                     `(lambda (slot-name) (function ,method-lambda)))))
             (declare (type compiled-function compiled-lambda))
             (setf (symbol-function writer-function-maker-name)
                   compiled-lambda) 
             (let ((func (eval `(function ,writer-function-maker-name))))
               (push (cons method-lambda func)
                     *documented-writer-method-function-makers*)
               func)))
       slot-name)))

(defmethod make-documented-boundp-method-function ((class slot-class)
                                                   generic-function
                                                   boundp-method-prototype
                                                   slot-name)
  ;;   Make the documented boundp-method-function.  To do this correctly for
  ;; all cases, we must build the boundp method function by passing it
  ;; through make-method-lambda in case the user does something funky
  ;; with their method lambdas.
  ;;   Since there usually won't be many different kinds of method
  ;; lambdas for boundps, this method actually dynamically builds a function
  ;; to make the documented-boundp-method-function from each particular
  ;; method lambda returned and stores it in
  ;; *documented-boundp-method-functions-makers* to be re-used each time
  ;; the boundp lambda is the same.
  (multiple-value-bind (method-lambda initargs)
    (call-make-method-lambda generic-function
                             boundp-method-prototype
                             '(lambda (instance)
                               (funcall #'slot-value instance slot-name))
                             ())
    (declare (ignore initargs))
    (funcall-compiled
       (or (cdr (assoc method-lambda
                       *documented-boundp-method-function-makers*
                       :test #'equal))
           (let ((boundp-function-maker-name
                   (gensym "MAKE-DOCUMENTED-READER-METHOD-FUNCTION"))
                 (compiled-lambda
                   (compile-lambda
                     `(lambda (slot-name) (function ,method-lambda)))))
             (declare (type compiled-function compiled-lambda))
             (setf (symbol-function boundp-function-maker-name)
                   compiled-lambda)
             (let ((func (eval `(function ,boundp-function-maker-name))))
               (push (cons method-lambda func)
                     *documented-boundp-method-function-makers*)
               func)))
      slot-name)))

(defmethod make-optimized-reader-method-function ((class slot-class)
                                                  generic-function
                                                  reader-method-prototype
                                                  slot-name)
  (declare (ignore generic-function reader-method-prototype))
  (make-std-reader-method-function slot-name))

(defmethod make-optimized-writer-method-function ((class slot-class)
                                                  generic-function
                                                  writer-method-prototype
                                                  slot-name)
  (declare (ignore generic-function writer-method-prototype))
  (make-std-writer-method-function slot-name))

(defmethod make-optimized-boundp-method-function ((class slot-class)
                                                  generic-function
                                                  boundp-method-prototype
                                                  slot-name)
  (declare (ignore generic-function boundp-method-prototype))
  (make-std-boundp-method-function slot-name))

(defmethod make-optimized-reader-method-function ((class standard-class)
                                                  generic-function
                                                  reader-method-prototype
                                                  slot-name)
  (declare (ignore generic-function reader-method-prototype))
  (make-standard-instance-reader-method-function slot-name))

(defmethod make-optimized-writer-method-function ((class standard-class)
                                                  generic-function
                                                  writer-method-prototype
                                                  slot-name)
  (declare (ignore generic-function writer-method-prototype))
  (make-standard-instance-writer-method-function slot-name))

(defmethod make-optimized-boundp-method-function ((class standard-class)
                                                  generic-function
                                                  boundp-method-prototype
                                                  slot-name)
  (declare (ignore generic-function boundp-method-prototype))
  (make-standard-instance-boundp-method-function slot-name))


(defun make-standard-instance-reader-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (instance)
      (standard-instance-slot-value instance slot-name)))

(defun make-standard-instance-writer-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (nv instance)
      (setf (standard-instance-slot-value instance slot-name) nv)))

(defun make-standard-instance-boundp-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (instance)
      (standard-instance-slot-boundp instance slot-name)))



(defvar *internal-reader-gf-table* (make-hash-table :test 'eql))
(defvar *internal-writer-gf-table* (make-hash-table :test 'eql))
(defvar *internal-boundp-gf-table* (make-hash-table :test 'eql))

(defun get-reader-function (slot-name)
  (or (gethash slot-name *internal-reader-gf-table*)
      (error "No class has a slot named ~s" slot-name)))

(defun get-writer-function (slot-name)
  (or (gethash slot-name *internal-writer-gf-table*)
      (error "No class has a slot named ~s" slot-name)))

(defun get-boundp-function (slot-name)
  (or (gethash slot-name *internal-boundp-gf-table*)
      (error "No class has a slot named ~s" slot-name)))

(defun initialize-internal-slot-reader-gfs (slot-name)
  (unless (gethash slot-name *internal-reader-gf-table*)
    (let* ((name (slot-reader-symbol slot-name))
	   (gf (setf (gethash slot-name *internal-reader-gf-table*)
		     (ensure-generic-function name))))
      (add-reader-method *the-class-slot-object* gf slot-name))))

(defun initialize-internal-slot-writer-gfs (slot-name)
  (unless (gethash slot-name *internal-writer-gf-table*)
    (let* ((name (slot-writer-symbol slot-name))
	   (gf (setf (gethash slot-name *internal-writer-gf-table*)
		     (ensure-generic-function name))))
      (add-writer-method *the-class-slot-object* gf slot-name))))

(defun initialize-internal-slot-boundp-gfs (slot-name)
  (unless (or (not *optimize-slot-boundp*)
	      (gethash slot-name *internal-boundp-gf-table*))
    (let* ((name (slot-boundp-symbol slot-name))
	   (gf (setf (gethash slot-name *internal-boundp-gf-table*)
		     (ensure-generic-function name))))
      (add-boundp-method *the-class-slot-object* gf slot-name))))


;;;; inform-type-system-about-class
;;;; make-type-predicate
;;;
;;; These are NOT part of the standard protocol.  They are internal mechanism
;;; which PCL uses to *try* and tell the type system about class definitions.
;;; In a more fully integrated implementation of CLOS, the type system would
;;; know about class objects and class names in a more fundamental way and
;;; the mechanism used to inform the type system about new classes would be
;;; different.
;;;
(defmethod inform-type-system-about-class ((class std-class) name)
  (inform-type-system-about-std-class name))



;;;
;;; These 4 definitions appear here for bootstrapping reasons.  Logically,
;;; they should be in the construct file.  For documentation purposes, a
;;; copy of these definitions appears in the construct file.  If you change
;;; one of the definitions here, be sure to change the copy there.
;;; 
(defvar *initialization-generic-functions*
	(list #'make-instance
	      #'default-initargs
	      #'allocate-instance
	      #'initialize-instance
	      #'shared-initialize))

(defmethod maybe-update-constructors
	   ((generic-function generic-function)
	    (method method))
  (when (memq generic-function *initialization-generic-functions*)
    (labels ((recurse (class)
	       (update-constructors class)
	       (dolist (subclass (class-direct-subclasses class))
		 (recurse subclass))))
      (when (classp (car (method-specializers method)))
	(recurse (car (method-specializers method)))))))

(defmethod update-constructors ((class slot-class))
  (dolist (cons (class-constructors class))
    (install-lazy-constructor-installer cons)))

(defmethod update-constructors ((class class))
  ())



(defmethod compatible-meta-class-change-p (class proto-new-class)
  (eq (class-of class) (class-of proto-new-class)))

(defmethod validate-superclass ((class class) (new-super class))
  (or (eq new-super *the-class-t*)
      (eq (class-of class) (class-of new-super))))



;;;
;;;
;;;
(defun force-cache-flushes (class)
  (let* ((owrapper (class-wrapper class))
	 (state (wrapper-state owrapper)))
    ;;
    ;; We only need to do something if the state is still T.  If the
    ;; state isn't T, it will be FLUSH or OBSOLETE, and both of those
    ;; will already be doing what we want.  In particular, we must be
    ;; sure we never change an OBSOLETE into a FLUSH since OBSOLETE
    ;; means do what FLUSH does and then some.
    ;; 
    (when (eq state 't)
      (let ((nwrapper (make-wrapper class)))
	(setf (wrapper-instance-slots-layout nwrapper)
	      (wrapper-instance-slots-layout owrapper))
	(setf (wrapper-class-slots nwrapper)
	      (wrapper-class-slots owrapper))
        (setf (wrapper-class-precedence-list nwrapper)
              (wrapper-class-precedence-list owrapper))
	(setf (wrapper-allocate-static-slot-storage-copy nwrapper)
	      (wrapper-allocate-static-slot-storage-copy owrapper))
	(setf (wrapper-unreserved-field nwrapper)
	      (wrapper-unreserved-field owrapper))
	(without-interrupts-simple
	  (setf (slot-value class 'wrapper) nwrapper)
	  (invalidate-wrapper owrapper 'flush nwrapper))
	(update-constructors class)))))		;??? ***

(defun flush-cache-trap (owrapper nwrapper instance)
  (declare (ignore owrapper))
  (set-wrapper instance nwrapper))



;;;
;;; make-instances-obsolete can be called by user code.  It will cause the
;;; next access to the instance (as defined in 88-002R) to trap through the
;;; update-instance-for-redefined-class mechanism.
;;; 

(defmethod make-instances-obsolete ((class std-class))
  (if *old-update-slots-wrapper*
      (let ((owrapper *old-update-slots-wrapper*))
        (setf *old-update-slots-wrapper* NIL)
        (without-interrupts-simple
          (invalidate-wrapper owrapper 'obsolete (class-wrapper class)))
        (dolist (generic-function (class-cached-in-generic-functions class))
          (update-dfun generic-function)))
      (let ((owrapper (class-wrapper class))
            (nwrapper (make-wrapper  class)))
        (setf (wrapper-instance-slots-layout nwrapper)
	      (wrapper-instance-slots-layout owrapper))
        (setf (wrapper-class-slots nwrapper)
	      (wrapper-class-slots owrapper))
        (setf (wrapper-class-precedence-list nwrapper)
              (wrapper-class-precedence-list owrapper))
        (setf (wrapper-allocate-static-slot-storage-copy nwrapper)
	      (wrapper-allocate-static-slot-storage-copy owrapper))
        (setf (wrapper-unreserved-field nwrapper)
	      (wrapper-unreserved-field owrapper))
        (without-interrupts-simple
	  (setf (slot-value class 'wrapper) nwrapper)
	  (invalidate-wrapper owrapper 'obsolete nwrapper)
	  class))))

(defmethod make-instances-obsolete ((class symbol))
  (make-instances-obsolete (find-class class)))


;;;
;;; obsolete-instance-trap is the internal trap that is called when we see
;;; an obsolete instance.  The times when it is called are:
;;;   - when the instance is involved in method lookup
;;;   - when attempting to access a slot of an instance
;;;
;;; It is not called by class-of, wrapper-of, or any of the low-level instance
;;; access macros.
;;;
;;; Of course these times when it is called are an internal implementation
;;; detail of PCL and are not part of the documented description of when the
;;; obsolete instance update happens.  The documented description is as it
;;; appears in 88-002R.
;;;
;;; This has to return the new wrapper, so it counts on all the methods on
;;; obsolete-instance-trap-internal to return the new wrapper.  It also does
;;; a little internal error checking to make sure that the traps are only
;;; happening when they should, and that the trap methods are computing
;;; apropriate new wrappers.
;;; 
(defun obsolete-instance-trap (owrapper nwrapper instance)  
  ;;
  ;; local  --> local        transfer 
  ;; local  --> shared       discard
  ;; local  -->  --          discard
  ;; shared --> local        transfer
  ;; shared --> shared       discard
  ;; shared -->  --          discard
  ;;  --    --> local        add
  ;;  --    --> shared        --
  ;;
  (unless (or (std-instance-p instance) (fsc-instance-p instance))
    (error "Trying to obsolete instance ~S of type ~S, but only know how
            to obsolete instances of type STD-INSTANCE or FSC-INSTANCE."
           instance (instance-type instance)))
  (let* ((class (wrapper-class nwrapper))
	 (guts (allocate-instance class))	;??? allocate-instance ???
	 (olayout (wrapper-instance-slots-layout owrapper))
	 (nlayout (wrapper-instance-slots-layout nwrapper))
	 (oslots (get-slots instance))
	 (nslots (get-slots guts))
	 (oclass-slots (wrapper-class-slots owrapper))
	 (added ())
	 (discarded ())
	 (plist ()))
    (declare (list olayout nlayout oclass-slots added discarded plist)
             (simple-vector oslots nslots))
    ;;
    ;; Go through all the old local slots.
    ;; 
    (iterate ((name (list-elements olayout))
	      (opos (interval :from 0)))
      (let ((npos (posq name nlayout)))
	(if npos
	    (setf (svref nslots npos) (svref oslots opos))
	    (progn (push name discarded)
		   (unless (eq (svref oslots opos) *slot-unbound*)
		     (setf (getf plist name) (svref oslots opos)))))))
    ;;
    ;; Go through all the old shared slots.
    ;;
    (iterate ((oclass-slot-and-val (list-elements oclass-slots)))
      (let ((name (car oclass-slot-and-val))
	    (val (cdr oclass-slot-and-val)))
	(let ((npos (posq name nlayout)))
	  (if npos
	      (setf (svref nslots npos) (cdr oclass-slot-and-val))
	      (progn (push name discarded)
		     (unless (eq val *slot-unbound*)
		       (setf (getf plist name) val)))))))
    ;;
    ;; Go through all the new local slots to compute the added slots.
    ;; 
    (dolist (nlocal nlayout)
      (unless (or (memq nlocal olayout)
		  (assq nlocal oclass-slots))
	(push nlocal added)))
      
    (without-interrupts-simple
      (set-wrapper instance nwrapper)
      (set-slots instance nslots))

    (update-instance-for-redefined-class instance
					 added
					 discarded
					 plist)
    nwrapper))



;;;
;;;
;;;
(defmacro change-class-internal (wrapper-fetcher slots-fetcher alloc)
  `(let* ((old-class (class-of instance))
	  (copy (,alloc old-class))
	  (guts (,alloc new-class))
	  (new-wrapper (,wrapper-fetcher guts))
	  (old-wrapper (class-wrapper old-class))
	  (old-layout (wrapper-instance-slots-layout old-wrapper))
	  (new-layout (wrapper-instance-slots-layout new-wrapper))
	  (old-slots (,slots-fetcher instance))
	  (new-slots (,slots-fetcher guts))
	  (old-class-slots (wrapper-class-slots old-wrapper)))
    (declare (list old-layout new-layout old-class-slots)
             (simple-vector old-slots new-slots))

    ;;
    ;; "The values of local slots specified by both the class Cto and
    ;; Cfrom are retained.  If such a local slot was unbound, it remains
    ;; unbound."
    ;;     
    (iterate ((new-slot (list-elements new-layout))
	      (new-position (interval :from 0)))
      (let ((old-position (posq new-slot old-layout)))
	(when old-position
	  (setf (svref new-slots new-position)
		(svref old-slots old-position)))))

    ;;
    ;; "The values of slots specified as shared in the class Cfrom and
    ;; as local in the class Cto are retained."
    ;;
    (iterate ((slot-and-val (list-elements old-class-slots)))
      (let ((position (posq (car slot-and-val) new-layout)))
	(when position
	  (setf (svref new-slots position) (cdr slot-and-val)))))

    ;; Make the copy point to the old instance's storage, and make the
    ;; old instance point to the new storage.
    (without-interrupts-simple
      (setf (,slots-fetcher copy) old-slots)
      
      (setf (,wrapper-fetcher instance) new-wrapper)
      (setf (,slots-fetcher instance) new-slots))

    (update-instance-for-different-class copy instance)
    instance))

(defmethod change-class ((instance standard-object)
			 (new-class standard-class))
  (unless (std-instance-p instance)
    (error "Can't change the class of ~S to ~S~@
            because it isn't already an instance with metaclass~%~S."
	   instance
	   new-class
	   'standard-class))
  (change-class-internal std-instance-wrapper
			 std-instance-slots
			 allocate-instance))

(defmethod change-class ((instance standard-object)
			 (new-class funcallable-standard-class))
  (unless (fsc-instance-p instance)
    (error "Can't change the class of ~S to ~S~@
            because it isn't already an instance with metaclass~%~S."
	   instance
	   new-class
	   'funcallable-standard-class))
  (change-class-internal fsc-instance-wrapper
			 fsc-instance-slots
			 allocate-instance))

(defmethod change-class ((instance t) (new-class-name symbol))
  (change-class instance (find-class new-class-name)))



;;;
;;; The metaclass BUILT-IN-CLASS
;;;
;;; This metaclass is something of a weird creature.  By this point, all
;;; instances of it which will exist have been created, and no instance
;;; is ever created by calling MAKE-INSTANCE.
;;;
;;; But, there are other parts of the protcol we must follow and those
;;; definitions appear here.
;;; 
(defmethod shared-initialize :before
	   ((class built-in-class) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (error "Attempt to initialize or reinitialize a built in class."))

(defmethod validate-superclass ((c class) (s built-in-class))
  (eq s *the-class-t*))



;;;
;;;
;;;

(defmethod validate-superclass ((c slot-class)
				(f forward-referenced-class))
  't)


;;;
;;;
;;;

(defmethod add-dependent ((metaobject dependent-update-mixin) dependent)
  (pushnew dependent (plist-value metaobject 'dependents)))

(defmethod remove-dependent ((metaobject dependent-update-mixin) dependent)
  (setf (plist-value metaobject 'dependents)
	(delete dependent (plist-value metaobject 'dependents))))

(defmethod map-dependents ((metaobject dependent-update-mixin) function)
  (declare (type real-function function))
  (dolist (dependent (plist-value metaobject 'dependents))
    (funcall function dependent)))


(declaim (ftype (function (T T) boolean) class-on-class-precedence-list-p))
(defun class-on-class-precedence-list-p (class1 class2)
  ;; Return whether class1 is on class2's class-precedence-list
  ;; without finalizing it.
  (if (class-finalized-p class2)
      (not (null (memq class1 (fast-slot-value class2 'class-precedence-list))))
      (let ((direct-superclasses (class-direct-superclasses class2)))
        (if (memq class1 direct-superclasses)
            T
            (dolist (superclass direct-superclasses NIL)
              (if (class-on-class-precedence-list-p class1 superclass)
                  (return T)))))))

(declaim (ftype (function (T) boolean) class-standard-p))
(defun class-standard-p (class)
  ;; Return whether class is standard without finalizing it.
  (if (class-finalized-p class)
      (not (null (memq *the-class-standard-object*
                       (fast-slot-value class 'class-precedence-list))))
      (let ((direct-superclasses (class-direct-superclasses class)))
        (if (memq *the-class-standard-object* direct-superclasses)
            T
            (dolist (superclass direct-superclasses NIL)
              (if (class-standard-p superclass)
                  (return T)))))))


