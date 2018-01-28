;;;-*-Mode:LISP; Package:(PCL (LISP WALKER)); Base:10; Syntax:Common-lisp -*-
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
;;; Bootstrapping the meta-braid.
;;;
;;; The code in this file takes the early definitions that have been saved
;;; up and actually builds those class objects.  This work is largely driven
;;; off of those class definitions, but the fact that STANDARD-CLASS is the
;;; class of all metaclasses in the braid is built into this code pretty
;;; deeply.
;;;
;;; 

(in-package 'pcl)

(defun early-class-name (class)
  (bootstrap-get-slot 'class class 'name))

(defun early-class-definition (class-name)
  (or (find class-name *early-class-definitions* :key #'ecd-class-name)
      (error "~S is not a class in *early-class-definitions*." class-name)))

(defun canonical-slot-name (canonical-slot)
  (getf canonical-slot :name))

(defun early-collect-inheritance (class-name)
  (declare (values slots cpl default-initargs direct-subclasses))
  (let ((cpl (early-collect-cpl class-name)))
    (values (early-collect-slots cpl)
	    cpl
	    (early-collect-default-initargs cpl)
	    (gathering1 (collecting)
	      (dolist (definition *early-class-definitions*)
		(when (memq class-name (ecd-superclass-names definition))
		  (gather1 (ecd-class-name definition))))))))

(defun early-collect-cpl (class-name)
  (labels ((walk (c)
	     (let* ((definition (early-class-definition c))
		    (supers (ecd-superclass-names definition)))
	       (cons c
		     (apply #'append (mapcar #'early-collect-cpl supers))))))
    (remove-duplicates (walk class-name) :from-end nil :test #'eq)))

(defun early-collect-slots (cpl)
  (let* ((definitions (mapcar #'early-class-definition cpl))
	 (super-slots (mapcar #'ecd-canonical-slots definitions))
	 (slots (apply #'append (reverse super-slots))))
    (dolist (s1 slots)
      (let ((name1 (canonical-slot-name s1)))
	(dolist (s2 (cdr (memq s1 slots)))
	  (when (eq name1 (canonical-slot-name s2))
	    (error "More than one early class defines a slot with the~%~
                    name ~S.  This can't work because the bootstrap~%~
                    object system doesn't know how to compute effective~%~
                    slots."
		   name1)))))
    slots))

(defun early-collect-default-initargs (cpl)
  (let ((default-initargs ()))
    (dolist (class-name cpl)
      (let ((definition (early-class-definition class-name)))
	(dolist (option (ecd-other-initargs definition))
	  (unless (eq (car option) :default-initargs)
	    (error "The defclass option ~S is not supported by the bootstrap~%~
                    object system."
		   (car option)))
	  (setq default-initargs
		(nconc default-initargs (reverse (cdr option)))))))
    (reverse default-initargs)))


;;;
;;; bootstrap-get-slot and bootstrap-set-slot are used to access and change
;;; the values of slots during bootstrapping.  During bootstrapping, there
;;; are only two kinds of objects whose slots we need to access, CLASSes
;;; and SLOT-DEFINITIONs.  The first argument to these functions tells whether the
;;; object is a CLASS or a SLOT-DEFINITION.
;;;
;;; Note that the way this works it stores the slot in the same place in
;;; memory that the full object system will expect to find it later.  This
;;; is critical to the bootstrapping process, the whole changeover to the
;;; full object system is predicated on this.
;;;
;;; One important point is that the layout of standard classes and standard
;;; slots must be computed the same way in this file as it is by the full
;;; object system later.
;;; 
(defun bootstrap-get-slot (type object slot-name)
  (let ((index (bootstrap-slot-index type slot-name)))
    (svref (std-instance-slots object) index)))

(defun bootstrap-set-slot (type object slot-name new-value)
  (let ((index (bootstrap-slot-index type slot-name)))
    (setf (svref (std-instance-slots object) index) new-value)))

(defvar *early-class-slots* nil)

(defun early-class-slots (class-name)
  (cdr (or (assoc class-name *early-class-slots*)
	   (let ((a (cons class-name
			  (mapcar #'canonical-slot-name
				  (early-collect-inheritance class-name)))))
	     (push a *early-class-slots*)
	     a))))

(defun early-class-original-static-slot-storage-copy (class-name)
  (%allocate-origional-static-slot-storage-copy
    (length (the list (early-class-slots class-name)))))

(defun bootstrap-slot-index (class-name slot-name)
  (or (posq slot-name (the list (early-class-slots class-name)))
      (error "~S not found" slot-name)))


;;;
;;; bootstrap-meta-braid
;;;
;;; This function builds the base metabraid from the early class definitions.
;;;   
(defun bootstrap-meta-braid ()
  (let* ((slot-class-original-slot-copy
           (early-class-original-static-slot-storage-copy 'slot-class))
	 (standard-class-original-slot-copy
           (early-class-original-static-slot-storage-copy 'standard-class))
	 (built-in-class-original-slot-copy
           (early-class-original-static-slot-storage-copy 'built-in-class))
	 (structure-class-original-slot-copy
           (early-class-original-static-slot-storage-copy 'structure-class))
         (slot-class      (%allocate-instance--class standard-class-original-slot-copy))
         (standard-class  (%allocate-instance--class standard-class-original-slot-copy))
         (built-in-class  (%allocate-instance--class standard-class-original-slot-copy))
         (structure-class (%allocate-instance--class standard-class-original-slot-copy))
	 (direct-slotd    (%allocate-instance--class standard-class-original-slot-copy))
	 (effective-slotd (%allocate-instance--class standard-class-original-slot-copy))
	 (class-eq        (%allocate-instance--class standard-class-original-slot-copy))
	 (slot-class-wrapper      (make-wrapper slot-class))
	 (standard-class-wrapper  (make-wrapper standard-class))
	 (built-in-class-wrapper  (make-wrapper built-in-class))
	 (structure-class-wrapper (make-wrapper structure-class))
	 (direct-slotd-wrapper    (make-wrapper direct-slotd))
	 (effective-slotd-wrapper (make-wrapper effective-slotd))
	 (class-eq-wrapper        (make-wrapper class-eq)))
    ;;
    ;; First, make a class metaobject for each of the early classes.  For
    ;; each metaobject we also set its wrapper.  Except for the class T,
    ;; the wrapper is always that of STANDARD-CLASS.
    ;; 
    (dolist (definition *early-class-definitions*)
      (let* ((name (ecd-class-name definition))
	     (meta (ecd-metaclass definition))
	     (original-slot-copy
               (ecase meta
		 (slot-class slot-class-original-slot-copy)
		 (standard-class standard-class-original-slot-copy)
		 (built-in-class built-in-class-original-slot-copy)
		 (structure-class structure-class-original-slot-copy)))
             (class (case name
		      (slot-class                         slot-class)
                      (standard-class                     standard-class)
                      (standard-direct-slot-definition    direct-slotd)
		      (standard-effective-slot-definition effective-slotd)
		      (built-in-class                     built-in-class)
		      (structure-class                    structure-class)
		      (class-eq-specializer               class-eq)
                      (otherwise (%allocate-instance--class original-slot-copy)))))
	(when (eq meta 'standard-class)
	  (inform-type-system-about-class class name))
	(setf (std-instance-wrapper class)
	      (ecase meta
		(slot-class slot-class-wrapper)
		(standard-class standard-class-wrapper)
		(built-in-class built-in-class-wrapper)
		(structure-class structure-class-wrapper)))
        (setf (find-class name) class)))
    ;;
    ;;
    ;;
    (dolist (definition *early-class-definitions*)
      (let ((name (ecd-class-name definition))
	    (meta (ecd-metaclass definition))
	    (source (ecd-source definition))
	    (direct-supers (ecd-superclass-names definition))
	    (direct-slots  (ecd-canonical-slots definition))
	    (other-initargs (ecd-other-initargs definition)))
	(let ((direct-default-initargs
		(getf other-initargs :default-initargs)))
	  (multiple-value-bind (slots cpl default-initargs direct-subclasses)
	      (early-collect-inheritance name)
            (declare (type list slots cpl default-initargs direct-subclasses))
	    (let* ((class (find-class name))
		   (wrapper
		     (cond
		       ((eq class slot-class)      slot-class-wrapper)
		       ((eq class standard-class)  standard-class-wrapper)
		       ((eq class direct-slotd)    direct-slotd-wrapper)
		       ((eq class effective-slotd) effective-slotd-wrapper)
		       ((eq class built-in-class)  built-in-class-wrapper)
		       ((eq class structure-class) structure-class-wrapper)
		       ((eq class class-eq)        class-eq-wrapper)
		       (t (make-wrapper class))))
		   (proto nil)
                   (slot-names
		    (mapcar #'canonical-slot-name slots))
                   (static-slot-copy
                    (%allocate-origional-static-slot-storage-copy (length slot-names))))
              (declare (type list slot-names))
	      (cond ((eq name 't)
		     (setq *the-wrapper-of-t* wrapper
			   *the-class-t* class))
		    ((memq name '(slot-object
				  standard-object
				  structure-object
				  built-in-class
				  slot-class
				  standard-class
				  funcallable-standard-class
				  structure-class
				  standard-direct-slot-definition
				  standard-effective-slot-definition))
		     (set (intern (format nil "*THE-CLASS-~A*" (symbol-name name))
				  *the-pcl-package*)
			  class)))
	      (dolist (slot slots)
		(unless (eq (getf slot :allocation :instance) :instance)
		  (error "Slot allocation ~S not supported in bootstrap.")))
	      
	      (setf (wrapper-instance-slots-layout wrapper) slot-names)
	      (setf (wrapper-allocate-static-slot-storage-copy wrapper)
                    static-slot-copy)
	      (setf (wrapper-class-slots wrapper)
		    ())
	      
	      (setq proto (%allocate-instance--class static-slot-copy))
	      (setf (std-instance-wrapper proto) wrapper)
	    
	      (setq direct-slots
		    (bootstrap-make-slot-definitions 
		      name class direct-slots direct-slotd-wrapper nil))
	      (setq slots
		    (bootstrap-make-slot-definitions 
		      name class slots effective-slotd-wrapper t))
	      
	      (case meta
		(standard-class
		 (bootstrap-initialize-standard-class
		  class name class-eq-wrapper source
		  direct-supers direct-subclasses cpl wrapper
		  direct-slots slots direct-default-initargs
		  default-initargs proto))
		(built-in-class ; *the-class-t*
		 (bootstrap-initialize-built-in-class
		  class name class-eq-wrapper source
		  direct-supers direct-subclasses cpl wrapper
		  proto nil))
		(slot-class ; *the-class-slot-object*
		 (bootstrap-initialize-built-in-class
		  class name class-eq-wrapper source
		  direct-supers direct-subclasses cpl wrapper
		  proto nil)
		 (bootstrap-set-slot 'slot-class class 'direct-slots nil)
		 (bootstrap-set-slot 'slot-class class 'slots nil)
		 (bootstrap-set-slot 'slot-class class 'internal-slotds nil)
		 (bootstrap-set-slot 'slot-class class
                                     'side-effect-internal-slotds nil))
		(structure-class ; *the-class-structure-object*
		 (bootstrap-initialize-structure-class
		  class name class-eq-wrapper source
		  direct-supers direct-subclasses cpl wrapper)))
	      
	      (let ((class-name 'standard-direct-slot-definition))
		(dolist (slotd direct-slots)
		  (bootstrap-accessor-definitions
		    name
		    (bootstrap-get-slot class-name slotd 'name)
		    (bootstrap-get-slot class-name slotd 'readers)
		    (bootstrap-get-slot class-name slotd 'writers)))))))))))

(defun bootstrap-accessor-definitions (class-name slot-name readers writers)
  (flet ((do-reader-definition (reader)
           (let ((optimized-method-function
                   (make-std-reader-method-function slot-name))
                 (generic-function
                   (ensure-generic-function reader)))
             (add-method
               generic-function
               (make-a-method
                 'standard-reader-method
                 ()
                 (list class-name)
                 (list class-name)
                 (when (call-store-method-function-p generic-function nil nil)
                   (make-documented-std-reader-method-function slot-name))
                 optimized-method-function
                 NIL
                 "automatically generated reader method"
                 slot-name
                 `(:needs-next-methods-p NIL)))))
         (do-writer-definition (writer)
           (let ((optimized-method-function
                   (make-std-writer-method-function slot-name))
                 (generic-function
                   (ensure-generic-function writer)))
             (add-method
               generic-function
               (make-a-method
                 'standard-writer-method
                 ()
                 (list 'new-value class-name)
                 (list 't class-name)
                 (when (call-store-method-function-p generic-function nil nil)
                   (make-documented-std-writer-method-function slot-name))
                 optimized-method-function
                 NIL
                 "automatically generated writer method"
                 slot-name
                 `(:needs-next-methods-p NIL))))))
    (dolist (reader readers) (do-reader-definition reader))
    (dolist (writer writers) (do-writer-definition writer))))


;;;
;;; Initialize a standard class metaobject.
;;;
(defun bootstrap-initialize-standard-class
       (class
	name class-eq-wrapper definition-source direct-supers direct-subclasses cpl
	wrapper direct-slots slots direct-default-initargs default-initargs proto)
  (declare (type list direct-slots slots))
  (flet ((classes (names) (mapcar #'find-class names))
	 (set-slot (slot-name value)
	   (bootstrap-set-slot 'standard-class class slot-name value)))
    (let ((class-precedence-list (classes cpl)))
    
      (set-slot 'default-initargs default-initargs)
      (set-slot 'direct-slots direct-slots)
      (set-slot 'direct-default-initargs direct-default-initargs)
      (set-slot 'direct-subclasses (classes direct-subclasses))
      (set-slot 'direct-superclasses (classes direct-supers))
      (set-slot 'finalized-p T)
      (set-slot 'name name)
      (set-slot 'class-precedence-list class-precedence-list)
      (set-slot 'prototype proto)
      (set-slot 'slots slots)

      (set-slot 'cached-in-generic-functions ())
      (set-slot 'can-precede-list (classes (cdr cpl)))
      (set-slot 'class-eq-specializer 
	        (let ((spec (%allocate-instance--class
                              (early-class-original-static-slot-storage-copy
                                'class-eq-specializer))))
		  (setf (std-instance-wrapper spec) class-eq-wrapper)
		  (bootstrap-set-slot 'class-eq-specializer spec 'type 
				      `(class-eq ,class))
		  (bootstrap-set-slot 'class-eq-specializer spec 'object
				      class)
		  spec))
      (set-slot 'direct-methods (cons nil nil))
      (set-slot 'incompatible-superclass-list nil)
      (set-slot 'predicate-name (or (cadr (assoc name *early-class-predicates*))
				    (make-class-predicate-name name)))
      (setf (wrapper-class-precedence-list wrapper) class-precedence-list)
      (set-slot 'wrapper wrapper)

      (set-slot 'plist nil)
      (set-slot 'source definition-source)
      (set-slot 'type `(class ,class))
      (set-slot 'documentation NIL)

      (let ((internal-slotds ())
            (side-effect-internal-slotds ()))
        (dolist (slot slots)
          (let ((internal-slotd
                  (bootstrap-get-slot 'standard-effective-slot-definition
                                      slot 'internal-slotd)))
            (push internal-slotd internal-slotds)
            (unless (bootstrap-get-slot 'standard-effective-slot-definition
                                        slot
                                        'initfunction-side-effect-free-p)
              (push internal-slotd side-effect-internal-slotds))))
        (set-slot 'internal-slotds (nreverse internal-slotds))
        (set-slot 'side-effect-internal-slotds
                  (nreverse side-effect-internal-slotds)))
      (setf (wrapper-allocate-static-slot-storage-copy wrapper)
            (%allocate-origional-static-slot-storage-copy (length slots)))
    )))

;;;
;;; Initialize a built-in-class metaobject.
;;;
(defun bootstrap-initialize-built-in-class
       (class name class-eq-wrapper source direct-supers direct-subclasses cpl 
	wrapper proto predicate-name)
  (flet ((classes (names) (mapcar #'find-class names))
	 (set-slot (slot-name value)
	   (bootstrap-set-slot 'built-in-class class slot-name value)))

    (set-slot 'default-initargs ())
    (set-slot 'direct-default-initargs ())
    (set-slot 'direct-slots ())
    (set-slot 'direct-subclasses (classes direct-subclasses))
    (set-slot 'direct-superclasses (classes direct-supers))
    (set-slot 'name name)
    (set-slot 'finalized-p T)
    (let ((real-cpl (classes cpl)))
      (set-slot 'class-precedence-list real-cpl)
      (setf (wrapper-class-precedence-list wrapper) real-cpl))
    (set-slot 'prototype
              (or proto
		  (let* ((proto (%allocate-instance--class *empty-vector*)))
		           (setf (std-instance-wrapper proto) wrapper)
		           proto)))
    (set-slot 'slots ())

    (set-slot 'source source)
    (set-slot 'type (if (eq class (find-class 't))
			t
			`(class ,class)))
    (set-slot 'class-eq-specializer 
	       (let ((spec (%allocate-instance--class
                             (early-class-original-static-slot-storage-copy
                                'class-eq-specializer))))
		 (setf (std-instance-wrapper spec) class-eq-wrapper)
		 (bootstrap-set-slot 'class-eq-specializer spec 'type 
				     `(class-eq ,class))
		 (bootstrap-set-slot 'class-eq-specializer spec 'object 
				     class)
		 spec))

    (set-slot 'direct-methods (cons nil nil))
    (set-slot 'can-precede-list (classes (cdr cpl)))
    (set-slot 'incompatible-superclass-list nil)
    (set-slot 'internal-slotds nil)
    (set-slot 'wrapper wrapper)
    (set-slot 'predicate-name (or (cadr (assoc name *early-class-predicates*))
				  predicate-name
				  (make-class-predicate-name name)))
    (set-slot 'plist nil)
    (set-slot 'documentation (format nil "Built-in-class ~S" name))))

(defun bootstrap-initialize-structure-class
    (class name class-eq-wrapper source direct-supers direct-subclasses cpl wrapper)
  (unless (eq name 'structure-object) (error "You forgot to do something"))
  (flet ((classes (names) (mapcar #'find-class names))
	 (set-slot (slot-name value)
	   (bootstrap-set-slot 'structure-class class slot-name value)))
    (let ((constructor-sym '|STRUCTURE-OBJECT class constructor|))

      (set-slot 'default-initargs nil)
      (set-slot 'direct-default-initargs nil)
      (set-slot 'direct-slots nil)
      (set-slot 'direct-subclasses (classes direct-subclasses))
      (set-slot 'direct-superclasses (classes direct-supers))
      (set-slot 'finalized-p T)
      (set-slot 'name name)
      (let ((real-cpl (classes cpl)))
        (set-slot 'class-precedence-list real-cpl)
        (setf (wrapper-class-precedence-list wrapper) real-cpl))
      (set-slot 'prototype (funcall (symbol-function constructor-sym)))
      (set-slot 'slots nil)

      (set-slot 'source source)
      (set-slot 'type `(class ,class))
      (set-slot 'class-eq-specializer 
		(let ((spec (%allocate-instance--class
                              (early-class-original-static-slot-storage-copy
                                 'class-eq-specializer))))
		  (setf (std-instance-wrapper spec) class-eq-wrapper)
		  (bootstrap-set-slot 'class-eq-specializer spec 'type 
				      `(class-eq ,class))
		  (bootstrap-set-slot 'class-eq-specializer spec 'object
				      class)
		  spec))
      (set-slot 'direct-methods (cons nil nil))
      (set-slot 'can-precede-list (classes (cdr cpl)))
      (set-slot 'incompatible-superclass-list nil)
      (set-slot 'wrapper wrapper)
      (set-slot 'predicate-name (or (cadr (assoc name *early-class-predicates*))
				    (make-class-predicate-name name)))
      (set-slot 'internal-slotds nil)
      (set-slot 'side-effect-internal-slotds nil)
      (set-slot 'defstruct-conc-name "|STRUCTURE-OBJECT class ")
      (set-slot 'defstruct-constructor constructor-sym)
      (set-slot 'from-defclass-p t)    
      (set-slot 'plist nil)
      (set-slot 'documentation NIL))))

(defun bootstrap-make-slot-definitions (name class slots wrapper effective-p)
  (let ((index -1))
    (declare (type fixnum index))
    (mapcar #'(lambda (slot)
		(incf index)
		(bootstrap-make-slot-definition
		  name class slot wrapper effective-p index))
	    slots)))

(defvar *early-slot-names* (make-hash-table :test 'eq))

(defun bootstrap-make-slot-definition (name class slot wrapper effective-p index)  
  (declare (ignore name))
  (let* ((slotd-class-name (if effective-p
			       'standard-effective-slot-definition
			       'standard-direct-slot-definition))
	 (slotd (%allocate-instance--class
                  (early-class-original-static-slot-storage-copy slotd-class-name)))
	 (slot-name (getf slot :name)))
    (setf (std-instance-wrapper slotd) wrapper)
    (flet ((get-val (name) (getf slot name))
	   (set-val (name val) (bootstrap-set-slot slotd-class-name slotd name val)))
      (let ((initfunction (get-val :initfunction))
            (initargs     (get-val :initargs)))
        (set-val 'name         slot-name)
        (set-val 'initform     (get-val :initform))
        (set-val 'initfunction initfunction)
        (set-val 'initargs     initargs)
        (set-val 'readers      (get-val :readers))
        (set-val 'writers      (get-val :writers))
        (set-val 'allocation   :instance)
        (set-val 'type         (get-val :type))
        (set-val 'class        class)
        (set-val 'documentation (get-val :documentation))
        (set-val 'initfunction-side-effect-free-p
                 (get-val :initfunction-side-effect-free-p))
        (when effective-p
	  (set-val 'location index)
	  (let* ((instance-type 'std-instance)
                 (reader-function
                   (make-optimized-std-reader-method-function 
	             instance-type slot-name index))
                 (writer-function
                   (make-optimized-std-writer-method-function 
	             instance-type slot-name index))
                 (boundp-function
                   (make-optimized-std-boundp-method-function 
	             instance-type slot-name index)))
	    (set-val 'reader-function reader-function)
	    (set-val 'writer-function writer-function)
	    (set-val 'boundp-function boundp-function)
	    (set-val 'accessor-flags 7)
            (let ((internal-slotd
                    (make-internal-slotd :name            slot-name
                                         :slot-definition slotd
                                         :location        index
                                         :initargs        initargs
                                         :initfunction    initfunction
                                         :reader-function reader-function
                                         :writer-function writer-function
                                         :boundp-function boundp-function)))
              (set-val 'internal-slotd internal-slotd)))
	    (push (cons class slotd) (gethash slot-name *early-slot-names*))))
      slotd)))

(defun early-initialize-slot-gfs ()
  (maphash #'(lambda (name class+slotd-list)
	       (let ((table (or (gethash name *name->class->slotd-table*)
				(setf (gethash name *name->class->slotd-table*)
				      (make-hash-table :test 'eq :size 5)))))
		 (dolist (c+s class+slotd-list)
		   (setf (gethash (car c+s) table) (cdr c+s)))
                 (unless *safe-to-use-slot-value-wrapper-optimizations-p*
                   (initialize-internal-slot-reader-gfs name))
                 (unless *safe-to-use-set-slot-value-wrapper-optimizations-p*
                   (initialize-internal-slot-writer-gfs name))
                 (unless *safe-to-use-slot-boundp-wrapper-optimizations-p*
                   (initialize-internal-slot-boundp-gfs name))))
	   *early-slot-names*)
  (clrhash *early-slot-names*))

(defun early-initialize-class-predicates ()
  (dolist (definition *early-class-definitions*)
    (let* ((name (ecd-class-name definition))
	   (class (find-class name)))
      (setf (find-class-predicate name)
	    (make-class-predicate class))))
  (dolist (e *built-in-classes*)
    (let* ((name (car e))
	   (class (find-class name))
	   (predicate-name (class-predicate-name class)))
      (setf (find-class-predicate name)
	    (if (fboundp predicate-name)
		(symbol-function predicate-name)
		(make-class-predicate class))))))

(defun bootstrap-built-in-classes ()
  ;;
  ;; First make sure that all the supers listed in *built-in-class-lattice*
  ;; are themselves defined by *built-in-class-lattice*.  This is just to
  ;; check for typos and other sorts of brainos.
  ;; 
  (dolist (e *built-in-classes*)
    (dolist (super (cadr e))
      (unless (or (eq super 't)
		  (assq super *built-in-classes*))
	(error "In *built-in-classes*: ~S has ~S as a super,~%~
                but ~S is not itself a class in *built-in-classes*."
	       (car e) super super))))

  ;;
  ;; In the first pass, we create a skeletal object to be bound to the
  ;; class name.
  ;;
  (let* ((built-in-class (find-class 'built-in-class))
	 (built-in-class-wrapper (class-wrapper built-in-class))
	 (built-in-class-original-slot-copy
           (early-class-original-static-slot-storage-copy 'built-in-class)))
    (dolist (e *built-in-classes*)
      (let ((class (%allocate-instance--class built-in-class-original-slot-copy)))
	(setf (std-instance-wrapper class) built-in-class-wrapper)
	(setf (find-class (car e)) class))))

  ;;
  ;; In the second pass, we initialize the class objects.
  ;;
  (let ((class-eq-wrapper (class-wrapper (find-class 'class-eq-specializer))))
    (dolist (e *built-in-classes*)
      (destructuring-bind (name supers subs cpl prototype predicate-name) e
	(let* ((class (find-class name))
	       (wrapper (make-wrapper class)))
	  (set (get-built-in-class-symbol name) class)
	  (set (get-built-in-wrapper-symbol name) wrapper)

	  (setf (wrapper-instance-slots-layout wrapper) ()
	        (wrapper-class-slots wrapper) ()
                (wrapper-allocate-static-slot-storage-copy wrapper) *empty-vector*)

	  (bootstrap-initialize-built-in-class class
					  name class-eq-wrapper nil
					  supers subs
					  (cons name cpl)
					  wrapper prototype predicate-name))))))


;;;
;;;
;;;

(defun wrapper-of (x) 
  (fast-wrapper-of x))

(defun class-of (x) (wrapper-class (fast-wrapper-of x)))

(defun structure-wrapper (x)
  (class-wrapper (find-class (structure-type x))))

(defvar find-structure-class nil)

(defun make-defclass-direct-slots-from-defstruct (structure-name)
  ;; Make the slot descriptions for defclass for structure STRUCTURE-NAME.
  ;;   If the structure was defined with PCL's redefined Defstruct macro,
  ;; build them from its trapped source.
  ;;   Otherwise hope that the lisp's low file redefined
  ;; Structure-type-slot-description-list et al. to get the info about
  ;; the structure from the lisp's internal record.  If not, then oh well...
  (let* ((defstruct-form (defstruct-form structure-name))
         (conc-name (defstruct-form-conc-name defstruct-form)))
    (declare (type simple-string conc-name))
    (if defstruct-form
        (mapcar
          #'(lambda (descrip)
              (let* ((name
                       (if (listp descrip) (car descrip) descrip))
                     (accessor-symbol
                      (intern
                        (concatenate 'simple-string
                                     conc-name (symbol-name name))
                        (symbol-package structure-name))))
                (if (listp descrip)
                    `(:name ,name
                      :initform ,(second descrip)
                      ,@(when (memq :type descrip)
                          `(:type ,(cadr (memq :type descrip))))
                      :defstruct-accessor-symbol ,accessor-symbol)
                    `(:name ,name
                      :initform NIL
                      :defstruct-accessor-symbol ,accessor-symbol))))
          (cdr defstruct-form))
        (if (known-structure-type-p structure-name)
	    (mapcar #'(lambda (slotd)
		        `(:name ,(structure-slotd-name slotd)
		          :defstruct-accessor-symbol 
		          ,(structure-slotd-accessor-symbol slotd)
		          :internal-reader-function 
		          ,(structure-slotd-reader-function slotd)
		          :internal-writer-function 
		          ,(structure-slotd-writer-function slotd)))
		    (structure-type-slot-description-list structure-name))
           NIL))))

(defun find-structure-class (symbol &key warn)
  (when warn
    (if (safe-subtypep symbol 'structure)
        (warn "Creating class for structure ~S even though don't know anything about it.~%Class will likely have incorrect slots and superclass information"
              symbol)
        (warn "Guessing that ~S is a structure-class, even though don't know anything about it.~%Class will likely have incorrect slots and superclass information."
              symbol)))
  (unless (eq find-structure-class symbol)
    (let ((find-structure-class symbol))
       (ensure-class-using-class
           symbol
           NIL
	   :metaclass 'structure-class
	   :name symbol
	   :direct-superclasses
	   (when (structure-type-included-type-name symbol)
	     (list (structure-type-included-type-name symbol)))
	   :direct-slots
             (make-defclass-direct-slots-from-defstruct symbol)))))

(eval-when (compile eval)

(defun make-built-in-class-subs ()
  (mapcar #'(lambda (e)
	      (let ((class (car e))
		    (class-subs ()))
		(dolist (s *built-in-classes*)
		  (when (memq class (cadr s)) (pushnew (car s) class-subs)))
		(cons class class-subs)))
	  (cons '(t) *built-in-classes*)))

(defun make-built-in-class-tree ()
  (let ((subs (make-built-in-class-subs)))
    (labels ((descend (class)
	       (cons class (mapcar #'descend (cdr (assq class subs))))))
      (descend 't))))

(defun make-built-in-wrapper-of-body ()
  (make-built-in-wrapper-of-body-1 (make-built-in-class-tree)
				   'x
				   #'get-built-in-wrapper-symbol))

(defun make-built-in-wrapper-of-body-1 (tree var get-symbol)
  (let ((*specials* ()))
    (declare (special *specials*))
    (let ((inner (make-built-in-wrapper-of-body-2 tree var get-symbol)))
      `(locally (declare (special .,*specials*)) ,inner))))

(defun make-built-in-wrapper-of-body-2 (tree var get-symbol)
  (declare (special *specials*))
  (let ((symbol (funcall get-symbol (car tree))))
    (push symbol *specials*)
    (let ((sub-tests
	    (mapcar #'(lambda (x)
			(make-built-in-wrapper-of-body-2 x var get-symbol))
		    (cdr tree))))
      `(and (typep ,var ',(car tree))
	    ,(if sub-tests
		 `(or ,.sub-tests ,symbol)
		 symbol)))))
)

(defun built-in-wrapper-of (x)
  #.(when (fboundp 'make-built-in-wrapper-of-body) ; so we can at least read this file
      (make-built-in-wrapper-of-body)))




(eval-when (load eval)
  (clrhash *find-class*)
  (bootstrap-meta-braid)
  (bootstrap-built-in-classes)
  (early-initialize-slot-gfs)
  (early-initialize-class-predicates)
  (setq *boot-state* 'braid)
  (setf (symbol-function 'load-defclass) #'real-load-defclass)
  )

(deftype slot-object ()
  '(or standard-object structure-object))


;;;
;;; All of these method definitions must appear here because the bootstrap
;;; only allows one method per generic function until the braid is fully
;;; built.
;;;
(defmethod print-object (instance stream)
  (printing-random-thing (instance stream)
    (let ((name (class-name (class-of instance))))
      (if name
	  (format stream "~S" name)
	  (format stream "Instance")))))

(defmethod print-object ((class class) stream)
  (named-object-print-function class stream))

(defmethod print-object ((slotd slot-definition) stream)
  (named-object-print-function slotd stream))

(defun named-object-print-function (instance stream
				    &optional (extra nil extra-p))
  (printing-random-thing (instance stream)
    (if extra-p					
	(format stream "~A ~S ~:S"
		(capitalize-words (class-name (class-of instance)))
		(slot-value-or-default instance 'name)
		extra)
	(format stream "~A ~S"
		(capitalize-words (class-name (class-of instance)))
		(slot-value-or-default instance 'name)))))



;;;
;;;
;;;
(defmethod shared-initialize :after ((slotd standard-slot-definition) slot-names &key)
  (declare (ignore slot-names))
  (if (eq (slot-value slotd 'allocation) :class)
      (setf (slot-value slotd 'allocation)
            (slot-value slotd 'class))))

(defmethod shared-initialize :after ((slotd structure-slot-definition) slot-names 
				     &key (allocation :instance))
  (declare (ignore slot-names))
  (unless (eq allocation :instance)
    (error "structure slots must have :instance allocation")))


(defmethod inform-type-system-about-class ((class structure-class) name)
  (inform-type-system-about-std-class name))

