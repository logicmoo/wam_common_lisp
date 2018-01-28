;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
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
;;;
;;; This file defines the initialization and related protocols.
;;; 

(in-package 'pcl)

(declaim (type boolean *check-initargs-p*))
(defvar *check-initargs-p* nil)

(defmacro checking-initargs (&body forms)
  `(when *check-initargs-p* ,@forms))

(declaim (type boolean *making-instance-p*))
(defvar *making-instance-p* nil)

(defmethod make-instance ((class slot-class) &rest initargs)
  (declare (list initargs))
  (unless (class-finalized-p class) (finalize-inheritance class))
  (let ((class-default-initargs (class-default-initargs class)))
    (when class-default-initargs
      (setf initargs (default-initargs class initargs class-default-initargs)))
    (when initargs
      (when (and (eq *boot-state* 'complete)
	         (not (getf initargs :allow-other-keys)))
        (let ((class-proto (class-prototype class)))
          (check-initargs-1
  	    class initargs
	    (append (compute-applicable-methods
		      #'allocate-instance (list* class initargs))
		    (compute-applicable-methods 
		      #'initialize-instance (list* class-proto initargs))
		    (compute-applicable-methods 
		      #'shared-initialize (list* class-proto t initargs)))))))
    (let* ((*making-instance-p* T)
           (instance (apply #'allocate-instance class initargs)))
      (apply #'initialize-instance instance initargs)
      instance)))

(defmethod make-instance ((class-name symbol) &rest initargs)
  (apply #'make-instance (find-class class-name) initargs))

(defvar *default-initargs-flag* (list nil))

(defmethod default-initargs ((class slot-class) supplied-initargs all-default)
  ;; This implementation of default initargs is critically dependent
  ;; on all-default-initargs not having any duplicate initargs in it.
  (let ((miss *default-initargs-flag*))
    (flet ((getf* (plist key)
	     (do ()
		 ((null plist) miss)
	       (if (eq (car plist) key)
		   (return (cadr plist))
		   (setq plist (cddr plist))))))
      (labels ((default-1 (tail)
		 (if (null tail)
		     nil
		     (if (eq (getf* supplied-initargs (caar tail)) miss)
			 (list* (caar tail)
				(funcall-function (cadar tail))
				(default-1 (cdr tail)))
			 (default-1 (cdr tail))))))
	(append supplied-initargs (default-1 all-default))))))


(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (unless (or *making-instance-p* (class-finalized-p class))
    (finalize-inheritance class))
  (let* ((class-wrapper (class-wrapper class))
         (instance (%allocate-instance--class
                     (wrapper-allocate-static-slot-storage-copy
                       class-wrapper))))
    (setf (std-instance-wrapper instance) class-wrapper)
    instance))

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (let ((constructor (class-defstruct-constructor class)))
    (if constructor
        (funcall-function (symbol-function constructor))
        (error "Can't allocate an instance of class ~S" (class-name class)))))

(defmethod initialize-instance ((instance slot-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))


(defmethod reinitialize-instance ((instance slot-object) &rest initargs)
  (checking-initargs
    (when (and initargs (eq *boot-state* 'complete)
	       (not (getf initargs :allow-other-keys)))
      (check-initargs-1
       (class-of instance) initargs
       (append (compute-applicable-methods 
		#'reinitialize-instance (list* instance initargs))
	       (compute-applicable-methods 
		#'shared-initialize (list* instance nil initargs))))))
  (apply #'shared-initialize instance nil initargs)
  instance)


(defmethod update-instance-for-different-class ((previous standard-object)
						(current standard-object)
						&rest initargs)
  ;; First we must compute the newly added slots.  The spec defines
  ;; newly added slots as "those local slots for which no slot of
  ;; the same name exists in the previous class."
  (let ((added-slots '())
	(current-slotds (class-slots (class-of current)))
	(previous-slot-names (mapcar #'slot-definition-name
				     (class-slots (class-of previous)))))
    (dolist (slotd current-slotds)
      (if (and (not (memq (slot-definition-name slotd) previous-slot-names))
	       (eq (slot-definition-allocation slotd) ':instance))
	  (push (slot-definition-name slotd) added-slots)))
    (checking-initargs
      (when (and initargs (not (getf initargs :allow-other-keys)))
	(check-initargs-1
	 (class-of current) initargs
	 (append (compute-applicable-methods 
		  #'update-instance-for-different-class 
		  (list* previous current initargs))
		 (compute-applicable-methods 
		  #'shared-initialize (list* current added-slots initargs))))))
    (apply #'shared-initialize current added-slots initargs)))

(defmethod update-instance-for-redefined-class ((instance standard-object)
						added-slots
						discarded-slots
						property-list
						&rest initargs)
  (checking-initargs
    (when (and initargs (not (getf initargs :allow-other-keys)))
      (check-initargs-1
       (class-of instance) initargs
       (append (compute-applicable-methods 
		#'update-instance-for-redefined-class 
		(list* instance added-slots discarded-slots property-list initargs))
	       (compute-applicable-methods 
		#'shared-initialize (list instance added-slots initargs))))))
  (apply #'shared-initialize instance added-slots initargs))

(defmethod shared-initialize
	   ((instance slot-object) slot-names &rest initargs)
  (declare (list initargs))
  (declare #.*optimize-speed*)
  ;;
  ;; initialize the instance's slots in a two step process
  ;;   1. A slot for which one of the initargs in initargs can set
  ;;      the slot, should be set by that initarg.  If more than
  ;;      one initarg in initargs can set the slot, the leftmost
  ;;      one should set it.
  ;;
  ;;   2. Any slot not set by step 1, may be set from its initform
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
  (flet
   ((init-safe (slots initing-internal-slotds)
      (dolist (internal-slotd initing-internal-slotds)
       (unless
         (and
           initargs
           ;; Try to initialize the slot from one of the initargs.
           (let ((slot-initargs (internal-slotd-initargs internal-slotd))
                 (initargs-ptr  initargs))
             (loop
               (when (memq (car initargs-ptr) slot-initargs)
                 (let ((location (internal-slotd-location internal-slotd)))
                   (typecase location
                     (fixnum (setf (%svref slots location)
                                   (cadr initargs-ptr)))
                     (cons   (setf (cdr location) (cadr initargs-ptr)))
                     (T (method-function-funcall
                          (internal-slotd-writer-function internal-slotd)
                          (cadr initargs-ptr) instance))))
                   (return 't))
                 (when (null (setf initargs-ptr (cddr initargs-ptr)))
                   (return)))))
           ;; Try to initialize the slot from its initform.
           (when (or (eq slot-names 't)
                     (memq (internal-slotd-name internal-slotd) slot-names))
             (let ((location (internal-slotd-location internal-slotd)))
               (typecase location
                 (fixnum
                   (when (eq (%svref slots location) *slot-unbound*)
                     (let ((initfn (internal-slotd-initfunction
                                      internal-slotd)))
                       (when initfn
                         (setf (%svref slots location)
                               (slot-initfunction-funcall initfn))))))
                 (cons
                   (when (eq (cdr location) *slot-unbound*)
                     (let ((initfn (internal-slotd-initfunction
                                     internal-slotd)))
                       (when initfn
                         (setf (cdr location)
                                    (slot-initfunction-funcall initfn))))))
                 (T
                   (unless (method-function-funcall
                             (internal-slotd-boundp-function internal-slotd)
                             instance)
                     (let ((initfn (internal-slotd-initfunction
                                     internal-slotd)))
                       (when initfn
                         (method-function-funcall
                           (internal-slotd-writer-function internal-slotd)
                           (slot-initfunction-funcall initfn) instance)))))))))))

    (init-unsafe (initing-internal-slotds)
      (dolist (internal-slotd initing-internal-slotds)
       (unless
         (and
           initargs
           ;; Try to initialize the slot from one of the initargs.
           (let ((slot-initargs (internal-slotd-initargs internal-slotd))
                 (initargs-ptr  initargs))
             (loop
               (when (memq (car initargs-ptr) slot-initargs)
                 (method-function-funcall
                   (internal-slotd-writer-function internal-slotd)
                   (cadr initargs-ptr) instance)
                 (return 't))
               (when (null (setf initargs-ptr (cddr initargs-ptr)))
                 (return)))))
           ;; Try to initialize the slot from its initform.
           (when (or (eq slot-names 't)
                     (memq (internal-slotd-name internal-slotd) slot-names))
             (unless (method-function-funcall
                       (internal-slotd-boundp-function internal-slotd)
                       instance)
               (let ((initfn (internal-slotd-initfunction internal-slotd)))
                 (when initfn
                   (method-function-funcall
                     (internal-slotd-writer-function internal-slotd)
                     (slot-initfunction-funcall initfn) instance)))))))))
    
    (when (or slot-names initargs)
      (let ((initing-internal-slotds
              (fast-slot-value (class-of instance)
                               (if (and *making-instance-p* (null initargs))
                                   'side-effect-internal-slotds
                                   'internal-slotds))))
      (if *safe-to-use-slot-wrapper-optimizations-p*
          (cond
            ((std-instance-p instance)
             (fast-check-wrapper-validity instance std-instance-wrapper)
             (init-safe (std-instance-slots instance) initing-internal-slotds))
            ((fsc-instance-p instance)
             (fast-check-wrapper-validity instance fsc-instance-wrapper)
             (init-safe (fsc-instance-slots instance) initing-internal-slotds))
            #+pcl-user-instances
            ((user-instance-p instance)
             (fast-check-wrapper-validity instance user-instance-wrapper)
             (init-safe (user-instance-slots instance) initing-internal-slotds))
            (T (init-unsafe initing-internal-slotds)))
          (init-unsafe initing-internal-slotds))))
    instance))



;;; 
;;; if initargs are valid return nil, otherwise signal an error
;;;

(declaim (ftype (function (T) (values list boolean)) function-keywords))

(defun check-initargs-1 (class initargs methods)
  (let ((legal (apply #'append (mapcar #'internal-slotd-initargs
				       (class-internal-slotds class)))))
    (unless nil ; (getf initargs :allow-other-keys) ; This is already checked.
      ;; Add to the set of slot-filling initargs the set of
      ;; initargs that are accepted by the methods.  If at
      ;; any point we come across &allow-other-keys, we can
      ;; just quit.
      (dolist (method methods)
	(multiple-value-bind (keys allow-other-keys)
	    (function-keywords method)
          (declare (type boolean allow-other-keys))
	  (when allow-other-keys
	    (return-from check-initargs-1 nil))
	  (setq legal (append keys legal))))
      ;; Now check the supplied-initarg-names and the default initargs
      ;; against the total set that we know are legal.
      (doplist (key val) initargs
	(unless (memq key legal)
	  (error "Invalid initialization argument ~S for class ~S"
		 key
		 (class-name class)))))))

