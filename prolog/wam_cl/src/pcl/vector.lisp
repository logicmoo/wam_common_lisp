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
;;; Permutation vectors.
;;;

(in-package 'pcl)

(defmacro instance-slot-index-from-slots-layout (slots-layout slot-name)
  `(locally (declare #.*optimize-speed*)
     (let ((slots-left ,slots-layout))
       (if slots-left
           (block nil
             (let ((index 0))
               (declare (type index index))
               (tagbody
                 begin-loop
                  (if (eq (car slots-left) ,slot-name)
                      (go return-index))
                  (setf index (the index (1+ index)))
                  (if (null (setf slots-left (cdr slots-left)))
                      (return NIL))
                  (go begin-loop)
                 return-index)
               index))))))

(defmacro instance-slot-index (wrapper slot-name)
  `(instance-slot-index-from-slots-layout
      (wrapper-instance-slots-layout ,wrapper) ,slot-name))



;;;
;;;
;;;
(defun optimize-slot-value-by-class-p (class slot-name type)
  (let ((slotd (find-slot-definition class slot-name)))
    (and slotd 
	 (or (not (eq *boot-state* 'complete))
	     (slot-accessor-std-p slotd type)))))

(defun optimize-generic-function-call (form required-parameters env)
  (declare (ignore env required-parameters))
  form
  #||
  (let* ((gf-name (car form))
	 (gf (gdefinition gf-name))
	 (arg-info (gf-arg-info gf))
	 (metatypes (arg-info-metatypes arg-info))
	 (nreq (length metatypes))
	 (applyp (arg-info-applyp arg-info)))
    (declare (type index nreq))
    (declare (ignore applyp))
    (if (or (zerop nreq)
	    (not (<= nreq (length (cdr form))))
	    (not (every #'(lambda (arg mt)
			    (declare (ignore mt))
			    (when (consp arg)
                              (setq arg (un-the arg)))
			    (and (symbolp arg)
				 (memq arg required-parameters))
			    (let ((class-name (caddr (variable-declaration 
						      'class arg env))))
			      (and class-name (not (eq 't class-name)))))
			(cdr form) metatypes)))
	form
	form))||#) ;`(maybe-fast-gf-call ,(car form) ,(cdr form))


;; For calls to a gf:
; gf-call-info: (gf call-info-vector . gf-function-vector)
; call-info-vector:     #(call-info1 ... call-infon)
; gf-function-vector:   #(function1 ... functionn)
; --> once an entry is made in call-info-vector, it is never moved or removed
; call-info:            (gf . arg-types)
; arg-type:             a type. `(arg ,n) is not allowed here.

;; For calls from a method:
; method-gf-call-info:  (method-specializers method-call-info-vector . ???)
; arg-type:             a type or `(arg ,n)
; when arg-type is (arg n) the real type is either:
;   the arg's specializer or
;   (wrapper-eq ,wrapper) for a call appearing within a caching dfun gf

; every optimized gf in a method has an entry in the method's method-call-info-vector
; a macro: (get-call-cell mciv-index .all-wrappers.) ->
;          index into the gf-function-vector

;(defmacro maybe-fast-gf-call (gf-name args)
;   nil)


(defun can-optimize-access (form required-parameters env)
  (let ((type (ecase (car form)
		(slot-value 'reader)
		(set-slot-value 'writer)
		(slot-boundp 'boundp)))
	(var (un-the (cadr form)))
	(slot-name (eval (caddr form)))) ; known to be constant
    (when (symbolp var)
      (let* ((rebound? (caddr (variable-declaration 'variable-rebinding var env)))
	     (parameter-or-nil (car (memq (or rebound? var) required-parameters))))
	(when parameter-or-nil
	  (let* ((class-name (caddr (variable-declaration 
				     'class parameter-or-nil env)))
		 (class (find-class class-name nil)))
	    (when (if (and class
			   (class-on-class-precedence-list-p
                             *the-class-structure-object* class))
		      (optimize-slot-value-by-class-p class slot-name type)
		      (and class-name (not (eq class-name 't))))
	      (cons parameter-or-nil (or class class-name)))))))))

(defun optimize-slot-value (generic-function method slots sparameter form)
  (if sparameter
      (destructuring-bind (ignore ignore slot-name-form) form
	(let ((slot-name (eval slot-name-form))
              (class (if (consp sparameter) (cdr sparameter) *the-class-t*))
	      (parameter (if (consp sparameter) (car sparameter) sparameter)))
          (if (eq *boot-state* 'complete)
	      (optimize-instance-access generic-function method class parameter
                                        slots :read slot-name nil)
              (optimize-std-instance-access class parameter
                                            slots :read slot-name nil))))
      `(fast-slot-value ,@(cdr form))))

(defun optimize-set-slot-value (generic-function method slots sparameter form)
  (if sparameter
      (destructuring-bind (ignore ignore slot-name-form new-value) form
	(let ((slot-name (eval slot-name-form))
              (class (if (consp sparameter) (cdr sparameter) *the-class-t*))
	      (parameter (if (consp sparameter) (car sparameter) sparameter)))
          (if (eq *boot-state* 'complete)
	      (optimize-instance-access generic-function method class parameter
	                                slots :write slot-name new-value)
	      (optimize-std-instance-access class parameter
	                                    slots :write slot-name new-value))))
      `(fast-set-slot-value ,@(cdr form))))

(defun optimize-slot-boundp (generic-function method slots sparameter form)
  (if sparameter
      (destructuring-bind (ignore ignore slot-name-form new-value) form
	(let ((slot-name (eval slot-name-form))
              (class (if (consp sparameter) (cdr sparameter) *the-class-t*))
	      (parameter (if (consp sparameter) (car sparameter) sparameter)))
          (if (eq *boot-state* 'complete)
	      (optimize-instance-access generic-function method class parameter
	                                slots :boundp slot-name new-value)
	      (optimize-std-instance-access class parameter
	                                    slots :boundp slot-name new-value))))
      `(fast-slot-boundp ,@(cdr form))))

;;;
;;; The <slots> argument is an alist, the CAR of each entry is the name of
;;; a required parameter to the function.  The alist is in order, so the
;;; position of an entry in the alist corresponds to the argument's position
;;; in the lambda list.
;;; 

(defun optimize-std-instance-access (class parameter slots read/write
                                     slot-name new-value)
  (let* ((parameter-entry (assq parameter slots))
         (class-name      (if (symbolp class) class (class-name class)))
         (slot-entry      (assq slot-name (cdr parameter-entry)))
         (index-name
           (or (second slot-entry)
               (intern
                (string-append "." (symbol-name parameter) "-"
                               (symbol-name slot-name) "-INDEX.")))))
    (unless parameter-entry
      (error "Internal error in slot optimization."))
    (unless (or slot-entry
                (skip-fast-slot-access-p
                  class-name slot-name
                  (ecase read/write
                    (:read 'reader) (:write 'writer) (:boundp 'boundp))))
      (setq slot-entry (list slot-name index-name))
      (push slot-entry (cdr parameter-entry)))
    (ecase read/write
      (:read
       (if slot-entry
           `(instance-read   ,parameter ',slot-name ,index-name ,class-name)
           `(fast-slot-value ,parameter ',slot-name)))
      (:write
       (if slot-entry
           `(instance-write ,parameter ',slot-name ,index-name ,class-name
                            ,new-value)
           `(fast-set-slot-value ,parameter ',slot-name ,new-value)))
      (:boundp
       (if slot-entry
           `(instance-boundp  ,parameter ',slot-name ,index-name ,class-name)
           `(fast-slot-boundp ,parameter ',slot-name))))))


;; It is safe for these two functions to be wrong.
;; They just try to guess what the most likely case will be.
(defun generate-fast-class-slot-access-p (class-form slot-name-form)
  (let ((class (and (constantp class-form) (eval class-form)))
	(slot-name (and (constantp slot-name-form) (eval slot-name-form))))
    (and (eq *boot-state* 'complete)
	 (standard-class-p class)
	 (not (eq class *the-class-t*)) ; shouldn't happen, though.
	 (let ((slotd (find-slot-definition class slot-name)))
           (and slotd (consp (slot-definition-location slotd)))))))

(defun skip-fast-slot-access-p (class-name slot-name-form type)
  (let ((class (find-class class-name nil))
	(slot-name
          (cond ((symbolp slot-name-form) slot-name-form)
                ((constantp slot-name-form) (eval slot-name-form)))))
    (and (eq *boot-state* 'complete)
	 (standard-class-p class)
	 (not (eq class *the-class-t*)) ; shouldn't happen, though.
         (progn
           (unless (class-finalized-p class) (finalize-inheritance class))
	   (let ((slotd (find-slot-definition class slot-name)))
	     (and slotd (skip-optimize-slot-value-by-class-p class slot-name type)))))))

(defun skip-optimize-slot-value-by-class-p (class slot-name type)
  (let ((slotd (find-slot-definition class slot-name)))
    (and slotd
	 (eq *boot-state* 'complete)
	 (not (the boolean (slot-accessor-std-p slotd type))))))

(defmacro instance-read (parameter slot-name index class)
  (if (skip-fast-slot-access-p class slot-name 'reader)
      `(fast-slot-value ,parameter ,slot-name)
      `(optimized-parameter-read ,parameter ,slot-name ,index)))

(defmacro instance-write (parameter slot-name index class new-value)
  (if (skip-fast-slot-access-p class slot-name 'writer)
      `(fast-set-slot-value ,parameter ,slot-name ,new-value)
      `(optimized-parameter-write ,parameter ,slot-name ,index ,new-value)))

(defmacro instance-boundp (parameter slot-name index class)
  (if (skip-fast-slot-access-p class slot-name 'boundp)
      `(fast-slot-boundp ,parameter ,slot-name)
      `(optimized-parameter-boundp ,parameter ,slot-name ,index)))


;;; closure-generators are used only by method-function-for-caching
;;; (in methods.lisp) and make-not-for-caching-method-function.

(defun make-not-for-caching-method-function (closure-generator)
  (let ((function (method-function-funcall closure-generator nil)))
    #+(and kcl turbo-closure) (si:turbo-closure function)
    function))

(declaim (ftype (function (T) boolean) all-standard-accesses-p))
(defun all-standard-accesses-p (slot-locs-and-fetchers-and-method)
  (let ((slot-locations (first  slot-locs-and-fetchers-and-method))
        (slot-fetchers  (second slot-locs-and-fetchers-and-method)))
    (and slot-locations
         slot-fetchers
         (every #'integerp slot-locations)
         (every #'(lambda (x) (eq x 'std-instance-slots)) slot-fetchers))))

(defun make-std-closure-generator-form
       (generic-function method optimized-method-lambda initargs)
  (declare (type list initargs))
  (let ((slot-indices
          (getf initargs :optimized-slot-indices))
        (runtime-compile-p
          (and (eq *boot-state* 'complete)
               (call-store-optimized-method-lambda-p
                 generic-function method initargs))))
   (declare (type boolean runtime-compile-p))
   `#'(lambda (slot-locs-and-fetchers-and-method)
        (let (,@(mapcar
                  #'(lambda (slot-index)
                      `(,(third slot-index)
                        (nth ,(posq slot-index slot-indices)
                             (the list
                                  (car slot-locs-and-fetchers-and-method)))))
                  slot-indices))
          (if (all-standard-accesses-p slot-locs-and-fetchers-and-method)
              (macrolet
                ((optimized-parameter-read (x slot-name index)
                   `(locally (declare #.*optimize-speed*)
                      (let ((.value. (%svref (std-instance-slots ,x) ,index)))
                        (if (eq .value. *slot-unbound*)
                            (funcall #'slot-value ,x ,slot-name)
                            .value.))))
                 (optimized-parameter-write (x slot-name index new)
                   (declare (ignore slot-name))
                   `(locally (declare #.*optimize-speed*)
                      (setf (%svref (std-instance-slots ,x) ,index) ,new)))
                 (optimized-parameter-boundp (x slot-name index)
                   (declare (ignore slot-name))
                   `(locally (declare #.*optimize-speed*)
                      (neq (%svref (std-instance-slots ,x) ,index)
                           *slot-unbound*))))
               (function ,optimized-method-lambda))
             ,(if runtime-compile-p
                  `(make-cached-method-function-from-stored-lambda
                      (third slot-locs-and-fetchers-and-method)
                      slot-locs-and-fetchers-and-method)
                `(macrolet
                   ((optimized-parameter-read (x slot-name index)
                     `(locally (declare #.*optimize-speed*)
                        (let ((.value.
                                (typecase ,index
                                  (fixnum (%svref (get-slots ,x) ,index))
                                  (cons   (cdr ,index))
                                  (T      *slot-unbound*))))
                          (if (eq .value. *slot-unbound*)
                              (funcall #'slot-value ,x ,slot-name)
                              .value.))))
                    (optimized-parameter-write (x slot-name index new)
                      (once-only (new)
                       `(locally (declare #.*optimize-speed*)
                          (typecase ,index
                            (fixnum (setf (%svref (get-slots ,x) ,index) ,new))
                            (cons   (setf (cdr ,index) ,new))
                            (T (funcall #'set-slot-value ,x ,slot-name
                                        ,new))))))
                    (optimized-parameter-boundp (x slot-name index)
                      `(locally (declare #.*optimize-speed*)
                         (typecase ,index
                           (fixnum (neq (%svref (get-slots ,x) ,index)
                                        *slot-unbound*))
                           (cons   (neq (cdr ,index) *slot-unbound*))
                           (T      (funcall #'slot-boundp ,x ,slot-name))))))
                  (function ,optimized-method-lambda))))))))


(defmethod wrapper-fetcher ((class standard-class))
  'std-instance-wrapper)

(defmethod slots-fetcher ((class standard-class))
  'std-instance-slots)

(defmethod raw-instance-allocator ((class standard-class))
  '%%allocate-instance--class)


(defun instance-type (instance)
  "Returns the underlying instance type of INSTANCE."
  (cond ((std-instance-p instance)  'std-instance)
        ((fsc-instance-p instance)  'fsc-instance)
        #+pcl-user-instances
        ((user-instance-p instance) 'user-instance)
        ((structurep      instance) 'structure-instance)
        (T (error "~S is not a standard kind of instance." instance))))

(defun class-instance-type (class)
  "Returns the underlying instance type CLASS instances."
  (if (eq class *the-class-standard-generic-function*)
      'fsc-instance
      (if (class-finalized-p class)
          (instance-type (class-prototype class))
          (instance-type (allocate-instance class)))))

;;;
;;;   These are the basic functions to allow programmers to define their
;;; own type of instances other than STD-INSTANCE, FSC-INSTANCE,
;;; STRUCTURE-INSTANCE, or BUILT-IN-INSTANCE.  It can be used for user-defined
;;; meta-classes whose instances' underlying implementation is different
;;; than the default PCL implementation of STD-INSTANCE for STANDARD-CLASS.
;;; For example, the programmer can define their own user instances to be
;;; represented as a simple-vector whose first element stores the information
;;; needed to control the instances (e.g. a user wrapper storing the normal
;;; PCL wrapper, the list of slot names for the class, and so on), and whose
;;; remaining elements are the slots of the instance itself.  This kind of
;;; defined user instance would take less space than the normal pcl
;;; STD-INSTANCE (which is represented as a structure, one of whose elements
;;; is another vector holding the slots), at the cost of some redefinition
;;; flexibility.  See user-instances.lisp for an example of how this can be done.
;;;   Defining USER-INSTANCES requires knowledge of how PCL's
;;; wrappers and slots work internally, but the basic functions that need
;;; to be implemented are USER-INSTANCE-P, USER-INSTANCE-WRAPPER,
;;; SET-USER-INSTANCE-WRAPPER, USER-INSTANCE-SLOTS, and
;;; SET-USER-INSTANCE-SLOTS.  These are analogous to STD-INSTANCE-P,
;;; STD-INSTANCE-WRAPPER, etc., and must return the same thing that they
;;; would.
;;;   Contact Trent Lange (lange@cs.ucla.edu) for any questions.
;;; The feature :PCL-USER-INSTANCES must be pushed onto the lisp *FEATURES*
;;; list in low.lisp to turn this feature on.
;;;

#+pcl-user-instances
(progn
(proclaim '(notinline user-instance-p
                      user-instance-wrapper
                      user-instance-slots
                      set-user-instance-wrapper
                      set-user-instance-slots))

(defun user-instance-p (x)
  (declare (ignore x))
  NIL)

(defun user-instance-wrapper (x)
  (declare (ignore x))
  (error "No user-instance-wrapper function defined for user instances."))

(defun set-user-instance-wrapper (x nv)
  (declare (ignore x nv))
  (error "No set-user-instance-wrapper function defined for user instances."))

(defun user-instance-slots (x)
  (declare (ignore x))
  (error "No user-instance-slots function defined for user instances."))

(defun set-user-instance-slots (x nv)
  (declare (ignore x nv))
  (error "No set-user-instance-slots function defined for user instances."))

(defsetf user-instance-wrapper set-user-instance-wrapper)
(defsetf user-instance-slots   set-user-instance-slots)

;;; The following three macros are used by PCL as hooks to call the three
;;; user-instance functions.  They can be redefined for particular
;;; instantiations of the user-instance instances to return the same
;;; values the functions would, but to work faster by avoiding the
;;; function call.

(defmacro get-user-instance-p (x)
  `(user-instance-p ,x))

(defmacro get-user-instance-wrapper (x)
  `(user-instance-wrapper ,x))

(defmacro get-user-instance-slots (x)
  `(user-instance-slots ,x))

(defmacro fast-check-user-wrapper-validity (object)
  `(fast-check-wrapper-validity ,object get-user-instance-wrapper))

) ;+pcl-user-instances

