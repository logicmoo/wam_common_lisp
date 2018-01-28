;;;-*-Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp -*-
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

;;;
;;; These four functions work on std-instances and fsc-instances.  These are
;;; instances for which it is possible to change the wrapper and the slots.
;;;
;;; For these kinds of instances, most specified methods from the instance
;;; structure protocol are promoted to the implementation-specific class
;;; std-class.  Many of these methods call these four functions.
;;;

(defun wrapper-error (&optional (object () object-p))
  (if object-p
      (error "What kind of instance is this: ~S" object)
    (error "What kind of instance is this?")))

(defmacro get-slots (inst &optional (else-clause '(wrapper-error)))
  (once-only (inst)
    `(cond ((std-instance-p ,inst)      (std-instance-slots ,inst))
           ((fsc-instance-p ,inst)      (fsc-instance-slots ,inst))
           #+pcl-user-instances
           ((get-user-instance-p ,inst) (get-user-instance-slots ,inst))
           (T ,else-clause))))

(defmacro get-slots-or-nil (inst)
  `(get-slots ,inst nil))

(defun set-wrapper (inst new)
  (cond ((std-instance-p inst)
         (setf (std-instance-wrapper inst) new))
        ((fsc-instance-p inst)
         (setf (fsc-instance-wrapper inst) new))
        #+pcl-user-instances
        ((user-instance-p inst)
         (setf (user-instance-wrapper inst) new))
        (t
         (wrapper-error inst))))

(defun set-slots (inst new)
  (cond ((std-instance-p inst)
         (setf (std-instance-slots inst) new))
        ((fsc-instance-p inst)
         (setf (fsc-instance-slots inst) new))
        #+pcl-user-instances
        ((user-instance-p inst)
         (setf (user-instance-slots inst) new))
        (t
         (wrapper-error inst))))



(defmacro get-class-slot-value-1 (object wrapper slot-name
                                  &optional
                                  (ignore-unbound-p NIL)
                                  (ignore-missing-p NIL)
                                  (return-cons-p    NIL)
                                  (operation        ''slot-value))
  ;; Search for slot-name in the class-slots of the object, returning
  ;; its value if found and the slot is bound, calling slot-unbound if
  ;; it was unbound (unless ignore-unbound is T), and calling slot-missing
  ;; if it wasn't found in the class slots at all.
  (once-only (object slot-name)
    `(block nil
       (locally (declare #.*optimize-speed*)
         (let ((slots-left (wrapper-class-slots ,wrapper)))
           (tagbody
               (if (null slots-left)
                   (go loop-slot-missing))
             begin-class-loop
               (if (eq (caar slots-left) ,slot-name)
                   ,(cond
                      (return-cons-p
                        `(progn
                           (setf slots-left (car slots-left))
                           (go return-slot-value)))
                      (ignore-unbound-p
                        `(progn
                           (setf slots-left (cdar slots-left))
                           (go return-slot-value)))
                      (T
                       `(if (eq (setf slots-left (cdar slots-left))
                                ',*slot-unbound*)
                            (return
                              (slot-unbound (wrapper-class ,wrapper)
                                            ,object ,slot-name))
                          (go return-slot-value)))))
               (if (null (setf slots-left (cdr slots-left)))
                   (go loop-slot-missing))
               (go begin-class-loop)
             loop-slot-missing
               ,@(unless ignore-missing-p
                  `((return (slot-missing (wrapper-class ,wrapper)
                                          ,object
                                          ,slot-name
                                          ,operation))))
             return-slot-value)
           slots-left)))))

(defmacro set-class-slot-value-1 (object wrapper slot-name new-value)
  ;; Search for slot-name in the class-slots of the object, setting it
  ;; to new-value if found, calling slot-missing otherwise.
  (once-only (object slot-name new-value)
    `(block nil
       (locally (declare #.*optimize-speed*)
         (let ((slots-left (wrapper-class-slots ,wrapper)))
           (tagbody
               (if (null slots-left)
                   (go loop-slot-missing))
             begin-class-loop
               (if (eq (caar slots-left) ,slot-name)
                   (return (setf (cdar slots-left) ,new-value)))
               (if (null (setf slots-left (cdr slots-left)))
                   (go loop-slot-missing))
               (go begin-class-loop)
             loop-slot-missing
               (return (slot-missing (wrapper-class ,wrapper)
                                     ,object
                                     ,slot-name
                                     'setf
                                     ,new-value))))))))

(defmacro slot-value-from-wrapper-and-slots
          (object slot-name wrapper slots-layout slots get-slots-fn)
  "Extra fast and ugly way to return the value of object's slot when given
   its wrapper and possibly slots-layout and slots vector."
  (once-only (wrapper)
   `(block nil
     (locally (declare #.*optimize-speed*)
       (let ((slots-left ,(if slots-layout
                              slots-layout
                              `(wrapper-instance-slots-layout ,wrapper))))
         (tagbody
             (if slots-left
                 (let ((index 0))
                   (declare (type index index))
                   (tagbody
                     begin-local-loop
                       (if (eq (car slots-left) ,slot-name)
                           (if (eq (setf slots-left
                                         (%svref
                                           ,(if slots
                                                slots
                                              `(,(or get-slots-fn 'get-slots)
                                                ,object))
                                           index))
                                   ',*slot-unbound*)
                                 (go loop-slot-unbound)
                             (go return-slot-value)))
                       (setf index (the index (1+ index)))
                       (if (null (setf slots-left (cdr slots-left)))
                           (go check-class-loop))
                       (go begin-local-loop))))
           check-class-loop
             (if (null (setf slots-left (wrapper-class-slots ,wrapper)))
                 (go loop-slot-missing))
           begin-class-loop
             (if (eq (caar slots-left) ,slot-name)
                 (if (eq (setf slots-left (cdar slots-left))
                         ',*slot-unbound*)
                     (go loop-slot-unbound)
                     (go return-slot-value)))
             (if (null (setf slots-left (cdr slots-left)))
                 (go loop-slot-missing))
             (go begin-class-loop)
           loop-slot-missing
             (return (slot-missing (wrapper-class ,wrapper)
                                   ,object
                                   ,slot-name
                                   'slot-value))
           loop-slot-unbound
             (return (slot-unbound (wrapper-class ,wrapper) ,object ,slot-name))
           return-slot-value)
         slots-left)))))

(defmacro set-slot-value-from-wrapper-and-slots
  (object slot-name wrapper slots-layout slots get-slots-fn new-value)
  "Extra fast and ugly way to set the value of object's slot when given its
   wrapper and possibly slots-layout and slots vector."
  (once-only (wrapper new-value)
   `(block nil
     (locally (declare #.*optimize-speed*)
       (let ((slots-left ,(if slots-layout
                              slots-layout
                              `(wrapper-instance-slots-layout ,wrapper))))
         (tagbody
             (if slots-left
                 (let ((index 0))
                   (declare (type index index))
                   (tagbody
                     begin-local-loop
                       (if (eq (car slots-left) ,slot-name)
                           (progn
                             (setf (%svref ,(if slots
                                                slots
                                              `(,(or get-slots-fn 'get-slots)
                                                ,object))
                                           index)
                                   ,new-value)
                             (go return-slot-value)))
                       (setf index (the index (1+ index)))
                       (if (null (setf slots-left (cdr slots-left)))
                           (go check-class-loop))
                       (go begin-local-loop))))
           check-class-loop
             (if (null (setf slots-left (wrapper-class-slots ,wrapper)))
                 (go loop-slot-missing))
           begin-class-loop
             (if (eq (caar slots-left) ,slot-name)
                 (progn
                   (setf (cdar slots-left) ,new-value)
                   (go return-slot-value)))
             (if (null (setf slots-left (cdr slots-left)))
                 (go loop-slot-missing))
             (go begin-class-loop)
           loop-slot-missing
             (return (slot-missing (wrapper-class ,wrapper)
                                   ,object
                                   ,slot-name
                                   'setf
                                   ,new-value))
           return-slot-value)
         ,new-value)))))

(defmacro slot-boundp-from-wrapper-and-slots
          (object slot-name wrapper slots-layout slots get-slots-fn)
  "Extra fast and ugly way to return whether object's slot is boundp when
   given its wrapper and possibly slots-layout and slots vector."
  (once-only (wrapper)
   `(block nil
     (locally (declare #.*optimize-speed*)
       (let ((slots-left ,(if slots-layout
                              slots-layout
                              `(wrapper-instance-slots-layout ,wrapper))))
         (tagbody
             (if slots-left
                 (let ((index 0))
                   (declare (type index index))
                   (tagbody
                     begin-local-loop
                       (when (eq (car slots-left) ,slot-name)
                         (return
                           (neq (%svref ,(if slots
                                             slots
                                           `(,(or get-slots-fn 'get-slots)
                                             ,object))
                                        index)
                                ',*slot-unbound*)))
                       (setf index (the index (1+ index)))
                       (if (null (setf slots-left (cdr slots-left)))
                           (go check-class-loop))
                       (go begin-local-loop))))
           check-class-loop
             (if (null (setf slots-left (wrapper-class-slots ,wrapper)))
                 (go loop-slot-missing))
           begin-class-loop
             (when (eq (caar slots-left) ,slot-name)
               (return (neq (cdar slots-left) ',*slot-unbound*)))
             (if (null (setf slots-left (cdr slots-left)))
                 (go loop-slot-missing))
             (go begin-class-loop)
           loop-slot-missing
             (return (slot-missing (wrapper-class ,wrapper)
                                   ,object
                                   ,slot-name
                                   'slot-value))))))))

#|
;; We can't use DEFSETF because the macroexpansion doesn't evaluate its
;; GET-SLOTS-FN argument.  And worse -- the macroexpansion examines the values
;; of its unevaluated arguments (SLOTS-LAYOUT and SLOTS).

(defsetf slot-value-from-wrapper-and-slots
         set-slot-value-from-wrapper-and-slots)
|#

(define-setf-method slot-value-from-wrapper-and-slots
    (object slot-name wrapper slots-layout slots get-slots-fn)
  (let ((object-temp (gensym))       ;; Always needed.
	(slot-name-temp (gensym))    ;; Always needed.
	(wrapper-temp (gensym))      ;; Always needed.
	(slots-layout-temp (gensym)) ;; Not always needed.
	(slots-temp (gensym))        ;; Not always needed.
	(store (gensym)))
    (values ;; The temporary variables.
	    (list object-temp slot-name-temp wrapper-temp slots-layout-temp
		  slots-temp)
	    ;; The value forms.
	    (list object slot-name wrapper slots-layout slots)
	    ;; The store variables.
	    (list store)
	    ;; The storing form.
	    `(set-slot-value-from-wrapper-and-slots ,object-temp
						    ,slot-name-temp
						    ,wrapper-temp
						    ,(if slots-layout
							 slots-layout-temp
							 nil)
						    ,(if slots
							 slots-temp
							 nil)
						    ,get-slots-fn
						    ,store)
	    ;; The accessing form.
	    `(slot-value-from-wrapper-and-slots ,object-temp
						,slot-name-temp
						,wrapper-temp
						,(if slots-layout
						     slots-layout-temp
						     nil)
						,(if slots
						     slots-temp
						     nil)
						,get-slots-fn)
	    )))

(defmethod class-slot-value ((class std-class) slot-name)
  (let ((wrapper (class-wrapper class))
        (prototype (class-prototype class)))
    (get-class-slot-value-1 prototype wrapper slot-name)))

(defmethod (setf class-slot-value) (nv (class std-class) slot-name)
  (let ((wrapper (class-wrapper class))
        (prototype (class-prototype class)))
    (set-class-slot-value-1 nv prototype wrapper slot-name)))


;;;
;;; The following highly optimized methods for accessing the slots
;;;   of standard instances and funcallable standard instances aren't
;;;   in the MOP, but it would be nice if they were...  - TL
;;;

(defmacro safe-funcallable-standard-instance-slot-value (instance slot-name)
  "Highly-optimized macro for returning the slot-value of instances that
   are guaranteed to be funcallable-standard-instances with normal slots."
  (once-only (instance)
    `(locally (declare #.*optimize-speed*)
       (slot-value-from-wrapper-and-slots
          ,instance ,slot-name
          (fast-check-wrapper-validity ,instance fsc-instance-wrapper)
          NIL NIL fsc-instance-slots))))

(defmacro set-safe-funcallable-standard-instance-slot-value
          (instance slot-name new-value)
  "Highly-optimized macro for setting the slot-value of instances that
   are guaranteed to be funcallable-standard-instances with normal slots."
  (once-only (instance)
    `(locally (declare #.*optimize-speed*)
       (setf (slot-value-from-wrapper-and-slots
                ,instance ,slot-name
                (fast-check-wrapper-validity ,instance fsc-instance-wrapper)
                NIL NIL fsc-instance-slots)
             ,new-value))))

(defmacro safe-funcallable-standard-instance-slot-boundp (instance slot-name)
  "Highly-optimized macro for checking whether a slot is bound for instances
   that are guaranteed to be funcallable-standard-instances with normal slots."
  (once-only (instance slot-name)
    `(locally (declare #.*optimize-speed*)
       (slot-boundp-from-wrapper-and-slots
          ,instance ,slot-name
          (fast-check-wrapper-validity ,instance fsc-instance-wrapper)
          NIL NIL fsc-instance-slots))))

(defsetf safe-funcallable-standard-instance-slot-value
         set-safe-funcallable-standard-instance-slot-value)


(defmacro funcallable-standard-instance-slot-value (instance slot-name)
  "Highly-optimized macro for returning the slot-value of instances that
   are guaranteed to be funcallable-standard-instances."
  (cond (*always-safe-to-use-slot-wrapper-optimizations-p*
         `(safe-funcallable-standard-instance-slot-value ,instance ,slot-name))
        (*safe-to-use-slot-value-wrapper-optimizations-p*
         (once-only (instance slot-name)
          `(locally (declare #.*optimize-speed*)
             (if *safe-to-use-slot-value-wrapper-optimizations-p*
                 (safe-funcallable-standard-instance-slot-value ,instance ,slot-name)
                 (accessor-slot-value ,instance ,slot-name)))))
        (T `(accessor-slot-value ,instance ,slot-name))))

(defmacro set-funcallable-standard-instance-slot-value
          (instance slot-name new-value)
  "Highly-optimized macro for setting the slot-value of instances that
   are guaranteed to be funcallable-standard-instances."
  (cond (*always-safe-to-use-slot-wrapper-optimizations-p*
         `(setf (safe-funcallable-standard-instance-slot-value ,instance ,slot-name)
                ,new-value))
        (*safe-to-use-set-slot-value-wrapper-optimizations-p*
         (once-only (instance slot-name new-value)
          `(locally (declare #.*optimize-speed*)
             (if *safe-to-use-set-slot-value-wrapper-optimizations-p*
                 (setf (safe-funcallable-standard-instance-slot-value
                           ,instance ,slot-name)
                       ,new-value)
                 (setf (accessor-slot-value ,instance ,slot-name) ,new-value)))))
        (T `(setf (accessor-slot-value ,instance ,slot-name) ,new-value))))

(defmacro funcallable-standard-instance-slot-boundp (instance slot-name)
  "Highly-optimized macro for checking whether a slot is bound for instances
   that are guaranteed to be funcallable-standard-instances."
  (cond (*always-safe-to-use-slot-wrapper-optimizations-p*
         `(safe-funcallable-standard-instance-slot-boundp ,instance ,slot-name))
        (*safe-to-use-slot-boundp-wrapper-optimizations-p*
         (once-only (instance slot-name)
          `(locally (declare #.*optimize-speed*)
             (if *safe-to-use-slot-boundp-wrapper-optimizations-p*
                 (safe-funcallable-standard-instance-slot-boundp ,instance ,slot-name)
                 (accessor-slot-boundp ,instance ,slot-name)))))
        (T `(accessor-slot-boundp ,instance ,slot-name))))

(defsetf funcallable-standard-instance-slot-value
         set-funcallable-standard-instance-slot-value)


(defmacro safe-standard-instance-slot-value (instance slot-name)
  "Highly-optimized macro for returning the slot-value of instances that
   are guaranteed to be standard instances with normal slots."
  (once-only (instance)
    `(locally (declare #.*optimize-speed*)
       (slot-value-from-wrapper-and-slots
          ,instance ,slot-name
          (fast-check-wrapper-validity ,instance std-instance-wrapper)
          NIL NIL std-instance-slots))))

(defmacro set-safe-standard-instance-slot-value (instance slot-name new-value)
  "Highly-optimized macro for setting the slot-value of instances that
   are guaranteed to be standard instances with normal slots."
  (once-only (instance)
    `(locally (declare #.*optimize-speed*)
       (setf (slot-value-from-wrapper-and-slots
                ,instance ,slot-name
                (fast-check-wrapper-validity ,instance std-instance-wrapper)
                NIL NIL std-instance-slots)
            ,new-value))))

(defmacro safe-standard-instance-slot-boundp (instance slot-name)
  "Highly-optimized macro for checking whether a slot is bound for instances
   that are guaranteed to be standard-instances with normal slots."
  (once-only (instance slot-name)
    `(locally (declare #.*optimize-speed*)
       (slot-boundp-from-wrapper-and-slots
          ,instance ,slot-name
          (fast-check-wrapper-validity ,instance std-instance-wrapper)
          NIL NIL std-instance-slots))))

(defsetf safe-standard-instance-slot-value set-safe-standard-instance-slot-value)

(defmacro standard-instance-slot-value (instance slot-name)
  "Highly-optimized macro for returning the slot-value of instances that
   are guaranteed to be standard-instances."
  (cond (*always-safe-to-use-slot-wrapper-optimizations-p*
         `(safe-standard-instance-slot-value ,instance ,slot-name))
        (*safe-to-use-slot-value-wrapper-optimizations-p*
         (once-only (instance slot-name)
          `(locally (declare #.*optimize-speed*)
             (if *safe-to-use-slot-value-wrapper-optimizations-p*
                 (safe-standard-instance-slot-value ,instance ,slot-name)
                 (accessor-slot-value ,instance ,slot-name)))))
        (T `(accessor-slot-value ,instance ,slot-name))))

(defmacro set-standard-instance-slot-value
          (instance slot-name new-value)
  "Highly-optimized macro for setting the slot-value of instances that
   are guaranteed to be standard-instances."
  (cond (*always-safe-to-use-slot-wrapper-optimizations-p*
         `(setf (safe-standard-instance-slot-value ,instance ,slot-name)
                ,new-value))
        (*safe-to-use-set-slot-value-wrapper-optimizations-p*
         (once-only (instance slot-name new-value)
          `(locally (declare #.*optimize-speed*)
             (if *safe-to-use-set-slot-value-wrapper-optimizations-p*
                 (setf (safe-standard-instance-slot-value
                           ,instance ,slot-name)
                       ,new-value)
                 (setf (accessor-slot-value ,instance ,slot-name) ,new-value)))))
        (T `(setf (accessor-slot-value ,instance ,slot-name) ,new-value))))

(defmacro standard-instance-slot-boundp (instance slot-name)
  "Highly-optimized macro for checking whether a slot is bound for instances
   that are guaranteed to be standard-instances."
  (cond (*always-safe-to-use-slot-wrapper-optimizations-p*
         `(safe-standard-instance-slot-boundp ,instance ,slot-name))
        (*safe-to-use-slot-boundp-wrapper-optimizations-p*
         (once-only (instance slot-name)
          `(locally (declare #.*optimize-speed*)
             (if *safe-to-use-slot-boundp-wrapper-optimizations-p*
                 (safe-standard-instance-slot-boundp ,instance ,slot-name)
                 (accessor-slot-boundp ,instance ,slot-name)))))
        (T `(accessor-slot-boundp ,instance ,slot-name))))

(defsetf standard-instance-slot-value set-standard-instance-slot-value)


#+pcl-user-instances
(progn
(defmacro safe-user-instance-slot-value (instance slot-name)
  "Highly-optimized macro for returning the slot-value of instances that
   are guaranteed to be user instances with normal slots."
  (once-only (instance)
    `(locally (declare #.*optimize-speed*)
       (slot-value-from-wrapper-and-slots
          ,instance ,slot-name
          (fast-check-wrapper-validity ,instance get-user-instance-wrapper)
          NIL NIL get-user-instance-slots))))

(defmacro set-safe-user-instance-slot-value (instance slot-name new-value)
  "Highly-optimized macro for setting the slot-value of instances that
   are guaranteed to be user instances with normal slots."
  (once-only (instance)
    `(locally (declare #.*optimize-speed*)
       (setf (slot-value-from-wrapper-and-slots
                ,instance ,slot-name
                (fast-check-wrapper-validity ,instance get-user-instance-wrapper)
                NIL NIL get-user-instance-slots)
            ,new-value))))

(defmacro safe-user-instance-slot-boundp (instance slot-name)
  "Highly-optimized macro for checking whether a slot is bound for instances
   that are guaranteed to be user-instances with normal slots."
  (once-only (instance slot-name)
    `(locally (declare #.*optimize-speed*)
       (slot-boundp-from-wrapper-and-slots
          ,instance ,slot-name
          (fast-check-wrapper-validity ,instance get-user-instance-wrapper)
          NIL NIL get-user-instance-slots))))

(defsetf safe-user-instance-slot-value set-safe-user-instance-slot-value)


(defmacro user-instance-slot-value (instance slot-name)
  "Highly-optimized macro for returning the slot-value of instances that
   are guaranteed to be user-instances."
  (cond (*always-safe-to-use-slot-wrapper-optimizations-p*
         `(safe-user-instance-slot-value ,instance ,slot-name))
        (*safe-to-use-slot-value-wrapper-optimizations-p*
         (once-only (instance slot-name)
          `(locally (declare #.*optimize-speed*)
             (if *safe-to-use-slot-value-wrapper-optimizations-p*
                 (safe-user-instance-slot-value ,instance ,slot-name)
                 (accessor-slot-value ,instance ,slot-name)))))
        (T `(accessor-slot-value ,instance ,slot-name))))

(defmacro set-user-instance-slot-value
          (instance slot-name new-value)
  "Highly-optimized macro for setting the slot-value of instances that
   are guaranteed to be user-instances."
  (cond (*always-safe-to-use-slot-wrapper-optimizations-p*
         `(setf (safe-user-instance-slot-value ,instance ,slot-name) ,new-value))
        (*safe-to-use-set-slot-value-wrapper-optimizations-p*
         (once-only (instance slot-name new-value)
          `(locally (declare #.*optimize-speed*)
             (if *safe-to-use-set-slot-value-wrapper-optimizations-p*
                 (setf (safe-user-instance-slot-value ,instance ,slot-name)
                       ,new-value)
                 (setf (accessor-slot-value ,instance ,slot-name) ,new-value)))))
        (T `(setf (accessor-slot-value ,instance ,slot-name) ,new-value))))

(defmacro user-instance-slot-boundp (instance slot-name)
  "Highly-optimized macro for checking whether a slot is bound for instances
   that are guaranteed to be user-instances."
  (cond (*always-safe-to-use-slot-wrapper-optimizations-p*
         `(safe-user-instance-slot-boundp ,instance ,slot-name))
        (*safe-to-use-slot-boundp-wrapper-optimizations-p*
         (once-only (instance slot-name)
          `(locally (declare #.*optimize-speed*)
             (if *safe-to-use-slot-boundp-wrapper-optimizations-p*
                 (safe-user-instance-slot-boundp ,instance ,slot-name)
                 (accessor-slot-boundp ,instance ,slot-name)))))
        (T `(accessor-slot-boundp ,instance ,slot-name))))

(defsetf user-instance-slot-value set-user-instance-slot-value)
) #+pcl-user-instances

#+(and excl sun4) (values)  ;; Weird bug in Allegro CL 3.1.13.1 [Sun4] requires this.

(defun make-slot-symbol (slot-name type pname)
  (unless (symbol-package slot-name)
    (slot-symbol-error slot-name))
  (setf (get slot-name pname)
        (intern (format nil "~A::~A slot ~a" 
                        (package-name (symbol-package slot-name))
                        (symbol-name slot-name)
                        type)
                *slot-accessor-name-package*)))

(defmacro slot-symbol (slot-name type pname)
  (once-only (slot-name)
    `(if (symbolp ,slot-name)
         (or (get ,slot-name ,pname)
             (make-slot-symbol ,slot-name ,type ,pname))
         (slot-symbol-error ,slot-name))))

(defun slot-symbol-error (slot-name)
  (error "non-symbol slot-names (~S) are not yet implemented" slot-name))

(defmacro slot-reader-symbol (slot-name)
  `(slot-symbol ,slot-name 'reader 'reader-slot-symbol))

(defmacro slot-writer-symbol (slot-name)
  `(slot-symbol ,slot-name 'writer 'writer-slot-symbol))

(defmacro slot-boundp-symbol (slot-name)
  `(slot-symbol ,slot-name 'boundp 'boundp-slot-symbol))

(defun find-slot-definition (class slot-name)
  (declare #.*optimize-speed*)
  (macrolet ((find-def (class-internal-slotds slot-name)
               `(let ((ptr ,class-internal-slotds))
                  (loop (when (null ptr)
                          (return NIL))
                        (when (eq (internal-slotd-name (car ptr)) ,slot-name)
                          (return (internal-slotd-slot-definition (car ptr))))
                        (setf ptr (cdr ptr))))))
    (if (or (eq class *the-class-standard-class*)
            (eq class *the-class-funcallable-standard-class*)
            (eq class *the-class-standard-effective-slot-definition*))
        (find-def (safe-standard-instance-slot-value class 'internal-slotds)
                  slot-name)
        (find-def (class-internal-slotds class) slot-name))))

(defun no-slot-accessor (object slot-name sym operation &optional new-value)
  (if (and (not (or (std-instance-p  object)
                    (fsc-instance-p  object)
                    #+pcl-user-instances
                    (user-instance-p object)))
           (typep object 'structure))
      (let ((structure-class (find-class (type-of object) nil)))
        (if structure-class
            (ecase operation
              (slot-value  (slot-value object slot-name))
              (setf        (setf (slot-value object slot-name) new-value))
              (slot-boundp (slot-boundp object slot-name)))
            (error
              "Trying to do ~S for slot ~S on ~S, an instance of structure ~S
               that was defined before PCL was loaded and that PCL can't make
               a class for in this lisp."
              (if (eq operation 'setf) '(setf slot-value) operation)
              slot-name object (type-of object))))
      (error "Trying to do ~S on ~S, but no class has a slot named ~S
              (~s has no function binding) (or maybe your files were
              compiled with an old version of PCL:  try recompiling.)"
              (if (eq operation 'setf) '(setf slot-value) operation)
              object slot-name sym)))

(defun no-slot-value-accessor (object slot-name sym)
  (no-slot-accessor object slot-name sym 'slot-value))

(defun no-set-slot-value-accessor (object slot-name sym new-value)
  (no-slot-accessor object slot-name sym 'setf new-value))

(defun no-slot-boundp-accessor (object slot-name sym)
  (no-slot-accessor object slot-name sym 'slot-boundp))


(defun slow-slot-value (object slot-name)
  (let ((class (class-of object)))
    (if (eq class *the-class-standard-effective-slot-definition*)
        (safe-standard-instance-slot-value object slot-name)
        (let ((slot-definition (find-slot-definition class slot-name)))
          (if (null slot-definition)
              (slot-missing class object slot-name 'slot-value)
              (slot-value-using-class class object slot-definition))))))

(defmacro accessor-slot-value (object slot-name-form)
  (if (and (constantp slot-name-form)
           (let ((slot-name (eval slot-name-form)))
             (and (symbolp slot-name) (symbol-package slot-name))))
      (let* ((slot-name (eval slot-name-form))
             (sym (slot-reader-symbol slot-name)))
        (once-only (object)
          `(if (fboundp ',sym)
               (funcall-compiled (symbol-function ',sym) ,object)
               (no-slot-value-accessor ,object ',slot-name ',sym))))
      (let ((sym (gensym "READER-SYMBOL")))
        (once-only (object slot-name-form)
          `(let ((,sym (slot-reader-symbol ,slot-name-form)))
             (declare (type symbol ,sym))
             (if (fboundp ,sym)
                 (funcall-compiled (symbol-function ,sym) ,object)
                 (no-slot-value-accessor ,object ,slot-name-form ',sym)))))))

(defmacro wrapper-optimized-slot-value (object slot-name
                                        &optional
                                        (alternate-sv 'accessor-slot-value))
  (once-only (object slot-name)
    `(locally (declare #.*optimize-speed*)
       (let ((wrapper NIL)
             (slots   NIL))
         (if (or (and (std-instance-p ,object)
                      (setf wrapper (fast-check-wrapper-validity
                                       ,object std-instance-wrapper))
                      (setf slots (std-instance-slots ,object)))
                 (and (fsc-instance-p ,object)
                      (setf wrapper (fast-check-wrapper-validity
                                      ,object fsc-instance-wrapper))
                      (setf slots (fsc-instance-slots ,object)))
                 #+pcl-user-instances
                 (and (get-user-instance-p ,object)
                      (setf wrapper (fast-check-user-wrapper-validity ,object))
                      (setf slots (get-user-instance-slots ,object))))
             (slot-value-from-wrapper-and-slots
               ,object ,slot-name wrapper NIL slots NIL)
             (,alternate-sv ,object ,slot-name))))))

(defmacro fast-slot-value (object slot-name
                           &optional (alternate-sv 'accessor-slot-value))
  "Optimized macro version of slot-value."
  (cond (*always-safe-to-use-slot-wrapper-optimizations-p*
         `(wrapper-optimized-slot-value ,object ,slot-name ,alternate-sv))
        (*safe-to-use-slot-value-wrapper-optimizations-p*
         (once-only (object slot-name)
          `(locally (declare #.*optimize-speed*)
             (if *safe-to-use-slot-value-wrapper-optimizations-p*
                 (wrapper-optimized-slot-value ,object ,slot-name ,alternate-sv)
                 (,alternate-sv ,object ,slot-name)))))
        (T `(,alternate-sv ,object ,slot-name))))

(defun slot-value (object slot-name)
  (fast-slot-value object slot-name))

(define-compiler-macro slot-value (object-form slot-name-form)
  `(fast-slot-value ,object-form ,slot-name-form))

(proclaim '(notinline unoptimized-slot-value))

(defun unoptimized-slot-value (object slot-name)
  (fast-slot-value object slot-name))


(defun slow-set-slot-value (object slot-name new-value)
  (let* ((class (class-of object))
         (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
        (slot-missing class object slot-name 'setf)
        (setf (slot-value-using-class class object slot-definition) new-value))))

(defsetf slow-slot-value slow-set-slot-value)

(defmacro accessor-set-slot-value (object slot-name-form new-value)
  (if (and (constantp slot-name-form)
           (let ((slot-name (eval slot-name-form)))
             (and (symbolp slot-name) (symbol-package slot-name))))
      (let* ((slot-name (eval slot-name-form))
             (sym (slot-writer-symbol slot-name)))
        (once-only (object new-value)
          `(if (fboundp ',sym)
               (funcall-compiled (symbol-function ',sym) ,new-value ,object)
               (no-set-slot-value-accessor ,object ',slot-name ',sym ,new-value))))
      (let ((sym (gensym "WRITER-SYM")))
        (once-only (object new-value slot-name-form)
          `(let ((,sym (slot-writer-symbol ,slot-name-form)))
             (declare (type symbol ,sym))
             (if (fboundp ,sym)
                 (funcall-compiled (symbol-function ,sym) ,new-value ,object)
                 (no-set-slot-value-accessor ,object ,slot-name-form ',sym
                                             ,new-value)))))))

(defmacro wrapper-optimized-set-slot-value (object slot-name new-value
                                            &optional
                                            (alternate-sv
                                              'accessor-slot-value))
  (once-only (object slot-name)
    `(locally (declare #.*optimize-speed*)
       (let ((wrapper NIL)
             (slots   NIL))
         (if (or (and (std-instance-p ,object)
                      (setf wrapper (fast-check-wrapper-validity
                                       ,object std-instance-wrapper))
                      (setf slots (std-instance-slots ,object)))
                 (and (fsc-instance-p ,object)
                      (setf wrapper (fast-check-wrapper-validity
                                      ,object fsc-instance-wrapper))
                      (setf slots (fsc-instance-slots ,object)))
                 #+pcl-user-instances
                 (and (get-user-instance-p ,object)
                      (setf wrapper (fast-check-user-wrapper-validity ,object))
                      (setf slots (get-user-instance-slots ,object))))
             (setf (slot-value-from-wrapper-and-slots
                     ,object ,slot-name wrapper NIL slots NIL)
                   ,new-value)
             (setf (,alternate-sv ,object ,slot-name) ,new-value))))))

(defmacro fast-set-slot-value (object slot-name new-value
                               &optional
                               (alternate-sv 'accessor-slot-value))
  "Optimized macro version of set-slot-value."
  (cond (*always-safe-to-use-slot-wrapper-optimizations-p*
         `(wrapper-optimized-set-slot-value ,object ,slot-name
                                            ,new-value ,alternate-sv))
        (*safe-to-use-slot-value-wrapper-optimizations-p*
         (once-only (object slot-name new-value)
          `(locally (declare #.*optimize-speed*)
             (if *safe-to-use-slot-value-wrapper-optimizations-p*
                 (wrapper-optimized-set-slot-value ,object ,slot-name
                                                   ,new-value ,alternate-sv)
                 (setf (,alternate-sv ,object ,slot-name) ,new-value)))))
        (T `(setf (,alternate-sv ,object ,slot-name) ,new-value))))

(defmacro do-fast-set-slot-value (object slot-name new-value)
  `(fast-set-slot-value ,object ,slot-name ,new-value))

(defsetf accessor-slot-value accessor-set-slot-value)
(defsetf fast-slot-value do-fast-set-slot-value)



(defun set-slot-value (object slot-name new-value)
  (fast-set-slot-value object slot-name new-value))

(define-compiler-macro set-slot-value (object-form slot-name-form new-value-form)
  `(fast-set-slot-value ,object-form ,slot-name-form ,new-value-form))

(proclaim '(notinline unoptimized-set-slot-value))

(defun unoptimized-set-slot-value (object slot-name new-value)
  (fast-set-slot-value object slot-name new-value))


(eval-when (compile load eval)
(defconstant *optimize-slot-boundp* nil))

(defun slow-slot-boundp (object slot-name)
  (let* ((class (class-of object))
         (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
        (slot-missing class object slot-name 'slot-boundp)
        (slot-boundp-using-class class object slot-definition))))

(defmacro accessor-slot-boundp (object slot-name-form)
  (cond ((not *optimize-slot-boundp*)
         `(slow-slot-boundp ,object ,slot-name-form))
        ((and (constantp slot-name-form)
              (let ((slot-name (eval slot-name-form)))
                (and (symbolp slot-name) (symbol-package slot-name))))
         (let* ((slot-name (eval slot-name-form))
                (sym (slot-boundp-symbol slot-name)))
           (once-only (object)
              `(if (fboundp ',sym)
                   (funcall-compiled (symbol-function ',sym) ,object)
                   (no-slot-boundp-accessor ,object ',slot-name ',sym)))))
        (T
         (let ((sym (gensym "BOUNDP-SYM")))
           (once-only (object slot-name-form)
             `(let ((,sym (slot-boundp-symbol ,slot-name-form)))
                (declare (type symbol ,sym))
                (if (fboundp ,sym)
                    (funcall-compiled (symbol-function ,sym) ,object)
                    (no-slot-boundp-accessor ,object ,slot-name-form ',sym))))))))

(defmacro wrapper-optimized-slot-boundp (object slot-name
                                        &optional
                                        (alternate-sv 'accessor-slot-boundp))
  (once-only (object slot-name)
    `(locally (declare #.*optimize-speed*)
       (let ((wrapper NIL)
             (slots   NIL))
         (if (or (and (std-instance-p ,object)
                      (setf wrapper (fast-check-wrapper-validity
                                       ,object std-instance-wrapper))
                      (setf slots (std-instance-slots ,object)))
                 (and (fsc-instance-p ,object)
                      (setf wrapper (fast-check-wrapper-validity
                                      ,object fsc-instance-wrapper))
                      (setf slots (fsc-instance-slots ,object)))
                 #+pcl-user-instances
                 (and (get-user-instance-p ,object)
                      (setf wrapper (fast-check-user-wrapper-validity ,object))
                      (setf slots (get-user-instance-slots ,object))))
             (slot-boundp-from-wrapper-and-slots
               ,object ,slot-name wrapper NIL slots NIL)
             (,alternate-sv ,object ,slot-name))))))

(defmacro fast-slot-boundp (object slot-name
                           &optional (alternate-sv 'accessor-slot-boundp))
  "Optimized macro version of slot-boundp."
  (cond (*always-safe-to-use-slot-wrapper-optimizations-p*
         `(wrapper-optimized-slot-boundp ,object ,slot-name ,alternate-sv))
        (*safe-to-use-slot-boundp-wrapper-optimizations-p*
         (once-only (object slot-name)
          `(locally (declare #.*optimize-speed*)
             (if *safe-to-use-slot-boundp-wrapper-optimizations-p*
                 (wrapper-optimized-slot-boundp ,object ,slot-name ,alternate-sv)
                 (,alternate-sv ,object ,slot-name)))))
        (T `(,alternate-sv ,object ,slot-name))))

(defun slot-boundp (object slot-name)
  (fast-slot-boundp object slot-name))

(define-compiler-macro slot-boundp (object-form slot-name-form)
  `(fast-slot-boundp ,object-form ,slot-name-form))


(defun slot-makunbound (object slot-name)
  (let* ((class (class-of object))
         (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
        (slot-missing class object slot-name 'slot-makunbound)
        (slot-makunbound-using-class class object slot-definition))))

(defun slot-exists-p (object slot-name)
  (let* ((class (class-of object))
         (slot-definition (find-slot-definition class slot-name)))
    (and slot-definition
         (slot-exists-p-using-class class object slot-definition))))

;;;
;;; This isn't documented, but is used within PCL in a number of print
;;; object methods (see named-object-print-function).
;;; 
(defun slot-value-or-default (object slot-name &optional (default "unbound"))
  (if (slot-boundp object slot-name)
      (slot-value object slot-name)
      default))


;;;
;;; 
;;; 
(defun standard-instance-access (instance location)
  (%svref (std-instance-slots instance) location))

(defun funcallable-standard-instance-access (instance location)
  (%svref (fsc-instance-slots instance) location))

(defun set-standard-instance-access (instance location new-value)
  (setf (%svref (std-instance-slots instance) location) new-value))

(defun set-funcallable-standard-instance-access (instance location new-value)
  (setf (%svref (fsc-instance-slots instance) location) new-value))

(defsetf standard-instance-access set-standard-instance-access)
(defsetf funcallable-standard-instance-access set-funcallable-standard-instance-access)

#+pcl-user-instances
(defun user-instance-access (instance location)
  (%svref (user-instance-slots instance) location))

#+pcl-user-instances
(defun set-user-instance-access (instance location new-value)
  (setf (%svref (user-instance-slots instance) location) new-value))

#+pcl-user-instances
(defsetf user-instance-access set-user-instance-access)

(defmethod slot-value-using-class ((class std-class)
                                   (object standard-object)
                                   (slotd standard-effective-slot-definition))
  (let* ((location (slot-definition-location slotd))
         (value (typecase location
                  (fixnum 
                   (cond ((std-instance-p object)
                          (unless (eq 't (wrapper-state (std-instance-wrapper object)))
                            (check-wrapper-validity object))
                          (%svref (std-instance-slots object) location))
                         ((fsc-instance-p object)
                          (unless (eq 't (wrapper-state (fsc-instance-wrapper object)))
                            (check-wrapper-validity object))
                          (%svref (fsc-instance-slots object) location))
                         #+pcl-user-instances
                         ((user-instance-p object)
                          (unless (eq 't (wrapper-state (user-instance-wrapper object)))
                            (check-wrapper-validity object))
                          (%svref (user-instance-slots object) location))
                         (t (wrapper-error object))))
                  (cons
                   (cdr location))
                  (t
                   (error "The slot ~s has neither :instance nor :class allocation, ~@
                           so it can't be read by the default ~s method."
                          slotd 'slot-value-using-class)))))
    (if (eq value *slot-unbound*)
        (slot-unbound class object (slot-definition-name slotd))
        value)))

(defmethod (setf slot-value-using-class)
           (new-value (class std-class)
                      (object standard-object)
                      (slotd standard-effective-slot-definition))
  (let ((location (slot-definition-location slotd)))
    (typecase location
      (fixnum 
       (cond ((std-instance-p object)
              (unless (eq 't (wrapper-state (std-instance-wrapper object)))
                (check-wrapper-validity object))
              (setf (%svref (std-instance-slots object) location) new-value))
             ((fsc-instance-p object)
              (unless (eq 't (wrapper-state (fsc-instance-wrapper object)))
                (check-wrapper-validity object))
              (setf (%svref (fsc-instance-slots object) location) new-value))
             #+pcl-user-instances
             ((user-instance-p object)
              (unless (eq 't (wrapper-state (user-instance-wrapper object)))
                (check-wrapper-validity object))
              (setf (%svref (user-instance-slots object) location) new-value))
             (t (wrapper-error object))))
      (cons
       (setf (cdr location) new-value))
      (t
       (error "The slot ~s has neither :instance nor :class allocation, ~@
                           so it can't be written by the default ~s method."
              slotd '(setf slot-value-using-class))))))

(defmethod slot-boundp-using-class
           ((class std-class) 
            (object standard-object) 
            (slotd standard-effective-slot-definition))
  (let* ((location (slot-definition-location slotd))
         (value (typecase location
                  (fixnum 
                   (cond ((std-instance-p object)
                          (unless (eq 't (wrapper-state (std-instance-wrapper object)))
                            (check-wrapper-validity object))
                          (%svref (std-instance-slots object) location))
                         ((fsc-instance-p object)
                          (unless (eq 't (wrapper-state (fsc-instance-wrapper object)))
                            (check-wrapper-validity object))
                          (%svref (fsc-instance-slots object) location))
                         #+pcl-user-instances
                         ((user-instance-p object)
                          (unless (eq 't (wrapper-state (user-instance-wrapper object)))
                            (check-wrapper-validity object))
                          (%svref (user-instance-slots object) location))
                         (t (wrapper-error object))))
                  (cons
                   (cdr location))
                  (t
                   (error "The slot ~s has neither :instance nor :class allocation, ~@
                           so it can't be read by the default ~s method."
                          slotd 'slot-boundp-using-class)))))
    (not (eq value *slot-unbound*))))

(defmethod slot-makunbound-using-class
           ((class std-class)
            (object standard-object) 
            (slotd standard-effective-slot-definition))
  (let ((location (slot-definition-location slotd)))
    (typecase location
      (fixnum 
       (cond ((std-instance-p object)
              (unless (eq 't (wrapper-state (std-instance-wrapper object)))
                (check-wrapper-validity object))
              (setf (%svref (std-instance-slots object) location) *slot-unbound*))
             ((fsc-instance-p object)
              (unless (eq 't (wrapper-state (fsc-instance-wrapper object)))
                (check-wrapper-validity object))
              (setf (%svref (fsc-instance-slots object) location) *slot-unbound*))
             #+pcl-user-instances
             ((user-instance-p object)
              (unless (eq 't (wrapper-state (user-instance-wrapper object)))
                (check-wrapper-validity object))
              (setf (%svref (user-instance-slots object) location) *slot-unbound*))
             (t (wrapper-error object))))
      (cons
       (setf (cdr location) *slot-unbound*))
      (t
       (error "The slot ~s has neither :instance nor :class allocation, ~@
                           so it can't be written by the default ~s method."
              slotd 'slot-makunbound-using-class))))
  nil)

(defmethod slot-exists-p-using-class
           ((class std-class)
            (object standard-object)
            (slotd standard-effective-slot-definition))
  t)


(defmacro structure-instance-slot-value (instance slot-name)
  "Highly-optimized macro for returning the slot-value of instances that
   are guaranteed to be structure instances with normal slots."
  `(accessor-slot-value ,instance ,slot-name))

(defmacro set-structure-instance-slot-value (instance slot-name new-value)
  "Highly-optimized macro for setting the slot-value of instances that
   are guaranteed to be structure instances with normal slots."
  `(accessor-set-slot-value ,instance ,slot-name ,new-value))

(defmacro structure-instance-slot-boundp (instance slot-name)
  "Highly-optimized macro for checking whether a slot is bound for instances
   that are guaranteed to be structure-instances with normal slots."
  `(accessor-slot-boundp ,instance ,slot-name))

(defsetf structure-instance-slot-value set-structure-instance-slot-value)


(defmethod slot-value-using-class
    ((class structure-class)
     (object structure-object)
     (slotd structure-effective-slot-definition))
  (let ((function (slot-definition-internal-reader-function slotd)))
    (method-function-funcall function object)))

(defmethod (setf slot-value-using-class)
    (new-value (class structure-class)
               (object structure-object)
               (slotd structure-effective-slot-definition))
  (let ((function (slot-definition-internal-writer-function slotd)))
    (method-function-funcall function new-value object)))

(defmethod slot-boundp-using-class
           ((class structure-class) 
            (object structure-object)
            (slotd structure-effective-slot-definition))
  t)

(defmethod slot-makunbound-using-class
           ((class structure-class)
            (object structure-object)
            (slotd structure-effective-slot-definition))
  (error "Structure slots can't be unbound"))


(defmethod slot-missing
           ((class t) instance slot-name operation &optional new-value)
  (error "When attempting to ~A,~%the slot ~S is missing from the object ~S."
         (ecase operation
           (slot-value "read the slot's value (slot-value)")
           (setf (format nil
                         "set the slot's value to ~S (setf of slot-value)"
                         new-value))
           (slot-boundp "test to see if slot is bound (slot-boundp)")
           (slot-makunbound "make the slot unbound (slot-makunbound)"))
         slot-name
         instance))

(defmethod slot-unbound ((class t) instance slot-name)
  (error "The slot ~S is unbound in the object ~S." slot-name instance))


(defun structure-slot-boundp (object)
  (declare (ignore object))
  t)

(declaim (ftype (function (T T T) (values function boolean))
		get-optimized-std-accessor-method-function))
(defun get-optimized-std-accessor-method-function (class slotd name)
  (if (structure-class-p class)
      (values
        (ecase name
          (reader (slot-definition-internal-reader-function slotd))
          (writer (slot-definition-internal-writer-function slotd))
          (boundp #'structure-slot-boundp))
        nil)
      (let* ((instance-type (class-instance-type class))
             (slot-name (slot-definition-name slotd))
             (index (slot-definition-location slotd))
             (function (ecase name
                         (reader #'get-optimized-std-reader-method-function)
                         (writer #'get-optimized-std-writer-method-function)
                         (boundp #'get-optimized-std-boundp-method-function)))
             (value (funcall-function function instance-type slot-name index)))
        (values value (not (null index))))))

(defvar *optimized-std-reader-table* (make-hash-table :test 'equal))
(defvar *optimized-std-writer-table* (make-hash-table :test 'equal))
(defvar *optimized-std-boundp-table* (make-hash-table :test 'equal))

(defun get-optimized-std-reader-method-function (instance-type slot-name index)
  (etypecase index
    (fixnum
      (let ((table-index (list instance-type slot-name index)))
        (or (gethash table-index *optimized-std-reader-table*)
            (setf (gethash table-index *optimized-std-reader-table*)
                  (make-optimized-std-reader-method-function
                     instance-type slot-name index)))))
    (cons
      (make-optimized-std-reader-method-function
         instance-type slot-name index))))

(defun get-optimized-std-writer-method-function (instance-type slot-name index)
  (etypecase index
    (fixnum
      (let ((table-index (list instance-type slot-name index)))
        (or (gethash table-index *optimized-std-writer-table*)
            (setf (gethash table-index *optimized-std-writer-table*)
                  (make-optimized-std-writer-method-function
                     instance-type slot-name index)))))
    (cons
      (make-optimized-std-writer-method-function
         instance-type slot-name index))))

(defun get-optimized-std-boundp-method-function (instance-type slot-name index)
  (etypecase index
    (fixnum
      (let ((table-index (list instance-type slot-name index)))
        (or (gethash table-index *optimized-std-boundp-table*)
            (setf (gethash table-index *optimized-std-boundp-table*)
                  (make-optimized-std-boundp-method-function
                     instance-type slot-name index)))))
    (cons
      (make-optimized-std-boundp-method-function
         instance-type slot-name index))))

(defun make-optimized-std-reader-method-function (instance-type slot-name index)
  (declare #.*optimize-speed*)
  (set-function-name
   (etypecase index
     (fixnum (ecase instance-type
               (std-instance
                 #'(lambda (instance)
                     (let ((value (%svref (std-instance-slots instance) index)))
                       (if (eq value *slot-unbound*)
                           (slot-unbound (class-of instance) instance slot-name)
                           value))))
               (fsc-instance
                 #'(lambda (instance)
                     (let ((value (%svref (fsc-instance-slots instance) index)))
                       (if (eq value *slot-unbound*)
                           (slot-unbound (class-of instance) instance slot-name)
                           value))))
               #+pcl-user-instances
               (user-instance
                 (make-optimized-user-reader-method-function slot-name index))))
     (cons   #'(lambda (instance)
                 (let ((value (cdr index)))
                   (if (eq value *slot-unbound*)
                       (slot-unbound (class-of instance) instance slot-name)
                       value)))))
   `(reader ,slot-name)))

(defun make-optimized-std-writer-method-function (instance-type slot-name index)
  (declare #.*optimize-speed*)
  (set-function-name
   (etypecase index
     (fixnum (ecase instance-type
               (std-instance
                 #'(lambda (nv instance)
                     (setf (%svref (std-instance-slots instance) index) nv)))
               (fsc-instance
                 #'(lambda (nv instance)
                     (setf (%svref (fsc-instance-slots instance) index) nv)))
               #+pcl-user-instances
               (user-instance
                 (make-optimized-user-writer-method-function index))))
     (cons   #'(lambda (nv instance)
                 (declare (ignore instance))
                 (setf (cdr index) nv))))
   `(writer ,slot-name)))

(defun make-optimized-std-boundp-method-function (instance-type slot-name index)
  (declare #.*optimize-speed*)
  (set-function-name
   (etypecase index
     (fixnum (ecase instance-type
               (std-instance
                 #'(lambda (instance)
                     (not (eq *slot-unbound* 
                              (%svref (std-instance-slots instance) index)))))
               (fsc-instance
                 #'(lambda (instance)
                     (not (eq *slot-unbound*
                              (%svref (fsc-instance-slots instance) index)))))
               #+pcl-user-instances
               (user-instance
                 (make-optimized-user-boundp-method-function index))))
     (cons   #'(lambda (instance)
                 (declare (ignore instance))
                 (not (eq *slot-unbound* (cdr index))))))
   `(boundp ,slot-name)))

#+pcl-user-instances
(defun make-optimized-user-reader-method-function (slot-name index)
  (declare #.*optimize-speed*)
  (progn slot-name)
  #'(lambda (instance)
      (let ((value (%svref (user-instance-slots instance) index)))
        (if (eq value *slot-unbound*)
            (slot-unbound (class-of instance) instance slot-name)
            value))))

#+pcl-user-instances
(defun make-optimized-user-writer-method-function (index)
  (declare #.*optimize-speed*)
  #'(lambda (nv instance)
      (setf (%svref (user-instance-slots instance) index) nv)))

#+pcl-user-instances
(defun make-optimized-user-boundp-method-function (index)
  (declare #.*optimize-speed*)
  #'(lambda (instance)
      (not (eq *slot-unbound* (%svref (user-instance-slots instance) index)))))


(defvar *accessor-from-svuc-table* (make-hash-table :test 'equal))

(defun get-accessor-from-svuc-method-function (class slotd sdfun name)
  (declare (type real-function sdfun))
  (let ((table-index (list class slotd sdfun name)))
    (or (gethash table-index *accessor-from-svuc-table*)
        (setf (gethash table-index *accessor-from-svuc-table*)
              (make-accessor-from-svuc-method-function
                 class slotd sdfun name)))))

(defun make-accessor-from-svuc-method-function (class slotd sdfun name)
  (declare #.*optimize-speed*)
  (declare (type real-function sdfun))
  (set-function-name
   (case name
     (reader #'(lambda (instance)
                 (method-function-funcall sdfun class instance slotd)))
     (writer #'(lambda (nv instance)
                 (method-function-funcall sdfun nv class instance slotd)))
     (boundp #'(lambda (instance)
                 (method-function-funcall sdfun class instance slotd))))
   `(,name ,(class-name class) ,(slot-definition-name slotd))))


(defun make-std-reader-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (instance)
      (fast-slot-value instance slot-name slow-slot-value)))

(defun make-std-writer-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (nv instance)
      (fast-set-slot-value instance slot-name nv slow-slot-value)))

(defun make-std-boundp-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (instance)
      (fast-slot-boundp instance slot-name slow-slot-boundp)))


(defun make-documented-std-reader-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (args next-methods)
      (declare (ignore next-methods))
      (let ((instance (car args)))
        (fast-slot-value instance slot-name slow-slot-value))))

(defun make-documented-std-writer-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (args next-methods)
      (declare (ignore next-methods))
      (let ((instance  (cadr args))
            (new-value (car args)))
        (fast-set-slot-value instance slot-name new-value slow-slot-value))))

(defun make-documented-std-boundp-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (args next-methods)
      (declare (ignore next-methods))
      (let ((instance (car args)))
        (fast-slot-boundp instance slot-name slow-slot-boundp))))


