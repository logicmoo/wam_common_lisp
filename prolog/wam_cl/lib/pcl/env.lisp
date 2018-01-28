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
;;; Basic environmental stuff.
;;;

(in-package 'pcl)

#+Lucid
(progn

(defun pcl-arglist (function &rest other-args)
  (let ((defn nil))
    (cond ((and (fsc-instance-p function)
                (generic-function-p function))
           (generic-function-pretty-arglist function))
          ((and (symbolp function)
                (fboundp function)
                (setq defn (symbol-function function))
                (fsc-instance-p defn)
                (generic-function-p defn))
           (generic-function-pretty-arglist defn))
          (t (apply (original-definition 'sys::arglist)
                    function other-args)))))

(redefine-function 'sys::arglist 'pcl-arglist)

)


;;;
;;;
;;;

(defgeneric describe-object (object stream))

#-Genera
(progn

(defun pcl-describe (object #+(or cltl2 UCL Lispm) &optional
			    #+(or cltl2 UCL) (stream *standard-output*)
			    #+Lispm no-complaints)
  (let (#+Lispm
	(*describe-no-complaints* no-complaints)
	#+(or cltl2 UCL)
	(describe-stream (cond ((null stream) *standard-output*)
			       ((eq stream T) *terminal-io*)
			       (T stream)))
	#-(or cltl2 UCL)
	(describe-stream *standard-output*))
    #+Lispm (declare (special *describe-no-complaints*))
    (describe-object object describe-stream)
    (fresh-line describe-stream)
    (values)))

(defmethod describe-object (object stream)
  (let ((*standard-output* stream))
    (funcall-compiled (original-definition 'describe)
		      object
		      #+(or cltl2 UCL) stream)))

(redefine-function 'describe 'pcl-describe)

)

(defmethod describe-object ((object slot-object) stream)
  (format stream "~%~S is an instance of class ~S:" object (class-of object))
  (describe-object-slots object stream))

(defmethod describe-object-slots
           ((object slot-object)
            stream
            &key
            (slots-to-inspect (slots-to-inspect (class-of object) object))
            &allow-other-keys)
  "Display the value of all the slots-to-inspect on this object."
  (let* ((max-slot-name-length 0)
         (instance-slotds ())
         (class-slotds ())
         (other-slotds ()))
    (declare (type index max-slot-name-length))
    (flet ((adjust-slot-name-length (name)
             (setq max-slot-name-length
                   (the index
                        (max max-slot-name-length
                             (length (the simple-string
                                          (symbol-name name)))))))
           (describe-slot (name value &optional (allocation () alloc-p))
             (if alloc-p
                 (format stream
                         "~% ~A ~S ~VT  "
                         name allocation (+ max-slot-name-length 7))
                 (format stream
                         "~% ~A~VT  "
                         name max-slot-name-length))
             (prin1 value stream)))

      ;; Figure out a good width for the slot-name column.
      (dolist (slotd slots-to-inspect)
        (adjust-slot-name-length (slot-definition-name slotd))
        (case (slot-definition-allocation slotd)
          (:instance (push slotd instance-slotds))
          (:class  (push slotd class-slotds))
          (otherwise (push slotd other-slotds))))
      (setq max-slot-name-length
            (the index (min (the index (+ max-slot-name-length 3)) 30)))

      (when instance-slotds
        (format stream "~% The following slots have :INSTANCE allocation:")
        (dolist (slotd (nreverse instance-slotds))
          (describe-slot (slot-definition-name slotd)
                         (slot-value-or-default
                           object (slot-definition-name slotd)))))

      (when class-slotds
        (format stream "~% The following slots have :CLASS allocation:")
        (dolist (slotd (nreverse class-slotds))
          (describe-slot (slot-definition-name slotd)
                         (slot-value-or-default
                            object (slot-definition-name slotd)))))

      (when other-slotds
        (format stream "~% The following slots have allocation as shown:")
        (dolist (slotd (nreverse other-slotds))
          (describe-slot (slot-definition-name slotd)
                         (slot-value-or-default
                           object (slot-definition-name slotd))
                         (slot-definition-allocation slotd))))
      (values))))

(defmethod slots-to-inspect ((class slot-class) (object slot-object))
  (class-slots class))

(defvar *describe-generic-functions-as-objects-p* nil)

(defmethod describe-object ((fun standard-generic-function) stream)
  (format stream "~A is a generic function.~%" fun)
  (format stream "Its arguments are:~%  ~S~%"
          (generic-function-pretty-arglist fun))
  (if *describe-generic-functions-as-objects-p*
      (describe-object-slots fun stream)
      (progn
        (format stream "Its methods are:")
        (dolist (meth (generic-function-methods fun))
          (format stream "~2%**** ~{~S ~}~:S =>~%"
                  (method-qualifiers meth)
                  (unparse-specializers meth))
          (describe-object meth stream)))))

;;;
;;;
;;;
(defvar *describe-classes-as-objects-p* nil)

(defmethod describe-object ((class class) stream)
  (flet ((pretty-class (c) (or (class-name c) c)))
    (macrolet ((ft (string &rest args) `(format stream ,string ,@args)))
      (ft "~&~S is a class, it is an instance of ~S.~%"
          class (pretty-class (class-of class)))
      (let ((name (class-name class)))
        (if name
            (if (eq class (find-class name nil))
                (ft "Its proper name is ~S.~%" name)
                (ft "Its name is ~S, but this is not a proper name.~%" name))
            (ft "It has no name (the name is NIL).~%")))
      (ft "The direct superclasses are: ~:S, and the direct~%~
           subclasses are: ~:S.  "
          (mapcar #'pretty-class (class-direct-superclasses class))
          (mapcar #'pretty-class (class-direct-subclasses class)))
      (if (class-finalized-p class)
          (ft "The class precedence list is:~%~S~%"
              (mapcar #'pretty-class (class-precedence-list class)))
          (ft "The class is not finalized.~%"))
      (ft "There are ~D methods specialized for this class."
          (length (the list (specializer-direct-methods class))))))
  (when *describe-classes-as-objects-p*
    (describe-object-slots class stream)))


(declaim (ftype (function (T &optional T) (values T T symbol))
		parse-method-or-spec))
(defun parse-method-or-spec (spec &optional (errorp t))
  (declare (values generic-function method method-name))
  (let (gf method name temp)
    (if (method-p spec) 
        (setq method spec
              gf (method-generic-function method)
              temp (and gf (generic-function-name gf))
              name (if temp
                       (intern-function-name
                         (make-method-spec temp
                                           (method-qualifiers method)
                                           (unparse-specializers
                                             (method-specializers method))))
                       (make-symbol (format nil "~S" method))))
        (multiple-value-bind (gf-spec quals specls)
            (parse-defmethod spec)
          (declare (list quals specls))
          (and (setq gf (and (or errorp (gboundp gf-spec))
                             (gdefinition gf-spec)))
               (let ((nreq (compute-discriminating-function-arglist-info gf)))
                 (declare (type index nreq))
                 (setq specls (append (parse-specializers specls)
                                      (make-list (the index (- nreq (length specls)))
                                                 :initial-element
                                                 *the-class-t*)))
                 (and 
                   (setq method (get-method gf quals specls errorp))
                   (setq name
                         (intern-function-name (make-method-spec gf-spec
                                                                 quals
                                                                 specls))))))))
    (values gf method name)))

(defmethod copy-instance-slots ((object1 slot-object)
                                (object2 slot-object)
                                &key
                                (exclude-slot-names NIL))
  (let ((obj1-slot-names
         (mapcar #'slot-definition-name (class-slots (class-of object1))))
        (obj2-slot-names
         (mapcar #'slot-definition-name (class-slots (class-of object2)))))
    (declare (type list obj1-slot-names obj2-slot-names))
    (dolist (slot-name obj1-slot-names)
      (when (and (not (memq slot-name exclude-slot-names))
                 (memq slot-name obj2-slot-names))
        (setf (slot-value object2 slot-name)
              (slot-value object1 slot-name))))))

;;;
;;; trace-method and untrace-method accept method specs as arguments.  A
;;; method-spec should be a list like:
;;;   (<generic-function-spec> qualifiers* (specializers*))
;;; where <generic-function-spec> should be either a symbol or a list
;;; of (SETF <symbol>).
;;;
;;;   For example, to trace the method defined by:
;;;
;;;     (defmethod foo ((x spaceship)) 'ss)
;;;
;;;   You should say:
;;;
;;;     (trace-method '(foo (spaceship)))
;;;
;;;   You can also provide a method object in the place of the method
;;;   spec, in which case that method object will be traced.
;;;
;;; For untrace-method, if an argument is given, that method is untraced.
;;; If no argument is given, all traced methods are untraced.
;;;

(defclass traced-method (standard-method)
     ((method :initarg :method)))

(defvar *traced-methods* ())

(defmethod trace-method ((spec cons) &rest options)
  (multiple-value-bind (gf method name)
      (parse-method-or-spec spec)
    (declare (ignore gf name))
    (apply #'trace-method method options)))

(defmethod trace-method ((tmethod traced-method) &rest options)
  (untrace-method tmethod)
  (apply #'trace-method (slot-value tmethod 'method) options))

(defmethod trace-method ((method standard-method) &rest options)
  (let* ((gf        (method-generic-function method))
         (base-name (symbol-name (method-function-name method)))
         (tmethod   (make-instance 'traced-method :method method))
         (function  (method-function method))
         (t-function
           (if function 
                (trace-function-internal
                 function (gentemp base-name) options)))
         (optimized-fn (method-optimized-function method))
         (t-optimized-fn
           (if optimized-fn
               (trace-function-internal
                 optimized-fn (gentemp base-name) options)))
         (traced-function-names
           (append (if function     (list t-function))
                   (if optimized-fn (list t-optimized-fn)))))
    (declare (type simple-string base-name)
             (type symbol        t-function t-optimized-fn))
    (copy-instance-slots method tmethod
                         :exclude-slot-names
                         '(function optimized-function cached-functions-alist
                           generic-function))
    (when function
      (setf (slot-value tmethod 'function)
            (symbol-function t-function)))
    (when optimized-fn
      (setf (slot-value tmethod 'optimized-function)
            (symbol-function t-optimized-fn)))
    (setf (slot-value tmethod 'cached-functions-alist)
          (mapcar
            #'(lambda (cached-fn)
                (let ((fn (cdr cached-fn)))
                  (cons
                    (car cached-fn)
                    (symbol-function
                      (the symbol
                           (cond ((eq fn function) t-function)
                                 ((eq fn optimized-fn) t-optimized-fn)
                                 (T
                                   (let ((t-name
                                          (trace-function-internal
                                            fn
                                            (gentemp base-name)
                                            options)))
                                     (push t-name traced-function-names)
                                     t-name))))))))
            (slot-value method 'cached-functions-alist)))
    (remove-method gf method)
    (add-method gf tmethod)
    (push (cons tmethod traced-function-names) *traced-methods*)
    tmethod))

(defun untrace-method (&optional spec)  
  (flet ((untrace-1 (method-cons-traces)
           (let* ((m  (car method-cons-traces))
                  (gf (method-generic-function m)))
             (when gf
               (remove-method gf m)
               (add-method gf (slot-value m 'method))))
           (untrace-method-function-names (cdr method-cons-traces))
           (setq *traced-methods*
                 (remove method-cons-traces *traced-methods* :test #'eq))))
    (cond ((consp spec)
           (multiple-value-bind (gf method)            
               (parse-method-or-spec spec)
             (declare (ignore gf))
             (let ((old-trace (assq method *traced-methods*)))
               (if old-trace
                   (untrace-1 old-trace)
                   (error "~S is not a traced method?" method)))))
          ((typep spec 'standard-method)
             (let ((old-trace (assq spec *traced-methods*)))
               (if old-trace
                   (untrace-1 old-trace)
                   (error "~S is not a traced method?" spec))))
          ((null spec)
           (dolist (trace *traced-methods*) (untrace-1 trace)))
          (T (error
              "Untrace-method needs method, method specifier, or nothing.")))))

(defun trace-function-internal (function name options)
  (eval `(untrace ,name))
  (setf (symbol-function name) function)
  (eval `(trace ,name ,@options))
  name)

(defun untrace-method-function-names (names)
  (dolist (name names)
    (setf (symbol-function name) NIL))
  (eval `(untrace ,@names)))

(defun trace-methods (gf)
  (let ((methods (generic-function-methods gf)))
    (dolist (method methods)
      (trace-method method))
    methods))



;(defun compile-method (spec)
;  (multiple-value-bind (gf method name)
;      (parse-method-or-spec spec)
;    (declare (ignore gf))
;    (compile name (method-function method))
;    (setf (method-function method) (symbol-function name))))

(defmacro undefmethod (&rest args)
  #+(or (not :lucid) :lcl3.0)
  (declare (arglist name {method-qualifier}* specializers))
  `(undefmethod-1 ',args))

(defun undefmethod-1 (args)
  (multiple-value-bind (gf method)
      (parse-method-or-spec args)
    (when (and gf method)
      (remove-method gf method)
      method)))


(pushnew :pcl *features*)
(pushnew :portable-commonloops *features*)
(pushnew :pcl-structures *features*)

#+cmu
(when (find-package "OLD-PCL")
  (setf (symbol-function 'old-pcl::print-object)
        (symbol-function 'pcl::print-object)))

