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
;;; This is the CMU Lisp version of the file low.
;;; 

(in-package 'pcl)

(defun function-ftype-declaimed-p (name)
  "Returns whether the function given by name already has its ftype declaimed."
  (multiple-value-bind (ftype-info recorded-p)
      (extensions:info function type name)
    (declare (ignore ftype-info))
    recorded-p))

(defmacro dotimes ((var count &optional (result nil)) &body body)
  `(lisp:dotimes (,var (the index ,count) ,result)
     (declare (type index ,var))
     ,@body))

;;; Just use our without-interrupts.  We don't have the INTERRUPTS-ON/OFF local
;;; macros spec'ed in low.lisp, but they aren't used.
;;;
(defmacro without-interrupts (&rest stuff)
  `(sys:without-interrupts ,@stuff))


;;; Print the object addr in default printers.
;;;
(defun printing-random-thing-internal (thing stream)
  (format stream "{~X}" (sys:%primitive c:make-fixnum thing)))


(eval-when (compile load eval)
  (c:def-source-transform std-instance-p (x)
    (ext:once-only ((n-x x))
      `(and (ext:structurep ,n-x)
            (eq (kernel:structure-ref ,n-x 0) 'std-instance)))))

  ;;   
;;;;;; Cache No's
  ;;  

(proclaim '(inline object-cache-no))

(defun object-cache-no (symbol mask)
  (logand (ext:truly-the fixnum (system:%primitive make-fixnum symbol))
          (the fixnum mask)))


(defun function-arglist (fcn)
  "Returns the argument list of a compiled function, if possible."
  (cond ((symbolp fcn)
         (when (fboundp fcn)
           (function-arglist (symbol-function fcn))))
        ((eval:interpreted-function-p fcn)
         (eval:interpreted-function-arglist fcn))
        ((functionp fcn)
         (let ((lambda-expr (function-lambda-expression fcn)))
           (if lambda-expr
               (cadr lambda-expr)
               (let ((function (kernel:%closure-function fcn)))
                 (values (read-from-string
                          (kernel:%function-header-arglist function)))))))))


;;; We have this here and in fin.lisp, 'cause PCL wants to compile this
;;; file first.
;;; 
(defsetf funcallable-instance-name set-funcallable-instance-name)

;;; And returns the function, not the *name*.
(defun set-function-name (fcn new-name)
  "Set the name of a compiled function object."
  (cond ((symbolp fcn)
         (set-function-name (symbol-function fcn) new-name))
        ((funcallable-instance-p fcn)
         (setf (funcallable-instance-name fcn) new-name)
         fcn)
        ((eval:interpreted-function-p fcn)
         (setf (eval:interpreted-function-name fcn) new-name)
         fcn)
        (t
         (let ((header (kernel:%closure-function fcn)))
           (system:%primitive c::set-function-name header new-name))
         fcn)))

(in-package "C")

;;From compiler/ir1util
(def-source-context pcl::defmethod (name &rest stuff)
  (declare (type list stuff))
  (let ((arg-pos (position-if #'listp stuff)))
    (declare (type (or null index) arg-pos))
    (if arg-pos
	`(pcl::defmethod ,name ,@(subseq stuff 0 arg-pos)
	   ,(nth-value 2 (pcl::parse-specialized-lambda-list
			  (elt stuff arg-pos))))
	`(pcl::defmethod ,name "<illegal syntax>"))))


(in-package 'pcl)

(pushnew :structure-wrapper *features*)
(pushnew :structure-functions *features*)

(import 'ext:structurep)

(defmacro structure-type (x)
  `(kernel:structure-ref ,x 0))

(defun known-structure-type-p (type)
  (not (null (ext:info c::type c::defined-structure-info type))))

(defun structure-type-included-type-name (type)
  (let ((include (c::dd-include (ext:info c::type c::defined-structure-info type))))
    (if (consp include)
	(car include)
	include)))

(defun structure-type-slot-description-list (type)
  (nthcdr (length (the list
                       (let ((include (structure-type-included-type-name type)))
		         (and include (structure-type-slot-description-list include)))))
	  (c::dd-slots (ext:info c::type c::defined-structure-info type))))

(defun structure-slotd-name (slotd)
  (intern (c::dsd-%name slotd) "USER"))

(defun structure-slotd-accessor-symbol (slotd)
  (c::dsd-accessor slotd))

(defun structure-slotd-reader-function (slotd)
  (fdefinition (c::dsd-accessor slotd)))

(defun structure-slotd-writer-function (slotd)
  (unless (c::dsd-read-only slotd)
    (fdefinition `(setf ,(c::dsd-accessor slotd)))))

