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
;;; This file contains the definition of the FUNCALLABLE-STANDARD-CLASS
;;; metaclass.  Much of the implementation of this metaclass is actually
;;; defined on the class STD-CLASS.  What appears in this file is a modest
;;; number of simple methods related to the low-level differences in the
;;; implementation of standard and funcallable-standard instances.
;;;
;;; As it happens, none of these differences are the ones reflected in
;;; the MOP specification; STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS
;;; share all their specified methods at STD-CLASS.
;;; 
;;; 
;;; workings of this metaclass and the standard-class metaclass.
;;; 

(in-package 'pcl)

(defmethod wrapper-fetcher ((class funcallable-standard-class))
  'fsc-instance-wrapper)

(defmethod slots-fetcher ((class funcallable-standard-class))
  'fsc-instance-slots)

(defmethod raw-instance-allocator ((class funcallable-standard-class))
  'allocate-funcallable-instance-1)

;;;
;;;
;;;

(defmethod validate-superclass
	   ((fsc funcallable-standard-class)
	    (class standard-class))
  t) ; was (null (wrapper-instance-slots-layout (class-wrapper class)))


(defmethod allocate-instance
	   ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  (unless (class-finalized-p class) (finalize-inheritance class))
  (let ((class-wrapper (class-wrapper class)))
    (allocate-funcallable-instance
      class-wrapper (wrapper-allocate-static-slot-storage-copy class-wrapper))))

(defmethod make-optimized-reader-method-function
           ((class funcallable-standard-class)
            generic-function
            reader-method-prototype
            slot-name)
  (declare (ignore generic-function reader-method-prototype))
  (make-funcallable-standard-instance-reader-method-function slot-name))

(defmethod make-optimized-writer-method-function
           ((class funcallable-standard-class)
            generic-function
            writer-method-prototype
            slot-name)
  (declare (ignore generic-function writer-method-prototype))
  (make-funcallable-standard-instance-writer-method-function slot-name))

(defmethod make-optimized-boundp-method-function
           ((class funcallable-standard-class)
            generic-function
            boundp-method-prototype
            slot-name)
  (declare (ignore generic-function boundp-method-prototype))
  (make-funcallable-standard-instance-boundp-method-function slot-name))

(defun make-funcallable-standard-instance-reader-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (instance)
      (funcallable-standard-instance-slot-value instance slot-name)))

(defun make-funcallable-standard-instance-writer-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (nv instance)
      (setf (funcallable-standard-instance-slot-value instance slot-name) nv)))

(defun make-funcallable-standard-instance-boundp-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (instance)
      (funcallable-standard-instance-slot-boundp instance slot-name)))

;;;;
;;;; See the comment about reader-function--std and writer-function--sdt.
;;;;
;(define-function-template reader-function--fsc () '(slot-name)
;  `(function
;     (lambda (instance)
;       (slot-value-using-class (wrapper-class (get-wrapper instance))
;			       instance
;			       slot-name))))
;
;(define-function-template writer-function--fsc () '(slot-name)
;  `(function
;     (lambda (nv instance)
;       (setf
;	 (slot-value-using-class (wrapper-class (get-wrapper instance))
;				 instance
;				 slot-name)
;	 nv))))
;
;(eval-when (load)
;  (pre-make-templated-function-constructor reader-function--fsc)
;  (pre-make-templated-function-constructor writer-function--fsc))



