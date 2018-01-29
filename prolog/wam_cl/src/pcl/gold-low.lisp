;;;-*-Mode:LISP; Package:(PCL Lisp 1000); Base:10; Syntax:Common-lisp -*-
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
;;; 

(in-package 'pcl)

;;; fix a bug in gcl macro-expander (or->cond->or->cond->...)
(setf (get 'cond 'lisp::macro-expander) nil)

;;; fix another bug in gcl3_0 case macro-expander
(defun lisp::eqv (a b) (eql a b))

(defun printing-random-thing-internal (thing stream)
  (multiple-value-bind (offaddr baseaddr)
      (sys:%pointer thing)
    (princ baseaddr stream)
    (princ ", " stream)
    (princ offaddr stream)))

;;;
;;; This allows the compiler to compile a file with many "DEFMETHODS"
;;; in succession.
;;;
(dolist (x '(defmethod defgeneric defclass precompile-random-code-segments))
  (setf (get x 'gcl::compile-separately) t))

