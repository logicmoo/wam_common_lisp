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
;;; The version of low for VAXLisp
;;; 
(in-package 'pcl)

(defmacro without-interrupts (&body body)
  `(macrolet ((interrupts-on  ()
	       `(when (null outer-scheduling-state)
		  (setq system::*critical-section-p* nil)
		  (when (system::%sp-interrupt-queued-p)
		    (system::interrupt-dequeuer t))))
	      (interrupts-off ()
	       `(setq system::*critical-section-p* t)))
     (let ((outer-scheduling-state system::*critical-section-p*))
       (prog1 (let ((system::*critical-section-p* t)) ,@body)
	      (when (and (null outer-scheduling-state)
			 (system::%sp-interrupt-queued-p))
		(system::interrupt-dequeuer t))))))


  ;;   
;;;;;; Load Time Eval
  ;;
(defmacro load-time-eval (form)
  `(progn ,form))

  ;;   
;;;;;; Generating CACHE numbers
  ;;
;;; How are symbols in VAXLisp actually arranged in memory?
;;; Should we be shifting the address?
;;; Are they relocated?
;;; etc.

;(defmacro symbol-cache-no (symbol mask)
;  `(logand (the fixnum (system::%sp-pointer->fixnum ,symbol)) ,mask))

(defmacro object-cache-no (object mask)
  `(logand (the fixnum (system::%sp-pointer->fixnum ,object)) ,mask))

  ;;   
;;;;;; printing-random-thing-internal
  ;;
(defun printing-random-thing-internal (thing stream)
  (format stream "~O" (system::%sp-pointer->fixnum thing)))


(defun function-arglist (fn)
  (system::function-lambda-vars (symbol-function fn)))

(defun set-function-name-1 (fn name ignore)
  (cond ((system::slisp-compiled-function-p fn)
	 (system::%sp-b-store fn 3 name)))
  fn)

