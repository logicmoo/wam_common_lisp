;;; -*- Mode:LISP; Package:(PCL (Lisp WALKER)); Base:10.; Syntax:Common-lisp; Patch-File: Yes -*-
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
;;; This is the 3600 version of the file portable-low.
;;;

(in-package 'pcl)

(defmacro without-interrupts (&body body)
  `(let ((outer-scheduling-state si:inhibit-scheduling-flag)
	 (si:inhibit-scheduling-flag t))
     (macrolet ((interrupts-on  ()
		  '(when (null outer-scheduling-state)
		     (setq si:inhibit-scheduling-flag nil)))
		(interrupts-off ()
		  '(setq si:inhibit-scheduling-flag t)))
       ,.body)))

(si:defsubst std-instance-p (x)
  (si:typep-structure-or-flavor x 'std-instance))

  ;;   
;;;;;; printing-random-thing-internal
  ;;
(defun printing-random-thing-internal (thing stream)
  (format stream "~O" (si:%pointer thing)))

(eval-when (compile load eval)             ;There seems to be some bug with
  (setq si::inhibit-displacing-flag t))	   ;macrolet'd macros or something.
					   ;This gets around it but its not
					   ;really the right fix.

(defun function-arglist (f)
  (sys::arglist f t))

(defun record-definition (type spec &rest ignore)
  (if (eql type 'method)
      (sys:record-source-file-name spec 'defun :no-query)
      (sys:record-source-file-name spec type :no-query)))

(ticl:defprop method method-function-spec-handler sys:function-spec-handler)
(defun method-function-spec-handler
       (function function-spec &optional arg1 arg2)
  (let ((symbol (second function-spec)))
    (case function
      (sys:validate-function-spec t)
      (otherwise
	(sys:function-spec-default-handler
	  function function-spec arg1 arg2)))))

;;;Edited by Reed Hastings         13 Aug 87  16:59
;;;Edited by Reed Hastings         2 Nov 87  22:58
(defun set-function-name (function new-name)
  (when (si:get-debug-info-struct function)
    (setf (si:get-debug-info-field (si:get-debug-info-struct function) :name)
	  new-name))
  function)



