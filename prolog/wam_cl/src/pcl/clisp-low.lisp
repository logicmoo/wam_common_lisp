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
;;; The version of low for CLISP.

(in-package 'pcl)

(defun printing-random-thing-internal (thing stream)
  (format stream "#x~8,'0X" (sys::address-of thing))
)

(defconstant *slot-unbound* '..slot-unbound..)

(defsetf sys::%record-ref sys::%record-store)

(defun function-arglist (function)
  (if (sys::closurep function)
    (let ((h (sys::%record-ref function 1))) ; lambdabody or code-vector
      (if (consp h)
        (car h) ; lambda list
        nil ; unknown
    ) )
    nil ; unknown
) )

(defun function-pretty-arglist (function)
  (function-arglist function)
)

(defsetf function-pretty-arglist set-function-pretty-arglist)

(defun set-function-pretty-arglist (function new-value)
  (if (sys::closurep function)
    (let ((h (sys::%record-ref function 1))) ; lambdabody or code-vector
      (if (consp h)
        (setf (car h) new-value) ; replace lambda list
    ) )
  )
  new-value
)

(defun set-function-name-1 (function new-name uninterned-name)
  (declare (ignore uninterned-name))
  (if (sys::closurep function)
    (setf (sys::%record-ref function 0) new-name)
  )
  function
)

(defconstant *compiler-present-p* (if (member 'COMPILER *features*) t nil))

(defvar *compiler-speed* :SLOW)

(defvar *compiler-reentrant-p* t) ; ??

(defun in-the-compiler-p () sys::*compiling*)

(defun compile-lambda-uncompiled (uncompiled)
  (eval `(function ,uncompiled))
)
