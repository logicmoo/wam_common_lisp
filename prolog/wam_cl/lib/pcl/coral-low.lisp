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

(in-package 'pcl)

#-(or :ccl-1.3 :cltl2)
(ccl::add-transform 'std-instance-p 
                     :inline 
                     #'(lambda (call)
                         (ccl::verify-arg-count call 1 1)
                         (let ((arg (cadr call)))
                           `(and (eq (ccl::%type-of ,arg) 'structure)
                                 (eq (%svref ,arg 0) 'std-instance)))))

(eval-when (eval compile load)
  (proclaim '(inline std-instance-p)))
#-:cltl2
(defun printing-random-thing-internal (thing stream)
  (prin1 (ccl::%ptr-to-int thing) stream))
#+:cltl2
(defun printing-random-thing-internal (thing stream)
  (prin1 (ccl::%address-of thing) stream))

(defun set-function-name-1 (function new-name uninterned-name)
  (declare (ignore uninterned-name))
  (cond ((ccl::lfunp function)
         (ccl::lfun-name function new-name)))
  function)


(defun doctor-dfun-for-the-debugger (gf dfun)
  #+:ccl-1.3
  (let* ((gfspec (and (symbolp (generic-function-name gf))
		      (generic-function-name gf)))
	 (arglist (generic-function-pretty-arglist gf)))
    (when gfspec
      (setf (get gfspec 'ccl::%lambda-list)
	    (if (and arglist (listp arglist))
		(format nil "~{~A~^ ~}" arglist)
		(format nil "~:A" arglist)))))
  dfun)

