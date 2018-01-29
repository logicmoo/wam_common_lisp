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

(in-package 'pcl)

;;;
;;; This little bit of magic keeps the dumper from dumping the lexical
;;; definition of call-next-method when it dumps method functions that
;;; come from defmethod forms.
;;; 
(proclaim '(notinline nil))

(eval-when (load)
  (setf (get 'function 'si:type-predicate) 'functionp))

;; fix defsetf to deal with do-standard-defsetf

#!C
; From file SETF.LISP#> KERNEL; VIRGO:
#8R SYSTEM#:
(COMPILER-LET ((*PACKAGE* (FIND-PACKAGE "SYSTEM"))
                          (SI:*LISP-MODE* :COMMON-LISP)
                          (*READTABLE* COMMON-LISP-READTABLE)
                          (SI:*READER-SYMBOL-SUBSTITUTIONS* *COMMON-LISP-SYMBOL-SUBSTITUTIONS*))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: KERNEL; SETF.#"


(defmacro defsetf (access-function arg1 &optional arg2  &environment env &body body)
  "Define a SETF expander for ACCESS-FUNCTION.
DEFSETF has two forms:

The simple form  (DEFSETF access-function update-function [doc-string])
can be used as follows: After (DEFSETF GETFROB PUTFROB),
\(SETF (GETFROB A 3) FOO) ==> (PUTFROB A 3 FOO).

The complex form is like DEFMACRO:

\(DEFSETF access-function access-lambda-list newvalue-lambda-list body...)

except there are TWO lambda-lists.
The first one represents the argument forms to the ACCESS-FUNCTION.
Only &OPTIONAL and &REST are allowed here.
The second has only one argument, representing the value to be stored.
The body of the DEFSETF definition must then compute a
replacement for the SETF form, just as for any other macro.
When the body is executed, the args in the lambda-lists will not
really contain the value-expression or parts of the form to be set;
they will contain gensymmed variables which SETF may or may not
eliminate by substitution."
  ;; REF and VAL are arguments to the expansion function
  (if (null body)
      `(defdecl ,access-function setf-method ,arg1)
      (multiple-value-bind (body decls doc-string)
	  (parse-body body env t)
	(let* ((access-ll arg1)
	       (value-names arg2)
	       (expansion
		 (let (all-arg-names)
		   (dolist (x access-ll)
		     (cond ((symbolp x)
			    (if (not (member x lambda-list-keywords :test #'eq))
				(push x all-arg-names)
				(when (eq x '&rest) (return))))  ;;9/20/88 clm
			   (t			; it's a list after &optional
			    (push (car x) all-arg-names))))
		   (setq all-arg-names (reverse all-arg-names))
		   `(let ((tempvars (mapcar #'(lambda (ignore) (gensym)) ',all-arg-names))
			  (storevar (gensym)))
		      (values tempvars (list . ,all-arg-names) (list storevar)
			      (let ((,(car value-names) storevar)
				    . ,(loop for arg in all-arg-names
					     for i = 0 then (1+ i)
					     collect `(,arg (nth ,i tempvars))))
				 ,@decls . ,body)
			      `(,',access-function . ,tempvars))))))
	  `(define-setf-method ,access-function ,arg1
	    ,@doc-string ,expansion)
	  ))))
))


