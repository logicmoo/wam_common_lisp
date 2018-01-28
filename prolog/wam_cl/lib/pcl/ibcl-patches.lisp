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

(in-package 'system)

;;;   This makes DEFMACRO take &WHOLE and &ENVIRONMENT args anywhere
;;;   in the lambda-list.  The former allows deviation from the CL spec,
;;;   but what the heck.

(eval-when (compile) (proclaim '(optimize (safety 2) (space 3))))

(defvar *old-defmacro*)

(defun new-defmacro (whole env)
  (flet ((call-old-definition (new-whole)
	   (funcall *old-defmacro* new-whole env)))
    (if (not (and (consp whole)
		  (consp (cdr whole))
		  (consp (cddr whole))
		  (consp (cdddr whole))))
	(call-old-definition whole)
	(let* ((ll (caddr whole))
	       (env-tail (do ((tail ll (cdr tail)))
			     ((not (consp tail)) nil)
			   (when (eq '&environment (car tail))
			     (return tail)))))
	  (if env-tail
	      (call-old-definition (list* (car whole)
					  (cadr whole)
					  (append (list '&environment
							(cadr env-tail))
						  (ldiff ll env-tail)
						  (cddr env-tail))
					  (cdddr whole)))
	      (call-old-definition whole))))))

(eval-when (load eval)
  (unless (boundp '*old-defmacro*)
    (setq *old-defmacro* (macro-function 'defmacro))
    (setf (macro-function 'defmacro) #'new-defmacro)))

;;;
;;; setf patches
;;;

(in-package 'system)

(defun get-setf-method (form)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method-multiple-value form)
    (unless (listp vars)
	    (error 
 "The temporary variables component, ~s, 
  of the setf-method for ~s is not a list."
             vars form))
    (unless (listp vals)
	    (error 
 "The values forms component, ~s, 
  of the setf-method for ~s is not a list."
             vals form))
    (unless (listp stores)
	    (error 
 "The store variables component, ~s,  
  of the setf-method for ~s is not a list."
             stores form))
    (unless (= (list-length stores) 1)
	    (error "Multiple store-variables are not allowed."))
    (values vars vals stores store-form access-form)))

(defun get-setf-method-multiple-value (form)
  (cond ((symbolp form)
	 (let ((store (gensym)))
	   (values nil nil (list store) `(setq ,form ,store) form)))
	((or (not (consp form)) (not (symbolp (car form))))
	 (error "Cannot get the setf-method of ~S." form))
	((get (car form) 'setf-method)
	 (apply (get (car form) 'setf-method) (cdr form)))
	((get (car form) 'setf-update-fn)
	 (let ((vars (mapcar #'(lambda (x)
	                         (declare (ignore x))
	                         (gensym))
	                     (cdr form)))
	       (store (gensym)))
	   (values vars (cdr form) (list store)
	           `(,(get (car form) 'setf-update-fn)
		     ,@vars ,store)
		   (cons (car form) vars))))
	((get (car form) 'setf-lambda)
	 (let* ((vars (mapcar #'(lambda (x)
	                          (declare (ignore x))
	                          (gensym))
	                      (cdr form)))
		(store (gensym))
		(l (get (car form) 'setf-lambda))
		(f `(lambda ,(car l) 
		      (funcall #'(lambda ,(cadr l) ,@(cddr l))
			       ',store))))
	   (values vars (cdr form) (list store)
		   (apply f vars)
		   (cons (car form) vars))))
	((macro-function (car form))
	 (get-setf-method-multiple-value (macroexpand-1 form)))
	(t
	 (error "Cannot expand the SETF form ~S." form))))

