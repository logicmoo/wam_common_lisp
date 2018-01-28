;;;-*-Mode:LISP; Package: PCL; Base:10; Syntax:Common-lisp -*-
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
;;; GET-FUNCTION is the main user interface to this code.  If it is called
;;; with a lambda expression only, it will return a corresponding function.
;;; The optional constant-converter argument, can be a function which will
;;; be called to convert each constant appearing in the lambda to whatever
;;; value should appear in the function.
;;;
;;; Whether the returned function is actually compiled depends on whether
;;; the compiler is present (see COMPILE-LAMBDA) and whether this shape of
;;; code was precompiled.
;;; 
(defun get-function (lambda
		      &optional (test-converter     #'default-test-converter)
		                (code-converter     #'default-code-converter)
				(constant-converter #'default-constant-converter))
  (apply-function (get-function-generator lambda test-converter code-converter)
		  (compute-constants      lambda constant-converter)))

(declaim (ftype (function (T &optional T T T) (values function T))
                get-function1))
(defun get-function1 (lambda
		      &optional (test-converter     #'default-test-converter)
		                (code-converter     #'default-code-converter)
				(constant-converter #'default-constant-converter))
  (values (the function
               (get-function-generator lambda test-converter code-converter))
	  (compute-constants      lambda constant-converter)))

(defun default-constantp (form)
  ;; Replace constants by reference .constant. to stop similar code from
  ;; being compiled in the future?
  (and (constantp form)
       (if (eq *compiler-speed* :slow)
           (not (symbolp (eval form)))
           (not (typep (eval form) '(or symbol fixnum))))))

(defun default-test-converter (form)
  (if (default-constantp form)
      '.constant.
      form))

(declaim (ftype (function (T) (values T list)) default-code-converter))
(defun default-code-converter  (form)
  (if (default-constantp form)
      (let ((gensym (gensym))) (values gensym (list gensym)))
      (values form nil)))

(defun default-constant-converter (form)
  (if (default-constantp form)
      (list (eval form))
      nil))


;;;
;;; *fgens* is a list of all the function generators we have so far.  Each 
;;; element is a FGEN structure as implemented below.  Don't ever touch this
;;; list by hand, use STORE-FGEN.
;;;
(defvar *fgens* ())

(defun store-fgen (fgen)
  (setq *fgens* (nconc *fgens* (list fgen))))

(defun lookup-fgen (test)
  (find test (the list *fgens*) :key #'fgen-test :test #'equal))

(defun make-fgen (test gensyms generator generator-lambda system)
  (let ((new (make-array 6)))
    (setf (svref new 0) test
	  (svref new 1) gensyms
	  (svref new 2) generator
	  (svref new 3) generator-lambda
	  (svref new 4) system)
    new))

(defun fgen-test             (fgen) (svref fgen 0))
(defun fgen-gensyms          (fgen) (svref fgen 1))
(defun fgen-generator        (fgen) (svref fgen 2))
(defun fgen-generator-lambda (fgen) (svref fgen 3))
(defun fgen-system           (fgen) (svref fgen 4))



(defun get-function-generator (lambda test-converter code-converter)
  (let* ((test (compute-test lambda test-converter))
	 (fgen (lookup-fgen test)))
    (if fgen
	(fgen-generator fgen)
	(get-new-function-generator lambda test code-converter))))

(declaim (ftype (function (T T) (values T list))
                get-new-function-generator-internal
                compute-code))

(defun get-new-function-generator (lambda test code-converter)
  (multiple-value-bind (gensyms generator-lambda)
      (get-new-function-generator-internal lambda code-converter)
    (let* ((generator (compile-lambda generator-lambda))
	   (fgen (make-fgen test gensyms generator generator-lambda nil)))
      (store-fgen fgen)
      generator)))

(defun get-new-function-generator-internal (lambda code-converter)
  (multiple-value-bind (code gensyms)
      (compute-code lambda code-converter)
    (values gensyms `(lambda ,gensyms (function ,code)))))


(defun compute-test (lambda test-converter)
  (let ((walk-form-expand-macros-p t))
    (walk-form lambda
	       nil
	       #'(lambda (f c e)
		   (declare (ignore e))
		   (if (neq c :eval)
		       f
		       (let ((converted (funcall test-converter f)))
			 (values converted (neq converted f))))))))

(defun compute-code (lambda code-converter)
  (let ((walk-form-expand-macros-p t)
	(gensyms ()))
    (values (walk-form lambda
		       nil
		       #'(lambda (f c e)
			   (declare (ignore e))
			   (if (neq c :eval)
			       f
			       (multiple-value-bind (converted gens)
				   (funcall code-converter f)
				 (when gens (setq gensyms (append gensyms gens)))
				 (values converted (neq converted f))))))
	      gensyms)))

(defun compute-constants (lambda constant-converter)
  (let ((walk-form-expand-macros-p t)) ; doesn't matter here.
    (macrolet ((appending ()
		 `(let ((result ()))
		   (values #'(lambda (value) (setq result (append result value)))
		    #'(lambda ()result)))))
      (gathering1 (appending)
		  (walk-form lambda
			     nil
			     #'(lambda (f c e)
				 (declare (ignore e))
				 (if (neq c :eval)
				     f
				     (let ((consts (funcall constant-converter f)))
				       (if consts
					   (progn (gather1 consts) (values f t))
					   f)))))))))


;;;
;;;
;;;
(defmacro precompile-function-generators (&optional system)
  (make-top-level-form `(precompile-function-generators ,system)
		       '(load)
    `(progn ,@(gathering1 (collecting)
		(dolist (fgen *fgens*)
		  (when (or (null (fgen-system fgen))
			    (eq (fgen-system fgen) system))
		    (when system (setf (svref fgen 4) system))
		    (gather1
		     `(load-function-generator
		       ',(fgen-test fgen)
		       ',(fgen-gensyms fgen)
		       (function ,(fgen-generator-lambda fgen))
		       ',(fgen-generator-lambda fgen)
		       ',system))))))))

(defun load-function-generator (test gensyms generator generator-lambda system)
  (store-fgen (make-fgen test gensyms generator generator-lambda system)))

