;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package 'clos)

(defun get-method-qualifiers (method) (nth 1 method))

(defun get-method-function (method) (nth 4 method))

;;;
;;; Convert an effective method form to a compiled effective method function.
;;; The strategy is to have compiled functions around which are are templates
;;; for effective method functions.  Then the effective method functions we
;;; generate are closures over the particular methods in the effective method
;;; form.  This strategy has the advantage that we don't have to call the
;;; compiler when we combine new methods.  It also has the advantage that
;;; same shape effective methods share the same code vector.  It is of course
;;; predicated on the same assumption PCL makes elsewhere that funcalling
;;; compiled closures is fast.
;;;
;;; *effective-method-templates* is a list of effective-method template
;;; entries.  Each entry is itself a list of the form:
;;; 
;;;   (<template> <match-function> <make-code-function>)
;;;
;;; The match function is usually simple-effective-method-match-p, but is
;;; sometimes complex-effective-method-match-p.  This depends on whether
;;; or not we can use a simple code walker to match the template and the
;;; form.  If we can, it means matching is faster.  We can so long as the
;;; template contains no quoted constants, macros, or macrolet forms.
;;; See the function forces-complex-walker-p for details.  This heuristic
;;; seems to be right, but I don't have a proof for it.  A proof for it
;;; would be nice though.
;;;
;;;

(defvar *effective-method-templates* ())

(defun make-effective-method-function (form)
  (declare (notinline get-method-function))
  (if (and (listp form)
	   (eq (car form) 'CALL-METHOD)
	   (method-p (cadr form))
	   (every #'method-p (caddr form)))
      ;; The effective method is just a call to call-method.  This opens
      ;; up a possibility of just using the method function of the method
      ;; being called as the effective method function.
      ;;
      ;; But we have to be careful.  We must be sure to communicate the
      ;; next methods to the method if it needs them.  If there are no
      ;; next methods we must communicate that fact to prevent the leaky
      ;; next methods bug.
      (let ((method-function (get-method-function (cadr form))))
	(if (method-needs-next-methods-p (cadr form))
	      (let ((next-method-functions
		     (mapcar #'get-method-function (caddr form))))
		#'(lambda (&rest .combined-method-args.)
		    (let ((*next-methods* next-method-functions))
		      (apply method-function .combined-method-args.))))
	  method-function))
    (funcall (get-effective-method-code-constructor form) form)))

(defun get-effective-method-code-constructor (form)
  (let ((entry nil))
    (dolist (e *effective-method-templates*)
      (let ((matchp (funcall (symbol-function (cadr e)) (car e) form)))
	(when matchp (return (setq entry e)))))
    (unless entry
      ;;
      ;; None of the recorded entries match.  Have to generate a new entry.
      (setq entry (compile-effective-method-template-entry form))
      (add-effective-method-template-entry entry))
    (incf (car (cddddr entry)))
    (caddr entry)))

(defun add-effective-method-template-entry (entry)
  ;; We keep the list of entries sorted so that the entries with complex
  ;; match functions stay at the end.  This prevents a newly defined
  ;; complex match function from slowing down all the more common cases.
  ;;
  (setq *effective-method-templates*
	(merge 'list
	       *effective-method-templates*
	       (list entry)
	       #'(lambda (a b)
		   (and (eq a 'simple-effective-method-match-p)
			(not (eq a b))))
	       :key #'cadr)))

(defun compile-effective-method-template-entry (form)
  (multiple-value-bind (template predicate constructor)
      (compile-effective-method-template-entry-internal form)
    (list template predicate (compile () constructor) 'on-the-fly 0)))

(defmacro precompile-effective-method-template-1
	  (effective-method &optional (when 'pre-made))
  (multiple-value-bind (template predicate constructor)
      (compile-effective-method-template-entry-internal effective-method)
      `(eval-when (load)
		  (add-effective-method-template-entry
		    (list ',template
			  ',predicate
			  (function ,constructor)
			  ',when
			  0)))))


(defun compile-effective-method-template-entry-internal (form)
  (let* ((complexp nil)
	 (walked-form
	   (walk-form form
		      nil
		      #'(lambda (f c e)
			  (declare (ignore c e))
			  (when (forces-complex-walker-p form)
			    (setq complexp 't))
			  (if (not (consp f))
			      f
			      (let ((fn (car f)))
				(if (eq fn 'call-method)
				    (if (= (length f) 3)
					'(_call-method_)
					(error "Wrong number of arguments to ~
                                                call-method."))
				    f)))))))
    (if complexp
	(values form
		'complex-effective-method-match-p
		(make-complex-effective-method-code-constructor form))
	(values walked-form
		'simple-effective-method-match-p
		(make-simple-effective-method-code-constructor form)))))

(defun forces-complex-walker-p (form)
  (and (listp form)
       (member (car form) '(quote macrolet flet labels))
       (not (and (fboundp (car form))
		 (not (macro-function (car form)))))))

(defun simple-effective-method-match-p (template form)
  (labels ((every* (fn l1 l2)
	     ;; This version of every is slightly different. It
	     ;; returns NIL if it reaches the end of one of the
	     ;; lists before reaching the end of the other.
	     (do ((t1 l1 (cdr t1))
		  (t2 l2 (cdr t2)))
		 (())
	       (cond ((null t1) (return (null t2)))
		     ((null t2) (return (null t1)))
		     ((funcall fn (car t1) (car t2)))
		     (t (return nil)))))
	   (walk (tm fm)
	     (cond ((eq tm fm) t)
		   ((and (listp tm)
			 (listp fm))
		    (if (eq (car fm) 'call-method)
			(eq (car tm) '_call-method_)
			(every* #'walk tm fm)))
		   ((and (stringp tm)
			 (stringp fm))
		    (string-equal tm fm))
		   (t nil))))
    (walk template form)))

(defun simple-code-walker (form env walk-function)
  (cond ((null form) ())
	((listp form)
	 (catch form
	   (let ((new (funcall walk-function form :eval nil)))
	     (walker::recons
	       new
	       (simple-code-walker (car new) env walk-function)
	       (simple-code-walker (cdr new) env walk-function)))))
	(t form)))

(defun complex-effective-method-match-p (template form)
  (and (simple-effective-method-match-p template form)
       (let ((trail1 ())
	     (trail2 ()))
	 (walk-form template
		    nil
		    #'(lambda (f c e)
			(declare (ignore e))
			(push (cons c f) trail1) f))
	 
	 (walk-form form
		    nil
		    #'(lambda (f c e)
			(declare (ignore e))
			(push (cons c f) trail2) f))
	 (equal trail1 trail2))))

;;;
;;; These two functions must to pass the symbols which name the functions
;;; not the actual functions.  This is done this way because we are going
;;; to have to compile forms which include them as constants.  Before the
;;; symbols are actually applied, symbol function is used to get the actual
;;; function.
;;; 
(defun make-simple-effective-method-code-constructor (form)
  (make-code-constructor 'simple-code-walker form))

#|
(defun make-complex-effective-method-code-constructor (form)
  (make-code-constructor 'walk-form form))
|#

(defvar *combined-method-next-methods-gensyms* ())
(defvar *combined-method-method-function-gensyms* ())

(eval-when (eval load)
  (dotimes (i 10)
    (push (make-symbol (format nil ".METHOD-~A-NEXT-METHODS." (- 9 i)))
	  *combined-method-next-methods-gensyms*)
    (push (make-symbol (format nil ".METHOD-~A-FUNCTION." (- 9 i)))
	  *combined-method-method-function-gensyms*)))
    

(defun make-code-constructor (walker form)
  (let* ((method-vars ())
	 (code-body nil)
	 (next-method-gensyms *combined-method-next-methods-gensyms*)
	 (method-function-gensyms *combined-method-method-function-gensyms*))
    (flet ((convert-function (f c e)
	     (declare (ignore e))
	     (cond ((and (listp f)
			 (eq c ':eval))
		    (if (or (eq (car f) '_call-method_)
			    (eq (car f) 'call-method))
			(let ((gensym1 (or (pop method-function-gensyms)
					   (gensym)))
			      (gensym2 (or (pop next-method-gensyms)
					   (gensym))))
			  (push gensym1 method-vars)
			  (push gensym2 method-vars)
			  `(let ((*next-methods* ,gensym2))
			     (declare (special *next-methods*))
			     (apply ,gensym1 .combined-method-args.)))
			f))
		   ((method-p f)
		    (error "Effective method body must be malformed."))
		   (t f))))
      (setq code-body (funcall walker form nil #'convert-function))
      ;;
      ;; This is written in a slightly screwey way because of a bug in the
      ;; 3600 compiler.  Basically, if both of the funargs in the compiled
      ;; up function close over method-vars the 3600 compiler loses.
      ;; 
      `(lambda (.form.)
	 (let ((methods nil) ,@method-vars)
	   (funcall ',walker
		    .form.
		    nil
		    #'(lambda (f c e)
			(declare (ignore e))
			(declare (notinline get-method-function))
			(if (and (eq c ':eval)
				 (listp f)
				 (eq (car f) 'call-method))
			    (progn 
			      (push (get-method-function (cadr f)) methods)
			      (push (caddr f) methods)
			      (throw f f))
			    f)
			f))
	   ,@(do ((mvs method-vars (cddr mvs))
		  (setqs))
		 ((null mvs) (nreverse setqs))
		 (push `(setq ,(car mvs)
			      (mapcar #'convert-effective-method
				      (pop methods)))
		       setqs)
		 (push `(setq ,(cadr mvs) (pop methods)) setqs))
	   #'(lambda (&rest .combined-method-args.)
	       ,code-body))))))



(defun convert-effective-method (effective-method)
  (declare (notinline get-method-function))
  (cond ((method-p effective-method)
	 (get-method-function effective-method))
	((and (listp effective-method)
	      (eq (car effective-method) 'make-method))
	 (format t "Inside convert-effective-method: ~a" effective-method)
	 (make-effective-method-function
	   (make-progn (cadr effective-method))))
	(t
	 (error "Effective-method form is malformed."))))

(defun make-method-call (method &optional next-methods)
  `(call-method ,method ,next-methods))

(defun make-progn (&rest forms)
  (let ((progn-form nil))
    (labels ((collect-forms (forms)
	       (unless (null forms)
		 (collect-forms (cdr forms))
		 (if (and (listp (car forms))
			  (eq (caar forms) 'progn))
		     (collect-forms (cdar forms))
		     (push (car forms) progn-form)))))
      (collect-forms forms)
      (cons 'progn progn-form))))


(defun compute-combined-method (gf methods)
  (declare (ignore gf) (notinline get-method-qualifiers))
  (let ((before ())
	(primary ())
	(after ())
	(around ()))
    (dolist (m methods)
      (let ((qualifiers (get-method-qualifiers m)))
	(cond ((member ':before qualifiers) (push m before))
	      ((member ':after  qualifiers) (push m after))
	      ((member ':around  qualifiers) (push m around))
	      (t
	       (push m primary)))))
    (setq before (nreverse before)
	  after after
	  primary (nreverse primary)
	  around (nreverse around))

    (make-effective-method-function
     (if (and (null before)
	      (null after))
	 (if (null around)
	     `(call-method ,(car primary) ,(cdr primary))
	     `(call-method ,(car around) ,(append (cdr around) primary)))
	 (let ((main-combined-method
		`(progn ,@(mapcar #'make-method-call before)
		  (multiple-value-prog1
		      ,(if primary
			   `(call-method ,(car primary) ,(cdr primary))
			   '(error "No applicable primary method."))
		    ,@(mapcar #'make-method-call after)))))
	   (if around
	       `(call-method ,(car around)
		 ,(append (cdr around)
			  `((make-method ,main-combined-method))))
	       main-combined-method))))))


