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

(defun null-wrappers-method-function (&rest args)
  ;; Function returned when get-method-function passed no wrappers for
  ;; caching.  I'm not exactly sure why get-method-function gets called
  ;; with null wrappers when a generic function is first created, but
  ;; they do.  However, the method-function returned never seemed to
  ;; get called, so to save a bunch of unneed closure-generation
  ;; and other muckity-muck, this function is just returned instead.
  (error "Internal PCL error:  Calling method-function created by
          get-method-function with wrappers NIL.  Called with args: ~S"
         args))

(defun get-method-function (method &optional method-alist wrappers)
  (or (cadr (assq method method-alist))
      (if wrappers
	  (method-function-for-caching method wrappers)
	  (or (method-optimized-function method)
              #'null-wrappers-method-function))))

(defun make-effective-method-function (generic-function form &optional 
				       method-alist wrappers)
  (funcall-function (make-effective-method-function1 generic-function form)
	            method-alist wrappers))

(defun make-effective-method-function1 (generic-function form)
  (if (and (listp form)
	   (eq (car form) 'call-method)
	   (method-p (cadr form))
	   (or (every #'method-p (caddr form))
	       (not (method-needs-next-methods-p (cadr form)))))
      (make-effective-method-function-simple generic-function form)
      ;;
      ;; We have some sort of `real' effective method.  Go off and get a
      ;; compiled function for it.  Most of the real hair here is done by
      ;; the GET-FUNCTION mechanism.
      ;; 
      (make-effective-method-function-internal generic-function form)))

(defun make-effective-method-function-simple (generic-function form)
  ;;
  ;; The effective method is just a call to call-method.  This opens up
  ;; the possibility of just using the method function of the method as
  ;; as the effective method function.
  ;;
  ;; But we have to be careful.  If that method function will ask for
  ;; the next methods we have to provide them.  We do not look to see
  ;; if there are next methods, we look at whether the method function
  ;; asks about them.  If it does, we must tell it whether there are
  ;; or aren't to prevent the leaky next methods bug.
  ;; 
  (let ((method (cadr form)))
    (if (not (method-needs-next-methods-p method))
	#'(lambda (method-alist wrappers)
	    (get-method-function method method-alist wrappers))
	(let* ((arg-info (gf-arg-info generic-function))
	       (metatypes (arg-info-metatypes arg-info))
	       (applyp (arg-info-applyp arg-info))
	       (next-methods (caddr form)))
	  (declare (type boolean applyp))
	  (multiple-value-bind (cfunction constants)
	      (get-function1
	       `(lambda ,(make-dfun-lambda-list metatypes applyp)
		  (let ((*next-methods* .next-methods.))
		    ,(make-dfun-call metatypes applyp '.method.)))
	       #'default-test-converter ;This could be optimized by making
					;the interface from here to the
					;walker more clear so that the
					;form wouldn't get walked at all.
	       #'(lambda (form)
		   (if (memq form '(.next-methods. .method.))
		       (values form (list form))
		       form))
	       #'(lambda (form)
		   (cond ((eq form '.next-methods.)
			  (list (cons '.meth-list. next-methods)))
			 ((eq form '.method.)
			  (list (cons '.meth. method))))))
	    #'(lambda (method-alist wrappers)
		(flet ((fix-meth (meth)
			 (get-method-function meth method-alist wrappers)))
		  (apply-function cfunction
				  (mapcar #'(lambda (constant)
					      (cond ((atom constant)
						     constant)
						    ((eq (car constant) '.meth.)
						     (fix-meth (cdr constant)))
						    ((eq (car constant) '.meth-list.)
						     (mapcar #'fix-meth (cdr constant)))
						    (t constant)))
					  constants)))))))))

(declaim (type list *global-effective-method-gensyms*))
(defvar *global-effective-method-gensyms* ())
(defvar *rebound-effective-method-gensyms*)

(defun get-effective-method-gensym ()
  (or (pop *rebound-effective-method-gensyms*)
      (let ((new (intern (format nil "EFFECTIVE-METHOD-GENSYM-~D" 
				 (length *global-effective-method-gensyms*))
			 "PCL")))
	(setq *global-effective-method-gensyms*
	      (append *global-effective-method-gensyms* (list new)))
	new)))

(let ((*rebound-effective-method-gensyms* ()))
  (dotimes (i 10) (get-effective-method-gensym)))

(defun make-effective-method-function-internal (generic-function effective-method)
  (let* ((*rebound-effective-method-gensyms* *global-effective-method-gensyms*)
	 (arg-info (gf-arg-info generic-function))
	 (metatypes (arg-info-metatypes arg-info))
	 (applyp (arg-info-applyp arg-info)))
    (declare (type boolean applyp))
    (labels ((test-converter (form)
	       (if (and (consp form) (eq (car form) 'call-method))
		   (if (caddr form)
		       '.call-method-with-next.
		       '.call-method-without-next.)
		   (default-test-converter form)))
	     (code-converter (form)
	       (if (and (consp form) (eq (car form) 'call-method))
		   ;;
		   ;; We have a `call' to CALL-METHOD.  There may or may not be next
		   ;; methods and the two cases are a little different.  It controls
		   ;; how many gensyms we will generate.
		   ;;
		   (let ((gensyms
			  (if (caddr form)
			      (list (get-effective-method-gensym)
				    (get-effective-method-gensym))
			      (list (get-effective-method-gensym)))))
		     (values `(let ((*next-methods* ,(cadr gensyms)))
			       ,(make-dfun-call metatypes applyp (car gensyms)))
			     gensyms))
		   (default-code-converter form)))
	     (constant-converter (form)
	       (if (and (consp form) (eq (car form) 'call-method))
		   (if (caddr form)
		       (list (cons '.meth. (check-for-make-method (cadr form)))
			     (cons '.meth-list.
				   (mapcar #'check-for-make-method (caddr form))))
		       (list (cons '.meth. (check-for-make-method (cadr form)))))
		   (default-constant-converter form)))
	     (check-for-make-method (effective-method)
	       (cond ((method-p effective-method)
		      effective-method)
		     ((and (listp effective-method)
			   (eq (car effective-method) 'make-method))
		      (make-effective-method-function1
		       generic-function
		       (make-progn (cadr effective-method))))
		     (t
		      (error "Effective-method form is malformed.")))))
      (multiple-value-bind (cfunction constants)
	  (get-function1 `(lambda ,(make-dfun-lambda-list metatypes applyp)
			   ,effective-method)
			 #'test-converter
			 #'code-converter
			 #'constant-converter)
	#'(lambda (method-alist wrappers)
	    (flet ((fix-meth (meth)
		     (if (method-p meth)
			 (get-method-function meth method-alist wrappers)
			 (funcall-function meth method-alist wrappers))))
	      (apply-function cfunction
			      (mapcar #'(lambda (constant)
					  (cond ((atom constant)
						 constant)
						((eq (car constant) '.meth.)
						 (fix-meth (cdr constant)))
						((eq (car constant) '.meth-list.)
						 (mapcar #'fix-meth (cdr constant)))
						(t constant)))
				      constants))))))))



(defvar *invalid-method-error*
	#'(lambda (&rest args)
	    (declare (ignore args))
	    (error
	      "INVALID-METHOD-ERROR was called outside the dynamic scope~%~
               of a method combination function (inside the body of~%~
               DEFINE-METHOD-COMBINATION or a method on the generic~%~
               function COMPUTE-EFFECTIVE-METHOD).")))

(defvar *method-combination-error*
	#'(lambda (&rest args)
	    (declare (ignore args))
	    (error
	      "METHOD-COMBINATION-ERROR was called outside the dynamic scope~%~
               of a method combination function (inside the body of~%~
               DEFINE-METHOD-COMBINATION or a method on the generic~%~
               function COMPUTE-EFFECTIVE-METHOD).")))

;(defmethod compute-effective-method :around        ;issue with magic
;	   ((generic-function generic-function)     ;generic functions
;	    (method-combination method-combination)
;	    applicable-methods)
;  (declare (ignore applicable-methods))
;  (flet ((real-invalid-method-error (method format-string &rest args)
;	   (declare (ignore method))
;	   (apply #'error format-string args))
;	 (real-method-combination-error (format-string &rest args)
;	   (apply #'error format-string args)))
;    (let ((*invalid-method-error* #'real-invalid-method-error)
;	  (*method-combination-error* #'real-method-combination-error))
;      (call-next-method))))

(defun invalid-method-error (&rest args)
  (declare (arglist method format-string &rest format-arguments))
  (apply *invalid-method-error* args))

(defun method-combination-error (&rest args)
  (declare (arglist format-string &rest format-arguments))
  (apply *method-combination-error* args))



;;;
;;; The STANDARD method combination type.  This is coded by hand (rather than
;;; with define-method-combination) for bootstrapping and efficiency reasons.
;;; Note that the definition of the find-method-combination-method appears in
;;; the file defcombin.lisp, this is because EQL methods can't appear in the
;;; bootstrap.
;;;
;;; The defclass for the METHOD-COMBINATION and STANDARD-METHOD-COMBINATION
;;; classes has to appear here for this reason.  This code must conform to
;;; the code in the file defcombin, look there for more details.
;;;

(defclass method-combination (metaobject) ()
  (:predicate-name method-combination-p))


(mapc
 #'proclaim-incompatible-superclasses
 '(;; superclass metaobject
   (class eql-specializer class-eq-specializer method method-combination
    generic-function slot-definition)
   ))

(defclass standard-method-combination
	  (documentation-mixin definition-source-mixin method-combination)
     ((type          :reader method-combination-type
	             :initarg :type)
      (options       :reader method-combination-options
	             :initarg :options)))

(defmethod print-object ((mc method-combination) stream)
  (printing-random-thing (mc stream)
    (format stream
	    "Method-Combination ~S ~S"
	    (method-combination-type mc)
	    (method-combination-options mc))))

(eval-when (load eval)
  (setq *standard-method-combination*
	(make-instance 'standard-method-combination
		       :type 'standard
		       :documentation "The standard method combination."
		       :options ())))

;This definition appears in defcombin.lisp.
;
;(defmethod find-method-combination ((generic-function generic-function)
;				     (type (eql 'standard))
;				     options)
;  (when options
;    (method-combination-error
;      "The method combination type STANDARD accepts no options."))
;  *standard-method-combination*)

(defun make-call-methods (methods)
  (mapcar #'(lambda (method) `(call-method ,method ())) methods))

(defmethod compute-effective-method ((generic-function generic-function)
				     (combin standard-method-combination)
				     applicable-methods)
  (let ((before ())
	(primary ())
	(after ())
	(around ()))
    (dolist (m applicable-methods)
      (let ((qualifiers (method-qualifiers m)))
	(cond ((memq ':before qualifiers)  (push m before))
	      ((memq ':after  qualifiers)  (push m after))
	      ((memq ':around  qualifiers) (push m around))
	      (t
	       (push m primary)))))
    (setq before  (reverse before)
	  after   (reverse after)
	  primary (reverse primary)
	  around  (reverse around))
    (cond ((null primary)
	   `(error "No primary method for the generic function ~S." ',generic-function))
	  ((and (null before) (null after) (null around))
	   ;;
	   ;; By returning a single call-method `form' here we enable an important
	   ;; implementation-specific optimization.
	   ;; 
	   `(call-method ,(first primary) ,(rest primary)))
	  (t
	   (let ((main-effective-method
		   (if (or before after)
		       `(multiple-value-prog1
			  (progn ,@(make-call-methods before)
				 (call-method ,(first primary) ,(rest primary)))
			  ,@(make-call-methods (reverse after)))
		       `(call-method ,(first primary) ,(rest primary)))))
	     (if around
		 `(call-method ,(first around)
			       (,@(rest around) (make-method ,main-effective-method)))
		 main-effective-method))))))

