;;; -*- Mode:LISP; Package:(PCL Lisp 1000); Base:10.; Syntax:Common-lisp; Patch-File: Yes -*-
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

(pushnew ':pcl-internals dbg:*all-invisible-frame-types*)

#+IMach						;On the I-Machine these are
(eval-when (compile load eval)			;faster than the versions
						;that use :test #'eq.  
(defmacro memq (item list) `(member ,item ,list))
(defmacro assq (item list) `(assoc ,item ,list))
(defmacro rassq (item list) `(rassoc ,item ,list))
(defmacro delq (item list) `(delete ,item ,list))
(defmacro posq (item list) `(position ,item ,list))

)

compiler::
(defoptimizer (cl:the the-just-gets-in-the-way-of-optimizers) (form)
  (matchp form
    (('cl:the type subform)
     (ignore type)
     subform)
    (* form)))

(defmacro %ash (x count)
  (if (and (constantp count) (zerop (eval count)))
      x
      `(the fixnum (ash (the fixnum ,x ) ,count))))

;;;
;;;
;;;

(defmacro without-interrupts (&body body)
  `(let ((outer-scheduling-state si:inhibit-scheduling-flag)
	 (si:inhibit-scheduling-flag t))
     (macrolet ((interrupts-on  ()
		  '(when (null outer-scheduling-state)
		     (setq si:inhibit-scheduling-flag nil)))
		(interrupts-off ()
		  '(setq si:inhibit-scheduling-flag t)))
       (progn outer-scheduling-state)
       ,.body)))

;;;
;;; It would appear that #, does not work properly in Genera.  At least I can't get it
;;; to work when I use it inside of std-instance-p (defined later in this file).  So,
;;; all of this is just to support that.
;;;
;;;     WHEN                       EXPANDS-TO
;;;   compile to a file          (#:EVAL-AT-LOAD-TIME-MARKER . <form>)
;;;   compile to core            '<result of evaluating form>
;;;   not in compiler at all     (progn <form>)
;;;
;;; Believe me when I tell you that I don't know why it is I need both a
;;; transformer and an optimizer to get this to work.  Believe me when I
;;; tell you that I don't really care why either.
;;;
(defmacro load-time-eval (form)
  ;; The interpreted definition of load-time-eval.  This definition
  ;; never gets compiled.
  (let ((value (gensym)))
    `(multiple-value-bind (,value)
	 (progn ,form)
       ,value)))

(compiler:deftransformer (load-time-eval optimize-load-time-eval) (form)
  (compiler-is-a-loser-internal form))

(compiler:defoptimizer (load-time-eval transform-load-time-eval) (form)
  (compiler-is-a-loser-internal form))

(defun compiler-is-a-loser-internal (form)  
  ;; When compiling a call to load-time-eval the compiler will call
  ;; this optimizer before the macro expansion.
  (if zl:compiler:(and (boundp '*compile-function*)	;Probably don't need
						        ;this boundp check
						        ;but it can't hurt.
		       (funcall *compile-function* :to-core-p))
      ;; Compiling to core.
      ;; Evaluate the form now, and expand into a constant
      ;; (the result of evaluating the form).
      `',(eval (cadr form))
      ;; Compiling to a file.
      ;; Generate the magic which causes the dumper compiler and loader
      ;; to do magic and evaluate the form at load time.
      `',(cons compiler:eval-at-load-time-marker (cadr form))))

;;   
;;;;;; Memory Block primitives.                ***
  ;;   


(defmacro make-memory-block (size &optional area)
  `(make-array ,size :area ,area))

(defmacro memory-block-ref (block offset)	;Don't want to go faster yet.
  `(aref ,block ,offset))

(defvar class-wrapper-area)
(eval-when (load eval)
  (si:make-area :name 'class-wrapper-area
		:room t
		:gc :static))

(eval-when (compile load eval)
  (remprop '%%allocate-instance--class 'inline))

(eval-when (compile load eval)
  
(scl:defflavor std-instance
	((wrapper nil)
	 (slots   nil))
	()
  (:constructor %%allocate-instance--class())
  :ordered-instance-variables)

(defvar *std-instance-flavor* (flavor:find-flavor 'std-instance))

)

#-imach
(scl:defsubst pcl-%instance-flavor (instance)
  (declare (compiler:do-not-record-macroexpansions))
  (sys::%make-pointer sys:dtp-array
		      (sys:%p-contents-as-locative
			(sys:follow-structure-forwarding instance))))

#+imach
(scl:defsubst pcl-%instance-flavor (instance)
  (sys:%instance-flavor instance))

(scl::defsubst std-instance-p (x)
  (and (sys:instancep x)
       (eq (pcl-%instance-flavor x) (load-time-eval *std-instance-flavor*))))

(scl:defmethod (:print-self std-instance) (stream depth slashify)
  (declare (ignore slashify))
  (print-std-instance scl:self stream depth))

(scl:defmethod (:describe std-instance) ()
  (describe-object scl:self *standard-output*))

(defmacro %std-instance-wrapper (std-instance)
  `(sys:%instance-ref ,std-instance 1))

(defmacro %std-instance-slots (std-instance)
  `(sys:%instance-ref ,std-instance 2))

(scl:compile-flavor-methods std-instance)


(defun printing-random-thing-internal (thing stream)
  (format stream "~\\si:address\\" (si:%pointer thing)))

;;;
;;; This is hard, I am sweating.
;;; 
(defun function-arglist (function) (zl:arglist function t))

(defun function-pretty-arglist (function) (zl:arglist function))


;; New (& complete) fspec handler.
;;   1. uses a single #'equal htable where stored elements are (fn . plist)
;;       (maybe we should store the method object instead)
;;   2. also implements the fspec-plist operators here.
;;   3. fdefine not only stores the method, but actually does the loading here!
;;

;;;
;;;  genera-low.lisp (replaces old method-function-spec-handler)
;;;

;; New (& complete) fspec handler.
;;   1. uses a single #'equal htable where stored elements are (fn . plist)
;;       (maybe we should store the method object instead)
;;   2. also implements the fspec-plist operators here.
;;   3. fdefine not only stores the method, but actually does the loading here!
;;

(defvar *method-htable* (make-hash-table :test #'equal :size 500))
(sys:define-function-spec-handler method (op spec &optional arg1 arg2)
  (if (eq op 'sys:validate-function-spec)
      (and (let ((gspec (cadr spec)))
	     (or (symbolp gspec)
		 (and (listp gspec)
		      (eq (car gspec) 'setf)
		      (symbolp (cadr gspec))
		      (null (cddr gspec)))))
	   (let ((tail (cddr spec)))
	     (loop (cond ((null tail) (return nil))
			 ((listp (car tail)) (return t))
			 ((atom (pop tail)))			 
			 (t (return nil))))))
      (let ((table *method-htable*)
	    (key spec))
	(case op
	  ((si:fdefinedp si:fdefinition)
	   (car (gethash key table nil)))
	  (si:fundefine
	    (remhash key table))
	  (si:fdefine
	    (let ((old (gethash key table nil))
		  (quals nil)
		  (specs nil)
		  (ptr (cddr spec)))
	      (setq specs
		    (loop (cond ((null ptr) (return nil))
				((listp (car ptr)) (return (car ptr)))
				(t (push (pop ptr) quals)))))
	      (setf (gethash key table) (cons arg1 (cdr old)))))
	  (si:get
	    (let ((old (gethash key table nil)))
	      (getf (cdr old) arg1)))
	  (si:plist
	    (let ((old (gethash key table nil)))
	      (cdr old)))
	  (si:putprop
	    (let ((old (gethash key table nil)))
	      (unless old
		(setf old (cons nil nil))
		(setf (gethash key table) old))
	      (setf (getf (cdr old) arg2) arg1)))
	  (si:remprop
	    (let ((old (gethash key table nil)))
	      (when old
		(remf (cdr old) arg1))))
	  (otherwise
	    (si:function-spec-default-handler op spec arg1 arg2))))))


#||
;; this guy is just a stub to make the fspec handler simpler (and so I could trace it
;; easier).
(defun pcl-fdefine-helper (gspec qualifiers specializers fn)
  (let* ((dlist (scl:debugging-info fn))
	 (class (cadr (assoc 'pcl-method-class dlist)))
	 (lambda-list (let ((ll-stuff (assoc 'pcl-lambda-list dlist)))
			(if ll-stuff (cadr ll-stuff) (arglist fn))))
	 (doc (cadr (assoc 'pcl-documentation dlist)))
	 (plist (cadr (assoc 'pcl-plist dlist))))
    (load-defmethod (or class 'standard-method)
		    gspec
		    qualifiers
		    specializers
		    lambda-list
		    doc
		    (getf plist :isl-cache-symbol)
		    plist
		    fn)))
||#

;; define a few special declarations to get pushed onto the function's debug-info
;; list... note that we do not need to do a (proclaim (declarations ...)) here.
;;
(eval-when (compile load eval)
  (setf (get 'pcl-plist 'si:debug-info) t)
  (setf (get 'pcl-documentation 'si:debug-info) t)
  (setf (get 'pcl-method-class 'si:debug-info) t)
  (setf (get 'pcl-lambda-list 'si:debug-info) t)
)

(eval-when (load eval)
  (setf
    (get 'defmethod      'zwei:definition-function-spec-type) 'defun
    (get 'defmethod-setf 'zwei:definition-function-spec-type) 'defun
    (get 'method 'si:definition-type-name) "method"
    (get 'method 'si:definition-type-name) "method"

    (get 'declass 'zwei:definition-function-spec-type) 'defclass
    (get 'defclass 'si:definition-type-name) "Class"
    (get 'defclass 'zwei:definition-function-spec-finder-template) '(0 1))
  )



(defun (:property defmethod zwei::definition-function-spec-parser) (bp)
  (zwei:parse-pcl-defmethod-for-zwei bp nil))

;;;
;;; Previously, if a source file in a PCL-based package contained what looks
;;; like flavor defmethod forms (i.e. an (IN-PACKAGE 'non-pcl-package) form
;;; appears at top level, and then a flavor-style defmethod form) appear, the
;;; parser would break.
;;;
;;; Now, if we can't parse the defmethod form, we send it to the flavor
;;; defmethod parser instead.
;;; 
;;; Also now supports multi-line arglist sectionizing.
;;;
zwei:
(defun parse-pcl-defmethod-for-zwei (bp-after-defmethod setfp)
  (block parser
    (flet ((barf (&optional (error t))
	     (return-from parser
	       (cond ((eq error :flavor)
		      (funcall (get 'flavor:defmethod
				    'zwei::definition-function-spec-parser)
			       bp-after-defmethod))
		     (t
		      (values nil nil nil error))))))
      (let ((bp-after-generic (forward-sexp bp-after-defmethod))
	    (qualifiers ())
	    (specializers ())
	    (spec nil)
	    (ignore1 nil)
	    (ignore2 nil))
	(when bp-after-generic
	  (multiple-value-bind (generic error-p)
	      (read-fspec-item-from-interval bp-after-defmethod
					     bp-after-generic)
	    (if error-p
		(barf)				; error here is really bad.... BARF!
		(progn
		  (when (listp generic)
		    (if (and (symbolp (car generic))
			     (string-equal (cl:symbol-name (car generic)) "SETF"))
			(setq generic (second generic)	; is a (setf xxx) form
			      setfp t)
			(barf :flavor)))	; make a last-ditch-effort with flavor parser
		  (let* ((bp1 bp-after-generic)
			 (bp2 (forward-sexp bp1)))
		      (cl:loop
			 (if (null bp2)
			     (barf :more)	; item not closed - need another line!
			     (multiple-value-bind (item error-p)
				 (read-fspec-item-from-interval bp1 bp2)
			       (cond (error-p (barf))	;
				     ((listp item)
				      (setq qualifiers (nreverse qualifiers))
				      (cl:multiple-value-setq (ignore1
								ignore2
								specializers)
					(pcl::parse-specialized-lambda-list item))
				      (setq spec (pcl::make-method-spec 
						   (if setfp
						       `(cl:setf ,generic)
						       generic)
						   qualifiers
						   specializers))
				      (return (values spec
						      'defun
						      (string-interval
							bp-after-defmethod
							bp2))))
				     (t (push item qualifiers)
					(setq bp1 bp2
					      bp2 (forward-sexp bp2))))))))))))))))

zwei:
(progn
  (defun indent-clos-defmethod (ignore bp defmethod-paren &rest ignore)
    (let ((here
	    (forward-over *whitespace-chars* (forward-word defmethod-paren))))
      (loop until (char-equal (bp-char here) #\()
	    do (setf here
		     (forward-over *whitespace-chars* (forward-sexp here))))
      (if (bp-< here bp)
	  (values defmethod-paren nil 2)
	  (values defmethod-paren nil 4))))
  
  (defindentation (pcl::defmethod . indent-clos-defmethod)))

;;;
;;; Teach zwei that when it gets the name of a generic function as an argument
;;; it should edit all the methods of that generic function.  This works for
;;; ED as well as meta-point.
;;;
(zl:advise (flavor:method :SETUP-FUNCTION-SPECS-TO-EDIT zwei:ZMACS-EDITOR)
	   :around
	   setup-function-specs-to-edit-advice
	   ()
  (let ((old-definitions (cadddr arglist))
	(new-definitions ())
	(new nil))
    (dolist (old old-definitions)
      (setq new (setup-function-specs-to-edit-advice-1 old))
      (push (or new (list old)) new-definitions))
    (setf (cadddr arglist) (apply #'append (reverse new-definitions)))
    :do-it))

(defun setup-function-specs-to-edit-advice-1 (spec)
  (and (or (symbolp spec)
	   (and (listp spec) (eq (car spec) 'setf)))
       (gboundp spec)
       (generic-function-p (gdefinition spec))
       (mapcar #'(lambda (m)
		   (make-method-spec spec
				     (method-qualifiers m)
				     (unparse-specializers
				       (method-specializers m))))
	       (generic-function-methods (gdefinition spec)))))


