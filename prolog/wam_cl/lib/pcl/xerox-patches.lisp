;;; -*- Mode: Lisp; Package: XCL-USER; Base: 10.; Syntax: Common-Lisp -*-
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
;;;

(in-package "XCL-USER")


;;; Patch a bug with Lambda-substitution

#+Xerox-Lyric
(defun compiler::meta-call-lambda-substitute (node)
  (let* ((fn (compiler::call-fn node))
	 (var-list (compiler::lambda-required fn))
	 (spec-effects
	  (il:for var il:in var-list
	      il:unless (eq (compiler::variable-scope var) :lexical)
	      il:collect (compiler::effects-representation var)))
	 ;; Bind *SUBST-OCCURED* just so that META-SUBST-VAR-REF ahs a binding
	 ;; to set even when nobody cares.
	 (compiler::*subst-occurred* nil))
    (il:for var il:in var-list
      il:as tail il:on (compiler::call-args node)
      il:when
	(and (eq (compiler::variable-scope var) :lexical)
	     (compiler::substitutable-p (car tail) var)
	     (dolist (compiler::spec-effect spec-effects t)
	       (when
		   (not (compiler::null-effects-intersection compiler::spec-effect
							     (compiler::node-affected (car tail))))
		 (return nil)))
	     (dolist (compiler::later-arg (cdr tail) t)
	       (when (not (compiler::passable (car tail) compiler::later-arg))
		 (return nil))))
	il:do
	  (setf (compiler::lambda-body fn)
		(compiler::meta-substitute (car tail) var
					   (compiler::lambda-body fn))))
    (when (null (compiler::node-meta-p (compiler::lambda-body fn)))
      (setf (compiler::node-meta-p fn) nil)
      (setq compiler::*made-changes* t))))

;;; Some simple optimizations missing from the compiler.


;; Shift by a constant.

;; Unfortunately, these cause the compiler to generate spurious warning
;; messages about "Unknown function IL:LLSH1 called from ..."  It's not often
;; you come across a place where COMPILER-LET is really needed.

#+Xerox-Lyric
(progn

(defvar *ignore-shift-by-constant-optimization* nil
  "Marker used for informing the shift-by-constant optimizers that they are in
 the shift function, and should not optimize.")

(defun il:lrsh1 (x)
  (compiler-let ((*ignore-shift-by-constant-optimization* t))
    (il:lrsh x 1)))

(defun il:lrsh8 (x)
  (compiler-let ((*ignore-shift-by-constant-optimization* t))
    (il:lrsh x 8)))

(defun il:llsh1 (x)
  (compiler-let ((*ignore-shift-by-constant-optimization* t))
    (il:llsh x 1)))

(defun il:llsh8 (x)
  (compiler-let ((*ignore-shift-by-constant-optimization* t))
    (il:llsh x 8)))

(defoptimizer il:lrsh il:right-shift-by-constant (x n &environment env)
  (if (and (constantp n)
	   (not *ignore-shift-by-constant-optimization*))
      (let ((shift-factor (eval n)))
	(cond
	  ((not (numberp shift-factor))
	   (error "Non-numeric arg to ~S, ~S" 'il:lrsh shift-factor))
	  ((= shift-factor 0)
	   x)
	  ((< shift-factor 0)
	   `(il:llsh ,x ,(- shift-factor)))
	  ((< shift-factor 8)
	   `(il:lrsh (il:lrsh1 ,x) ,(1- shift-factor)))
	  (t `(il:lrsh (il:lrsh8 ,x) ,(- shift-factor 8)))))
      'compiler:pass))

(defoptimizer il:llsh il:left-shift-by-constant (x n &environment env)
  (if (and (constantp n)
	   (not *ignore-shift-by-constant-optimization*))
      (let ((shift-factor (eval n)))
	(cond
	  ((not (numberp shift-factor))
	   (error "Non-numeric arg to ~S, ~S" 'il:llsh shift-factor))
	  ((= shift-factor 0)
	   x)
	  ((< shift-factor 0)
	   `(il:lrsh ,x ,(- shift-factor)))
	  ((< shift-factor 8)
	   `(il:llsh (il:llsh1 ,x) ,(1- shift-factor)))
	  (t `(il:llsh (il:llsh8 ,x) ,(- shift-factor 8)))))
      'compiler:pass))

)


;; Simple TYPEP optimiziation

#+Xerox-Lyric
(defoptimizer typep type-t-test (object type)
  "Everything is of type T"
  (if (and (constantp type) (eq (eval type) t))
      `(progn ,object t)
      'compiler:pass))

;;; Declare side-effects (actually, lack of side-effects) info for some
;;; internal arithmetic functions.  These are needed because the compiler runs
;;; the optimizers before checking the side-effects, so side-effect
;;; declarations on the "real" functions are oft times ignored.

#+Xerox-Lyric
(progn

(il:putprops cl::%+ compiler::side-effects-data (:none . :none))
(il:putprops cl::%- compiler::side-effects-data (:none . :none))
(il:putprops cl::%* compiler::side-effects-data (:none . :none))
(il:putprops cl::%/ compiler::side-effects-data (:none . :none))
(il:putprops cl::%logior compiler::side-effects-data (:none . :none))
(il:putprops cl::%logeqv compiler::side-effects-data (:none . :none))
(il:putprops cl::%= compiler::side-effects-data (:none . :none))
(il:putprops cl::%> compiler::side-effects-data (:none . :none))
(il:putprops cl::%< compiler::side-effects-data (:none . :none))
(il:putprops cl::%>= compiler::side-effects-data (:none . :none))
(il:putprops cl::%<= compiler::side-effects-data (:none . :none))
(il:putprops cl::%/= compiler::side-effects-data (:none . :none))
(il:putprops il:lrsh1 compiler::side-effects-data (:none . :none))
(il:putprops il:lrsh8 compiler::side-effects-data (:none . :none))
(il:putprops il:llsh1 compiler::side-effects-data (:none . :none))
(il:putprops il:llsh8 compiler::side-effects-data (:none . :none))

)

;;; Fix a nit in the compiler
#+Xerox-Lyric
(progn

(il:unadvise 'compile)
(il:advise 'compile ':around '(let (compiler::*input-stream*) (inner)))

)

;;; While no person would generate code like (logor x), macro can (and do).

(defun optimize-logical-op-1-arg (form env ctxt)
  (declare (ignore env ctxt))
  (if (= 2 (length form))
      (second form)
      'compiler::pass))

(xcl:defoptimizer logior optimize-logical-op-1-arg)
(xcl:defoptimizer logxor optimize-logical-op-1-arg)
(xcl:defoptimizer logand optimize-logical-op-1-arg)
(xcl:defoptimizer logeqv optimize-logical-op-1-arg)


#+Xerox-Medley

;; A bug compiling LABELS

(defun compiler::meta-call-labels (compiler::node compiler:context)
  ;; This is similar to META-CALL-LAMBDA, but we have some extra information.
  ;; There are only required arguments, and we have the correct number of them.
  (let ((compiler::*made-changes* nil))
    ;; First, substitute the functions wherever possible.
    (dolist (compiler::fn-pair (compiler::labels-funs compiler::node)
	     (when (null (compiler::node-meta-p (compiler::labels-body compiler::node)))
	       (setf (compiler::node-meta-p compiler::node) nil)
	       (setq compiler::*made-changes* t)))
      (when (compiler::substitutable-p (cdr compiler::fn-pair)
				       (car compiler::fn-pair))
	(let ((compiler::*subst-occurred* nil))
	  ;; First try substituting into the body.
	  (setf (compiler::labels-body compiler::node)
		(compiler::meta-substitute (cdr compiler::fn-pair)
					   (car compiler::fn-pair)
					   (compiler::labels-body compiler::node))) 
	  (when (not compiler::*subst-occurred*)
	    ;; Wasn't in the body - try the other functions.
	    (dolist (compiler::target-pair (compiler::labels-funs compiler::node))
	      (unless (eq compiler::target-pair compiler::fn-pair)
		(setf (cdr compiler::target-pair)
		      (compiler::meta-substitute (cdr compiler::fn-pair)
						 (car compiler::fn-pair)
						 (cdr compiler::target-pair)))
		(when compiler::*subst-occurred* ;Found it, we can stop now.
		  (setf (compiler::node-meta-p compiler::node) nil)
		  (setq compiler::*made-changes* t) (return)))))
	  ;; May need to reanalyze the node, since things might have changed.
	  ;; Note that reanalyzing the parts of the node this way means the the
	  ;; state in the enclosing loop is not lost.
	  (dolist (compiler::fns (compiler::labels-funs compiler::node))
	    (compiler::meval (cdr compiler::fns) :argument))
	  (compiler::meval (compiler::labels-body compiler::node) :return))))
    ;; Now remove any functions that aren't referenced.
    (dolist (compiler::fn-pair (prog1 (compiler::labels-funs compiler::node)
				 (setf (compiler::labels-funs compiler::node) nil)))
      (cond ((null (compiler::variable-read-refs (car compiler::fn-pair)))
	     (compiler::release-tree (cdr compiler::fn-pair))
	     (setq compiler::*made-changes* t))
	    (t (push compiler::fn-pair (compiler::labels-funs compiler::node)))))
    ;; If there aren't any functions left, replace the node with its body.
    (when (null (compiler::labels-funs compiler::node))
      (let ((compiler::body (compiler::labels-body compiler::node)))
	(setf (compiler::labels-body compiler::node) nil)
	(compiler::release-tree compiler::node)
	(setq compiler::node compiler::body compiler::*made-changes* t)))
    ;; Finally, set the meta-p flag if everythings OK.
    (if (null compiler::*made-changes*)
	(setf (compiler::node-meta-p compiler::node) compiler:context)
	(setf (compiler::node-meta-p compiler::node) nil)))
  compiler::node)

