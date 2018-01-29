;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPSPECIAL  Miscellaneous special forms.

(in-package 'compiler)

(defun c1quote (args)
  (when (endp args) (too-few-args 'QUOTE 1 0))
  (unless (endp (cdr args)) (too-many-args 'QUOTE 1 (length args)))
  (c1constant-value (car args) t)
  )

(defun c1eval-when (args)
  (when (endp args) (too-few-args 'EVAL-WHEN 1 0))
  (dolist (situation (car args) (c1nil))
    (case situation
      ((EVAL #+ansi :EXECUTE) (return-from c1eval-when (c1progn (cdr args))))
      ((LOAD COMPILE #+ansi :LOAD-TOPLEVEL #+ansi :COMPILE-TOPLEVEL))
      (otherwise
           (cmperr "The situation ~s is illegal." situation))))
  )

(defun c1declare (args)
  (cmperr "The declaration ~s was found in a bad place." (cons 'DECLARE args))
  )

(defun c1the (args &aux info form type)
  (when (or (endp args) (endp (cdr args)))
        (too-few-args 'THE 2 (length args)))
  (unless (endp (cddr args))
          (too-many-args 'THE 2 (length args)))
  (setq form (c1expr (second args)))
  (setq info (copy-info (second form)))
  (setq type (type-and (type-filter (car args)) (info-type info)))
  (unless type
    (cmpwarn "Type mismatch was found in ~s." (cons 'THE args))
    (setq type T))
  (setf (info-type info) type)
  (setf (second form) info)
  form
  )

(defun c1compiler-let (args &aux (symbols nil) (values nil))
  (when (endp args) (too-few-args 'COMPILER-LET 1 0))
  (dolist (spec (car args))
    (cond ((consp spec)
           (cmpck (not (and (symbolp (car spec))
                            (or (endp (cdr spec))
                                (endp (cddr spec)))))
                  "The variable binding ~s is illegal." spec)
           (push (car spec) symbols)
           (push (if (endp (cdr spec)) nil (eval (second spec))) values))
          ((symbolp spec)
           (push spec symbols)
           (push nil values))
          (t (cmperr "The variable binding ~s is illegal." spec))))
  (setq symbols (nreverse symbols))
  (setq values (nreverse values))
  (setq args (progv symbols values (c1progn (cdr args))))
  (list 'COMPILER-LET (second args) symbols values args)
  )

(defun c2compiler-let (symbols values body)
  (progv symbols values (c2expr body)))

(defun c1local-closure (fname &aux (ccb nil) (clb nil))
  ;;; Called only from C1FUNCTION.
  (dolist (fun *funs*)
    (cond ((eq fun 'CB) (setq ccb t))
	  ((eq fun 'LB) (setq clb t))
          ((consp fun)
           (when (eq (car fun) fname) (return (second fun))))
          ((eq (fun-name fun) fname)
	   (setf (fun-ref-ccb fun) t)
	   (incf (fun-ref fun))
	   ;; we introduce a variable to hold the funob
	   (let ((var (or (fun-var fun)
			  (setf (fun-var fun)
				(make-var :name fname :kind 'OBJECT)))))
	     (cond (ccb (setf (var-ref-ccb var) t
			      (var-kind var) 'LEXICAL))
		   (clb (setf (var-loc var) 'CLB
			      (var-kind var) 'LEXICAL)))
	     (incf (var-ref var))
	     (return (list 'VAR (make-info :referred-vars (list var)
					   :local-referred (list var))
			   (list var)))	; ccb
	     ))))
  )

(defun c1function (args &aux fd)
  (when (endp args) (too-few-args 'function 1 0))
  (unless (endp (cdr args)) (too-many-args 'function 1 (length args)))
  (let ((fun (car args)))
    #+ansi
    (setq fun (or (si:setf-namep fun) fun))
    (cond ((symbolp fun)
           (if (and (setq fd (c1local-closure fun))
                    (eq 'VAR (car fd)))
	       fd
               `(FUNCTION ,(make-info :sp-change (not (get fun 'NO-SP-CHANGE)))
		 GLOBAL nil ,fun)))
          ((and (consp fun) (eq (car fun) 'LAMBDA))
           (cmpck (endp (cdr fun))
                  "The lambda expression ~s is illegal." fun)
           (let* ((funob (let ((*vars* (cons 'CB *vars*))
			       (*funs* (cons 'CB *funs*))
			       (*blocks* (cons 'CB *blocks*))
			       (*tags* (cons 'CB *tags*)))
			   (c1lambda-expr (cdr fun))))
		  (info (second funob))
		  (closure (closure-p funob))
		  (body (cddr fun))
		  (fun (make-fun :name
				 (when (and (consp body)
					    (null (cdr body))
					    (consp (first body))
					    (eq 'BLOCK (first (first body))))
				   (list (second (first body))))
				 :cfun (gen-init-keywords funob)
				 :closure closure)))
	     (if closure
		 `(FUNCTION ,info CLOSURE ,funob ,fun)
		 (progn
		   (push `(FUNCTION-CONSTANT ,funob ,fun)
			 *top-level-forms*)
		   `(FUNCTION ,info CONSTANT ,funob ,fun)))))
          (t (cmperr "The function ~s is illegal." fun))))
  )

(defun c2function (kind funob fun)
  (case kind
    (GLOBAL
     (unwind-exit (list 'SYMBOL-FUNCTION (add-symbol fun))))
    (CLOSURE
     (setf (fun-closure fun) (> *env* 0))
     (new-local 0 fun funob)	; 0 was *level*
     (unwind-exit `(MAKE-CCLOSURE ,fun)))
    (CONSTANT
     (unwind-exit (fun-var fun))))
  )

;;; Mechanism for sharing code.
(defun new-local (level fun funob)
  ;; returns the previous function or NIL.
  (declare (type fun fun))
  (let ((previous (dolist (local *local-funs*)
		    (when (similar funob (third local))
		      (return (second local)))))
        (closure (when (fun-closure fun) 'CLOSURE)))
    (if previous
	(progn
          (if closure
	      (cmpnote "Sharing code for closure")
	      (cmpnote "Sharing code for local function ~A" (fun-name fun)))
	  (setf (fun-cfun fun) (fun-cfun previous))
	  previous)
        (progn
          (setf (fun-level fun) (if (fun-ref-ccb fun) 0 level)
                (fun-env fun) *env*)
          (push (list closure fun funob) *local-funs*)
          (when closure (push fun *closures*))
	  NIL)))
  )

(defun gen-init-keywords (lambda-expr)
  (let* ((keys (fifth (third lambda-expr)))
	 (cfun (next-cfun)))
    (when keys
      (push (list 'INIT-KEYWORDS (length keys) cfun lambda-expr)
	    *top-level-forms*))
    cfun))

(defun wt-symbol-function (vv)
  (if *safe-compile*
      (wt "symbol_function(VV[" vv "])")
      (wt "(VV[" vv "]->s.s_gfdef)")))

(defun wt-make-closure (fun &aux (cfun (fun-cfun fun)))
  (declare (type fun fun))
  (if (fun-closure fun)
      (wt "make_cclosure(LC" cfun ",env" *env-lvl*)
      (wt "make_cfun(LC" cfun ",Cnil")) ; empty environment
  (wt ",&Cblock)"))


;;; ----------------------------------------------------------------------

(setf (get 'quote 'c1special) 'c1quote)
(setf (get 'function 'c1special) 'c1function)
(setf (get 'function 'c2) 'c2function)
(setf (get 'the 'c1special) 'c1the)
(setf (get 'eval-when 'c1special) 'c1eval-when)
(setf (get 'declare 'c1special) 'c1declare)
(setf (get 'compiler-let 'c1special) 'c1compiler-let)
(setf (get 'compiler-let 'c2) 'c2compiler-let)

(setf (get 'symbol-function 'wt-loc) 'wt-symbol-function)
(setf (get 'make-cclosure 'wt-loc) 'wt-make-closure)
