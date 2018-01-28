;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(si::select-package "SYSTEM")


(defmacro defun (name vl &body body &aux doc-string)
  "Syntax: (defun name lambda-list {decl | doc}* {form}*)
Defines a global function named by NAME.
The complete syntax of a lambda-list is:
	({var}*
	 [&optional {var | (var [init [svar]])}*]
	 [&rest var]
	 [&key {var | ({var | (keyword var)} [init [svar]])}*
	       [&allow-other-keys]]
	 [&aux {var | (var [init])}*])
The doc-string DOC, if supplied, is saved as a FUNCTION doc and can be
retrieved by (documentation 'NAME 'function)."
  (multiple-value-setq (body doc-string) (remove-documentation body))
  (let* ((function `#'(lambda-block ,name ,vl ,@body)))
    (when *dump-defun-definitions*
      (print function)
      (setq function `(si::bc-disassemble ,function)))
  `(progn
    (si::fset ',name ,function)
    ,@(si::expand-set-documentation name 'function doc-string)
    ',name)))

(defmacro defmacro (name vl &body body &aux doc-string)
  "Syntax: (defmacro name defmacro-lambda-list {decl | doc}* {form}*)
Defines a global macro named by NAME.  The complete syntax of DEFMACRO-LAMBDA-
LIST is:
	( [&whole var] [&environment var] . pvar )
where PVAR may be a symbol,
	( {pvar}* [&optional {var | (pvar [init [pvar]])}*] . var )
or
	( {pvar}*
	  [&optional {var | (pvar [init [pvar]])}*]
	  [{&rest | &body} pvar]
	  [&key {var | ({var | (keyword pvar)} [init [pvar]])}*
	        [&allow-other-keys]]
	  [&aux {var | (pvar [init])}*] )
The doc-string DOC, if supplied, is saved as a FUNCTION doc and can be
retrieved by (documentation 'NAME 'function).  See LIST for the backquote
macro useful for defining macros."
  (multiple-value-bind (expr pprint doc-string)
      (sys::expand-defmacro name vl body)
    (let* ((function `#'(lambda-block ,name ,@(cdr expr))))
      (when *dump-defun-definitions*
	(print function)
	(setq function `(si::bc-disassemble ,function)))
      `(progn
	(si::fset ',name ,function t ,pprint)
	,@(si::expand-set-documentation name 'function doc-string)
	',name))))

(defmacro defvar (var &optional (form nil form-sp) doc-string)
  "Syntax: (defvar name [form [doc]])
Declares the variable named by NAME as a special variable.  If the variable
does not have a value, then evaluates FORM and assigns the value to the
variable.  FORM defaults to NIL.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
  `(PROGN (SYS:*MAKE-SPECIAL ',var)
    ,@(si::expand-set-documentation var 'variable doc-string)
    ,@(when form-sp
	  `((UNLESS (BOUNDP ',var)
	      (SETQ ,var ,form))))
    #+PDE (SYS:RECORD-SOURCE-PATHNAME ',var 'defvar)
    ',var))

(defmacro defparameter (var form &optional doc-string)
  "Syntax: (defparameter name form [doc])
Declares the global variable named by NAME as a special variable and assigns
the value of FORM to the variable.  The doc-string DOC, if supplied, is saved
as a VARIABLE doc and can be retrieved by (documentation 'NAME 'variable)."
  `(PROGN (SYS:*MAKE-SPECIAL ',var)
    ,@(si::expand-set-documentation var 'variable doc-string)
    (SETQ ,var ,form)
    (EVAL-WHEN (COMPILE)		; Beppe
      (WHEN ,(CONSTANTP form)
	(PROCLAIM '(TYPE ,(type-of form) ,var))))
;    (eval-when (load eval)		; Beppe
;      (compiler::proclaim-var (type-of ,var) ',var))
    #+PDE (SYS:RECORD-SOURCE-PATHNAME ',var 'DEFPARAMETER)
    ',var))

(defmacro defconstant (var form &optional doc-string)
  `(PROGN (SYS:*MAKE-CONSTANT ',var ,form)
    ,@(si::expand-set-documentation var 'variable doc-string)
    #+PDE (SYS:RECORD-SOURCE-PATHNAME ',var 'defconstant)
    ',var))

;;;
;;; This is a no-op unless the compiler is installed
;;;
(defmacro define-compiler-macro (name vl &rest body)
  (multiple-value-bind (expr pprint doc-string)
      (sys::expand-defmacro name vl body)
    (let* ((function `#'(lambda-block ,name ,@(cdr expr))))
      (when *dump-defun-definitions*
	(print function)
	(setq function `(si::bc-disassemble ,function)))
      `(progn
	 (put-sysprop ',name 'sys::compiler-macro ,function)
	 ,@(si::expand-set-documentation name 'function doc-string)
	 ',name))))

(defun compiler-macro-function (name &optional env)
  (declare (ignore env))
  (get-sysprop name 'sys::compiler-macro))

;;; Each of the following macros is also defined as a special form,
;;; as required by CLtL. Some of them are used by the compiler (e.g.
;;; dolist), some not at all (e.g. defun).
;;; Thus their names need not be exported.

(defmacro and (&rest forms)
  "Syntax: (and {form}*)
Evaluates FORMs in order.  If any FORM evaluates to NIL, returns
immediately with the value NIL.  Otherwise, returns all values of the
last FORM."
  (if (endp forms)
      T
      (do* ((res '(NIL))
	    (insert res (cddar (rplaca insert `(IF ,(car fs) NIL))))
	    (fs forms (cdr fs)))
	   ((endp (cdr fs))
	    (rplaca insert (car fs))
	    (car res))))
  )

(defmacro or (&rest forms)
  "Syntax: (or {form}*)
Evaluates FORMs in order from left to right.  If any FORM evaluates to non-
NIL, quits and returns that (single) value.  If the last FORM is reached,
returns whatever values it returns."
  (if (endp forms)
      nil
      (let ((x (reverse forms)))
           (do ((forms (cdr x) (cdr forms))
                (form (car x)
                      (let ((temp (gensym)))
                           `(LET ((,temp ,(car forms)))
;			     (DECLARE (:READ-ONLY ,temp)) ; Beppe
                                 (IF ,temp ,temp ,form)))))
               ((endp forms) form))))
  )
               
(defmacro loop (&rest body &aux (tag (gensym)))
  "Syntax: (loop {form}*)
Establishes a NIL block and executes FORMs repeatedly.  The loop is normally
terminated by a non-local exit."
  `(BLOCK NIL (TAGBODY ,tag (PROGN ,@body) (GO ,tag))))

(defmacro lambda (&rest body)
  `(function (lambda ,@body)))

(defmacro lambda-block (name lambda-list &rest lambda-body)
  (multiple-value-bind (decl body doc)
      (si::process-declarations lambda-body)
    (when decl (setq decl (list (cons 'declare decl))))
    `(lambda ,lambda-list ,@doc ,@decl
      (block ,(si::function-block-name name) ,@body))))

; assignment

(defmacro psetq (&rest args)
  "Syntax: (psetq {var form}*)
Similar to SETQ, but evaluates all FORMs first, and then assigns each value to
the corresponding VAR.  Returns NIL."
   (do ((l args (cddr l))
        (forms nil)
        (bindings nil))
       ((endp l) (list* 'LET* (nreverse bindings) (nreverse (cons nil forms))))
       (let ((sym (gensym)))
            (push (list sym (cadr l)) bindings)
            (push (list 'setq (car l) sym) forms)))
   )

; conditionals

(defmacro cond (&rest clauses &aux (form nil))
  "Syntax: (cond {(test {form}*)}*)
Evaluates TESTs in order until one evaluates to non-NIL.  Then evaluates FORMs
in order that follow the TEST and returns all values of the last FORM.  If no
forms follow the TEST, then returns the value of the TEST.  Returns NIL, if no
TESTs evaluates to non-NIL."
  (dolist (l (reverse clauses) form)	; don't use nreverse here
    (if (endp (cdr l))
	(if (eq (car l) 't)
	    (setq form 't)
	    (let ((sym (gensym)))
	      (setq form `(LET ((,sym ,(car l)))
;			   (DECLARE (:READ-ONLY ,sym)) ; Beppe
			   (IF ,sym ,sym ,form)))))
	(if (eq (car l) 't)
	    (setq form (if (endp (cddr l))
			   (cadr l)
			   `(PROGN ,@(cdr l))))
	    (setq form (if (endp (cddr l))
			   `(IF ,(car l) ,(cadr l) ,form)
			   `(IF ,(car l) (PROGN ,@(cdr l)) ,form))))))
  )

(defmacro when (pred &rest body)
  "Syntax: (when test {form}*)
If TEST evaluates to non-NIL, then evaluates FORMs and returns all values of
the last FORM.  If not, simply returns NIL."
  `(IF ,pred (PROGN ,@body)))

(defmacro unless (pred &rest body)
  "Syntax: (unless test {form}*)
If TEST evaluates to NIL, then evaluates FORMs and returns all values of the
last FORM.  If not, simply returns NIL."
  `(IF (NOT ,pred) (PROGN ,@body)))

; program feature

(defmacro prog (vl &rest body &aux (decl nil))
  "Syntax: (prog ({var | (var [init])}*) {decl}* {tag | statement}*)
Establishes a NIL block, binds each VAR to the value of INIT (which defaults
to NIL) in parallel, and executes STATEMENTs.  Returns NIL."
  (multiple-value-setq (decl body)
    (find-declarations body))
  `(BLOCK NIL (LET ,vl ,@decl (TAGBODY ,@body)))
  )

(defmacro prog* (vl &rest body &aux (decl nil))
  "Syntax: (prog* ({var | (var [init])}*) {decl}* {tag | statement}*)
Establishes a NIL block, binds each VAR to the value of INIT (which defaults
to NIL) sequentially, and executes STATEMENTs.  Returns NIL."
  (multiple-value-setq (decl body)
    (find-declarations body))
  `(BLOCK NIL (LET* ,vl ,@decl (TAGBODY ,@body)))
  )

; sequencing

(defmacro prog1 (first &rest body &aux (sym (gensym)))
  "Syntax: (prog1 first-form {form}*)
Evaluates FIRST-FORM and FORMs in order.  Returns the value of FIRST-FORM."
  (if (null body) first
  `(LET ((,sym ,first))
;    (DECLARE (:READ-ONLY ,sym)) ; Beppe
    ,@body ,sym)))

(defmacro prog2 (first second &rest body &aux (sym (gensym)))
  "Syntax: (prog2 first-form second-form {forms}*)
Evaluates FIRST-FORM, SECOND-FORM, and FORMs in order.  Returns the value of
SECOND-FORM."
  `(PROGN ,first (LET ((,sym ,second))
;		       (DECLARE (:READ-ONLY ,sym)) ; Beppe
		       ,@body ,sym)))

; multiple values

(defmacro multiple-value-list (form)
  `(MULTIPLE-VALUE-CALL 'LIST ,form))

(defmacro multiple-value-setq (vars form)
  (do ((vl vars (cdr vl))
       (sym (gensym))
       (forms nil)
       (n 0 (1+ n)))
      ((endp vl) `(LET ((,sym (MULTIPLE-VALUE-LIST ,form))) ,@forms))
      (declare (fixnum n))
      (push `(SETQ ,(car vl) (NTH ,n ,sym)) forms))
  )

(defmacro multiple-value-bind (vars form &rest body)
  (do ((vl vars (cdr vl))
       (sym (gensym))
       (bind nil)
       (n 0 (1+ n)))
      ((endp vl) `(LET* ((,sym (MULTIPLE-VALUE-LIST ,form)) ,@(nreverse bind))
		   ,@body))
    (declare (fixnum n))
    (push `(,(car vl) (NTH ,n ,sym)) bind))
  )

(defun do/do*-expand (control test result body let psetq
			      &aux (decl nil) (label (gensym)) (exit (gensym))
			      (vl nil) (step nil))
  (declare (si::c-local))
  (multiple-value-setq (decl body)
    (find-declarations body))
  (dolist (c control)
    (when (symbolp  c) (setq c (list c))) ; convenient extension to CL. Beppe
    (case (length c)
      ((1 2)
       (push c vl))
      ((3)
       (push (butlast c) vl)
       (push (first c) step)
       (push (third c) step))
      (t
       (error "Too many arguments in init form of do/do*"))))
  `(BLOCK NIL
          (,let ,(nreverse vl)
               ,@decl
               (TAGBODY
		  (GO ,exit)
		,label
		  ,@body
		  ,@(when step (list (cons psetq (nreverse step))))
		,exit
		  (UNLESS ,test (GO ,label)))
	       ,@result)))

(defmacro do (control (test . result) &rest body)
  (do/do*-expand control test result body 'LET 'PSETQ))

(defmacro do* (control (test . result) &rest body)
  (do/do*-expand control test result body 'LET* 'SETQ))

(defmacro case (keyform &rest clauses &aux (form nil) (key (gensym)))
  (dolist (clause (reverse clauses)
	   `(LET ((,key ,keyform))
;	     (DECLARE (:READ-ONLY ,key)) ; Beppe
	     ,form))
    (if (or (eq (car clause) 'T) (eq (car clause) 'OTHERWISE))
	(setq form `(PROGN ,@(cdr clause)))
	(if (consp (car clause))
	    (setq form `(IF (MEMBER ,key ',(car clause))
			 (PROGN ,@(cdr clause))
			 ,form))
	    (if (car clause)
		(setq form `(IF (EQL ,key ',(car clause))
			     (PROGN ,@(cdr clause))
			     ,form))))))
  )

(defmacro return (&optional (val nil)) `(RETURN-FROM NIL ,val))

(defmacro dolist ((var form &optional (val nil)) &rest body
                                                 &aux (temp (gensym)) decl)
  (multiple-value-setq (decl body)
    (find-declarations body))
  `(DO* ((,temp ,form (cdr ,temp)) (,var))
	((ENDP ,temp) ,val)
    ,@decl
    (SETQ ,var (CAR ,temp))
    ,@body))

(defmacro dotimes ((var form &optional (val nil)) &rest body
                                                  &aux (temp (gensym)))
  `(DO* ((,temp ,form) (,var 0 (1+ ,var)))
        ((>= ,var ,temp) ,val)
        (DECLARE (FIXNUM ,var)) ; Beppe  (:READ-ONLY ,temp)
        ,@body))

;; Declarations
(defmacro declaim (&rest decl-specs)
  (if (cdr decl-specs)
    `(eval-when (compile load eval) (mapcar #'proclaim ',decl-specs))
    `(eval-when (compile load eval) (proclaim ',(car decl-specs)))))

(defmacro c-declaim (&rest decl-specs)
  (if (cdr decl-specs)
    `(eval-when (compile) (mapcar #'proclaim ',decl-specs))
    `(eval-when (compile) (proclaim ',(car decl-specs)))))

(defmacro in-package (name)
  `(si::select-package ,(string name)))

;; FIXME!
(defmacro the (type value)
  (declare (ignore type))
  value)

(defmacro define-symbol-macro (symbol expansion)
  (cond ((not (symbolp symbol))
	 (error "DEFINE-SYMBOL-MACRO: ~A is not a symbol"
		symbol))
	((specialp symbol)
	 (error "DEFINE-SYMBOL-MACRO: cannot redefine a special variable, ~A"
		symbol))
	(t
	 `(progn
	   (put-sysprop ',symbol 'si::symbol-macro (lambda (form env) ',expansion))
	   ',symbol))))

(defmacro nth-value (n expr)
  `(nth ,n (multiple-value-list ,expr)))

(defmacro load-time-value (form)
  `(quote ,(eval form)))
