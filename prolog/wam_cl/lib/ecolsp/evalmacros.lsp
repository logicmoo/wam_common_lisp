;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package #:lisp)

(export '(defvar defparameter defconstant))

(in-package #:system)


(eval-when (compile) (proclaim '(optimize (safety 2) (space 3))))
(eval-when (eval compile) (defun sys:clear-compiler-properties (symbol)))
(eval-when (eval compile) (setq sys:*inhibit-macro-special* nil))

(defmacro defvar (var &optional (form nil form-sp) doc-string)
  `(PROGN (SYS:*MAKE-SPECIAL ',var)
    ,@(if doc-string
	  `((SYS:PUTPROP ',var ,doc-string 'VARIABLE-DOCUMENTATION)))
    ,@(if form-sp
	  `((UNLESS (BOUNDP ',var)
	      (SETQ ,var ,form))))
    #+PDE (SYS:RECORD-SOURCE-PATHNAME ',var 'defvar)
    ',var))

(defmacro defparameter (var form &optional doc-string)
  `(PROGN (SYS:*MAKE-SPECIAL ',var)
    ,@(if doc-string
	  `((SYS:PUTPROP ',var ,doc-string 'VARIABLE-DOCUMENTATION)))
    (SETQ ,var ,form)
    (EVAL-WHEN (COMPILE)		; Beppe
      (WHEN (CONSTANTP ',form)
	(PROCLAIM '(TYPE ,(type-of form) ,var))))
;    (eval-when (load eval)		; Beppe
;      (compiler::proclaim-var (type-of ,var) ',var))
    #+PDE (SYS:RECORD-SOURCE-PATHNAME ',var 'DEFPARAMETER)
    ',var))

(defmacro defconstant (var form &optional doc-string)
  `(PROGN (SYS:*MAKE-CONSTANT ',var ,form)
    ,@(if doc-string
	  `((SYS:PUTPROP ',var ,doc-string 'VARIABLE-DOCUMENTATION)))
    #+PDE (SYS:RECORD-SOURCE-PATHNAME ',var 'defconstant)
    ',var))


;;; Each of the following macros is also defined as a special form,
;;; as required by CLtL. Some of them are used by the compiler (e.g.
;;; dolist), some not at all (e.g. defun).
;;; Thus their names need not be exported.

(defmacro and (&rest forms)
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
               
(defmacro locally (&rest body) `(LET () ,@body))

(defmacro loop (&rest body &aux (tag (gensym)))
  `(BLOCK NIL (TAGBODY ,tag (PROGN ,@body) (GO ,tag))))

(defmacro defun (name lambda-list &rest body)
  (multiple-value-bind (doc decl body)
      (find-doc body nil)
    `(PROGN
      (SETF (SYMBOL-FUNCTION ',name)
       #'(LAMBDA ,lambda-list
	   ,@decl (BLOCK ,name ,@body)))
      ,@(when doc
	  `((SETF (GET ',name 'FUNCTION-DOCUMENTATION) ,doc)))
      ',name)
    ))

; assignment

(defmacro psetq (&rest args)
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
  `(IF ,pred (PROGN ,@body)))

(defmacro unless (pred &rest body)
  `(IF (NOT ,pred) (PROGN ,@body)))

; program feature

(defmacro prog (vl &rest body &aux (decl nil))
  (multiple-value-setq (decl body)
    (find-declarations body))
  `(BLOCK NIL (LET ,vl ,@decl (TAGBODY ,@body)))
  )

(defmacro prog* (vl &rest body &aux (decl nil))
  (multiple-value-setq (decl body)
    (find-declarations body))
  `(BLOCK NIL (LET* ,vl ,@decl (TAGBODY ,@body)))
  )

; sequencing

(defmacro prog1 (first &rest body &aux (sym (gensym)))
  `(LET ((,sym ,first))
;    (DECLARE (:READ-ONLY ,sym)) ; Beppe
    ,@body ,sym))

(defmacro prog2 (first second &rest body &aux (sym (gensym)))
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
			      &aux (decl nil) (label (gensym))
			      (vl nil) (step nil))
  (multiple-value-setq (decl body)
    (find-declarations body))
  (dolist (c control)
    (when (symbolp  c) (setq c (list c))) ; convenient extension to CL. Beppe
    (push (list (car c) (cadr c)) vl)
    (unless (endp (cddr c))
            (push (car c) step)
            (push (caddr c) step)))
  `(BLOCK NIL
          (,let ,(nreverse vl)
               ,@decl
               (TAGBODY
                ,label (IF ,test (RETURN (PROGN ,@result)))
                       ,@body
                       (,psetq ,@(nreverse step))
                       (GO ,label)))))

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
