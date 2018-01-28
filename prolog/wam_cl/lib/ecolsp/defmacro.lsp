;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;         defines SYS:DEFMACRO*, the defmacro preprocessor

(in-package 'system)

(eval-when (compile) (proclaim '(optimize (safety 2) (space 3))))

;;; valid lambda-list to DEFMACRO is:
;;;
;;;	( [ &whole sym ]
;;;	  [ &environment sym ]
;;;	  { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )
;;;
;;; where v is short for { defmacro-lambda-list | sym }.
;;; A symbol may be accepted as a DEFMACRO lambda-list, in which case
;;; (DEFMACRO <name> <symbol> ... ) is equivalent to
;;; (DEFMACRO <name> (&REST <symbol>) ...).
;;; Defamcro-lambda-list is defined as:
;;;
;;;	( { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )

(defvar *dl*)
(defvar *key-check*)
(defvar *arg-check*)

(defmacro defmacro (name vl &rest body)
  `(multiple-value-bind (expr doc pprint)
    (sys::expand-defmacro ',name ',vl ',body)
    (sys:define-macro ',name expr doc pprint)))

(defun sys::expand-defmacro (name vl body
				 &aux *dl* (*key-check* nil)
				 (*arg-check* nil)
				 doc decls whole ppn (env nil) (envp nil))
  (labels ((dm-vl (vl whole top)
	     (do ((optionalp) (restp) (keyp)
		  (allow-other-keys-p) (auxp)
		  (rest) (allow-other-keys) (keys) (no-check)
		  (n (if top 1 0)) (ppn 0) (v)
		  )
		 ((not (consp vl))
		  (when vl
		    (when restp (dm-bad-key '&rest))
		    (push (list vl (dm-nth-cdr n whole)) *dl*)
		    (setq no-check t))
		  (when (and rest (not allow-other-keys))
		    (push (cons rest keys) *key-check*))
		  (unless no-check (push (cons whole n) *arg-check*))
		  ppn
		  )
	       (declare (fixnum n ppn))
	       (setq v (car vl))
	       (cond
		 ((eq v '&optional)
		  (when optionalp (dm-bad-key '&optional))
		  (setq optionalp t)
		  (pop vl))
		 ((or (eq v '&rest) (eq v '&body))
		  (when restp (dm-bad-key v))
		  (dm-v (second vl) (dm-nth-cdr n whole))
		  (setq restp t optionalp t no-check t)
		  (setq vl (cddr vl))
		  (when (eq v '&body)
		    (setq ppn (if top (the fixnum (1- n)) n))))
		 ((eq v '&key)
		  (when keyp (dm-bad-key '&key))
		  (setq rest (gensym))
		  (push (list rest (dm-nth-cdr n whole)) *dl*)
		  (setq keyp t restp t optionalp t no-check t)
		  (pop vl))
		 ((eq v '&allow-other-keys)
		  (when (or (not keyp) allow-other-keys-p)
		    (dm-bad-key '&allow-other-keys))
		  (setq allow-other-keys-p t)
		  (setq allow-other-keys t)
		  (pop vl))
		 ((eq v '&aux)
		  (when auxp (dm-bad-key '&aux))
		  (setq auxp t allow-other-keys-p t keyp t restp t optionalp t)
		  (pop vl))
		 (auxp
		  (let (x (init nil))
		    (cond ((symbolp v) (setq x v))
			  (t (setq x (car v))
			     (unless (endp (cdr v)) (setq init (second v)))))
		    (dm-v x init))
		  (pop vl))
		 (keyp
		  (let ((temp (gensym)) x k (init nil) (sv nil))
		    (cond ((symbolp v) (setq x v
					     k (intern (string v) 'keyword)))
			  (t (if (symbolp (car v))
				 (setq x (car v)
				       k (intern (string (car v)) 'keyword))
				 (setq x (cadar v) k (caar v)))
			     (unless (endp (cdr v))
			       (setq init (second v))
			       (unless (endp (cddr v))
				 (setq sv (caddr v))))))
		    (dm-v temp `(getf ,rest ,k 'failed))
		    (dm-v x `(if (eq ,temp 'failed) ,init ,temp))
		    (when sv (dm-v sv `(not (eq ,temp 'failed))))
		    (push k keys))
		  (pop vl))
		 (optionalp
		  (let (x (init nil) (sv nil))
		    (cond ((symbolp v) (setq x v))
			  (t (setq x (car v))
			     (unless (endp (cdr v))
			       (setq init (second v))
			       (unless (endp (cddr v))
				 (setq sv (caddr v))))))
		    (dm-v x `(if ,(dm-nth-cdr n whole) ,(dm-nth n whole) ,init))
		    (when sv (dm-v sv `(not (null ,(dm-nth-cdr n whole))))))
		  (incf n)
		  (pop vl)
		  )
		 (t (dm-v v `(if ,(dm-nth-cdr n whole)
			      ,(dm-nth n whole)
			      (dm-too-few-arguments)))
		    (incf n)
		    (pop vl))
		 )))
	   (dm-v (v init)
	     (if (symbolp v)
		 (push (if init (list v init) v) *dl*)
		 (let ((temp (gensym)))
		   (push (if init (list temp init) temp) *dl*)
		   (dm-vl v temp nil))))

	   (dm-nth (n v)
	     (multiple-value-bind (q r) (floor n 4)
	       (declare (fixnum q r))
	       (dotimes (i q) (setq v (list 'CDDDDR v)))
	       (case r
		 (0 (list 'CAR v))
		 (1 (list 'CADR v))
		 (2 (list 'CADDR v))
		 (3 (list 'CADDDR v))
		 )))

	   (dm-nth-cdr (n v)
	     (multiple-value-bind (q r) (floor n 4)
	       (declare (fixnum q r))
	       (dotimes (i q) (setq v (list 'CDDDDR v)))
	       (case r
		 (0 v)
		 (1 (list 'CDR v))
		 (2 (list 'CDDR v))
		 (3 (list 'CDDDR v))
		 )))
	   )
  (cond ((listp vl))
        ((symbolp vl) (setq vl (list '&rest vl)))
        (t (error "The defmacro-lambda-list ~s is not a list." vl)))
  (multiple-value-setq (doc decls body) (find-doc body nil))
  (if (and (listp vl) (eq (car vl) '&whole))
      (setq whole (second vl)
	    vl (cddr vl))
      (setq whole (gensym)))
  (if (setq env (member '&environment vl :test #'eq))
      (setq vl (nconc (ldiff vl env) (cddr env))
	    env (second env))
      (progn
	(setq env (gensym))
	(push `(DECLARE (ignore ,env)) decls)))
  (setq *dl* `(&aux ,env ,whole))
  (setq ppn (dm-vl vl whole t))
  (dolist (kc *key-check*)
          (push `(unless (getf ,(car kc) :allow-other-keys)
                         (do ((vl ,(car kc) (cddr vl)))
                             ((endp vl))
                             (unless (member (car vl) ',(cdr kc))
                                     (dm-key-not-allowed (car vl))
                                     )))
                body))
  (dolist (ac *arg-check*)
          (push `(unless (endp ,(dm-nth-cdr (cdr ac) (car ac)))
                         (dm-too-many-arguments)) body))
  (values `(LAMBDA-BLOCK ,name ,(nreverse *dl*) ,@(nconc decls body))
      doc ppn))
  )

(defun dm-bad-key (key)
       (error "Defmacro-lambda-list contains illegal use of ~s." key))

(defun dm-too-few-arguments ()
       (error "Too few arguments are supplied to defmacro-lambda-list."))

(defun dm-too-many-arguments ()
       (error "Too many arguments are supplied to defmacro-lambda-list."))

(defun dm-key-not-allowed (key)
       (error "The key ~s is not allowed." key))

(defun find-doc (body ignore-doc)
  (if (endp body)
      (values nil nil nil)
      (let ((d (macroexpand (car body))))
        (cond ((stringp d)
               (if (or (endp (cdr body)) ignore-doc)
                   (values nil nil (cons d (cdr body)))
                   (multiple-value-bind (doc decls b) (find-doc (cdr body) t)
                     (declare (ignore doc))
                     (values d decls b))))
              ((and (consp d) (eq (car d) 'DECLARE))
               (multiple-value-bind (doc decls b)
                                    (find-doc (cdr body) ignore-doc)
                 (values doc (cons d decls) b)))
              (t (values nil nil (cons d (cdr body))))))))

(defun find-declarations (body)
  (if (endp body)
      (values nil nil)
      (let ((d (macroexpand (car body))))
        (cond ((stringp d)
               (if (endp (cdr body))
                   (values nil (list d))
                   (multiple-value-bind (ds b)
                       (find-declarations (cdr body))
                     (values (cons d ds) b))))
              ((and (consp d) (eq (car d) 'DECLARE))
               (multiple-value-bind (ds b)
                   (find-declarations (cdr body))
                 (values (cons d ds) b)))
              (t
               (values nil (cons d (cdr body))))))))
