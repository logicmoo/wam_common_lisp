;;;;  CMPBIND  Variable Binding.
;;;;
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;  This file is part of ECoLisp, herein referred to as ECL.
;;;;
;;;;    ECL is free software; you can redistribute it and/or modify it under
;;;;    the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;;;;    the Free Software Foundation; either version 2 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package 'compiler)

;;; bind must be called for each variable in a lambda or let, once the value
;;; to be bound has been placed in loc.
;;; bind takes care of setting var-loc, so it must distinguish when it is
;;; called more than once for the same variable: for example an optional
;;; variable can be bound either to the supplied value or to the default one.
;;; The optional parameter ALTERNATIVE is used for his purpose.
;;; Is it still used?

(defun bind (loc var &optional alternative)
  ;; loc can be either (LCL n), (VA-ARGS kind), (KEYVARS n), (CAR n),
  ;; a constant, or (VAR var) from a let binding. ; ccb
  (declare (type var var))
  (case (var-kind var)
    (LEXICAL
     (let ((var-loc (var-loc var)))
       (if (var-ref-ccb var)
	   (progn
	     (unless (sys:fixnump var-loc)
	       ;; first binding: assign location
	       (setq var-loc (next-env))
	       (setf (var-loc var) var-loc))
	     (when (zerop var-loc) (wt-nl "env" *env-lvl* " = Cnil;"))
	     (wt-nl "CLV" var-loc
		    "=&CAR(env" *env-lvl* "=CONS(" loc ",env" *env-lvl* "));"))
	   (progn
	     (unless (consp var-loc)
	       ;; first binding: assign location
	       (setq var-loc (next-lex))
	       (setf (var-loc var) var-loc))
	     (wt-nl) (wt-lex var-loc) (wt "=" loc ";")))
       (wt-comment (var-name var))))
    (SPECIAL
     (bds-bind loc var)
     (unless alternative
       (push 'BDS-BIND *unwind-exit*)))
    (OBJECT
     (if (eq (var-loc var) 'OBJECT)
	 ;; set location for lambda list requireds
	 (setf (var-loc var) (second loc))
	 ;; already has location (e.g. optional in lambda list)
	 (unless (and (consp loc)	; check they are not the same
		      (eq 'LCL (car loc))
		      (= (var-loc var) (second loc)))
	   (wt-nl) (wt-lcl (var-loc var)) (wt "= " loc ";"))
	 ))
    (t
     (wt-nl) (wt-lcl (var-loc var)) (wt "= ")
     (case (var-kind var)
       (FIXNUM (wt-fixnum-loc loc))
       (CHARACTER (wt-character-loc loc))
       (LONG-FLOAT (wt-long-float-loc loc))
       (SHORT-FLOAT (wt-short-float-loc loc))
       (t (baboon)))
     (wt ";")))
  )

;;; Used by let*, defmacro and lambda's &aux, &optional, &rest, &keyword
(defun bind-init (var form &optional alternative)
  (let ((*destination* `(BIND ,var)))
    ;; assigning location must be done before calling c2expr*,
    ;; otherwise the increment to *env* or *lex* is done during
    ;; unwind-exit and will be shadowed by functions (like c2let)
    ;; which rebind *env* or *lex*.
    (when (eq (var-kind var) 'LEXICAL)
      (if (var-ref-ccb var)
	  (unless (si:fixnump (var-loc var))
	    (setf (var-loc var) (next-env)))
	  (unless (consp (var-loc var))
	    (setf (var-loc var) (next-lex)))))
    (c2expr* form)
    (when (and (eq (var-kind var) 'SPECIAL)
	       (not alternative))
      (push 'BDS-BIND *unwind-exit*))))

(defun bds-bind (loc var)
  (wt-nl "bds_bind(VV[" (var-loc var) "]," loc ");")
  (wt-comment (var-name var)))

(setf (get 'BIND 'SET-LOC) 'bind)
