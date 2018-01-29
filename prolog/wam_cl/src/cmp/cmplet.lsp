;;;;  CMPLET  Let and Let*.
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    ECL is free software; you can redistribute it and/or modify it
;;;;    under the terms of the GNU Library General Public License as
;;;;    published by the Free Software Foundation; either version 2 of
;;;;    the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package 'compiler)

(defun c1let (args &aux (info (make-info))
                   	(setjmps *setjmps*)
                        (forms nil) (vars nil) (vnames nil)
                        ss is ts body other-decls
                        (*vars* *vars*))
  (when (endp args) (too-few-args 'let 1 0))

  (multiple-value-setq (body ss ts is other-decls) (c1body (cdr args) nil))

  (c1add-globals ss)

  (dolist (x (car args))
    (cond ((symbolp x)
           (let ((v (c1make-var x ss is ts)))
                (push x vnames)
                (push v vars)
                (push (default-init (var-type v)) forms)))
          (t (cmpck (not (and (consp x) (or (endp (cdr x)) (endp (cddr x)))))
                    "The variable binding ~s is illegal." x)
             (let* ((vname (car x))
		    (v (c1make-var vname ss is ts))
		    (form (if (endp (cdr x))
                            (default-init (var-type v))
                            (and-form-type (var-type v)
                                           (c1expr* (second x) info)
                                           (second x)))))
	       ;; :read-only variable handling. Beppe
;	       (when (read-only-variable-p vname ts)
;		     (setf (var-type v) (info-type (second form))))
	       (push vname vnames)
	       (push v vars)
	       (push form forms)))))

  (dolist (v (reverse vars)) (push v *vars*))

  (check-vdecl vnames ts is)

  (setq body (c1decl-body other-decls body))

  (add-info info (second body))
  (setf (info-type info) (info-type (second body)))

  ;; since the body may produce type constraints on variables:
  ;; (let (x) (foo x)) where (type (function (fixnum) fixnum) foo)
  ;; do it again
  (do ((vars vars (cdr vars))
       (forms forms (cdr forms))
       (form) (var))
      ((null vars))
    (declare (type var var))
    (setq var (first vars)
	  form (first forms))
    (setf (car forms)
	  (and-form-type (var-type var) form (var-name var)))
    (when (member (info-type (second (car forms)))
		  '(FIXNUM CHARACTER LONG-FLOAT SHORT-FLOAT) :test #'eq)
      (incf (var-ref var)))		; force unboxing
    ;; Automatic treatement for READ-ONLY variables:
    (unless (var-changed-in-forms var (list body))
      (let ((type (info-type (second (car forms)))))
	(setf (var-type var) type)
	(update-var-type var type body)))
    (check-vref var)
    )

  (unless (eql setjmps *setjmps*) (setf (info-volatile info) t))

  (list 'LET info (reverse vars) (reverse forms) body)
  )

(defun update-var-type (var type x)
  (when (listp x)
    (if (and (eq (car x) 'VAR)
	     (eq var (first (third x))))
	(setf (info-type (second x))
	      ;; some occurrences might be typed with 'the'
	      (type-and (info-type (second x)) type))
	(dolist (e x)
	  (update-var-type var type e)))))

;(defun read-only-variable-p (v l) (eq 'READ-ONLY (cdr (assoc v l))))

(defun c2let (vars forms body
                   &aux (block-p nil) (bindings nil)
                   initials
                   (*unwind-exit* *unwind-exit*)
		   (*env* *env*)
                   (*env-lvl* *env-lvl*) env-grows)
  (declare (type boolean block-p))

  ;; Allocation is needed for:
  ;; 1. each variable which is LOCAL and which is not REPLACED
  ;;    or whose value is not DISCARDED
  ;; 2. each init form for the remaining variables except last

  ;; Determine which variables are really necessary and create list of init's
  ;; and list of bindings. Bindings for specials must be done after all inits.
  (labels ((do-decl (var lcl)
	     (declare (type var var))
	     (setf (var-loc var) lcl)	; must be set or bind will clobber it
	     (wt-nl)
	     (unless block-p
	       (wt "{") (setq block-p t))
	     (wt *volatile* (register var) (rep-type (var-kind var)))
	     (wt-lcl lcl) (wt ";")
	     (when (local var)
	       (wt-comment (var-name var))))
	   (do-init (var form fl)
	     (if (and (local var)
		      (not (args-cause-side-effect (cdr fl))))
		 ;; avoid creating temporary for init
		 (push (cons (list 'VAR var) form) initials)
		 (let* ((lcl (next-lcl))
			(loc (list 'LCL lcl)))
		   (do-decl (make-var) lcl)
		   (push (cons loc form) initials)
		   (push (cons var loc) bindings)))))

    (do ((vl vars (rest vl))
         (fl forms (rest fl))
         (form) (var)
         (prev-ss nil) (used t t))
        ((endp vl))
      (declare (type var var))
      (setq form (first fl)
            var (first vl))
      (when (and (local var)
		 (setq used (not (discarded var form body))))
	(do-decl var (next-lcl)))
      (when used
	(if (unboxed var)
	    (push (cons (list 'VAR var) form) initials)	; nil (ccb)
	    ;; LEXICAL, SPECIAL, GLOBAL or OBJECT
	    (case (car form)
	      (LOCATION
	       (if (can-be-replaced var body)
		   (setf (var-kind var) 'REPLACED
			 (var-loc var) (third form))
		   (push (cons var (third form)) bindings)))
	      (VAR
	       (let* ((vref1 (third form))
		      (var1 (car vref1)))
		 (cond ((or (var-changed-in-forms var1 (cdr fl))
			    (and (member (var-kind var1) '(SPECIAL GLOBAL)
					 :test #'eq)
				 (member (var-name var1) prev-ss :test #'eq)))
			(do-init var form fl))
		       ((and (can-be-replaced var body)
			     (member (var-kind var1) '(LEXICAL REPLACED OBJECT)
				     :test #'eq)
			     (not (var-ref-ccb var1))
			     (not (member var1
					  (info-changed-vars (second body)))))
			(setf (var-kind var) 'REPLACED
			      (var-loc var) (cons 'VAR vref1)))
		       (t (push (cons var (cons 'VAR vref1)) bindings)))))
	      (t (do-init var form fl))))
	(unless env-grows
	  (setq env-grows (var-ref-ccb var))))
      (when (eq (var-kind var) 'SPECIAL) (push (var-name var) prev-ss))))

  (when (env-grows env-grows)
    (unless block-p
      (wt-nl "{ ") (setq block-p t))
    (let ((env-lvl *env-lvl*))
      (wt "object env" (incf *env-lvl*) " = env" env-lvl ";")))

  ;; eval INITFORM's and bind variables
  (dolist (init (nreverse initials))
    (let ((*destination* (car init))
	  (*lcl* *lcl*))
      (c2expr* (cdr init))))
  ;; bind LET variables
  (dolist (binding (nreverse bindings))
    (bind (cdr binding) (car binding)))

  (c2expr body)
  (when block-p (wt-nl "}"))
  )

(defun env-grows (possibily)
  ;; if additional closure variables are introduced and this is not
  ;; last form, we must use a new env.
  (and possibily
       (plusp *env*)
       (dolist (exit *unwind-exit*)
	 (case exit
	   (RETURN (return NIL))
	   (BDS-BIND)
	   (t (return T))))))

(defun c1let* (args &aux (forms nil) (vars nil) (vnames nil)
                    (setjmps *setjmps*)
                    ss is ts body other-decls
                    (info (make-info)) (*vars* *vars*))
  (when (endp args) (too-few-args 'let* 1 0))

  (multiple-value-setq (body ss ts is other-decls) (c1body (cdr args) nil))
  (c1add-globals ss)

  (dolist (x (car args))
    (cond ((symbolp x)
           (let ((v (c1make-var x ss is ts)))
	     (push x vnames)
	     (push (default-init (var-type v)) forms)
	     (push v vars)
	     (push v *vars*)))
          ((not (and (consp x) (or (endp (cdr x)) (endp (cddr x)))))
           (cmperr "The variable binding ~s is illegal." x))
          (t (let* ((v (c1make-var (car x) ss is ts))
		    (form (if (endp (cdr x))
			      (default-init (var-type v))
			      (and-form-type (var-type v)
					     (c1expr* (second x) info)
					     (second x)))))
	       ;; :read-only variable handling.
;	       (when (read-only-variable-p (car x) ts)
;		     (setf (var-type v) (info-type (second form))))
	       (push (car x) vnames)
	       (push form forms)
	       (push v vars)
	       (push v *vars*)))))

  (check-vdecl vnames ts is)
  (setq body (c1decl-body other-decls body))
  (add-info info (second body))
  (setf (info-type info) (info-type (second body)))

  ;; since the body may produce type constraints on variables,
  ;; do it again:
  (let (used-vars used-forms)
    (do ((vs (setq vars (nreverse vars)) (cdr vs))
	 (fs (nreverse forms) (cdr fs))
	 (var) (form))
	((null vs))
      (setq var (car vs)
	    form (and-form-type (var-type var) (car fs) (cadar args)))
      ;; Automatic treatement for READ-ONLY variables:
      (let ((rest-forms (cons body (cdr fs))))
	(unless (var-changed-in-forms var rest-forms)
	  (let ((type (info-type (second form))))
	    (setf (var-type var) type)
	    (update-var-type var type rest-forms))))
      (check-vref var)

      ;;  (let* ((v1 e1) (v2 e2) (v3 e3)) (expr e4 v2 e5))
      ;;  can become
      ;;  (let* ((v1 e1) (v3 e3)) (expr e4 e2 e5))
      ;;  provided
      ;;  - v2 appears only once
      ;;  - v2 appears only in body
      ;;  - e2 does not affect v1 nor e3, e3 does not affect e2
      ;;  - e4 does not affect e2

      (if (and (= 1 (var-ref var))
	       (member var (info-referred-vars (second body)))
	       (or (and (null (cdr fs))	; last variable
			;; its form does not affect previous variables
			(let ((tforms (list (car fs))))
			  (dolist (v vars)
			    (when (eq v var) (return t))
			    (when (var-changed-in-forms v tforms)
			      (return nil)))))
		   (not (args-cause-side-effect fs)))
	       (catch var
		 (replaceable var body)))

	  (locally (declare (notinline nsubst))
	    (setq body
		  (nsubst-if form #'(lambda (x)
				      (and (listp x)
					   (eq (first x) 'VAR)
					   (eq var (first (third x)))))
			     body)))
	  (progn
	    (push var used-vars)
	    (push form used-forms)
	    (when (member (info-type (second form))
			  '(FIXNUM CHARACTER LONG-FLOAT SHORT-FLOAT)
			  :test #'eq)
	      (incf (var-ref var))))))		; force unboxing

    (unless (eql setjmps *setjmps*) (setf (info-volatile info) t))
    (list 'LET* info (nreverse used-vars) (nreverse used-forms) body)))

;; should check whether a form before var causes a side-effect
;; exactly one occurrence of var is present in forms
(defun replaceable (var form)
  (case (car form)
    (VAR
     (if (eq var (first (third form)))
	 (throw var T)
	 T))
    ((LOCATION SYS:STRUCTURE-REF) T)
    (CALL-GLOBAL
     (dolist (arg (fourth form) T)
       (when (or (not (replaceable var arg))
		 (args-cause-side-effect (list arg)))
	 (return nil))))
    (SETQ (replaceable var (fourth form)))))

(defun c2let* (vars forms body
                    &aux (block-p nil)
                    (*unwind-exit* *unwind-exit*)
		    (*env* *env*)
		    (*env-lvl* *env-lvl*) env-grows)
  (declare (type boolean block-p))

  (do ((vl vars (cdr vl))
       (fl forms (cdr fl))
       (var) (form) (kind))
      ((endp vl))
    (declare (type var var))
    (setq form (car fl)
          var (car vl)
          kind (local var))
    (unless (unboxed var)
      ;; LEXICAL, SPECIAL, GLOBAL or OBJECT
      (case (car form)
        (LOCATION
         (when (can-be-replaced* var body (cdr fl))
           (setf (var-kind var) 'REPLACED
                 (var-loc var) (third form))))
        (VAR
         (let* ((vref1 (third form))
		(var1 (car vref1)))
           (declare (type var var1))
           (when (and (can-be-replaced* var body (cdr fl))
		      (member (var-kind var1)
			      '(LEXICAL REPLACED OBJECT) :test #'eq)
		      (not (var-ref-ccb var1))
		      (not (var-changed-in-forms var1 (cdr fl)))
		      (not (member var1 (info-changed-vars (second body)))))
             (setf (var-kind var) 'REPLACED
                   (var-loc var) (cons 'VAR vref1))))))
      (unless env-grows
	(setq env-grows (var-ref-ccb var))))
    (when (and kind (not (eq (var-kind var) 'REPLACED)))
      (setf (var-loc var) (next-lcl))
      (wt-nl) (unless block-p (wt "{") (setq block-p t))
      (wt *volatile* (register var) (rep-type kind))
      (wt-lcl (var-loc var)) (wt ";")
      (wt-comment (var-name var)))
    )

  (when (env-grows env-grows)
    (unless block-p
      (wt-nl "{ ") (setq block-p t))
    (let ((env-lvl *env-lvl*))
      (wt "object env" (incf *env-lvl*) " = env" env-lvl ";")))

  (do ((vl vars (cdr vl))
       (fl forms (cdr fl))
       (var nil) (form nil))
      ((null vl))
    (declare (type var var))
    (setq var (car vl)
	  form (car fl))
    (case (var-kind var)
      ((FIXNUM CHARACTER LONG-FLOAT SHORT-FLOAT OBJECT) ; (local var)
       (let ((*destination* `(VAR ,var))) ; nil (ccb)
	 (c2expr* form)))
      (REPLACED)
      (t (case (car form)
	   (LOCATION (bind (third form) var))
	   (VAR (bind (cons 'VAR (third form)) var))
	   (t (bind-init var form))))))

  (c2expr body)

  (when block-p (wt-nl "}"))
  )

(defun discarded (var form body &aux last)
  (labels ((last-form (x)
	     (case (car x)
	       (PROGN
		 (last-form (car (last (third x)))))
	       ((LET LET* FLET LABELS BLOCK CATCH)
		(last-form (car (last x))))
	       (VAR (car (third x)))
	       (t x))))
    (and (not (args-cause-side-effect (list form)))
	 (or (< (var-ref var) 1)
	     (and (= (var-ref var) 1)
		  (eq var (last-form body))
		  (eq 'TRASH *destination*))))))

(defun can-be-replaced (var body)
  (declare (type var var))
  (and (eq (var-kind var) 'OBJECT)
       (< (var-ref var) *register-min*)
       (not (member var (info-changed-vars (second body))))))
#|  (and (or (eq (var-kind var) 'LEXICAL)
	   (and (eq (var-kind var) 'OBJECT)
		(< (var-ref var) *register-min*)))
       (not (var-ref-ccb var))
       (not (member var (info-changed-vars (second body)))))
|#

(defun can-be-replaced* (var body forms)
  (declare (type var var))
  (and (can-be-replaced var body)
       (dolist (form forms t)
         (when (member var (info-changed-vars (second form)))
               (return nil)))))

;;; ----------------------------------------------------------------------

(setf (get 'LET 'C1SPECIAL) #'c1let)
(setf (get 'LET 'C2) 'c2let)
(setf (get 'LET* 'C1SPECIAL) #'c1let*)
(setf (get 'LET* 'C2) 'c2let*)
