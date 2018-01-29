;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPMULT  Multiple-value-call and Multiple-value-prog1.

(in-package 'compiler)

(defun c1multiple-value-call (args &aux info funob)
  (when (endp args) (too-few-args 'multiple-value-call 1 0))
  (cond ((endp (rest args)) (c1funcall args))
        (t (setq funob (c1funob (first args)))
           (setq info (copy-info (second funob)))
           (setq args (c1args (rest args) info))
           (list 'MULTIPLE-VALUE-CALL info funob args)))
  )

(defun c2multiple-value-call (funob forms)
  (let ((tot (list 'LCL (next-lcl)))
	(nr (list 'LCL (next-lcl)))
        (loc (save-funob funob)))
    (wt-nl "{ int " nr "," tot ";")
    (let ((*destination* (list 'VALUES tot)))
	(c2expr* (first forms)))
    (dolist (form (rest forms))
      (wt-nl "MV_SAVE(" tot ");")
      (let ((*destination* (list 'VALUES nr)))
	(c2expr* form))
      (wt-nl "MV_SHIFT(" nr "," tot ");")
      (wt-nl "MV_RESTORE(" tot ");")
      (wt-nl tot "+=" nr ";"))
    (c2funcall funob 'ARGS-PUSHED loc tot)
    (wt "}"))
  )

(defun c1multiple-value-prog1 (args &aux (info (make-info)) form)
  (when (endp args) (too-few-args 'multiple-value-prog1 1 0))
  (setq form (c1expr* (first args) info))
  (setq args (c1args (rest args) info))
  (list 'MULTIPLE-VALUE-PROG1 info form args)
  )

(defun c2multiple-value-prog1 (form forms)
  (if (eq 'TRASH *destination*)
      ;; dont bother saving values
      (c2progn (cons form forms))
      (let* ((nr (list 'LCL (next-lcl)))
	     (VALUES_nr (list 'VALUES nr)))
	(wt-nl "{ int " nr ";")
	(let ((*destination* VALUES_nr)) (c2expr* form))
	(wt-nl "MV_SAVE(" nr ");")
	(dolist (form forms)
	  (let ((*destination* 'TRASH)) (c2expr* form)))
	(wt-nl "MV_RESTORE(" nr ");")
	(unwind-exit VALUES_nr)
	(wt "}")))
  )

;;; Beppe:
;;; this is the WRONG way to handle 1 value problem.
;;; should be done in c2values, so that (values (truncate a b)) can
;;; be used to restrict to one value, so we would not have to warn
;;; if this occurred in a proclaimed fun.

(defun c1values (args &aux (info (make-info)))
  (if (and args (null (rest args)))
    ;; unnecessary code is produced for expression (values nil)
    (c1expr (first args))
    (progn
      (setq args (c1args args info))
      (list 'VALUES info args))))

(defun c2values (forms)
  (when (and (eq *destination* 'RETURN-OBJECT)
             (rest forms)
             (consp *current-form*)
             (eq 'DEFUN (first *current-form*)))
    (cmpwarn "Trying to return multiple values. ~
              ~%;But ~a was proclaimed to have single value.~
              ~%;Only first one will be assured."
             (second *current-form*)))
  (let ((nv (length forms)))
    (declare (fixnum nv))
    (case nv
      (0 (unwind-exit '(VALUES 0)))
      (1 (c2expr (first forms)))
      (t (let ((*inline-blocks* 0))
	   ;; reverse args to avoid clobbering VALUES(0)
	   (do ((vl (nreverse (inline-args forms)) (rest vl))
		(i (1- (length forms)) (1- i)))
	       ((null vl))
	     (declare (fixnum i))
	     (wt-nl "VALUES(" i ") = " (second (first vl)) ";"))
	   (unwind-exit (list 'VALUES nv))
	   (close-inline-blocks)))))
  )

(defun c1multiple-value-setq (args &aux (info (make-info)) (vrefs nil))
  (when (or (endp args) (endp (rest args)))
        (too-few-args 'multiple-value-setq 2 0))
  (unless (endp (cddr args))
          (too-many-args 'multiple-value-setq 2 (length args)))
  (dolist (var (first args))
          (cmpck (not (symbolp var)) "The variable ~s is not a symbol." var)
          (cmpck (constantp var)
                 "The constant ~s is being assigned a value." var)
          (setq var (c1vref var))
          (push var vrefs)
          (push (first var) (info-changed-vars info))
          )
  (list 'MULTIPLE-VALUE-SETQ info (nreverse vrefs) (c1expr* (second args) info))
  )

(defun multiple-value-check (vrefs form)
  (and (rest vrefs)
       (eq (first form) 'CALL-GLOBAL)
       (let ((fname (third form)))
         (when (and (symbolp fname)
                    (get fname 'PROCLAIMED-RETURN-TYPE))
           (cmpwarn "~A was proclaimed to have only one return value. ~
                     ~%;;; But you appear to want multiple values." fname)))))

(defun c2multiple-value-setq (vrefs form)
  (multiple-value-check vrefs form)
  (let* ((*lcl* *lcl*)
         (nr (list 'LCL (next-lcl))))
    (wt-nl "{ int " nr ";")
    (let ((*destination* (list 'VALUES nr))) (c2expr* form))
    (do ((vs vrefs (rest vs))
         (i 0 (1+ i))
         (vref))
        ((endp vs))
      (declare (fixnum i))
      (setq vref (first vs))
      (wt-nl "if (" nr ">0) {")
      (set-var (list 'VALUE i) (first vref)) ; (second vref) ccb
      (unless (endp (rest vs)) (wt-nl nr "--;"))
      (wt-nl "} else {") (set-var nil (first vref)) ; (second vref) ccb
      (wt "}"))
    (unless (eq *exit* 'RETURN) (wt-nl))
    (unwind-exit (if vrefs (cons 'VAR (first vrefs)) '(VALUE 0)))
    (wt-nl "}"))
  )

(defun c1multiple-value-bind (args &aux (info (make-info))
                                   (vars nil) (vnames nil) init-form
                                   ss is ts body other-decls
                                   (*vars* *vars*))
  (when (or (endp args) (endp (rest args)))
    (too-few-args 'multiple-value-bind 2 (length args)))

  (multiple-value-setq (body ss ts is other-decls) (c1body (cddr args) nil))

  (c1add-globals ss)

  (dolist (s (first args))
    (push s vnames)
    (push (c1make-var s ss is ts) vars))
  (setq init-form (c1expr* (second args) info))
  (dolist (v (setq vars (nreverse vars))) (push v *vars*))
  (check-vdecl vnames ts is)
  (setq body (c1decl-body other-decls body))
  (add-info info (second body))
  (setf (info-type info) (info-type (second body)))
  (dolist (var vars) (check-vref var))
  (list 'MULTIPLE-VALUE-BIND info vars init-form body)
  )

(defun c2multiple-value-bind (vars init-form body
                                   &aux (labels nil)
                                   (*unwind-exit* *unwind-exit*)
                                   (*env* *env*) env-grows
                                   (*lcl* *lcl*)
                                   (nr (list 'LCL (next-lcl))))

  (multiple-value-check vars init-form)
  (wt-nl "{ int " nr ";")
  (dolist (var vars)
    (declare (type var var))
    (let ((kind (local var)))
      (if kind
        (let ((lcl (next-lcl)))
          (setf (var-loc var) lcl)
          (wt-nl *volatile* (register var) (rep-type kind)) (wt-lcl lcl)
          (wt ";")
	  (wt-comment (var-name var)))
	(unless env-grows (setq env-grows (var-ref-ccb var))))))

  (let ((*destination* (list 'VALUES nr))) (c2expr* init-form))
  (let ((*env-lvl* *env-lvl*))
    (when (setq env-grows (env-grows env-grows))
      (let ((env-lvl *env-lvl*))
	(wt-nl "{ object env" (incf *env-lvl*) " = env" env-lvl ";")))
    (let ((*unwind-exit* *unwind-exit*))
      (do ((vs vars (rest vs))
	   (i 0 (1+ i))
	   (value '(VALUE 0)))
	  ((endp vs))
	(declare (fixnum i))
	(push (next-label) labels)
	(wt-nl "if (" nr "--==0) ") (wt-go (first labels))
	(setf (second value) i)
	(bind value (first vs))))

    (let ((label (next-label)))
      (wt-nl) (wt-go label)

      (setq labels (nreverse labels))

      (let ((*suppress-compiler-warnings* t))
	;; suppress the warning by default-init
	(dolist (v vars)
	  (wt-label (first labels))
	  (pop labels)
	  (bind (third (default-init (var-type v))) v)))

      (wt-label label))

    (c2expr body)
    (when env-grows (wt "}")))
  (wt "}")
  )

;;; ----------------------------------------------------------------------

(setf (get 'multiple-value-call 'c1special) #'c1multiple-value-call)
(setf (get 'multiple-value-call 'c2) #'c2multiple-value-call)
(setf (get 'multiple-value-prog1 'c1special) #'c1multiple-value-prog1)
(setf (get 'multiple-value-prog1 'c2) #'c2multiple-value-prog1)
(setf (get 'values 'c1) #'c1values)
(setf (get 'values 'c2) #'c2values)
(setf (get 'multiple-value-setq 'c1) #'c1multiple-value-setq)
(setf (get 'multiple-value-setq 'c2) #'c2multiple-value-setq)
(setf (get 'multiple-value-bind 'c1) #'c1multiple-value-bind)
(setf (get 'multiple-value-bind 'c2) #'c2multiple-value-bind)
