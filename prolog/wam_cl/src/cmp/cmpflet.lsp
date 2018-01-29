;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPFLET  Flet, Labels, and Macrolet.

(in-package 'compiler)

#|
;;; Use a structure of type vector to avoid creating
;;; normal structures before booting CLOS:
(defstruct (fun (:type vector) :named)
  name			;;; Function name.
  ref			;;; Referenced or not.
  			;;; During Pass1, T or NIL.
       			;;; During Pass2, the vs-address for the
       			;;; function closure, or NIL.
  ref-ccb		;;; Cross closure reference.
 			;;; During Pass1, T or NIL.
	 		;;; During Pass2, the vs-address for the
       			;;; function closure, or NIL.
  cfun			;;; The cfun for the function.
  level			;;; Level of nesting for a function.
  env     		;;; Size of env of closure.
  closure		;;; During Pass1, T if the function is returned
			;;; During Pass2, T if env is used inside the function
  ) |#

(defvar *funs* nil)

;;; During Pass 1, *funs* holds a list of fun objects, local macro definitions
;;; and the symbol 'CB' (Closure Boundary) or 'LB' (Level Boundary).
;;; 'CB' will be pushed on *funs* when the compiler begins to process a closure.
;;; 'LB' will be pushed on *funs* when the compiler begins to process a .
;;; A local macro definition is a list ( macro-name expansion-function).

(defun c1flet (args &aux body ss ts is other-decl info
                         (defs1 nil) (local-funs nil) (closures nil))
  (when (endp args) (too-few-args 'flet 1 0))
  (let ((*funs* *funs*))
    (dolist (def (car args))
      (cmpck (or (endp def)
                 (not (symbolp (car def)))
                 (endp (cdr def)))
             "The function definition ~s is illegal." def)
      (let ((fun (make-fun :name (car def))))
        (push fun *funs*)
        (push (list fun (cdr def)) defs1)))

    (multiple-value-setq (body ss ts is other-decl) (c1body (cdr args) t))
    
    (let ((*vars* *vars*))
      (c1add-globals ss)
      (check-vdecl nil ts is)
      (setq body (c1decl-body other-decl body)))
    (setq info (copy-info (second body))))

  (dolist (def (nreverse defs1))
    (let ((fun (car def)))
      (if (fun-ref-ccb fun)	; was fun-var
        (let ((*vars* (cons 'CB *vars*))
              (*funs* (cons 'CB *funs*))
              (*blocks* (cons 'CB *blocks*))
              (*tags* (cons 'CB *tags*)))
          (let ((lam (c1lambda-expr (second def) (fun-name fun))))
            (add-info info (second lam) 'CB)
            (push (list fun lam) local-funs))) ; was closures

        (when (plusp (fun-ref fun))
          (let ((*vars* (cons 'LB *vars*))
                (*funs* (cons 'LB *funs*))	; for computing level of funob
                (*blocks* (cons 'LB *blocks*))
                (*tags* (cons 'LB *tags*)))
            (let ((lam (c1lambda-expr (second def) (fun-name fun))))
              (add-info info (second lam) 'LB)
              (push (list fun lam) local-funs)))))

      (when (plusp (fun-ref fun))
        (setf (fun-cfun fun) (next-cfun)))))

  (if local-funs ; (or local-funs closures)
      (list 'LOCALS info (nreverse local-funs) body nil) ; had (nreverse closures)
      body)
  )

(defun closure-p (funob)
  (let ((info (second funob)))
    (or (some #'ref-ref-ccb (info-referred-vars info))
	(some #'(lambda (x)
		  (and (fun-p x)
		       (fun-ref-ccb x)))	; was fun-closure
	      (info-local-referred info)))))

(defun c2locals (funs body labels ;; labels is T when deriving from labels
		      &aux block-p
		      (level (if (plusp *lex*) (1+ *level*) *level*))
		      (*env* *env*)
		      (*env-lvl* *env-lvl*) env-grows)
  (dolist (def funs)
    (let* ((fun (car def)) (var (fun-var fun)) previous)
      (setf (fun-closure fun) (closure-p (second def)))
      (when (setq previous (new-local level fun (second def)))
	(setf (fun-level fun) (fun-level previous)
	      (fun-env fun) (fun-env previous)))
      (when var			; the function is returned
        (unless (eq 'LEXICAL (var-kind var))
          (setf (var-loc var) (next-lcl))
          (unless block-p
            (setq block-p t) (wt-nl "{ "))
          (wt "object ") (wt-lcl (var-loc var)) (wt ";"))
	(unless env-grows
	  (setq env-grows (var-ref-ccb var))))))

  (when (env-grows env-grows)
    (unless block-p
      (wt-nl "{ ") (setq block-p t))
    (let ((env-lvl *env-lvl*))
      (wt "object env" (incf *env-lvl*) " = env" env-lvl ";")))

  (dolist (def funs)
    (let* ((fun (car def)) (var (fun-var fun)))
      (when var
	(let ((closure (list 'MAKE-CCLOSURE fun)))
	  (if labels
	      (progn
		(incf (fun-env fun))	; var is included in the closure env
		(bind nil var)
		(wt-nl) (wt-var var) (wt "=" closure ";"))
	      (bind closure var))))))

  (let ((*level* level))
    (c2expr body))
  (when block-p (wt-nl "}"))
  )

(defun c1labels (args &aux body ss ts is other-decl info
                      (defs1 nil) (local-funs nil) (closures nil)
                      (fnames nil) (*funs* *funs*))
  (when (endp args) (too-few-args 'labels 1 0))

  ;;; bind local-functions
  (dolist (def (car args))
    (cmpck (or (endp def) (not (symbolp (car def))) (endp (cdr def)))
           "The local function definition ~s is illegal." def)
    (cmpck (member (car def) fnames)
           "The function ~s was already defined." (car def))
    (push (car def) fnames)
    (let ((fun (make-fun :name (car def))))
      (push fun *funs*)
      (push (list fun nil nil (cdr def)) defs1)))

  (setq defs1 (nreverse defs1))

  ;;; Now DEFS1 holds ( { ( fun-object NIL NIL body ) }* ).

  (multiple-value-setq (body ss ts is other-decl) (c1body (cdr args) t))
  (let ((*vars* *vars*))
    (c1add-globals ss)
    (check-vdecl nil ts is)
    (setq body (c1decl-body other-decl body)))
  (setq info (copy-info (second body)))

  (do ((none-processed) (fun))
      (none-processed)
    (setq none-processed t)
    (dolist (def defs1)
      (setq fun (car def))
      (when (and (plusp (fun-ref fun))	; referred
                 (not (fun-var fun))	; locally and
                 (null (second def)))	; not processed yet
        (setq none-processed nil)
        (setf (second def) t)
        (let ((*vars* (cons 'LB *vars*))
              (*funs* (cons 'LB *funs*)) ; for computing level of funob
              (*blocks* (cons 'LB *blocks*))
              (*tags* (cons 'LB *tags*)))
          (let ((lam (c1lambda-expr (fourth def) (fun-name fun))))
            (add-info info (second lam) 'LB)
            (push (list fun lam) local-funs)))))
    )

  (do ((none-processed) (fun))
      (none-processed)
    (setq none-processed t)
    (dolist (def defs1)
      (setq fun (car def))
      (when (and (fun-ref-ccb fun)	; referred across closure (was fun-var)
                 (null (third def)))	; and not processed yet
        (setq none-processed nil)
        (setf (third def) t)
        (when (second def)
          ;; also processed as local, e.g.:
	  ;; (defun foo (z) (labels ((g () z) (h (y) #'g)) (list (h z) (g))))
          (setq local-funs (delete fun local-funs :key #'car)))
        (let ((*vars* (cons 'CB *vars*))
              (*funs* (cons 'CB *funs*))
              (*blocks* (cons 'CB *blocks*))
              (*tags* (cons 'CB *tags*)))
          (let ((lam (c1lambda-expr (fourth def) (fun-name fun))))
            (add-info info (second lam) 'CB)
            (push (list fun lam) local-funs)))))	; was closures
      )

  (dolist (def defs1)
;    (when (or (plusp (fun-ref (car def))) (fun-ref-ccb (car def)))
          (setf (fun-cfun (car def)) (next-cfun)));)

  (if local-funs ;(or local-funs closures)
      (list 'LOCALS info local-funs body T) ; T means labels
      body)
  )

(defun c1macrolet (args &aux (*funs* *funs*) (*vars* *vars*))
  (when (endp args) (too-few-args 'macrolet 1 0))
  (dolist (def (car args))
    (cmpck (or (endp def) (not (symbolp (car def))) (endp (cdr def)))
           "The macro definition ~s is illegal." def)
    (push (list (car def)
		(sys::expand-defmacro (car def) (second def) (cddr def)))
          *funs*))
  (multiple-value-bind (body ss ts is other-decl)
      (c1body (cdr args) t)
    (c1add-globals ss)
    (check-vdecl nil ts is)
    (c1decl-body other-decl body))
  )

(defun c1call-local (fname &aux (ccb nil))
  (dolist (fun *funs*)
    (cond ((eq fun 'CB) (setq ccb t))
          ((consp fun)			; macro definition
           (when (eq (car fun) fname) (return (second fun))))
          ((eq (fun-name fun) fname)
           (when ccb
	     (setf (fun-ref-ccb fun) t))
	   (incf (fun-ref fun))
           (return (list 'CALL-LOCAL (make-info :local-referred (list fun))
                         fun)))))	; ccb
  )

(defun sch-local-fun (fname)
  ;;; Returns fun-ob for the local function (not locat macro) named FNAME,
  ;;; if any.  Otherwise, returns FNAME itself.
  (dolist (fun *funs* fname)
    (when (and (not (eq fun 'CB))
               (not (consp fun))
               (eq (fun-name fun) fname))
          (return fun)))
  )

(defun c2call-local (fd args &optional narg &aux (fun (car fd)))
  ;; FD is a list ( fun-object ).  ; ccb
  (declare (type fun fun))
  (cond
   ((and (listp args)
         *tail-recursion-info*
         (eq (car *tail-recursion-info*) (fun-name fun))
         (eq *exit* 'RETURN)
         (tail-recursion-possible)
         (= (length args) (length (cdr *tail-recursion-info*))))
    (let* ((*destination* 'TRASH)
           (*exit* (next-label))
           (*unwind-exit* (cons *exit* *unwind-exit*)))
          (c2psetq
	   (mapcar #'(lambda (v) (list v)) ; nil (ccb)
		   (cdr *tail-recursion-info*))
	   args)
          (wt-label *exit*))
    (unwind-no-exit 'TAIL-RECURSION-MARK)
    (wt-nl "goto TTL;")
    (cmpnote "Tail-recursive call of ~s was replaced by iteration."
             (fun-name fun)))
   (t (let ((*inline-blocks* 0)
	    (fun (format nil "LC~d" (fun-cfun fun)))
	    (lex-level (fun-level fun))
	    (closure-p (fun-closure fun))
	    (fname (fun-name fun)))
	(unwind-exit
	 (if (eq 'ARGS-PUSHED args)
	     (list 'CALL-LOCAL "APPLY" lex-level closure-p
		   (list fun "&VALUES(0)") narg fname)
	     (list 'CALL-LOCAL fun lex-level closure-p
		   (coerce-locs (inline-args args) nil) narg fname)))
	(close-inline-blocks))))
  )

(defun wt-call-local (fun lex-lvl closure-p args narg fname)
  ;; if NARG is non-NIL it is location containing narg
  (wt fun "(")
  (if narg
      (wt-loc narg)
      (progn
	(when (plusp lex-lvl)
	  (dotimes (n lex-lvl)
	    (wt "lex" n ",")))
	(wt (length args))
	(when closure-p
	  ;; env of local fun is ALWAYS contained in current env (?)
	  (wt ", env" *env-lvl*))))
  (dolist (arg args)
    (wt "," arg))
  (wt ")")
  (when fname (wt-comment fname)))


;;; ----------------------------------------------------------------------

(setf (get 'FLET 'C1SPECIAL) 'c1flet)
(setf (get 'LABELS 'C1SPECIAL) 'c1labels)
(setf (get 'MACROLET 'C1SPECIAL) 'c1macrolet)

(setf (get 'LOCALS 'c2) 'c2locals)	; replaces both c2flet and c2lables
;(setf (get 'flet 'c2) 'c2flet)
;(setf (get 'labels 'c2) 'c2labels)
;;; c2macrolet is not defined, because MACROLET is replaced by PROGN
;;; during Pass 1.
(setf (get 'CALL-LOCAL 'C2) 'c2call-local)

(setf (get 'CALL-LOCAL 'WT-LOC) #'wt-call-local)
