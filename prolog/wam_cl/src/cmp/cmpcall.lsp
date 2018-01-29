;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPCALL  Function call.

(in-package 'compiler)

(export '*compile-to-linking-call*)

(defvar *compile-to-linking-call* t)

(defun fast-link-proclaimed-type-p (fname &optional args)
  (and *compile-to-linking-call*
       (symbolp fname)
       (and (< (the fixnum (length args)) 10)
            (or (and (get fname 'FIXED-ARGS)
                     (listp args))
                (and
                 (get fname 'PROCLAIMED-FUNCTION)
                 (eq (get fname 'PROCLAIMED-RETURN-TYPE) t)
                 (every #'(lambda (v) (eq v t))
                        (get fname 'PROCLAIMED-ARG-TYPES)))))))

;;; Like macro-function except it searches the lexical environment,
;;; to determine if the macro is shadowed by a function or a macro.
(defun cmp-macro-function (name &aux fd)
  (if (setq fd (c1call-local name))
      (if (eq (first fd) 'CALL-LOCAL) nil fd)
      (macro-function name)))

(defun c1funob (fun &aux fd function)
  ;; fun is an expression appearing in functional position, in particular
  ;; (QUOTE (LAMBDA ..)), (FUNCTION (LAMBDA ..))
  (when (and (consp fun)
	     (symbolp (first fun))
	     (cmp-macro-function (first fun)))
    (setq fun (cmp-macroexpand fun)))
  (if (and (consp fun)
	   (member (first fun) '(QUOTE FUNCTION) :test #'eq)
	   (consp (cdr fun))
	   (endp (cddr fun)))
      (if (symbolp (setq function (second fun)))
	  (if (and (setq fd (c1call-local function))
		   (eq (first fd) 'CALL-LOCAL))
	      fd
	      (list 'CALL-GLOBAL
		    (make-info :sp-change
			       (not (get function 'NO-SP-CHANGE)))
		    function))
	  (if (and (consp function)
		   (eq (first function) 'LAMBDA)
		   (consp (rest function)))
	      (case (first fun)
		(QUOTE
		 (let* ((*vars* nil) (*funs* nil)
			(*blocks* nil) (*tags* nil)
			(lambda-expr (c1lambda-expr (rest function))))
		   (list 'CALL-LAMBDA (second lambda-expr) lambda-expr
			 nil (gen-init-keywords lambda-expr))))
		(FUNCTION
		 ;; Don't create closure boundary like in c1function
		 ;; since funob is used in this same environment
		 (let ((lambda-expr (c1lambda-expr (rest function))))
		   (list 'CALL-LAMBDA (second lambda-expr) lambda-expr
			 nil (gen-init-keywords lambda-expr))))
		)
	      (cmperr "Malformed function: ~A" fun)))
      (let ((x (c1expr fun)) (info (make-info :sp-change t)))
	(add-info info (second x))
	(list 'ORDINARY info x))
      ))

(defun c2funcall (funob args &optional loc narg
			&aux (form (third funob)))
  ;; Usually, ARGS holds a list of forms, which are arguments to the
  ;; function.  If, however, the arguments are on VALUES,
  ;; ARGS should be set to the symbol ARGS-PUSHED, and NARG to a location
  ;; containing the number of arguments.
  ;; LOC is the location of the function object (created by save-funob).
  (case (first funob)
    (CALL-GLOBAL (c2call-global form args loc t narg))
    (CALL-LOCAL (c2call-local (cddr funob) args narg))
    (CALL-LAMBDA (c2call-lambda form args (fifth funob) narg))
    (ORDINARY		;;; An ordinary expression.  In this case, if
              		;;; arguments are already on VALUES, then
              		;;; LOC cannot be NIL.  Callers of C2FUNCALL must be
              		;;; responsible for maintaining this condition.
     (let ((fun (third form)))
       (unless loc
	 (cond ((eq (first form) 'LOCATION) (setq loc fun))
	       ((and (eq (first form) 'VAR)
		     (not (var-changed-in-forms (first fun) args)))
		(setq loc (cons 'VAR fun))) ; i.e. (VAR var) ; ccb
	       (t
		(setq loc (list 'TEMP (next-temp)))
		(let ((*destination* loc)) (c2expr* form)))))

       (let ((*inline-blocks* 0))
	 (c2call-unknown-global nil (if (eq args 'ARGS-PUSHED)
					args
					(inline-args args)) loc nil narg)
	 (close-inline-blocks))))
    (otherwise (baboon))
    ))

(defun c2call-lambda (lambda-expr args cfun &optional narg)
  ;; ARGS is either the list of arguments or 'ARGS-PUSHED
  ;; NARG is a location containing the number of ARGS-PUSHED
  (let ((lambda-list (third lambda-expr))
	(args-pushed (eq 'ARGS-PUSHED args)))
    (if (or (second lambda-list)		;;; Has optional?
	    (third lambda-list)			;;; Has rest?
	    (fourth lambda-list)		;;; Has key?
	    args-pushed				;;; Args already pushed?
	    )
	(let* ((requireds (first lambda-list))
	       (nreq (length requireds)))
	  (unless args-pushed (setq narg (length args)))
	  (wt-nl "{ ")
	  ;; In reverse order, since stack grows downward:
	  (if args-pushed
	      (wt-nl "object *args = &VALUES(" nreq ");")
	      (wt-nl "object args[" (- narg nreq) "];"))
	  (when requireds
	    (wt-nl "object ")
	    (let ((lcl (+ *lcl* nreq)))
	      (declare (fixnum lcl))
	      (do ((args requireds (cdr args)))
		  ((null args))
		(wt-lcl lcl) (when (cdr args) (wt ", ")) (decf lcl)))
	    (wt ";"))
	  (wt-nl "int narg = ")
	  (wt (if args-pushed			;;; Args already pushed?
		  narg
		  (length args)) ";")
	  (if args-pushed
	      (dotimes (i nreq)
		(wt-nl) (wt-lcl (next-lcl)) (wt "=VALUES(" i ");"))
	      (progn
		(dotimes (i nreq)
		  (let ((*destination* `(LCL ,(next-lcl))))
		    (c2expr* (pop args))))
		(do* ((*inline-blocks* 0)
		      (vals (inline-args args) (cdr vals))
		      (i 0 (1+ i)))
		     ((null vals) (close-inline-blocks))
		  (declare (fixnum i))
		  (wt-nl "args[" i "]=" (second (first vals)) ";"))
		(wt-nl "narg = " (- narg nreq) ";")))
	  (c2lambda-expr lambda-list (third (cddr lambda-expr)) cfun
			 nil nil 'CALL-LAMBDA)
	  (wt-nl "}"))
	(c2let (first lambda-list) args (third (cddr lambda-expr))))))

;;;
;;; c2call-global:
;;;   ARGS is either the list of arguments or 'ARGS-PUSHED
;;;   NARG is a location containing the number of ARGS-PUSHED
;;;   LOC is either NIL or the location of the function object
;;;
(defun c2call-global (fname args loc return-type &optional narg)
  (unless (eq 'ARGS-PUSHED args)
    (case fname
      (AREF
       (let ((etype (info-type (cadar args))))
	 (when (or (and (eq etype 'STRING)
			(setq etype 'CHARACTER))
		   (and (consp etype)
			(or (eq (car etype) 'ARRAY)
			    (eq (car etype) 'VECTOR))
			(setq etype (second etype))))
	   (setq etype (type-and return-type etype))
	   (unless etype
	     (cmpwarn "Type mismatch was found in ~s."
		      (cons fname args))
	     (setq etype T))		; assume no information
	   (setf return-type etype))))
      (SYS:ASET				; (sys:aset value array i0 ... in)
       (let ((etype (info-type (cadr (second args)))))
	 (when (or (and (eq etype 'STRING)
			(setq etype 'CHARACTER))
		   (and (consp etype)
			(or (eq (car etype) 'ARRAY)
			    (eq (car etype) 'VECTOR))
			(setq etype (second etype))))
	   (setq etype
		 (type-and return-type
			   (type-and (info-type (cadr (first args)))
				     etype)))
	   (unless etype
	     (cmpwarn "Type mismatch was found in ~s."
		      (cons fname args))
	     (setq etype T))
	   (setf return-type etype)
	   (setf (info-type (cadr (first args))) etype))))))
  (if (and (inline-possible fname)
	   (not (eq 'ARGS-PUSHED args))
	   *tail-recursion-info*
	   (eq (first *tail-recursion-info*) fname)
	   (last-call-p)
	   (tail-recursion-possible)
	   (= (length args) (length (cdr *tail-recursion-info*))))
      ;; Tail-recursive case.
      (let* ((*destination* 'TRASH)
	     (*exit* (next-label))
	     (*unwind-exit* (cons *exit* *unwind-exit*)))
	(c2psetq
	 (mapcar #'(lambda (v) (list v)) ; nil (ccb)
		 (cdr *tail-recursion-info*))
	 args)
	(wt-label *exit*)
	(unwind-no-exit 'TAIL-RECURSION-MARK)
	(wt-nl "goto TTL;")
	(cmpnote "Tail-recursive call of ~s was replaced by iteration."
		 fname))
      ;; else
      (let ((*inline-blocks* 0))
	(call-global fname (if (eq args 'ARGS-PUSHED) args (inline-args args))
		     loc return-type narg)
	(close-inline-blocks))))

;;;
;;; call-global:
;;;   LOCS is either the list of typed locs with the arguments or 'ARGS-PUSHED
;;;   NARG is a location containing the number of ARGS-PUSHED
;;;   LOC is either NIL or the location of the function object
;;;
(defun call-global (fname locs loc return-type narg &aux fd)
  (flet ((emit-linking-call (fname locs narg &aux i)
	   (cond ((null *linking-calls*)
		  (push (list fname 0 (add-symbol fname))
			*linking-calls*)
		  (setq i 0))
		 ((setq i (assoc fname *linking-calls*))
		  (setq i (second i)))
		 (t (setq i (1+ (cadar *linking-calls*)))
		    (push (list fname i (add-symbol fname))
			  *linking-calls*)))
	   (unwind-exit
	    (call-loc fname (format nil "(*LK~d)" i) locs narg))))
    (if (inline-possible fname)
	(cond 
	  ;; Open-codable function call.
	  ((and (not (eq 'ARGS-PUSHED locs))
		(null loc)
		(setq loc (inline-function fname locs return-type)))
	   (unwind-exit (fix-loc loc)))

	  ;; Call to a function whose C language function name is known.
	  ((setq fd (get fname 'Lfun))
	   (wt-h "int " fd "();")
	   (unwind-exit (call-loc fname fd locs narg)))

	  ;; Call to a function defined in the same file.
	  ((setq fd (assoc fname *global-funs*))
	   (unwind-exit (call-loc fname (format nil "L~d" (cdr fd))
				  locs narg)))

	  ;; Linking call
	  (*compile-to-linking-call*	; disabled within init_code
	   (emit-linking-call fname locs narg))

	  (t (c2call-unknown-global fname locs loc t narg)))

	;; else not inline-possible
	(emit-linking-call fname locs narg)))
  )

;;; Functions that use SAVE-FUNOB should rebind *temp*.
(defun save-funob (funob)
  (case (first funob)
    ((CALL-LAMBDA CALL-QUOTE-LAMBDA CALL-LOCAL))
    (CALL-GLOBAL
     (unless (and (inline-possible (third funob))
                  (or (get (third funob) 'Lfun)
                      (assoc (third funob) *global-funs*)))
       (let ((temp (list 'TEMP (next-temp))))
         (if *safe-compile*
           (wt-nl temp "=symbol_function(VV[" (add-symbol (third funob)) "]);")
           (wt-nl temp "=VV[" (add-symbol (third funob)) "]->s.s_gfdef;"))
         temp)))
    (ORDINARY (let* ((temp (list 'TEMP (next-temp)))
                     (*destination* temp))
                (c2expr* (third funob))
                temp))
    (otherwise (baboon))
    ))

;;;
;;; call-loc:
;;;   args are typed locations as produced by inline-args
;;;
(defun call-loc (fname fun args &optional narg-loc)
  (if (eq 'ARGS-PUSHED args)
      (list 'CALL (if (stringp fun)
		      "APPLY"		; call to a C function
		      "apply")		; call to a Lisp function
	    narg-loc (list fun "&VALUES(0)") fname)
      (list 'CALL fun (length args) (coerce-locs args nil) fname)))

(defun wt-call (fun narg args &optional fname)
  (wt fun "(" narg)
  (dolist (arg args)
    (wt "," arg))
  (wt ")")
  (when fname (wt-comment fname)))

;;;
;;; c2call-unknown-global
;;;   LOC is NIL or location containing function
;;;   ARGS is either the list of typed locations for arguments or 'ARGS-PUSHED
;;;   NARG is a location containing the number of ARGS-PUSHED
;;;
(defun c2call-unknown-global (fname args loc inline-p narg)
  (unless loc
    (setq loc
	  (if *compiler-push-events*
	      `(VV ,(add-symbol fname))
	      (format nil (if *safe-compile* 
				  "symbol_function(VV[~d])"
				  "VV[~d]->s.s_gfdef") (add-symbol fname)))))
  (unwind-exit
   (if (eq args 'ARGS-PUSHED)
       (list 'CALL "apply" narg (list loc "&VALUES(0)") fname)
       (call-loc fname "funcall" (cons (list T loc) args)))))

;;; ----------------------------------------------------------------------

(setf (get 'funcall 'c2) #'c2funcall)
(setf (get 'call-lambda 'c2) #'c2call-lambda)
(setf (get 'call-global 'c2) #'c2call-global)

(setf (get 'CALL 'WT-LOC) #'wt-call)
