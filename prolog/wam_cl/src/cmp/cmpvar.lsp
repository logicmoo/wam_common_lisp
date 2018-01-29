;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPVAR  Variables.

(in-package 'compiler)

#|
;;; Use a structure of type vector to avoid creating
;;; normal structures before booting CLOS:
(defstruct (var (:type vector) :named)
  name		;;; Variable name.
  (ref 0 :type fixnum)
		;;; Number of references to the variable (-1 means IGNORE).
		;;; During Pass 2: set below *register-min* for non register.
  ref-ccb	;;; Cross closure reference: T or NIL.
  kind		;;; One of LEXICAL, SPECIAL, GLOBAL, OBJECT, FIXNUM,
  		;;; CHARACTER, LONG-FLOAT, SHORT-FLOAT, or REPLACED (used for
		;;; LET variables).
  (loc 'OBJECT)	;;; During Pass 1: indicates whether the variable can
		;;; be allocated on the c-stack: OBJECT means
		;;; the variable is declared as OBJECT, and CLB means
		;;; the variable is referenced across Level Boundary and thus
		;;; cannot be allocated on the C stack.  Note that OBJECT is
		;;; set during variable binding and CLB is set when the
		;;; variable is used later, and therefore CLB may supersede
		;;; OBJECT.
		;;; During Pass 2:
  		;;; For REPLACED: the actual location of the variable.
  		;;; For FIXNUM, CHARACTER, LONG-FLOAT, SHORT-FLOAT, OBJECT:
  		;;;   the cvar for the C variable that holds the value.
  		;;; For LEXICAL: the frame-relative address for the variable.
		;;; For SPECIAL and GLOBAL: the vv-index for variable name.
  (type t)	;;; Type of the variable.
  ) |#

;;; A special binding creates a var object with the kind field SPECIAL,
;;; whereas a special declaration without binding creates a var object with
;;; the kind field GLOBAL.  Thus a reference to GLOBAL may need to make sure
;;; that the variable has a value.

(defvar *vars* nil)
(defvar *undefined-vars* nil)
(defvar *special-binding* nil)

;;;  Bootstrap problem: proclaim needs this function:
(defun sch-global (name)
  (dolist (var *undefined-vars* nil)
    (declare (type var var))
    (when (eq (var-name var) name)
      (return-from sch-global var))))

;;;
(defvar *register-min* 3)		; criteria for putting in register.
(proclaim '(fixnum *register-min*))

;;;
;;; Check if a variable has been declared as a special variable with a global
;;; value.

(defun check-global (name)
  (let ((x (assoc name *objects*)))
    (when x
      (dolist (tlf *top-level-forms*)
        (when (or (and (eq (car tlf) 'DEFVAR)
		       (= (var-loc (second tlf)) (second x)))
		  (and (eq (car tlf) 'DECLARE)
		       (= (second tlf) (second x))))
          (return tlf))))))

;;; During Pass 1, *vars* holds a list of var objects and the symbols 'CB'
;;; (Closure Boundary) and 'LB' (Level Boundary).  'CB' will be pushed on
;;; *vars* when the compiler begins to process a closure.  'LB' will be pushed
;;; on *vars* when *level* is incremented.
;;; *GLOBALS* holds a list of var objects for those variables that are
;;; not defined.  This list is used only to suppress duplicated warnings when
;;; undefined variables are detected.

(defun c1make-var (name specials ignores types &aux x)
  (let ((var (make-var :name name)))
    (declare (type var var))		; Beppe
    (cmpck (not (symbolp name)) "The variable ~s is not a symbol." name)
    (cmpck (constantp name) "The constant ~s is being bound." name)

    (cond ((or (member name specials) (sys:specialp name)
               (check-global name))	;; added. Beppe 17 Aug 1987
           
           (setf (var-kind var) 'SPECIAL)
           (setf (var-loc var) (add-symbol name))
           (cond ((setq x (assoc name types))
                  (setf (var-type var) (cdr x)))
                 ((setq x (get name 'CMP-TYPE))
                  (setf (var-type var) x)))
           (setq *special-binding* t))
          (t
           (dolist (v types)
             (when (eq (car v) name)
               (case (cdr v)
;                 (OBJECT (setf (var-loc var) 'OBJECT))
                 (REGISTER
                  (incf (var-ref var) 100))
                 (t (setf (var-type var) (cdr v))))))
;           (when (or (null (var-type var))
;                     (eq t (var-type var)))
;             (setf (var-loc var) 'OBJECT))
           ;; :READ-ONLY variable treatment.
;           (when (eq 'READ-ONLY (var-type var))
;             (setf (var-type var) 't))
           (setf (var-kind var) 'LEXICAL))) ; we rely on check-vref to fix it
    (when (member name ignores) (setf (var-ref var) -1)) ; IGNORE.
    var)
  )

(defun check-vref (var)
  (when (and (eq (var-kind var) 'LEXICAL)
             (not (var-ref-ccb var)))
    (when (zerop (var-ref var)) ;;; This field may be -1 (IGNORE). Beppe
        (cmpwarn "The variable ~s is not used." (var-name var)))
    (when (not (eq (var-loc var) 'CLB))
      ;; if the variable can be stored locally, set it var-kind to its type
      (setf (var-kind var)
	    (if (> (var-ref var) 1)
		(let ((type (var-type var)))
		  (cond ((type>= 'FIXNUM type) 'FIXNUM)
			((type>= 'CHARACTER type) 'CHARACTER)
			((type>= 'LONG-FLOAT type) 'LONG-FLOAT)
			((type>= 'SHORT-FLOAT type) 'SHORT-FLOAT)
			(t 'OBJECT)))
		'OBJECT))))
  )

(defun c1var (name)
  (let ((info (make-info))
        (vref (c1vref name)))
    (push (car vref) (info-referred-vars info))
    (push (car vref) (info-local-referred info))
    (setf (info-type info) (var-type (car vref)))
    (list 'VAR info vref))
  )

;;; A variable reference (vref for short) is a list: pair
;;;	( var-object ) Beppe(ccb) ccb-reference )

(defun c1vref (name &aux (ccb nil) (clb nil))
  (dolist (var *vars*)
    (declare (type var var))
    (cond ((eq var 'CB) (setq ccb t))	; closure boundary
          ((eq var 'LB) (setq clb t))	; level boundary
          ((eq (var-name var) name)
           (when (minusp (var-ref var)) ; IGNORE.
             (cmpwarn "The ignored variable ~s is used." name)
             (setf (var-ref var) 0))
           (cond (ccb (setf (var-ref-ccb var) t
			    (var-loc var) 'OBJECT)) ; replace a previous 'CLB
                 (clb (when (eq (var-kind var) 'LEXICAL)
                        (setf (var-loc var) 'CLB))))
           (incf (var-ref var))
           (return-from c1vref (list var))))) ; ccb
  (let ((var (sch-global name)))
    (unless var
      (if *compile-time-too* 
        (unless (sys:specialp name) (undefined-variable name))
        (unless (or (sys:specialp name)
                    (check-global name))
          (undefined-variable name)))
      (setq var (make-var :name name
                          :kind 'GLOBAL
                          :loc (add-symbol name)
                          :type (or (get name 'CMP-TYPE) t)))
      (push var *undefined-vars*))
    (list var))				; ccb
  )

(defun unboxed (var)
  (member (var-kind var) '(FIXNUM CHARACTER LONG-FLOAT SHORT-FLOAT)
          :test #'eq))

(defun local (var)
  (car (member (var-kind var) '(FIXNUM CHARACTER LONG-FLOAT SHORT-FLOAT OBJECT)
	       :test #'eq)))

(defun c2var (vref) (unwind-exit (cons 'VAR vref)))

(defun c2location (loc) (unwind-exit loc))

(defun wt-var (var &aux (var-loc (var-loc var))) ; ccb
  (declare (type var var))
  (case (var-kind var)
    (LEXICAL (cond ;(ccb (wt-env var-loc))
                   ((var-ref-ccb var) (wt-env var-loc))
                   (t (wt-lex var-loc))))
    (SPECIAL (wt "(VV[" var-loc "]->s.s_dbind)"))
    (REPLACED (wt var-loc))
    (GLOBAL (if *safe-compile*
              (wt "symbol_value(VV[" var-loc "])")
              (wt "(VV[" var-loc "]->s.s_dbind)")))
    (t (case (var-kind var)
         (FIXNUM    (wt "MAKE_FIXNUM"))
         (CHARACTER (wt "code_char"))
         (LONG-FLOAT (wt "make_longfloat"))
         (SHORT-FLOAT (wt "make_shortfloat"))
         (OBJECT)
         (t (baboon)))
       (wt "(") (wt-lcl var-loc) (wt ")"))
    ))

(defun set-var (loc var &aux (var-loc (var-loc var))) ;  ccb
  (unless (and (consp loc)
               (eq (car loc) 'VAR)
               (eq (second loc) var)
;               (eq (third loc) ccb)
	       )
    (case (var-kind var)
      (LEXICAL (wt-nl)
               (if (var-ref-ccb var)
		   (wt-env var-loc)
		   (wt-lex var-loc))
               (wt "= " loc ";"))
      (SPECIAL (wt-nl "(VV[" var-loc "]->s.s_dbind)= " loc ";"))
      (GLOBAL
       (if *safe-compile*
         (wt-nl "setq(VV[" var-loc "]," loc ");")
         (wt-nl "(VV[" var-loc "]->s.s_dbind)= " loc ";")))
      (t
       (wt-nl) (wt-lcl var-loc) (wt "= ")
       (case (var-kind var)
         (FIXNUM (wt-fixnum-loc loc))
         (CHARACTER (wt-character-loc loc))
         (LONG-FLOAT (wt-long-float-loc loc))
         (SHORT-FLOAT (wt-short-float-loc loc))
         (OBJECT (wt-loc loc))
         (t (baboon)))
       (wt ";"))
      )))

(defun set-lex (loc lex)
  (unless (and (consp loc)
               (eq (car loc) 'LEX)
               (equal (second loc) lex))
    (wt-nl) (wt-lex lex) (wt "= " loc ";")))
          
(defun wt-lex (lex)
  (if (consp lex)
    (wt "lex" (car lex) "[" (cdr lex) "]")
    (wt-lcl lex)))

;;; reference to variable of inner closure.
(defun wt-env (clv) (wt "*CLV" clv))

;;; ----------------------------------------------------------------------

(defun c1add-globals (globals)
  (dolist (name globals)
    (push (make-var :name name
                    :kind 'GLOBAL
                    :loc (add-symbol name)
                    :type (let ((x (get name 'CMP-TYPE))) (if x x t))
                    )
          *vars*))
  )

(defun c1setq (args)
  (cond ((endp args) (c1nil))
        ((endp (cdr args)) (too-few-args 'SETQ 2 1))
        ((endp (cddr args)) (c1setq1 (car args) (second args)))
        (t
         (do ((pairs args (cddr pairs))
              (forms nil))
             ((endp pairs) (c1expr (cons 'PROGN (nreverse forms))))
             (declare (object pairs))
             (cmpck (endp (cdr pairs))
                    "No form was given for the value of ~s." (car pairs))
             (push (list 'SETQ (car pairs) (second pairs)) forms)
             )))
  )

(defun c1setq1 (name form &aux (info (make-info)) type form1 name1)
  (cmpck (not (symbolp name)) "The variable ~s is not a symbol." name)
  (cmpck (constantp name) "The constant ~s is being assigned a value." name)
  (setq name1 (c1vref name))
  (pushnew (car name1) (info-changed-vars info))
  (pushnew (car name1) (info-referred-vars info))
  (pushnew (car name1) (info-local-referred info))
  (setq form1 (c1expr form))
  (add-info info (second form1))
  (setq type (type-and (var-type (car name1)) (info-type (second form1))))
  (unless type
    (cmpwarn "Type mismatch between ~s and ~s." name form)
    (setq type T))
  (unless (eq type (info-type (second form1)))
    (let ((info1 (copy-info (second form1))))
         (setf (info-type info1) type)
         (setq form1 (list* (car form1) info1 (cddr form1)))))
  (setf (info-type info) type)
  (list 'SETQ info name1 form1)
  )

(defun c2setq (vref form &aux (dest (cons 'VAR vref)))
  (let ((*destination* dest)) (c2expr* form))
  (if (eq (car form) 'LOCATION)
    (c2location (third form))
    (unwind-exit dest))
  )

(defun c1progv (args &aux symbols values (info (make-info)) forms)
  (when (or (endp args) (endp (cdr args)))
        (too-few-args 'PROGV 2 (length args)))
  (setq symbols (c1expr* (car args) info))
  (setq values (c1expr* (second args) info))
  (setq forms (c1progn (cddr args)))
  (add-info info (second forms))
  (list 'PROGV info symbols values forms)
  )

(defun c2progv (symbols values body
                &aux (*unwind-exit* *unwind-exit*))
  (let* ((*lcl* *lcl*)
         (lcl (next-lcl))
         (sym-loc (list 'LCL (next-lcl)))
         (val-loc (list 'LCL (next-lcl))))
    (wt-nl "{object " sym-loc "," val-loc ";")
    (wt-nl "bds_ptr ") (wt-lcl lcl) (wt "=bds_top;")
    (push lcl *unwind-exit*)
    
    (let ((*destination* sym-loc)) (c2expr* symbols))
    
    (let ((*destination* val-loc)) (c2expr* values))
    
    (wt-nl "while(!endp(" sym-loc ")) {")
    (when *safe-compile*
      (wt-nl "if(type_of(CAR(" sym-loc "))!=t_symbol)")
      (wt-nl
       "FEinvalid_variable(\"~s is not a symbol.\",CAR(" sym-loc "));"))
    (wt-nl "if(endp(" val-loc "))bds_bind(CAR(" sym-loc "),OBJNULL);")
    (wt-nl "else{bds_bind(CAR(" sym-loc "),CAR(" val-loc "));")
    (wt-nl val-loc "=CDR(" val-loc ");}")
    (wt-nl sym-loc "=CDR(" sym-loc ");}")
    )
  (c2expr body)
  (wt "}")
  )

(defun c1psetq (args &aux (vrefs nil) (forms nil)
                          (info (make-info :type '(MEMBER NIL))))
  (do ((l args (cddr l)))
      ((endp l))
      (declare (object l))
      (cmpck (not (symbolp (car l)))
             "The variable ~s is not a symbol." (car l))
      (cmpck (constantp (car l))
             "The constant ~s is being assigned a value." (car l))
      (cmpck (endp (cdr l))
             "No form was given for the value of ~s." (car l))
      (let* ((vref (c1vref (car l)))
             (form (c1expr (second l)))
             (type (type-and (var-type (car vref))
                             (info-type (second form)))))
            (unless (equal type (info-type (second form)))
              (let ((info1 (copy-info (second form))))
		(setf (info-type info1) type)
		(setq form (list* (car form) info1 (cddr form)))))
            (push vref vrefs)
            (push form forms)
            (push (car vref) (info-changed-vars info))
            (add-info info (cadar forms)))
      )
  (list 'PSETQ info (nreverse vrefs) (nreverse forms))
  )

(defun var-referred-in-forms (var forms)
  (case (var-kind var)
    ((LEXICAL REPLACED FIXNUM CHARACTER LONG-FLOAT SHORT-FLOAT OBJECT)
     (dolist (form forms nil)
       (when (member var (info-referred-vars (second form)))
	 (return-from var-referred-in-forms t))))
    (t (dolist (form forms nil)
	 (when (or (member var (info-referred-vars (second form)))
		   (info-sp-change (second form)))
	   (return-from var-referred-in-forms t))))
    ))

(defun c2psetq (vrefs forms &aux (*lcl* *lcl*) (saves nil) (blocks 0))
  ;; similar to inline-args
  (do ((vrefs vrefs (cdr vrefs))
       (forms forms (cdr forms))
       (var) (form))
      ((null vrefs))
    (setq var (caar vrefs)
	  form (car forms))
    (if (or (var-changed-in-forms var (cdr forms))
            (var-referred-in-forms var (cdr forms)))
        (case (car form)
          (LOCATION (push (cons var (third form)) saves))
          (otherwise
            (if (local var)
                (let* ((kind (var-kind var))
                       (lcl (next-lcl))
		       (temp (list 'VAR (make-var :kind kind :loc lcl))))
                  (wt-nl "{" *volatile* (rep-type kind)) (wt-lcl lcl) (wt ";")
                  (incf blocks)
                  (let ((*destination* temp)) (c2expr* form))
                  (push (cons var temp) saves))
                (let ((*destination* (list 'TEMP (next-temp))))
                  (c2expr* form)
                  (push (cons var *destination*) saves)))))
        (let ((*destination* (cons 'VAR (car vrefs)))) (c2expr* form))))
  (dolist (save saves) (set-var (cdr save) (car save)))
  (dotimes (i blocks) (wt "}"))
  (unwind-exit nil)
  )

;;; ----------------------------------------------------------------------

(setf (get 'VAR 'C2) 'c2var)
(setf (get 'LOCATION 'C2) 'c2location)
(setf (get 'SETQ 'c1special) 'c1setq)
(setf (get 'SETQ 'C2) 'c2setq)
(setf (get 'PROGV 'c1special) 'c1progv)
(setf (get 'PROGV 'C2) 'c2progv)
(setf (get 'PSETQ 'c1) 'c1psetq)
(setf (get 'PSETQ 'C2) 'c2psetq)

(setf (get 'VAR 'SET-LOC) 'set-var)
(setf (get 'VAR 'WT-LOC) 'wt-var)

(setf (get 'LEX 'SET-LOC) 'set-lex)
(setf (get 'LEX 'WT-LOC) 'wt-lex)
