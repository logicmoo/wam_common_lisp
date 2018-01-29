;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPLOC  Set-loc and Wt-loc.

(in-package 'compiler)

(defvar *destination*)

;;; Valid locations are:
;;;	NIL
;;;	T
;;;	( 'VALUE' i )			VALUES(i)
;;;	( 'VALUES' nr )			nr is number of values 
;;;	( 'VAR' var-object ) ; ccb
;;;	( 'VV' vv-index )
;;;	( 'LCL' lcl )			local variable, type unboxed
;;;	( 'TEMP' temp )			local variable, type object
;;;	( 'CALL' fun narg locs fname )	locs are locations containing the arguments
;;;	( 'CALL-LOCAL' fun lex closure args narg fname )
;;;	( 'INLINE' side-effect-p fun/string locs )	fun is applied to locs
;;;	( 'INLINE-COND' side-effect-p fun/string locs )
;;;	( 'INLINE-FIXNUM' side-effect-p fun/string locs )
;;;	( 'INLINE-CHARACTER' side-effect-p fun/string locs )
;;;	( 'INLINE-LONG-FLOAT' side-effect-p fun/string locs )
;;;	( 'INLINE-SHORT-FLOAT' side-effect-p fun/string locs )
;;;	( 'CAR' lcl )
;;;	( 'CADR' lcl )
;;;	( 'SYMBOL-FUNCTION' vv-index )
;;;	( 'MAKE-CCLOSURE' cfun )
;;;	( 'FIXNUM-VALUE' fixnum-value )
;;;	( 'CHARACTER-VALUE' character-code )
;;;	( 'LONG-FLOAT-VALUE' long-float-value vv )
;;;	( 'SHORT-FLOAT-VALUE' short-float-value vv )
;;;;	These are never passed to unwind-exit:
;;;	( 'FIXNUM->OBJECT' loc )
;;;	( 'CHARACTER->OBJECT' loc )
;;;	( 'LONG-FLOAT->OBJECT' loc )
;;;	( 'SHORT-FLOAT->OBJECT' loc )


;;; Valid *DESTINATION* locations are:
;;;
;;;	'RETURN'	The value is returned from the current function.
;;;	'RETURN-FIXNUM'
;;;	'RETURN-CHARACTER'
;;;	'RETURN-LONG-FLOAT'
;;;	'RETURN-SHORT-FLOAT'
;;;	'RETURN-OBJECT
;;;	'TRASH'			The value may be thrown away.
;;;	( 'VALUES' nr )		Values go to VALUES, their number in nr.
;;;	( 'VAR' var-object ) ; ccb
;;;	( 'LCL' lcl )
;;;	( 'LEX' lex-address )
;;;	( 'BIND' var alternative )	; alternative is optional
;;;	( 'JUMP-TRUE' label )
;;;	( 'JUMP-FALSE' label )
;;;	( 'PUSH-CATCH-FRAME' nr )

(defun set-loc (loc &aux fd (VALUE0 '(VALUE 0)))
  (when (and (consp loc) (member (car loc) '(CALL CALL-LOCAL) :test #'eq))
    (unless (and (consp *destination*) (eq (car *destination*) 'VALUES))
      (wt-nl loc ";")
      (setq loc VALUE0)))
  (case *destination*
    (RETURN
      (unless (and (consp loc) (or (eq (car loc) 'VALUES)
				   (eq (car loc) 'VALUE)))
	;; values are already in place
	(wt-nl "VALUES(0) = " loc ";")))
    (TRASH
     (when (consp loc)
       (case (car loc)
         ((INLINE INLINE-COND INLINE-FIXNUM
                  INLINE-CHARACTER INLINE-LONG-FLOAT
                  INLINE-SHORT-FLOAT)
          (when (second loc)
;;;	Removed (void) specifier, for the Prolog inline code.
;;;         (wt-nl "(void)(") (wt-inline t (third loc) (fourth loc))
            (wt-nl) (wt-inline t (third loc) (fourth loc))
            (wt ";")))
         ((CALL CALL-LOCAL)
          (wt-nl "(void)" loc ";")))))
    (t (cond
        ((or (not (consp *destination*))
             (not (symbolp (car *destination*))))
         (baboon))
        ((setq fd (get (car *destination*) 'SET-LOC))
         (apply fd loc (cdr *destination*)))
        ((setq fd (get (car *destination*) 'WT-LOC))
         (wt-nl) (apply fd (cdr *destination*)) (wt "= " loc ";"))
        (t (baboon)))))
  )

(defun wt-loc (loc &aux fd)
  (cond ((eq loc nil) (wt "Cnil"))
        ((eq loc t) (wt "Ct"))
        ((eq loc 'RETURN) (wt "VALUES(0)"))	; added for last inline-arg
        ((or (not (consp loc))
             (not (symbolp (car loc))))
         (baboon))
        ((setq fd (get (car loc) 'WT-LOC))
	 (apply fd (cdr loc)))
	(t (baboon)))
  )

(defun last-call-p ()
  (member *exit*
          '(RETURN RETURN-FIXNUM RETURN-CHARACTER RETURN-SHORT-FLOAT
            RETURN-LONG-FLOAT RETURN-OBJECT)))

(defun wt-car (lcl) (wt "CAR(") (wt-lcl lcl) (wt ")"))

(defun wt-cdr (lcl) (wt "CDR(") (wt-lcl lcl) (wt ")"))

(defun wt-cadr (lcl) (wt "CADR(") (wt-lcl lcl) (wt ")"))

(defun wt-lcl (lcl) (wt "V" lcl))

(defun wt-vv (vv) (wt "VV[" vv "]"))

(defun wt-lcl-loc (lcl)
  (wt-lcl lcl))

(defun wt-temp (temp)
  (wt "T" temp))

(defun wt-number (value &optional vv)
  (typecase value
    (fixnum (wt "MAKE_FIXNUM(" value ")"))
    (t (wt "VV[" vv "]"))))

(defun wt-character (value &optional vv)
  (wt (format nil "code_char('\\~O')" value)))

(defun wt-fixnum-loc (loc)
  (if (consp loc)
      (case (car loc)
	(VAR
	 (if (eq (var-kind (second loc)) 'FIXNUM)
	     (wt-lcl (var-loc (second loc)))
	     (wt "fix(" loc ")")))
	(INLINE-FIXNUM
	 (wt-inline-loc (third loc) (fourth loc)))
	(FIXNUM-VALUE
	 (wt (second loc)))
	((INLINE-SHORT-FLOAT INLINE-LONG-FLOAT)
	 (wt "((int)(")
	 (wt-inline-loc (third loc) (fourth loc))
	 (wt "))"))
	(t (wt "fix(" loc ")")))
      (wt "fix(" loc ")")))

(defun wt-character-loc (loc)
  (if (consp loc)
      (case (car loc)
	(VAR
	 (if (eq (var-kind (second loc)) 'CHARACTER)
	     (wt-lcl (var-loc (second loc)))
	     (wt "char_code(" loc ")")))
        (INLINE-CHARACTER
         (wt-inline-loc (third loc) (fourth loc)))
        (CHARACTER-VALUE
         (wt (second loc)))
        (t (wt "char_code(" loc ")")))
      (wt "char_code(" loc ")")))

(defun wt-long-float-loc (loc)
  (if (consp loc)
      (case (car loc)
	(VAR
	 (if (eq (var-kind (second loc)) 'LONG-FLOAT)
	     (wt-lcl (var-loc (second loc)))
	     (wt "lf(" loc ")")))
	(INLINE-LONG-FLOAT
	 (wt-inline-loc (third loc) (fourth loc)))
	(LONG-FLOAT-VALUE
	 (wt (second loc)))
	(t (wt "lf(" loc ")")))
      (wt "lf(" loc ")")))

(defun wt-short-float-loc (loc)
  (if (consp loc)
      (case (car loc)
	(VAR
	 (if (eq (var-kind (second loc)) 'SHORT-FLOAT)
	     (wt-lcl (var-loc (second loc)))
	     (wt "sf(" loc ")")))
	(INLINE-SHORT-FLOAT
	 (wt-inline-loc (third loc) (fourth loc)))
	(SHORT-FLOAT-VALUE
	 (wt (second loc)))
	(t (wt "sf(" loc ")")))
      (wt "sf(" loc ")")))

(defun wt-va_arg (call-lambda)
  (if call-lambda
      (wt "args[i]")
      (wt "va_arg(args, object)")))
;(defun set-va_arg (loc) (wt-nl "va_arg(args, object)=" loc ";"))

(defun wt-value (i) (wt "VALUES(" i ")"))

(defun wt-values (nr) (wt "VALUES(0)"))

(defun set-values (loc nr)
  ;; used when *destination* = (VALUES nr)
  (if (and (consp loc)
	   (or (member (car loc) '(CALL CALL-LOCAL) :test #'eq)
	       (when (eq (car loc) 'VALUES)
		 ;; from multiple-value-prog1 or values
		 (setq loc (second loc)))))
      (wt-nl nr "=" loc ";")
      (progn
	(unless (and (consp loc)
		     (member (car loc) '(CALL CALL-LOCAL) :test #'eq))
	  (wt-nl "VALUES(0)="))
	(wt loc ";")
	(wt-nl nr "=1;"))))

(defun wt-keyvars (i) (wt "keyvars[" i "]"))

;;; -----------------------------------------------------------------

(setf (get 'TEMP 'WT-LOC) #'wt-temp)
(setf (get 'LCL 'WT-LOC) #'wt-lcl-loc)
(setf (get 'VV 'WT-LOC) #'wt-vv)
(setf (get 'CAR 'WT-LOC) #'wt-car)
(setf (get 'CDR 'WT-LOC) #'wt-cdr)
(setf (get 'CADR 'WT-LOC) #'wt-cadr)
(setf (get 'FIXNUM-VALUE 'WT-LOC) #'wt-number)
(setf (get 'FIXNUM-LOC 'WT-LOC) #'wt-fixnum-loc) ; used in cmpfun.lsp
(setf (get 'CHARACTER-VALUE 'WT-LOC) #'wt-character)
;(setf (get 'CHARACTER-LOC 'WT-LOC) #'wt-character-loc)
(setf (get 'LONG-FLOAT-VALUE 'WT-LOC) #'wt-number)
;(setf (get 'LONG-FLOAT-LOC 'WT-LOC) #'wt-long-float-loc)
(setf (get 'SHORT-FLOAT-VALUE 'WT-LOC) #'wt-number)
;(setf (get 'SHORT-FLOAT-LOC 'WT-LOC) #'wt-short-float-loc)
(setf (get 'VA-ARG 'WT-LOC) #'wt-va_arg)
;(setf (get 'VA-ARG 'SET-LOC) #'set-va_arg)
(setf (get 'VALUE 'WT-LOC) #'wt-value)
(setf (get 'VALUES 'WT-LOC) #'wt-values)
(setf (get 'VALUES 'SET-LOC) #'set-values)
(setf (get 'KEYVARS 'WT-LOC) #'wt-keyvars)
