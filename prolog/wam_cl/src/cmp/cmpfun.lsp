;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi and William F. Schelter.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPFUN  Library functions.

(in-package 'compiler)

(defvar *princ-string-limit* 80)

(defun c1princ (args &aux stream (info (make-info)))
  (when (endp args) (too-few-args 'PRINC 1 0))
  (unless (or (endp (cdr args)) (endp (cddr args)))
	  (too-many-args 'PRINC 2 (length args)))
  (setq stream (if (endp (cdr args))
		   (c1nil)
		   (c1expr* (second args) info)))
  (if (and (or (and (stringp (car args))
		    (<= (length (car args)) *princ-string-limit*))
	       (characterp (car args)))
	   (or (endp (cdr args))
	       (and (eq (car stream) 'VAR)
		    (member (var-kind (car (third stream)))
			    '(GLOBAL SPECIAL) :test #'eq))))
      (list 'PRINC info (car args)
	    (if (endp (cdr args)) nil (var-loc (caaddr stream)))
	    stream)
      (list 'CALL-GLOBAL info 'PRINC
	    (list (c1expr* (car args) info) stream))))

(defun c2princ (string vv-index stream)
  (cond ((eq *destination* 'TRASH)
	 (cond ((characterp string)
		(wt-nl "princ_char(" (char-code string))
		(if (null vv-index) (wt ",Cnil")
		    (wt ",symbol_value(VV[" vv-index "])"))
		(wt ");"))
	       ((= (length string) 1)
		(wt-nl "princ_char(" (char-code (aref string 0)))
		(if (null vv-index) (wt ",Cnil")
		    (wt ",symbol_value(VV[" vv-index "])"))
		(wt ");"))
	       (t
		(wt-nl "princ_str(\"")
		(dotimes (n (length string))
		  (declare (fixnum n))
		  (let ((char (schar string n)))
		       (cond ((char= char #\\) (wt "\\\\"))
			     ((char= char #\") (wt "\\\""))
			     ((char= char #\Newline) (wt "\\n"))
			     (t (wt char)))))
		(wt "\",")
		(if (null vv-index) (wt "Cnil")
		    (wt "symbol_value(VV[" vv-index "])"))
		(wt ");")))
	 (unwind-exit nil))
	((eql string #\Newline) (c2call-global 'TERPRI (list stream) nil t))
	(t (c2call-global
	    'PRINC
	    (list (list 'LOCATION
			(make-info :type
			  (if (characterp string) 'CHARACTER 'STRING))
			(list 'VV (add-object string)))
		  stream) nil t))))

(defun c1terpri (args &aux stream (info (make-info)))
  (unless (or (endp args) (endp (cdr args)))
	  (too-many-args 'TERPRI 1 (length args)))
  (setq stream (if (endp args)
		   (c1nil)
		   (c1expr* (car args) info)))
  (if (or (endp args)
	  (and (eq (car stream) 'VAR)
	       (member (var-kind (car (third stream)))
		       '(GLOBAL SPECIAL) :test #'eq)))
      (list 'PRINC info #\Newline
	    (if (endp args) nil (var-loc (caaddr stream)))
	    stream)
      (list 'CALL-GLOBAL info 'TERPRI (list stream))))

(defun c1apply (args &aux info)
  (when (or (endp args) (endp (cdr args)))
	(too-few-args 'APPLY 2 (length args)))
  (let* ((fun (first args))
	 (funob (c1funob fun))
	lambda-expr lambda-list)
    (setq info (second funob)
	  args (c1args (cdr args) info))
    (if (and (eq (first funob) 'CALL-LAMBDA)
	     (null (second (setq lambda-list ; No optional
				 (third (setq lambda-expr (third funob))))))
	     (null (fourth lambda-list))) ; No keyword
	(c1apply-optimize info
			  (first lambda-list)
			  (third lambda-list)
			  (fifth lambda-expr)
			  args)
	(case (first funob)
	  (ORDINARY
	   (list 'CALL-GLOBAL info 'APPLY (cons (third funob) args)))
	  (CALL-GLOBAL
	   (list 'CALL-GLOBAL info 'APPLY (cons (c1function (cdr fun)) args)))
	  ((CALL-LAMBDA CALL-LOCAL)
	   (list 'APPLY-LAMBDA/LOCAL info funob args)))))
  )

(defun c2apply-lambda/local (funob args)
  (let* ((loc (save-funob funob))
	 (temp *temp*)
	 (*temp* temp)			; allow reuse of TEMP variables
	 (arg (list 'TEMP 0))
	 (narg (list 'LCL (next-lcl)))
	 (is-lambda (eq 'CALL-LAMBDA (first funob))))
    (wt-nl "{ int " narg ", i=0;")
    (dolist (expr args)
      (setf (second arg) (next-temp))
      (let ((*destination* arg)) (c2expr* expr)))
    (wt-nl narg "=length(" arg ");")
    (setf (second arg) temp)		; restart numbering
    (unless is-lambda
      (let* ((fun (third funob))
	     (lex-lvl (fun-level fun))
	     (closure-lvl (when (fun-closure fun) (- *env* (fun-env fun)))))
	(when (plusp lex-lvl)
	  (dotimes (n lex-lvl)
	    (wt-nl "VALUES(i++)=(object)lex" n ";")))
	(when closure-lvl
	  ;; env of local fun is ALWAYS contained in current env (?)
	  ;; so, instead of getting it through the closure env like this:
	  ;; (wt-env (fun-ref-ccb fun)) (wt "->cc.cc_env")
	  ;; we can reach it through ours (and avoid creating closures at all
	  ;; in flet or labels):
	  (wt-nl "VALUES(i++)=(object)env" *env-lvl* ";"))))
    (dotimes (i (1- (length args)))
      (wt-nl "VALUES(i++)=" arg ";")
      (incf (second arg)))
    (wt-nl narg "+=i;")
    (wt-nl "for (; i<" narg ";i++,"arg "=CDR(" arg "))")
    (wt-nl "	VALUES(i)=CAR(" arg ");")
    (if is-lambda
	(c2funcall funob 'ARGS-PUSHED loc narg)
	(c2call-local (cddr funob) 'ARGS-PUSHED narg))
    (wt-nl "}"))
  )

(defun c1apply-optimize (info requireds rest body args
			      &aux (vl nil) (fl nil))
  (do ()
      ((or (endp (cdr args)) (endp requireds)))
    (push (pop requireds) vl)
    (push (pop args) fl))

  (cond ((cdr args)
	 (cmpck (null rest)
		"APPLY passes too many arguments to LAMBDA expression.")
	 (push rest vl)
	 (push (list 'CALL-GLOBAL info 'LIST* args) fl)
	 (list 'LET info (nreverse vl) (nreverse fl) body))
	(t
	 (let ((*vars* *vars*)
	       (temp (or rest (make-var :name (gensym) :kind 'OBJECT
					:ref (length args)))))
	   (push temp *vars*)
	   (push temp vl)
	   (push (car args) fl)
	   (list 'LET info (nreverse vl) (nreverse fl)
		 (list 'LET*
		       (second body)
		       requireds
		       (make-list (length requireds)
				  :initial-element
				  (c1expr `(pop ,(var-name temp))))
		       body))))
	)
  )

(defun c1funcall (args &aux funob (info (make-info)))
  (when (endp args) (too-few-args 'FUNCALL 1 0))
  (setq funob (c1funob (car args)))
  (add-info info (second funob))
  (list 'FUNCALL info funob (c1args (cdr args) info))
  )

(defun c1rplaca (args &aux (info (make-info)))
  (when (or (endp args) (endp (cdr args)))
	(too-few-args 'RPLACA 2 (length args)))
  (unless (endp (cddr args))
	  (too-many-args 'RPLACA 2 (length args)))
  (setq args (c1args args info))
  (list 'RPLACA info args))

(defun c2rplaca (args &aux (*inline-blocks* 0) x y)
  (setq args (inline-args args)
	x (second (first args))
	y (second (second args)))
  (safe-compile
   (wt-nl "if(type_of(" x ")!=t_cons)"
	  "FEwrong_type_argument(Scons," x ");"))
  (wt-nl "CAR(" x ") = " y ";")
  (unwind-exit x)
  (close-inline-blocks)
  )

(defun c1rplacd (args &aux (info (make-info)))
  (when (or (endp args) (endp (cdr args)))
	(too-few-args 'RPLACD 2 (length args)))
  (when (not (endp (cddr args)))
	(too-many-args 'RPLACD 2 (length args)))
  (setq args (c1args args info))
  (list 'RPLACD info args))

(defun c2rplacd (args &aux (*inline-blocks* 0) x y)
  (setq args (inline-args args)
	x (second (first args))
	y (second (second args)))
  (safe-compile
   (wt-nl "if(type_of(" x ")!=t_cons)"
	  "FEwrong_type_argument(Scons," x ");"))
  (wt-nl "CDR(" x ") = " y ";")
  (unwind-exit x)
  (close-inline-blocks)
  )

(defun c1member (args &aux (info (make-info)))
  (when (or (endp args) (endp (cdr args)))
	(too-few-args 'MEMBER 2 (length args)))
  (cond ((endp (cddr args))
	 (list 'MEMBER!2 info 'EQL (c1args args info)))
	((and (eq (third args) :test)
	      (= (length args) 4)       ; Beppe
	      (member (fourth args) '('EQ #'EQ 'EQUAL #'EQUAL
				      'EQUALP #'EQUALP 'EQL #'EQL)
		      :test #'EQUAL))
	 (list 'MEMBER!2 info (second (fourth args))
	       (c1args (list (car args) (second args)) info)))
	(t
	 (list 'CALL-GLOBAL info 'MEMBER (c1args args info)))))

(defun c2member!2 (fun args
		       &aux (*inline-blocks* 0))
  (setq args (coerce-locs (inline-args args) nil))
;  (if (= *space* 3)
      (unwind-exit
       (list 'INLINE nil
	     (case fun
	       (eq "memq(#0,#1)")
	       (eql "memql(#0,#1)")
	       (equal "member(#0,#1)")) args))
#|
	Not worthwile to inline (Beppe)
      (let ((l (next-lcl)))
	(wt-nl "{register object x= " (car args) ",") (wt-lcl l)
	(wt "= " (second args) ";") ; Beppe

	(if *safe-compile*
	  (progn (wt-nl "while(!endp(") (wt-lcl l) (wt "))"))
	  (progn (wt-nl "while(") (wt-lcl l) (wt "!=Cnil)")))
	(if (eq fun 'EQ)
	  (progn (wt-nl "if(x==CAR(") (wt-lcl l) (wt ")){"))
	  (progn (wt-nl "if(" (string-downcase (symbol-name fun))
		   "(x, CAR(") (wt-lcl l) (wt "))){"))) ; Beppe
	(if (and (consp *destination*)
		 (or (eq (car *destination*) 'JUMP-TRUE)
		     (eq (car *destination*) 'JUMP-FALSE)))
	    (unwind-exit t 'JUMP)
	    (unwind-exit (list 'LCL l) 'JUMP))
	(wt-nl "}else ") (wt-lcl l) (wt "=CDR(") (wt-lcl l) (wt ");")
	(unwind-exit nil)
	(wt "}")
	))
|#
  (close-inline-blocks)
  )

(defun c1assoc (args &aux (info (make-info)))
  (when (or (endp args) (endp (cdr args)))
	(too-few-args 'ASSOC 2 (length args)))
  (cond ((endp (cddr args))
	 (list 'ASSOC!2 info 'EQL (c1args args info)))
	((and (eq (third args) ':TEST)
	      (= (length args) 4)       ; Beppe
	      (member (fourth args) '('EQ #'EQ 'EQUAL #'EQUAL
				      'EQUALP #'EQUALP 'EQL #'EQL)
		      :test 'EQUAL))
	 (list 'ASSOC!2 info (second (fourth args))
	       (c1args (list (car args) (second args)) info)))
	(t
	 (list 'CALL-GLOBAL info 'ASSOC (c1args args info)))))

(defun c2assoc!2 (fun args
		      &aux (*inline-blocks* 0))
  (setq args (coerce-locs (inline-args args) nil))
;  (if (= *space* 3)
      (unwind-exit
       (list 'INLINE nil
	     (case fun
	       (eq "assq(#0,#1)")
	       (eql "assql(#0,#1)")
	       (equal "assoc(#0,#1)")
	       (equalp "assqlp(#0,#1)")) args)) ;Beppe
#|
	Not worthwile to inline (Beppe)
      (let ((al (next-lcl)))
	(wt-nl "{register object x= " (car args) ",") (wt-lcl al)
	(wt "= " (second args) ";");Beppe
	(cond (*safe-compile*
	       (wt-nl "while(!endp(") (wt-lcl al) (wt "))")
	       (wt-nl "if(CAR(") (wt-lcl al) (wt ") != Cnil && ")
	       (if (eq fun 'EQ)
		   (progn (wt-nl "x==car(CAR(") (wt-lcl al) (wt "))){"))
		   (progn (wt-nl (string-downcase (symbol-name fun))    
			  "(x,car(CAR(") (wt-lcl al) (wt ")))){"))))
	      (t
	       (wt-nl "while(") (wt-lcl al) (wt "!=Cnil)")
	       (wt-nl "if(CAR(") (wt-lcl al) (wt ") != Cnil && ")
	       (if (eq fun 'EQ)
		 (progn (wt-nl "x==(CAAR(") (wt-lcl al) (wt "))){"))
		 (progn (wt-nl (string-downcase (symbol-name fun)) ; Beppe
			  "(x,CAAR(") (wt-lcl al) (wt "))){")))))
	(if (and (consp *destination*)
		 (or (eq (car *destination*) 'JUMP-TRUE)
		     (eq (car *destination*) 'JUMP-FALSE)))
	    (unwind-exit t 'JUMP)
	    (unwind-exit (list 'CAR al) 'JUMP))
	(wt-nl "}else ") (wt-lcl al) (wt "=CDR(") (wt-lcl al) (wt ");")
	(unwind-exit nil)
	(wt "}")
	))
|#
  (close-inline-blocks)
  )

#|
Use the optimizers. Full inline expansion not worth while.
(defun c1get (args &aux (info (make-info)))
  (when (or (endp args) (endp (cdr args)))
	(too-few-args 'GET 2 (length args)))
  (when (and (not (endp (cddr args))) (not (endp (cdddr args))))
	(too-many-args 'GET 3 (length args)))
  (list 'GET info (c1args args info)))

(defun c2get (args)
  (if *safe-compile*
      (c2call-global 'GET args nil t)
      (let ((*inline-blocks* 0))
	(setq args (coerce-locs (inline-args args) nil))
;	(if (= *space* 3)
	    (unwind-exit
	     (list 'INLINE nil
		   (if (cddr args)
		       "getf(#0->s.s_plist,#1,#2)"
		       "getf(#0->s.s_plist,#1,Cnil)") args))
#|
	Not worthwile to inline (Beppe)
	    (let ((pl (next-lcl)))
	      (wt-nl "{object ") (wt-lcl pl) (wt" =(" (car args) ")->s.s_plist;")
	      (wt-nl " object ind= " (second args) ";")
	      (wt-nl "while(") (wt-lcl pl) (wt "!=Cnil){")
	      (wt-nl "if(CAR(") (wt-lcl pl) (wt ")==ind){")
	      (unwind-exit (list 'CADR pl) 'JUMP)
	      (wt-nl "}else ") (wt-lcl pl) (wt "=CDDR(") (wt-lcl pl)
	      (wt ");}")
	      (unwind-exit (if (cddr args) (third args) nil))
	      (wt "}")
	      ))
|#
	(close-inline-blocks)))
  )

;; I don't feel it is good to replace the list call, but rather
;; usually better the other way around.  We removed c1list
;; because of possible feedback.

(defun c1list-condition (args) (and (= *space* 0) (< (length args) 4))); Beppe

(defun c1list (args)
  (do ((l (reverse args) (cdr l))
       (form nil))
      ((endp l) (c1expr form))
      (setq form (list 'CONS (car l) form))))

(defun c1list* (args)
  (when (endp args) (too-few-args 'LIST* 1 0))
  (setq args (reverse args))
  (do ((l (cdr args) (cdr l))
       (form (car args)))
      ((endp l) (c1expr form))
      (setq form (list 'CONS (car l) form))))
|#


(defun co1nth (args)
  (and (not (endp args))
       (not (endp (cdr args)))
       (endp (cddr args))
       (numberp (car args))
       (<= 0 (car args) 7)
       (c1expr (case (car args)
		     (0 (cons 'CAR (cdr args)))
		     (1 (cons 'CADR (cdr args)))
		     (2 (cons 'CADDR (cdr args)))
		     (3 (cons 'CADDDR (cdr args)))
		     (4 (list 'CAR (cons 'CDDDDR (cdr args))))
		     (5 (list 'CADR (cons 'CDDDDR (cdr args))))
		     (6 (list 'CADDR (cons 'CDDDDR (cdr args))))
		     (7 (list 'CADDDR (cons 'CDDDDR (cdr args))))
		     ))))

(defun co1nthcdr (args)
  (and (not (endp args))
       (not (endp (cdr args)))
       (endp (cddr args))
       (numberp (car args))
       (<= 0 (car args) 7)
       (c1expr (case (car args)
		 (0 (second args))
		 (1 (cons 'CDR (cdr args)))
		 (2 (cons 'CDDR (cdr args)))
		 (3 (cons 'CDDDR (cdr args)))
		 (4 (cons 'CDDDDR (cdr args)))
		 (5 (list 'CDR (cons 'CDDDDR (cdr args))))
		 (6 (list 'CDDR (cons 'CDDDDR (cdr args))))
		 (7 (list 'CDDDR (cons 'CDDDDR (cdr args))))
		 ))))

(defun c1rplaca-nthcdr (args &aux (info (make-info)))
  (when (or (endp args) (endp (cdr args)) (endp (cddr args)))
	(too-few-args 'SYS:RPLACA-NTHCDR 3 (length args)))
  (unless (endp (cdddr args))
	  (too-few-args 'SYS:RPLACA-NTHCDR 3 (length args)))
  (if (and (numberp (second args)) (<= 0 (second args) 10))
      (list 'RPLACA-NTHCDR-IMMEDIATE info
	    (second args)
	    (c1args (list (car args) (third args)) info))
      (list 'CALL-GLOBAL info 'SYS:RPLACA-NTHCDR (c1args args info))))

(defun c2rplaca-nthcdr-immediate (index args
					&aux (*inline-blocks* 0))
  (declare (fixnum index))
  (setq args (coerce-locs (inline-args args) nil))
  (if *safe-compile*
      (progn
       (wt-nl "{object l= ")
       (dotimes (i index) (declare (fixnum i)) (wt "cdr("))
       (wt (car args))
       (dotimes (i index)(declare (fixnum i)) (wt ")"))
       (wt ";")
       (wt-nl "if(type_of(l)!=t_cons)FEwrong_type_argument(Scons,l);")
       (wt-nl "CAR(l)= " (second args) ";}")
       )
      (progn
	(wt-nl "CAR(")
       (dotimes (i index) (declare (fixnum i)) (wt "CDR("))
       (wt (car args))
       (dotimes (i index) (declare (fixnum i)) (wt ")"))
       (wt ")= " (second args) ";")))
  (unwind-exit (second args))
  (close-inline-blocks)
  )

(defun c1list-nth (args &aux (info (make-info)))
  (when (or (endp args) (endp (cdr args)))
	(too-few-args 'SYS:RPLACA-NTHCDR 2 (length args)))
  (unless (endp (cddr args))
	  (too-few-args 'SYS:RPLACA-NTHCDR 2 (length args)))
  (if (and (numberp (car args)) (<= 0 (car args) 10))
      (list 'LIST-NTH-IMMEDIATE info
	    (car args)
	    (c1args (list (second args)) info))
      (list 'CALL-GLOBAL info 'SYS:LIST-NTH (c1args args info))))

(defun c2list-nth-immediate (index args &aux (l (next-lcl))
					     (*inline-blocks* 0))
  (declare (fixnum index))
  (setq args (coerce-locs (inline-args args) nil))
  (wt-nl "{object ") (wt-lcl l) (wt "= ")
  (if *safe-compile*
      (progn
       (dotimes (i index) (declare (fixnum i)) (wt "cdr("))
       (wt (car args))
       (dotimes (i index) (declare (fixnum i)) (wt ")"))
       (wt ";")
       (wt-nl "if(type_of(") (wt-lcl l) (wt ")!=t_cons)")
       (wt-nl " FEwrong_type_argument(Scons,") (wt-lcl l) (wt ");")
       )
      (progn
       (dotimes (i index) (declare (fixnum i)) (wt "CDR("))
       (wt (car args))
       (dotimes (i index) (declare (fixnum i)) (wt ")"))
       (wt ";")))
  (unwind-exit (list 'CAR l))
  (wt "}")
  (close-inline-blocks)
  )

;----------------------------------------------------------------------

(defun co1ash (args)
  (let ((shamt (second args)) type fun)
    (when (cond ((and (constantp shamt)
		      (sys::fixnump (setq shamt (eval shamt))))
		 (setq fun (if (< shamt 0) 'SHIFT>> 'SHIFT<<)))
		((and (consp shamt)
		      (eq (car shamt) 'THE)
		      (or (subtypep (setq type (second shamt))
				    '(INTEGER 0 100))
			  (and (boundp 'SYS::*ASH->>*) sys::*ash->>*
			       (subtypep type '(INTEGER -100 0)))))
		 (setq fun
		       ;; it had to be a (the type..)
		       (cond ((subtypep type '(INTEGER 0 100))
			      'SHIFT<<)
			     ((subtypep type '(INTEGER -100 0))
			      'SHIFT>>)))))
      (c1expr (cons fun args)))))

(defun shift>> (a b) (ash a b))
(defun shift<< (a b) (ash a b))
(setf (get 'SHIFT>> 'Lfun) "Lash")
(setf (get 'SHIFT<< 'Lfun) "Lash")

;----------------------------------------------------------------------

(defun co1boole (args)
   (and (not (endp (cddr args)))
	(endp (cdddr args))
	(let ((op-code (car args)) (info (make-info)))
	  (and (constantp op-code)
	       (sys:fixnump (setq op-code (eval op-code)))
	       `(BOOLE3 ,info ,(c1args (cons op-code (rest args)) info))))))

(defun c2boole3 (args)
  (flet ((coerce-to-fixnums (locs)
	   (do ((l locs (cdr l)))
	       ((null l) locs)
	     (unless (eq 'FIXNUM (caar l))
	       (setf (caar l) 'fixnum-loc)))))
    (let* ((boole-op-arg (third (first args)))
	   (string (ecase (second boole-op-arg)
		     (#. boole-clr "(0)")
		     (#. boole-set "(1)")
		     (#. boole-1 "(#0)")
		     (#. boole-2 "(#1)")
		     (#. boole-c1 "(~(#0))")
		     (#. boole-c2 "(~(#1))")
		     (#. boole-and "((#0) & (#1))")
		     (#. boole-ior "((#0) | (#1))")
		     (#. boole-xor "((#0) ^ (#1))")
		     (#. boole-eqv   "(~((#0) ^ (#1)))")
		     (#. boole-nand "(~((#0) & (#1)))")
		     (#. boole-nor   "(~((#0)|(#1)))")
		     (#. boole-andc1 "((~(#0))&(#1))")
		     (#. boole-andc2 "(((#0))&(~(#1)))")
		     (#. boole-orc1  "(~(#0) | (#1))")
		     (#. boole-orc2  "((#0) | (~(#1)))"))))
      (let ((*inline-blocks* 0))
	(unwind-exit
	 (list 'INLINE-FIXNUM nil string
	       (coerce-to-fixnums (inline-args (rest args)))))
	(close-inline-blocks)))))

;----------------------------------------------------------------------

(defun co1coerce (args &aux expr type (info (make-info)))
  (and args (cdr args) (endp (cddr args))
       (let ((expr (first args))
	     (type (second args)))
	 (and (listp type)
	      (eq (car type) 'QUOTE)
	      (case (second type)
		((CHARACTER STRING-CHAR) (c1expr `(CHARACTER ,expr)))
		(FLOAT (c1expr `(FLOAT ,expr)))
		((SINGLE-FLOAT SHORT-FLOAT) (c1expr `(FLOAT ,expr 0.0S0)))
		((DOUBLE-FLOAT LONG-FLOAT) (c1expr `(FLOAT ,expr 0.0L0))))))))

;----------------------------------------------------------------------
;; turn repetitious cons's into a list*

(defun co1cons (args &aux temp)
  (labels ((cons-to-lista (x)
	     (let ((tem (last x)))
	       (if (and (consp tem)
			(consp (car tem))
			(eq (caar tem) 'CONS)
			(eql (length (cdar tem)) 2))
		   (cons-to-lista (append (butlast x) (cdar tem)))
		   x))))
    (and (eql (length args) 2)
	 (not (eq args (setq temp (cons-to-lista args))))
	 (c1expr (if (equal '(nil) (last temp))
		     (cons 'LIST (butlast temp))
		     (cons 'LIST* temp))))))

;----------------------------------------------------------------------

;; Return the most particular type we can EASILY obtain from x.  
(defun result-type (x)
  (cond ((symbolp x)
	 (info-type (second (c1expr x))))
	((constantp x)
	 (type-filter (type-of x)))
	((and (consp x) (eq (car x) 'the))
	 (type-filter (second x)))
	(t t)))

(defun co1eql (args)
  (when (and (cdr args)
	     (not *safe-compile*)
	     (flet ((replace-constant (lis)
		      (do ((v lis (cdr v))
			   (found) (tem))
			  ((null v) found)
			(when (and (constantp (car v))
				   (or (numberp (setq tem (eval (car v))))
				       (characterp tem)))
			  (setq found t) (setf (car v) tem)))))
	       (replace-constant args)))
    (when (characterp (second args))
      (setq args (reverse args)))
    (when (characterp (car args))
      (let ((c (gensym)))
	(c1expr
	 `(let ((,c ,(second args)))
	   (declare (type ,(result-type (second args)) ,c))
	   (and (characterp ,c)
	    (= (char-code ,(car args))
	     (the fixnum (char-code (the character ,c)))))))))))

;----------------------------------------------------------------------

(defun co1ldb (args &aux (arg1 (first args))
		    (len (integer-length most-positive-fixnum))
		    size pos)
    (and (consp arg1)
	 (eq 'BYTE (car arg1))
	 (integerp (setq size (second arg1)))
	 (integerp (setq pos (third arg1)))
	 (< (+ size pos) len)
	 (subtypep (result-type (second args)) 'FIXNUM)
	 (c1expr `(the fixnum (ldb1 ,size ,pos ,(second args))))
	 ))

(push '((fixnum fixnum fixnum) fixnum nil nil
	"((((~(-1 << (#0))) << (#1)) & (#2)) >> (#1))")
      (get 'ldb1 ':INLINE-ALWAYS))

;----------------------------------------------------------------------

(defun co1vector-push (args) (co1vector-push1 nil args))
(defun co1vector-push-extend (args) (co1vector-push1 t args))
(defun co1vector-push1 (extend args)
  (unless (or *safe-compile*
	      (> *space* 3)
	      (null (cdr args)))
    (let ((*space* 10))
      (c1expr
       `(let* ((.val ,(car args))
	       (.vec ,(second args))
	       (.i (fill-pointer .vec))
	       (.dim (array-total-size .vec)))
	 (declare (fixnum .i .dim)
	  (type ,(result-type (second args)) .vec)
	  (type ,(result-type (car args)) .val))
	 (cond ((< .i .dim)
		(the fixnum (sys::fill-pointer-set .vec (the fixnum (+ 1 .i))))
		(sys::aset .val .vec .i)
		.i)
	       (t ,(when extend
		     `(vector-push-extend .val .vec ,@(cddr args))))))))))

;----------------------------------------------------------------------

(defun c1length (args &aux (info (make-info)) args1)
  (setf (info-type info) 'FIXNUM)
  (if (and (consp (car args))
	   (eq (caar args) 'SYMBOL-NAME)
	   (setq args1 (cdar args))
	   (not (cddr args1)))
      (list 'CALL-GLOBAL info 'SYMBOL-LENGTH (c1args args1 info))
      (list 'CALL-GLOBAL info 'LENGTH (c1args args info))))

(defun co1schar (args)
   (and (listp (car args)) (not *safe-compile*)
	(cdr args)
	(eq (caar args) 'SYMBOL-NAME)
	(c1expr `(aref (the string ,(second (car args)))
		  ,(second args)))))

;;; ----------------------------------------------------------------------

(setf (get 'princ 'C1) 'c1princ)
(setf (get 'princ 'C2) 'c2princ)
(setf (get 'terpri 'C1) 'c1terpri)

(setf (get 'apply 'C1) 'c1apply)
(setf (get 'apply-lambda/local 'C2) 'c2apply-lambda/local)
(setf (get 'funcall 'C1) 'c1funcall)

(setf (get 'rplaca 'C1) 'c1rplaca)
(setf (get 'rplaca 'C2) 'c2rplaca)
(setf (get 'rplacd 'C1) 'c1rplacd)
(setf (get 'rplacd 'C2) 'c2rplacd)

(setf (get 'member 'C1) 'c1member)
(setf (get 'member!2 'C2) 'c2member!2)
(setf (get 'assoc 'C1) 'c1assoc)
(setf (get 'assoc!2 'C2) 'c2assoc!2)

(setf (get 'nth 'C1CONDITIONAL) 'co1nth)
(setf (get 'nthcdr 'C1CONDITIONAL) 'co1nthcdr)
(setf (get 'sys:rplaca-nthcdr 'C1) 'c1rplaca-nthcdr)
(setf (get 'rplaca-nthcdr-immediate 'C2) 'c2rplaca-nthcdr-immediate)
(setf (get 'sys:list-nth 'C1) 'c1list-nth)
(setf (get 'list-nth-immediate 'C2) 'c2list-nth-immediate)

(setf (get 'ash 'C1CONDITIONAL) 'co1ash)
(setf (get 'boole3 'C2) 'c2boole3)
(setf (get 'boole 'C1CONDITIONAL) 'co1boole)
(setf (get 'coerce 'C1CONDITIONAL) 'co1coerce)
(setf (get 'cons 'C1CONDITIONAL) 'co1cons)
(setf (get 'eql 'C1CONDITIONAL) 'co1eql)		    
(setf (get 'ldb 'C1CONDITIONAL) 'co1ldb)
(setf (get 'length 'C1) 'c1length)
(setf (get 'schar 'C1CONDITIONAL) 'co1schar)
(setf (get 'vector-push 'C1CONDITIONAL) 'co1vector-push)
(setf (get 'vector-push-extend 'C1CONDITIONAL) 'co1vector-push-extend)

#|
(setf (get 'get 'C1) 'c1get)
(setf (get 'get 'C2) 'c2get)
(setf (get 'list 'C1CONDITIONAL) '(c1list-condition . c1list))
(setf (get 'list* 'C1CONDITIONAL) '(c1list-condition . c1list*))
|#
