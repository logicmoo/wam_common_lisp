;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPINLINE  Open coding optimizer.

(in-package 'compiler)

;;; Pass 1 generates the internal form
;;;	( id  info-object . rest )
;;; for each form encountered.

#|
;;; Use a structure of type vector to avoid creating
;;; normal structures before booting CLOS:
(defstruct (info (:type vector) :named)
  (changed-vars nil)	;;; List of var-objects changed by the form.
  (referred-vars nil)	;;; List of var-objects referred in the form.
  (type t)		;;; Type of the form.
  (sp-change nil)	;;; Whether execution of the form may change
			;;; the value of a special variable.
  (volatile nil)	;;; whether there is a possible setjmp. Beppe
  (local-referred nil)  ;;; directly referenced in the body.
  ) |#

(defvar *info* (make-info))
(defvar *object-types* (let ((x '(OBJECT))) (rplacd x x)))

(defun add-info (to-info from-info &optional boundary)
  (setf (info-changed-vars to-info)
        (union (info-changed-vars from-info)
                (info-changed-vars to-info)))
  (setf (info-referred-vars to-info)
        (union (info-referred-vars from-info)
                (info-referred-vars to-info)))
  (when (info-sp-change from-info)
        (setf (info-sp-change to-info) t))
;  (setf (info-referred-tags to-info)
;	(union (info-referred-tags from-info)
;	       (info-referred-tags to-info)))
  (unless boundary
    (setf (info-local-referred to-info)
	  (union (info-local-referred from-info)
		 (info-local-referred to-info))))
  )

(defun var-changed-in-forms (var forms)
  (declare (type var var))
  (case (var-kind var)
    ((LEXICAL FIXNUM CHARACTER LONG-FLOAT SHORT-FLOAT OBJECT)
     (dolist (form forms)
       (when (member var (info-changed-vars (second form)))
         (return t))))
    (REPLACED (let ((loc (var-loc var)))
		(when (and (consp loc) (eq 'VAR (first loc)))
		  (var-changed-in-forms (second loc) forms))))
    (t (dolist (form forms)
         (when (or (member var (info-changed-vars (second form)))
                   (info-sp-change (second form)))
           (return t)))))
  )

;;; Valid property names for open coded functions are:
;;;  :INLINE-ALWAYS
;;;  :INLINE-SAFE	safe-compile only
;;;  :INLINE-UNSAFE	non-safe-compile only
;;;
;;; Each property is a list of 'inline-info's, where each inline-info is:
;;; ( types { type | boolean } side-effect new-object { string | function } ).
;;;
;;; For each open-codable function, open coding will occur only if there exits
;;; an appropriate property with the argument types equal to 'types' and with
;;; the return-type equal to 'type'.  The third element
;;; is T if and only if side effects may occur by the call of the function.
;;; Even if *DESTINATION* is TRASH, open code for such a function with side
;;; effects must be included in the compiled code.
;;; The forth element is T if and only if the result value is a new Lisp
;;; object, i.e., it must be explicitly protected against GBC.

(defvar *inline-functions* nil)
(defvar *inline-blocks* 0)
;;; *inline-functions* holds:
;;;	(...( function-name . inline-info )...)
;;;
;;; *inline-blocks* holds the number of C blocks opened for declaring
;;; temporaries for intermediate results of the evaluation of inlined
;;; function calls.

;;;
;;; inline-args:
;;;   returns a list of pairs (type loc)
;;;   side effects: emits code for temporary variables
;;;
;;; Whoever calls inline-args must bind *inline-blocks* to 0 and afterwards
;;; call close-inline-blocks
;;;
(defun inline-args (forms &optional types)
  ;; all uses of next-lcl may be eliminated??? Beppe
  (flet ((all-locations (args &aux (res t))
	   (dolist (arg args res)
	     (unless (member (car arg) '(LOCATION VAR SYS:STRUCTURE-REF
					 #+clos SYS:INSTANCE-REF)
			     :test #'eq)
	       (setq res nil))))

	 (form-type (form)
	   (info-type (second form))))

    (do ((forms forms (cdr forms))
	 (form) (locs))
	((endp forms) (nreverse locs))
      (setq form (car forms))
      (case (car form)
	(LOCATION (push (list (form-type form) (third form)) locs))
	(VAR
	 (let ((var (caaddr form)))
	   (if (var-changed-in-forms var (cdr forms))
	       (let ((lcl-loc (list 'LCL (next-lcl)))
		     (var-type (var-kind var)))
		 (wt-nl "{" (rep-type var-type) lcl-loc "= ")
		 (wt-var var) (wt ";")
		 (push (list (form-type form)
			     (if (unboxed var)
				 lcl-loc
				 (list 'T lcl-loc))) ; global proclaimed var.
		       locs)
		 (incf *inline-blocks*))
	       (push (list (form-type form) (cons 'VAR (third form)))
		     locs))))

	(CALL-GLOBAL
	 (let* ((fname (third form))
		(args (fourth form))
		(return-type (info-type (second form)))
		(arg-locs (inline-args args))
		loc)
	   (if (and (inline-possible fname)
		    (not (get fname 'C2)) ; no special treatment
		    (setq loc (inline-function fname arg-locs return-type)))
	     (let* ((arg-type (first loc))
		    (and-type (type-and arg-type return-type))
		    (typed-loc (list and-type (fix-loc loc))))
	       (cond
		 ((and (member arg-type '(FIXNUM LONG-FLOAT
					  SHORT-FLOAT BOOLEAN CHARACTER)
			       :test #'eq)
		       ;; fix to: (bar (+ (bar) y) (bar y))
		       (all-locations (cdr forms)))
		  (push typed-loc locs))
		 ((or (need-to-protect (cdr forms))
		      (and (second loc)	; side-effectp
			   (cdr forms)))
		  ;; if there are side-effects, order of execution matters
		  (let* ((var (make-var :loc (next-lcl)
					:kind (if (member arg-type
							  '(T BOOLEAN)
							  :test #'eq)
						  'OBJECT arg-type)
					:type and-type))
			 (lcl-var (list 'VAR var)))
		    ;; use a variable of type arg-type to save the value
		    ;; if (return-type >= arg-type)
		    ;; then
		    ;;   coerce the value to arg-type
		    ;; otherwise
		    ;;   save the value without coercion and return the
		    ;;   variable tagged with and-type,
		    ;;   so that whoever uses it may coerce it to such type
		    (wt-nl "{" (rep-type arg-type)) (wt-lcl (var-loc var))
		    (wt "= " (if (type>= return-type arg-type)
				  typed-loc loc) ";")
		    (push (list and-type lcl-var) locs)
		    (incf *inline-blocks*)))
		 (t (push typed-loc locs))))

	     (let* ((temp (if (all-locations (cdr forms))
			      'RETURN
			      (list 'TEMP (next-temp))))
		    ;; bindings like c1expr*
		    (*exit* (next-label))
		    (*unwind-exit* (cons *exit* *unwind-exit*))
		    (*lcl* *lcl*)
		    (*temp* *temp*)
		    (*destination* temp))
	       (call-global fname arg-locs nil return-type nil)
	       (wt-label *exit*)
	       (push (list (if (eq 'T return-type)
			       (or (get-return-type fname) 'T)
			       return-type)
			   temp) locs)))))

	(SYS:STRUCTURE-REF
	 (let ((type (form-type form)))
	   (if (args-cause-side-effect (cdr forms))
	       (let* ((temp (list 'TEMP (next-temp)))
		      (*destination* temp))
		 (c2expr* form)
		 (push (list type temp) locs))
	       (push (list type
			   (list 'SYS:STRUCTURE-REF
				 (second (first
					  (inline-args (list (third form)))))
				 (fourth form)
				 (fifth form)))
		     locs))))
	#+clos
	(SYS:INSTANCE-REF
	 (let ((type (form-type form)))
	   (if (args-cause-side-effect (cdr forms))
	       (let* ((temp (list 'TEMP (next-temp)))
		      (*destination* temp))
		 (c2expr* form)
		 (push (list type temp) locs))
	       (push (list type
			   (list 'SYS:INSTANCE-REF
				 (second (first
					  (inline-args (list (third form)))))
				 (fourth form)
				 (fifth form)))
		     locs))))
	(SETQ
	 (let ((vref (third form))
	       (form1 (fourth form)))
	   (let ((*destination* (cons 'VAR vref))) (c2expr* form1))
	   (if (eq (car form1) 'LOCATION)
	       (push (list (form-type form1) (third form1)) locs)
	       (setq forms (list* nil	; discarded at iteration
				  (list 'VAR (second form) vref) (cdr forms))
		     ))))

	(t (let ((temp (if (all-locations (cdr forms))
			   'RETURN
			   (list 'TEMP (next-temp)))))
	     (let ((*destination* temp)) (c2expr* form))
	     (push (list (form-type form) temp) locs))))))
  )

(defun coerce-locs (args types)
  ;; each arg is pair (type location).
  ;; if TYPES is NIL, all types are meant to be T.
  ;; If type matches the corresponding required type, leave arg as is;
  ;; otherwise, if type is simple, replace type with coercion to object;
  ;; otherwise, remove type (WT-LOC will take care of producing an object,
  ;; except for LCLs).
  ;;
  (do ((args args (cdr args))
       (types (or types '(T)) (or (cdr types) '(T)))
       (arg-type) (loc))
      ((null args))
    (setq arg-type (car (member (caar args)
				'(FIXNUM CHARACTER LONG-FLOAT SHORT-FLOAT)
				:test #'eq))
	  loc (second (car args)))
    (if arg-type
	(if (eq arg-type (car types))
	    (when (and (consp loc) (eq 'LCL (car loc)))
	      (setf (car args) loc))
	    (if (consp loc)
		(case arg-type
		  (FIXNUM (if (member (car loc)
				      ;; TEMPs contain object
				      '(VAR TEMP FIXNUM-VALUE INLINE-FIXNUM
					INLINE SI:STRUCTURE-REF)) ; T
			      (setf (car args) loc)
			      (setf (caar args) 'FIXNUM->OBJECT)))
		  (CHARACTER (if (member (car loc)
					 '(VAR TEMP CHARACTER-VALUE INLINE ; T
					   INLINE-CHARACTER SI:STRUCTURE-REF))
				 (setf (car args) loc)
				 (setf (caar args) 'CHARACTER->OBJECT)))
		  (LONG-FLOAT (if (member (car loc)
					  '(VAR TEMP LONG-FLOAT-VALUE ; T
					    INLINE-LONG-FLOAT INLINE
					    SI:STRUCTURE-REF))
				  (setf (car args) loc)
				  (setf (caar args) 'LONG-FLOAT->OBJECT)))
		  (SHORT-FLOAT (if (member (car loc)
					   '(VAR TEMP SHORT-FLOAT-VALUE	; T
					     INLINE-SHORT-FLOAT INLINE
					     SI:STRUCTURE-REF))
				   (setf (car args) loc)
				   (setf (caar args) 'SHORT-FLOAT->OBJECT))))
		(setf (car args) loc)))
	(setf (car args) loc)))
  args)

;;; this function may go away if we replace all uses of inline-? with
;;; just the type name. Or else we could use a single tag INLINE
;;; and put the type in second position, replacing side-effect-p which
;;; is not used from now on.
(defun fix-loc (loc)
  (setf (car loc)
	(case (car loc)
	  (BOOLEAN 'INLINE-COND)
	  (FIXNUM 'INLINE-FIXNUM)
	  (CHARACTER 'INLINE-CHARACTER)
	  (LONG-FLOAT 'INLINE-LONG-FLOAT)
	  (SHORT-FLOAT 'INLINE-SHORT-FLOAT)
	  (otherwise 'INLINE)))
  loc)

(defun destination-type ()
  (if (and (consp *destination*)
	   (member (car *destination*) '(VAR BIND) :test #'eq))
      (var-type (second *destination*))
      T))

;;;
;;; inline-function:
;;;   locs are typed locs as produced by inline-args
;;;   returns NIL if inline expansion of the function is not possible
;;;
(defun inline-function (fname locs return-type)
  ;; Those functions that use INLINE-FUNCTION must rebind
  ;; the variable *INLINE-BLOCKS*.

  (setq return-type (type-and return-type (destination-type)))

  (let* ((ii (get-inline-info fname (mapcar #'car locs) return-type))
	 (fun))
    (when ii
      (setq fun (fifth ii))
      ;; remove coercion where not necessary:
      (coerce-locs locs (first ii))
      (when (and (stringp fun) (char= (char (the string fun) 0) #\@))
	(let ((saves nil))
	  (do* ((i 1 (1+ i))
		(char (char (the string fun) i) (char (the string fun) i)))
	       ((char= char #\;))
	    (declare (fixnum i) (character char))
	    (push (the fixnum (- (char-code char) #.(char-code #\0))) saves))
	  (do ((l locs (cdr l))
	       (n 0 (1+ n))
	       (locs1 nil))
	      ((endp l) (setq locs (nreverse locs1)))
	    (declare (fixnum n))
	    (if (member n saves)
		(let* ((loc1 (car l)) (loc loc1) (coersion nil))
		  (when (and (consp loc1)
			     (member (car loc1)
				     '(FIXNUM CHARACTER
				       LONG-FLOAT SHORT-FLOAT)
				     :test #'eq))
		    (setq coersion (car loc1))
		    (setq loc (second loc1))) ; remove coersion
		  (if
		   (or (eq loc 'RETURN)
		       (member (car loc) '(VAR FIXNUM-VALUE SHORT-FLOAT-VALUE
					   LONG-FLOAT-VALUE CHARACTER-VALUE VV)
			       :test #'eq))
		   (push loc1 locs1)
		   ;; else
		   (let ((lcl (list 'LCL (next-lcl))))
		     (push lcl locs1)
		     (incf *inline-blocks*)
		     (wt-nl "{" (rep-type coersion) lcl "= " loc1 ";"))))
		;; else
		(push (car l) locs1)))))
      (list (second ii)
	    (third ii)
	    fun
	    locs)))
  )

(defun get-inline-info (fname types return-type &aux ii iia)
  (dolist (x *inline-functions*)
    (when (and (eq (car x) fname)
	       (setq ii (inline-type-matches (cdr x) types return-type)))
      (return-from get-inline-info ii)))
  (dolist (x (get fname (if *safe-compile*
			    ':INLINE-SAFE
			    ':INLINE-UNSAFE)))
    (when (setq ii (inline-type-matches x types return-type))
      (return)))
  (dolist (x (get fname ':INLINE-ALWAYS))
    (when (setq iia (inline-type-matches x types return-type))
      (return)))
  (if (and ii iia)
      (if (and (every #'type>= (first ii) (first iia))
	       (type>= (second ii) (second iia)) ; no contravariance here
	       (not (and (every #'equal (first ii) (first iia))
			 (equal (second iia) (second ii))))) iia ii)
      (or ii iia))
  )

(defun inline-type-matches (inline-info arg-types return-type
                                        &aux (rts nil)
					(number-max nil)
					(inline-return-type
					 (second inline-info)))
  ;; In sysfun.lsp optimizers must be listed with most specific cases last.
  (flet ((float-type-max (t1 t2)
	   (if t1
	       (if (or (eq t1 'LONG-FLOAT)
		       (eq t2 'LONG-FLOAT))
		   'LONG-FLOAT
		   (if (or (eq t1 'SHORT-FLOAT)
			   (eq t2 'SHORT-FLOAT))
		       'SHORT-FLOAT
		       'FIXNUM))
	       t2)))
    (if (and (do ((arg-types arg-types (cdr arg-types))
		  (types (car inline-info) (cdr types))
		  (arg-type) (type))
		 ((or (endp arg-types) (endp types))
		  (and (endp arg-types) (endp types)))
	       (setq arg-type (car arg-types)
		     type (car types))
	       (cond ((eq type 'FIXNUM-FLOAT)
		      (cond ((type>= 'FIXNUM arg-type)
			     (push 'FIXNUM rts))
			    ((type>= 'LONG-FLOAT arg-type)
			     (push 'LONG-FLOAT rts))
			    ((type>= 'SHORT-FLOAT arg-type)
			     (push 'SHORT-FLOAT rts))
			    (t (return nil)))
		      ;; compute max of FIXNUM-FLOAT arguments types
		      (setq number-max
			    (float-type-max number-max (first rts))))
		     ((type>= type arg-type)
		      (push type rts))
		     (t (return nil))))
	     (or (eq inline-return-type 'BOOLEAN)
		 (if number-max
		     ;; for arithmetic operators we take the maximal type
		     ;; as possible result type
		     (and (type>= return-type number-max)
			  (type>= number-max inline-return-type))
		     (type>= inline-return-type return-type))))
	(cons (nreverse rts) (cdr inline-info))
	nil))
  )

(defun need-to-protect (forms &aux ii)
  (do ((forms forms (cdr forms))
       (res nil))
      ((or res (endp forms)) res)
    (let ((form (car forms)))
      (declare (object form))
      (case (car form)
	(LOCATION)
	(VAR
	 (when (var-changed-in-forms (car (third form)) (cdr forms))
	   (setq res t)))
	(CALL-GLOBAL
	 (let ((fname (third form))
	       (args (fourth form)))
	   (when (or (not (inline-possible fname))
		     (null (setq ii (get-inline-info
				     fname
				     (mapcar #'(lambda (x)
						 (info-type (second x)))
					     args)
				     (info-type (second form)))))
		     (third ii)
		     (fourth ii)
		     (need-to-protect args))
	     (setq res t))))
	(SYS:STRUCTURE-REF
	 (when (need-to-protect (list (third form)))
	   (setq res t)))
	(t (setq res t)))))
  )

(defun close-inline-blocks ()
  (dotimes (i *inline-blocks*) (declare (fixnum i)) (wt "}")))

(defun wt-inline-loc (fun locs &aux (i 0))
  (declare (fixnum i))
  (cond ((stringp fun)
         (when (char= (char (the string fun) 0) #\@)
           (setq i 1)
           (do ()
               ((char= (char (the string fun) i) #\;) (incf i))
               (incf i)))
         (do ((size (length (the string fun)))
              (char))
             ((>= i size))
           (declare (fixnum size) (character char))
           (setq char (char (the string fun) i))
           (if (char= char #\#)
             (wt-loc (nth (the fixnum (- (char-code (char (the string fun)
                                                          (incf i)))
                                         #.(char-code #\0)))
                          locs))
             (princ char *compiler-output1*))
           (incf i))
         )
        (t (apply fun locs))))

(defun wt-inline (side-effectp fun locs)
  (declare (ignore side-effectp))
  (wt-inline-loc fun locs))

(defun wt-inline-cond (side-effectp fun locs)
  (declare (ignore side-effectp))
  (wt "(") (wt-inline-loc fun locs) (wt "?Ct:Cnil)"))

(defun wt-inline-fixnum (side-effectp fun locs)
  (declare (ignore side-effectp))
  (wt "MAKE_FIXNUM(") (wt-inline-loc fun locs) (wt ")"))

(defun wt-inline-character (side-effectp fun locs)
  (declare (ignore side-effectp))
  (wt "code_char(") (wt-inline-loc fun locs) (wt ")"))

(defun wt-inline-long-float (side-effectp fun locs)
  (declare (ignore side-effectp))
  (wt "make_longfloat(") (wt-inline-loc fun locs) (wt ")"))

(defun wt-inline-short-float (side-effectp fun locs)
  (declare (ignore side-effectp))
  (wt "make_shortfloat(") (wt-inline-loc fun locs) (wt ")"))

(defun args-cause-side-effect (forms &aux ii)
  (dolist (form forms nil)
    (case (car form)
      ((LOCATION VAR SYS:STRUCTURE-REF #+clos SYS:INSTANCE-REF))
      (CALL-GLOBAL
       (let ((fname (third form)))
	 (unless (and (inline-possible fname)
		      (setq ii (get-inline-info
				fname (mapcar #'(lambda (x)
						 (info-type (second x)))
					      (fourth form))
				(info-type (second form))))
		      (not (third ii))	; no side-effectp
		      )
	   (return t))))
      (otherwise (return t)))))

(defun list-inline (&rest x)
       (wt "list(" (length x)) (dolist (loc x) (wt #\, loc)) (wt #\)))

(defun list*-inline (&rest x)
  (case (length x)
        (1 (wt (car x)))
        (2 (wt "make_cons(" (car x) "," (second x) ")"))
        (otherwise
         (wt "listA(" (length x)) (dolist (loc x) (wt #\, loc)) (wt #\)))))

;;; ----------------------------------------------------------------------

(setf (get 'INLINE 'WT-LOC) 'wt-inline)
(setf (get 'INLINE-COND 'WT-LOC) 'wt-inline-cond)
(setf (get 'INLINE-FIXNUM 'WT-LOC) 'wt-inline-fixnum)
(setf (get 'INLINE-CHARACTER 'WT-LOC) 'wt-inline-character)
(setf (get 'INLINE-LONG-FLOAT 'WT-LOC) 'wt-inline-long-float)
(setf (get 'INLINE-SHORT-FLOAT 'WT-LOC) 'wt-inline-short-float)

(setf (get 'FIXNUM 'WT-LOC) 'wt-fixnum-loc)
(setf (get 'CHARACTER 'WT-LOC) 'wt-character-loc)
(setf (get 'LONG-FLOAT 'WT-LOC) 'wt-long-float-loc)
(setf (get 'SHORT-FLOAT 'WT-LOC) 'wt-short-float-loc)
(setf (get 'BOOLEAN 'WT-LOC) 'wt-loc)
(setf (get 'T 'WT-LOC) 'wt-loc)
;;; Since they are possible locations, we must add:
(setf (get 'STRING 'WT-LOC) 'wt-loc)
(setf (get 'BIT-VECTOR 'WT-LOC) 'wt-loc)
(setf (get 'UNKNOWN 'WT-LOC) 'wt-loc)

(defun wt-fixnum->object (loc)
  (wt "MAKE_FIXNUM(" loc ")"))
(defun wt-character->object (loc)
  (wt "code_char(" loc ")"))
(defun wt-short-float->object (loc)
  (wt "make_shortfloat(" loc ")"))
(defun wt-long-float->object (loc)
  (wt "make_longfloat(" loc ")"))

(setf (get 'FIXNUM->OBJECT 'WT-LOC) 'wt-fixnum->object)
(setf (get 'CHARACTER->OBJECT 'WT-LOC) 'wt-character->object)
(setf (get 'LONG-FLOAT->OBJECT 'WT-LOC) 'wt-long-float->object)
(setf (get 'SHORT-FLOAT->OBJECT 'WT-LOC) 'wt-short-float->object)
