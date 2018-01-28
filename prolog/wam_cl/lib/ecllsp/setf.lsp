;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                                setf routines

(in-package "SYSTEM")

;;; DEFSETF macro.
(defmacro defsetf (access-fn &rest rest)
  "Syntax: (defsetf symbol update-fun [doc])
	or
	(defsetf symbol lambda-list (store-var) {decl | doc}* {form}*)
Defines an expansion
	(setf (SYMBOL arg1 ... argn) value)
	=> (UPDATE-FUN arg1 ... argn value)
	   or
	   (let* ((temp1 ARG1) ... (tempn ARGn) (temp0 value)) rest)
where REST is the value of the last FORM with parameters in LAMBDA-LIST bound
to the symbols TEMP1 ... TEMPn and with STORE-VAR bound to the symbol TEMP0.
The doc-string DOC, if supplied, is saved as a SETF doc and can be retrieved
by (documentation 'SYMBOL 'setf)."
  (cond ((and (car rest) (or (symbolp (car rest)) (functionp (car rest))))
         `(eval-when (compile load eval)
		 (put-sysprop ',access-fn 'SETF-UPDATE-FN ',(car rest))
                 (rem-sysprop ',access-fn 'SETF-LAMBDA)
                 (rem-sysprop ',access-fn 'SETF-METHOD)
		 (rem-sysprop ',access-fn 'SETF-SYMBOL)
		 ,@(si::expand-set-documentation access-fn 'setf (cadr rest))
                 ',access-fn))
	(t
	 (let* ((store (second rest))
		(args (first rest))
		(body (cddr rest))
		(doc (find-documentation body)))
	   (unless (and (= (list-length store) 1) (symbolp (first store)))
		(error "Single store-variable expected."))
	   (setq rest `(lambda ,args #'(lambda ,store ,@body)))
	   `(eval-when (compile load eval)
	      (put-sysprop ',access-fn 'SETF-LAMBDA #'(lambda (,@store ,@args) ,@body))
	      (rem-sysprop ',access-fn 'SETF-UPDATE-FN)
	      (rem-sysprop ',access-fn 'SETF-METHOD)
	      (rem-sysprop ',access-fn 'SETF-SYMBOL)
	      ,@(si::expand-set-documentation access-fn 'setf doc)
	      ',access-fn)))))


;;; DEFINE-SETF-METHOD macro.
(defmacro define-setf-expander (access-fn args &rest body)
  "Syntax: (define-setf-expander symbol defmacro-lambda-list {decl | doc}*
          {form}*)
Defines the SETF-method for generalized-variables (SYMBOL ...).
When a form (setf (SYMBOL arg1 ... argn) value-form) is evaluated, the FORMs
given in the DEFINE-SETF-EXPANDER are evaluated in order with the parameters in
DEFMACRO-LAMBDA-LIST bound to ARG1 ... ARGn.  The last FORM must return five
values
	(var1 ... vark)
	(form1 ... formk)
	(value-var)
	storing-form
	access-form
in order.  These values are collectively called the five gangs of the
generalized variable (SYMBOL arg1 ... argn).  The whole SETF form is then
expanded into
	(let* ((var1 from1) ... (vark formk)
	       (value-var value-form))
	  storing-form)
The doc-string DOC, if supplied, is saved as a SETF doc and can be retrieved
by (DOCUMENTATION 'SYMBOL 'SETF)."
  (let ((env (member '&environment args :test #'eq)))
    (if env
	(setq args (cons (second env)
			 (nconc (ldiff args env) (cddr env))))
	(progn
	  (setq env (gensym))
	  (setq args (cons env args))
	  (push `(declare (ignore ,env)) body))))
  `(eval-when (compile load eval)
	  (put-sysprop ',access-fn 'SETF-METHOD #'(lambda ,args ,@body))
          (rem-sysprop ',access-fn 'SETF-LAMBDA)
          (rem-sysprop ',access-fn 'SETF-UPDATE-FN)
	  (rem-sysprop ',access-fn 'SETF-SYMBOL)
	  ,@(si::expand-set-documentation access-fn 'setf
					  (find-documentation body))
          ',access-fn))


;;; GET-SETF-METHOD.
;;; It just calls GET-SETF-METHOD-MULTIPLE-VALUE
;;;  and checks the number of the store variable.
(defun get-setf-expansion (form &optional env)
  "Args: (place)
Returns the 'five gangs' (see DEFINE-SETF-EXPANDER) for PLACE as five values.
Checks if the third gang is a single-element list."
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method-multiple-value form env)
    (unless (= (list-length stores) 1)
	    (error "Multiple store-variables are not allowed."))
    (values vars vals stores store-form access-form)))


;;;; GET-SETF-METHOD-MULTIPLE-VALUE.

(defun get-setf-method-multiple-value (form &optional env &aux f)
  "Args: (form)
Returns the 'five gangs' (see DEFINE-SETF-EXPANDER) for PLACE as five values.
Does not check if the third gang is a single-element list."
  (flet ((rename-arguments (vars &aux names values all-args)
	   (dolist (item vars)
	     (unless (or (fixnump item) (keywordp item))
	       (push item values)
	       (setq item (gensym))
	       (push item names))
	     (push item all-args))
	   (values (gensym) (nreverse names) (nreverse values) (nreverse all-args))))
    (cond ((and (setq f (macroexpand form env)) (not (equal f form)))
	   (return-from get-setf-method-multiple-value
	     (get-setf-method-multiple-value f env)))
	  ((symbolp form)
	   (let ((store (gensym)))
	     (values nil nil (list store) `(setq ,form ,store) form)))
	  ((or (not (consp form)) (not (symbolp (car form))))
	   (error "Cannot get the setf-method of ~S." form))
	  ((setq f (get-sysprop (car form) 'SETF-METHOD))
	   (apply f env (cdr form)))
	  (t
	   (let* ((name (car form)) writer)
	     (multiple-value-bind (store vars inits all)
		 (rename-arguments (cdr form))
	       (setq writer
		     (cond ((setq f (get-sysprop name 'SETF-UPDATE-FN))
			    `(,f ,@all ,store))
			   ((setq f (get-sysprop name 'STRUCTURE-ACCESS))
			    (setf-structure-access (car all) (car f) (cdr f) store))
			   ((setq f (get-sysprop (car form) 'SETF-LAMBDA))
			    (apply f store all))
			   (t
			    `(funcall #'(SETF ,name) ,store ,@all))))
	       (values vars inits (list store) writer (cons name all))))))))

;;;; SETF definitions.

(defsetf car (x) (y) `(progn (rplaca ,x ,y) ,y))
(defsetf cdr (x) (y) `(progn (rplacd ,x ,y), y))
(defsetf caar (x) (y) `(progn (rplaca (car ,x) ,y) ,y))
(defsetf cdar (x) (y) `(progn (rplacd (car ,x) ,y) ,y))
(defsetf cadr (x) (y) `(progn (rplaca (cdr ,x) ,y) ,y))
(defsetf cddr (x) (y) `(progn (rplacd (cdr ,x) ,y) ,y))
(defsetf caaar (x) (y) `(progn (rplaca (caar ,x) ,y) ,y))
(defsetf cdaar (x) (y) `(progn (rplacd (caar ,x) ,y) ,y))
(defsetf cadar (x) (y) `(progn (rplaca (cdar ,x) ,y) ,y))
(defsetf cddar (x) (y) `(progn (rplacd (cdar ,x) ,y) ,y))
(defsetf caadr (x) (y) `(progn (rplaca (cadr ,x) ,y) ,y))
(defsetf cdadr (x) (y) `(progn (rplacd (cadr ,x) ,y) ,y))
(defsetf caddr (x) (y) `(progn (rplaca (cddr ,x) ,y) ,y))
(defsetf cdddr (x) (y) `(progn (rplacd (cddr ,x) ,y) ,y))
(defsetf caaaar (x) (y) `(progn (rplaca (caaar ,x) ,y) ,y))
(defsetf cdaaar (x) (y) `(progn (rplacd (caaar ,x) ,y) ,y))
(defsetf cadaar (x) (y) `(progn (rplaca (cdaar ,x) ,y) ,y))
(defsetf cddaar (x) (y) `(progn (rplacd (cdaar ,x) ,y) ,y))
(defsetf caadar (x) (y) `(progn (rplaca (cadar ,x) ,y) ,y))
(defsetf cdadar (x) (y) `(progn (rplacd (cadar ,x) ,y) ,y))
(defsetf caddar (x) (y) `(progn (rplaca (cddar ,x) ,y) ,y))
(defsetf cdddar (x) (y) `(progn (rplacd (cddar ,x) ,y) ,y))
(defsetf caaadr (x) (y) `(progn (rplaca (caadr ,x) ,y) ,y))
(defsetf cdaadr (x) (y) `(progn (rplacd (caadr ,x) ,y) ,y))
(defsetf cadadr (x) (y) `(progn (rplaca (cdadr ,x) ,y) ,y))
(defsetf cddadr (x) (y) `(progn (rplacd (cdadr ,x) ,y) ,y))
(defsetf caaddr (x) (y) `(progn (rplaca (caddr ,x) ,y) ,y))
(defsetf cdaddr (x) (y) `(progn (rplacd (caddr ,x) ,y) ,y))
(defsetf cadddr (x) (y) `(progn (rplaca (cdddr ,x) ,y) ,y))
(defsetf cddddr (x) (y) `(progn (rplacd (cdddr ,x) ,y) ,y))
(defsetf first (x) (y) `(progn (rplaca ,x ,y) ,y))
(defsetf second (x) (y) `(progn (rplaca (cdr ,x) ,y) ,y))
(defsetf third (x) (y) `(progn (rplaca (cddr ,x) ,y) ,y))
(defsetf fourth (x) (y) `(progn (rplaca (cdddr ,x) ,y) ,y))
(defsetf fifth (x) (y) `(progn (rplaca (cddddr ,x) ,y) ,y))
(defsetf sixth (x) (y) `(progn (rplaca (nthcdr 5 ,x) ,y) ,y))
(defsetf seventh (x) (y) `(progn (rplaca (nthcdr 6 ,x) ,y) ,y))
(defsetf eighth (x) (y) `(progn (rplaca (nthcdr 7 ,x) ,y) ,y))
(defsetf ninth (x) (y) `(progn (rplaca (nthcdr 8 ,x) ,y) ,y))
(defsetf tenth (x) (y) `(progn (rplaca (nthcdr 9 ,x) ,y) ,y))
(defsetf rest (x) (y) `(progn (rplacd ,x ,y) ,y))
(defsetf svref sys:svset)
(defsetf elt sys:elt-set)
(defsetf symbol-value set)
(defsetf symbol-function sys:fset)
(defsetf fdefinition sys:fset)
(defsetf macro-function (s) (v) `(sys:fset ,s ,v t))
(defsetf aref (a &rest il) (v) `(sys:aset ,v ,a ,@il))
(defsetf row-major-aref (a i) (v) `(sys:row-major-aset ,a ,i ,v))
(defsetf get (s p &optional d) (v)
  (if d `(progn ,d (sys:putprop ,s ,v ,p)) `(sys:putprop ,s ,v ,p)))
(defsetf get-sysprop put-sysprop)
(defsetf nth (n l) (v) `(progn (rplaca (nthcdr ,n ,l) ,v) ,v))
(defsetf char sys:char-set)
(defsetf schar sys:schar-set)
(defsetf bit (a &rest il) (v) `(sys:aset ,v ,a ,@il))
(defsetf sbit (a &rest il) (v) `(sys:aset ,v ,a ,@il))
(defsetf fill-pointer sys:fill-pointer-set)
(defsetf symbol-plist sys:set-symbol-plist)
(defsetf gethash (k h &optional d) (v) `(sys:hash-set ,k ,h ,v))
#-clos
(defsetf documentation (s d) (v) `(sys::set-documentation ,s ,d ,v))
#+clos
(defsetf sys:instance-ref sys:instance-set)
#+clos
(defsetf sys:gfun-spec-how-ref sys:gfun-spec-how-set)
#+clos
(defsetf sys:gfun-instance sys:gfun-instance-set)
(defsetf compiler-macro-function (fname) (function)
  `(sys::put-sysprop ,fname 'sys::compiler-macro ,function))


(define-setf-expander getf (&environment env place indicator &optional default)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (let* ((itemp (gensym)) (store (gensym)) (def (gensym)))
      (values `(,@vars ,itemp ,def)
              `(,@vals ,indicator ,default)
              `(,store)
              `(let ((,(car stores) (sys:put-f ,access-form ,store ,itemp)))
                 ,store-form
                 ,store)
              `(getf ,access-form ,itemp ,default)))))

(defsetf subseq (sequence1 start1 &optional end1)
		(sequence2)
  `(PROGN (REPLACE ,sequence1 ,sequence2 :START1 ,start1 :END1 ,end1)
    ,sequence2))

(define-setf-expander THE (&environment env type place)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (values vars vals stores
            (subst `(THE ,type ,(first stores)) (first stores) store-form)
            `(THE ,type ,access-form))))

#|
(define-setf-expander apply (&environment env fn &rest rest)
  (unless (and (consp fn) (eq (car fn) 'FUNCTION) (symbolp (cadr fn))
	       (null (cddr fn)))
	  (error "Can't get the setf-method of ~S." fn))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion (cons (cadr fn) rest) env)
    (unless (eq (car (last store-form)) (car (last vars)))
            (error "Can't get the setf-method of ~S." fn))
    (values vars vals stores
	    `(apply #',(car store-form) ,@(cdr store-form))
	    `(apply #',(cadr fn) ,@(cdr access-form)))))
|#

(define-setf-expander apply (&environment env fn &rest rest)
  (unless (and (consp fn)
               (or (eq (car fn) 'FUNCTION) (eq (car fn) 'QUOTE))
               (symbolp (cadr fn))
               (null (cddr fn)))
    (error "Can't get the setf-method of ~S." fn))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion (cons (cadr fn) rest) env)
    (cond ((eq (car (last store-form)) (car (last vars)))
           (values vars vals stores
                   `(apply #',(car store-form) ,@(cdr store-form))
                   `(apply #',(cadr fn) ,@(cdr access-form))))
          ((eq (car (last (butlast store-form))) (car (last vars)))
           (values vars vals stores
                   `(apply #',(car store-form)
                           ,@(cdr (butlast store-form 2))
                           (append ,(car (last (butlast store-form)))
                                   (list ,(car (last store-form)))))
                   `(apply #',(cadr fn) ,@(cdr access-form))))
          (t (error "Can't get the setf-method of ~S." fn)))))

(define-setf-expander ldb (&environment env bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion int env)
    (let* ((btemp (gensym))
	   (store (gensym))
	   (stemp (first stores)))
      (values `(,btemp ,@temps)
	      `(,bytespec ,@vals)
	      `(,store)
	      `(let ((,stemp (dpb ,store ,btemp ,access-form)))
	         ,store-form ,store)
	      `(ldb ,btemp ,access-form)))))

(define-setf-expander mask-field (&environment env bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion int env)
    (let* ((btemp (gensym))
	   (store (gensym))
	   (stemp (first stores)))
      (values `(,btemp ,@temps)
	      `(,bytespec ,@vals)
	      `(,store)
	      `(let ((,stemp (deposit-field ,store ,btemp ,access-form)))
	         ,store-form ,store)
	      `(mask-field ,btemp ,access-form)))))


;;; The expansion function for SETF.
(defun setf-expand-1 (place newvalue env)
  (declare (si::c-local))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    (declare (ignore access-form))
    `(let* ,(mapcar #'list
		    (append vars stores)
		    (append vals (list newvalue)))
       (declare (:read-only ,@ (append vars stores))) ; Beppe
       ,store-form)))

(defun setf-structure-access (struct type index newvalue)
  (declare (si::c-local))
  (case type
    (LIST `(sys:rplaca-nthcdr ,struct ,index ,newvalue))
    (VECTOR `(sys:elt-set ,struct ,index ,newvalue))
    (t `(sys::structure-set ,struct ',type ,index ,newvalue))))

(defun setf-expand (l env)
  (declare (si::c-local))
  (cond ((endp l) nil)
        ((endp (cdr l)) (error "~S is an illegal SETF form." l))
        (t
         (cons (setf-expand-1 (car l) (cadr l) env)
               (setf-expand (cddr l) env)))))


;;; SETF macro.
(defmacro setf (&environment env &rest rest)
  "Syntax: (setf {place form}*)
Evaluates each FORM and assigns the value to the corresponding PLACE in order.
Returns the value of the last FORM.
Each PLACE may be any one of the following:
  * A symbol that names a variable.
  * A function call form whose first element is the name of the following
    functions:
	nth	elt	subseq	rest	first ... tenth
	c?r	c??r	c???r	c????r
	aref	svref	char	schar	bit	sbit	fill-pointer
	get	getf	documentation	symbol-value	symbol-function
	symbol-plist	macro-function	gethash		fdefinition
	char-bit	ldb	mask-field
	apply	slot-value
    where '?' stands for either 'a' or 'd'.
  * A function call form whose first element is:
        1. an access function for a structure slot
        1. an accessor method for a CLOS object
  * the form (THE type place) with PLACE being a place recognized by SETF.
  * a macro call which expands to a place recognized by SETF.
  * any form for which a DEFSETF or DEFINE-SETF-EXPANDER declaration has been
    made."
  (cond ((endp rest) nil)
        ((endp (cdr rest)) (error "~S is an illegal SETF form." rest))
        ((endp (cddr rest)) (setf-expand-1 (car rest) (cadr rest) env))
        (t (cons 'progn (setf-expand rest env)))))

;;; PSETF macro.

(defmacro psetf (&environment env &rest rest)
  "Syntax: (psetf {place form}*)
Similar to SETF, but evaluates all FORMs first, and then assigns each value to
the corresponding PLACE.  Returns NIL."
  (cond ((endp rest) nil)
        ((endp (cdr rest)) (error "~S is an illegal PSETF form." rest))
        ((endp (cddr rest))
         `(progn ,(setf-expand-1 (car rest) (cadr rest) env)
                 nil))
        (t
	 (do ((r rest (cddr r))
	      (pairs nil)
	      (store-forms nil))
	     ((endp r)
	      `(let* ,pairs
		 ,@(nreverse store-forms)
		 nil))
	   (when (endp (cdr r)) (error "~S is an illegal PSETF form." rest))
	   (multiple-value-bind (vars vals stores store-form access-form)
	       (get-setf-expansion (car r) env)
             (declare (ignore access-form))
	     (setq store-forms (cons store-form store-forms))
	     (setq pairs
		   (nconc pairs
			  (mapcar #'list
				  (append vars stores)
				  (append vals (list (cadr r)))))))))))


;;; SHIFTF macro.
(defmacro shiftf (&environment env &rest rest)
  "Syntax: (shiftf {place}+ form)
Saves the values of PLACE and FORM, and then assigns the value of each PLACE
to the PLACE on its left.  The rightmost PLACE gets the value of FORM.
Returns the original value of the leftmost PLACE."
  (do ((r rest (cdr r))
       (pairs nil)
       (stores nil)
       (store-forms nil)
       (g (gensym))
       (access-forms nil))
      ((endp (cdr r))
       (setq stores (nreverse stores))
       (setq store-forms (nreverse store-forms))
       (setq access-forms (nreverse access-forms))
       `(let* ,(nconc pairs
		      (list (list g (car access-forms)))
		      (mapcar #'list stores (cdr access-forms))
		      (list (list (car (last stores)) (car r))))
	    ,@store-forms
	    ,g))
    (multiple-value-bind (vars vals stores1 store-form access-form)
	(get-setf-expansion (car r) env)
      (setq pairs (nconc pairs (mapcar #'list vars vals)))
      (setq stores (cons (car stores1) stores))
      (setq store-forms (cons store-form store-forms))
      (setq access-forms (cons access-form access-forms)))))


;;; ROTATEF macro.
(defmacro rotatef (&environment env &rest rest)
  "Syntax: (rotatef {place}*)
Saves the values of PLACEs, and then assigns to each PLACE the saved value of
the PLACE to its right.  The rightmost PLACE gets the value of the leftmost
PLACE.  Returns NIL."
  (do ((r rest (cdr r))
       (pairs nil)
       (stores nil)
       (store-forms nil)
       (access-forms nil))
      ((endp r)
       (setq stores (nreverse stores))
       (setq store-forms (nreverse store-forms))
       (setq access-forms (nreverse access-forms))
       `(let* ,(nconc pairs
		      (mapcar #'list stores (cdr access-forms))
		      (list (list (car (last stores)) (car access-forms))))
	    ,@store-forms
	    nil))
    (multiple-value-bind (vars vals stores1 store-form access-form)
	(get-setf-expansion (car r) env)
      (setq pairs (nconc pairs (mapcar #'list vars vals)))
      (setq stores (cons (car stores1) stores))
      (setq store-forms (cons store-form store-forms))
      (setq access-forms (cons access-form access-forms)))))


;;; DEFINE-MODIFY-MACRO macro, by Bruno Haible.
(defmacro define-modify-macro (name lambdalist function &optional docstring)
  "Syntax: (define-modify-macro symbol lambda-list function-name [doc])
Defines a read-modify-write macro like INCF.  The defined macro will expand
a form (SYMBOL place form1 ... formn) into a form that in effect SETFs the
value of (FUNCTION-NAME place arg1 ... argm) into PLACE, where ARG1 ... ARGm
are parameters in LAMBDA-LIST which are bound to FORM1 ... FORMn.  For
example, INCF could be defined as
	(define-modify-macro incf (&optional (x 1)) +)
The doc-string DOC, if supplied, is saved as a FUNCTION doc and can be
retrieved by (DOCUMENTATION 'SYMBOL 'FUNCTION)."
  (let* ((varlist nil)
         (restvar nil))
    (do* ((lambdalistr lambdalist (cdr lambdalistr))
          (next))
         ((null lambdalistr))
      (setq next (first lambdalistr))
      (cond ((eq next '&OPTIONAL))
            ((eq next '&REST)
             (if (symbolp (second lambdalistr))
               (setq restvar (second lambdalistr))
               (error "In the definition of ~S: &REST variable ~S should be a symbol."
                      name (second lambdalistr)
             ) )
             (if (null (cddr lambdalistr))
               (return)
               (error "Only one variable is allowed after &REST, not ~S"
                      lambdalistr
            )) )
            ((or (eq next '&KEY) (eq next '&ALLOW-OTHER-KEYS) (eq next '&AUX))
             (error "Illegal in a DEFINE-MODIFY-MACRO lambda list: ~S"
                    next
            ))
            ((symbolp next) (push next varlist))
            ((and (listp next) (symbolp (first next)))
             (push (first next) varlist)
            )
            (t (error "lambda list may only contain symbols and lists, not ~S"
                      next
            )  )
    ) )
    (setq varlist (nreverse varlist))
    `(DEFMACRO ,name (&ENVIRONMENT ENV %REFERENCE ,@lambdalist) ,docstring
       (MULTIPLE-VALUE-BIND (VARS VALS STORES SETTER GETTER)
           (GET-SETF-EXPANSION %REFERENCE ENV)
	 (IF (SYMBOLP GETTER)
	     (SUBST (LIST* (QUOTE ,function) GETTER ,@varlist ,restvar)
                    (CAR STORES)
                    SETTER)
	     (DO ((D VARS (CDR D))
		  (V VALS (CDR V))
		  (LET-LIST NIL (CONS (LIST (CAR D) (CAR V)) LET-LIST)))
		 ((NULL D)
		  (PUSH
		   (LIST
		    (CAR STORES)
		    (IF (AND (LISTP %REFERENCE) (EQ (CAR %REFERENCE) 'THE))
			(LIST 'THE (CADR %REFERENCE)
			      (LIST* (QUOTE ,function) GETTER ,@varlist ,restvar))
			(LIST* (QUOTE ,function) GETTER ,@varlist ,restvar)))
		   LET-LIST)
		  `(LET* ,(NREVERSE LET-LIST) ,SETTER))))))))
#|
(defmacro define-modify-macro (name lambda-list function &optional doc-string)
  (let ((update-form
	 (do ((l lambda-list (cdr l))
	      (vs nil))
	     ((null l) `(list ',function access-form ,@(nreverse vs)))
	   (unless (eq (car l) '&optional)
		   (if (eq (car l) '&rest)
		       (return `(list* ',function
				       access-form
				       ,@(nreverse vs)
				       ,(cadr l))))
		   (if (symbolp (car l))
		       (setq vs (cons (car l) vs))
		       (setq vs (cons (caar l) vs)))))))
    `(defmacro ,name (&environment env reference . ,lambda-list)
       ,@(if doc-string (list doc-string))
       (when (symbolp reference)
             (return-from ,name
               (let ((access-form reference))
                 (list 'setq reference ,update-form))))
       (multiple-value-bind (vars vals stores store-form access-form)
	   (get-setf-expansion reference env)
         `(let* ,(mapcar #'list
		  (append vars stores)
		  (append vals (list ,update-form)))
	   (declare (:read-only ,@stores)) ; Beppe
	   ,store-form)))))
|#

;;; Some macro definitions.

(defmacro remf (&environment env place indicator)
  "Syntax: (remf place form)
Removes the property specified by FORM from the property list stored in PLACE.
Returns T if the property list had the specified property; NIL otherwise."
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    `(let* ,(mapcar #'list vars vals)
       (declare (:read-only ,@vars)) ; Beppe
       (multiple-value-bind (,(car stores) flag)
           (sys:rem-f ,access-form ,indicator)
         ,store-form
         flag))))

(define-modify-macro incf (&optional (delta 1)) +
  "Syntax: (incf place [form])
Increments the value of PLACE by the value of FORM.  FORM defaults to 1.")

(define-modify-macro decf (&optional (delta 1)) -
  "Syntax: (decf place [form])
Decrements the value of PLACE by the value of FORM.  FORM defaults to 1.")

(defmacro push (&environment env item place)
  "Syntax: (push form place)
Evaluates FORM, conses the value of FORM to the value stored in PLACE, and
makes it the new value of PLACE.  Returns the new value of PLACE."
  (when (symbolp place)
    (return-from push `(setq ,place (cons ,item ,place))))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    ;; The item to be pushed has to be evaluated before the destination
    (unless (constantp item)
      (setq vals (cons item vals)
	    item (gensym)
	    vars (cons item vars)))
    `(let* ,(mapcar #'list
		    (append vars stores)
		    (append vals (list (list 'cons item access-form))))
       (declare (:read-only ,@vars)) ; Beppe
       ,store-form)))

(defmacro pushnew (&environment env item place &rest rest)
  "Syntax: (pushnew form place {keyword-form value-form}*)
Evaluates FORM first.  If the value is already in the list stored in PLACE,
does nothing.  Else, conses the value onto the list and makes the result the
new value of PLACE.  Returns NIL.  KEYWORD-FORMs and VALUE-FORMs are used to
check if the value of FORM is already in PLACE as if their values are passed
to MEMBER."
  (cond ((symbolp place)
	 (return-from pushnew `(setq ,place (adjoin ,item ,place ,@rest)))))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    ;; The item to be pushed has to be evaluated before the destination
    (unless (constantp item)
      (setq vals (cons item vals)
	    item (gensym)
	    vars (cons item vars)))
    `(let* ,(mapcar #'list
		    (append vars stores)
		    (append vals
			    (list (list* 'adjoin item access-form rest))))
       (declare (:read-only ,@vars)) ; Beppe
       ,store-form)))

(defmacro pop (&environment env place)
  "Syntax: (pop place)
Gets the cdr of the value stored in PLACE and makes it the new value of PLACE.
Returns the car of the old value in PLACE."
  (when (symbolp place)
        (return-from pop
          (let ((temp (gensym)))
            `(let ((,temp (car ,place)))
                (setq ,place (cdr ,place))
                ,temp))))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-expansion place env)
    `(let* ,(mapcar #'list
		    (append vars stores)
		    (append vals (list (list 'cdr access-form))))
       (declare (:read-only ,@vars)) ; Beppe
       (prog1 (car ,access-form)
              ,store-form))))
#|
;;; Proposed extension:
; Expansion of (SETF (VALUES place1 ... placek) form)
; --> (MULTIPLE-VALUE-BIND (dummy1 ... dummyk) form
;       (SETF place1 dummy1 ... placek dummyk)
;       (VALUES dummy1 ... dummyk))
(define-setf-expander VALUES (&environment env &rest subplaces)
  (do ((temps) (vals) (stores)
       (storeforms) (accessforms)
       (placesr subplaces))
      ((atom placesr)
       (setq temps (nreverse temps)
	     vals (nreverse vals)
	     stores (nreverse stores)
	     storeforms (nreverse storeforms)
	     accessforms (nreverse accessforms))
       (values temps
            vals
            stores
            `(VALUES ,@storeforms)
            `(VALUES ,@accessforms)))
    (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
        (get-setf-expansion (pop placesr) env)
      (setq temps (revappend SM1 temps)
	    vals (revappend SM2 vals)
	    stores (revappend SM3 stores)
	    storeforms (cons SM4 storeforms)
	    accessforms (cons SM5 accessforms)))))
|#
