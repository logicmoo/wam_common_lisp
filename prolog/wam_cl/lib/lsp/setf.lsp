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


(in-package 'lisp)


(export '(setf psetf shiftf rotatef
          define-modify-macro defsetf
          getf remf incf decf push pushnew pop
          define-setf-method get-setf-method get-setf-method-multiple-value))


(in-package 'system)

(eval-when (compile) (proclaim '(optimize (safety 2) (space 3))))
(eval-when (eval compile) (defun sys::clear-compiler-properties (symbol)))
(eval-when (eval compile) (setq sys:*inhibit-macro-special* nil))


;;; DEFSETF macro.
(defmacro defsetf (access-fn &rest rest)
  (cond ((and (car rest) (or (symbolp (car rest)) (functionp (car rest))))
         `(eval-when (compile load eval)
		 (sys:putprop ',access-fn ',(car rest) 'SETF-UPDATE-FN)
                 (remprop ',access-fn 'SETF-LAMBDA)
                 (remprop ',access-fn 'SETF-METHOD)
                 (sys:putprop ',access-fn
                             ,(when (not (endp (cdr rest)))
                                    (unless (stringp (cadr rest))
                                            (error "A doc-string expected."))
                                    (unless (endp (cddr rest))
                                            (error "Extra arguments."))
                                    (cadr rest))
                             'SETF-DOCUMENTATION)
                 ',access-fn))
	(t
	 (unless (= (list-length (cadr rest)) 1)
		 (error "(store-variable) expected."))
         `(eval-when (compile load eval)
		 (sys:putprop ',access-fn ',rest 'SETF-LAMBDA)
                 (remprop ',access-fn 'SETF-UPDATE-FN)
                 (remprop ',access-fn 'SETF-METHOD)
                 (sys:putprop ',access-fn
                             ,(find-documentation (cddr rest))
                             'SETF-DOCUMENTATION)
                 ',access-fn))))


;;; DEFINE-SETF-METHOD macro.
(defmacro define-setf-method (access-fn args &rest body)
  (let ((env (member '&environment args :test #'eq)))
    (if env
	(setq args (cons (second env)
			 (nconc (ldiff args env) (cddr env))))
	(progn
	  (setq env (gensym))
	  (setq args (cons env args))
	  (push `(declare (ignore ,env)) body))))
  `(eval-when (compile load eval)
	  (sys:putprop ',access-fn #'(lambda ,args ,@body) 'SETF-METHOD)
          (remprop ',access-fn 'SETF-LAMBDA)
          (remprop ',access-fn 'SETF-UPDATE-FN)
          (sys:putprop ',access-fn
                      ,(find-documentation body)
                      'SETF-DOCUMENTATION)
          ',access-fn))


;;; GET-SETF-METHOD.
;;; It just calls GET-SETF-METHOD-MULTIPLE-VALUE
;;;  and checks the number of the store variable.
(defun get-setf-method (form &optional env)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method-multiple-value form env)
    (unless (= (list-length stores) 1)
	    (error "Multiple store-variables are not allowed."))
    (values vars vals stores store-form access-form)))


;;;; GET-SETF-METHOD-MULTIPLE-VALUE.

(defun get-setf-method-multiple-value (form &optional env &aux tem)
  (cond ((symbolp form)
	 (let ((store (gensym)))
	   (values nil nil (list store) `(setq ,form ,store) form)))
	((or (not (consp form)) (not (symbolp (car form))))
	 (error "Cannot get the setf-method of ~S." form))
	((and env (setq tem (assoc (car form) (second env))))
	 (setq tem (macroexpand form env))
	 (when (eq form tem)
	   (error "Cannot get setf-method for ~a" form))
	 (return-from get-setf-method-multiple-value
		      (get-setf-method-multiple-value tem env)))
	((get (car form) 'SETF-METHOD)
	 (apply (get (car form) 'SETF-METHOD) env (cdr form)))
	((or (get (car form) 'SETF-UPDATE-FN)
	     (setq tem (get (car form) 'STRUCTURE-ACCESS)))
	 (let ((vars (mapcar #'(lambda (x)
	                         (declare (ignore x))
	                         (gensym))
	                     (cdr form)))
	       (store (gensym)))
	   (values vars (cdr form) (list store)
		   (if tem
		       (setf-structure-access (car vars) (car tem)
					      (cdr tem) store)
		       `(,(get (car form) 'SETF-UPDATE-FN)
			 ,@vars ,store))
		   (cons (car form) vars))))
	((get (car form) 'SETF-LAMBDA)
	 (let* ((vars (mapcar #'(lambda (x)
	                          (declare (ignore x))
	                          (gensym))
	                      (cdr form)))
		(store (gensym))
		(l (get (car form) 'SETF-LAMBDA))
		(f `(lambda ,(car l) #'(lambda ,(cadr l) ,@(cddr l)))))
	   (values vars (cdr form) (list store)
		   (funcall (apply f vars) store)
		   (cons (car form) vars))))
	((macro-function (car form))
	 (get-setf-method-multiple-value (macroexpand form)))
	#+clos
        ((special-form-p (car form))
	 (error "Cannot expand the SETF form ~S." form))
	#+clos
	((get (car form) 'SETF-SYMBOL)
	 (let ((vars (mapcar #'(lambda (x)
	                         (declare (ignore x))
	                         (gensym))
	                     (cdr form)))
	       (store (gensym)))
	   (values vars (cdr form) (list store)
		   ;; use the symbol here, otherwise the CLOS walker punts.
	           `(funcall #',(get (car form) 'SETF-SYMBOL) ,store ,@vars)
		   (cons (car form) vars))))
	(t
	 (error "Cannot expand the SETF form ~S." form))))


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
(defsetf macro-function (s) (v) `(progn (sys:fset ,s (cons 'MACRO ,v)) ,v))
(defsetf aref (a &rest il) (v) `(sys:aset ,v ,a ,@il))
(defsetf get (s p &optional d) (v) `(sys:putprop ,s ,v ,p))
(defsetf nth (n l) (v) `(progn (rplaca (nthcdr ,n ,l) ,v) ,v))
(defsetf char sys:char-set)
(defsetf schar sys:schar-set)
(defsetf bit (a &rest il) (v) `(sys:aset ,v ,a ,@il))
(defsetf sbit (a &rest il) (v) `(sys:aset ,v ,a ,@il))
(defsetf fill-pointer sys:fill-pointer-set)
(defsetf symbol-plist sys:set-symbol-plist)
(defsetf gethash (k h &optional d) (v) `(sys:hash-set ,k ,h ,v))
(defsetf documentation (s d) (v)
  `(case ,d
     (variable (sys:putprop ,s ,v 'VARIABLE-DOCUMENTATION))
     (function (sys:putprop ,s ,v 'FUNCTION-DOCUMENTATION))
     (structure (sys:putprop ,s ,v 'STRUCTURE-DOCUMENTATION))
     (type (sys:putprop ,s ,v 'TYPE-DOCUMENTATION))
     (setf (sys:putprop ,s ,v 'SETF-DOCUMENTATION))
     (t (error "~S is an illegal documentation type." ,d))))
#+clos
(defsetf sys:instance-ref sys:instance-set)
#+clos
(defsetf sys:gfun-spec-how-ref sys:gfun-spec-how-set)
#+clos
(defsetf sys:gfun-instance sys:gfun-instance-set)


(define-setf-method getf (&environment env place indicator &optional default)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method place env)
    (let ((itemp (gensym)) (store (gensym)))
      (values `(,@vars ,itemp)
              `(,@vals ,indicator)
              `(,store)
              `(let ((,(car stores) (sys:put-f ,access-form ,store ,itemp)))
                 ,store-form
                 ,store)
              `(getf ,access-form ,itemp ,default)))))

(defsetf subseq (sequence1 start1 &optional end1)
		(sequence2)
  `(PROGN (REPLACE ,sequence1 ,sequence2 :START1 ,start1 :END1 ,end1)
    ,sequence2))

(define-setf-method THE (&environment env type place)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method place env)
    (values vars vals stores
            (subst `(THE ,type ,(first stores)) (first stores) store-form)
            `(THE ,type ,access-form))))

#|
(define-setf-method apply (&environment env fn &rest rest)
  (unless (and (consp fn) (eq (car fn) 'FUNCTION) (symbolp (cadr fn))
	       (null (cddr fn)))
	  (error "Can't get the setf-method of ~S." fn))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method (cons (cadr fn) rest) env)
    (unless (eq (car (last store-form)) (car (last vars)))
            (error "Can't get the setf-method of ~S." fn))
    (values vars vals stores
	    `(apply #',(car store-form) ,@(cdr store-form))
	    `(apply #',(cadr fn) ,@(cdr access-form)))))
|#

(define-setf-method apply (&environment env fn &rest rest)
  (unless (and (consp fn)
               (or (eq (car fn) 'FUNCTION) (eq (car fn) 'QUOTE))
               (symbolp (cadr fn))
               (null (cddr fn)))
    (error "Can't get the setf-method of ~S." fn))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method (cons (cadr fn) rest) env)
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

(define-setf-method char-bit (&environment env char name)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method char env)
    (let ((ntemp (gensym))
	  (store (gensym))
	  (stemp (first stores)))
      (values `(,ntemp ,@temps)
	      `(,name ,@vals)
	      `(,store)
	      `(let ((,stemp (set-char-bit ,access-form ,ntemp ,store)))
	         ,store-form ,store)
	      `(char-bit ,access-form ,ntemp)))))

(define-setf-method ldb (&environment env bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method int env)
    (let ((btemp (gensym))
	  (store (gensym))
	  (stemp (first stores)))
      (values `(,btemp ,@temps)
	      `(,bytespec ,@vals)
	      `(,store)
	      `(let ((,stemp (dpb ,store ,btemp ,access-form)))
	         ,store-form ,store)
	      `(ldb ,btemp ,access-form)))))

(define-setf-method mask-field (&environment env bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method int env)
    (let ((btemp (gensym))
	  (store (gensym))
	  (stemp (first stores)))
      (values `(,btemp ,@temps)
	      `(,bytespec ,@vals)
	      `(,store)
	      `(let ((,stemp (deposit-field ,store ,btemp ,access-form)))
	         ,store-form ,store)
	      `(mask-field ,btemp ,access-form)))))


;;; The expansion function for SETF.
(defun setf-expand-1 (place newvalue env &aux g)
  (when (and (consp place) (eq (car place) 'THE))
        (return-from setf-expand-1
          (setf-expand-1 (caddr place) `(the ,(cadr place) ,newvalue) env)))
  (when (symbolp place)
        (return-from setf-expand-1 `(setq ,place ,newvalue)))
  (when (and (consp place)
	     (not (or (get (car place) 'SETF-LAMBDA)
		      (get (car place) 'SETF-UPDATE-FN))))
	(multiple-value-setq (place g) (macroexpand place env))
	(if g (return-from setf-expand-1 (setf-expand-1 place newvalue env))))
  (when (and (symbolp (car place)) (setq g (get (car place) 'SETF-UPDATE-FN)))
        (return-from setf-expand-1 `(,g ,@(cdr place) ,newvalue)))
  (when (and (symbolp (car place))
             (setq g (get (car place) 'STRUCTURE-ACCESS)))
    (return-from setf-expand-1
      (setf-structure-access (cadr place) (car g) (cdr g) newvalue)))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method place env)
;    (declare (ignore access-form))
    `(let* ,(mapcar #'list
		    (append vars stores)
		    (append vals (list newvalue)))
       (declare (:read-only ,@ (append vars stores))) ; Beppe
       ,store-form)))

(defun setf-structure-access (struct type index newvalue)
  (case type
    (LIST `(sys:rplaca-nthcdr ,struct ,index ,newvalue))
    (VECTOR `(sys:elt-set ,struct ,index ,newvalue))
    (t `(sys::structure-set ,struct ',type ,index ,newvalue))))

(defun setf-expand (l env)
  (cond ((endp l) nil)
        ((endp (cdr l)) (error "~S is an illegal SETF form." l))
        (t
         (cons (setf-expand-1 (car l) (cadr l) env)
               (setf-expand (cddr l) env)))))


;;; SETF macro.
(defmacro setf (&environment env &rest rest)
  (cond ((endp rest) nil)
        ((endp (cdr rest)) (error "~S is an illegal SETF form." rest))
        ((endp (cddr rest)) (setf-expand-1 (car rest) (cadr rest) env))
        (t (cons 'progn (setf-expand rest env)))))


;;; PSETF macro.

(defmacro psetf (&environment env &rest rest)
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
	       (get-setf-method (car r) env)
             (declare (ignore access-form))
	     (setq store-forms (cons store-form store-forms))
	     (setq pairs
		   (nconc pairs
			  (mapcar #'list
				  (append vars stores)
				  (append vals (list (cadr r)))))))))))


;;; SHIFTF macro.
(defmacro shiftf (&environment env &rest rest)
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
	(get-setf-method (car r) env)
      (setq pairs (nconc pairs (mapcar #'list vars vals)))
      (setq stores (cons (car stores1) stores))
      (setq store-forms (cons store-form store-forms))
      (setq access-forms (cons access-form access-forms)))))


;;; ROTATEF macro.
(defmacro rotatef (&environment env &rest rest)
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
	(get-setf-method (car r) env)
      (setq pairs (nconc pairs (mapcar #'list vars vals)))
      (setq stores (cons (car stores1) stores))
      (setq store-forms (cons store-form store-forms))
      (setq access-forms (cons access-form access-forms)))))


;;; DEFINE-MODIFY-MACRO macro, by Bruno Haible.
(defmacro define-modify-macro (name lambdalist function &optional docstring)
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
           (GET-SETF-METHOD %REFERENCE ENV)
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
	   (get-setf-method reference env)
         `(let* ,(mapcar #'list
		  (append vars stores)
		  (append vals (list ,update-form)))
	   (declare (:read-only ,@stores)) ; Beppe
	   ,store-form)))))
|#

;;; Some macro definitions.

(defmacro remf (&environment env place indicator)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method place env)
    `(let* ,(mapcar #'list vars vals)
       (declare (:read-only ,@vars)) ; Beppe
       (multiple-value-bind (,(car stores) flag)
           (sys:rem-f ,access-form ,indicator)
         ,store-form
         flag))))

(define-modify-macro incf (&optional (delta 1)) +)
(define-modify-macro decf (&optional (delta 1)) -)

(defmacro push (&environment env item place)
  (when (symbolp place)
    (return-from push `(setq ,place (cons ,item ,place))))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method place env)
    `(let* ,(mapcar #'list
		    (append vars stores)
		    (append vals (list (list 'cons item access-form))))
       (declare (:read-only ,@vars)) ; Beppe
       ,store-form)))

(defmacro pushnew (&environment env item place &rest rest)
  (cond ((symbolp place)
	 (return-from pushnew `(setq ,place (adjoin ,item ,place ,@rest)))))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method place env)
    `(let* ,(mapcar #'list
		    (append vars stores)
		    (append vals
			    (list (list* 'adjoin item access-form rest))))
       (declare (:read-only ,@vars)) ; Beppe
       ,store-form)))

(defmacro pop (&environment env place)
  (when (symbolp place)
        (return-from pop
          (let ((temp (gensym)))
            `(let ((,temp (car ,place)))
                (setq ,place (cdr ,place))
                ,temp))))
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method place env)
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
(define-setf-method VALUES (&environment env &rest subplaces)
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
        (get-setf-method (pop placesr) env)
      (setq temps (revappend SM1 temps)
	    vals (revappend SM2 vals)
	    stores (revappend SM3 stores)
	    storeforms (cons SM4 storeforms)
	    accessforms (cons SM5 accessforms)))))
|#
