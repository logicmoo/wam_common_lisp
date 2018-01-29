;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-flow.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 5, Data and Control Flow.

(IN-PACKAGE "EMACS-CL")

(define-storage-layout interp-fn (lambda-exp env name doc))

(defvar *setf-definitions* (make-hash-table))

(unless (fboundp 'functionp)
  (defun functionp (object)
    (or (subrp object)
	(byte-code-function-p object)
	(and (consp object)
	     (eq (car object) 'lambda)))))

(defun APPLY (fn &rest args)
  (cond
    ((COMPILED-FUNCTION-P fn)
     (cond
       ((null args)		(funcall fn))
       ((null (rest args))	(apply fn (first args)))
       (t			(apply #'apply fn args))))
    ((INTERPRETED-FUNCTION-P fn)
     (eval-lambda-expr (interp-fn-lambda-exp fn)
		       (append (butlast args)
			       (car (last args)))
		       (interp-fn-env fn)))
    ((functionp fn)
     (apply #'apply fn args))
    ((or (symbolp fn) (consp fn))
     (apply #'APPLY (FDEFINITION fn) args))
    (t
     (type-error fn '(OR FUNCTION SYMBOL CONS)))))

(defun function-block-name (name)
  (cond
    ((symbolp name)		name)
    ((setf-name-p name)		(second name))
    (t				(not-function-name-error name))))

(cl:defmacro DEFUN (name lambda-list &body forms)
  (MULTIPLE-VALUE-BIND (body decls doc) (parse-body forms t)
    `(EVAL-WHEN (,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
       (cl-defun (QUOTE ,name) (LAMBDA ,lambda-list
				 ,@(when doc `(,doc))
				 ,@(when decls `((DECLARE ,@decls)))
				 (BLOCK ,(function-block-name name)
				   ,@body))))))

(defun cl-defun (name fn)
  (setf (FDEFINITION name) fn)
  (setf (function-name fn) name)
  name)

(defun setf-name-p (name)
  (and (consp name)
       (eq (car name) 'SETF)
       (symbolp (cadr name))
       (null (cddr name))))

(defun not-function-name-error (name)
  (type-error name '(OR SYMBOL (CONS (EQL SETF) (CONS SYMBOL NULL)))))

(defun FDEFINITION (name)
  (cond
    ((symbolp name)
     (unless (FBOUNDP name)
       (ERROR 'UNDEFINED-FUNCTION (kw NAME) name))
     (if (or (SPECIAL-OPERATOR-P name) (MACRO-FUNCTION name))
	 nil
	 (SYMBOL-FUNCTION name)))
    ((setf-name-p name)
     (let ((fn (gethash (second name) *setf-definitions*)))
       (if (null fn)
	   (ERROR 'UNDEFINED-FUNCTION (kw NAME) name)
	   fn)))
    (t
     (not-function-name-error name))))

(defsetf FDEFINITION (name) (fn)
  `(cond
    ((symbolp ,name)
     (setf (SYMBOL-FUNCTION ,name) ,fn))
    ((setf-name-p ,name)
     (puthash (second ,name) ,fn *setf-definitions*))
    (t
     (not-function-name-error ,name))))

(defun FBOUNDP (name)
  (cond
    ((symbolp name)
     (if (SPECIAL-OPERATOR-P name)
	 'T
	 (fboundp name)))
    ((setf-name-p name)
     (not (null (gethash (second name) *setf-definitions*))))
    (t
     (not-function-name-error name))))
    
(defun FMAKUNBOUND (name)
  (cond
    ((symbolp name)
     (fmakunbound name)
     (remhash name *macro-functions*))
    ((setf-name-p name)
     (remhash (second name) *setf-definitions*))
    (t
     (not-function-name-error name)))
  name)
    
;;; Special operators: FLET, LABELS, MACROLET

(defun FUNCALL (fn &rest args)
  (cond
    ((COMPILED-FUNCTION-P fn)
     (apply fn args))
    ((INTERPRETED-FUNCTION-P fn)
     (eval-lambda-expr (interp-fn-lambda-exp fn)
		       args
		       (interp-fn-env fn)))
    ((functionp fn)
     (apply fn args))
    (t
     (APPLY (FDEFINITION fn) args))))

;;; Special operator: FUNCTION

(defun FUNCTION-LAMBDA-EXPRESSION (fn)
  (cond
    ((INTERPRETED-FUNCTION-P fn)	(cl:values (interp-fn-lambda-exp fn)
						   T nil))
    ((subrp fn)				(cl:values nil nil nil))
    ((byte-code-function-p fn)		(cl:values nil nil nil))
    ((FUNCTIONP fn)			(cl:values nil T nil))
    (t					(type-error fn 'FUNCTION))))

(defun FUNCTIONP (object)
  (or (byte-code-function-p object)
      (subrp object)
      (INTERPRETED-FUNCTION-P object)))

(defun COMPILED-FUNCTION-P (object)
  (or (byte-code-function-p object)
      (subrp object)))

(defvar *constants* '(nil T PI))

(defmacro* DEFCONSTANT (name initial-value &optional documentation)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defconst ,name ,initial-value
       ,@(when documentation `(,documentation)))
     (pushnew ',name *constants*)
     ',name))

(cl:defmacro DEFCONSTANT (name initial-value &optional documentation)
  (with-gensyms (value)
    `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
       (PUSHNEW (QUOTE ,name) *constants*)
       (LET ((,value ,initial-value))
	 (WHEN (AND (BOUNDP (QUOTE ,name))
		    (NOT (EQL ,name ,value)))
	   (WARN "Redefining constant ~S." (QUOTE ,name)))
	 ;; Using Emacs Lisp set lets of sneak by the access check
	 ;; that Common Lisp SETQ does for constants.
	 (set (QUOTE ,name) ,value))
       (QUOTE ,name))))

(DEFCONSTANT CALL-ARGUMENTS-LIMIT 50)

(DEFCONSTANT LAMBDA-LIST-KEYWORDS
  '(&ALLOW-OTHER-KEYS &AUX &BODY &ENVIRONMENT &KEY &OPTIONAL &REST &WHOLE))

(DEFCONSTANT LAMBDA-PARAMETERS-LIMIT 50)

(defvar *specials* (list '*MACROEXPAND-HOOK*))

(defmacro DEFVAR (name &rest rest)
  `(progn
    (pushnew ',name *specials*)
    (defvar ,name ,@rest)))

(cl:defmacro DEFVAR (name &optional (initial-value nil valuep) documentation)
  `(PROGN
     (EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
       (DECLAIM (SPECIAL ,name)))
     (EVAL-WHEN (,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
       ,@(when valuep
	   `((UNLESS (BOUNDP (QUOTE ,name))
	       (SETQ ,name ,initial-value))))
       (QUOTE ,name))))

(cl:defmacro DEFPARAMETER (name initial-value &optional documentation)
  `(PROGN
     (EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
       (DECLAIM (SPECIAL ,name)))
     (EVAL-WHEN (,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
       (SETQ ,name ,initial-value)
       (QUOTE ,name))))

(defun lambda-list-keyword-p (x)
  (member x LAMBDA-LIST-KEYWORDS))

(defun dotted-memq (item list)
  (let ((result nil))
    (while (consp list)
      (if (eq (car list) item)
	  (setq result list list nil)
	  (setq list (cdr list))))
    result))

(defun destructure (lambda-list list)
  (let ((result nil)
	(current nil)
	(seen-&rest nil)
	(seen-&key nil))
    (do-lambda-list (((key var) default supp) kind lambda-list)
      (setq current
	    (case kind
	      (&WHOLE		(if (eql (length lambda-list) 2)
				    `(PROG1 ,list (SETQ ,list nil))
				    list))
	      (:required	`(IF ,list
				     (POP ,list)
				     (ERROR (QUOTE PROGRAM-ERROR))))
	      (&OPTIONAL	`(PROGN
				   ,@(when supp
				       `((SETQ ,supp (NOT (NULL ,list)))))
				   (IF ,list (POP ,list) ,default)))
	      ((&REST &BODY)	(setq seen-&rest t)
				list)
	      (&KEY		(setq seen-&key t)
				(with-gensyms (foo val valp)
				  `(MULTIPLE-VALUE-BIND (,foo ,val ,valp)
				       (GET-PROPERTIES ,list (QUOTE (,key)))
				    ,@(when supp
				        `((SETQ ,supp (NOT (NULL ,valp)))))
				    (REMF ,list (QUOTE ,key))
				    (IF ,valp ,val ,default))))
	      (&AUX		default)
	      (t		(ERROR "~S not allowed in destructuring."
				       kind))))
      (if (symbolp var)
	  (push `(SETQ ,var ,current) result)
	  (let ((sublist (gensym)))
	    (push `(LET ((,sublist ,current))
		     ,@(destructure var sublist))
		  result))))
    (cond
      ((dotted-memq '&ALLOW-OTHER-KEYS lambda-list))
      (seen-&key
       (push `(WHEN (AND ,list (NOT (GETF ,list ,(kw ALLOW-OTHER-KEYS-P))))
	        (ERROR (QUOTE PROGRAM-ERROR)))
	     result))
      (seen-&rest)
      (t
       (push `(WHEN ,list
	        (ERROR (QUOTE PROGRAM-ERROR)))
	     result)))
    (nreverse result)))

(cl:defmacro DESTRUCTURING-BIND (lambda-list form &body forms)
  (with-gensyms (list)
    (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
      `(LET ,(lambda-list-variables lambda-list)
	 ,@(when decls `((DECLARE ,@decls)))
	 (LET ((,list ,form))
	   ,@(destructure lambda-list list))
	 (PROGN ,@body)))))

;;; Special Operators: LET, LET*

;;; Special Operator: PROGV

;;; Special Operator: SETQ

(cl:defmacro PSETQ (&rest forms)
  `(PSETF ,@forms))

;;; Special Operator: BLOCK

;;; Special Operator: CATCH

;;; Special Operator: GO

;;; Special Operator: RETURN-FROM

(cl:defmacro RETURN (&optional form)
  `(RETURN-FROM nil ,form))

;;; Special Operator: TAGBODY

;;; Special Operator: THROW

;;; Special Operator: UNWIND-PROTECT

;;; Constant Variable: NIL

(defun NOT (x)
  (if x nil 'T))

(DEFCONSTANT T 'T)

(fset 'EQ (symbol-function 'eq))

(defun EQL (x y)
  (or (eq x y)
      (cond
	((and (CHARACTERP x) (CHARACTERP y))
	 (eq (CHAR-CODE x) (CHAR-CODE y)))
	((and (floatp x) (floatp y))
	 (equal x y))
	((and (bignump x) (bignump y))
	 (and (eq (length x) (length y))
	      (every #'eq x y)))
	((and (ratiop x) (ratiop y))
	 (and (EQL (NUMERATOR x) (NUMERATOR y))
	      (EQL (DENOMINATOR x) (DENOMINATOR y))))
	((and (COMPLEXP x) (COMPLEXP y))
	 (and (EQL (REALPART x) (REALPART y))
	      (EQL (IMAGPART x) (IMAGPART y))))
	(t
	 nil))))

(defun EQUAL (x y)
  (or (EQL x y)
      (cond
	((and (consp x) (consp y))
	 (and (EQUAL (car x) (car y))
	      (EQUAL (cdr x) (cdr y))))
	((and (STRINGP x) (STRINGP y))
	 (and (eq (LENGTH x) (LENGTH y))
	      (EVERY #'CHAR= x y)))
	((and (BIT-VECTOR-P x) (BIT-VECTOR-P y))
	 (and (eq (LENGTH x) (LENGTH y))
	      (EVERY #'EQL x y)))
	((and (PATHNAMEP x) (PATHNAMEP y))
	 ;; TODO: pathnames
	 (and (EQUAL (pathname-host x) (pathname-host y))
	      (EQUAL (pathname-device x) (pathname-device y))
	      (EQUAL (pathname-directory x) (pathname-directory y))
	      (EQUAL (pathname-name x) (pathname-name y))
	      (EQUAL (pathname-type x) (pathname-type y))
	      (EQUAL (pathname-version x) (pathname-version y))))
	(t
	 nil))))

(defun EQUALP (x y)
  (or (EQUAL x y)
      (cond
	((and (CHARACTERP x) (CHARACTERP y))
	 (CHAR-EQUAL x y))
	((and (NUMBERP x) (NUMBERP y))
	 (cl:= x y))
	((and (consp x) (consp y))
	 (and (EQUAL (car x) (car y))
	      (EQUAL (cdr x) (cdr y))))
	((and (VECTORP x) (VECTORP y))
	 (and (eq (LENGTH x) (LENGTH y))
	      (EVERY #'EQUALP x y)))
	((and (ARRAYP x) (ARRAYP y))
	 (and (equal (ARRAY-DIMENSIONS x) (ARRAY-DIMENSIONS y))
	      (catch 'result
		(dotimes (i (ARRAY-TOTAL-SIZE x) 'T)
		  (unless (EQUALP (ROW-MAJOR-AREF x i)
				  (ROW-MAJOR-AREF y i))
		    (throw 'result nil))))))
	;; TODO: structures and hash tables
	(t
	 nil))))

(cl:defun IDENTITY (object)
  object)

(defun COMPLEMENT (fn)
  (let ((env (augment-environment nil :variable '(fn)))
	(name "complemented function"))
    (setf (lexical-value 'fn env) fn)
    (enclose '(LAMBDA (&REST args) (NOT (APPLY fn args))) env name)))

(defun CONSTANTLY (value)
  (let ((env (augment-environment nil :variable '(value))))
    (setf (lexical-value 'value env) value)
    (enclose '(LAMBDA (&REST x) value) env
	     (format "\"constantly %s\"" (PRIN1-TO-STRING value)))))

(defun EVERY (predicate &rest sequences)
  (catch 'EVERY
    (apply #'MAP nil
	   (lambda (&rest elts)
	     (unless (APPLY predicate elts)
	       (throw 'EVERY nil)))
	   sequences)
    T))

(defun SOME (predicate &rest sequences)
  (catch 'SOME
    (apply #'MAP nil
	   (lambda (&rest elts)
	     (let ((val (APPLY predicate elts)))
	       (when val
		 (throw 'SOME val))))
	   sequences)
    nil))

(defun NOTEVERY (predicate &rest sequences)
  (not (apply #'EVERY predicate sequences)))

(defun NOTANY (predicate &rest sequences)
  (not (apply #'SOME predicate sequences)))

(cl:defmacro AND (&rest forms)
  (cond
    ((null forms)
     T)
    ((null (rest forms))
     (first forms))
    (t
     `(IF ,(first forms) (AND ,@(rest forms))))))

(cl:defmacro COND (&rest clauses)
  (if (null clauses)
      nil
      (let ((clause (first clauses)))
	(case (length clause)
	  (0	(ERROR 'PROGRAM-ERROR))
	  (1	`(OR ,(first clause) (COND ,@(rest clauses))))
	  (t	`(IF ,(first clause)
		     (PROGN ,@(rest clause))
		     (COND ,@(rest clauses))))))))

;;; Special Operator: IF

(cl:defmacro OR (&rest forms)
  (cond
    ((null forms)		nil)
    ((null (rest forms))	(first forms))
    (t				(with-gensyms (x)
				  `(LET ((,x ,(first forms)))
				    (IF ,x ,x (OR ,@(rest forms))))))))

(cl:defmacro WHEN (condition &body body)
  `(IF ,condition (PROGN ,@body)))

(cl:defmacro UNLESS (condition &body body)
  `(IF ,condition nil (PROGN ,@body)))

(cl:defmacro CASE (form &rest clauses)
  (let ((val (gensym))
	(seen-otherwise nil))
    `(LET ((,val ,form))
       (COND
	 ,@(mapcar (destructuring-lambda ((key &rest forms))
		     (when seen-otherwise
		       (ERROR 'PROGRAM-ERROR))
		     (setq seen-otherwise
			   (memq key '(T OTHERWISE)))
		     (cond
		       (seen-otherwise
			`(T (PROGN ,@forms)))
		       ((null key)
			'(nil))
		       ((atom key)
			`((EQL ,val (QUOTE ,key))
			  (PROGN ,@forms)))
		       (t
			`((MEMBER ,val (QUOTE ,key))
			  (PROGN ,@forms)))))
		   clauses)))))

(cl:defmacro CCASE (place &rest clauses)
  (with-gensyms (block loop)
    `(BLOCK ,block
       (TAGBODY
	 ,loop
	 (RESTART-BIND ((STORE-VALUE (LAMBDA (object)
				       (SETF ,place object)
				       (GO ,loop))))
	   (RETURN-FROM ,block (ECASE ,place ,@clauses)))))))

(cl:defmacro ECASE (form &rest clauses)
  (with-gensyms (val)
    `(LET ((,val ,form))
       (CASE ,val
	 ,@(mapcar (destructuring-lambda ((&whole clause key &rest forms))
		     (if (memq key  '(T OTHERWISE))
			 `((,key) ,@forms)
			 clause))
		   clauses)
	 (T (type-error ,val (QUOTE (MEMBER ,@(mappend
					       (lambda (x)
						 (ensure-list (first x)))
					       clauses)))))))))

(cl:defmacro TYPECASE (form &rest clauses)
  (let ((val (gensym))
	(seen-otherwise nil))
    `(LET ((,val ,form))
       (COND
	 ,@(mapcar (destructuring-lambda ((key &rest forms))
		     (when seen-otherwise
		       (ERROR (QUOTE PROGRAM-ERROR)))
		     (setq seen-otherwise
			   (memq key '(T OTHERWISE)))
		     `(,(if seen-otherwise
			    T
			    `(TYPEP ,val (QUOTE ,key)))
		       (PROGN ,@forms)))
		   clauses)))))

(cl:defmacro CTYPECASE (place &rest clauses)
  (with-gensyms (block loop)
    `(BLOCK ,block
       (TAGBODY
	 ,loop
	 (RESTART-BIND ((STORE-VALUE (LAMBDA (object)
				       (SETF ,place object)
				       (GO ,loop))))
	   (RETURN-FROM ,block (ETYPECASE ,place ,@clauses)))))))

(cl:defmacro ETYPECASE (form &rest clauses)
  (with-gensyms (val)
    `(LET ((,val ,form))
       (TYPECASE ,val
	 ,@clauses
	 ,@(unless (some (lambda (clause) (eq (first clause) T)) clauses)
	    `((T (type-error ,val
		             (QUOTE (OR ,@(mapcar #'first clauses)))))))))))

(defmacro* MULTIPLE-VALUE-BIND (vars form &body body)
  (case (length vars)
    (0	`(progn ,form ,@body))
    (1	`(let ((,(first vars) ,form)) ,@body))
    (t	(let ((n -1))
	  `(let* ((,(first vars) ,form)
		  ,@(mapcar (lambda (var) `(,var (nth ,(incf n) mvals)))
			    (rest vars)))
	     ,@body)))))

(cl:defmacro MULTIPLE-VALUE-BIND (vars form &body body)
  (case (length vars)
    (0	`(PROGN ,form ,@body))
    (1	`(LET ((,(first vars) ,form)) ,@body))
    (t	(with-gensyms (ignore)
	  `(MULTIPLE-VALUE-CALL
	    (LAMBDA (&OPTIONAL ,@vars &REST ,ignore) ,@body) ,form)))))

;;; MULTIPLE-VALUE-CALL is a special operator.

(defmacro* MULTIPLE-VALUE-LIST (form)
  (let ((val (gensym)))
    `(let ((,val ,form))
       (if (zerop nvals)
	   nil
	   (cons ,val mvals)))))

(cl:defmacro MULTIPLE-VALUE-LIST (form)
  `(MULTIPLE-VALUE-CALL (FUNCTION LIST) ,form))

;;; MULTIPLE-VALUE-PROG1 is a special operator.

(defmacro* MULTIPLE-VALUE-SETQ (vars form)
  (if (null vars)
      form
      (let ((n -1))
	`(setq ,(first vars) ,form
	       ,@(mappend (lambda (var) `(,var (nth ,(incf n) mvals)))
			  (rest vars))))))

(cl:defmacro MULTIPLE-VALUE-SETQ (vars form)
  (let ((vals (gensym))
	(n -1))
    `(LET ((,vals (MULTIPLE-VALUE-LIST ,form)))
       (SETQ ,@(mappend (lambda (var) `(,var (NTH ,(incf n) ,vals))) vars))
       (FIRST ,vals))))

(defun VALUES (&rest vals)
  (VALUES-LIST vals))

(defun VALUES-LIST (list)
  (setq nvals (length list))
  (setq mvals (cdr-safe list))
  (car list))

(DEFCONSTANT MULTIPLE-VALUES-LIMIT 20)

(defmacro* NTH-VALUE (n form)
  (cond
    ((eq n 0)		`(cl:values ,form))
    ((integerp n)	`(progn ,form (cl:values (nth ,(1- n) mvals))))
    (t			`(progn ,form (cl:values (nth (1- ,n) mvals))))))

(cl:defmacro NTH-VALUE (n form)
  `(NTH ,n (MULTIPLE-VALUE-LIST ,form)))

(defun expand-prog (let bindings body)
  (MULTIPLE-VALUE-BIND (body decl) (parse-body body)
    `(BLOCK nil
       (,let ,bindings
	 (DECLARE ,@decl)
	 (TAGBODY ,@body)))))

(cl:defmacro PROG (bindings &body body)
  (expand-prog 'LET bindings body))

(cl:defmacro PROG* (bindings &body body)
  (expand-prog 'LET* bindings body))

(cl:defmacro PROG1 (form1 &rest forms)
  (with-gensyms (val)
    `(LET ((,val ,form1))
       ,@forms
       ,val)))

(cl:defmacro PROG2 (form1 form2 &rest forms)
  (with-gensyms (val)
    `(PROGN
       ,form1
       (LET ((,val ,form2))
	 ,@forms
	 ,val))))

;;; Special Operator: PROGN

(cl:defmacro DEFINE-MODIFY-MACRO (name lambda-list fn &optional documentation)
  (with-gensyms (place env temps values variables setter getter)
    `(DEFMACRO ,name (,place ,@lambda-list &ENVIRONMENT ,env)
       ,documentation
       (MULTIPLE-VALUE-BIND (,temps ,values ,variables ,setter ,getter)
	   (GET-SETF-EXPANSION ,place ,env)
	 (BACKQUOTE
	   (LET* ((COMMA-AT (MAPCAR (FUNCTION LIST) ,temps ,values))
		  ((COMMA (FIRST ,variables))
		   ;; TODO: only params from lambda-list
		   (,fn (COMMA ,getter) ,@lambda-list)))
	     (COMMA ,setter)))))))

(defmacro* DEFSETF (access-fn &rest args)
  (case (length args)
    (0 (ERROR "Syntax error"))
    (1 (short-form-defsetf access-fn (first args)))
    (t (apply #'long-form-defsetf access-fn args))))

(defun short-form-defsetf (access-fn update-fn)
  `(DEFINE-SETF-EXPANDER ,access-fn (&rest args)
     (let ((var (gensym))
	   (temps (map-to-gensyms args)))
       (cl:values temps
		  args
		  (list var)
		  (append '(,update-fn) temps (list var))
		  (list* ',access-fn temps)))))

(defun* long-form-defsetf (access-fn lambda-list variables &body body)
  (let ((args (remove-if (lambda (x) (memq x '(&optional &rest &key)))
			 lambda-list)))
    `(DEFINE-SETF-EXPANDER ,access-fn ,lambda-list
       (let* ((var (gensym))
	     (temps (map-to-gensyms ',args))
	     (,(first variables) var))
	 (cl:values temps
		    (list ,@args)
		    (list var)
		    (apply (lambda ,lambda-list ,@body) temps)
		    (cons ',access-fn temps))))))

(cl:defmacro DEFSETF (access-fn &rest args)
  (case (length args)
    (0	(ERROR "Syntax error"))
    (1	(cl-short-form-defsetf access-fn (first args)))
    (2	(cond
	  ((STRINGP (second args))
	   (cl-short-form-defsetf access-fn (first args) (second args)))
	  ((listp (second args))
	   (apply #'cl-long-form-defsetf access-fn args))
	  (t
	   (type-error (second args) '(OR STRING LIST)))))
    (t	(apply #'cl-long-form-defsetf access-fn args))))

(defun cl-short-form-defsetf (access-fn update-fn &optional doc)
  (with-gensyms (args var temps)
    `(DEFINE-SETF-EXPANDER ,access-fn (&REST ,args)
      (LET ((,var (GENSYM))
	    (,temps (map-to-gensyms ,args)))
	(VALUES ,temps
		,args
		(LIST ,var)
		(BACKQUOTE (,update-fn (COMMA-AT ,temps) (COMMA ,var)))
		(BACKQUOTE (,access-fn (COMMA-AT ,temps))))))))

(defun* cl-long-form-defsetf (access-fn lambda-list variables &body body)
  (let ((args (remove-if (lambda (x) (memq x LAMBDA-LIST-KEYWORDS))
			 lambda-list)))
    (with-gensyms (var temps)
      `(DEFINE-SETF-EXPANDER ,access-fn ,lambda-list
	(LET* ((,var (GENSYM))
	       (,temps (map-to-gensyms (QUOTE ,args)))
	       (,(first variables) ,var))
	  (VALUES ,temps
		  (LIST ,@args)
		  (LIST ,var)
		  (APPLY (LAMBDA ,lambda-list ,@body) ,temps)
		  (BACKQUOTE (,access-fn (COMMA-AT ,temps)))))))))

(defvar *setf-expanders* (make-hash-table))

(defmacro* DEFINE-SETF-EXPANDER (access-fn lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',access-fn *setf-expanders*)
           ,(make-macro-el-function access-fn lambda-list body))
     ',access-fn))

(cl:defmacro DEFINE-SETF-EXPANDER (access-fn lambda-list &body body)
  `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
      (puthash (QUOTE ,access-fn) 
               ,(make-macro-function access-fn lambda-list body)
	       *setf-expanders*)
      (QUOTE ,access-fn)))

(DEFINE-SETF-EXPANDER VALUES (&rest forms)
  (let ((temporaries nil)
	(values nil)
	(vars nil)
	(setters nil)
	(getters nil))
    (dolist (form forms)
      (MULTIPLE-VALUE-BIND (temps vals variables setter getter)
	  (GET-SETF-EXPANSION form nil) ;TODO: env
	(setq temporaries (append temporaries temps))
	(setq values (append values vals))
	(push (first variables) vars)
	(push setter setters)
	(push getter getters)))
    (cl:values temporaries
	       values
	       (nreverse vars)
	       `(PROGN ,@(nreverse setters))
	       `(VALUES ,@(nreverse getters)))))

(DEFSETF FDEFINITION (name) (fn)
  `(COND
    ((SYMBOLP ,name)
     (SETF (SYMBOL-FUNCTION ,name) ,fn))
    ((setf-name-p ,name)
     (puthash (SECOND ,name) ,fn *setf-definitions*))
    (T
     (not-function-name-error ,name))))

(DEFSETF MACRO-FUNCTION (name &optional env) (fn)
  `(IF (NULL ,env)
       (puthash ,name ,fn *macro-functions*)
       (set-local-macro ,name ,fn ,env)))

(DEFSETF COMPILER-MACRO-FUNCTION (name &optional env) (fn)
  `(puthash ,name ,fn *compiler-macro-functions*))

(DEFINE-SETF-EXPANDER THE (type form)
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION form nil) ;TODO: env
    (with-gensyms (val)
      (cl:values temps
		 values
		 (list val)
		 `(LET ((,(first variables) (THE ,type ,val))) ,setter)
		 getter))))

(defun GET-SETF-EXPANSION (place &optional env)
  (cond
   ((consp place)
    (let* ((name (car place))
	   (fn (gethash name *setf-expanders*)))
      (if fn
	  (FUNCALL fn place env)
	  (MULTIPLE-VALUE-BIND (place expandedp) (MACROEXPAND place env)
	    (if expandedp
		(GET-SETF-EXPANSION place env)
		(let ((temps (map-to-gensyms (rest place)))
		      (var (gensym)))
		  (cl:values temps
			     (rest place)
			     (list var)
			     `(FUNCALL (FUNCTION (SETF ,name)) ,var ,@temps)
			     `(,name ,@temps))))))))
   ((symbolp place)
    (if (eq (nth-value 0 (variable-information place env)) :symbol-macro)
	(GET-SETF-EXPANSION (MACROEXPAND place env) env)
	(let ((var (gensym)))
	  (cl:values nil nil (list var) `(SETQ ,place ,var) place))))
   (t
    (type-error place '(OR CONS SYMBOL)))))

(defmacro* SETF (place value &rest more &environment env)
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place env)
    `(let* (,@(MAPCAR #'list temps values)
	    (,(first variables) ,value))
       ,setter
       ,@(when more
	   `((SETF ,@more))))))

(cl:defmacro SETF (&rest forms &environment env)
  (when (oddp (length forms))
    (ERROR 'PROGRAM-ERROR))
  (when (not (null forms))
    (let ((place (first forms))
	  (value (second forms))
	  (more (cddr forms)))
      (MULTIPLE-VALUE-BIND (temps values variables setter getter)
	  (GET-SETF-EXPANSION place env)
	`(LET* ,(MAPCAR #'list temps values)
	   (MULTIPLE-VALUE-BIND ,variables ,value
	     ,setter
	     ,(if more
		  `(SETF ,@more)
		  `(VALUES ,@variables))))))))

(cl:defmacro PSETF (&rest forms &environment env)
  (when (oddp (length forms))
    (ERROR 'PROGRAM-ERROR))
  (when (not (null forms))
    (let ((place (first forms))
	  (value (second forms))
	  (more (cddr forms)))
      (MULTIPLE-VALUE-BIND (temps values variables setter getter)
	  (GET-SETF-EXPANSION place env)
	`(LET* ,(MAPCAR #'list temps values)
	   (MULTIPLE-VALUE-BIND ,variables ,value
	     ,@(when more
	         `((PSETF ,@more)))
	     ,setter
	     nil))))))

(cl:defmacro SHIFTF (place x &rest more) ;TODO: &environment
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place)
    (with-gensyms (val)
      `(LET* (,@(MAPCAR #'list temps values)
	      (,val ,getter)
	      (,(first variables) ,(if (null more) x `(SHIFTF ,x ,@more))))
	 ,setter
	 ,val))))

(cl:defmacro ROTATEF (&rest places) ;TODO: &environment
  (if (or (null places) (null (rest places)))
      nil
      (let ((place (first places))
	    (places (rest places))
	    (val (gensym)))
      (MULTIPLE-VALUE-BIND (temps values variables setter getter)
	  (GET-SETF-EXPANSION place env)
	`(LET* (,@(MAPCAR #'list temps values)
		(,val ,getter)
		(,(first variables) (SHIFTF ,@places ,val)))
	   ,setter
	   nil)))))
    
;;; CONTROL-ERROR, PROGRAM-ERROR, and UNDEFINED-FUNCTION are defined
;;; in cl-conditions.el.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-flow.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-packages.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 11, Packages.

(IN-PACKAGE "EMACS-CL")

;;; A note about the EMACS-LISP package: This package isn't
;;; implemented the same way all other packages are.  It doesn't have
;;; a hash table or a list of exported symbols.  Instead, symbols are
;;; searched with intern-soft, and all symbols are exported.

(defconst kw:EXTERNAL (keyword "EXTERNAL"))
(defconst kw:INTERNAL (keyword "INTERNAL"))
(defconst kw:INHERITED (keyword "INHERITED"))

;;; The PACKAGE system class is built in.

(defun PACKAGE-NAME (package)
  (aref (FIND-PACKAGE package) 1))

(defun PACKAGE-NICKNAMES (package)
  (aref (FIND-PACKAGE package) 2))

(defun PACKAGE-SHADOWING-SYMBOLS (package)
  (aref (FIND-PACKAGE package) 3))

(defun PACKAGE-USE-LIST (package)
  (aref (FIND-PACKAGE package) 4))

(defun PACKAGE-USED-BY-LIST (package)
  (aref (FIND-PACKAGE package) 5))

(defun package-table (package)
  (aref package 6))

(defun package-exported (package)
  (aref package 7))

(defun PACKAGEP (package)
  (vector-and-typep package 'PACKAGE))

(DEFVAR *all-packages* nil)

(defun* EXPORT (symbols &optional (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (do-list-designator (sym symbols (cl:values T))
      (MULTIPLE-VALUE-BIND (s status) (FIND-SYMBOL (SYMBOL-NAME sym) package)
	(cond
	  ((eq status kw:INHERITED)
	   (IMPORT sym package))
	  ((null status)
	   (ERROR 'PACKAGE-ERROR (kw PACKAGE) package))))
      (pushnew sym (aref package 7)))))

(defun FIND-PACKAGE (name)
  (if (PACKAGEP name)
      name
      (let ((string (STRING name)))
	(find-if 
	 (lambda (p)
	   (or (STRING= string (PACKAGE-NAME p))
	       (find string (PACKAGE-NICKNAMES p) :test 'equal)))
	 *all-packages*))))

(cl:defun MAKE-PACKAGE (name &KEY NICKNAMES USE)
  (let ((package (make-vector 8 'PACKAGE))
	(use-packages (mapcar #'FIND-PACKAGE USE)))
    (aset package 1 (STRING name))			;name
    (aset package 2 (mapcar #'STRING NICKNAMES))	;nicknames
    (aset package 3 nil)				;shadowing symbols
    (aset package 4 use-packages)			;use-list
    (aset package 5 nil)				;used-by-list
    (aset package 6 (make-hash-table :test 'equal))	;table
    (aset package 7 nil)				;exported symbols
    (dolist (p use-packages)
      (push package (aref p 5)))
    (push package *all-packages*)
    package))

(defvar *keyword-package*
  (MAKE-PACKAGE "KEYWORD"))
(defvar *emacs-lisp-package*
  (MAKE-PACKAGE "EMACS-LISP" (kw NICKNAMES) '("EL")))
(defvar *emacs-cl-package*
  (MAKE-PACKAGE "EMACS-COMMON-LISP" (kw NICKNAMES) '("EMACS-CL")))
(defvar *mop-package*
  (MAKE-PACKAGE "EMACS-COMMON-LISP-MOP" (kw NICKNAMES) '("MOP")))
(defvar *cl-package*
  (MAKE-PACKAGE "COMMON-LISP" (kw NICKNAMES) '("CL")))
(defvar *cl-user-package*
  (MAKE-PACKAGE "COMMON-LISP-USER"
		(kw NICKNAMES) '("CL-USER") (kw USE) '("CL" "EMACS-CL")))

(defconst not-found (cons nil nil))

(defvar *find-symbol-searched-packages* nil)

(defun* FIND-SYMBOL (string &optional (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (when (memq package *find-symbol-searched-packages*)
      (return-from FIND-SYMBOL (cl:values nil nil)))
    (cond
      ((null package)
       (ERROR "Package ~S not found" package-designator))
      ;; Special EMACS-LISP magic: EMACS-LISP doesn't have a separate
      ;; table, use intern-soft to find symbols instead.
      ((eq package *emacs-lisp-package*)
       (let ((symbol (intern-soft string)))
	 (if symbol
	     (progn
	       (unless (SYMBOL-PACKAGE symbol)
		 (setf (SYMBOL-PACKAGE symbol) *emacs-lisp-package*))
	       (cl:values symbol kw:EXTERNAL))
	     (cl:values nil nil))))
      (t
       (let* ((table (package-table package))
	      (symbol (gethash string table not-found)))
	 (if (not (eq symbol not-found))
	     (cl:values symbol
			(if (memq symbol (package-exported package))
			    kw:EXTERNAL
			    kw:INTERNAL))
	     (let ((*find-symbol-searched-packages*
		    (cons package *find-symbol-searched-packages*)))
	       (dolist (p (PACKAGE-USE-LIST package) (cl:values nil nil))
		 (MULTIPLE-VALUE-BIND (symbol found) (FIND-SYMBOL string p)
		   (when (eq found kw:EXTERNAL)
		     (return-from FIND-SYMBOL
		       (cl:values symbol kw:INHERITED))))))))))))

(defun FIND-ALL-SYMBOLS (name)
  (let ((string (STRING name))
	(syms nil))
    (dolist (p *all-packages* (cl:values syms))
      (MULTIPLE-VALUE-BIND (sym status) (FIND-SYMBOL string p)
	(if (or (eq status :internal) (eq status kw:EXTERNAL))
	    (push sym syms))))))

(defun* IMPORT (symbols &optional (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (do-list-designator (symbol symbols (cl:values T))
      (MULTIPLE-VALUE-BIND (sym found)
	  (FIND-SYMBOL (SYMBOL-NAME symbol) package)
	(when (and found (not (eq sym symbol)))
	  (ERROR "Importing ~S into ~S clash with existing symbol"
		 sym package)))
      (setf (gethash (SYMBOL-NAME symbol) (package-table package)) symbol)
      (when (null (SYMBOL-PACKAGE symbol))
	(setf (SYMBOL-PACKAGE symbol) package)))))

(defun LIST-ALL-PACKAGES ()
  (copy-list *all-packages*))

(defun RENAME-PACKAGE (package-designator name &optional new-nicknames)
  (let ((package (FIND-PACKAGE package-designator)))
    (aset package 1 (if (PACKAGEP name)
			(PACKAGE-NAME name)
			(STRING name)))
    (aset package 2 (mapcar #'STRING new-nicknames))))

(defun* SHADOW (symbol-names &optional (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (do-list-designator (name symbol-names (cl:values T))
      (setq name (STRING name))
      (MULTIPLE-VALUE-BIND (sym status) (FIND-SYMBOL name package)
	(when (or (null status) (eq status kw:INHERITED))
	  (setq sym (INTERN name package)))
	(pushnew sym (aref package 3))))))

(defun* SHADOWING-IMPORT (symbols &optional (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (do-list-designator (symbol symbols (cl:values T))
      (MULTIPLE-VALUE-BIND (sym found)
	  (FIND-SYMBOL (SYMBOL-NAME symbol) package)
	(when found
	  (UNINTERN sym package)))
      (setf (gethash (SYMBOL-NAME symbol) (package-table package)) symbol)
      (when (null (SYMBOL-PACKAGE symbol))
	(setf (SYMBOL-PACKAGE symbol) package)))))

(defun DELETE-PACKAGE (package-designator)
  (let ((package (FIND-PACKAGE package-designator)))
    (if (null (aref package 1))
	nil
	(let ((package (FIND-PACKAGE package-designator)))
	  (unless package
	    (ERROR "Package ~S not found" package-designator))
	  (when (PACKAGE-USED-BY-LIST package)
	    (ERROR 'PACKAGE-ERROR))
	  (dolist (p (PACKAGE-USE-LIST package))
	    (aset p 5 (delete package (PACKAGE-USED-BY-LIST p))))
	  (setq *all-packages* (delete package *all-packages*))
	  (aset package 1 nil)
	  T))))

(defun package-symbols (package types)
  (unless (null package)
    (let ((result nil))
      (maphash (lambda (ignore symbol)
		 (MULTIPLE-VALUE-BIND (sym status)
		     (FIND-SYMBOL (SYMBOL-NAME symbol) package)
		   (when (and status (member status types))
		     (push (cons sym status) result))))
	       (package-table package))
      (when (memq kw:INHERITED types)
	(dolist (p (PACKAGE-USE-LIST package))
	  (dolist (s (package-exported p))
	    (push (cons s kw:INHERITED) result))))
      result)))

(cl:defmacro WITH-PACKAGE-ITERATOR ((name packages &rest types) &body body)
  (with-gensyms (p s)
    `(LET* ((,p (MAPCAR (FUNCTION FIND-PACKAGE) (ensure-list ,packages)))
	    (,s (package-symbols (CAR ,p) (QUOTE ,types))))
       (MACROLET ((,name ()
		    (QUOTE
		      (IF (AND (NULL ,s) (NULL (CDR ,p)))
			  nil
			  (PROGN
			    (WHEN (NULL ,s)
			      (SETQ ,p (CDR ,p))
			      (SETQ ,s (package-symbols (CAR ,p)
							(QUOTE ,types))))
			    (LET ((cons (POP ,s)))
			      (cl:values T
					 (CAR cons) (CDR cons) (CAR ,p))))))))
	  ,@body))))

(cl:defun UNEXPORT (symbols &OPTIONAL (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (do-list-designator (symbol symbols)
      (MULTIPLE-VALUE-BIND (sym found)
	  (FIND-SYMBOL (symbol-name symbol) package)
	(if (and found (eq sym symbol))
	    (aset package 7 (delete symbol (aref package 7)))
	    (ERROR 'PACKAGE-ERROR (kw PACKAGE) package)))))
  (cl:values T))

(cl:defun UNINTERN (symbol &OPTIONAL (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (when (eq (SYMBOL-PACKAGE symbol) package)
      (setf (SYMBOL-PACKAGE symbol) nil))
    (let* ((table (package-table package))
	   (name (symbol-name symbol))
	   (sym (gethash name table not-found)))
      (unless (eq sym not-found)
	(remhash name table)))
    (aset package 3 (delete symbol (aref package 3)))
    (aset package 7 (delete symbol (aref package 7))))
  'T)

;;; Emacs' load function doesn't restore *PACKAGE* after loading a file.
; (defmacro IN-PACKAGE (package)
;   `(eval-when (:compile-toplevel :load-toplevel :execute)
;     (setq *PACKAGE* (FIND-PACKAGE ,package))))

(cl:defmacro IN-PACKAGE (package)
  `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
     (LET ((p (FIND-PACKAGE (QUOTE ,package))))
       (IF p
	   (SETQ *PACKAGE* p)
	   (ERROR (QUOTE PACKAGE-ERROR) ,(kw PACKAGE) (QUOTE ,package))))))

(cl:defun UNUSE-PACKAGE (packages-to-unuse &OPTIONAL (package *PACKAGE*))
  (let ((package (FIND-PACKAGE package)))
    (do-list-designator (p packages-to-unuse)
      (let ((p (FIND-PACKAGE p)))
	(aset package 4 (delete p (PACKAGE-USE-LIST package)))
	(aset p 5 (delete package (PACKAGE-USED-BY-LIST p))))))
  T)

(cl:defun USE-PACKAGE (packages-to-use &OPTIONAL (package *PACKAGE*))
  (let ((package (FIND-PACKAGE package)))
    (do-list-designator (p packages-to-use)
      (pushnew (FIND-PACKAGE p) (aref package 4))))
  T)

(cl:defmacro DEFPACKAGE (name &rest options)
  (let ((nicknames nil)
	(documentation nil)
	(use-list nil)
	(shadow-list nil)
	(shadowing-import nil)
	(import-list nil)
	(intern-list nil)
	(export-list nil)
	(doc nil))
    (dolist (option options)
      (let ((keyword (first option)))
	(cond
	  ((eq keyword (kw NICKNAMES))
	   (dolist (i (rest option))
	     (push (STRING i) nicknames)))
	  ((eq keyword (kw DOCUMENTATION))
	   (setq doc (second option)))
	  ((eq keyword (kw SHADOW))
	   (dolist (i (rest option))
	     (push (STRING i) shadow-list)))
	  ((eq keyword (kw SHADOWING-IMPORT-FROM))
	   (push (mapcar #'STRING (rest option)) shadowing-import))
	  ((eq keyword (kw USE))
	   (dolist (i (rest option))
	     (push (STRING i) use-list)))
	  ((eq keyword (kw IMPORT-FROM))
	   (push (mapcar #'STRING (rest option)) shadowing-import))
	  ((eq keyword (kw INTERN))
	   (dolist (i (rest option))
	     (push (STRING i) intern-list)))
	  ((eq keyword (kw EXPORT))
	   (dolist (i (rest option))
	     (push (STRING i) export-list)))
	  ((eq keyword (kw SIZE))
	   nil)
	  (t
	   (ERROR "Unknown DEFPACKAGE option: ~S" option)))))
    (with-gensyms (x package)
      `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
	(%defpackage
	 ,(STRING name)
	 (QUOTE ,nicknames)
	 (QUOTE ,shadow-list)
	 (QUOTE ,shadowing-import)
	 (QUOTE ,use-list)
	 (QUOTE ,import-list)
	 (QUOTE ,intern-list)
	 (QUOTE ,export-list))))))

(defun %defpackage (name nicknames shadow-list shadowing-import
		    use-list import-list intern-list export-list)
  (let ((package (FIND-PACKAGE name)))
    (if package
	(aset package 2 (mapcar #'STRING nicknames))
	(setq package (MAKE-PACKAGE name (kw NICKNAMES) nicknames)))
    (SHADOW shadow-list package)
    (dolist (list shadowing-import)
      (let ((p (FIND-PACKAGE (first list))))
	(dolist (name (rest list))
	  (SHADOWING-IMPORT (FIND-SYMBOL name p) package))))
    (dolist (p use-list)
      (USE-PACKAGE p package))
    (dolist (list import-list)
      (let ((p (FIND-PACKAGE (first list))))
	(dolist (name (rest list))
	  (IMPORT (FIND-SYMBOL name p) package))))
    (dolist (x intern-list)
      (INTERN x package))
    (dolist (name export-list)
      (EXPORT (INTERN name package) package))
    package))

(defun el-maphash (fn hash)
  (maphash (lambda (k v) (FUNCALL fn k v)) hash))

(defmacro* DO-SYMBOLS ((var &optional package result) &body body)
  (with-gensyms (p1 p2 p3 ignore)
    `(let* ((,p1 ,package)
	    (,p2 (if ,p1 (FIND-PACKAGE ,p1) *PACKAGE*)))
       (dolist (,p3 (cons ,p2 (PACKAGE-USE-LIST ,p2)))
	 (el-maphash (lambda (,ignore ,var) ,@body)
		     (package-table ,p3)))
       ,result)))

(cl:defmacro DO-SYMBOLS ((var &optional package result) &body body)
  (with-gensyms (p1 p2 p3 ignore)
    `(LET* ((,p1 ,package)
	    (,p2 (IF ,p1 (FIND-PACKAGE ,p1) *PACKAGE*)))
       (DOLIST (,p3 (CONS ,p2 (PACKAGE-USE-LIST ,p2)))
	 (el-maphash (LAMBDA (,ignore ,var) ,@body)
		     (package-table ,p3)))
       ,result)))

(cl:defmacro DO-EXTERNAL-SYMBOLS ((var &optional package result) &body body)
  (with-gensyms (p1 p2)
    `(LET* ((,p1 ,package)
	    (,p2 (IF ,p1 (FIND-PACKAGE ,p1) *PACKAGE*)))
       (DOLIST (,var (aref ,p2 7) ,result)
	 ,@body))))

(cl:defmacro DO-ALL-SYMBOLS ((var &optional result) &body body)
  (with-gensyms (p ignore)
    `(DOLIST (,p *all-packages* ,result)
       (el-maphash (LAMBDA (,ignore ,var) ,@body)
	           (package-table ,p)))))

(cl:defun INTERN (name &OPTIONAL (package-designator *PACKAGE*))
  (let ((package (FIND-PACKAGE package-designator)))
    (when (null package)
      (ERROR "Package ~S not found" package-designator))
    (MULTIPLE-VALUE-BIND (symbol found) (FIND-SYMBOL name package)
      (if found
	  (cl:values symbol found)
	  (let ((symbol (if (eq package *emacs-lisp-package*)
			    (intern name)
			    (make-symbol name))))
	    (setf (SYMBOL-PACKAGE symbol) package)
	    (unless (eq package *emacs-lisp-package*)
	      (setf (gethash name (package-table package)) symbol))
	    (when (eq package *keyword-package*)
	      (set symbol symbol)
	      (pushnew symbol (aref package 7)))
	    (cl:values symbol nil))))))

(DEFVAR *PACKAGE* *cl-user-package*)

;;; PACKAGE-ERROR and PACKAGE-ERROR-PACKAGE are defined in cl-conditions.el.



;;; Bootstrap magic: take the list of symbols created by the old
;;; keyword function, and import them into the KEYWORD package.
(dolist (sym *initial-keywords*)
  (IMPORT sym *keyword-package*)
  (EXPORT sym *keyword-package*))

;;; Redefine the keyword function (initially defined in utils.el).
(defun keyword (name)
  (cl:values (INTERN name *keyword-package*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-packages.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-printer.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 22, Printer.

(IN-PACKAGE "EMACS-CL")

(cl:defun COPY-PPRINT-DISPATCH (&OPTIONAL (table *PRINT-PPRINT-DISPATCH*))
  (unless table
    (setq table *initial-pprint-dispatch*))
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (k v) (setf (gethash k copy) (cons (car v) (cdr v)))
		     table)
    copy)))

;;; FORMATTER is defined in cl-format.el.

(cl:defun PPRINT-DISPATCH (object &OPTIONAL (table *PRINT-PPRINT-DISPATCH*))
  (unless table
    (setq table *initial-pprint-dispatch*))
  (let ((fn nil)
	(priority nil))
    (maphash (lambda (k v)
	       (when (and (TYPEP object k)
			  (or (null priority)
			      (cl:> (cdr v) priority)))
		 (setq fn (car v)
		       priority (cdr v))))
	     table)
    (cl:values fn priority)))

;;; TODO: Local Macro PPRINT-EXIT-IF-LIST-EXHAUSTED

;;; TODO: Function PPRINT-FILL
;;; TODO:          PPRINT-LINEAR
;;; TODO:          PPRINT-TABULAR

;;; TODO: Function PPRINT-INDENT

;;; TODO: Macro PPRINT-LOGICAL-BLOCK

;;; TODO: Function PPRINT-NEWLINE

;;; TODO: Local Macro PPRINT-POP

;;; TODO: Function PPRINT-TAB

(defvar *print-circle-table* nil)
(defvar *print-circle-counter* 0)

(defun check-circles (object)
  (unless (or (symbolp object)
	      (and (vectorp object) (eql (length object) 0))
	      (INTEGERP object))
    (let ((n (gethash object *print-circle-table*)))
      (cond
	((null n)
	 (setf (gethash object *print-circle-table*) t)
	 (cond
	   ((consp object)
	    (check-circles (car object))
	    (check-circles (cdr object)))
	   ((arrayp object)
	    (dotimes (i (length object))
	      (check-circles (aref object i))))))
	((eq n 0))
	(t
	 (setf (gethash object *print-circle-table*) 0))))))

(defun write-object (object stream)
  (if *PRINT-CIRCLE*
      (if *print-circle-table*
	  (let ((n (gethash object *print-circle-table*)))
	    (cond
	      ((eq n 0)
	       (WRITE-CHAR (ch 35) stream)
	       (print-integer (setf (gethash object *print-circle-table*)
				    (incf *print-circle-counter*))
			      stream 10)
	       (WRITE-CHAR (ch 61) stream)
	       (PRINT-OBJECT object stream))
	      ((integerp n)
	       (WRITE-CHAR (ch 35) stream)
	       (print-integer n stream 10)
	       (WRITE-CHAR (ch 35) stream))
	      (t
	       (PRINT-OBJECT object stream))))
	  (let ((*print-circle-table* (make-hash-table))
		(*print-circle-counter* 0))
	    (check-circles object)
	    (write-object object stream)))
      (or (and *PRINT-PRETTY*
	       (MULTIPLE-VALUE-BIND (fn found) (PPRINT-DISPATCH object)
		 (when found
		   (FUNCALL fn stream object)
		   t)))
	  (PRINT-OBJECT object stream))))

(defun positive-integer-p (object)
  (and (integerp object) (plusp object)))

;;; TODO: PRINT-OBJECT should be a generic function
(defun built-in-print-object (object stream)
  (cond
    ((INTEGERP object)
     (print-integer object stream *PRINT-BASE* *PRINT-RADIX*))
    ((floatp object)
     ;; http://www.common-lisp.net/paste/display/743
     (print-float object stream))
    ((symbolp object)
     (let ((name (SYMBOL-NAME object)))
       (if (printer-escaping-p)
	   (progn
	     (print-symbol-prefix object stream)
	     (print-symbol-name name stream))
	   (cond
	     ((eq *PRINT-CASE* (kw UPCASE))
	      (WRITE-STRING name stream))
	     ((eq *PRINT-CASE* (kw DOWNCASE))
	      (WRITE-STRING (STRING-DOWNCASE name) stream))
	     ((eq *PRINT-CASE* (kw CAPITALIZE))
	      (WRITE-STRING (symbol-name-capitalize name) stream))
	     (t
	      (type-error *PRINT-CASE* `(MEMBER ,(kw UPCASE) ,(kw DOWNCASE)
						,(kw CAPITALIZE))))))))
    ((CHARACTERP object)
     (if (printer-escaping-p)
	 (progn
	   (WRITE-STRING "#\\" stream)
	   (WRITE-STRING (or (and (ch= object 32) " ")
			     (CHAR-NAME object)
			     (string (CHAR-CODE object)))
			 stream))
	 (WRITE-CHAR object stream)))
    ((consp object)
     (let ((n 0))
       (WRITE-STRING "(" stream)
       (write-object (car object) stream)
       (setq object (cdr object))
       (while (consp object)
	 (if (and *print-circle-table*
		  (positive-integer-p (gethash object *print-circle-table*)))
	     (progn
	       (WRITE-STRING " . " stream)
	       (write-object object stream)
	       (setq object nil))
	     (progn
	       (WRITE-STRING " " stream)
	       (write-object (car object) stream)
	       (setq object (cdr object))
	       (when (and *PRINT-LENGTH* (cl:>= (incf n) *PRINT-LENGTH*))
		 (WRITE-STRING " ..." stream)
		 (setq object nil)))))
       (unless (null object)
	 (WRITE-STRING " . " stream)
	 (write-object object stream)))
     (WRITE-STRING ")" stream))
    ((FUNCTIONP object)
     (PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t (kw IDENTITY) t)
       (PRINC (function-name object) stream)))
    ((ratiop object)
     (WRITE (NUMERATOR object) (kw STREAM) stream)
     (WRITE-STRING "/" stream)
     (WRITE (DENOMINATOR object) (kw STREAM) stream (kw RADIX) nil))
    ((COMPLEXP object)
     (WRITE-STRING "#C(" stream)
     (WRITE (REALPART object) (kw STREAM) stream)
     (WRITE-STRING " " stream)
     (WRITE (IMAGPART object) (kw STREAM) stream)
     (WRITE-STRING ")" stream))
    ((BIT-VECTOR-P object)
     (cond
       (*PRINT-ARRAY*
	(WRITE-STRING "#*" stream)
	(dotimes (i (LENGTH object))
	  (PRIN1 (AREF object i) stream)))
       (t
	(PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t
						(kw IDENTITY) t)))))
    ((STRINGP object)
     (cond
       (*PRINT-ESCAPE*
	(WRITE-CHAR (ch 34) stream)
	(dotimes (i (LENGTH object))
	  (let ((char (CHAR object i)))
	    (case (CHAR-CODE char)
	      (34	(WRITE-STRING "\\\"" stream))
	      (92	(WRITE-STRING "\\\\" stream))
	      (t	(WRITE-CHAR char stream)))))
	(WRITE-CHAR (ch 34) stream))
       (t
	(WRITE-STRING object stream))))
    ((VECTORP object)
     (if *PRINT-ARRAY*
	 (let ((*PRINT-LENGTH* (if (BIT-VECTOR-P object) nil *PRINT-LENGTH*)))
	   (WRITE-STRING "#(" stream)
	   (catch 'too-long
	     (dotimes (i (LENGTH object))
	       (when (> i 0)
		 (WRITE-STRING " " stream))
	       (PRIN1 (AREF object i) stream)
	       (when (and *PRINT-LENGTH* (>= i *PRINT-LENGTH*))
		 (WRITE-STRING " ..." stream)
		 (throw 'too-long nil))))
	   (WRITE-STRING ")" stream))
	 (PRINT-UNREADABLE-OBJECT (object stream
				   (kw TYPE) t (kw IDENTITY) t))))
    ((ARRAYP object)
     (cond
       (*PRINT-ARRAY*
	(print-array object stream))
       (t
	(PRINT-UNREADABLE-OBJECT (object stream
				  (kw TYPE) t (kw IDENTITY) t)))))
    ((PACKAGEP object)
     (PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t)
       (PRIN1 (PACKAGE-NAME object) stream)))
    ((READTABLEP object)
     (PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t (kw IDENTITY) t)))
    ((TYPEP object 'FILE-STREAM)
     (PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t (kw IDENTITY) t)
       (PRIN1 (FILE-STREAM-filename object) stream)))
    ((STREAMP object)
     (PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t (kw IDENTITY) t)))
    ((TYPEP object 'CONDITION)
     (PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t (kw IDENTITY) t)))
    ((restartp object)
     (PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t (kw IDENTITY) t)
       (PRIN1 (RESTART-NAME object) stream)
       (when (RESTART-condition object)
	 (WRITE-STRING " " stream)
	 (PRIN1 (RESTART-condition object) stream))))
    ((PATHNAMEP object)
     (WRITE-STRING "#P" stream)
     (PRIN1 (NAMESTRING object) stream))
    ((structurep object)
     (WRITE-STRING "#S(" stream)
     (PRIN1 (TYPE-OF object) stream)
     (let ((index 0))
       (dolist (slot (struct-slots (aref object 0)))
	 (FORMAT stream " :~A ~S"
		 (SYMBOL-NAME (first slot)) (aref object (incf index)))))
     (WRITE-CHAR (ch 41) stream))
    ((classp object)
     (PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t)
       (PRIN1 (class-name object) stream)))
    ((subclassp (CLASS-OF object) +standard-object+)
     (PRINT-UNREADABLE-OBJECT (object stream (kw TYPE) t (kw IDENTITY) t)))
    (t
     ;; TODO:
     (if *PRINT-READABLY*
	 (ERROR 'PRINT-NOT-READABLE (kw OBJECT) object)
	 (WRITE-STRING "#<FIXME:UNPRINTABLE>" stream))))
  (cl:values object))

(defvar *object-identities* (make-hash-table :test #'eq :weakness t))

(defvar *identity-counter* 12345)

(defun object-identity (object)
  ;; TODO: Perhaps flush a non-weak hash table occasionally.
  (or (gethash object *object-identities*)
      (setf (gethash object *object-identities*) (incf *identity-counter*))))

(defmacro* PRINT-UNREADABLE-OBJECT ((object stream &rest keys) &body body)
  `(print-unreadable-object ,object ,stream (lambda () ,@body) ,@keys))

(cl:defmacro PRINT-UNREADABLE-OBJECT ((object stream &rest keys) &body body)
  `(print-unreadable-object ,object ,stream (LAMBDA () ,@body) ,@keys))

(cl:defun print-unreadable-object (object stream fn &KEY TYPE IDENTITY)
  (when *PRINT-READABLY*
    (ERROR 'PRINT-NOT-READABLE (kw OBJECT) object))
  (WRITE-STRING "#<" stream)
  (when TYPE
    (PRIN1 (TYPE-OF object) stream)
    (WRITE-STRING " " stream))
  (FUNCALL fn)
  (when IDENTITY
    (WRITE-STRING " {" stream)
    (PRIN1 (object-identity object) stream)
    (WRITE-STRING "}" stream))
  (WRITE-STRING ">" stream)
  nil)

(cl:defun SET-PPRINT-DISPATCH (type fn
			       &OPTIONAL (priority 0)
					 (table *PRINT-PPRINT-DISPATCH*))
  (if (null fn)
      (remhash type table)
      (setf (gethash type table) (cons fn priority)))
  nil)

(defun external-symbol-p (symbol)
  (eq (NTH-VALUE 1 (FIND-SYMBOL (SYMBOL-NAME symbol) (SYMBOL-PACKAGE symbol)))
      kw:EXTERNAL))

(defun print-symbol-prefix (symbol stream)
  (cond
    ((eq (SYMBOL-PACKAGE symbol) *keyword-package*)
     (WRITE-STRING ":" stream))
    ((eq (NTH-VALUE 0 (FIND-SYMBOL (SYMBOL-NAME symbol) *PACKAGE*)) symbol))
    ((null (SYMBOL-PACKAGE symbol))
     (when *PRINT-GENSYM*
       (WRITE-STRING "#:" stream)))
    (t
     (WRITE-STRING (PACKAGE-NAME (SYMBOL-PACKAGE symbol)) stream)
     (WRITE-STRING (if (external-symbol-p symbol) ":" "::") stream))))

(defun print-symbol-name (name stream)
  (let* ((read-sym (unless (EQUAL name "")
		     (READ-FROM-STRING name)))
	 (escape (not (and (symbolp read-sym)
			   (string= name (SYMBOL-NAME read-sym))))))
    (when escape
      (WRITE-STRING "|" stream))
    (WRITE-STRING (cond
		    (escape
		     name)
		    ((eq *PRINT-CASE* (kw UPCASE))
		     name)
		    ((eq *PRINT-CASE* (kw DOWNCASE))
		     (STRING-DOWNCASE name))
		    ((eq *PRINT-CASE* (kw CAPITALIZE))
		     (symbol-name-capitalize name))
		    (t
		     (type-error *PRINT-CASE*
				 `(MEMBER ,(kw UPCASE) ,(kw DOWNCASE)
					  ,(kw CAPITALIZE)))))
		  stream)
    (when escape
      (WRITE-STRING "|" stream))))

(defun printer-escaping-p ()
  (or *PRINT-READABLY* *PRINT-ESCAPE*))

(cl:defun WRITE (object &KEY
		 (ARRAY *PRINT-ARRAY*)
		 (BASE *PRINT-BASE*)
		 (CASE *PRINT-CASE*)
		 (CIRCLE *PRINT-CIRCLE*)
		 (ESCAPE *PRINT-ESCAPE*)
		 (GENSYM *PRINT-GENSYM*)
		 (LENGTH *PRINT-LENGTH*)
		 (LEVEL *PRINT-LEVEL*)
		 (LINES *PRINT-LINES*)
		 (MISER-WIDTH *PRINT-MISER-WIDTH*)
		 (PPRINT-DISPATCH *PRINT-PPRINT-DISPATCH*)
		 (PRETTY *PRINT-PRETTY*)
		 (RADIX *PRINT-RADIX*)
		 (READABLY *PRINT-READABLY*)
		 (RIGHT-MARGIN *PRINT-RIGHT-MARGIN*)
		 STREAM)
  (let ((stream (output-stream STREAM))
	(*PRINT-ARRAY* ARRAY)
	(*PRINT-BASE* BASE)
	(*PRINT-CASE* CASE)
	(*PRINT-CIRCLE* CIRCLE)
	(*PRINT-ESCAPE* ESCAPE)
	(*PRINT-GENSYM* GENSYM)
	(*PRINT-LENGTH* LENGTH)
	(*PRINT-LEVEL* LEVEL)
	(*PRINT-LINES* LINES)
	(*PRINT-MISER-WIDTH* MISER-WIDTH)
	(*PRINT-PPRINT-DISPATCH* PPRINT-DISPATCH)
	(*PRINT-PRETTY* PRETTY)
	(*PRINT-RADIX* RADIX)
	(*PRINT-READABLY* READABLY)
	(*PRINT-RIGHT-MARGIN* RIGHT-MARGIN))
    (write-object object stream))
  (cl:values object))

(defun symbol-name-capitalize (string)
  (setq string (copy-sequence string))
  (do ((i 0 (1+ i))
       (in-word-p nil))
      ((eq i (length string))
       string)
    (let* ((char (CHAR string i))
	   (alnump (ALPHANUMERICP char)))
      (when (UPPER-CASE-P char)
	(setf (CHAR string i)
	      (if in-word-p (CHAR-DOWNCASE char) (CHAR-UPCASE char))))
      (setq in-word-p alnump))))

(cl:defun print-integer (number stream &OPTIONAL (base 10) radix)
  (when radix
    (case base
      (2	(WRITE-STRING "#b" stream))
      (8	(WRITE-STRING "#o" stream))
      (10)
      (16	(WRITE-STRING "#x" stream))
      (t	(WRITE-CHAR (ch 35) stream)
		(print-integer base stream)
		(WRITE-STRING (ch 114) stream))))
  (cond
    ((ZEROP number)
     (WRITE-STRING "0" stream))
    ((MINUSP number)
     (WRITE-STRING "-" stream)
     (setq number (cl:- number))))
  (print-digits number stream base)
  (when (and radix (eq base 10))
    (WRITE-STRING "." stream)))

(defun print-digits (number stream base)
  (when (PLUSP number)
    (MULTIPLE-VALUE-BIND (number digit) (TRUNCATE number base)
      (print-digits number stream base)
      (WRITE-CHAR (AREF "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" digit)
		  stream))))

(defun write-char-to-*standard-output* (char)
  (WRITE-CHAR (cl-char char) *STANDARD-OUTPUT*))

(defun print-float (float stream)
  ;; http://www.common-lisp.net/paste/display/743
  (let ((*STANDARD-OUTPUT* stream)
	(standard-output #'write-char-to-*standard-output*))
    (prin1 float)))

(defun print-array (array stream)
  (let ((dims (ARRAY-DIMENSIONS array)))
    (WRITE-CHAR (ch 35) stream)
    (print-integer (LENGTH dims) stream)
    (WRITE-CHAR (ch 65) stream)
    (if (zerop (LENGTH dims))
	(WRITE-CHAR (ch 48) stream)
	(print-array-elts array stream dims '()))))

(defun print-array-elts (array stream dims indices)
  (if (null dims)
      (PRIN1 (apply #'AREF array indices))
      (progn
	(WRITE-CHAR (ch 40) stream)
	(dotimes (i (first dims))
	  (when (> i 0)
	    (WRITE-STRING " " stream))
	  (print-array-elts array stream (rest dims)
			    (append indices (list i))))
	(WRITE-CHAR (ch 41) stream))))

(defun PRIN1 (object &optional stream)
  (WRITE object (kw STREAM) stream (kw ESCAPE) t))

(defun PRINT (object &optional stream)
  (TERPRI stream)
  (PRIN1 object stream)
  (WRITE-CHAR (ch 32) stream)
  object)

(cl:defun PPRINT (object &OPTIONAL stream)
  (TERPRI stream)
  (WRITE object (kw STREAM) stream (kw ESCAPE) t (kw PRETTY) t)
  (cl:values))

(cl:defun PRINC (object &OPTIONAL stream)
  (WRITE object (kw STREAM) stream (kw ESCAPE) nil (kw READABLY) nil))

(cl:defun WRITE-TO-STRING (object &REST keys)
  (WITH-OUTPUT-TO-STRING (stream)
    (apply (cl:function WRITE) object (kw STREAM) stream keys)))

(cl:defun PRIN1-TO-STRING (object)
  (WITH-OUTPUT-TO-STRING (stream)
    (PRIN1 object stream)))

(cl:defun PRINC-TO-STRING (object)
  (WITH-OUTPUT-TO-STRING (stream)
    (PRINC object stream)))

(DEFVAR *PRINT-ARRAY* T)

(DEFVAR *PRINT-BASE* 10)

(DEFVAR *PRINT-RADIX* nil)

(DEFVAR *PRINT-CASE* (kw UPCASE))

(DEFVAR *PRINT-CIRCLE* nil)

(DEFVAR *PRINT-ESCAPE* nil)

(DEFVAR *PRINT-GENSYM* T)

(DEFVAR *PRINT-LEVEL* nil)

(DEFVAR *PRINT-LENGTH* nil)

(DEFVAR *PRINT-LINES* nil)

(DEFVAR *PRINT-MISER-WIDTH* nil)

(defvar *initial-pprint-dispatch*
  (let ((table (make-hash-table :test #'equal)))
    (SET-PPRINT-DISPATCH '(CONS (EQL QUOTE) (CONS T NULL))
			 (lambda (stream object)
			   (WRITE-CHAR (ch 39) stream)
			   (write-object (second object) stream))
			 100 table)
    (SET-PPRINT-DISPATCH '(CONS (EQL FUNCTION) (CONS T NULL))
			 (lambda (stream object)
			   (WRITE-STRING "#'" stream)
			   (write-object (second object) stream))
			 100 table)
    (SET-PPRINT-DISPATCH '(CONS (EQL LAMBDA) CONS)
			 (lambda (stream object)
			   (WRITE-STRING "(LAMBDA " stream)
			   (if (null (second object))
			       (WRITE-STRING "()" stream)
			       (write-object (second object) stream))
			   (dolist (x (cddr object))
			     (WRITE-CHAR (ch 32) stream)
			     (write-object x stream))
			   (WRITE-CHAR (ch 41) stream))
			 100 table)
    (SET-PPRINT-DISPATCH '(CONS (EQL DEFUN) (CONS T (CONS LIST LIST)))
			 (lambda (stream object)
			   (WRITE-STRING "(DEFUN " stream)
			   (write-object (second object) stream)
			   (WRITE-CHAR (ch 32) stream)
			   (if (null (third object))
			       (WRITE-STRING "()" stream)
			       (write-object (third object) stream))
			   (dolist (x (cdddr object))
			     (FORMAT stream "~%  ")
			     (write-object x stream))
			   (WRITE-CHAR (ch 41) stream))
			 100 table)
    (SET-PPRINT-DISPATCH '(CONS (EQL BACKQUOTE) (CONS T NULL))
			 (lambda (stream object)
			   (WRITE-STRING "`" stream)
			   (write-object (second object) stream))
			 100 table)
    (SET-PPRINT-DISPATCH '(CONS (EQL COMMA) (CONS T NULL))
			 (lambda (stream object)
			   (WRITE-STRING "," stream)
			   (let ((obj (second object)))
			     (when (and (symbolp obj)
					(find (aref (symbol-name obj) 0) ".@"))
			       (WRITE-CHAR (ch 32) stream))
			     (write-object obj stream)))
			 100 table)
    (SET-PPRINT-DISPATCH '(CONS (EQL COMMA-AT) (CONS T NULL))
			 (lambda (stream object)
			   (WRITE-STRING ",@" stream)
			   (write-object (second object) stream))
			 100 table)
    table))

(DEFVAR *PRINT-PPRINT-DISPATCH* *initial-pprint-dispatch*)

(DEFVAR *PRINT-PRETTY* nil)

(DEFVAR *PRINT-READABLY* nil)

(DEFVAR *PRINT-RIGHT-MARGIN* nil)

;;; PRINT-NOT-READABLE and PRINT-NOT-READABLE-OBJECT defined in
;;; cl-conditions.el.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-printer.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-sequences.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 17, Sequences.

(IN-PACKAGE "EMACS-CL")

;;; System Class SEQUENCE

(defun COPY-SEQ (sequence)
  (cond
    ((listp sequence)
     (copy-list sequence))
    ((or (stringp sequence) (bit-vector-p sequence) (SIMPLE-VECTOR-P sequence))
     (copy-sequence sequence))
    ((vector-and-typep sequence 'VECTOR)
     (let ((storage (vector-storage sequence))
	   (vector (make-vector
		    (1+ (or (FILL-POINTER sequence) (LENGTH sequence)))
		    'SIMPLE-VECTOR)))
       (do ((i 1 (1+ i)))
	   ((= i (length vector)))
	 (aset vector i (aref storage (1- i))))
       vector))
    ((VECTORP sequence)
     (subseq (vector-storage sequence) 0 (FILL-POINTER sequence)))
    (t
     (type-error sequence 'SEQUENCE))))

(defun ELT (sequence index)
  (cond
    ((listp sequence)
     (nth index sequence))
    ((VECTORP sequence)
     (if (ARRAY-HAS-FILL-POINTER-P sequence)
	 (if (cl:< index (FILL-POINTER sequence))
	     (AREF sequence index)
	     (error "error"))
	 (AREF sequence index)))
    (t
     (type-error sequence 'SEQUENCE))))

(defsetf ELT (sequence index) (obj)
  `(if (listp ,sequence)
       (setf (nth ,index ,sequence) ,obj)
       (setf (AREF ,sequence ,index) ,obj)))

(DEFSETF ELT (sequence index) (obj)
  `(IF (LISTP ,sequence)
       (SETF (NTH ,index ,sequence) ,obj)
       (SETF (AREF ,sequence ,index) ,obj)))

(cl:defun FILL (seq obj &KEY (START 0) END)
  ;; TODO: use fillarray when possible
  (unless END
    (setq END (LENGTH seq)))
  (do ((i START (1+ i)))
      ((eq i END))
    (setf (ELT seq i) obj))
  seq)

(cl:defun MAKE-SEQUENCE (type size &KEY INITIAL-ELEMENT)
  ;; TODO: An error of type type-error should be signaled if the
  ;; result type specifies the number of elements and size is
  ;; different from that number.
  (subtypecase type
    (nil
     (ERROR "Can't make sequence of type nil."))
    (LIST
     (make-list size INITIAL-ELEMENT))
    (BIT-VECTOR
     (make-bit-vector size (or INITIAL-ELEMENT 0)))
    (STRING
     (make-string size (if INITIAL-ELEMENT (CHAR-CODE INITIAL-ELEMENT) 0)))
    (VECTOR
     (make-simple-vector size INITIAL-ELEMENT))
    (T
     (type-error type '(MEMBER LIST BIT-VECTOR STRING VECTOR)))))

(defun SUBSEQ (seq start &optional end)
  (unless end
    (setq end (LENGTH seq)))
  (cond
    ((SIMPLE-STRING-P seq)
     (substring seq start end))
    ((STRINGP seq)
     (substring (vector-storage seq) start end))
    ((listp seq)
     (if (eq start end)
	 nil
	 (let* ((new (copy-list (nthcdr start seq)))
		(last (nthcdr (- end start 1) new)))
	   (when last
	     (setcdr last nil))
	   new)))
    ((VECTORP seq)
     (let ((len (- end start))
	   (i0 0))
       (when (eq (aref seq 0) 'SIMPLE-VECTOR)
	 (incf i0)
	 (incf len)
	 (incf start))
       (let ((new (if (BIT-VECTOR-P seq)
		      (make-bit-vector len 0)
		      (make-vector len 'SIMPLE-VECTOR)))
	     (storage (if (or (bit-vector-p seq)
			      (stringp seq)
			      (SIMPLE-VECTOR-P seq))
			  seq
			  (vector-storage seq))))
	 (do ((i i0 (1+ i))
	      (j start (1+ j)))
	     ((eq i len))
	   (aset new i (aref storage j)))
	 new)))
    (t
     (type-error seq 'SEQUENCE))))

(DEFSETF SUBSEQ (seq1 start &optional end) (seq2)
  `(PROGN
     (REPLACE ,seq1 ,seq2 ,(kw START1) ,start ,(kw END1) ,end)
     ,seq2))

(defun* MAP (type fn &rest sequences)
  (let ((len (apply #'min (mapcar #'LENGTH sequences)))
	(i 0)
	(result nil))
    (loop
      (when (eq i len)
	(return-from MAP
	  (when type
	    (setq result (nreverse result))
	    (subtypecase type
	      (LIST	
	       result)
	      (STRING
	       (if (null result)
		   ""
		   (apply #'string (mapcar #'CHAR-CODE result))))
	      (BIT-VECTOR
	       (apply #'make-bit-vector result))
	      (VECTOR
	       (apply #'vector 'SIMPLE-VECTOR result))
	      (SEQUENCE
	       result)
	      (T
	       (ERROR "Type specifier ~S is not a subtype of SEQUENCE."
		      type))))))
      (let ((item (APPLY fn (mapcar (lambda (seq) (ELT seq i)) sequences))))
	(when type
	  (push item result)))
      (incf i))))

(defun* MAP-INTO (result fn &rest sequences)
  (let ((len (apply #'min (mapcar #'LENGTH (cons result sequences))))
	(i 0))
    (loop
      (when (eq i len)
	(return-from MAP-INTO result))
      (setf (ELT result i)
	    (APPLY fn (mapcar (lambda (seq) (ELT seq i)) sequences)))
      (incf i))))

(cl:defun REDUCE (fn seq &KEY KEY FROM-END (START 0) END
			      (INITIAL-VALUE nil initial-value-p))
  (unless KEY
    (setq KEY (cl:function IDENTITY)))
  (unless END
    (setq END (LENGTH seq)))
  (let ((len (- END START))
	result)
    (cond
      ((and (eq len 1) (not initial-value-p))
       (ELT seq START))
      ((zerop len)
       (if initial-value-p
	   INITIAL-VALUE
	   (FUNCALL fn)))
      (t
       (if FROM-END
	   (progn
	     (when initial-value-p
	       (decf END)
	       (setq result (FUNCALL fn INITIAL-VALUE
				     (FUNCALL KEY (ELT seq END)))))
	     (do ((i (1- END) (1- i)))
		 ((< i START))
	       (setq result (FUNCALL fn (FUNCALL KEY (ELT seq i)) result))))
	   (progn
	     (when initial-value-p
	       (setq result (FUNCALL fn INITIAL-VALUE
				     (FUNCALL KEY (ELT seq START))))
	       (incf START))
	     (do ((i START (1+ i)))
		 ((>= i END))
	       (setq result (FUNCALL fn result (FUNCALL KEY (ELT seq i)))))))
       result))))

(cl:defun COUNT (obj seq &KEY FROM-END TEST TEST-NOT (START 0) END KEY)
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST (cl:function EQL)))
  (COUNT-IF (lambda (x) (FUNCALL TEST obj x)) seq
	    (kw FROM-END) FROM-END (kw START) START
	    (kw END) END (kw KEY) KEY))

(cl:defun COUNT-IF (predicate seq &KEY FROM-END (START 0) END KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (unless END
    (setq END (LENGTH seq)))
  (let ((n 0))
    (if FROM-END
	(do ((i (1- END) (1- i)))
	    ((eq i (1- START)))
	  (let ((elt (ELT seq i)))
	    (when (FUNCALL predicate (FUNCALL KEY elt))
	      (incf n))))
	(do ((i START (1+ i)))
	    ((eq i END))
	  (let ((elt (ELT seq i)))
	    (when (FUNCALL predicate (FUNCALL KEY elt))
	      (incf n)))))
    n))

(cl:defun COUNT-IF-NOT (predicate &REST args)
  (apply (cl:function COUNT-IF) (COMPLEMENT predicate) args))

(defun LENGTH (sequence)
  (cond
    ((or (listp sequence)
	 (bit-vector-p sequence)
	 (stringp sequence))
     (length sequence))
    ((SIMPLE-VECTOR-P sequence)
     (1- (length sequence)))
    ((VECTORP sequence)
     (or (and (ARRAY-HAS-FILL-POINTER-P sequence)
	      (FILL-POINTER sequence))
	 (vector-size sequence)))
    (t
     (type-error sequence 'SEQUENCE))))

(defun REVERSE (seq)
  (cond
   ((listp seq)
    (reverse seq))
   ((VECTORP seq)
    (NREVERSE (COPY-SEQ seq)))
   (t
    (type-error seq 'SEQUENCE))))

(defun NREVERSE (seq)
  (cond
    ((listp seq)
     (nreverse seq))
    ((VECTORP seq)
     (do* ((len (LENGTH seq))
	   (end (/ len 2))
	   (i 0 (1+ i))
	   (j (1- len) (1- j)))
	  ((eq i end)
	   seq)
       (rotatef (AREF seq i) (AREF seq j))))
    (t
     (type-error seq 'SEQUENCE))))

(cl:defun SORT (sequence predicate &KEY KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (cond 
    ((listp sequence)
     (sort sequence (lambda (x y)
		      (FUNCALL predicate (FUNCALL KEY x) (FUNCALL KEY y)))))
    ((VECTORP sequence)
     (MAP-INTO sequence
	       #'IDENTITY
	       (SORT (MAP 'LIST #'IDENTITY sequence) predicate (kw KEY) KEY)))
    (t
     (type-error sequence 'SEQUENCE))))

(fset 'STABLE-SORT (symbol-function 'SORT))

(cl:defun FIND (obj seq &KEY FROM-END TEST TEST-NOT (START 0) END KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST (cl:function EQL)))
  (FIND-IF (lambda (x) (FUNCALL TEST obj x)) seq
	   (kw FROM-END) FROM-END (kw START) START
	   (kw END) END (kw KEY) KEY))

(cl:defun FIND-IF (predicate seq &KEY FROM-END (START 0) END KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (let ((len (LENGTH seq)))
    (unless END
      (setq END len))
    (catch 'FIND
      (if FROM-END
	  (do ((i (1- END) (1- i)))
	      ((eq i -1))
	    (let ((elt (ELT seq i)))
	      (when (FUNCALL predicate (FUNCALL KEY elt))
		(throw 'FIND elt))))
	  (do ((i START (1+ i)))
	      ((eq i END))
	    (let ((elt (ELT seq i)))
	      (when (FUNCALL predicate (FUNCALL KEY elt))
		(throw 'FIND elt)))))
      nil)))

(cl:defun FIND-IF-NOT (predicate &REST args)
  (apply (cl:function FIND-IF) (COMPLEMENT predicate) args))

(cl:defun POSITION (obj seq &KEY FROM-END TEST TEST-NOT (START 0) END KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST (cl:function EQL)))
  (POSITION-IF (lambda (x) (FUNCALL TEST obj x)) seq
	       (kw FROM-END) FROM-END (kw START) START
	       (kw END) END (kw KEY) KEY))

(cl:defun POSITION-IF (predicate seq &KEY FROM-END (START 0) END KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (let ((len (LENGTH seq)))
    (unless END
      (setq END len))
    (catch 'POSITION
      (if FROM-END
	  (do ((i (1- END) (1- i)))
	      ((eq i -1))
	    (when (FUNCALL predicate (FUNCALL KEY (ELT seq i)))
	      (throw 'POSITION i)))
	  (do ((i START (1+ i)))
	      ((eq i END))
	    (when (FUNCALL predicate (FUNCALL KEY (ELT seq i)))
	      (throw 'POSITION i))))
      nil)))

(cl:defun POSITION-IF-NOT (predicate &REST args)
  (apply (cl:function POSITION-IF) (COMPLEMENT predicate) args))

(defun subseq-p (seq1 start1 end1 seq2 start2 end2 TEST KEY)
  (catch 'subseq-p
    (do ((i start1 (1+ i))
	 (j start2 (1+ j)))
	((or (eq i end1) (eq j end2))
	 (eq i end1))
      (unless (FUNCALL TEST (FUNCALL KEY (ELT seq1 i))
		            (FUNCALL KEY (ELT seq2 j)))
	(throw 'subseq-p nil)))))

(cl:defun SEARCH (seq1 seq2 &KEY FROM-END TEST TEST-NOT KEY
		                 (START1 0) (START2 0) END1 END2)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (unless END1
    (setq END1 (LENGTH seq1)))
  (unless END2
    (setq END2 (LENGTH seq2)))
  (catch 'SEARCH
    (if FROM-END
	(do ((i (1- END2) (1- i)))
	    ((minusp i))
	  (when (subseq-p seq1 START1 END1 seq2 i END2 TEST KEY)
	    (throw 'SEARCH i)))
	(do ((i START2 (1+ i)))
	    ((eq i END2))
	  (when (subseq-p seq1 START1 END1 seq2 i END2 TEST KEY)
	    (throw 'SEARCH (+ i (- END1 START1) -1)))))
    nil))

(cl:defun MISMATCH (seq1 seq2 &KEY FROM-END TEST TEST-NOT KEY
				   (START1 0) (START2 0) END1 END2)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (unless END1
    (setq END1 (LENGTH seq1)))
  (unless END2
    (setq END2 (LENGTH seq2)))
  (catch 'MISMATCH
    (if FROM-END
	(do ((i (1- END1) (1- i))
	     (j (1- END2) (1- j)))
	    ((or (< i START1) (< j START2))
	     (unless (and (< i START1) (< j START2)) i))
	  (unless (FUNCALL TEST (FUNCALL KEY (ELT seq1 i))
				(FUNCALL KEY (ELT seq2 j)))
	    (throw 'MISMATCH i)))
	(do ((i START1 (1+ i))
	     (j START2 (1+ j)))
	    ((or (eq i END1) (eq j END2))
	     (unless (and (eq i END1) (eq j END2)) i))
	  (unless (FUNCALL TEST (FUNCALL KEY (ELT seq1 i))
				(FUNCALL KEY (ELT seq2 j)))
	    (throw 'MISMATCH i))))))

(cl:defun REPLACE (seq1 seq2 &KEY (START1 0) (START2 0) END1 END2)
  (unless END1
    (setq END1 (LENGTH seq1)))
  (unless END2
    (setq END2 (LENGTH seq2)))
  (do ((i START1 (1+ i))
       (j START2 (1+ j)))
      ((or (eq i END1) (eq j END2)))
    (setf (ELT seq1 i) (ELT seq2 j)))
  seq1)

(cl:defun NSUBSTITUTE (new old seq &KEY FROM-END TEST TEST-NOT (START 0) END
					COUNT KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (ERROR 'ERROR))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (NSUBSTITUTE-IF new (lambda (x) (FUNCALL TEST old x)) seq
		  (kw FROM-END) FROM-END (kw START) START
		  (kw END) END (kw COUNT) COUNT (kw KEY) KEY))

(cl:defun NSUBSTITUTE-IF (obj predicate seq &KEY FROM-END (START 0) END COUNT
						 KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (unless END
    (setq END (LENGTH seq)))
  (if FROM-END
      (do ((i (1- END) (1- i)))
	  ((or (minusp i) (and COUNT (<= COUNT 0))))
	(when (FUNCALL predicate (FUNCALL KEY (ELT seq i)))
	  (setf (ELT seq i) obj))
	(when COUNT (decf COUNT)))
      (do ((i START (1+ i)))
	  ((or (eq i END) (and COUNT (<= COUNT 0))))
	(when (FUNCALL predicate (FUNCALL KEY (ELT seq i)))
	  (setf (ELT seq i) obj))
	(when COUNT (decf COUNT))))
  seq)

(cl:defun NSUBSTITUTE-IF-NOT (predicate &REST args)
  (apply (cl:function NSUBSTITUTE-IF) (COMPLEMENT predicate) args))

(cl:defun SUBSTITUTE (new old seq &REST keys)
  (apply (cl:function NSUBSTITUTE) new old (COPY-SEQ seq) keys))

(cl:defun SUBSTITUTE-IF (obj predicate seq &REST keys)
  (apply (cl:function NSUBSTITUTE-IF) obj predicate (COPY-SEQ seq) keys))

(cl:defun SUBSTITUTE-IF-NOT (obj predicate seq &REST keys)
  (apply (cl:function NSUBSTITUTE-IF)
	 obj (COMPLEMENT predicate) (COPY-SEQ seq) keys))

(defun CONCATENATE (type &rest sequences)
  (subtypecase type
    (nil
     (ERROR "Can't concatenate to type nil."))
    (LIST
     (let ((result nil))
       (dolist (seq sequences (nreverse result))
	 (dosequence (x seq)
           (push x result)))))
    (STRING
     (let ((string (make-string (reduce #'+ (mapcar #'LENGTH sequences)) 0))
	   (i -1))
       (dolist (seq sequences)
	 (dosequence (x seq)
	   (aset string (incf i) (CHAR-CODE x))))
       string))
    (BIT-VECTOR
     (let ((vector (vector (reduce #'+ (mapcar #'LENGTH sequences)) 0))
	   (i -1))
       (dolist (seq sequences)
	 (dosequence (x seq)
	   (setf (bref vector (incf i)) x)))
       vector))
    (VECTOR
     (let ((vector (make-vector (1+ (reduce #'+ (mapcar #'LENGTH sequences)))
				'SIMPLE-VECTOR))
	   (i 0))
       (dolist (seq sequences)
	 (dosequence (x seq)
	   (aset vector (incf i) x)))
       vector))
    (T
     (ERROR "~S is not a recognizable subtype of LIST or VECTOR." type))))

(cl:defun MERGE (type seq1 seq2 predicate &KEY KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (let* ((len1 (LENGTH seq1))
	 (len2 (LENGTH seq2))
	 (len (+ len1 len2))
	 (result (subtypecase type
		   (nil		(ERROR "Can't merge to type nil."))
		   (LIST	(make-list len nil))
		   (STRING	(make-string len 0))
		   (BIT-VECTOR	(make-bit-vector len 0))
		   (VECTOR	(make-vector (1+ len) 'SIMPLE-VECTOR))
		   (T		(type-error type '(MEMBER LIST VECTOR))))))
    (do ((i 0 (1+ i))
	 (j 0)
	 (k 0))
	((= i len))
      (setf (ELT result i)
	    (if (or (eq j len1)
		    (and (not (eq k len2))
			 (FUNCALL predicate (FUNCALL KEY (ELT seq2 k))
				            (FUNCALL KEY (ELT seq1 j)))))
		(prog1 (ELT seq2 k) (incf k))
		(prog1 (ELT seq1 j) (incf j)))))
    result))

(cl:defun REMOVE (obj seq &KEY FROM-END TEST TEST-NOT (START 0) END COUNT KEY)
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (REMOVE-IF (lambda (x) (FUNCALL TEST obj x)) seq (kw FROM-END) FROM-END
	     (kw START) START (kw END) END (kw COUNT) COUNT (kw KEY) KEY))

(defun list-remove (predicate list end count key)
  (cond
    ((or (null list) (zerop end) (when count (zerop count)))
     nil)
    ((FUNCALL predicate (FUNCALL key (first list)))
     (list-remove predicate (rest list) (1- end) (when count (1- count)) key))
    (t
     (cons (first list)
	   (list-remove predicate (rest list) (1- end) count key)))))

(cl:defun REMOVE-IF (predicate seq &KEY FROM-END (START 0) END COUNT KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (cond
    ((listp seq)
     (if FROM-END
	 (ERROR "REMOVE-IF doesn't implement :FROM-END T.")
	 (list-remove predicate (nthcdr START seq) (or END (LENGTH seq))
		      COUNT KEY)))
    ((VECTORP seq)
     (ERROR "REMOVE-IF not implemented for vectors."))
    (t
     (type-error seq 'SEQUENCE))))

(cl:defun REMOVE-IF-NOT (predicate &REST args)
  ;;(apply (cl:function REMOVE-IF) (COMPLEMENT predicate) args))
  (apply #'REMOVE-IF (COMPLEMENT predicate) args))

(cl:defun DELETE (obj seq &KEY FROM-END TEST TEST-NOT (START 0) END COUNT KEY)
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (DELETE-IF (lambda (x) (FUNCALL TEST obj x)) seq (kw FROM-END) FROM-END
	     (kw START) START (kw END) END (kw COUNT) COUNT (kw KEY) KEY))

(defun list-delete (predicate list end count key)
  (cond
    ((or (null list) (zerop end) (when count (zerop count)))
     nil)
    ((FUNCALL predicate (FUNCALL key (first list)))
     (let ((rest (list-delete predicate (rest list) (1- end)
			      (when count (1- count)) key)))
       (if (null rest)
	   nil
	 (RPLACA list (car rest))
	 (RPLACD list (cdr rest)))))
    (t
     (RPLACD list (list-delete predicate (rest list) (1- end) count key)))))

(cl:defun DELETE-IF (predicate seq &KEY FROM-END (START 0) END COUNT KEY)
  (cond
    ((listp seq)
     (progn
       (unless KEY
	 (setq KEY #'IDENTITY))
       (if FROM-END
	   (REMOVE-IF predicate seq (kw FROM-END) FROM-END (kw KEY) KEY
		      (kw START) START (kw END) END (kw COUNT) COUNT)
	   (list-delete predicate (nthcdr START seq) (or END (LENGTH seq))
			COUNT KEY))))
    ((VECTORP seq)
     (if (ARRAY-HAS-FILL-POINTER-P seq)
	 (ERROR "DELETE-IF not implemented for vectors with fill pointers.")
	 (REMOVE-IF predicate seq (kw FROM-END) FROM-END (kw KEY) KEY
		    (kw START) START (kw END) END (kw COUNT) COUNT)))
    (t
     (type-error seq 'SEQUENCE))))

(cl:defun DELETE-IF-NOT (predicate &REST args)
  (apply (cl:function DELETE-IF) (COMPLEMENT predicate) args))

(cl:defun REMOVE-DUPLICATES (seq &KEY FROM-END TEST TEST-NOT (START 0) END KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (let ((i START))
    (REMOVE-IF (lambda (x)
		 (POSITION x seq (kw TEST) TEST (kw KEY) KEY
				 (kw START) (incf i) (kw FROM-END) FROM-END))
	       seq (kw KEY) KEY (kw START) START (kw END) END)))

(cl:defun DELETE-DUPLICATES (seq &KEY FROM-END TEST TEST-NOT (START 0) END KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (let ((i START))
    (DELETE-IF (lambda (x)
		 (POSITION x seq (kw TEST) TEST (kw KEY) KEY
				 (kw START) (incf i) (kw FROM-END) FROM-END))
	       seq (kw KEY) KEY (kw START) START (kw END) END)))



(defmacro* dovector ((var vector &optional result) &body body)
  (with-gensyms (i len vec)
    `(let* (,var (,i 0) (,vec ,vector) (,len (LENGTH ,vec)))
       (while (< ,i ,len)
	 (setq ,var (AREF ,vec ,i))
	 ,@body
	 (incf ,i))
       ,result)))

(defmacro* dosequence ((var sequence &optional result) &body body)
  (let ((seq (gensym)))
    `(let ((,seq ,sequence))
       (if (listp ,seq)
	   (dolist (,var ,seq ,result) ,@body)
	   (dovector (,var ,seq ,result) ,@body)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-sequences.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-arrays.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 15, Arrays.

(IN-PACKAGE "EMACS-CL")

;;; System Class	ARRAY
;;; Type		SIMPLE-ARRAY
;;; System Class	VECTOR
;;; Type		SIMPLE-VECTOR
;;; System Class	BIT-VECTOR
;;; Type		SIMPLE-BIT-VECTOR

(define-storage-layout array (dims storage offset))

(define-storage-layout vector (size storage offset fp))

(defun set-initial-contents (n i storage contents fn)
  (cond
    ((zerop n)	(aset storage 0 (funcall fn contents)))
    ((eq n 1)	(dosequence (x contents i)
		  (aset storage i (funcall fn x))
		  (incf i)))
    (t		(dosequence (x contents i)
		  (setq i (set-initial-contents (1- n) i storage x fn))))))

(defun bit-bool (bit)
  (ecase bit
    (0 nil)
    (1 t)))

(if (fboundp 'make-bit-vector)
    (progn
      (defmacro bref (vector index)
	`(aref ,vector ,index))
      (defsetf bref (vector index) (bit)
	`(aset ,vector ,index ,bit)))
    (progn
      (defun make-bit-vector (length bit)
	(make-bool-vector length (bit-bool bit)))
      (defmacro bit-vector-p (object)
	`(bool-vector-p ,object))
      (defmacro bref (vector index)
	`(if (aref ,vector ,index) 1 0))
      (defsetf bref (vector index) (bit)
	`(aset ,vector ,index (bit-bool ,bit)))))

(defun make-simple-vector (size initial-element)
  (let ((vector (make-vector (1+ size) initial-element)))
    (aset vector 0 'SIMPLE-VECTOR)
    vector))

(cl:defun MAKE-ARRAY (dimensions &KEY (ELEMENT-TYPE T) INITIAL-ELEMENT
		      INITIAL-CONTENTS ADJUSTABLE FILL-POINTER
		      DISPLACED-TO DISPLACED-INDEX-OFFSET)
  (setq dimensions (ensure-list dimensions))
  (when (eq FILL-POINTER T)
    (setq FILL-POINTER (just-one dimensions)))
  (let* ((size (apply #'cl:* dimensions))
	 (start-index 0)
	 (ndims (length dimensions))
	 (vectorp (eq ndims 1))
	 (simplep (not (or ADJUSTABLE
			   FILL-POINTER
			   DISPLACED-TO
			   (not vectorp)))))
    (multiple-value-bind (initial-element make-fn convert-fn
			  vector-type array-type)
	(ecase (UPGRADED-ARRAY-ELEMENT-TYPE ELEMENT-TYPE)
	  (BIT
	   (values (or INITIAL-ELEMENT 0)
		   ;; TODO: clean up
		   (if (fboundp 'make-bool-vector)
				#'make-bool-vector
				#'make-bit-vector)
		   (if (fboundp 'make-bool-vector) #'bit-bool #'IDENTITY)
		   'BIT-VECTOR
		   'bit-array))
	  (CHARACTER
	   (values (or INITIAL-ELEMENT (ch 0))
		   #'make-string
		   (if use-character-type-p #'IDENTITY #'CHAR-CODE)
		   'STRING
		   'char-array))
	  ((T)
	   (when simplep
	     (setq start-index 1))
	   (values INITIAL-ELEMENT
		   (if simplep #'make-simple-vector #'make-vector)
		   #'IDENTITY
		   'VECTOR
		   'ARRAY)))
      (let ((storage (or DISPLACED-TO
			 (funcall make-fn size
				  (funcall convert-fn initial-element)))))
	(when INITIAL-CONTENTS
	  (set-initial-contents
	   ndims start-index storage INITIAL-CONTENTS convert-fn))
	(cond
	  (simplep	storage)
	  (vectorp	(vector vector-type (just-one dimensions) storage
				DISPLACED-INDEX-OFFSET FILL-POINTER))
	  (t		(vector array-type dimensions storage
				DISPLACED-INDEX-OFFSET)))))))


(defun cl-vector (vector)
  (unless (vectorp vector)
    (type-error vector 'vector))
  (vector 'VECTOR (length vector) vector 0 nil))

(defun el-vector (vector)
  (unless (VECTORP vector)
    (type-error vector 'VECTOR))
  (if (and (eq (aref vector 0) 'VECTOR)
	   (eq (vector-size vector) (length (vector-storage vector)))
	   (zerop (vector-offset vector)))
      (vector-storage vector)
      (let* ((size (LENGTH vector))
	     (new (make-vector size nil)))
	(dotimes (i size)
	  (aset new i (AREF vector i)))
	new)))

(defun el-string (string)
  (unless (STRINGP string)
    (type-error string 'STRING))
  (cond
    ((stringp string)
     string)
    ((and (eq (vector-size string) (length (vector-storage string)))
	  (zerop (vector-offset string)))
     (vector-storage string))
    (t
     (let* ((size (LENGTH string))
	    (new (make-string size (ch 0))))
       (dotimes (i size)
	 (aset new i (CHAR string i)))
       new))))

(if (eval-when-compile (featurep 'xemacs))
    (defun el-bit-vector (bit-vector)
      (unless (BIT-VECTOR-P bit-vector)
	(type-error bit-vector 'BIT-VECTOR))
      (cond
	((bit-vector-p bit-vector)
	 bit-vector)
	((and (eq (vector-size bit-vector)
		  (length (vector-storage bit-vector)))
	      (zerop (vector-offset bit-vector)))
	 (vector-storage bit-vector))
	(t
	 (let* ((size (LENGTH bit-vector))
		(new (make-bit-vector size 0)))
	   (dotimes (i size)
	     (aset new i (BIT bit-vector i)))
	   new))))
    (defun el-bool-vector (bool-vector)
      (unless (BIT-VECTOR-P bool-vector)
	(type-error bool-vector 'BIT-VECTOR))
      (cond
	((bool-vectorp bool-vector)
	 bool-vector)
	((and (eq (vector-size bool-vector)
		  (length (vector-storage bool-vector)))
	      (zerop (vector-offset bool-vector)))
	 (vector-storage bool-vector))
	(t
	 (let* ((size (LENGTH bool-vector))
		(new (make-bool-vector size 0)))
	   (dotimes (i size)
	     (aset new i (not (zerop (BIT bool-vector i)))))
	   new)))))

(cl:defun ADJUST-ARRAY (array new-dimensions
			&KEY ELEMENT-TYPE INITIAL-ELEMENT INITIAL-CONTENTS
			     FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET)
  (if (ADJUSTABLE-ARRAY-P array)
      (error "TODO")
      (MAKE-ARRAY new-dimensions
		  (kw ELEMENT-TYPE) ELEMENT-TYPE
		  (kw INITIAL-ELEMENT) INITIAL-ELEMENT
		  (kw INITIAL-CONTENTS) INITIAL-CONTENTS
		  (kw FILL-POINTER) FILL-POINTER
		  (kw DISPLACED-TO) DISPLACED-TO
		  (kw DISPLACED-INDEX-OFFSET) DISPLACED-INDEX-OFFSET)))

(defun ADJUSTABLE-ARRAY-P (array)
  (and (vectorp array)
       (case (aref array 0)
	 ((BIT-VECTOR bit-array STRING char-array VECTOR ARRAY) T))))

(defun AREF (array &rest subscripts)
  (cond
    ((BIT-VECTOR-P array)
     (BIT array (just-one subscripts)))
    ((STRINGP array)
     (CHAR array (just-one subscripts)))
    ((vector-and-typep array 'SIMPLE-VECTOR)
     (SVREF array (just-one subscripts)))
    ((vector-and-typep array 'VECTOR)
     (aref (array-storage array) (just-one subscripts)))
    ((vector-and-typep array 'ARRAY)
     (aref (array-storage array) (apply #'ARRAY-ROW-MAJOR-INDEX array subscripts)))
    ((vector-and-typep array 'bit-array)
     (BIT (array-storage array) (apply #'ARRAY-ROW-MAJOR-INDEX array subscripts)))
    ((vector-and-typep array 'char-array)
     (CHAR (array-storage array) (apply #'ARRAY-ROW-MAJOR-INDEX array subscripts)))
    (t
     (type-error array 'ARRAY))))

(defsetf AREF (array &rest subscripts) (obj)
  `(ASET ,obj ,array ,@subscripts))

(DEFINE-SETF-EXPANDER AREF (array &rest subscripts)
  (let ((obj (gensym))
	(atemp (gensym))
	(stemps (map-to-gensyms subscripts)))
    (cl:values (cons atemp stemps)
	       (cons array subscripts)
	       (list obj)
	       `(ASET ,obj ,atemp ,@stemps)
	       `(AREF ,atemp ,@stemps))))

(defun ASET (object array &rest subscripts)
  (cond
    ((BIT-VECTOR-P array)
     (setf (BIT array (just-one subscripts)) object))
    ((STRINGP array)
     (setf (CHAR array (just-one subscripts)) object))
    ((vector-and-typep array 'SIMPLE-VECTOR)
     (setf (SVREF array (just-one subscripts)) object))
    ((vector-and-typep array 'VECTOR)
     (aset (array-storage array) (just-one subscripts) object))
    ((vector-and-typep array 'ARRAY)
     (aset (array-storage array)
	   (apply #'ARRAY-ROW-MAJOR-INDEX array subscripts)
	   object))
    ((vector-and-typep array 'bit-array)
     (aset (array-storage array)
	   (apply #'ARRAY-ROW-MAJOR-INDEX array subscripts)
	   (bit-bool object)))
    ((vector-and-typep array 'char-array)
     (aset (array-storage array)
	   (apply #'ARRAY-ROW-MAJOR-INDEX array subscripts)
	   (CHAR-CODE object)))
    (t
     (type-error array 'ARRAY))))

(defun ARRAY-DIMENSION (array axis)
  (let ((dims (ARRAY-DIMENSIONS array)))
    (if (< axis (length dims))
	(nth axis dims)
	(ERROR "ARRAY-DIMENSION axis out of range."))))

(defun ARRAY-DIMENSIONS (array)
  (cond
    ((or (stringp array)
	 (bit-vector-p array))	(list (length array)))
    ((SIMPLE-VECTOR-P array)	(list (1- (length array))))
    ((VECTORP array)		(list (vector-size array)))
    ((ARRAYP array)		(array-dims array))
    (t				(type-error array 'ARRAY))))

(defun ARRAY-ELEMENT-TYPE (array)
  (cond
    ((stringp array)			'CHARACTER)
    ((bit-vector-p array)		'BIT)
    ((vectorp array)
     (case (aref array 0)
       ((BIT-VECTOR bit-array)		'BIT)
       ((STRING char-array)		'CHARACTER)
       ((SIMPLE-VECTOR VECTOR ARRAY)	'T)
       (t				(type-error array 'ARRAY))))
    (t					(type-error array 'ARRAY))))

(defun ARRAY-HAS-FILL-POINTER-P (array)
  (unless (ARRAYP array)
    (type-error array 'ARRAY))
  (and (vectorp array)
       (memq (aref array 0) '(VECTOR STRING BIT-VECTOR))))

(defun ARRAY-DISPLACEMENT (array)
  (unless (ARRAYP array)
    (type-error array 'ARRAY))
  (if (or (bit-vector-p array)
	  (stringp array)
	  (eq (aref array 0) 'SIMPLE-VECTOR))
      (cl:values nil 0)
      (cl:values (array-storage array) (array-offset array))))

(defun ARRAY-IN-BOUNDS-P (array &rest subscripts)
  (unless (ARRAYP array)
    (type-error array 'ARRAY))
  (and (not (some #'MINUSP subscripts))
       (every #'cl:< subscripts (ARRAY-DIMENSIONS array))))

(defun ARRAY-RANK (array)
  (unless (ARRAYP array)
    (type-error array 'ARRAY))
  (length (ARRAY-DIMENSIONS array)))

(defun ARRAY-ROW-MAJOR-INDEX (array &rest subscripts)
  (unless (ARRAYP array)
    (type-error array 'ARRAY))
  (apply #'cl:+ (maplist (lambda (x y) (cl:* (car x) (apply #'cl:* (cdr y))))
			 subscripts (ARRAY-DIMENSIONS array))))

(defun ARRAY-TOTAL-SIZE (array)
  (unless (ARRAYP array)
    (type-error array 'ARRAY))
  (reduce #'* (ARRAY-DIMENSIONS array)))

(defun ARRAYP (object)
  (or (stringp object)
      (bit-vector-p object)
      (and (vectorp object)
	   (case (aref object 0)
	     ((BIT-VECTOR bit-array STRING char-array
	       SIMPLE-VECTOR VECTOR ARRAY) T)))))

(defun FILL-POINTER (vector)
  (unless (ARRAY-HAS-FILL-POINTER-P vector)
    (ERROR 'TYPE-ERROR))
  (vector-fp vector))

(defsetf FILL-POINTER (vector) (fill-pointer)
  `(setf (vector-fp ,vector) ,fill-pointer))

(DEFSETF FILL-POINTER (vector) (fill-pointer)
  `(aset ,vector 4 ,fill-pointer))

(defun ROW-MAJOR-AREF (array index)
  (cond
    ((VECTORP array)
     (AREF array index))
    ((ARRAYP array)
     (aref (array-storage array) index))
    (t
     (type-error array 'ARRAY))))

(defsetf ROW-MAJOR-AREF (array index) (new)
  `(cond
    ((VECTORP ,array)
     (setf (AREF ,array ,index) ,new))
    ((ARRAYP array)
     (aset (array-storage ,array) ,index ,new))
    (t
     (type-error array 'ARRAY))))

(defun UPGRADED-ARRAY-ELEMENT-TYPE (type &optional env)
  (case type
    ((nil)		nil)
    (BIT		'BIT)
    (CHARACTER		'CHARACTER)
    (T			'T)
    (t
     (subtypecase type
       (BIT		'BIT)
       (CHARACTER	'CHARACTER)
       (T		'T)))))

(DEFCONSTANT ARRAY-DIMENSION-LIMIT (/ MOST-POSITIVE-FIXNUM 10))
(DEFCONSTANT ARRAY-RANK-LIMIT 100) ;(/ MOST-POSITIVE-FIXNUM 10))
(DEFCONSTANT ARRAY-TOTAL-SIZE-LIMIT (/ MOST-POSITIVE-FIXNUM 10))

(defun SIMPLE-VECTOR-P (object)
  (vector-and-typep object 'SIMPLE-VECTOR))

(defun SVREF (vector index)
  (aref vector (1+ index)))

(defsetf SVREF (vector index) (object)
  `(setf (aref ,vector (1+ ,index)) ,object))

(DEFINE-SETF-EXPANDER SVREF (vector index)
  (let ((object (gensym))
	(vtemp (gensym))
	(itemp (gensym)))
    (cl:values (list vtemp itemp)
	       (list vector index)
	       (list object)
	       `(SET-SVREF ,object ,vtemp ,itemp)
	       `(SVREF ,vtemp ,itemp))))

(defun SET-SVREF (object vector index)
  (aset vector (1+ index) object))

(defun VECTOR (&rest objects)
  (let ((vector (make-simple-vector (length objects) nil))
	(i 0))
    (dolist (obj objects vector)
      (aset vector (incf i) obj))))

(defun VECTOR-POP (vector)
  (unless (and (VECTORP vector)
	       (ARRAY-HAS-FILL-POINTER-P vector)
	       (plusp (FILL-POINTER vector)))
    (error "error"))
  (aref (vector-storage vector) (decf (vector-fp vector))))

(defun VECTOR-PUSH (object vector)
  (unless (and (VECTORP vector) (ARRAY-HAS-FILL-POINTER-P vector))
    (error "error"))
  (let ((ptr (FILL-POINTER vector))
	(storage (vector-storage vector)))
    (unless (eq ptr (vector-size vector))
      (aset storage ptr (ecase (aref vector 0)
			  (BIT-VECTOR	(if object 1 0))
			  (STRING	(CHAR-CODE object))
			  (VECTOR	object)))
      (setf (vector-fp vector) (1+ ptr)))))

(defun VECTOR-PUSH-EXTEND (object vector &optional extension)
  (unless (and (VECTORP vector) (ARRAY-HAS-FILL-POINTER-P vector))
    (type-error vector '(AND VECTOR (NOT SIMPLE-VECTOR))))
  (let ((storage (vector-storage vector))
	(len (vector-size vector))
	(ptr (FILL-POINTER vector)))
    (when (eq ptr len)
      (let ((new-storage (make-vector (+ len (or extension len)) nil)))
	(dotimes (i len)
	  (aset new-storage i (aref storage i)))
	(setf (vector-storage vector) (setq storage new-storage))))
    (aset storage ptr (ecase (aref vector 0)
			(BIT-VECTOR	(if object 1 0))
			(STRING		(CHAR-CODE object))
			(VECTOR		object)))
    (setf (vector-fp vector) (1+ ptr))))

(defun VECTORP (object)
  (or (stringp object)
      (bit-vector-p object)
      (and (vectorp object)
	   (case (aref object 0)
	     ((STRING BIT-VECTOR VECTOR SIMPLE-VECTOR) T)))))

(defun BIT (array &rest subscripts)
  (cond
    ((SIMPLE-BIT-VECTOR-P array)
     (bref array (just-one subscripts)))
    ((BIT-VECTOR-P array)
     (bref (array-storage array) (just-one subscripts)))
    ((vector-and-typep array 'bit-array)
     (bref (array-storage array) (apply #'ARRAY-ROW-MAJOR-INDEX subscripts)))
    (t
     (type-error array '(ARRAY ,star BIT)))))

(defsetf BIT set-bit)

(DEFSETF BIT set-bit)

(defun set-bit (array index bit)
  (cond
    ((SIMPLE-BIT-VECTOR-P array)
     (setf (SBIT array index) bit))
    ((BIT-VECTOR-P array)
     (setf (SBIT (array-storage array) index) bit))
    ((vector-and-typep array 'bit-array)
     (setf (SBIT (array-storage array) index) bit))
    (t
     (type-error array '(ARRAY ,star BIT)))))

(defun SBIT (array index)
  (bref array index))

(defsetf SBIT set-sbit)

(DEFSETF SBIT set-sbit)

(defun set-sbit (array index bit)
  (setf (bref array index) bit))

(defun bit-array-p (object)
  (or (bit-vector-p object)
      (vector-and-typep object 'bit-array)))

(defun default-result (array result)
  (cond
    ((bit-array-p result)	result)
    ((eq result T)		array)
    (t				(if (BIT-VECTOR-P array)
				    (make-bit-vector (LENGTH array) 0)
				    (vector 'bit-array
					    (ARRAY-DIMENSIONS array)
					    (make-bit-vector
					     (ARRAY-TOTAL-SIZE array) 0))))))

(defun BIT-AND (array1 array2 &optional result)
  (let ((result (default-result array1 result))
	(storage1 (if (bit-vector-p array1) array1 (array-storage array1)))
	(storage2 (if (bit-vector-p array2) array2 (array-storage array2))))
    (dotimes (i (ARRAY-TOTAL-SIZE result))
      (aset result i (and (aref storage1 i) (aref storage2 i))))))

(defun BIT-ANDC1 (array1 array2 &optional result)
  (BIT-AND (BIT-NOT array1) array2 result))

(defun BIT-ANDC2 (array1 array2 &optional result)
  (BIT-AND (BIT-NOT array1) array2 result))

(defun BIT-EQV (array1 array2 &optional result)
  (BIT-NOT (BIT-XOR array1 array2 result)))

(defun BIT-IOR (array1 array2 &optional result)
  (let ((result (default-result array1 result))
	(storage1 (if (bit-vector-p array1) array1 (array-storage array1)))
	(storage2 (if (bit-vector-p array2) array2 (array-storage array2))))
    (dotimes (i (ARRAY-TOTAL-SIZE result))
      (aset result i (or (aref storage1 i) (aref storage2 i))))))

(defun BIT-NAND (array1 array2 &optional result)
  (BIT-NOT (BIT-AND array1 array2 result)))

(defun BIT-NOR (array1 array2 &optional result)
  (BIT-NOT (BIT-IOR array1 array2 result)))

(defun BIT-ORC1 (array1 array2 &optional result)
  (BIT-IOR (BIT-NOT array1) array2 result))

(defun BIT-ORC2 (array1 array2 &optional result)
  (BIT-IOR array1 (BIT-NOT array2) result))

(defun binary-xor (x y)
  (and (or x y)
       (not (and x y))))

(defun BIT-XOR (array1 array2 &optional result)
  (let ((result (default-result array1 result))
	(storage1 (if (bit-vector-p array1) array1 (array-storage array1)))
	(storage2 (if (bit-vector-p array2) array2 (array-storage array2))))
    (dotimes (i (ARRAY-TOTAL-SIZE result))
      (aset result i (binary-xor (aref storage1 i) (aref storage2 i))))))

(defun BIT-NOT (array &optional result)
  (let ((result (default-result array result))
	(storage (if (bit-vector-p array) array (array-storage array))))
    (dotimes (i (ARRAY-TOTAL-SIZE array))
      (aset result i (not (aref storage i))))))

(defun BIT-VECTOR-P (object)
  (or (bit-vector-p object)
      (vector-and-typep object 'BIT-VECTOR)))

(defun SIMPLE-BIT-VECTOR-P (object)
  (bit-vector-p object))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-arrays.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-compile.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements the compiler.

(IN-PACKAGE "EMACS-CL")

;;; TODO: `(QUOTE ,foo) -> `(LOAD-TIME-VALUE ,(MAKE-LOAD-FORM foo) T)

;;; (defun fac (n) (if (< n 2) 1 (* n (fac (1- n)))))
;;; (fac 100)
;;; 0.31 seconds

;;; (defun fib (n) (if (< n 2) 1 (+ (fib (1- n)) (fib (- n 2)))))
;;; (fib 22)
;;; 2.223 seconds

;;; (defun tak (x y z)
;;;   (if (not (< y x))
;;;       z
;;;       (tak (tak (1- x) y z) (tak (1- y) z x) (tak (1- z) x y))))
;;; (tak 18 12 6)
;;; 1.134 seconds

(defun COMPILE (name &optional definition)
  (when (null definition)
    (setq definition (if (FBOUNDP name)
			 (FDEFINITION name)
			 (MACRO-FUNCTION name))))
  (when (INTERPRETED-FUNCTION-P definition)
    (setq definition (FUNCTION-LAMBDA-EXPRESSION definition)))
  (when (consp definition)
    (setq definition (compile1 definition)))
  (if name
      (progn
	(if (FBOUNDP name)
	    (setf (FDEFINITION name) definition)
	    (setf (MACRO-FUNCTION name) definition))
	(cl:values name nil nil))
      (cl:values definition nil nil)))

; .elc header for GNU Emacs 20.7:
;ELC   
;;; Compiled by lars@nocrew.org on Sun Mar 21 10:33:13 2004
;;; from file /home/lars/src/emacs-cl/func.el
;;; in Emacs version 20.7.2
;;; with bytecomp version 2.56
;;; with all optimizations.

; .elc header for GNU Emacs 21.3:
;ELC   
;;; Compiled by lars@nocrew.org on Sun Mar 21 10:01:44 2004
;;; from file /home/lars/src/emacs-cl/func.el
;;; in Emacs version 21.3.1
;;; with bytecomp version 2.85.4.1
;;; with all optimizations.

; .elc header for XEmacs 21.4:
;ELC   
;;; compiled by lars@nocrew.org on Sun Mar 21 10:29:35 2004
;;; from file /home/lars/src/emacs-cl/func.el
;;; emacs version 21.4 (patch 6) "Common Lisp" XEmacs Lucid.
;;; bytecomp version 2.27 XEmacs; 2000-09-12.
;;; optimization is on.
;;; this file uses opcodes which do not exist in Emacs 19.

(cond
  ((eval-when-compile (eq (type-of (make-hash-table)) 'hash-table))
   (defmacro make-literal-table ()
     `(make-hash-table :test ',(if (featurep 'xemacs) 'equal 'EQUAL)))
   (defmacro get-literal (literal)
     `(gethash ,literal *compile-file-literals*))
   (defmacro put-literal (literal symbol)
     `(puthash ,literal ,symbol *compile-file-literals*))
   (defmacro remove-literal (literal)
     `(remhash ,literal *compile-file-literals*)))
  (t
   (defmacro make-literal-table ()
     `(list nil))
   (defmacro get-literal (literal)
     `(cdr (assq ,literal *compile-file-literals*)))
   (defmacro put-literal (literal symbol)
     `(push (cons ,literal ,symbol) *compile-file-literals*))
   (defmacro remove-literal (literal)
     `(setq *compile-file-literals*
            (assq-delete-all ,literal *compile-file-literals*)))))

(unless (fboundp 'assq-delete-all)
  (defun assq-delete-all (key alist)
    (delete* key alist :test 'eq :key 'car)))

(cl:defun COMPILE-FILE (input-file
			&REST keys
			&KEY OUTPUT-FILE
			     (VERBOSE *COMPILE-VERBOSE*)
			     (PRINT *COMPILE-PRINT*)
			     EXTERNAL-FORMAT)
  (let* ((*PACKAGE* *PACKAGE*)
	 (*READTABLE* *READTABLE*)
	 (*COMPILE-FILE-PATHNAME* (MERGE-PATHNAMES input-file))
	 (*COMPILE-FILE-TRUENAME* (TRUENAME *COMPILE-FILE-PATHNAME*))
	 (output (apply #'COMPILE-FILE-PATHNAME input-file keys))
	 (warnings-p nil)
	 (failure-p nil)
	 (*compile-file-mode* :not-compile-time)
	 (*compile-file-literals* (make-literal-table)))
    (WITH-COMPILATION-UNIT ()
      (let ((coding-system-for-read 'no-conversion)
	    (coding-system-for-write 'no-conversion))
	(WITH-OPEN-FILE (in *COMPILE-FILE-PATHNAME*)
	  (when VERBOSE
	    (FORMAT T "~&;Compiling ~A~%"
		    (NAMESTRING *COMPILE-FILE-PATHNAME*)))
	  (WITH-OPEN-FILE (out "/tmp/temp.el" (kw DIRECTION) (kw OUTPUT))
	    (let ((eof (gensym)))
	      (do ((form (READ in nil eof) (READ in nil eof)))
		  ((eq form eof))
		(when PRINT
		  (FORMAT T "~&;  ~S~%" (if (consp form)
					    (car form)
					    form)))
		(let* ((*compile-file-forms* nil)
		       (compiled-form (compile2 form)))
		  (dolist (form (nreverse *compile-file-forms*))
		    (WRITE-LINE (prin1-to-string form) out))
		  (WRITE-LINE (prin1-to-string compiled-form) out))))))
	(if (byte-compile-file "/tmp/temp.el")
	    (rename-file "/tmp/temp.elc" (NAMESTRING output) t)
	    (setq failure-p t))))
    (delete-file "/tmp/temp.el")
    (cl:values (TRUENAME output) warnings-p failure-p)))



(defvar *compile-file-mode* nil
  "Indicates whether file compilation is in effect, and if so, which
   mode: :compile-time-too, :not-compile-time, or t (neither of the
   previous two, but still compiling a file).")

(defvar *compile-file-literals* nil
  "A table for literals found by COMPILE-FILE.")

(defvar *compile-file-forms*)

(defvar *genreg-counter* 0)

(defun genreg ()
  (prog1 (make-symbol (format "R%d" *genreg-counter*))
    (incf *genreg-counter*)))

(defvar *registers* (list (genreg)))
(defvar *next-register* nil)

(defvar *bound* nil
  "A list of variables bound in the function currently being compiled.")

(defvar *free* nil
  "An alist of all variables are ever free in any function in the
   top-level form being compiled.")

(defvar *blocks-mentioned* nil
  "A list of all block names mentioned in the top-level form being compiled.")

(defun new-register ()
  (prog1
      (car *next-register*)
    (when (null (cdr *next-register*))
      (setf (cdr *next-register*) (list (genreg))))
    (setf *next-register* (cdr *next-register*))))

(defun lambda-expr-p (form)
  (and (consp form)
       (eq (car form) 'LAMBDA)))

(defmacro* with-fresh-context (&body body)
  `(let ((*next-register* *registers*)
	 (*free* nil)
	 (*bound* nil)
	 (*blocks-mentioned* nil))
     ,@body))

(defun compile1 (form)
  (byte-compile (compile2 form)))

(defun compile2 (form)
  (with-fresh-context
    (compile-form form *global-environment*)))

(defun cl-compiler-macroexpand (form name env)
  (let* ((fn (COMPILER-MACRO-FUNCTION name))
	 (new (if fn (FUNCALL *MACROEXPAND-HOOK* fn form env) form)))
    (when (and (not (eq form new))
	       (consp form))
      (setq form (cl-compiler-macroexpand
		  new
		  (if (eq (first new) 'FUNCALL)
		      (second new)
		      (first new))
		  env)))
    form))

(defconst +toplevel-forms+
  '(PROGN LOCALLY MACROLET SYMBOL-MACROLET EVAL-WHEN))

(defun* compile-form (form &optional env &key (values 1))
  (unless env
    (setq env *global-environment*))
  (when (and (consp form) (symbolp (first form)))
    (let* ((name (first form))
	   (fn (gethash name *form-compilers*)))
      (when fn
	(let ((*compile-file-mode*
	       (if (memq name +toplevel-forms+)
		   *compile-file-mode*
		   (when *compile-file-mode* t))))
	  (return-from compile-form (apply fn env (rest form)))))))
  (setq form (cl:values (MACROEXPAND form env)))
  (cond
    ((and (symbolp form) (not (KEYWORDP form)))
     (unless (eq values 0)
       (let ((val (compile-variable form env)))
	 (if (eq values t)
	     (if (null val)
		 `(setq nvals 1 mvals nil)
		 `(progn (setq nvals 1 mvals nil) ,val))
	     val))))
    ((atom form)
     (unless (eq values 0)
       (let ((val (compile-literal form env)))
	 (if (eq values t)
	     (if (null val)
		 `(setq nvals 1 mvals nil)
		 `(progn (setq nvals 1 mvals nil) ,val))
	     val))))
    ((lambda-expr-p (first form))
     (let* ((lexp (first form))
	    (vars (cadr lexp))
	    (body (cddr lexp))
	    (args (rest form))
	    (*compile-file-mode* (when *compile-file-mode* t)))
     (if (and (every #'symbolp vars)
	      (notany #'lambda-list-keyword-p vars)
	      (eq (length vars) (length args)))
	 (compile-form `(LET ,(MAPCAR #'list vars args) ,@body) env)
	 `(,(compile-lambda vars body env t) ,@(compile-forms args env)))))
    ((symbolp (first form))
     (let* ((name (first form))
	    (fn (gethash name *form-compilers*)))
       (if fn
	   (let ((*compile-file-mode*
		  (if (memq name +toplevel-forms+)
		      *compile-file-mode*
		      (when *compile-file-mode* t))))
	     (apply fn env (rest form)))
	   (let ((*compile-file-mode* (when *compile-file-mode* t)))
	     (compile-funcall `(FUNCTION ,name) (rest form) env)))))
    (t
     (ERROR "Syntax error: ~S" form))))

(defun compile-forms (args env)
  (mapcar (lambda (arg) (compile-form arg env)) args))

(defun* compile-body (forms env &key (values t))
  (if (null forms)
      nil
      (do* ((forms forms (cdr forms))
	    (form (car forms) (car forms))
	    (result nil))
	   ((null (cdr forms))
	    (push (compile-form form env :values values) result)
	    (nreverse result))
	(let ((comp (compile-form form env :values 0)))
	  (when comp
	    (push comp result))))))

(defun compile-variable (var env)
  (multiple-value-bind (type localp decls) (variable-information var env)
    (ecase type
      ((:special nil)	(if *compile-file-mode*
			    (if (interned-p var)
				var
				`(symbol-value ,(compile-literal var env)))
			    var))
      (:lexical		(when (and (not (memq var *bound*))
				   (not (MEMBER var *free* (kw KEY) #'car)))
			  (push (cons var (lexical-value var env)) *free*))
			(lexical-value var env))
      (:constant	(compile-literal (symbol-value var) env)))))

(defvar *literal-counter* 0)

(defun compile-load-time-value (form env &optional read-only-p)
  (if *compile-file-mode*
      (let ((symbol (intern (format "--emacs-cl-load-time--%d"
				    (incf *literal-counter*)))))
	(push `(setq ,symbol ,(compile-form form nil)) *compile-file-forms*)
	symbol)
      `(quote ,(eval-with-env form nil))))

(defun compile-literal (literal env)
  (cond
    ((null literal)
     nil)
    (*compile-file-mode*
     (or (and (symbolp literal)
	      (interned-p literal)
	      `(quote ,literal))
	 (get-literal literal)
	 (make-file-literal literal env)))
    ((or (consp literal) (symbolp literal))
     `(quote ,literal))
    (t
     literal)))

(defun make-file-literal (literal env)
  (let ((symbol (intern (format "--emacs-cl-literal--%d"
				(incf *literal-counter*)))))
    (put-literal literal symbol)
    (MULTIPLE-VALUE-BIND (load-form init-form) (MAKE-LOAD-FORM literal env)
      (cond
	((eq load-form literal)
	 (remove-literal literal)
	 literal)
	(t
	 (push `(setq ,symbol ,(compile-form load-form env))
	       *compile-file-forms*)
	 (push (compile-form init-form env) *compile-file-forms*)
	 symbol)))))

(defun built-in-make-load-form (form &optional env)
  (cl:values nil)
  (cond
    ((null form)
     nil)
    ((or (NUMBERP form) (CHARACTERP form) (SIMPLE-VECTOR-P form)  (subrp form)
	 (stringp form) (bit-vector-p form) (byte-code-function-p form))
     form)
    ((symbolp form)
     (cond
       ((SYMBOL-PACKAGE form)
	`(INTERN ,(SYMBOL-NAME form) ,(PACKAGE-NAME (SYMBOL-PACKAGE form))))
       (t
	`(MAKE-SYMBOL ,(SYMBOL-NAME form)))))
    ((PACKAGEP form)
     `(FIND-PACKAGE ,(PACKAGE-NAME form)))
    ;; TODO: RANDOM-STATE
    ((consp form)
     (if (and (ok-for-file-literal-p (car form))
	      (ok-for-file-literal-p (cdr form)))
	 (cl:values `(cons ,(car form) ,(cdr form)))
	 (cl:values
	  `(cons nil nil)
	  `(LET ((form (QUOTE ,form)))
	    (setcar form (QUOTE ,(car form)))
	    (setcdr form (QUOTE ,(cdr form)))))))
;;     ((and (bit-vector-p form)
;; 	  (not (featurep 'xemacs)))
;;      ;; There's a bug in GNU Emacs (20.7 and 21.3).  Bit vectors
;;      ;; printed to a source file and then byte-compiled can make the
;;      ;; compiled file unloadable due to error in the #& syntax.
;;	;; RESOLVED: Bind coding-system-for-write and coding-system-for-read
;;	;; to 'no-conversion.
;;      `(READ-FROM-STRING ,(PRIN1-TO-STRING form)))
    ((VECTORP form)
     (COPY-SEQ form))
    ((ARRAYP form)
     ;; Just dump the raw vector.
     form)
    ((HASH-TABLE-P form)
     `(LET ((table (MAKE-HASH-TABLE ,(kw TEST)
				    (QUOTE ,(HASH-TABLE-TEST form)))))
        (DOLIST (cons (QUOTE
		       ,(let ((result nil))
			  (MAPHASH (lambda (k v) (push (cons k v) result))
				   form)
			  result)))
	  (SETF (GETHASH (CAR cons) table) (CDR cons)))
       table))
    ((PATHNAMEP form)
     ;; Just dump the raw vector.
     form)
    ((and (INTERPRETED-FUNCTION-P form)
	  (eq (interp-fn-env form) *global-environment*))
     (byte-compile (compile-form (interp-fn-lambda-exp form))))
    (t
     (ERROR "~S is not an exteralizable object." form))))

(defun ok-for-file-literal-p (object)
  (or (stringp object)
      (null object)))
;;    (and (symbolp object)
;; 	   (interned-p object))))

(defvar *form-compilers* (make-hash-table))

(defmacro* define-compiler (operator lambda-list env &body body)
  `(setf (gethash (if (stringp ',operator)
		      (INTERN ,operator *emacs-cl-package*)
		      ',operator)
		  *form-compilers*)
	 (function* (lambda (,env ,@lambda-list) ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-compiler "*" (&rest args) env
  (case (length args)
    (0	1)
    (1	(compile-form (first args) env))
    (2	`(binary* ,@(compile-forms args env)))
    (t	(compile-funcall `(QUOTE ,(INTERN "*" *cl-package*)) args env))))

(define-compiler "+" (&rest args) env
  (case (length args)
    (0	0)
    (1	(compile-form (first args) env))
    (2	`(binary+ ,@(compile-forms args env)))
    (t	(compile-funcall `(QUOTE ,(INTERN "+" *cl-package*)) args env))))

(define-compiler "1+" (arg) env
  `(prog1 (binary+ ,(compile-form arg env) 1)
     (setq nvals 1 mvals nil)))

(define-compiler "1-" (arg) env
  `(binary+ ,(compile-form arg env) -1))

(define-compiler "=" (&rest args) env
  (case (length args)
    (0	(ERROR "'=' needs at least one argument"))
    (1	'T)
    (2	`(binary= ,@(compile-forms args env)))
    (t	(compile-funcall `(QUOTE ,(INTERN "=" *cl-package*)) args env))))

(define-compiler "/=" (&rest args) env
  (case (length args)
    (0	(ERROR "'/=' needs at least one argument"))
    (1	'T)
    (2	`(not (binary= ,@(compile-forms args env))))
    (t	(compile-funcall `(QUOTE ,(INTERN "/=" *cl-package*)) args env))))

(define-compiler "<" (&rest args) env
  (case (length args)
    (0	(ERROR "'<' needs at least one argument"))
    (1	'T)
    (2	`(binary< ,@(compile-forms args env)))
    (t	(compile-funcall `(QUOTE ,(INTERN "<" *cl-package*)) args env))))

(define-compiler "<=" (&rest args) env
  (case (length args)
    (0	(ERROR "'<=' needs at least one argument"))
    (1	'T)
    (2	`(binary<= ,@(compile-forms args env)))
    (t	(compile-funcall `(QUOTE ,(INTERN "<=" *cl-package*)) args env))))

(define-compiler ">" (&rest args) env
  (case (length args)
    (0	(ERROR "'>' needs at least one argument"))
    (1	'T)
    (2	(let ((forms (compile-forms args env)))
	  `(binary< ,(second forms) ,(first forms))))
    (t	(compile-funcall `(QUOTE ,(INTERN ">" *cl-package*)) args env))))

(define-compiler ">=" (&rest args) env
  (case (length args)
    (0	(ERROR "'>=' needs at least one argument"))
    (1	'T)
    (2	(let ((forms (compile-forms args env)))
	  `(binary<= ,(second forms) ,(first forms))))
    (t	(compile-funcall `(QUOTE ,(INTERN ">=" *cl-package*)) args env))))

(define-compiler APPLY (fn &rest args) env
  (if (zerop (length args))
      (ERROR 'PROGRAM-ERROR)
      (let ((fn (compile-form fn env))
	    (args (compile-forms args env)))
	(cond
	  ((subrp fn)
	   `(apply #',(intern (function-name fn)) ,@args))
	  ((byte-code-function-p fn)
	   `(apply ,fn ,@args))
	  (t
	   `(APPLY ,fn ,@args))))))

(define-compiler BLOCK (tag &rest body) env
  (let* ((block (gensym))
	 (new-env (augment-environment env :block (cons tag block)))
	 (compiled-body (compile-body body new-env)))
    (if (memq block *blocks-mentioned*)
	`(catch ',block ,@compiled-body)
	(body-form compiled-body))))

(define-compiler CATCH (tag &rest body) env
  `(catch ,(compile-form tag env) ,@(compile-body body env)))

(define-compiler COMPLEMENT (fn) env
  (setq fn (MACROEXPAND fn env))
  (if (and (consp fn)
	   (eq (first fn) 'FUNCTION))
      (if (lambda-expr-p (second fn))
	  (let ((args (cadadr fn))
		(body (cddadr fn)))
	    (compile-form `(LAMBDA ,args (not (PROGN ,@body))) env))
	  (with-gensyms (args)
	    `(lambda (&rest ,args)
	       (not (APPLY ,(compile-form fn env) ,args)))))
      `(COMPLEMENT ,(compile-form fn env))))

(define-compiler COND (&rest clauses) env
  `(cond
     ,@(mapcar (destructuring-lambda ((&whole clause condition &rest forms))
	         (if (and (CONSTANTP condition)
			  (not (null condition)))
		     `(t ,@(compile-body forms env))
		     (mapcar (lambda (form) (compile-form form env))
			     clause)))
	       clauses)))

(define-compiler EVAL-WHEN (situations &rest body) env
  (cond
    ((or (eq *compile-file-mode* :compile-time-too)
	 (eq *compile-file-mode* :not-compile-time))
     (compile-file-eval-when situations body env))
    ((or (memq (kw EXECUTE) situations)
	 (memq 'EVAL situations))
     (body-form (compile-body body env)))))

(defun compile-file-eval-when (situations body env)
  (let* ((ex (or (memq (kw EXECUTE) situations)
		 (memq 'EVAL situations)))
	 (ct (or (memq (kw COMPILE-TOPLEVEL) situations)
		 (memq 'COMPILE situations)))
	 (lt (or (memq (kw LOAD-TOPLEVEL) situations)
		 (memq 'LOAD situations)))
	 (ctt (eq *compile-file-mode* :compile-time-too))
	 (nct (eq *compile-file-mode* :not-compile-time)))
    ;; Figure 3-7 from CLHS:
    ;;   CT   LT   EX   Mode  Action    New Mode
    ;;   -----------------------------------------------
    ;; 1 Yes  Yes  ---  ---   Process   compile-time-too
    ;; 2 No   Yes  Yes  CTT   Process   compile-time-too
    ;; 3 No   Yes  Yes  NCT   Process   not-compile-time
    ;; 4 No   Yes  No   ---   Process   not-compile-time
    ;; 5 Yes  No   ---  ---   Evaluate  ---
    ;; 6 No   No   Yes  CTT   Evaluate  ---
    ;; 7 No   No   Yes  NCT   Discard   ---
    ;; 8 No   No   No   ---   Discard   ---
    (cond
      ((or (and ct lt)				;1
	   (and (not ct) lt ex ctt))		;2
       (eval-with-env `(PROGN ,@body) env)
       (let ((*compile-file-mode* :compile-time-too))
	 (body-form (compile-body body env))))
      ((or (and (not ct) lt ex nct)		;3
	   (and (not ct) lt (not ex)))		;4
       (let ((*compile-file-mode* :not-compile-time))
	 (body-form (compile-body body env))))
      ((or (and ct (not lt))			;5
	   (and (not ct) (not lt) ex ctt))	;6
       (let* ((*compile-file-mode* t)
	      (result (eval-with-env `(PROGN ,@body) env)))
	 (unless (eq values 0)
	   (compile-literal result env))))
      ((or (and (not ct) (not lt) ex nct)	;7
	   (and (not ct) (not lt) (not ex)))	;8
       nil)
      (t
       (ERROR "Bug here.")))))

(define-compiler FLET (fns &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let ((new-env (augment-environment env :function (mapcar #'first fns)))
	  (bindings nil))
      (dolist (fn fns)
	(destructuring-bind (name lambda-list &rest forms) fn
	  (let ((reg (new-register)))
	    (setf (lexical-function name new-env) reg)
	    (MULTIPLE-VALUE-BIND (body decls doc) (parse-body forms t)
	      (push `(,reg ,(compile-lambda
			     lambda-list
			     `((BLOCK ,(function-block-name name) ,@body))
			     env))
		    bindings)))))
      (let ((compiled-body (compile-body body new-env)))
	(cond
	  ((null *free*))
	  ((create-environment-p env new-env)
	   (setq compiled-body (compile-environment compiled-body new-env)))
	  (t
	   (setq compiled-body
		 (compile-env-inits compiled-body
				    (mapcar
				     (lambda (fn)
				       (lexical-function (car fn) new-env))
				     fns)
				    new-env))))
	`(let ,bindings ,@compiled-body)))))

(defun constant-function-name-p (object)
  (and (consp object)
       (memq (first object) '(QUOTE FUNCTION))
       (or (symbolp (second object))
	   (setf-name-p (second object)))))

(defun compile-funcall (fn args env)
  (let ((compiled-args (compile-forms args env)))
    (unless (null compiled-args)
      (let ((last (last compiled-args)))
	(unless (or (atom (car last))
		    (eq (first (car last)) 'quote))
	  (setf (car last)
		`(prog1 ,(car last) (setq nvals 1 mvals nil))))))
    (if (constant-function-name-p fn)
	(let ((name (second fn)))
	  (multiple-value-bind (type localp decls)
	      (function-information name (unless (eq (first fn) 'QUOTE) env))
	    (cond
	      ((eq name 'FUNCALL)
	       (when (null args)
		 (ERROR 'PROGRAM-ERROR))
	       (compile-funcall (first args) (rest args) env))
	      (localp
	       (when (and (not (memq name *bound*))
			  (not (MEMBER name *free* (kw KEY) #'car)))
		 (push (cons name (lexical-function name env))
		       *free*))
	       `(funcall ,(lexical-function name env) ,@compiled-args))
	      ((and (symbolp name)
		    (fboundp name)
		    (subrp (symbol-function name)))
	       `(,(intern (function-name (symbol-function name)))
		 ,@compiled-args))
	      ((interned-p name)
	       `(,name ,@compiled-args))
	      ((COMPILER-MACRO-FUNCTION name env)
	       (compile-form (cl-compiler-macroexpand form name env) env))
	      ((and (symbolp name) (not *compile-file-mode*))
	       `(,name ,@compiled-args))
	      (t
	       `(FUNCALL ,(compile-literal name env) ,@compiled-args)))))
	(let ((fn (compile-form fn env)))
	  (cond
	    ((subrp fn)
	     `(,(intern (function-name fn)) ,@compiled-args))
	    ((byte-code-function-p fn)
	     `(funcall ,fn ,@compiled-args))
	    (t
	     `(FUNCALL ,fn ,@compiled-args)))))))

(define-compiler FUNCALL (fn &rest args) env
  (compile-funcall fn args env))

(defun known-function-p (name)
  (and (symbolp name)
       (eq (SYMBOL-PACKAGE name) *emacs-cl-package*)))

(define-compiler FUNCTION (name) env
  (if (lambda-expr-p name)
      (compile-lambda (cadr name) (cddr name) env)
      (multiple-value-bind (type localp decl) (function-information name env)
	(cond
	  (localp	(when (and (not (memq name *bound*))
				   (not (MEMBER name *free*
						(kw KEY) #'car)))
			  (push (cons name (lexical-function name env))
				*free*))
			(lexical-function name env))
	  ((or (symbolp name) (setf-name-p name))
			(if (known-function-p name)
			    (compile-load-time-value
			     `(FDEFINITION (QUOTE ,name)) env)
			    `(FDEFINITION ,(compile-literal name env))))
	  (t		(ERROR "Syntax error: (FUNCTION ~S)" name))))))

(define-compiler GO (tag) env
  (let ((info (tagbody-information tag env)))
    (if info
	`(throw ',info ',tag)
	(ERROR "No tagbody for (GO ~S)" tag))))

(define-compiler IF (condition then &optional else) env
  (let ((compiled-condition (compile-form condition env))
	(compiled-then (compile-form then env)))
    (cond
      ((null compiled-condition)
       (compile-form else env))
      ((CONSTANTP condition env)
       compiled-then)
      ((equal compiled-condition compiled-then)
       `(or ,compiled-condition
	    ,@(when else
		(list (compile-form else env)))))
      (t
       `(if ,compiled-condition
	    ,compiled-then
	    ,@(when else
	        (list (compile-form else env))))))))

(define-compiler LABELS (fns &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let ((new-env (augment-environment env :function (mapcar #'first fns)))
	  (bindings nil)
	  (inits nil))
      (dolist (fn fns)
	(let ((reg (new-register)))
	  (setf (lexical-function (first fn) new-env) reg)
	  (push reg bindings)))
      (dolist (fn fns)
	(destructuring-bind (name lambda-list &rest forms) fn
	  (MULTIPLE-VALUE-BIND (body decls doc) (parse-body forms t)
	    (setq inits `(,(lexical-function name new-env)
			  ,(compile-lambda
			    lambda-list
			    `((BLOCK ,(function-block-name name) ,@body))
			    new-env)
			  ,@inits)))))
      (let ((compiled-body (compile-body body new-env)))
	(cond
	  ((null *free*))
	  ((create-environment-p env new-env)
	   (setq compiled-body (compile-environment compiled-body new-env)))
	  (t
	   (setq compiled-body
		 (compile-env-inits compiled-body
				    (mapcar
				     (lambda (fn)
				       (lexical-function (car fn) new-env))
				     fns)
				    new-env))))
	`(let ,bindings (setf ,@inits) ,@compiled-body)))))

(if (fboundp 'compiled-function-constants)
    (progn
      (defconst compiled-function-accessors
	'(compiled-function-arglist compiled-function-instructions
	  compiled-function-constants compiled-function-stack-depth
	  compiled-function-doc-string compiled-function-interactive))
      (defun cfref (fn i)
	(funcall (nth i compiled-function-accessors) fn)))
    (defun cfref (fn i)
      (aref fn i)))

(let ((fn (vector))
      (env (vector)))
  (defvar *trampoline-template*
    (byte-compile `(lambda (&rest args) (let ((env ,env)) (apply ,fn args)))))
  (defvar *trampoline-constants*
    (cfref *trampoline-template* 2))
  (defvar *trampoline-fn-pos*
    (position fn *trampoline-constants*))
  (defvar *trampoline-env-pos*
    (position env *trampoline-constants*)))

(defvar *trampoline-length*
  (condition-case c
      (length *trampoline-template*)
    (error 6)))

(defmacro defun-make-closure ()
  `(defun make-closure (fn env)
     (let* ((consts (copy-sequence ,*trampoline-constants*))
	    (tramp
	     (make-byte-code
	      ,@(let ((args nil))
		  (dotimes (i *trampoline-length* (nreverse args))
		    (push (if (eq i 2)
			      'consts
			      `',(cfref *trampoline-template* i))
			  args))))))
       (aset consts *trampoline-fn-pos* fn)
       (aset consts *trampoline-env-pos* env)
       tramp)))

(defun-make-closure)

(defun env-with-vars (env vars decls)
  (if vars
      (let ((new-env (augment-environment env :variable vars :declare decls)))
	(dolist (var vars)
	  (when (lexical-variable-p var new-env)
	    (setf (lexical-value var new-env) (new-register))
	    (push var *bound*)))
	new-env)
      env))

(defun variable-bound-p (var env)
  (nth-value 0 (variable-information var env)))

(defun create-environment-p (env new-env)
  (let ((vars (mapcar #'car *free*)))
    (and (every (lambda (var) (not (variable-bound-p var env))) vars)
	 (some (lambda (var) (variable-bound-p var new-env)) vars))))

(defun initial-environment (env)
  (mapcar (lambda (var)
	    (when (variable-bound-p (car var) env)
	      (cdr var)))
	  *free*))

(defun compile-environment (body env)
  (let ((inits (initial-environment env))
	(nfree (length *free*))
	make-env)
    (cond
      ((<= nfree 2)
       (setq make-env (if (eq nfree 1) `(cons ,@inits nil) `(cons ,@inits)))
       (MAPC (lambda (var accessor)
	       (setq body (NSUBST `(,accessor env) (cdr var) body)))
	     *free* '(car cdr)))
      (t
       (setq make-env `(vector ,@inits))
       (let ((i -1))
	 (dolist (var *free*)
	   (setq body (NSUBST `(aref env ,(incf i)) (cdr var) body))))))
    (prog1 `((let ((env ,make-env)) ,@body))
      (setq *free* nil))))

(defun compile-env-inits (body vars env)
  (let ((inits nil))
    (dolist (var *free*)
      (when (memq (cdr var) vars)
	    ;(and (memq (car var) vars)
	    ;	 (eq (compile-variable (car var) env) (cdr var)))
	(let ((reg (new-register)))
	  (setq inits `(,reg ,(cdr var) ,@inits))
	  (setq body (NSUBST reg (cdr var) body))
	  (setf (cdr var) reg))))
    (if inits
	`((setf ,@inits) ,@body)
	body)))

(defun compile-closure (lambda-list body env)
  `(make-closure ,(expand-lambda
		   lambda-list
		   (compile-env-inits body (compile-forms lambda-list env) env)
		   env)
		 env))

(defun compile-lambda (lambda-list forms env &optional keep-bindings)
  (MULTIPLE-VALUE-BIND (body decls doc) (parse-body forms t)
    (let* ((vars (lambda-list-variables lambda-list))
	   (*bound* (when keep-bindings *bound*))
	   (new-env (env-with-vars env vars decls))
	   (compiled-body (compile-body body new-env)))
      (dolist (decl decls)
	(when (eq (first decl) 'INTERACTIVE)
	  (push `(interactive ,@(rest decl)) compiled-body)))
      (when doc
	(when (null compiled-body)
	  (push nil compiled-body))
	(push doc compiled-body))
      (cond
	((null *free*)
	 (expand-lambda lambda-list compiled-body new-env))
	((create-environment-p env new-env)
	 (expand-lambda
	  lambda-list (compile-environment compiled-body new-env) new-env))
	(t
	 (compile-closure lambda-list compiled-body new-env))))))

(defun partition-bindings (bindings env)
  (let ((lexical-bindings nil)
	(special-bindings nil))
    (dolist (binding bindings)
      (let ((list (if (symbolp binding)
		      (list binding nil)
		      binding)))
	(if (lexical-variable-p (first list) env)
	    (push list lexical-bindings)
	    (push list special-bindings))))
    (cl:values lexical-bindings special-bindings)))

(defun first-or-identity (x)
  (if (atom x) x (car x)))

(defun body-form (body)
  (cond
    ((null body)				nil)
    ((and (consp body) (null (cdr body)))	(car body))
    (t						`(progn ,@body))))

(defun side-effect-free-p (form)
  (or (atom form)
      (let ((fn (car form)))
	(or (eq fn 'quote)
	    (and (symbolp fn)
		 (get fn 'side-effect-free)
		 (every #'side-effect-free-p (cdr form)))))))

(define-compiler LET (bindings &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let* ((vars (mapcar #'first-or-identity bindings))
	   (new-env (env-with-vars env vars decls))
	   (compiled-body (compile-body body new-env))
	   (special-bindings nil)
	   (let-bindings
	    (mapcan (lambda (binding)
		      (multiple-value-bind (var val save)
			  (if (consp binding)
			      (values (first binding) (second binding))
			      (values binding nil))
			(cond
			  ((and *compile-file-mode*
				(special-variable-p var new-env))
			   (let ((old (new-register))
				 (new (new-register)))
			     (setq var (compile-variable var new-env))
			     (push (list var old new) special-bindings)
			     (setq save `((,old ,var)))
			     (setq var new)))
			  (t
			   (setq var (compile-variable var new-env))))
			`(,@save (,var ,(compile-form val env)))))
		    bindings))
	   (body (cond
		   ((null *free*)
		    compiled-body)
		   ((create-environment-p env new-env)
		    (compile-environment compiled-body new-env))
		   (t
		    (compile-env-inits
		     compiled-body
		     (compile-forms (remove-if-not
				     (lambda (var)
				       (lexical-variable-p var new-env))
				     vars)
				    new-env)
		     new-env)))))
      (dolist (var vars)
	(when (lexical-variable-p var new-env)
	  (let* ((reg (compile-variable var new-env))
		 (list (find-if (lambda (list) (eq (first list) reg))
				let-bindings)))
	    (when nil ;(side-effect-free-p (second list))
	      (case (tree-count (first list) body)
		(0 (setq let-bindings (delq list let-bindings)))
		(1 (setq body (NSUBST (second list) (first list) body))
		   (setq let-bindings (delq list let-bindings))))))))
      (when special-bindings
	(setq body
	      `((unwind-protect
		    (progn
		      ,@(mapcar (destructuring-lambda ((var old new))
				  `(setf ,var ,new))
				special-bindings)
		      ,@body)
		  ,@(mapcar (destructuring-lambda ((var old new))
			      `(setf ,var ,old))
			    special-bindings)))))
      (if let-bindings
	  `(let ,let-bindings ,@body)
	  (body-form body)))))

(define-compiler LET* (bindings &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (compile-form (if (null bindings)
		      `(LOCALLY ,@body)
		      `(LET (,(first bindings))
			(LET* ,(rest bindings)
			  ,@body)))
		  env)))

(define-compiler LOAD-TIME-VALUE (form &optional read-only-p) env
  (compile-load-time-value form env read-only-p))

(define-compiler LOCALLY (&rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    ;; TODO: process decls
    (body-form (compile-body body env))))

(define-compiler MACROLET (macros &body forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let ((new-env (env-with-macros env macros decls)))
      (body-form (compile-body body new-env)))))

(define-compiler MULTIPLE-VALUE-BIND (vars form &body forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let* ((new-env (env-with-vars env vars decls))
	   (compiled-body (compile-body body new-env)))
      (unless (null *free*)
	(setq compiled-body
	      (if (create-environment-p env new-env)
		  (compile-environment compiled-body new-env)
		  (compile-env-inits compiled-body
				     (compile-forms vars new-env)
				     new-env))))
      (case (length vars)
	(0 `(progn
	     ,(compile-form form env :values 0)
	     ,@compiled-body))
	(1 `(let ((,(compile-variable (first vars) new-env)
		   ,(compile-form form env :values 1)))
	     ,@compiled-body))
	(t `(let* ((,(compile-variable (first vars) new-env)
		    ,(compile-form form env :values t))
		   ,@(mapcar (lambda (var)
			       `(,(compile-variable var new-env)
				 (pop mvals)))
			     (rest vars)))
	     ,@compiled-body))))))

(define-compiler MULTIPLE-VALUE-CALL (fn &rest forms) env
  (if (null forms)
      (compile-form `(FUNCALL ,fn) env)
      `(APPLY ,(compile-form fn env)
	      (append ,@(mapcar (lambda (form)
				  (compile-form
				   `(MULTIPLE-VALUE-LIST ,form)
				   env :values t))
				forms)))))

(define-compiler MULTIPLE-VALUE-LIST (form) env
  (let ((val (new-register)))
    `(let ((,val ,(compile-form form env :values t)))
       (if (zerop nvals)
	   nil
	   (cons ,val mvals)))))

(define-compiler MULTIPLE-VALUE-PROG1 (form &rest forms) env
  (let ((val (new-register))
	(ntemp (new-register))
	(mtemp (new-register)))
    `(let* ((,val ,(compile-form form env :values t))
	    (,ntemp nvals)
	    (,mtemp mvals))
       ,@(compile-body forms env :values 0)
       (setq nvals ,ntemp mvals ,mtemp)
       ,val)))

(define-compiler NOT (form) env
  `(if ,(compile-form form env) nil 'T))

(define-compiler NULL (form) env
  `(if ,(compile-form form env) nil 'T))

(define-compiler PROGN (&rest body) env
  (body-form (compile-body body env)))

(define-compiler PROGV (symbols values &body body) env
  `(do-progv ,(compile-form symbols env)
             ,(compile-form values env)
	     ,(compile-lambda () body env)))

(define-compiler QUOTE (form) env
  (compile-literal form env))

(define-compiler RETURN-FROM (tag &optional form) env
  (let ((block (block-information tag env)))
    (if block
	(let ((block-tag (cdr block)))
	  (pushnew block-tag *blocks-mentioned*)
	  `(throw ',block-tag ,(compile-form form env)))
	(ERROR "No block for (RETURN-FROM ~S)" form))))

(define-compiler SETQ (&rest forms) env
  (when (oddp (length forms))
    (ERROR "Odd number of forms in SETQ"))
  (body-form
   (mapcar2
    (lambda (var val)
      (unless (symbolp var)
	(ERROR "Setting non-symbol ~S." var))
      (multiple-value-bind (type localp) (variable-information var env)
	(ecase type
	  (:lexical
	   `(setf ,(compile-variable var env) ,(compile-form val env)))
	  ((:special nil)
	   (when (null type)
	     (WARN "Setting undefined variable ~S." var))
	   (if (and *compile-file-mode*
		    (not (interned-p var)))
	       `(set ,(compile-literal var env) ,(compile-form val env))
	       `(setq ,var ,(compile-form val env))))
	  (:symbol-macro
	   (compile-form `(SETF ,(MACROEXPAND var env) ,val) env))
	  (:constant
	   (ERROR "Setting constant ~S." var)))))
    forms)))

(define-compiler SYMBOL-MACROLET (macros &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let ((new-env (augment-environment env :symbol-macro
					(mapcar #'first macros))))
      (dolist (macro macros)
	(setf (lexical-value (first macro) new-env)
	      (enclose `(LAMBDA (form env) (QUOTE ,(second macro)))
		       env (first macro))))
      (body-form (compile-body body new-env)))))

(defun compile-tagbody-forms (forms tagbody start env)
  (let* ((nofirst (eq start (car forms)))
	 (clause (unless nofirst (list (list start)))))
    (do ((clauses nil)
	 (forms forms (cdr forms)))
	((null forms)
	 (unless (eq (first (car (last clause))) 'throw)
	   (setq clause (append clause `((throw ',tagbody nil)))))
	 (setq clauses (append clauses `(,clause)))
	 clauses)
      (let ((form (first forms)))
	(cond
	  ((atom form)
	   (unless nofirst
	     (setq clause (append clause `((throw ',tagbody ',form))))
	     (setq nofirst nil))
	   (when clause
	     (setq clauses (append clauses `(,clause))))
	   (setq clause `((,form))))
	  (t
	   (setq clause (append clause `(,(compile-form form env))))))))))

(define-compiler TAGBODY (&rest forms) env
  (let* ((tagbody (gensym))
	 (new-env (augment-environment
		   env :tagbody
		   (cons tagbody (remove-if-not #'go-tag-p forms))))
	 (pc (new-register))
	 (start (if (go-tag-p (car forms)) (car forms) (gensym))))
    (let ((last (last forms)))
      (cond
	((notany #'go-tag-p forms)
	 `(progn ,@forms nil))
	((and (consp last)
	      (setq last (car last))
	      (consp last)
	      (eq (first last) 'GO)
	      (eq (second last) start)
	      (notany #'go-tag-p (rest forms)))
	 `(while t ,@(compile-body (butlast (rest forms)) env :values 0)))
	(t
	 `(let ((,pc ',start))
	    (while (setq ,pc (catch ',tagbody
			       (case ,pc
				 ,@(compile-tagbody-forms
				    forms tagbody start new-env)))))))))))

(define-compiler THE (type form) env
  (compile-form form env))

(define-compiler THROW (tag form) env
  `(throw ,(compile-form tag env) ,(compile-form form env)))

(define-compiler UNWIND-PROTECT (protected &rest cleanups) env
  (let ((ntmp (new-register))
	(mtmp (new-register)))
    `(let (,ntmp ,mtmp)
       (prog1 (unwind-protect (prog1 ,(compile-form protected env)
				(setq ,ntmp nvals ,mtmp mvals))
		,@(compile-body cleanups env :values 0))
	 (setq nvals ,ntmp mvals ,mtmp)))))

(define-compiler VALUES (&rest forms) env
  (let ((n (length forms)))
    (case n
      (0	`(setq nvals 0 mvals nil))
      (1	`(prog1 ,(compile-form (car forms) env)
		   (setq nvals 1 mvals nil)))
      (t	`(prog1 ,(compile-form (car forms) env)
		   (setq nvals ,n
		         mvals (list ,@(compile-forms (cdr forms) env))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-compile.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-conses.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 14, Conses.

(IN-PACKAGE "EMACS-CL")

;;; System Class LIST
;;; System Class NULL
;;; System Class CONS
;;; Type ATOM

(dolist (sym '(cons consp atom))
  (apply (lambda (to from) (fset to (symbol-function from)))
	 (list (intern (upcase (symbol-name sym))) sym)))

(defun RPLACA (cons object)
  (setcar cons object)
  cons)

(defun RPLACD (cons object)
  (setcdr cons object)
  cons)

(fset 'CAR (symbol-function 'car))

(DEFSETF CAR (cons) (car)
  `(PROGN
     (RPLACA ,cons ,car)
     ,car))

(fset 'CDR (symbol-function 'cdr))

(DEFSETF CDR (cons) (cdr)
  `(PROGN
     (RPLACD ,cons ,cdr)
     ,cdr))

(defun build-cxr (string index var fn)
  (case (aref string index)
    (?A		(funcall fn 'CAR (build-cxr string (1+ index) var fn)))
    (?D		(funcall fn 'CDR (build-cxr string (1+ index) var fn)))
    (t		var)))

(defun plain-cxr (fn arg)
  `(,fn ,arg))

(defun backquoted-cxr (fn arg)
  `(list ',fn ,arg))

(macrolet ((def (sym)
	     (let ((name (symbol-name sym)))
	       `(progn
		 (defun ,sym (cons)
		   ,(build-cxr name 1 'cons #'plain-cxr))
		 (DEFSETF ,sym (cons) (obj)
		   (list ',(if (eq (aref name 1) ?A) 'setcar 'setcdr)
			 ,(build-cxr name 2 'cons #'backquoted-cxr)
			 obj))))))
  (def CAAR) (def CADR) (def CDAR) (def CDDR)
  (def CAAAR) (def CAADR) (def CADAR) (def CADDR)
  (def CDAAR) (def CDADR) (def CDDAR) (def CDDDR)
  (def CAAAAR) (def CAAADR) (def CAADAR) (def CAADDR)
  (def CADAAR) (def CADADR) (def CADDAR) (def CADDDR)
  (def CDAAAR) (def CDAADR) (def CDADAR) (def CDADDR)
  (def CDDAAR) (def CDDADR) (def CDDDAR) (def CDDDDR))

(defun COPY-TREE (tree)
  (if (CONSP tree)
      (CONS (COPY-TREE (CAR tree)) (COPY-TREE (CDR tree)))
      tree))

(cl:defun SUBLIS (alist tree &KEY KEY TEST TEST-NOT)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (let ((pair (ASSOC tree alist (kw KEY) KEY (kw TEST) TEST)))
    (cond
      (pair		(CDR pair))
      ((ATOM tree)	tree)
      (t		(CONS
			 (SUBLIS alist (CAR tree) (kw KEY) KEY (kw TEST) TEST)
			 (SUBLIS alist (CDR tree) (kw KEY) KEY (kw TEST) TEST))))))

(cl:defun NSUBLIS (alist tree &KEY KEY TEST TEST-NOT)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (let ((pair (ASSOC tree alist (kw KEY) KEY (kw TEST) TEST)))
    (cond
      (pair		(CDR pair))
      ((ATOM tree)	tree)
      (t
       (progn
	 (RPLACA tree (NSUBLIS alist (CAR tree) (kw KEY) KEY (kw TEST) TEST))
	 (RPLACD tree (NSUBLIS alist (CDR tree) (kw KEY) KEY (kw TEST) TEST)))))))

(cl:defun SUBST (new old tree &KEY KEY TEST TEST-NOT)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (cond
    ((FUNCALL TEST old (FUNCALL KEY tree))
     new)
    ((ATOM tree)
     tree)
    (t
     (CONS (SUBST new old (CAR tree) (kw KEY) KEY (kw TEST) TEST)
	   (SUBST new old (CDR tree) (kw KEY) KEY (kw TEST) TEST)))))

(cl:defun SUBST-IF (new predicate tree &KEY KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (cond
    ((FUNCALL predicate (FUNCALL KEY tree))
     new)
    ((ATOM tree)
     tree)
    (t
     (CONS (SUBST-IF new predicate (CAR tree) (kw KEY) KEY)
	   (SUBST-IF new predicate (CDR tree) (kw KEY) KEY)))))

(cl:defun SUBST-IF-NOT (new predicate tree &KEY KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (cond
    ((not (FUNCALL predicate (FUNCALL KEY tree)))
     new)
    ((ATOM tree)
     tree)
    (t
     (CONS (SUBST-IF new predicate (CAR tree) (kw KEY) KEY)
	   (SUBST-IF new predicate (CDR tree) (kw KEY) KEY)))))

(cl:defun NSUBST (new old tree &KEY KEY TEST TEST-NOT)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (cond
    ((FUNCALL TEST old (FUNCALL KEY tree))
     new)
    ((ATOM tree)
     tree)
    (t
     (RPLACA tree (NSUBST new old (CAR tree) (kw KEY) KEY (kw TEST) TEST))
     (RPLACD tree (NSUBST new old (CDR tree) (kw KEY) KEY (kw TEST) TEST)))))

(cl:defun NSUBST-IF (new predicate tree &KEY KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (cond
    ((FUNCALL predicate (FUNCALL KEY tree))
     new)
    ((ATOM tree)
     tree)
    (t
     (RPLACA tree (NSUBST-IF new predicate (CAR tree) (kw KEY) KEY))
     (RPLACD tree (NSUBST-IF new predicate (CDR tree) (kw KEY) KEY)))))

(cl:defun NSUBST-IF-NOT (new predicate tree &KEY KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (cond
    ((not (FUNCALL predicate (FUNCALL KEY tree)))
     new)
    ((ATOM tree)
     tree)
    (t
     (RPLACA tree (NSUBST-IF new predicate (CAR tree) (kw KEY) KEY))
     (RPLACD tree (NSUBST-IF new predicate (CDR tree) (kw KEY) KEY)))))

(cl:defun TREE-EQUAL (tree1 tree2 &KEY TEST TEST-NOT)
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (cond
    ((and (ATOM tree1) (ATOM tree2))
     (cl:values (FUNCALL TEST tree1 tree2)))
    ((and (CONSP tree1) (CONSP tree2))
     (and (TREE-EQUAL (CAR tree1) (CAR tree2) (kw TEST) TEST)
	  (TREE-EQUAL (CDR tree1) (CDR tree2) (kw TEST) TEST)))))

(fset 'COPY-LIST (symbol-function 'copy-list))

(fset 'LIST (symbol-function 'list))

(fset 'LIST* (symbol-function 'list*))

(fset 'LIST-LENGTH (symbol-function 'list-length))

(fset 'LISTP (symbol-function 'listp))

(cl:defun MAKE-LIST (size &KEY INITIAL-ELEMENT)
  (make-list size INITIAL-ELEMENT))

(cl:defmacro PUSH (object place)
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place nil) ;TODO: environment
    (with-gensyms (obj)
      `(LET* ((,obj ,object)
	      ,@(MAPCAR #'list temps values)
	      (,(first variables) (CONS ,obj ,getter)))
	 ,setter))))

(cl:defmacro POP (place)
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place nil) ;TODO: environment
    (with-gensyms (car get)
      `(LET* (,@(MAPCAR #'list temps values)
	      (,get ,getter)
	      (,car (CAR ,get))
	      (,(first variables) (CDR ,get)))
	 ,setter
	 ,car))))

(fset 'FIRST (symbol-function 'car))

(DEFSETF FIRST (list) (new)
  `(PROGN
     (RPLACA ,list ,new)
     ,new))

(defun SECOND (list)
  (CADR list))

(DEFSETF SECOND (list) (new)
  `(PROGN
     (RPLACA (CDR ,list) ,new)
     ,new))

(defun THIRD (list)
  (CADDR list))

(DEFSETF THIRD (list) (new)
  `(PROGN
     (RPLACA (CDDR ,list) ,new)
     ,new))

(defun FOURTH (list)
  (CADDDR list))

(DEFSETF FOURTH (list) (new)
  `(PROGN
     (RPLACA (CDDDR ,list) ,new)
     ,new))

(defun FIFTH (list)
  (CAR (CDDDDR list)))

(DEFSETF FIFTH (list) (new)
  `(PROGN
     (RPLACA (CDDDDR ,list) ,new)
     ,new))

(defun SIXTH (list)
  (CADR (CDDDDR list)))

(DEFSETF SIXTH (list) (new)
  `(PROGN
     (RPLACA (CDR (CDDDDR ,list)) ,new)
     ,new))

(defun SEVENTH (list)
  (CADDR (CDDDDR list)))

(DEFSETF SEVENTH (list) (new)
  `(PROGN
     (RPLACA (CDDR (CDDDDR ,list)) ,new)
     ,new))

(defun EIGHTH (list)
  (CADDDR (CDDDDR list)))

(DEFSETF EIGHTH (list) (new)
  `(PROGN
     (RPLACA (CDDDR (CDDDDR ,list)) ,new)
     ,new))

(defun NINTH (list)
  (CAR (CDDDDR (CDDDDR list))))

(DEFSETF NINTH (list) (new)
  `(PROGN
     (RPLACA (CDDDDR (CDDDDR ,list)) ,new)
     ,new))

(defun TENTH (list)
  (CADR (CDDDDR (CDDDDR list))))

(DEFSETF TENTH (list) (new)
  `(PROGN
     (RPLACA (CDR (CDDDDR (CDDDDR ,list))) ,new)
     ,new))

(fset 'NTH (symbol-function 'nth))

(DEFSETF NTH (n list) (new)
  (with-gensyms (cons)
    `(LET ((,cons (NTHCDR ,n ,list)))
       (WHEN ,cons
	 (RPLACA ,cons ,new))
       ,new)))

(defun ENDP (object)
  (cond
    ((null object)	'T)
    ((consp object)	nil)
    (t			(type-error object 'LIST))))

(defun NULL (list)
  (if list nil 'T))

(fset 'NCONC (symbol-function 'nconc))

(fset 'APPEND (symbol-function 'append))

(defun REVAPPEND (list tail)
  (nconc (reverse list) tail))

(defun NRECONC (list tail)
  (nconc (nreverse list) tail))

(cl:defun BUTLAST (list &OPTIONAL (n 1))
  (LDIFF list (LAST list n)))

(cl:defun NBUTLAST (list &OPTIONAL (n 1))
  (unless (listp list)
    (type-error list 'LIST))
  (unless (TYPEP n '(INTEGER 0))
    (type-error n '(INTEGER 0)))
  (do ((l list (cdr l))
       (r nil)
       (i 0 (binary+ i 1)))
      ((atom l)
       (when (consp r)
	 (setcdr r nil)
	 list))
    (cond
      ((binary= n i) (setq r list))
      ((binary< n i) (pop r)))))

(cl:defun LAST (list &OPTIONAL (n 1))
  (unless (listp list)
    (type-error list 'LIST))
  (unless (TYPEP n '(INTEGER 0))
    (type-error n '(INTEGER 0)))
  (do ((l list (cdr l))
       (r list)
       (i 0 (binary+ i 1)))
      ((atom l) r)
    (if (binary<= n i) (pop r))))

(defun LDIFF (list object)
  (unless (listp list)
    (type-error list 'LIST))
  (catch 'LDIFF
    (do ((list list (cdr list))
	 (r '() (cons (car list) r)))
	((atom list)
	 (if (EQL list object) (nreverse r) (NRECONC r list)))
      (when (EQL object list)
	(throw 'LDIFF (nreverse r))))))

(defun TAILP (object list)
  (unless (listp list)
    (type-error list 'LIST))
  (catch 'TAILP
    (do ((list list (cdr list)))
	((atom list) (EQL list object))
      (if (EQL object list)
	  (throw 'TAILP T)))))

(fset 'NTHCDR (symbol-function 'nthcdr))

(fset 'REST (symbol-function 'cdr-safe))

(DEFSETF REST (cons) (cdr)
  `(setcdr ,cons ,cdr))

(cl:defun MEMBER (object list &KEY KEY TEST TEST-NOT)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (catch 'MEMBER
    (do ((list list (cdr list)))
	((null list) nil)
      (when (FUNCALL TEST object (FUNCALL KEY (car list)))
	(throw 'MEMBER list)))))

(cl:defun MEMBER-IF (predicate list &KEY KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (catch 'MEMBER-IF
    (do ((list list (cdr list)))
	((null list) nil)
      (when (FUNCALL predicate (FUNCALL KEY (car list)))
	(throw 'MEMBER-IF list)))))

(cl:defun MEMBER-IF-NOT (predicate list &KEY KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (catch 'MEMBER-IF-NOT
    (do ((list list (cdr list)))
	((null list) nil)
      (unless (FUNCALL predicate (FUNCALL KEY (car list)))
	(throw 'MEMBER-IF-NOT list)))))

(defun MAPC (fn &rest lists)
  (when (null lists)
    (ERROR 'PROGRAM-ERROR))
  (let ((result (car lists)))
    (while (notany (cl:function ENDP) lists)
      (APPLY fn (mapcar (cl:function car) lists))
      (setq lists (mapcar (cl:function cdr) lists)))
    result))

(defun MAPCAR (fn &rest lists)
  (when (null lists)
    (ERROR 'PROGRAM-ERROR))
  (let ((result nil))
    (while (notany (cl:function ENDP) lists)
      (push (APPLY fn (mapcar (cl:function car) lists)) result)
      (setq lists (mapcar 'cdr lists)))
    (nreverse result)))

(defun MAPCAN (fn &rest lists)
  (apply (cl:function nconc)
	 (apply (cl:function MAPCAR) fn lists)))

(defun MAPL (fn &rest lists)
  (when (null lists)
    (ERROR 'PROGRAM-ERROR))
  (let ((result (car lists)))
    (while (notany (cl:function ENDP) lists)
      (APPLY fn lists)
      (setq lists (mapcar (cl:function cdr) lists)))
    result))

(defun MAPLIST (fn &rest lists)
  (when (null lists)
    (ERROR 'PROGRAM-ERROR))
  (let ((result nil))
    (while (notany (cl:function ENDP) lists)
      (push (APPLY fn lists) result)
      (setq lists (mapcar 'cdr lists)))
    (nreverse result)))

(defun MAPCON (fn &rest lists)
  (apply (cl:function nconc)
	 (apply (cl:function MAPLIST) fn lists)))

(defun ACONS (key datum alist)
  (CONS (CONS key datum) alist))

(cl:defun ASSOC (item alist &KEY KEY TEST TEST-NOT)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (catch 'ASSOC
    (dolist (pair alist)
      (when (and pair (FUNCALL TEST item (FUNCALL KEY (car pair))))
	(throw 'ASSOC pair)))))

(cl:defun ASSOC-IF (predicate alist &KEY KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (catch 'ASSOC-IF
    (dolist (pair alist)
      (when (and pair (FUNCALL predicate (FUNCALL KEY (car pair))))
	(throw 'ASSOC-IF pair)))))

(cl:defun ASSOC-IF-NOT (predicate alist &KEY KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (catch 'ASSOC-IF-NOT
    (dolist (pair alist)
      (when (and pair (not (FUNCALL predicate (FUNCALL KEY (car pair)))))
	(throw 'ASSOC-IF-NOT pair)))))

(defun COPY-ALIST (alist)
  (mapcar (lambda (pair) (CONS (CAR pair) (CDR pair))) alist))

(defun PAIRLIS (keys data &optional alist)
  (NCONC (MAPCAR #'CONS keys data) alist))

(cl:defun RASSOC (item alist &KEY KEY TEST TEST-NOT)
  (unless KEY
    (setq KEY #'IDENTITY))
  (when (and TEST TEST-NOT)
    (error "error"))
  (when TEST-NOT
    (setq TEST (COMPLEMENT TEST-NOT)))
  (unless TEST
    (setq TEST #'EQL))
  (catch 'RASSOC
    (dolist (pair alist)
      (when (and pair (FUNCALL TEST item (FUNCALL KEY (cdr pair))))
	(throw 'RASSOC pair)))))

(cl:defun RASSOC-IF (predicate alist &KEY KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (catch 'RASSOC-IF
    (dolist (pair alist)
      (when (and pair (FUNCALL predicate (FUNCALL KEY (cdr pair))))
	(throw 'RASSOC-IF pair)))))

(cl:defun RASSOC-IF-NOT (predicate alist &KEY KEY)
  (unless KEY
    (setq KEY #'IDENTITY))
  (catch 'RASSOC-IF-NOT
    (dolist (pair alist)
      (when (and pair (not (FUNCALL predicate (FUNCALL KEY (cdr pair)))))
	(throw 'RASSOC-IF pair)))))

(defun* GET-PROPERTIES (plist indicators)
  (do ((plist plist (cddr plist)))
      ((null plist)
       (cl:values nil nil nil))
    (when (memq (car plist) indicators)
      (return-from GET-PROPERTIES
	(cl:values (car plist) (cadr plist) plist)))))

(defun* GETF (plist indicator &optional default)
  (do ((plist plist (cddr plist)))
      ((null plist)
       default)
    (when (eq (car plist) indicator)
      (return-from GETF (cadr plist)))))

(DEFINE-SETF-EXPANDER GETF (plist indicator &optional default)
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION plist nil) ;TODO: env
    (with-gensyms (itemp dtemp obj)
      (let (ilist)
	(unless (null default)
	  (push dtemp temps)
	  (push default values))
	(if (CONSTANTP indicator)
	    (setq itemp (eval-with-env indicator nil) ;TODO: env
		  ilist `(QUOTE (,itemp)))
	    (setq temps (cons itemp temps)
		  values (cons indicator values)
		  ilist `(LIST ,itemp)))
	(cl:values temps
		   values
		   (list obj)
		   `(MULTIPLE-VALUE-BIND (ind val tail)
			(GET-PROPERTIES ,getter ,ilist)
		      (IF (NULL tail)
			  (MULTIPLE-VALUE-BIND ,variables 
			      (LIST* ,itemp ,obj ,getter)
			    ,setter)
			  (SETF (SECOND tail) ,obj)))
		   `(GETF ,getter ,itemp))))))

(defun delete-property (plist indicator)
  (cond
    ((null plist)			nil)
    ((eq (car plist) indicator)		(cddr plist))
    (t					(RPLACD (cdr plist)
						(delete-property (cddr plist)
								 indicator))
					plist)))

(cl:defmacro REMF (place indicator) ;TODO: &environment
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place nil)
    `(LET* (,@(MAPCAR #'list temps values)
	    (,(first variables) (delete-property ,getter ,indicator)))
       ,setter)))

(cl:defun INTERSECTION (list1 list2 &REST keys)
  (let ((result nil))
    (dolist (x list1 result)
      (when (apply #'MEMBER x list2 keys)
	(push x result)))))

(fset 'NINTERSECTION (symbol-function 'INTERSECTION))

(cl:defun ADJOIN (object list &REST keys)
  (if (apply #'MEMBER object list keys)
      list
      (cons object list)))

(cl:defmacro PUSHNEW (object place &rest keys)
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place nil) ;TODO: environment
    (with-gensyms (obj)
      `(LET* ((,obj ,object)
	      ,@(MAPCAR #'list temps values)
	      (,(first variables) (ADJOIN ,obj ,getter ,@keys)))
	 ,setter))))

(cl:defun SET-DIFFERENCE (list1 list2 &REST keys)
  (let ((result nil))
    (dolist (x list1 result)
      (unless (apply #'MEMBER x list2 keys)
	(push x result)))))

(fset 'NSET-DIFFERENCE (symbol-function 'SET-DIFFERENCE))

(cl:defun SET-EXCLUSIVE-OR (list1 list2 &REST keys)
  (let ((result nil))
    (dolist (x list1)
      (unless (apply #'MEMBER x list2 keys)
	(push x result)))
    (dolist (x list2 result)
      (unless (apply #'MEMBER x list1 keys)
	(push x result)))))

(fset 'NSET-EXCLUSIVE-OR (symbol-function 'SET-EXCLUSIVE-OR))

(cl:defun SUBSETP (list1 list2 &REST keys)
  (EVERY (lambda (x) (apply #'MEMBER x list2 keys))
	 list1))

(cl:defun UNION (list1 list2 &REST keys)
  (let ((result nil))
    (dolist (x list1)
      (setq result (apply #'ADJOIN x result keys)))
    (dolist (x list2 result)
      (setq result (apply #'ADJOIN x result keys)))))

(fset 'NUNION (symbol-function 'UNION))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-conses.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-structures.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 8, Structures.

(IN-PACKAGE "EMACS-CL")

(defvar *initial-classes* nil)

;;; Redefined later.
(defun ENSURE-CLASS (name &rest keys)
  (unless (memq (kw METACLASS) keys)
    (setq keys (list* (kw METACLASS) 'STANDARD-CLASS keys)))
  (push (cons name keys) *initial-classes*)
  (ensure-type
   name
   (byte-compile `(lambda (obj env)
		    (and (vectorp obj)
			 (struct-subtypep (aref obj 0) ',name))))))

;;; A hash table keyed on a structure name.  The data is a cons which
;;; car is a list of structure names that are subtypes of the key.
;;; The cdr is a list of slot descriptions in the form `(,name
;;; ,initval ,type ,read-only).
(defvar *structure-info* (make-hash-table))

(defun struct-subtypep (type1 type2)
  (or (eq type1 type2)
      (let ((subtypes (car (gethash type2 *structure-info*))))
	(if (memq type1 subtypes)
	    T
	    (some (lambda (type)
		    (and (struct-subtypep type1 type)
			 (struct-subtypep type type2)))
		  subtypes)))))

(defun add-struct-subtype (struct sub)
  (maphash (lambda (key val)
	     (setf (car val) (delete sub (car val))))
	   *structure-info*)
  (push sub (car (gethash struct *structure-info*))))

(defun structurep (object)
  (and (vectorp object)
       (gethash (aref object 0) *structure-info*)))

(defun struct-slots (struct)
  (cdr (gethash struct *structure-info*)))

(defsetf struct-slots (struct) (slots)
  `(setf (gethash ,struct *structure-info*) (cons nil ,slots)))

(defun slot-name (slot)
  (first slot))

(defun slot-initval (slot)
  (second slot))

(defun slot-type (slot)
  (third slot))

(defun slot-read-only-p (slot)
  (fourth slot))

(defun canonicalize-slot (slot)
  (cond
    ((atom slot)
     (list slot unbound t nil))
    ((= (length slot) 1)
     (list (first slot) unbound t nil))
    (t
     (list (first slot) (second slot)
	   (getf (cddr slot) (kw TYPE) T)
	   (getf (cddr slot) (kw READ-ONLY))))))

(defun param-with-default (param slots)
  (cond
    ((consp param)
     (if (> (length param) 1)
	 param
	 (param-with-default (first param) slots)))
    ((let ((slot (find param slots :key #'slot-name)))
       (when slot
	 (list param (slot-initval slot)))))
    (t
     param)))

(defun lambda-list-with-defaults (lambda-list slots)
  (let ((required t))
    (mapcar
     (lambda (param)
       (cond
	 ;; A lambda list keyword is passed
	 ;; through unchanged.
	 ((member param LAMBDA-LIST-KEYWORDS)
	  (setq required nil)
	  param)
	 ;; A required argument is passed
	 ;; through unchanged.
	 (required
	  param)
	 ;; If a non-required argument
	 ;; doesn't have a default value,
	 ;; supply the default value of the
	 ;; slot.
	 (t
	  (param-with-default param slots))))
     lambda-list)))

(defun slot-name-or-initval (slot constructor)
  (let ((name (slot-name slot))
	(initval (slot-initval slot))
	(lambda-list (second constructor)))
    (if ;(member name lambda-list)
        (some (lambda (parm)
		(or (eq name parm)
		    (and (consp parm) (eq name (car parm)))))
	      lambda-list)
	name
	initval)))


;;; The defstruct macro proper.
(defmacro* DEFSTRUCT (name &rest slots)
  (multiple-value-bind (name options) (if (consp name)
					  (values (first name) (rest name))
					  (values name nil))
    (let ((conc-name		(strcat name "-"))
	  (constructors		nil)
	  (no-constructor	nil)
	  (copier		(symcat "COPY-" name))
	  (include		nil)
	  (initial-offset	nil)
	  (named		nil)
	  (predicate		nil)
	  (print-object		nil)
	  (print-function	nil)
	  (type			nil)
	  (struct-size		nil)
	  (unbound		nil))

      ;; Process structure options.
      (dolist (option options)
	(multiple-value-bind (name args) (if (atom option)
					     (values option nil)
					     (values (first option)
						     (rest option)))
	  (ecase name
	    (:conc-name		(setq conc-name (STRING (or (first args) ""))))
	    (:constructor	(ecase (length args)
				  (0)
				  (1	(if (null (first args))
					    (setq no-constructor t)
					    (push (first args)
						  constructors)))
				  (2	(push args constructors))))
	    (:copier		(setq copier (first args)))
	    (:include		(setq include args))
	    (:initial-offset	(setq initial-offset (first args)))
	    (:named		(setq named t))
	    (:predicate		(setq predicate (first args)))
	    (:print-object	(setq print-object (first args)))
	    (:print-function	(setq print-function (first args)))
	    (:type		(setq type (first args))))))

      ;; Provide a default constructor if appropriate.
      (when (and (null constructors) (not no-constructor))
	(setq constructors (list (symcat "MAKE-" name))))

      ;; Calculate the effective slot list.
      (setq slots (mapcar #'canonicalize-slot slots))
      (when include
	(let ((included-slots (struct-slots (first include))))
	  (dolist (slot (rest include))
	    (setq slot (canonicalize-slot slot))
	    (setq included-slots
		  (nsubstitute
		   slot
		   (find (slot-name slot) included-slots :key #'slot-name)
		   included-slots)))
	  (setq slots (append included-slots slots))))

      ;; Calculate initial-offset and structure size.
      (when (and initial-offset (not type))
	(error ":initial-offset used without :type"))
      (unless initial-offset
	(setq initial-offset 0))
      (setq struct-size (+ initial-offset (length slots)))
      (unless (and type (not named))
	(incf struct-size))

      ;; Provide a default predicate if appropriate.
      (when (and type (not named) predicate)
	(error "error"))
      (unless predicate
	(setq predicate (symcat name "-P")))

      ;; Generate or process the lambda lists of the constructors.
      (setq constructors
	    (mapcar (lambda (constructor)
		      (if (atom constructor)
			  `(,constructor
			    ,(lambda-list-with-defaults
			      `(&KEY ,@(mapcar #'slot-name slots)) slots))
			  `(,(first constructor)
			    ,(lambda-list-with-defaults
			      (second constructor) slots))))
		    constructors))

      ;; Macro expansion.
      `(eval-when (:load-toplevel :execute)

	;; Constructors.
	,@(mapcar
	    (lambda (constructor)
	      `(cl:defun ,@constructor
		 ,(ecase type
		    ((nil)
		     `(let ((object (make-vector ,struct-size ',name)))
		        ,@(let ((index initial-offset))
			    (mapcar (lambda (slot)
				      `(aset object ,(incf index)
					     ,(slot-name-or-initval
					       slot constructor)))
				    slots))
		        object))
		    (vector
		     `(let ((object (MAKE-ARRAY ,struct-size)))
		        ,@(let ((index (1- initial-offset)))
			    `(,@(when named
				  `((setf (AREF object ,(incf index)) ',name)))
			      ,@(mapcar (lambda (slot)
					  `(setf (AREF object ,(incf index))
					         ,(slot-name-or-initval
						   slot constructor)))
					slots)))
		        object))
		    (list
		     `(list ,@(make-list initial-offset nil)
		            ,@(when named (list (list 'quote name)))
		            ,@(mapcar (lambda (slot)
					(slot-name-or-initval
					 slot constructor))
				      slots))))))
	    constructors)

	;; Copier.
	,@(when copier
	   `((defun ,copier (object)
	       (copy-sequence object))))

	;; Predicate.
	,@(when predicate
	    (multiple-value-bind (type-predicate get-type)
		(ecase type
		  ((nil)      (values 'vectorp '(aref object 0)))
		  (vector     (values 'VECTORP `(AREF object ,initial-offset)))
		  (list	      (values 'listp   `(nth ,initial-offset object))))
	      `((defun ,predicate (object)
		  (and (,type-predicate object)
		       (struct-subtypep ,get-type ',name))))))

	;; Remember information about the slots.
	(setf (struct-slots ',name) ',slots)

	;; Register the structure as a subtype of an included structure,
	,@(when include
	    `((add-struct-subtype ',(first include) ',name)))

	;; Define a new class (and type).
	,@(unless type
	   `((ENSURE-CLASS
	       ',name
	       (keyword "METACLASS") 'STRUCTURE-CLASS
	       (keyword "DIRECT-SUPERCLASSES")
	       '(,(if include (first include) 'STRUCTURE-OBJECT)))))

	;; Accessors.
	,@(let ((index initial-offset))
	    (when (or named (not type))
	      (incf index))
	    (mappend
	     (lambda (slot)
	       (multiple-value-bind (getter setter)
		   (ecase type
		     ((nil)
		      (values `(aref object ,index)
			      `(list 'aset object ,index new)))
		     (vector
		      (values `(AREF object ,index)
			      `(list 'setf (list 'AREF object ,index) new)))
		     (list
		      (values `(nth ,index object)
			      `(list 'setf (list 'nth ,index object) new))))
		 (incf index)
		 (let ((name (symcat conc-name (slot-name slot))))
		   `((defun ,name (object) ,getter)
		     ,@(unless (slot-read-only-p slot)
		         `((defsetf ,name (object) (new) ,setter)
		           (DEFSETF ,name (object) (new) ,setter)))))))
	       slots))

	;; Finally, return structure name.
	',name))))

;;; The defstruct macro proper.
(cl:defmacro DEFSTRUCT (name &rest slots)
  (multiple-value-bind (name options) (if (consp name)
					  (values (first name) (rest name))
					  (values name nil))
    (let ((conc-name		(strcat name "-"))
	  (constructors		nil)
	  (no-constructor	nil)
	  (copier		(cl:symcat "COPY-" name))
	  (include		nil)
	  (initial-offset	nil)
	  (named		nil)
	  (predicate		nil)
	  (print-object		nil)
	  (print-function	nil)
	  (type			nil)
	  (struct-size		nil)
	  (unbound		nil))

      ;; Process structure options.
      (dolist (option options)
	(multiple-value-bind (name args) (if (atom option)
					     (values option nil)
					     (values (first option)
						     (rest option)))
	  (cond
	    ((eq name (kw CONC-NAME))
	     (setq conc-name (STRING (or (first args) ""))))
	    ((eq name (kw CONSTRUCTOR))
	     (ecase (length args)
	       (0)
	       (1	(if (null (first args))
			    (setq no-constructor t)
			    (push (first args)
				  constructors)))
	       (2	(push args constructors))))
	    ((eq name (kw COPIER))
	     (setq copier (first args)))
	    ((eq name (kw INCLUDE))
	     (setq include args))
	    ((eq name (kw INITIAL-OFFSET))
	     (setq initial-offset (first args)))
	    ((eq name (kw NAMED))
	     (setq named t))
	    ((eq name (kw PREDICATE))
	     (setq predicate (first args)))
	    ((eq name (kw PRINT-OBJECT))
	     (setq print-object (first args)))
	    ((eq name (kw PRINT-FUNCTION))
	     (setq print-function (first args)))
	    ((eq name (kw TYPE))
	     (setq type (first args)))
	    (t
	     (ERROR "Unknown DEFSTRUCT option: ~S" name)))))

      (unless (SUBTYPEP type '(OR LIST VECTOR))
	(ERROR "Invalid defstruct option :type ~S." type))

      ;; Provide a default constructor if appropriate.
      (when (and (null constructors) (not no-constructor))
	(setq constructors (list (cl:symcat "MAKE-" name))))

      ;; Calculate the effective slot list.
      (setq slots (mapcar #'canonicalize-slot slots))
      (when include
	(let ((included-slots (struct-slots (first include))))
	  (dolist (slot (rest include))
	    (setq slot (canonicalize-slot slot))
	    (setq included-slots
		  (nsubstitute
		   slot
		   (find (slot-name slot) included-slots :key #'slot-name)
		   included-slots)))
	  (setq slots (append included-slots slots))))

      ;; Calculate initial-offset and structure size.
      (when (and initial-offset (not type))
	(ERROR ":initial-offset used without :type"))
      (unless initial-offset
	(setq initial-offset 0))
      (setq struct-size (+ initial-offset (length slots)))
      (unless (and type (not named))
	(incf struct-size))

      ;; Provide a default predicate if appropriate.
      (when (and type (not named) predicate)
	(ERROR "error"))
      (unless predicate
	(setq predicate (cl:symcat name "-P")))

      ;; Generate or process the lambda lists of the constructors.
      (setq constructors
	    (mapcar (lambda (constructor)
		      (if (atom constructor)
			  `(,constructor
			    ,(lambda-list-with-defaults
			      `(&KEY ,@(mapcar #'slot-name slots)) slots))
			  `(,(first constructor)
			    ,(lambda-list-with-defaults
			      (second constructor) slots))))
		    constructors))

      ;; Macro expansion.
      `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))

	;; Constructors.
	,@(mapcar
	    (lambda (constructor)
	      `(DEFUN ,@constructor
		 ,(subtypecase type
		    (nil
		     `(LET ((object (make-vector ,struct-size (QUOTE ,name))))
		        ,@(let ((index initial-offset))
			    (mapcar (lambda (slot)
				      `(aset object ,(incf index)
					     ,(slot-name-or-initval
					       slot constructor)))
				    slots))
		        object))
		    (VECTOR
		     `(LET ((object (MAKE-ARRAY ,struct-size)))
		        ,@(let ((index (1- initial-offset)))
			    `(,@(when named
				  `((SETF (AREF object ,(incf index))
				          (QUOTE ,name))))
			      ,@(mapcar (lambda (slot)
					  `(SETF (AREF object ,(incf index))
					         ,(slot-name-or-initval
						   slot constructor)))
					slots)))
		        object))
		    (LIST
		     `(LIST ,@(make-list initial-offset nil)
		            ,@(when named (list (list 'QUOTE name)))
		            ,@(mapcar (lambda (slot)
					(slot-name-or-initval
					 slot constructor))
				      slots))))))
	    constructors)

	;; Copier.
	,@(when copier
	   `((DEFUN ,copier (object)
	       (copy-sequence object))))

	;; Predicate.
	,@(when predicate
	    (multiple-value-bind (type-predicate get-type)
		(subtypecase type
		  (nil        (values 'vectorp '(aref object 0)))
		  (VECTOR     (values 'VECTORP `(AREF object ,initial-offset)))
		  (LIST	      (values 'LISTP   `(NTH ,initial-offset object))))
	      `((DEFUN ,predicate (object)
		  (AND (,type-predicate object)
		       (struct-subtypep ,get-type (QUOTE ,name)))))))

	;; Remember information about the slots.
	(puthash (QUOTE ,name) (QUOTE (nil ,@slots)) *structure-info*)

	;; Register the structure as a subtype of an included structure,
	,@(when include
	    `((add-struct-subtype (QUOTE ,(first include)) (QUOTE ,name))))

	;; Define a new class (and type).
	,@(unless type
	    `((ENSURE-CLASS
	       (QUOTE ,name)
	       ,(kw METACLASS) (QUOTE STRUCTURE-CLASS)
	       ,(kw DIRECT-SUPERCLASSES)
	       (QUOTE (,(if include (first include) 'STRUCTURE-OBJECT))))))

	;; Accessors.
	,@(let ((index initial-offset))
	    (when (or named (not type))
	      (incf index))
	    (mappend
	     (lambda (slot)
	       (multiple-value-bind (getter setter)
		   (ecase type
		     ((nil)
		      (values `(aref object ,index)
			      `(BACKQUOTE
				(aset (COMMA object) ,index (COMMA new)))))
		     (VECTOR
		      (values `(AREF object ,index)
			      `(BACKQUOTE
				(SETF (AREF (COMMA object) ,index)
				      (COMMA new)))))
		     (LIST
		      (values `(NTH ,index object)
			      `(BACKQUOTE
				(SETF (NTH ,index (COMMA object))
				      (COMMA new))))))
		 (incf index)
		 (let ((name (cl:symcat conc-name (slot-name slot))))
		   `((DEFUN ,name (object) ,getter)
		     ,@(unless (slot-read-only-p slot)
		         `((DEFSETF ,name (object) (new) ,setter)))))))
	       slots))

	;; Finally, return structure name.
	(QUOTE ,name)))))

(defun COPY-STRUCTURE (object)
  (copy-sequence object))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-structures.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-conditions.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 9, Conditions.

(defvar *condition-constructors* (make-hash-table))

(defmacro* DEFINE-CONDITION (name parents slots &rest options)
  ;(with-gensyms (constructor)
  (let ((constructor (symcat "condition-constructor-" name)))
    `(progn
      (DEFSTRUCT (,name
		   (:copier nil)
		   (:constructor ,constructor)
		   ,@(when parents
		       `((:include ,(first parents)))))
	,@slots)
      (puthash ',name #',constructor *condition-constructors*)
      ',name)))

(cl:defmacro DEFINE-CONDITION (name parents slots &rest options)
  `(DEFCLASS ,name ,parents ,slots ,@options))

;; (defun ensure-condition (name parents slots options)
;;   (ENSURE-CLASS name
;; 		(kw DIRECT-SUPERCLASSES) parents
;; 		(kw DIRECT-SLOTS) slots)
;;   (let ((*PACKAGE* *emacs-cl-package*))
;;     (dolist (slot slots)
;;       (when (listp slot)
;; 	(setq slot (first slot)))
;;       (ensure-method (symcat name "-" slot)
;; 		     '(condition)
;; 		     `((SLOT-VALUE condition (QUOTE ,slot)))))))

(DEFINE-CONDITION CONDITION () ())

(DEFINE-CONDITION WARNING (CONDITION) ())

(DEFINE-CONDITION STYLE-WARNING (WARNING) ())

(DEFINE-CONDITION SERIOUS-CONDITION (CONDITION) ())

(DEFINE-CONDITION ERROR (SERIOUS-CONDITION) ())

(DEFINE-CONDITION CELL-ERROR (ERROR) (NAME))

(DEFINE-CONDITION PARSE-ERROR (ERROR) ())

(DEFINE-CONDITION STORAGE-CONDITION (SERIOUS-CONDITION) ())

(cl:defmacro ASSERT (form &optional places datum &rest args)
  (with-gensyms (continue)
    `(DO ()
         (,form)
       (RESTART-BIND ((CONTINUE (LAMBDA () (GO ,continue))))
	 (ERROR ,@(if datum `(,datum ,@args) '((QUOTE ERROR)))))
       ,continue
       (PRINC "\nFix the error already!"))))

(defun condition (datum args default-type)
  (cond
    ((TYPEP datum 'CONDITION)
     datum)
    ((symbolp datum)
     (apply #'MAKE-CONDITION datum args))
    ((STRINGP datum)
     ;; TODO: (kw FORMAT-CONTROL) and (kw FORMAT-ARGUMENTS)
     (MAKE-CONDITION default-type (kw format) datum (kw args) args))
    (t
     (ERROR "Invalid condition designator: ~S ~S."))))

(defun ERROR (datum &rest args)
  (let ((condition (condition datum args 'SIMPLE-ERROR)))
    (SIGNAL condition)
    (INVOKE-DEBUGGER condition)))

(defmacro* restart-bind (bindings &body body)
  `(let ((*restart-alist*
	  (append (list ,@(mapcar (lambda (binding)
				    `(CONS ',(first binding)
				           ,(second binding)))
				  bindings))
		  *restart-alist*)))
     ,@body))

(defun* CERROR (format datum &rest args)
  (restart-bind ((CONTINUE (lambda () (return-from CERROR))))
    (apply #'ERROR datum args)))

(cl:defmacro CHECK-TYPE (place type &optional string)
  `(UNLESS (TYPEP ,place (QUOTE ,type))
     ;; TODO...
     (type-error ,place (QUOTE ,type))))

;; TODO: inherit from SIMPLE-CONDITION
(DEFINE-CONDITION SIMPLE-ERROR (ERROR) (format args))

(defun INVALID-METHOD-ERROR (method format &rest args)
  (apply #'ERROR format args))

(defun METHOD-COMBINATION-ERROR (format &rest args)
  (apply #'ERROR format args))

(DEFVAR *condition-handler-alist* nil)

(defun SIGNAL (datum &rest args)
  (let ((condition (condition datum args 'SIMPLE-CONDITION)))
    (when (TYPEP condition *BREAK-ON-SIGNALS*)
      (INVOKE-DEBUGGER condition))
    (let ((handler (ASSOC condition *condition-handler-alist*
			  (kw TEST) #'TYPEP)))
      (when handler
	(let ((*condition-handler-alist* (cddr handler)))
	  (FUNCALL (cadr handler) condition))))
    nil))

(DEFINE-CONDITION SIMPLE-CONDITION (CONDITION) (format args))

(defun SIMPLE-CONDITION-FORMAT-CONTROL (condition)
  (cond
    ((TYPEP condition 'SIMPLE-CONDITION)  (SIMPLE-CONDITION-format condition))
    ((TYPEP condition 'SIMPLE-ERROR)      (SIMPLE-ERROR-format condition))
    ((TYPEP condition 'SIMPLE-WARNING)    (SIMPLE-WARNING-format condition))
    ((TYPEP condition 'SIMPLE-TYPE-ERROR) (SIMPLE-TYPE-ERROR-format condition))
    (t					  (error "this sucks"))))

(defun SIMPLE-CONDITION-FORMAT-ARGUMENTS (condition)
  (cond
    ((TYPEP condition 'SIMPLE-CONDITION)  (SIMPLE-CONDITION-args condition))
    ((TYPEP condition 'SIMPLE-ERROR)      (SIMPLE-ERROR-args condition))
    ((TYPEP condition 'SIMPLE-WARNING)    (SIMPLE-WARNING-args condition))
    ((TYPEP condition 'SIMPLE-TYPE-ERROR) (SIMPLE-TYPE-ERROR-args condition))
    (t					  (error "this sucks"))))

(defun* WARN (datum &rest args)
  (let ((condition (condition datum args 'SIMPLE-WARNING)))
    (restart-bind ((MUFFLE-WARNING (lambda () (return-from WARN))))
      (SIGNAL condition))
    (if (TYPEP condition 'SIMPLE-WARNING)
	(progn
	  (FORMAT *ERROR-OUTPUT* "~&WARNING: ")
	  (apply #'FORMAT *ERROR-OUTPUT*
		 (SIMPLE-CONDITION-FORMAT-CONTROL condition)
		 (SIMPLE-CONDITION-FORMAT-ARGUMENTS condition)))
	(PRINT condition *ERROR-OUTPUT*))
    nil))

;; TODO: inherit from SIMPLE-CONDITION
(DEFINE-CONDITION SIMPLE-WARNING (WARNING) (format args))

(defun INVOKE-DEBUGGER (condition)
  (let* ((hook *DEBUGGER-HOOK*)
	 (*DEBUGGER-HOOK* nil))
    (when hook
      (FUNCALL hook condition hook))
    (FORMAT T "~&~%Debugger invoked on condition ~A" condition)
    (when (eq (TYPE-OF condition) 'SIMPLE-ERROR)
      (FORMAT T ":~%  ")
      (apply #'FORMAT T (SIMPLE-ERROR-format condition)
	     (SIMPLE-ERROR-args condition)))
    (FORMAT T "~&Available restarts:")
    (do ((restarts (COMPUTE-RESTARTS) (cdr restarts))
	 (i 0 (1+ i)))
	((null restarts))
      (FORMAT T "~&  ~D  ~A" i (RESTART-NAME (car restarts))))
    (FORMAT T "~&Type \"r <n>\" or just \"<n>\" to in invoke a restart,~@
                 and \"b\" to print a backtrace.~%")
    (let ((n -1) c)
      (while (minusp n)
	(message "Debugger command: ")
	(case (setq c (if (eval-when-compile (featurep 'xemacs))
			  (char-to-int (read-char-exclusive))
			  (read-char-exclusive)))
	  ((114 48 49 50 51 52 53 54 55 56 57)
	   (cond
	     ((eq c 114)
	      (setq n (read-minibuffer "Restart number: "))
	      (unless (integerp n)
		(setq n -1)))
	     (t
	      (setq n (- c 48)))))
	  (98
	   (FORMAT T "~&Backtrace: ~%~A~%"
		   (with-output-to-string (backtrace))))
	  (t
	   (message "Invalid debugger command.")
	   (sit-for 1))))
      (INVOKE-RESTART (nth n (COMPUTE-RESTARTS))))))

(defun* BREAK (&optional format &rest args)
  (restart-bind ((CONTINUE (lambda () (return-from BREAK))))
    (debug)))

(DEFVAR *DEBUGGER-HOOK* nil)

(DEFVAR *BREAK-ON-SIGNALS* nil)

(defmacro* HANDLER-BIND (bindings &body body)
  `(let ((*condition-handler-alist*
	  (append (list ,@(mapcar (lambda (binding)
				    `(list* ',(first binding)
				            ,(second binding)
					    *condition-handler-alist*))
				  bindings))
		  *condition-handler-alist*)))
     ,@body))

(cl:defmacro HANDLER-BIND (bindings &body body)
  `(LET ((*condition-handler-alist*
	  (APPEND (LIST ,@(mapcar (lambda (binding)
				    `(LIST* (QUOTE ,(first binding))
				            ,(second binding)
				            *condition-handler-alist*))
				  bindings))
		  *condition-handler-alist*)))
     ,@body))

(cl:defmacro HANDLER-CASE (form &rest clauses)
  (with-gensyms (block)
    `(BLOCK ,block
       (HANDLER-BIND
	   ,(mapcar
	      (lambda (clause)
		(destructuring-bind
		      (typespec (&optional var) &body body) clause
		  (unless var
		    (setq var (gensym)))
		  `(,typespec (LAMBDA (,var)
				(RETURN-FROM ,block (PROGN ,@body))))))
	      clauses)
	 ,form))))

(cl:defmacro IGNORE-ERRORS (&rest forms)
  (with-gensyms (block)
    `(BLOCK ,block
       (HANDLER-BIND ((ERROR (LAMBDA (c) (RETURN-FROM ,block (VALUES nil c)))))
	 ,@forms))))

(defun MAKE-CONDITION (type &rest args)
  (let ((fn (gethash type *condition-constructors*)))
    (if fn
	(APPLY fn args)
	(error "no such condition type"))))

;; (defun MAKE-CONDITION (type &rest args)
;;   (apply #'MAKE-INSTANCE type args))

(DEFSTRUCT (RESTART
	     (:constructor make-restart (NAME handler &OPTIONAL condition))
	     (:predicate restartp))
  NAME handler condition)

(DEFVAR *restart-alist* nil)

(defun COMPUTE-RESTARTS (&optional condition)
  (mapcar (lambda (cons) (make-restart (car cons) (cdr cons)))
	  *restart-alist*))

(defun FIND-RESTART (restart &optional condition)
  ;; TODO: consider condition
  (cond
    ((restartp restart)		restart)
    ((null restart)		(error "TODO"))
    ((symbolp restart)		(let ((cons (assq restart *restart-alist*)))
				  (when cons
				    (make-restart restart (cdr cons)))))
    (t				(type-error restart '(OR RESTART SYMBOL)))))

(defun INVOKE-RESTART (restart-designator &rest args)
  (let ((restart (FIND-RESTART restart-designator)))
    (if restart
	(APPLY (RESTART-handler restart) args)
	(ERROR 'CONTROL-ERROR))))

;;; TODO: INVOKE-RESTART-INTERACTIVELY

(cl:defmacro RESTART-BIND (bindings &body forms)
  `(LET ((*restart-alist*
	  (APPEND (LIST ,@(mapcar (lambda (binding)
				    `(CONS (QUOTE ,(first binding))
				           ,(second binding)))
				  bindings))
		  *restart-alist*)))
     ,@forms))

;;; TODO: RESTART-CASE

;;; RESTART-NAME defined by defstruct.

;;; TODO: WITH-CONDITION-RESTARTS

;;; TODO: WITH-SIMPLE-RESTART
; (cl:defmacro WITH-SIMPLE-RESTART ((name format &rest args)
; 				  &body body)
;   `(RESTART-CASE (PROGN ,@body)
;      (,name ()
;        :report (LAMBDA (stream) (FORMAT stream ,format ,@args))
;        (cl:values nil T))))

(defun ABORT (&optional condition)
  (INVOKE-RESTART 'ABORT))

(defun CONTINUE (&optional condition)
  (let ((restart (FIND-RESTART 'CONTINUE)))
    (when restart
      (INVOKE-RESTART restart))))

(defun MUFFLE-WARNING (&optional condition)
  (INVOKE-RESTART 'MUFFLE-WARNING))

(defun STORE-VALUE (value &optional condition)
  (let ((restart (FIND-RESTART 'STORE-VALUE)))
    (when restart
      (INVOKE-RESTART restart value))))

(defun USE-VALUE (value &optional condition)
  (let ((restart (FIND-RESTART 'USE-VALUE)))
    (when restart
      (INVOKE-RESTART restart value))))


(DEFINE-CONDITION PROGRAM-ERROR (ERROR) ())
(DEFINE-CONDITION CONTROL-ERROR (ERROR) ())

(DEFINE-CONDITION TYPE-ERROR (ERROR) (DATUM EXPECTED-TYPE))
;; TODO: inherit from SIMPLE-CONDITION
(DEFINE-CONDITION SIMPLE-TYPE-ERROR (TYPE-ERROR) (format args))

(DEFINE-CONDITION UNBOUND-VARIABLE (CELL-ERROR) ())
(DEFINE-CONDITION UNDEFINED-FUNCTION (CELL-ERROR) ())
(DEFINE-CONDITION UNBOUND-SLOT (CELL-ERROR) (INSTANCE))

(DEFINE-CONDITION PACKAGE-ERROR (ERROR) (PACKAGE))

(DEFINE-CONDITION STREAM-ERROR (ERROR) (STREAM))
(DEFINE-CONDITION END-OF-FILE (STREAM-ERROR) ())
(DEFINE-CONDITION READER-ERROR (STREAM-ERROR) ()) ;Also PARSE-ERROR.

(DEFINE-CONDITION FILE-ERROR (ERROR) (PATHNAME))

(DEFINE-CONDITION ARITHMETIC-ERROR (ERROR) (OPERATION OPERANDS))
(DEFINE-CONDITION DIVISION-BY-ZERO (ARITHMETIC-ERROR) ())
(DEFINE-CONDITION FLOATING-POINT-INVALID-OPERATION (ARITHMETIC-ERROR) ())
(DEFINE-CONDITION FLOATING-POINT-INEXACT (ARITHMETIC-ERROR) ())
(DEFINE-CONDITION FLOATING-POINT-OVERFLOW (ARITHMETIC-ERROR) ())
(DEFINE-CONDITION FLOATING-POINT-UNDERFLOW (ARITHMETIC-ERROR) ())

(DEFINE-CONDITION PRINT-NOT-READABLE (ERROR) (OBJECT))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-conditions.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-environment.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 25, Environment.

(IN-PACKAGE "EMACS-CL")

(defun leap-year-p (year)
  (and (zerop (REM year 4))
       (or (not (zerop (REM year 100)))
	   (zerop (REM year 400)))))

(defun days-to-month (month year)
  (+ (ecase month
       (1	0)
       (2	31)
       (3	59)
       (4	90)
       (5	120)
       (6	151)
       (7	181)
       (8	212)
       (9	243)
       (10	273)
       (11	304)
       (12	334))
     (if (and (leap-year-p year) (> month 2)) 1 0)))

(defun days-to-year (year)
  (do* ((days 0)
	(y 1900 (1+ y)))
       ((EQL y year)
	days)
    (incf days (if (leap-year-p y) 366 365))))

(defun DECODE-UNIVERSAL-TIME (universal-time &optional time-zone)
  (do ((next-year 1900 (1+ next-year))
       (year 1900 next-year)
       (next-time universal-time
		  (binary- next-time
			   (cl:* 3600 24 (if (leap-year-p next-year) 366 365))))
       (time universal-time next-time))
      ((MINUSP next-time)
       (do ((month 1 (1+ month)))
	   ((or (eq month 13)
		(> (* 3600 24 (days-to-month month year)) time))
	    (decf month)
	    (decf time (* 3600 24 (days-to-month month year)))
	    (MULTIPLE-VALUE-BIND (date time) (TRUNCATE time (* 3600 24))
	      (MULTIPLE-VALUE-BIND (hour time) (TRUNCATE time 3600)
		(MULTIPLE-VALUE-BIND (minute second) (TRUNCATE time 60)
		  ;; TODO:
		  (let (day daylight-p zone)
		    (cl:values second minute hour (1+ date) month year
			       day daylight-p zone))))))))))

(defun encode-days (date month year)
  (+ date -1 (days-to-month month year) (days-to-year year)))

(defun ENCODE-UNIVERSAL-TIME (second minute hour date month year
			      &optional time-zone)
  (let* ((days (encode-days date month year))
	 (hours (binary+ hour (binary* 24 days)))
	 (minutes (binary+ minute (binary* 60 hours))))
    (cl:+ second
	  (binary* 60 minutes)
	  (if time-zone (* 3600 time-zone) 0))))

;;; Difference between Unix time and Common Lisp universal time is 70 years.
(defconst universal-time-offset (ENCODE-UNIVERSAL-TIME 0 0 0 1 1 1970 0))

(defun GET-UNIVERSAL-TIME ()
  (let* ((time (current-time))
	 (high (first time))
	 (low (second time)))
    (cl:+ (binary* high 65536) low universal-time-offset)))

(defun GET-DECODED-TIME ()
  (DECODE-UNIVERSAL-TIME (GET-UNIVERSAL-TIME)))

(defun SLEEP (seconds)
  (sleep-for (if (or (integerp seconds) (floatp seconds))
		 seconds
		 (FLOAT seconds)))
  nil)

(defun APROPOS (string &optional package)
  (dolist (symbol (APROPOS-LIST string package))
    (PRINT symbol))
  (cl:values))

(defun APROPOS-LIST (string-designator &optional package)
  (let ((string (STRING string-designator))
	(packages (if (null package)
		      *all-packages*
		      (list (FIND-PACKAGE package))))
	(result nil))
    (dolist (p packages)
      (DO-SYMBOLS (symbol p)
	(when (SEARCH string (SYMBOL-NAME symbol))
	  (push symbol result))))
    result))

(defun DESCRIBE (object &optional stream-designator)
  (let ((stream (output-stream stream-designator)))
    (DESCRIBE-OBJECT object stream)
    (cl:values)))

;;; TODO: DESCRIBE-OBJECT should be a generic function.
(defun DESCRIBE-OBJECT (object stream)
  (cond
    ((symbolp object)
     (cond
       ((SYMBOL-PACKAGE object)
	(FORMAT stream
		"~&~S is an ~:[internal~;external~] symbol in ~S."
		object (external-symbol-p object) (SYMBOL-PACKAGE object))
	(FORMAT stream "~%It is also accessible in packages ~{~A~^, ~}"
		(let ((name (SYMBOL-NAME object))
		      (home (SYMBOL-PACKAGE object))
		      (packages nil))
		  (dolist (p *all-packages* packages)
		    (MULTIPLE-VALUE-BIND (sym found) (FIND-SYMBOL name p)
		      (when (and found (eq sym object) (not (eq p home)))
			(push (PACKAGE-NAME p) packages)))))))
       (t
	(FORMAT stream "~&~S is an uninterned symbol." object)))
     (when (SPECIAL-OPERATOR-P object)
       (FORMAT stream "~%It is a special operator."))
     (when (boundp object)
       (FORMAT stream "~%~:[Constant ~]Value: ~S"
	       (not (CONSTANTP object)) (symbol-value object)))
     (cond
       ((MACRO-FUNCTION object)
	(FORMAT stream "~%Macro Function: ~S" (MACRO-FUNCTION object)))
       ((fboundp object)
	(FORMAT stream "~%Function: ~S" (SYMBOL-FUNCTION object))))
     (when (COMPILER-MACRO-FUNCTION object)
       (FORMAT stream "~%Compiler Macro Function: ~S"
	       (COMPILER-MACRO-FUNCTION object)))
     (when (symbol-plist object)
       (FORMAT stream "~%Property list: ~S" (symbol-plist object))))
    ((integerp object)
     (FORMAT stream "~&~A is a fixnum." object))
    ((bignump object)
     (FORMAT stream "~&~A is a bignum." object)
     (FORMAT stream "~%It is encoded in ~D fixnums." (1- (length object))))
    ((ratiop object)
     (FORMAT stream "~&~A is a ratio." object)
     (FORMAT stream "~%Numerator: ~D" (NUMERATOR object))
     (FORMAT stream "~%Denominator: ~D" (DENOMINATOR object)))
    ((floatp object)
     ;; TODO: better description
     (FORMAT stream "~&~A is a float." object))
    ((COMPLEXP object)
     (FORMAT stream "~&~A is a complex." object)
     (FORMAT stream "~%Real part: ~A" (REALPART object))
     (FORMAT stream "~%Imaginary part: ~A" (IMAGPART object)))
    ((INTERPRETED-FUNCTION-P object)
     (FORMAT stream "~&~A is an interpreted function." object)
     (FORMAT stream "~%Lambda expression: ~S"
	     (FUNCTION-LAMBDA-EXPRESSION object))
     (when (function-name object)
       (FORMAT stream "~%Name: ~A" (function-name object))))
    ((byte-code-function-p object)
     (FORMAT stream "~&~A is a byte-compiled function." object)
     (when (function-name object)
       (FORMAT stream "~%Name: ~A" (function-name object)))
     (FORMAT stream "~%Lambda list: ~A" (aref object 0))
     (FORMAT stream "~%Byte code: ...")
     (when (> (length object) 2)
       (FORMAT stream "~%Constants: ..."))
     (when (> (length object) 3)
       (FORMAT stream "~%Maximum stack size: ~D" (aref object 3)))
     (when (documentation object)
       (FORMAT stream "~%Documentation: ~A" (documentation object)))
     (when (> (length object) 5)
       (FORMAT stream "~%Interactive: ~S" (aref object 5))))
    ((subrp object)
     (FORMAT stream "~&~A is a built-in subroutine." object)
     (FORMAT stream "~%Name: ~A" (function-name object))
     (when (documentation object)
       (FORMAT stream "~%Documentation: ~A" (documentation object))))
    ((PACKAGEP object)
     (FORMAT stream "~&~A is a package." object)
     (FORMAT stream "~%Nicknames:~{ ~S~}" (PACKAGE-NICKNAMES object))
     (let* ((ext (length (package-exported object)))
	    (int (- (hash-table-count (package-table object)) ext)))
       (FORMAT stream "~%Internal symbols: ~D" int)
       (FORMAT stream "~%External symbols: ~D" ext))
     (FORMAT stream "~%Shadowing symbols: ~D"
	     (length (PACKAGE-SHADOWING-SYMBOLS object)))
     (FORMAT stream "~%Use list: ~S" (PACKAGE-USE-LIST object))
     (FORMAT stream "~%Used by list: ~S" (PACKAGE-USED-BY-LIST object)))
    ((BIT-VECTOR-P object)
     (FORMAT stream "~&~S is a bit vector of length ~D."
	     object (LENGTH object))
     (when (ARRAY-HAS-FILL-POINTER-P object)
       (FORMAT stream "~%Fill pointer: ~A" (FILL-POINTER object))))
    ((STRINGP object)
     (FORMAT stream "~&~S is a string of length ~D." object (LENGTH object))
     (when (ARRAY-HAS-FILL-POINTER-P object)
       (FORMAT stream "~%Fill pointer: ~A" (FILL-POINTER object))))
    ((VECTORP object)
     (FORMAT stream "~&~S is a vector of length ~D." object (LENGTH object))
     (when (vectorp object)
       (FORMAT stream "~%Fill pointer: ~A" (FILL-POINTER object))))
    ((ARRAYP object)
     (FORMAT stream "~&~S is an array with dimensions ~A."
	     object (ARRAY-DIMENSIONS object))
     (FORMAT stream "~@[~%It is specialized to hold ~A.~]"
	     (case (aref object 0)
	       (bit-array "bits")
	       (char-array "characters"))))
    ((consp object)
     (catch 'done
       (HANDLER-BIND
	   ((ERROR (lambda (c)
		     (if (atom (cdr object))
			 (FORMAT stream "~&~S is a cons cell." object)
			 (FORMAT stream "~&~S is a dotted list." object))
		     (throw 'done nil))))
	 (if (LIST-LENGTH object)
	     (FORMAT stream "~&~S is a list of length ~D."
		     object (length object))
	     (FORMAT stream "~&~S is a circular list." object)))))
    ((vectorp object)
     ;; TODO:
     (FORMAT stream "~&FIXME: This is a fall-back description.")
     (FORMAT stream "~%~A is an instance of ~S" object (TYPE-OF object))
     (FORMAT stream "~%The values of its slots are:")
     (do* ((len (length object))
	   (i 1 (1+ i)))
	  ((eq i len))
       (FORMAT stream "~%  Slot ~D: ~S" i (aref object i))))
    (t
     (FORMAT stream "~&Don't know how to describe ~A" object))))

(defvar *traced-functions* nil)

(defun traced-fn (name)
  `(lambda (&rest args)
     (PRINT (format ,(format "Trace: %s %%s" name) args) *TRACE-OUTPUT*)
     (APPLY ,(FDEFINITION name) args)))

(defun trace-fn (name)
  (unless (assoc name *traced-functions*)
    (push (cons name (FDEFINITION name)) *traced-functions*)
    (setf (FDEFINITION name) (traced-fn name))))

(cl:defmacro TRACE (&rest names)
  (if (null names)
      `(QUOTE ,(mapcar #'car *traced-functions*))
      `(DOLIST (name (QUOTE ,names))
	 (trace-fn name))))
      
(defun untrace-fn (name)
  (let ((fn (assoc name *traced-functions*)))
    (when fn
      (setq *traced-functions* (delq fn *traced-functions*))
      (setf (FDEFINITION name) (cdr fn)))))

(cl:defmacro UNTRACE (&rest names)
  `(MAPC (FUNCTION untrace-fn) ,(if (null names) '(TRACE) `(QUOTE ,names))))

(cl:defmacro STEP (form)
  ;; TODO: stepping
  `(LET () ,form))

(cl:defmacro TIME (form)
  (with-gensyms (start val end time)
    `(LET* ((,start (GET-INTERNAL-REAL-TIME))
	    (,val (MULTIPLE-VALUE-LIST ,form))
	    (,end (GET-INTERNAL-REAL-TIME))
	    (,time (,(INTERN "-" "CL") ,end ,start)))
       (FORMAT *TRACE-OUTPUT* "~&Elapsed real time: ~A seconds"
	       (,(INTERN "*" "CL")
		(ROUND (,(INTERN "*" "CL") ,time
			 ,(/ 1000.0 INTERNAL-TIME-UNITS-PER-SECOND)))
		0.001))
       (VALUES-LIST ,val))))

(DEFCONSTANT INTERNAL-TIME-UNITS-PER-SECOND 1000000)

(defun GET-INTERNAL-REAL-TIME ()
  (let* ((time (current-time))
	 (high (first time))
	 (low (second time))
	 (microsec (third time)))
    (binary+ (binary* (binary+ (binary* high 65536) low) 1000000) microsec)))

;;; TODO: Function GET-INTERNAL-RUN-TIME
(if (fboundp 'get-internal-run-time)
    (defun GET-INTERNAL-RUN-TIME ()
      (let* ((time (get-internal-run-time))
	     (high (first time))
	     (low (second time))
	     (microsec (third time)))
	(binary+ (binary* (binary+ (binary* high 65536) low) 1000000)
		 microsec)))
    (defun GET-INTERNAL-RUN-TIME ()
      (GET-INTERNAL-REAL-TIME)))

(defun DISASSEMBLE (fn)
  (when (or (symbolp fn) (setf-name-p fn))
    (setq fn (FDEFINITION fn)))
  (when (INTERPRETED-FUNCTION-P fn)
    (setq fn (COMPILE nil fn)))
  (disassemble fn)
  nil)

;;; TODO: Standard Generic Function DOCUMENTATION, (SETF DOCUMENTATION)

(cl:defun ROOM (&OPTIONAL (x (kw DEFAULT)))
  (let* ((info (garbage-collect))
         (foo '("conses" "symbols" "misc" "string chars"
                "vector slots" "floats" "intervals" "strings"))
         (cons-info (first info))
         (sym-info (second info))
         (misc-info (third info))
         (used-string-chars (fourth info))
         (used-vector-slots (fifth info))
         (float-info (sixth info))
         (interval-info (seventh info))
         (string-info (eighth info)))
    (cond
      ((eq x nil))
      ((eq x (kw DEFAULT))
       (do ((i info (cdr i))
            (j foo (cdr j)))
           ((null i) nil)
         (PRINC (format "\nUsed %-13s:  " (car j)))
         (cond
           ((null (car i)))
           ((atom (car i))
            (PRINC (format "%7d." (car i))))
           (t
            (PRINC (format "%7d, free %-10s: %7d"
                           (caar i) (car j) (cdar i)))))))
      ((eq x 'T)
       (ROOM)
       (PRINC "\nConsed so far:")
       (PRINC (format "\n%d conses," cons-cells-consed))
       (PRINC (format "\n%d floats," floats-consed))
       (PRINC (format "\n%d vector cells," vector-cells-consed))
       (PRINC (format "\n%d symbols," symbols-consed))
       (PRINC (format "\n%d string chars," string-chars-consed))
       (PRINC (format "\n%d misc objects," misc-objects-consed))
       (PRINC (format "\n%d intervals" intervals-consed))
       (if (boundp 'strings-consed)
	   ;; Use symbol-value to shut up compiler warnings.
	   (PRINC (format "\n%d strings." (symbol-value 'strings-consed)))
	   (PRINC ".")))
      (t
       (type-error x `(OR BOOLEAN (EQL ,(kw DEFAULT))))))))

(defun ED (&optional x)
  (cond
    ((null x)
     (switch-to-buffer (generate-new-buffer "*ED*")))
    ((or (PATHNAMEP x) (STRINGP x))
     (find-file (NAMESTRING (PATHNAME x))))
    ((or (SYMBOLP x) (setf-name-p x))
     (find-tag (prin1-to-string x)))
    (t
     (type-error x '(OR NULL PATHNAME STRING SYMBOL
		        (CONS (EQ SETF) (CONS SYMBOL NULL)))))))

;;; TODO: Function INSPECT

;;; TODO: Function DRIBBLE

(defvar cl:- nil)
(defvar cl:+ nil)
(defvar ++ nil)
(defvar +++ nil)
(defvar cl:* nil)
(defvar ** nil)
(defvar *** nil)
(defvar cl:/ nil)
(defvar // nil)
(defvar /// nil)

(defun LISP-IMPLEMENTATION-TYPE ()
  "Emacs Common Lisp")

(defun LISP-IMPLEMENTATION-VERSION ()
  "0.8")

(defun SHORT-SITE-NAME ()
  nil)

(defun LONG-SITE-NAME ()
  nil)

(defun MACHINE-INSTANCE ()
  (system-name))

(defun MACHINE-TYPE ()
  (subseq system-configuration 0 (position 45 system-configuration)))

(defun MACHINE-VERSION ()
  nil)

(defun SOFTWARE-TYPE ()
  (STRING system-type))

(defun SOFTWARE-VERSION ()
  nil)

(defun USER-HOMEDIR-PATHNAME (&optional host)
  ;; TODO: look at host
  (PATHNAME "~/"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-environment.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-objects.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 7, Objects.

;; Important CLOS classes:
;;   CLASS
;;	BUILT-IN-CLASS, STANDARD-CLASS, STANDARD-OBJECT, STRUCTURE-CLASS,
;;	STRUCTURE-OBJECT
;;   GENERIC-FUNCTION
;;	STANDARD-GENERIC-FUNCTION
;;   METHOD
;;	STANDARD-METHOD
;;   METHOD-COMBINATION

(defmacro define-class-layout (type slots)
  (let ((index -1))
    `(progn
       ,@(mapcar (lambda (slot)
		   `(defmacro ,(symcat type "-" slot) (class)
		      (list 'aref (list 'aref class 1) ,(incf index))))
		 slots)
       ',type)))

;;; Potential class slots:
;; default-initargs direct-default-initargs direct-slots
;; direct-subclasses direct-superclasses finalized-p name
;; precedence-list prototype slots
(define-class-layout class (name superclasses slots default-initargs
			    precedence-list finalized-p subclasses
			    direct-slots direct-default-initargs))

(defun make-class (metaclass name superclasses slots)
  (when (vectorp metaclass)
    (setq metaclass (class-name metaclass)))
  (vector metaclass (vector name superclasses slots
			    nil nil nil nil nil nil nil)))

(let ((fn (vector)))
  (defconst +gf-template+
    (byte-compile `(lambda (&rest args) (apply ,fn args))))
  (defconst +gf-template-fn-pos+
    (position fn (cfref +gf-template+ 2))))

;;; Potential generic-function slots:
;; name lambda-list declarations
;; methods method-class method-combination
;; arguments-precedence-order
(defun make-gf (name lambda-list)
  (let* ((placeholder (list))
 	 (fn (COMPILE nil `(LAMBDA (&REST args)
 			     (APPLY (FUNCTION NO-APPLICABLE-METHOD)
 			            ,placeholder args))))
	 (index (position placeholder (cfref fn 2)))
	 (gf (make-byte-code (cfref +gf-template+ 0)
			     (cfref +gf-template+ 1)
			     (copy-sequence (cfref +gf-template+ 2))
			     (cfref +gf-template+ 3))))
    (SET-FUNCALLABLE-INSTANCE-FUNCTION gf fn)
    (aset (cfref fn 2) index gf)
    (setf (gethash gf *funcallable-objects*)
	  (cons 'STANDARD-GENERIC-FUNCTION
		(vector name lambda-list nil)))
    (cl:values gf)))

(defvar *funcallable-objects* (make-hash-table :test #'eq))

(defun gf-slots (gf)
  (cdr (gethash gf *funcallable-objects*)))

(defmacro gf-name (gf)		`(aref (gf-slots ,gf) 0))
(defmacro gf-lambda-list (gf)	`(aref (gf-slots ,gf) 1))
(defmacro gf-methods (gf)	`(aref (gf-slots ,gf) 2))

;;; MOP
(defun GENERIC-FUNCTION-LAMBDA-LIST (gf)
  (gf-lambda-list gf))

;;; MOP
(defun GENERIC-FUNCTION-METHODS (gf)
  (gf-methods gf))

(define-class-layout method (lambda-list body function generic-function
			     specializers qualifiers environment))

(defun make-method (lambda-list specializers body)
  (vector 'STANDARD-METHOD
	  (vector lambda-list body nil nil specializers nil nil)))

;;; These are redefined later.
(defun FIND-CLASS (name &optional errorp env) nil)
(defun CLASS-OF (object) nil)
(defvar +built-in-class+ t)

;;; MOP
(defun ADD-DIRECT-SUBCLASS (superclass subclass)
  ;; Nothing to do, as long as CLASS-DIRECT-SUBCLASSES does all the work.
  nil)

;;; MOP
(defun ENSURE-CLASS (name &rest keys)
  (apply #'ENSURE-CLASS-USING-CLASS (FIND-CLASS name nil) name keys))

;;; MOP
(cl:defun ENSURE-CLASS-USING-CLASS (class name &KEY
				    DIRECT-SUPERCLASSES
				    DIRECT-SLOTS
				    DIRECT-DEFAULT-INITARGS
				    (METACLASS 'STANDARD-CLASS))
  (when (some #'built-in-class-p DIRECT-SUPERCLASSES)
    (ERROR "Can't make subclass of a built-in class."))
  (unless (symbolp METACLASS)
    (setq METACLASS (CLASS-NAME METACLASS)))
  (unless (eq METACLASS 'BUILT-IN-CLASS)
    (setq *subtypep-objects* (nconc *subtypep-objects* (list (vector name)))))
  (case METACLASS
    (BUILT-IN-CLASS)	;Do nothing.
    (STANDARD-CLASS
     (ensure-type name
		  (byte-compile
		   `(lambda (object env)
		      (and (vectorp object)
			   (subclassp (CLASS-OF object)
				      (FIND-CLASS ',name)))))))
    (STRUCTURE-CLASS
     (ensure-type name
		  (byte-compile
		   `(lambda (obj env)
		     (and (vectorp obj)
		          (struct-subtypep (aref obj 0) ',name)))))))
  (let ((class (make-class METACLASS name DIRECT-SUPERCLASSES DIRECT-SLOTS)))
    (puthash name class *classes*)
    class))

;;; MOP
(defun CLASS-DIRECT-SUPERCLASSES (class)
  (let ((classes (class-superclasses class)))
    (cond
      ((eq class +t-class+)
			nil)
      ((null classes)	(list +t-class+))
      (t		(mapcar #'FIND-CLASS (class-superclasses class))))))

;;; MOP
(defun CLASS-DIRECT-SUBCLASSES (class)
  (let ((classes nil))
    (maphash (lambda (name class2)
	       (when (or (and (eq class +t-class+)
			      (not (eq class2 +t-class+))
			      (null (class-superclasses class2)))
			 (memq (class-name class) (class-superclasses class2)))
		 (pushnew class2 classes)))
	     *classes*)
    classes))

;;; MOP
(defun CLASS-SLOTS (class)
  (class-slots class))

(defun make-built-in-class (name &optional superclasses)
  (ENSURE-CLASS name (kw METACLASS) 'BUILT-IN-CLASS
		     (kw DIRECT-SUPERCLASSES) superclasses))

(defun make-standard-class (name &optional superclasses slots)
  (when (null superclasses)
    (setq superclasses
	  (if (eq name 'STANDARD-OBJECT) '(T) '(STANDARD-OBJECT))))
  (ENSURE-CLASS name (kw DIRECT-SUPERCLASSES) superclasses
		     (kw DIRECT-SLOTS) slots))

(defun make-structure-class (name &optional superclasses)
  (ENSURE-CLASS name (kw METACLASS) 'STRUCTURE-CLASS
		     (kw DIRECT-SUPERCLASSES) superclasses))

(defun make-funcallable-standard-class (name superclasses)
  (ENSURE-CLASS name (kw METACLASS) 'FUNCALLABLE-STANDARD-CLASS
		     (kw DIRECT-SUPERCLASSES) superclasses))

(defconst +unbound+ (make-symbol "UNBOUND"))

;;; Figure 4-8. Classes that correspond to pre-defined type specifiers
;;; These are required to exist, and most may be built-in.
;; arithmetic-error                 GENERIC-FUNCTION   simple-error
;; array                            hash-table         simple-type-error
;; bit-vector                       integer            simple-warning
;; broadcast-stream                 list               STANDARD-CLASS
;; BUILT-IN-CLASS                   logical-pathname  STANDARD-GENERIC-FUNCTION
;; cell-error                       METHOD             STANDARD-METHOD
;; character                        METHOD-COMBINATION STANDARD-OBJECT
;; CLASS                            null               storage-condition
;; complex                          number             stream
;; concatenated-stream              package            stream-error
;; condition                        package-error      string
;; cons                             parse-error        string-stream
;; control-error                    pathname           STRUCTURE-CLASS
;; division-by-zero                 print-not-readable STRUCTURE-OBJECT
;; echo-stream                      program-error      style-warning
;; end-of-file                      random-state       symbol
;; error                            ratio              synonym-stream
;; file-error                       rational           t
;; file-stream                      reader-error       two-way-stream
;; float                            readtable          type-error
;; floating-point-inexact           real               unbound-slot
;; floating-point-invalid-operation restart            unbound-variable
;; floating-point-overflow          sequence           undefined-function
;; floating-point-underflow         serious-condition  vector
;; function                         simple-condition   warning

;;; These classes are optional, and may be built-in.
;; atom			nil?
;; base-char		short-float
;; base-string		signed-byte
;; bignum		simple-array
;; bit			simple-base-string
;; compiled-function	simple-bit-vector
;; double-float		simple-string
;; extended-char	simple-vector
;; fixnum		single-float
;; keyword		standard-char
;; long-float		unsigned-byte

(defun built-in-class-p (class) nil)
(defvar *classes* (make-hash-table :test #'eq))
(defconst +array-class+	       (make-built-in-class 'ARRAY))
(defconst +bit-vector-class+   (make-built-in-class 'BIT-VECTOR '(VECTOR)))
(defconst +built-in-class+     (make-standard-class 'BUILT-IN-CLASS '(CLASS)))
(defconst +character-class+    (make-built-in-class 'CHARACTER))
(defconst +class-class+	       (make-standard-class 'CLASS '(SPECIALIZER)))
(defconst +complex-class+      (make-built-in-class 'COMPLEX '(NUMBER)))
;(defconst +condition-class+   (make-standard-class 'CONDITION))
(defconst +cons-class+	       (make-built-in-class 'CONS '(LIST)))
(defconst +eql-specializer-class+
			       (make-standard-class 'EQL-SPECIALIZER
						    '(SPECIALIZER)))
(defconst +float-class+	       (make-built-in-class 'FLOAT '(REAL)))
(defconst +funcallable-standard-class+
			       (make-standard-class 'FUNCALLABLE-STANDARD-CLASS
						    '(CLASS)))
(defconst +funcallable-standard-object+
			       (make-standard-class
				'FUNCALLABLE-STANDARD-OBJECT
				'(STANDARD-OBJECT FUNCTION)))
(defconst +function-class+     (make-built-in-class 'FUNCTION))
(defconst +generic-function-class+
			       (make-funcallable-standard-class
				'GENERIC-FUNCTION
				'(METAOBJECT FUNCALLABLE-STANDARD-OBJECT)))
(defconst +hash-table-class+   (make-built-in-class 'HASH-TABLE))
(defconst +integer-class+      (make-built-in-class 'INTEGER '(RATIONAL)))
(defconst +list-class+	       (make-built-in-class 'LIST '(SEQUENCE)))
(defconst +logical-pathname-class+
			       (make-built-in-class 'LOGICAL-PATHNAME
						    '(PATHNAME)))
(defconst +metaobject-class+   (make-standard-class 'METAOBJECT))
(defconst +method-class+       (make-standard-class 'METHOD '(METAOBJECT)))
(defconst +method-combination-class+
			       (make-standard-class 'METHOD-COMBINATION
						    '(METAOBJECT)))
(defconst +null-class+	       (make-built-in-class 'NULL '(SYMBOL LIST)))
(defconst +number-class+       (make-built-in-class 'NUMBER))
(defconst +package-class+      (make-built-in-class 'PACKAGE))
(defconst +pathname-class+     (make-built-in-class 'PATHNAME))
(defconst +ratio-class+	       (make-built-in-class 'RATIO '(RATIONAL)))
(defconst +rational-class+     (make-built-in-class 'RATIONAL '(REAL)))
(defconst +real-class+	       (make-built-in-class 'REAL '(NUMBER)))
(defconst +readtable-class+    (make-built-in-class 'READTABLE))
;(defconst +restart-class+     (make-standard-class 'RESTART))
(defconst +sequence-class+     (make-built-in-class 'SEQUENCE))
(defconst +slot-definition-class+
			       (make-standard-class 'SLOT-DEFINITION
						    '(METAOBJECT)))
(defconst +specializer-class+  (make-standard-class 'SPECIALIZER
						    '(METAOBJECT)))
(defconst +standard-class+     (make-standard-class 'STANDARD-CLASS '(CLASS)))
(defconst +standard-generic-function-class+
			       (make-funcallable-standard-class
				'STANDARD-GENERIC-FUNCTION
				'(GENERIC-FUNCTION)))
(defconst +standard-method+    (make-standard-class 'STANDARD-METHOD
						    '(METHOD)))
(defconst +standard-accessor-method+
			       (make-standard-class 'STANDARD-ACCESSOR-METHOD
						    '(STANDARD-METHOD)))
(defconst +standard-reader-method+
			       (make-standard-class
				'STANDARD-READER-METHOD
				'(STANDARD-ACCESSOR-METHOD)))
(defconst +standard-writer-method+
			       (make-standard-class
				'STANDARD-WRITER-METHOD
				'(STANDARD-ACCESSOR-METHOD)))
(defconst +standard-object+    (make-standard-class 'STANDARD-OBJECT))
;(defconst +stream-class+      (make-standard-class 'STREAM))
(defconst +string-class+       (make-built-in-class 'STRING '(VECTOR)))
(defconst +structure-class+    (make-standard-class 'STRUCTURE-CLASS '(CLASS)))
(defconst +structure-object+   (make-structure-class 'STRUCTURE-OBJECT))
(defconst +symbol-class+       (make-built-in-class 'SYMBOL))
(defconst +t-class+	       (make-built-in-class 'T))
(defconst +vector-class+       (make-built-in-class 'VECTOR '(ARRAY SEQUENCE)))
(dolist (args *initial-classes*)
  (apply #'ENSURE-CLASS args))
(defun built-in-class-p (class)
  (when (symbolp class)
    (setq class (FIND-CLASS class)))
  (eq (CLASS-OF class) +built-in-class+))

(defun funcallable-object-p (object)
  (and (COMPILED-FUNCTION-P object)
       (gethash object *funcallable-objects*)))

(defun generic-function-p (object &optional env)
  (and (funcallable-object-p object)
       (subclassp (CLASS-OF object) +generic-function-class+)))

(ensure-type 'GENERIC-FUNCTION #'generic-function-p)

;;; TODO: Standard Generic Function FUNCTION-KEYWORDS

;;; MOP
(cl:defun ENSURE-GENERIC-FUNCTION-USING-CLASS (gf name &KEY LAMBDA-LIST)
  (cond
    ((null gf)
     (setq gf (make-gf name LAMBDA-LIST))
     (cl-defun name gf))
    (*DEBUG-IO*
     (FRESH-LINE *DEBUG-IO*)
     (WRITE-STRING "Redefinition of generic function " *DEBUG-IO*)
     (WRITE-STRING (SYMBOL-NAME (gf-name gf)) *DEBUG-IO*)
     (WRITE-STRING " not properly implemented." *DEBUG-IO*)))
  gf)

(defun gf-or-error (name)
  (when (FBOUNDP name)
    (let ((fn (FDEFINITION name)))
      (if (generic-function-p fn)
	  fn
	  (ERROR "~A is not a generic function." name)))))

;;; TODO: Function ENSURE-GENERIC-FUNCTION
(defun ENSURE-GENERIC-FUNCTION (name &rest keys)
  (apply #'ENSURE-GENERIC-FUNCTION-USING-CLASS (gf-or-error name) name keys))

;;; MOP
(defun FINALIZE-INHERITANCE (class)
  (setf (class-finalized-p class) 'T))

;;; MOP
(defun CLASS-FINALIZED-P (class)
  (class-finalized-p class))

;;; TODO: Standard Generic Function ALLOCATE-INSTANCE
(defun ALLOCATE-INSTANCE (class &rest initargs)
  (unless (CLASS-FINALIZED-P class)
    (FINALIZE-INHERITANCE class))
  (vector (CLASS-NAME class)
	  (make-vector (length (CLASS-SLOTS class)) +unbound+)))

;;; TODO: Standard Generic Function REINITIALIZE-INSTANCE

;;; TODO: Standard Generic Function SHARED-INITIALIZE
(defun SHARED-INITIALIZE (instance slot-names &rest initargs)
  (when (eq slot-names 'T)
    (setq slot-names (CLASS-SLOTS (CLASS-OF instance))))
  instance)

;;; TODO: Standard Generic Function UPDATE-INSTANCE-FOR-DIFFERENT-CLASS
;;; TODO: Standard Generic Function UPDATE-INSTANCE-FOR-REDEFINED-CLASS
;;; TODO: Standard Generic Function CHANGE-CLASS

;;; TODO: Function SLOT-BOUNDP
(defun SLOT-BOUNDP (object slot)
  (SLOT-BOUNDP-USING-CLASS (CLASS-OF object) object slot))

(defun slot-index (class object slot)
  (or (position slot (class-slots class))
      (SLOT-MISSING class object slot)))

;;; MOP
(defun SLOT-BOUNDP-USING-CLASS (class object slot)
  (if (eq (CLASS-OF class) +standard-class+)
      (not (eq (aref (aref object 1) (slot-index class object slot))
	       +unbound+))
      (ERROR "SLOT-BOUNDP-USING-CLASS not implemented for ~A." class)))

;;; TODO: Function SLOT-EXISTS-P
(defun SLOT-EXISTS-P (object slot)
  (position slot (class-slots (CLASS-OF object))))

;;; TODO: Function SLOT-MAKUNBOUND
(defun SLOT-MAKUNBOUND (object slot)
  (aset (aref object 1) (slot-index (CLASS-OF object) object slot) +unbound+))

;;; TODO: Standard Generic Function SLOT-MISSING
(defun SLOT-MISSING (class instance slot-name type &optional value)
  (ERROR "SLOT-MISSING"))

;;; TODO: Standard Generic Function SLOT-UNBOUND
(defun SLOT-UNBOUND (class instance slot-name)
  (ERROR "SLOT-UNBOUND"))

(defun FUNCALLABLE-STANDARD-INSTANCE-ACCESS (object location)
  (aref (gf-slots object) location))

;;; TODO: Function SLOT-VALUE
(defun SLOT-VALUE (object slot)
  (let ((class (CLASS-OF object)))
    (cond
      ((funcallable-object-p object)
       (let ((value (FUNCALLABLE-STANDARD-INSTANCE-ACCESS
		     object (slot-index class object slot))))
	 (if (eq value +unbound+)
	     (SLOT-UNBOUND class object slot)
	     value)))
      ((eq (CLASS-OF class) +standard-class+)
       (let ((value (aref (aref object 1) (slot-index class object slot))))
	 (if (eq value +unbound+)
	     (SLOT-UNBOUND class object slot)
	     value)))
      (t
       (SLOT-VALUE-USING-CLASS class object slot)))))

(DEFSETF SLOT-VALUE set-slot-value)

(defun set-slot-value (object slot value)
  (let ((class (CLASS-OF object)))
    (cond
      ((funcallable-object-p object)
       (aset (gf-slots object) (slot-index class object slot) value))
      ((eq (CLASS-OF class) +standard-class+)
       (aset (aref object 1) (slot-index class object slot) value))
      (t
       (FUNCALL '(SETF SLOT-VALUE-USING-CLASS) value class object slot)))))

;;; TODO: Standard Generic Function METHOD-QUALIFIERS

;;; TODO: Standard Generic Function NO-APPLICABLE-METHOD
(defun NO-APPLICABLE-METHOD (gf &rest args)
  (ERROR "No applicable method for ~A called with arguments ~A." gf args))

;;; TODO: Standard Generic Function NO-NEXT-METHOD
;;; TODO: Standard Generic Function REMOVE-METHOD

;;; TODO: Standard Generic Function MAKE-INSTANCE
(defun MAKE-INSTANCE (class &rest initargs)
  (when (symbolp class)
    (setq class (FIND-CLASS class)))
  (when (built-in-class-p class)
    (ERROR "Can't make instance of a built-in class."))
  ;; TODO: Augment initargs with default initargs.
  (let ((instance (apply #'ALLOCATE-INSTANCE class initargs)))
    (apply #'INITIALIZE-INSTANCE instance initargs)
    instance))

;;; TODO: Standard Generic Function MAKE-INSTANCES-OBSOLETE

;;; MOP
(defun SLOT-NAME (slot)
  ;; TODO:
  slot)

;;; TODO: Function MAKE-LOAD-FORM-SAVING-SLOTS
(cl:defun MAKE-LOAD-FORM-SAVING-SLOTS (object &KEY SLOT-NAMES ENVIRONMENT)
  (if (structurep object)
      (cl:values
       `(make-vector ,(length object) (QUOTE ,(aref object 0)))
       `(LET ((object ,object))
	  ,@(do ((result nil)
		 (length (length object))
		 (i 1 (1+ i)))
		((>= i length)
		 (nreverse result))
	      (push `(aset object ,i (QUOTE ,(aref object i))) result))))
      (let ((class (CLASS-OF object)))
	(when (null SLOT-NAMES)
	  (setq SLOT-NAMES (mapcar #'SLOT-NAME (CLASS-SLOTS class))))
	(cl:values
	 `(ALLOCATE-INSTANCE (FIND-CLASS (QUOTE ,(CLASS-NAME class))))
	 `(LET ((object ,object))
	    ,@(mapcar (lambda (slot)
			(if (SLOT-BOUNDP object slot)
			    `(SETF (SLOT-VALUE object (QUOTE ,slot))
			           (QUOTE ,(SLOT-VALUE object slot)))
			    `(SLOT-MAKUNBOUND object (QUOTE ,slot))))
		      SLOT-NAMES)
	    (INITIALIZE-INSTANCE object))))))

;;; TODO: Macro WITH-ACCESSORS
;;; TODO: Macro WITH-SLOTS

;;; TODO: Macro DEFCLASS
(cl:defmacro DEFCLASS (name superclasses slots &rest options)
  (when (null superclasses)
    (setq superclasses '(STANDARD-OBJECT)))
  `(ENSURE-CLASS (QUOTE ,name)
		 ,(kw DIRECT-SUPERCLASSES) (QUOTE ,superclasses)
		 ,(kw DIRECT-SLOTS) (QUOTE ,slots)))

;;; TODO: Macro DEFGENERIC
(cl:defmacro DEFGENERIC (name lambda-list &rest stuff)
  `(ENSURE-GENERIC-FUNCTION (QUOTE ,name)
			    ,(kw LAMBDA-LIST) (QUOTE ,lambda-list)))

(defmacro cl:defgeneric (name lambda-list &rest stuff)
  (when byte-compile-warnings
    (byte-compile-log-1 (format "cl:defgeneric %s" name)))
  `(ENSURE-GENERIC-FUNCTION ',name ',(kw LAMBDA-LIST) ',lambda-list))

;;; TODO: Macro DEFMETHOD
(cl:defmacro DEFMETHOD (name lambda-list &rest forms)
  (when (not (listp lambda-list))
    (ERROR "Only primary methods are implemented."))
  `(ensure-method (QUOTE ,name) (QUOTE ,lambda-list) (QUOTE ,forms)))

(defmacro cl:defmethod (name lambda-list &rest forms)
  (when byte-compile-warnings
    (byte-compile-log-1 (format "cl:defmethod %s" name)))
  `(ensure-method ',name ',lambda-list ',forms))

;;; MOP
(defun EXTRACT-LAMBDA-LIST (lambda-list)
  (let* ((lambda-list (copy-list lambda-list))
	 (not-required (MEMBER-IF #'lambda-list-keyword-p lambda-list))
	 (required (LDIFF lambda-list not-required)))
    (append
     (mapcar (lambda (x)
	       (typecase x
		 (symbol	x)
		 (cons		(car x))
		 (t		(type-error x '(OR SYMBOL CONS)))))
	     required)
     not-required)))

;;; MOP
(defun EXTRACT-SPECIALIZER-NAMES (lambda-list)
  (let* ((lambda-list (copy-list lambda-list))
	 (required (LDIFF lambda-list
			  (MEMBER-IF #'lambda-list-keyword-p lambda-list))))
    (mapcar (lambda (x)
	      (typecase x
		(symbol		'T)
		(cons		(second x))
		(t		(type-error x '(OR SYMBOL CONS)))))
	    required)))

(defun specializer (name)
  (cond
    ((symbolp name)	(FIND-CLASS name))
    ((and (listp name)
	  (= (length name) 2)
	  (eq (first name) 'EQL))
			(INTERN-EQL-SPECIALIZER (second name)))
    (t			(type-error name '(OR SYMBOL CONS)))))

(defconst +eql-fn+ (if (featurep 'xemacs) #'eql #'EQL))
(defvar *eql-specializers* (make-hash-table :test +eql-fn+))

(defun INTERN-EQL-SPECIALIZER (object)
  (or (gethash object *eql-specializers*)
      (puthash object (make-eql-specializer object) *eql-specializers*)))

(defun EQL-SPECIALIZER-OBJECT (eql-specializer)
  (aref (aref eql-specializer 1) 0))

(defun make-eql-specializer (object)
  (vector 'EQL-SPECIALIZER (vector object)))

(defun ensure-method (name lambda-list forms)
  (MULTIPLE-VALUE-BIND (body decls doc) (parse-body forms t)
    (let ((gf (ENSURE-GENERIC-FUNCTION name (kw LAMBDA-LIST) lambda-list))
	  (method (make-method
		   (EXTRACT-LAMBDA-LIST lambda-list)
		   (mapcar #'specializer
			   (EXTRACT-SPECIALIZER-NAMES lambda-list))
		   body)))
      (ADD-METHOD gf method)
      method)))

;;; TODO: Accessor FIND-CLASS
(cl:defun FIND-CLASS (name &OPTIONAL (errorp t) env)
  (let ((class (gethash name *classes*)))
    (when (and (null class) errorp)
      (ERROR "No such class ~A." name))
    class))

(cl-defun '(SETF FIND-CLASS)
	  (lambda (class name &optional errorp env)
	    (puthash name class *classes*)))

;;; TODO: Local Function NEXT-METHOD-P
;;; TODO: Local Macro CALL-METHOD,
;;; TODO:             MAKE-METHOD
;;; TODO: Local Function CALL-NEXT-METHOD
;;; TODO: Standard Generic Function COMPUTE-APPLICABLE-METHODS
;;; TODO: Macro DEFINE-METHOD-COMBINATION
;;; TODO: Standard Generic Function FIND-METHOD

;;; MOP
(defun SET-FUNCALLABLE-INSTANCE-FUNCTION (gf fn)
  (aset (cfref gf 2) +gf-template-fn-pos+ fn))

;;; TODO: Standard Generic Function ADD-METHOD
(defun ADD-METHOD (gf method)
  ;; (i)
  (push method (gf-methods gf))
  (setf (method-generic-function method) gf)
  ;; (ii) (ADD-DIRECT-METHOD ...)
  ;; (iii)
  (SET-FUNCALLABLE-INSTANCE-FUNCTION gf (COMPUTE-DISCRIMINATING-FUNCTION gf))
  ;; (iv) Update dependents.
  gf)

(defun more-specializing-p (s1 s2)
  (or (eq (type-of s1) 'EQL-SPECIALIZER)
      (and (classp s2)
	   (subclassp s1 s2))))

(defun sort-methods (methods)
  (let ((result nil)
	(least-specific-method nil))
    (while methods
      (setq least-specific-method (first methods))
      (dolist (method (rest methods))
	(when (more-specializing-p
	       (first (method-specializers least-specific-method))
	       (first (method-specializers method)))
	  (setq least-specific-method method)))
      (setq methods (delq least-specific-method methods))
      (push least-specific-method result))
    result))

;;; MOP
(defun METHOD-SPECIALIZERS (method)
  (method-specializers method))

;;; MOP
(defun COMPUTE-DISCRIMINATING-FUNCTION (gf)
  (with-gensyms (args)
    (COMPILE nil `(LAMBDA (&REST ,args)
		   (TYPECASE (car ,args)
		     ,@(mapcar
			(lambda (method)
			  `(,(first (method-specializers method))
			    (APPLY (LAMBDA ,(method-lambda-list method)
				     ,@(method-body method))
			           ,args)))
			(sort-methods (copy-list (gf-methods gf))))
		     (T (APPLY (FUNCTION NO-APPLICABLE-METHOD)
			       ,gf ,args)))))))

;; (lambda (arg &rest rest)
;;   (flet ((call-next-method ()
;; 	   (if (typep arg 'bar)
;; 	       (call-method ...)	;Around method 2
;; 	       ...)))
;;     (if (typep arg 'foo)
;; 	   (call-method ...)		;Around method 1
;; 	   (apply #'call-next-method arg rest))))

;; (lambda (arg &rest rest)
;;   (when (typep arg 'foo)
;;     (call-method ...))		;Before method 1
;;   (when (typep arg 'bar)
;;     (call-method ...))		;Before method 2
;;   (typecase arg
;;     (foo (call-method ...))		;Primary method 1
;;     (bar (call-method ...))		;Primary method 2
;;     (t (no-applicable-method ...)))
;;   (when (typep arg 'bar)
;;     (call-method ...))		;After method 1
;;   (when (typep arg 'foo)
;;     (call-method ...)))		;After method 2

;;; TODO: Standard Generic Function INITIALIZE-INSTANCE
(defun INITIALIZE-INSTANCE (instance &rest initargs)
  (apply #'SHARED-INITIALIZE instance 'T initargs))

;;; TODO: Standard Generic Function CLASS-NAME
(defun CLASS-NAME (class)
  (class-name class))

;;; TODO: Standard Generic Function (SETF CLASS-NAME)
(cl-defun '(SETF CLASS-NAME)
	  (lambda (name class)
	    (setf (class-name class) name)))

;;; TODO: Function CLASS-OF
(defun CLASS-OF (object)
  (if (null object)
      +null-class+
      (ecase (type-of object)
	;; This is supposed to be an exhaustive enumeration of all
	;; possible return values for Emacs Lisp type-of.
	((bit-vector bool-vector)
			+bit-vector-class+)
	(subr		+function-class+)
	(compiled-function
			(let ((info (gethash object *funcallable-objects*)))
			  (if info
			      (FIND-CLASS (car info))
			      +function-class+)))
	(character	+character-class+)
	(cons		+cons-class+)
	(float		+float-class+)
	(hash-table	+hash-table-class+)
	(integer	+integer-class+)
	(string		+string-class+)
	(symbol		+symbol-class+)
	(vector
	 (let ((class (FIND-CLASS (aref object 0) nil)))
	   (or class
	       (ecase (aref object 0)
		 ((bit-array char-array)
				+array-class+)
		 (BIGNUM	+integer-class+)
		 (INTERPRETED-FUNCTION
				+function-class+)
		 (SIMPLE-VECTOR	+vector-class+)))))
	;; For now, throw an error on these.
	((buffer char-table frame marker overlay process
		 subr window window-configuration)
			(error "Unknown type: %s" (type-of object))))))

(defun subclassp (class1 class2)
  (or (eq class2 +t-class+)
      (eq class1 class2)
      (some (lambda (name) (subclassp (FIND-CLASS name) class2))
	    (class-superclasses class1))))

(defun classp (object)
  (subclassp (CLASS-OF object) +class-class+))

;;; UNBOUND-SLOT and UNBOUND-SLOT-INSTANCE defined in cl-conditions.el.

;;; TODO:
(cl-defun '(SETF DOCUMENTATION) (lambda (value object type) value))

(cl:defmethod MAKE-LOAD-FORM  ((object T) &OPTIONAL env)
  (built-in-make-load-form object env))
(cl:defmethod MAKE-LOAD-FORM  ((object STANDARD-OBJECT) &OPTIONAL env)
  (ERROR (QUOTE ERROR)))
(cl:defmethod MAKE-LOAD-FORM  ((object STRUCTURE-OBJECT) &OPTIONAL env)
  (ERROR (QUOTE ERROR)))
(cl:defmethod MAKE-LOAD-FORM  ((object STRUCTURE-OBJECT) &OPTIONAL env)
  (BACKQUOTE (FIND-CLASS (QUOTE (COMMA (CLASS-NAME object))))))

(cl:defmethod PRINT-OBJECT ((object T) stream)
  (built-in-print-object object stream))

(cl:defmethod DOCUMENTATION ((object FUNCTION) type)
  (WHEN (> (length object) 4)
    (aref object 4)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-objects.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-format.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements the FORMAT function from chapter 22, Printer.

;;; This implementation is somewhat silly.  I don't know what I was
;;; thinking.  I will improve it later.

(IN-PACKAGE "EMACS-CL")

;;; Some tests:
;;; (FORMAT nil "~D~?~D" 1 "~X" '(10) 3)
;;; (FORMAT nil "foo~[1~;2~:;3~]bar" -1)
;;; (FORMAT nil "foo~@[~D~]bar~D" 1 2)
;;; (FORMAT nil "foo~:[1~;2~]bar" t)
;;; (FORMAT nil "foo~{ ~D~} bar" '(1 2 3))
;;; (FORMAT nil "foo~@{ ~D~} bar" 1 2 3)
;;; (FORMAT nil "foo~:{ ~D~} bar" '((1 4) (2) (3)))
;;; (FORMAT nil "foo~:@{ ~D~} bar" '(1) '(2) '(3))

(defvar *format-directives* (make-hash-table :test #'eq))

(defmacro* define-format-directive (char lambda-list &body body)
  `(setf (gethash ,char *format-directives*)
         (cl:lambda ,lambda-list ,@body)))

(DEFSTRUCT (format-state (:conc-name nil)
			 (:predicate nil)
			 (:copier nil)
			 (:constructor make-format-state (format-string
							  format-args)))
  format-args
  format-string
  (arg-index 0)
  (format-index 0)
  conditional-arg
  conditional-index
  iteration-kind
  iteration-index
  (conditional-printing-p t)
  (iterative-printing-p t)
  (nesting-stack nil))

(defun printing-p (state)
  (and (conditional-printing-p state)
       (iterative-printing-p state)))

(defun next-arg (state)
  (when (printing-p state)
    (prog1
	(nth (arg-index state) (format-args state))
      (incf (arg-index state)))))

(defun next-char (state)
  (prog1
      (CHAR (format-string state) (format-index state))
    (incf (format-index state))))

(defun push-nesting (state type &optional option)
  (ecase type
    (:conditional
     (push (conditional-arg state) (nesting-stack state))
     (push (conditional-index state) (nesting-stack state))
     (push (conditional-printing-p state) (nesting-stack state)))
    (:iteration
     (unless option
       (push (format-args state) (nesting-stack state))
       (push (arg-index state) (nesting-stack state)))
     (push (iterative-printing-p state) (nesting-stack state))
     (push (iteration-kind state) (nesting-stack state))
     (push (iteration-index state) (nesting-stack state)))))

(defun pop-nesting (state type &optional option)
  (ecase type
    (:conditional
     (setf (conditional-printing-p state) (pop (nesting-stack state)))
     (setf (conditional-index state) (pop (nesting-stack state)))
     (setf (conditional-arg state) (pop (nesting-stack state))))
    (:iteration
     (setf (iteration-index state) (pop (nesting-stack state)))
     (setf (iteration-kind state) (pop (nesting-stack state)))
     (setf (iterative-printing-p state) (pop (nesting-stack state)))
     (unless option
       (setf (arg-index state) (pop (nesting-stack state)))
       (setf (format-args state) (pop (nesting-stack state)))))))



;;; Basic Output ----------------------------------------

(defun printing-char-p (char)
  (and (not (ch= char 32)) (GRAPHIC-CHAR-P char)))

(define-format-directive 67 (stream state atp colonp)	; ~C
  (when (printing-p state)
    (let ((char (next-arg state)))
      (cond
	(colonp
	 (if (printing-char-p char)
	     (WRITE-CHAR char stream)
	     (WRITE-STRING (CHAR-NAME char) stream)))
	(atp
	 (WRITE char (kw STREAM) stream (kw READABLY) t))
	(t
	 (WRITE-CHAR char stream))))))

(define-format-directive 37 (stream state atp colonp &OPTIONAL (n 1))	; ~%
  (when (printing-p state)
    (dotimes (i n)
      (TERPRI stream))))

(define-format-directive 38 (stream state atp colonp &OPTIONAL (n 1))	; ~&
  (when (printing-p state)
    (when (plusp n)
      (FRESH-LINE stream)
      (dotimes (i (1- n))
	(TERPRI stream)))))

(define-format-directive 124 (stream state atp colonp &OPTIONAL (n 1))	; ~|
  (when (printing-p state)
    (dotimes (i n)
      (WRITE-CHAR (ch 12) stream))))

(define-format-directive 126 (stream state atp colonp &OPTIONAL (n 1))	; ~~
  (when (printing-p state)
    (dotimes (i n)
      (WRITE-CHAR (ch 126) stream))))

;;; Radix Control ----------------------------------------

(define-format-directive 82 (stream state atp colonp &OPTIONAL radix mincol	; ~R
				    padchar commachar comma-interval)
  (when (printing-p state)
    (let ((num (next-arg state)))
      (if (null radix)
	  nil ;TODO
	  (WRITE num (kw STREAM) stream (kw BASE) radix (kw RADIX) nil)))))

(defun print-in-radix (num stream atp radix)
  (when (and atp (not (ZEROP num)))
    (WRITE-CHAR (ch 43) stream))
  (WRITE num (kw STREAM) stream (kw ESCAPE) nil (kw RADIX) nil
	     (kw BASE) radix (kw READABLY) nil))
      
(define-format-directive 68 (stream state atp colonp &OPTIONAL mincol padchar	; ~D
				    commachar comma-interval)
  (when (printing-p state)
    (print-in-radix (next-arg state) stream atp 10)))

(define-format-directive 66 (stream state atp colonp &OPTIONAL mincol padchar	; ~B
				    commachar comma-interval)
  (when (printing-p state)
    (print-in-radix (next-arg state) stream atp 2)))

(define-format-directive 79 (stream state atp colonp &OPTIONAL mincol padchar	; ~O
				    commachar comma-interval)
  (when (printing-p state)
    (print-in-radix (next-arg state) stream atp 8)))

(define-format-directive 88 (stream state atp colonp &OPTIONAL mincol padchar	; ~X
				    commachar comma-interval)
  (when (printing-p state)
    (print-in-radix (next-arg state) stream atp 16)))

;;; Floating Point Printers ----------------------------------------

;;; TODO: ~F
;;; TODO: ~E
;;; TODO: ~G
;;; TODO: ~$

;;; Printer Operations ----------------------------------------

(define-format-directive 65 (stream state atp colonp	; ~A
			     &OPTIONAL mincol colinc minpad padchar)
  (when (printing-p state)
    (PRINC (next-arg state) stream)))

(define-format-directive 83 (stream state atp colonp	; ~S
			     &OPTIONAL mincol colinc minpad padchar)
  (when (printing-p state)
    (PRIN1 (next-arg state) stream)))

(define-format-directive 87 (stream state atp colonp)	; ~W
  (when (printing-p state)
    (let ((*PRINT-PRETTY* colonp)
	  (*PRINT-LEVEL* (if atp nil *PRINT-LEVEL*))
	  (*PRINT-LENGTH* (if atp nil *PRINT-LENGTH*)))
      (WRITE (next-arg state) (kw STREAM) stream))))

;;; Pretty Printer Operations ----------------------------------------

(define-format-directive 95 (stream state atp colonp &REST args)	; ~_
  (when (printing-p state)
    (PPRINT-NEWLINE (if atp
			(if colonp (kw MANDATORY) (kw MISER))
			(if colonp (kw FILL) (kw LINEAR)))
		    stream)))

;;; TODO: ~<
(define-format-directive 60 (stream state atp colonp)			; ~<
  nil)

(define-format-directive 73 (stream state atp colonp &OPTIONAL (n 0))	; ~I
  (PPRINT-INDENT (if colonp (kw CURRENT) (kw BLOCK)) n))

(define-format-directive 47 (stream state atp colonp &REST args)	; ~/
  (when (printing-p state)
    (let ((name "")
	  (package *cl-user-package*)
	  char)
      (while (not (ch= (setq char (next-char state)) 47))
	(when (and (ch= char 58) (eq package *cl-user-package*))
	  (setq package name)
	  (setq name "")
	  (when (ch= (setq char (next-char state)) 58)
	    (setq char (next-char state))))
	(setq name (concat name (string (char-upcase-code char)))))
      (MULTIPLE-VALUE-BIND (symbol found) (FIND-SYMBOL name package)
	(APPLY (symbol-function symbol)
	       stream (next-arg state) colonp atp args)))))

;;; Layout Control ----------------------------------------

;;; TODO: ~T
(define-format-directive 84 (stream state atp colonp)			; ~T
  nil)

;;; TODO: ~<
(define-format-directive 60 (stream state atp colonp)			; ~<
  nil)

;;; TODO: ~>
(define-format-directive 62 (stream state atp colonp)			; ~>
  nil)

;;; Control-Flow Operations ----------------------------------------

(define-format-directive 42 (stream state atp colonp &OPTIONAL n)	; ~*
  (when (printing-p state)
    (cond
      (atp	(setf (arg-index state) (or n 0)))
      (colonp	(decf (arg-index state) (or n 1)))
      (t	(incf (arg-index state) (or n 1))))))

(defun check-condition (state &optional colonp)
  (setf (conditional-printing-p state)
	(or (and colonp (not (null (conditional-arg state))))
	    (eq (conditional-arg state) (conditional-index state))))
  (when (conditional-printing-p state)
    (setf (conditional-arg state) nil)))

(define-format-directive 91 (stream state atp colonp &OPTIONAL n)     ; ~[
  (push-nesting state :conditional)
  (cond
    (colonp
     (setf (conditional-arg state) (if (next-arg state) 1 0))
     (setf (conditional-index state) 0)
     (check-condition state))
    (atp
     (when (setf (conditional-printing-p state) (next-arg state))
       (decf (arg-index state))))
    (t
     (setf (conditional-arg state) (or n (next-arg state)))
     (setf (conditional-index state) 0)
     (check-condition state))))

(define-format-directive 93 (stream state atp colonp)		      ; ~]
  (pop-nesting state :conditional))

(define-format-directive 123 (stream state atp colonp &OPTIONAL n)	; ~{
  (if atp
      (progn
	(push-nesting state :iteration :at)
	(setf (iteration-kind state) :at))
      (let ((arg (next-arg state)))
	(push-nesting state :iteration)
	(setf (format-args state) arg)
	(setf (iteration-kind state) nil)
	(setf (arg-index state) 0)))
  (setf (iteration-index state) (format-index state))
  (when colonp
    (let ((arg (next-arg state)))
      (push-nesting state :iteration)
      (setf (format-args state) arg))
    (setf (arg-index state) 0)
    (setf (iteration-kind state) :colon)))

(define-format-directive 125 (stream state atp colonp &OPTIONAL n)	; ~}
  (cond
    ((eq (iteration-kind state) :colon)
     (pop-nesting state :iteration)
     (if (>= (arg-index state) (length (format-args state)))
	 (pop-nesting state :iteration (iteration-kind state))
	 (let ((arg (next-arg state)))
	   (setf (format-index state) (iteration-index state))
	   (push-nesting state :iteration)
	   (setf (format-args state) arg)
	   (setf (arg-index state) 0)
	   (setf (iteration-kind state) :colon))))
    ((>= (arg-index state) (length (format-args state)))
     (pop-nesting state :iteration (iteration-kind state)))
    (t
     (setf (format-index state) (iteration-index state)))))

(define-format-directive 63 (stream state atp colonp)	; ~?
  (when (printing-p state)
    (let* ((format (next-arg state))
	   (args (next-arg state)))
      (apply #'FORMAT stream format args))))

;;; Miscellaneous Operations ----------------------------------------

;;; TODO: ~(
(define-format-directive 40 (stream state atp colonp)	; ~(
  nil)

;;; TODO: ~)
(define-format-directive 41 (stream state atp colonp)	; ~)
  nil)

(define-format-directive 80 (stream state atp colonp)	; ~P
  (when (printing-p state)
    (when colonp
      (decf (arg-index state)))
    (let ((pluralp (not (eq (next-arg state) 1))))
      (if atp
	  (WRITE-STRING (if pluralp "ies" "y") stream)
	  (when pluralp
	    (WRITE-CHAR (ch 115) stream))))))

;;; Miscellaneous Pseudo-Operations ----------------------------------------

(define-format-directive 59 (stream state atp colonp)		      ; ~;
  (when (conditional-index state) ; Ugly hack!
    (incf (conditional-index state)))
  (check-condition state colonp))

(define-format-directive 94 (stream state atp colonp &OPTIONAL n1 n2 n3)	; ~^
  (when (and (printing-p state)
	     (cond
	       ((null n1)	(>= (arg-index state)
				    (length (format-args state))))
	       ((null n2)	(zerop n1))
	       ((null n3)	(eq n1 n2))
	       (t		(cl:<= n1 n2 n3))))
    (if (iteration-index state)
	(progn
	  (setf (iterative-printing-p state) nil)
	  (when colonp
	    (setf (arg-index state) (length (format-args state)))))
	(setf (format-index state) (LENGTH (format-string state))))))

(define-format-directive 10 (stream state atp colonp)
  (when (printing-p state)
    (when atp
      (TERPRI stream))
    (when (not colonp)
      (let ((char (CHARACTER " ")))
	(while (and (< (format-index state) (LENGTH (format-string state)))
		    (progn
		      (setq char (next-char state))
		      (whitespacep char))))
	(unless (whitespacep char)
	  (decf (format-index state)))))))



(cl:defmacro FORMATTER (format)
  (unless (STRINGP format)
    (type-error format 'STRING))
  ;; TODO: better implementation
  (let ((env (augment-environment nil :variable '(format))))
    (setf (lexical-value 'format env) format)
    (enclose '(LAMBDA (*STANDARD-OUTPUT* &REST args)
	        (APPLY (FUNCTION FORMAT) T format args)
	        nil)
	     env (format "\"formatter \"%s\"\"" format))))

(defun FORMAT (stream-designator format &rest args)
  (let ((stream (or (and (eq stream-designator 'T) *STANDARD-OUTPUT*)
		    stream-designator
		    (MAKE-STRING-OUTPUT-STREAM))))
    (if (FUNCTIONP format)
	(APPLY format stream args)
	(let ((state (make-format-state format args)))
	  (while (< (format-index state) (LENGTH format))
	    (let ((char (next-char state)))
	      (cond
		((ch= char 126)
		 (let ((atp nil)
		       (colonp nil)
		       (parameters nil))
		   (setq char (next-char state))
		   (while (FIND char "0123456789")
		     (setq char (next-char state)))
		   (while (FIND char ":@")
		     (cond
		       ((ch= char 58)	(setq colonp t))
		       ((ch= char 64)	(setq atp t)))
		     (setq char (next-char state)))
		   (let ((fn (gethash (char-upcase-code char)
				      *format-directives*)))
		     (if (null fn)
			 (ERROR "Uknown FORMAT directive ~~~A" char)
			 (apply fn stream state atp colonp parameters)))))
		((printing-p state)
		 (WRITE-CHAR char stream)))))))
    (if stream-designator
	nil
	(GET-OUTPUT-STREAM-STRING stream))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-format.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-reader.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 23, Reader.

(IN-PACKAGE "EMACS-CL")

(defvar *backquote-level* 0)

(DEFSTRUCT (READTABLE (:predicate READTABLEP) (:copier nil))
  CASE
  SYNTAX-TYPE
  MACRO-TABLE
  DISPATCH-TABLE)

(defun* COPY-READTABLE (&optional (from *READTABLE*) to)
  (unless from
    (setq from *standard-readtable*))
  (unless to
    (setq to (MAKE-READTABLE)))
  (setf (READTABLE-CASE to) (READTABLE-CASE from))
  (setf (READTABLE-SYNTAX-TYPE to)
	(copy-sequence (READTABLE-SYNTAX-TYPE from)))
  (setf (READTABLE-MACRO-TABLE to)
	(copy-sequence (READTABLE-MACRO-TABLE from)))
  (setf (READTABLE-DISPATCH-TABLE to)
	(let ((hash (make-hash-table :test #'equal)))
	  (maphash (lambda (key val) (setf (gethash key hash) val))
		   (READTABLE-DISPATCH-TABLE from))
	  hash))
  to)

(cl:defun MAKE-DISPATCH-MACRO-CHARACTER (char &OPTIONAL non-terminating-p
					      (readtable *READTABLE*))
  (SET-MACRO-CHARACTER char #'dispatch-reader non-terminating-p readtable)
  T)

(defun* read-token (stream)
  (let (char
	(escape nil)
	(package nil)
	(colons 0)
	(token nil))
    (tagbody
      STEP-1
       (setq char (READ-CHAR stream t nil t))

      (case (char-syntx char)
	(:whitespace
	 (go STEP-1))
	((:terminating-macro :non-terminating-macro)
	 (let* ((fn (GET-MACRO-CHARACTER char))
		(list (MULTIPLE-VALUE-LIST (FUNCALL fn stream char))))
	   (return-from read-token (cl:values (first list)))))
	(:single-escape
	 (setq escape t)
	 (setq char (READ-CHAR stream T nil T))
	 (setq token (concat token (list (CHAR-CODE char))))
	 (go STEP-8))
	(:multiple-escape
	 (go STEP-9))
	(:constituent
	 (setq char (char-convert-case char))))

      STEP-8a
      (cond
	((ch= char 58)
	 (incf colons)
	 (when (or (and package (not (zerop (length token))))
		   (> colons 2))
	   (ERROR 'READER-ERROR))
	 (if (= colons 1)
	     (setq package token))
	 (setq escape nil)
	 (setq token nil))
	(t
	 (setq token (concat token (list (CHAR-CODE char))))))
      STEP-8
      (setq char (READ-CHAR stream nil nil T))
      (when (null char)
	(go STEP-10))
      (case (char-syntx char)
	((:constituent :non-terminating-macro)
	 (setq char (char-convert-case char))
	 (go STEP-8a))
	(:single-escape
	 (setq escape t)
	 (setq char (READ-CHAR stream T nil T))
	 (setq token (concat token (list (CHAR-CODE char))))
	 (go STEP-8))
	(:multiple-escape
	 (go STEP-9))
	(:terminating-macro
	 (UNREAD-CHAR char stream)
	 (go STEP-10))
	(:whitespace
	 (UNREAD-CHAR char stream)
	 (go STEP-10)))

      STEP-9
      (setq escape t)
      (setq char (READ-CHAR stream nil nil T))
      (when (null char)
	(ERROR 'READER-ERROR))
      (case (char-syntx char)
	((:constituent :non-terminating-macro :terminating-macro :whitespace)
	 (setq token (concat token (list (CHAR-CODE char))))
	 (go STEP-9))
	(:single-escape
	 (setq char (READ-CHAR stream T nil T))
	 (setq token (concat token (list (CHAR-CODE char))))
	 (go STEP-9))
	(:multiple-escape
	 (when (null token)
	   (setq token ""))
	 (go STEP-8)))

      STEP-10
      (cl:values package colons token escape))))

(defun* read1 (stream eof-error-p eof-value recursive-p preserve-whitespace
	       &optional return-when-nothing)
  (let (char
	(escape nil)
	(package nil)
	(colons 0)
	(token nil))
    (tagbody
      STEP-1
       (setq char (READ-CHAR stream eof-error-p eof-value recursive-p))
       (when (EQL char eof-value)
	 (return-from read1 eof-value))

      (case (char-syntx char)
	(:whitespace
	 (go STEP-1))
	((:terminating-macro :non-terminating-macro)
	 (let* ((fn (GET-MACRO-CHARACTER char))
		(list (MULTIPLE-VALUE-LIST (FUNCALL fn stream char))))
	   (if (null list)
	       (if return-when-nothing
		   (return-from read1 (cl:values nil t))
		   (go STEP-1))
	       (return-from read1 (cl:values (first list))))))
	(:single-escape
	 (setq escape t)
	 (setq char (READ-CHAR stream T nil T))
	 (setq token (concat token (list (CHAR-CODE char))))
	 (go STEP-8))
	(:multiple-escape
	 (go STEP-9))
	(:constituent
	 (setq char (char-convert-case char))))

      STEP-8a
      (cond
	((ch= char 58)
	 (incf colons)
	 (when (or (and package (not (zerop (length token))))
		   (> colons 2))
	   (ERROR 'READER-ERROR))
	 (if (= colons 1)
	     (setq package token))
	 (setq escape nil)
	 (setq token nil))
	(t
	 (setq token (concat token (list (CHAR-CODE char))))))
      STEP-8
      (setq char (READ-CHAR stream nil nil T))
      (when (null char)
	(go STEP-10))
      (case (char-syntx char)
	((:constituent :non-terminating-macro)
	 (setq char (char-convert-case char))
	 (go STEP-8a))
	(:single-escape
	 (setq escape t)
	 (setq char (READ-CHAR stream T nil T))
	 (setq token (concat token (list (CHAR-CODE char))))
	 (go STEP-8))
	(:multiple-escape
	 (go STEP-9))
	(:terminating-macro
	 (UNREAD-CHAR char stream)
	 (go STEP-10))
	(:whitespace
	 (when preserve-whitespace
	   (UNREAD-CHAR char stream))
	 (go STEP-10)))

      STEP-9
      (setq escape t)
      (setq char (READ-CHAR stream nil nil T))
      (when (null char)
	(ERROR 'READER-ERROR))
      (case (char-syntx char)
	((:constituent :non-terminating-macro :terminating-macro :whitespace)
	 (setq token (concat token (list (CHAR-CODE char))))
	 (go STEP-9))
	(:single-escape
	 (setq char (READ-CHAR stream T nil T))
	 (setq token (concat token (list (CHAR-CODE char))))
	 (go STEP-9))
	(:multiple-escape
	 (when (null token)
	   (setq token ""))
	 (go STEP-8)))

      STEP-10
      (unless *READ-SUPPRESS*
	(return-from read1 (process-token package colons token escape))))))

(defmacro* let-unless (condition bindings &body body)
  `(if ,condition
       ,@body
       (let ,bindings
	 ,@body)))

(cl:defun READ (&OPTIONAL stream (eof-error-p T) eof-value recursive-p)
  (let-unless recursive-p
	    ((*sharp-equal-table* (make-hash-table :test #'equal)))
    (read1 stream eof-error-p eof-value recursive-p nil)))

(cl:defun READ-PRESERVING-WHITESPACE (&OPTIONAL stream (eof-error-p T)
				      eof-value recursive-p)
  (let-unless recursive-p
	    ((*sharp-equal-table* (make-hash-table :test #'equal)))
    (read1 stream eof-error-p eof-value recursive-p t)))

(defmacro* unless-read-suppress-let ((var form) &body body)
  `(let ((,var ,form))
     (unless *READ-SUPPRESS*
       ,@body)))

(defun* READ-DELIMITED-LIST (delimiter &optional (stream *STANDARD-INPUT*)
			                         recursive-p)
  (do ((list nil)
       (char (PEEK-CHAR T stream T nil recursive-p)
	     (PEEK-CHAR T stream T nil recursive-p)))
      ((CHAR= char delimiter)
       (READ-CHAR stream t nil recursive-p)
       (nreverse list))
    (unless-read-suppress-let (object (READ stream t nil recursive-p))
      (push object list))))

(cl:defun READ-FROM-STRING (string &OPTIONAL (eof-error-p T) eof-value
			           &KEY (START 0) END PRESERVE-WHITESPACE)
  (let ((stream (MAKE-STRING-INPUT-STREAM string START END)))
    (cl:values (if PRESERVE-WHITESPACE
		   (READ-PRESERVING-WHITESPACE stream eof-error-p eof-value)
		   (READ stream eof-error-p eof-value))
	       (STREAM-position stream))))

;;; READTABLE-CASE defined by defstruct.

;;; READTABLEP defined by defstruct.

(defun* SET-DISPATCH-MACRO-CHARACTER (disp-char sub-char new-function
				      &optional (readtable *READTABLE*))
  (let ((string (concat (list (CHAR-CODE disp-char) (CHAR-CODE sub-char)))))
    (setf (gethash string (READTABLE-DISPATCH-TABLE readtable))
	  new-function))
  T)

(defun* GET-DISPATCH-MACRO-CHARACTER (disp-char sub-char
				      &optional (readtable *READTABLE*))
  (let ((string (concat (list (CHAR-CODE disp-char) (CHAR-CODE sub-char)))))
    (gethash string (READTABLE-DISPATCH-TABLE readtable))))

(defun* SET-MACRO-CHARACTER (char new-function
			     &optional non-terminating-p
			               (readtable *READTABLE*))
  (setf (aref (READTABLE-MACRO-TABLE readtable) (CHAR-CODE char)) new-function)
  (setf (aref (READTABLE-SYNTAX-TYPE readtable) (CHAR-CODE char))
	(if non-terminating-p
	    :non-terminating-macro
	    :terminating-macro))
  T)

(defun* char-syntx (char &optional (readtable *READTABLE*))
  (aref (READTABLE-SYNTAX-TYPE readtable) (CHAR-CODE char)))

(defun* GET-MACRO-CHARACTER (char &optional (readtable *READTABLE*))
  (cl:values (aref (READTABLE-MACRO-TABLE readtable) (CHAR-CODE char))
	     (eq (char-syntx char readtable) :non-terminating-macro)))

(defun* SET-SYNTAX-FROM-CHAR (to-char from-char
			      &optional (to-readtable *READTABLE*)
			                (from-readtable *standard-readtable*))
  (setf (aref (READTABLE-SYNTAX-TYPE to-readtable) (CHAR-CODE to-char))
	(char-syntx from-char from-readtable))
  T)

(cl:defmacro WITH-STANDARD-IO-SYNTAX (&body body)
  `(LET ((*PACKAGE*			*cl-user-package*)
	 (*PRINT-ARRAY*			T)
	 (*PRINT-BASE*			10)
	 (*PRINT-CASE*			(kw UPCASE))
	 (*PRINT-CIRCLE*		nil)
	 (*PRINT-ESCAPE*		T)
	 (*PRINT-GENSYM*		T)
	 (*PRINT-LENGTH*		nil)
	 (*PRINT-LEVEL*			nil)
	 (*PRINT-LINES*			nil)
	 (*PRINT-MISER-WIDTH*		nil)
	 (*PRINT-PPRINT-DISPATCH*	*initial-pprint-dispatch*)
	 (*PRINT-PRETTY*		nil)
	 (*PRINT-RADIX*			nil)
	 (*PRINT-READABLY*		T)
	 (*PRINT-RIGHT-MARGIN*		nil)
	 (*READ-BASE*			10)
	 (*READ-DEFAULT-FLOAT-FORMAT*	'SINGLE-FLOAT)
	 (*READ-EVAL*			T)
	 (*READ-SUPPRESS*		nil)
	 (*READTABLE*			*standard-readtable*))
     ,@body))

(DEFVAR *READ-BASE* 10)

(DEFVAR *READ-DEFAULT-FLOAT-FORMAT* 'SINGLE-FLOAT)

(DEFVAR *READ-EVAL* T)

(DEFVAR *READ-SUPPRESS* nil)

(message "Loading (defvar *standard-readtable* ...)")

(defvar *standard-readtable*
  (let ((readtable (MAKE-READTABLE)))
    (setf (READTABLE-CASE readtable) (kw UPCASE))

    (setf (READTABLE-SYNTAX-TYPE readtable) (make-vector 256 :constituent))

    (do-plist (char type '(32 :whitespace
			   92 :single-escape
			   124 :multiple-escape))
      (setf (aref (READTABLE-SYNTAX-TYPE readtable) char) type))

    (dolist (char (mapcar #'CODE-CHAR '(9 10 12 13)))
      (SET-SYNTAX-FROM-CHAR char (ch 32) readtable readtable))

    (setf (READTABLE-MACRO-TABLE readtable) (make-vector 256 nil))
    (MAKE-DISPATCH-MACRO-CHARACTER (ch 35) T readtable)

    (do-plist (char fn (list (ch 34) #'double-quote-reader
			     (ch 39) #'quote-reader
			     (ch 40) #'left-paren-reader
			     (ch 41) #'right-paren-reader
			     (ch 44) #'comma-reader
			     (ch 59) #'semicolon-reader
			     (ch 96) #'backquote-reader))
      (SET-MACRO-CHARACTER char fn nil readtable))

    (setf (READTABLE-DISPATCH-TABLE readtable) (make-hash-table :test #'equal))

    (do-plist (char fn (list 92 #'sharp-backslash-reader
			     39 #'sharp-quote-reader
			     40 #'sharp-left-paren-reader
			     42 #'sharp-asterisk-reader
			     58 #'sharp-colon-reader
			     46 #'sharp-dot-reader
			     98 #'sharp-b-reader
			     66 #'sharp-b-reader
			     111 #'sharp-o-reader
			     79 #'sharp-o-reader
			     120 #'sharp-x-reader
			     88 #'sharp-x-reader
			     114 #'sharp-r-reader
			     82 #'sharp-r-reader
			     99 #'sharp-c-reader
			     67 #'sharp-c-reader
			     97 #'sharp-a-reader
			     65 #'sharp-a-reader
			     115 #'sharp-s-reader
			     83 #'sharp-s-reader
			     112 #'sharp-p-reader
			     80 #'sharp-p-reader
			     61 #'sharp-equal-reader
			     35 #'sharp-sharp-reader
			     43 #'sharp-plus-reader
			     45 #'sharp-minus-reader
			     124 #'sharp-bar-reader
			     60 #'sharp-less-reader
			     32 #'sharp-space-reader
			     41 #'sharp-right-paren-reader))
      (SET-DISPATCH-MACRO-CHARACTER (ch 35) (CODE-CHAR char) fn readtable))

    readtable))

(message "Loaded (defvar *standard-readtable* ...)")

(DEFVAR *READTABLE* (COPY-READTABLE nil))

;;; READER-ERROR defined in cl-conditions.el.

(defun whitespacep (char)
  (eq (char-syntx char) :whitespace))

(defun constituentp (char)
  (eq (char-syntx char) :constituent))



(defun dispatch-reader (stream char1)
  (do* ((param nil)
	(char (READ-CHAR stream T nil T) (READ-CHAR stream T nil T))
	(digit (DIGIT-CHAR-P char 10) (DIGIT-CHAR-P char 10)))
      ((not digit)
       (let ((fn (GET-DISPATCH-MACRO-CHARACTER char1 char)))
	 (cond
	   (fn			(FUNCALL fn stream char param))
	   (*READ-SUPPRESS*	(cl:values))
	   (t			nil))))
    (setq param (binary+ (binary* (or param 0) 10) digit))))

(defun double-quote-reader (stream double-quote-char)
  (do ((string "")
       (char (READ-CHAR stream T nil T) (READ-CHAR stream T nil T)))
      ((CHAR= char double-quote-char)
       (cl:values (if *READ-SUPPRESS* nil string)))
    (when (eq (char-syntx char) :single-escape)
      (setq char (READ-CHAR stream T nil T)))
    (unless *READ-SUPPRESS*
      (setq string (concat string (list (CHAR-CODE char)))))))

(defun quote-reader (stream ch)
  (let ((object (READ stream T nil T)))
    (unless *READ-SUPPRESS*
      (cl:values (list 'QUOTE object)))))

(defun* left-paren-reader (stream char)
  (do ((list nil)
       (char (PEEK-CHAR T stream) (PEEK-CHAR T stream)))
      ((ch= char 41)
       (READ-CHAR stream)
       (cl:values (nreverse list)))
    (MULTIPLE-VALUE-BIND (object nothingp) (read1 stream t nil t t t)
      (unless (or *READ-SUPPRESS* nothingp)
	(if (and (symbolp object) (string= (SYMBOL-NAME object) "."))
	    (let ((cdr (READ stream T nil T)))
	      (unless (ch= (READ-CHAR stream) 41)
		(ERROR 'READER-ERROR))
	      (return-from left-paren-reader
		(cl:values (nconc (nreverse list) cdr))))
	    (push object list))))))

(defun right-paren-reader (stream char)
  (unless *READ-SUPPRESS*
    (ERROR "Unbalanced '~A'." char)))

(defun comma-reader (stream char)
  (unless (or (plusp *backquote-level*) *READ-SUPPRESS*)
    (ERROR "Comma outside backquote."))
  (let ((next-char (READ-CHAR stream T nil T)))
    (let ((*backquote-level* (1- *backquote-level*)))
      (cond
	((ch= next-char 64)
	 (unless-read-suppress-let (object (READ stream T nil T))
	   (cl:values (list 'COMMA-AT object))))
	((ch= next-char 46)
	 (unless-read-suppress-let (object (READ stream T nil T))
	   (cl:values (list 'COMMA-DOT object))))
	(t
	 (UNREAD-CHAR next-char stream)
	 (unless-read-suppress-let (object (READ stream T nil T))
	   (cl:values (list 'COMMA object))))))))

(defun semicolon-reader (stream ch)
  (do ()
      ((ch= (READ-CHAR stream nil (ch 10) T) 10)
       (cl:values))))

(defun backquote-reader (stream char)
  (let* ((*backquote-level* (1+ *backquote-level*))
	 (form (READ stream T nil T)))
    (unless *READ-SUPPRESS*
      (cl:values (list 'BACKQUOTE form)))))

(defun no-param (char n)
  (when n
    (WARN "Parameter ~D ignored in #~C." n char)))

(defun sharp-backslash-reader (stream char n)
  (no-param (ch 92) n)
  (do ((token (concat (list (CHAR-CODE (READ-CHAR stream nil (ch 32) T)))))
       (char (READ-CHAR stream nil (ch 32) T)
	     (READ-CHAR stream nil (ch 32) T)))
      ((not (constituentp char))
       (UNREAD-CHAR char stream)
       (cl:values (cond
		    (*READ-SUPPRESS*		nil)
		    ((= (length token) 1)	(CHAR token 0))
		    (t				(NAME-CHAR token)))))
    (unless *READ-SUPPRESS*
      (setq token (concat token (list (CHAR-CODE char)))))))

(defun sharp-quote-reader (stream char n)
  (no-param (ch 39) n)
  (unless-read-suppress-let (object (READ stream T nil T))
    (cl:values (list 'FUNCTION object))))

(defun sharp-left-paren-reader (stream char n)
  (unless-read-suppress-let (list (READ-DELIMITED-LIST (ch 41) stream t))
    (cl:values
      (if (and n (plusp n))
	  (MAP-INTO (MAKE-ARRAY n (kw INITIAL-ELEMENT) (car (last list)))
		    #'IDENTITY list)
	  (CONCATENATE 'VECTOR list)))))

(defun bit-vector (contents n)
  (let* ((len (or n (length contents)))
	 (vec (make-bit-vector len (if (plusp len)
				       (car (last contents))
				       0))))
    (dotimes (i (min len (length contents)) vec)
      (setf (bref vec i) (nth i contents)))))

(defun sharp-asterisk-reader (stream char n)
  (do ((contents nil)
       (char (READ-CHAR stream nil (ch 32) T)
	     (READ-CHAR stream nil (ch 32) T)))
      ((not (constituentp char))
       (UNREAD-CHAR char stream)
       (cl:values (unless *READ-SUPPRESS* (bit-vector (nreverse contents) n))))
    (unless *READ-SUPPRESS*
      (push (ecase (CHAR-CODE char) (48 0) (49 1)) contents))))

(defun sharp-colon-reader (stream char n)
  (no-param (ch 58) n)
  (MULTIPLE-VALUE-BIND (package colons token escape) (read-token stream)
    (cl:values (unless *READ-SUPPRESS* (make-symbol token)))))

(defun sharp-dot-reader (stream char n)
  (no-param (ch 46) n)
  (unless-read-suppress-let (object (READ stream T nil T))
    (if *READ-EVAL*
	(cl:values (EVAL object))
	(ERROR 'READER-ERROR))))

(defun read-in-base (stream base)
  (let* ((*READ-BASE* base)
	 (num (READ stream T nil T)))
    (unless *READ-SUPPRESS*
      (if (RATIONALP num)
	  (cl:values num)
	  (ERROR 'READER-ERROR)))))

(defun sharp-b-reader (stream char n)
  (no-param (ch 66) n)
  (read-in-base stream 2))

(defun sharp-o-reader (stream char n)
  (no-param (ch 66) n)
  (read-in-base stream 8))

(defun sharp-x-reader (stream char n)
  (no-param (ch 66) n)
  (read-in-base stream 16))

(defun sharp-r-reader (stream char n)
  (when (or (null n)
	    (not (cl:<= 2 n 36)))
    (ERROR 'READER-ERROR))
  (read-in-base stream n))

(defun sharp-c-reader (stream char n)
  (no-param (ch 67) n)
  (let ((list (READ stream T nil T)))
    (unless *READ-SUPPRESS*
      (if (and (consp list) (= (length list) 2))
	  (cl:values (COMPLEX (first list) (second list)))
	  (ERROR "#C~S is not valid syntax for complex." list)))))

(defun array-content-dimensions (n contents)
  (cond
    ((zerop n)	nil)
    ((eq n 1)	(list (LENGTH contents)))
    (t		(cons (LENGTH contents)
		      (array-content-dimensions (1- n) (ELT contents 0))))))

(defun sharp-a-reader (stream char n)
  (unless-read-suppress-let (contents (READ stream T nil T))
    (unless n
      (ERROR 'READER-ERROR))
    (MAKE-ARRAY (array-content-dimensions n contents)
		(kw INITIAL-CONTENTS) contents)))

(message "Loading (defun sharp-s-reader ...)")

(defun sharp-s-reader (stream char n)
  (no-param (ch 83) n)
  (unless-read-suppress-let (contents (READ stream T nil T))
    (let ((type (first contents)))
      ;; TODO: Verify that there really is a constructor for the structure.
      (setq contents (cdr contents))
      (do ((list contents (cddr list)))
	  ((null list))
	(setf (car list) (INTERN (STRING (car list)) *keyword-package*)))
      (APPLY (INTERN (concat "MAKE-" (STRING type)) (SYMBOL-PACKAGE type))
	     contents))))

(message "Loaded (defun sharp-s-reader ...)")

(defun sharp-p-reader (stream char n)
  (no-param (ch 80) n)
  (unless-read-suppress-let (string (READ stream T nil T))
    (unless (STRINGP string)
      (ERROR 'READER-ERROR))
    (PARSE-NAMESTRING string)))

(defvar *sharp-equal-table* nil)

(defun replace-sharp-equal (tree object temp)
  (cond
    ((eq tree temp)
     object)
    ((consp tree)
     (RPLACA tree (replace-sharp-equal (car tree) object temp))
     (RPLACD tree (replace-sharp-equal (cdr tree) object temp)))
    ((arrayp tree)
     (dotimes (i (length tree) tree)
       (aset tree i
	     (replace-sharp-equal (aref tree i) object temp))))
    (t
     tree)))

(defun sharp-equal-reader (stream char n)
  (let ((temp nil))
    (unless *READ-SUPPRESS*
      (setf (gethash n *sharp-equal-table*)
	    (setq temp (cons nil nil))))
    (unless-read-suppress-let (object (READ stream T nil T))
      (replace-sharp-equal object object temp)
      (setf (gethash n *sharp-equal-table*) object))))

(defun sharp-sharp-reader (stream char n)
  (unless *READ-SUPPRESS*
    (let ((object (gethash n *sharp-equal-table* not-found)))
      (if (eq object not-found)
	  (ERROR "There is no object labelled #~D#" n)
	  object))))

(defun eval-feature-test (expr)
  (cond
    ((symbolp expr)
     (member expr *FEATURES*))
    ((atom expr)
     (ERROR "~S is not valid syntax in a feature test."))
    ((eq (first expr) (kw NOT))
     (not (eval-feature-test (second expr))))
    ((eq (first expr) (kw AND))
     (every #'eval-feature-test (rest expr)))
    ((eq (first expr) (kw OR))
     (some #'eval-feature-test (rest expr)))
    (t
     (ERROR 'READER-ERROR))))

(defun sharp-plus-reader (stream char n)
  (no-param (ch 43) n)
  (if (eval-feature-test (let ((*PACKAGE* *keyword-package*))
			   (READ stream T nil T)))
      (cl:values (READ stream T nil T))
      (let ((*READ-SUPPRESS* T))
	(READ stream T nil T)
	(cl:values))))

(defun sharp-minus-reader (stream char n)
  (no-param (ch 45) n)
  (if (eval-feature-test (let ((*PACKAGE* *keyword-package*))
			   (READ stream T nil T)))
      (let ((*READ-SUPPRESS* T))
	(READ stream T nil T)
	(cl:values))
      (cl:values (READ stream T nil T))))

(defun sharp-bar-reader (stream char n)
  (no-param (ch 124) n)
  (let ((level 1)
	(last nil)
	(char nil))
    (while (plusp level)
      (setq last char)
      (setq char (READ-CHAR stream T nil T))
      (when (and last (ch= last 35) (ch= char 124))
	(incf level))
      (when (and last (ch= last 124) (ch= char 35))
	(decf level)))
    (cl:values)))

(defun sharp-less-reader (stream char n)
  (ERROR "syntax error"))
(defun sharp-space-reader (stream char n)
  (ERROR "syntax error"))
(defun sharp-right-paren-reader (stream char n)
  (ERROR "syntax error"))



(cl:defmacro BACKQUOTE (form)
  (let ((result (expand-bq form)))
    (if t
	(optimize-bq result)
	result)))

(defun expand-bq (form)
  (cond
    ((consp form)
     (case (car form)
       (COMMA
	(second form))
       ((COMMA-AT COMMA-DOT)
	(ERROR "Syntax error in backquote."))
       (t
	(cons 'APPEND (expand-bq-list form)))))
    ((SIMPLE-VECTOR-P form)
     `(APPLY (FUNCTION VECTOR) ,(expand-bq (MAP 'LIST #'IDENTITY form))))
    (t
     `(QUOTE ,form))))

(defun* expand-bq-list (list)
  (let ((car (car list))
	(cdr (cdr list)))
    (cons
     (if (consp car)
	 (case (first car)
	   (COMMA			`(LIST ,(second car)))
	   ((COMMA-AT COMMA-DOT)	(second car))
	   (t				`(LIST ,(expand-bq car))))
	 (case car
	   (COMMA			(return-from expand-bq-list
					  (list (second list))))
	   ((COMMA-AT COMMA-DOT)	(ERROR "Syntax error in backquote."))
	   (t				`(LIST ,(expand-bq car)))))
     (if (consp cdr)
	 (expand-bq-list cdr)
	 `((QUOTE ,cdr))))))

(defun optimize-bq (form)
  (if (and (consp form)
	   (eq (first form) 'APPEND))
      (progn
	(setf (rest form) (remove-if (lambda (x)
				       (or (null x)
					   (equal x '(QUOTE nil))))
				     (rest form)))
	(let ((list (butlast (rest form)))
	      (tail (car (last (rest form)))))
	  (if (every (lambda (x) (and (consp x) (eq (first x) 'LIST))) list)
	      (progn
		(setq list (mapcar (lambda (x) (optimize-bq (second x)))
				   list))
		(if (and (consp tail) (eq (first tail) 'LIST))
		    `(LIST ,@list ,(second tail))
		    `(LIST* ,@list ,tail)))
	      form)))
      form))



(defun char-convert-case (char)
  (let ((case (READTABLE-CASE *READTABLE*)))
    (cond
      ((eq case (kw PRESERVE))	char)
      ((eq case (kw UPCASE))	(CHAR-UPCASE char))
      ((eq case (kw DOWNCASE))	(CHAR-DOWNCASE char))
      ((eq case (kw INVERT))	(ERROR "TODO: readtable case :invert."))
      (t			(type-error case `(MEMBER ,(kw PRESERVE)
							  ,(kw UPCASE)
							  ,(kw DOWNCASE)
							  ,(kw INVERT)))))))

(defun* process-token (package colons token escape)
  "Process a token and return the corresponding Lisp object.
   PACKAGE is a string, or nil if there was no package prefix.
   COLONS is the number of colons before the token.
   TOKEN is a string, or nil if there was no token after the colons.
   ESCAPE is t if any character in TOKEN was escaped, and nil otherwise."
  (when (and (zerop colons) (not escape))
    (let ((n (parse-number token)))
      (when n
	(return-from process-token n))))
  (when (null package)
    (case colons
      (0 (setq package *PACKAGE*))
      (1 (setq package *keyword-package*))
      (2 (ERROR "Too many colons in token."))))
  (when (null token)
    (ERROR "Token terminated by colon."))
  (MULTIPLE-VALUE-BIND (sym status) (FIND-SYMBOL token package)
    (cl:values
      (cond
	((or (eq status kw:EXTERNAL) (eq status kw:INHERITED))
	 sym)
	((eq status kw:INTERNAL)
	 (if (and (< colons 2) (not (eq package *PACKAGE*)))
	     (ERROR "Internal symbol.")
	     sym))
	((null status)
	 (NTH-VALUE 0 (INTERN token package)))))))

(defun potential-number-p (string)
  (and
   (every (lambda (char)
	    (or (DIGIT-CHAR-P (CODE-CHAR char) *READ-BASE*)
		(find char "+-/.^_DEFLSdefls")))
	  string)
   (or (some ;;(lambda (char) (DIGIT-CHAR-P (CODE-CHAR char)))
	     (compose DIGIT-CHAR-P CODE-CHAR)
	     string)
       (and (some (lambda (char)
		    (DIGIT-CHAR-P (CODE-CHAR char) *READ-BASE*))
		  string)
	    (not (find 46 string))))
   (let ((char (aref string 0)))
     (or (DIGIT-CHAR-P (CODE-CHAR char) *READ-BASE*)
	 (find char "+-.^_")))
   (not (find (aref string (1- (length string))) "+-"))))

(cl:defun parse-number (string)
  (catch 'parse-number
    ;; Cheap test to avoid many expensive computations below.
    (when (and (eq *READ-BASE* 10)
	       (not (string-match "^[-+0-9.]" string)))
      (throw 'parse-number (cl:values nil)))

    (MULTIPLE-VALUE-BIND (integer end)
	(PARSE-INTEGER string (kw RADIX) 10 (kw JUNK-ALLOWED) T)
      ;; First, is it a string of decimal digits followed by a period or an
      ;; exponent marker?  If so, can be either a decimal integer or a float.
      ;; TODO: PARSE-INTEGER doesn't differentiate between 0 and -0, so
      ;; e.g. -0.5 comes out wrong.
      (when (and integer
		 (< end (LENGTH string))
		 (FIND (CHAR string end) ".DEFLSdefls"))
	(if (and (eq (1+ end) (LENGTH string))
		 (ch= (CHAR string end) 46))
	    (throw 'parse-number (cl:values integer))
	    (let ((fraction 0)
		  (exponent 0)
		  (end2 end))
	      (when (ch= (CHAR string end) 46)
		(MULTIPLE-VALUE-SETQ (fraction end2)
		  (PARSE-INTEGER string (kw RADIX) 10 (kw START) (incf end)
				 (kw JUNK-ALLOWED) T))
		(when (eq end end2)
		  (setq fraction 0)))
	      (when (< end2 (LENGTH string))
		(unless (FIND (CHAR string end2) "DEFLSdefls")
		  (ERROR 'READ-ERROR))
		(MULTIPLE-VALUE-SETQ (exponent end2)
		  (PARSE-INTEGER string (kw RADIX) 10
				 (kw START) (1+ end2)
				 (kw JUNK-ALLOWED) T)))
	      (when (= end2 (LENGTH string))
		(case *READ-DEFAULT-FLOAT-FORMAT*
		  ((SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT))
		  (t (ERROR 'PARSE-ERROR)))
		(throw 'parse-number
		  (cl:values
		   (* (+ (FLOAT integer)
			 (* (if (MINUSP integer) -1 1)
			    (FLOAT fraction)
			    (expt 10.0 (- end end2))))
		      (expt 10.0 (FLOAT exponent)))))))))
      ;; Second, is it a period followed by a string of decimal digits?
      ;; TODO: minus sign.
      ;; TODO: exponent.
      (when (and (eq end 0)
		 (> (LENGTH string) 1)
		 (ch= (CHAR string 0) 46))
	(MULTIPLE-VALUE-BIND (fraction end2)
	    (PARSE-INTEGER string (kw RADIX) 10 (kw START) 1
			   (kw JUNK-ALLOWED) T)
	  (when (and integer (= end2 (LENGTH string)))
	    (throw 'parse-number
	      (cl:values
	       (* (FLOAT fraction) (expt 10.0 (- 1 (LENGTH string))))))))))
    ;; Third, try parsing as a number in current input radix.  It can
    ;; be either an integer or a ratio.
    (MULTIPLE-VALUE-BIND (integer end)
	(PARSE-INTEGER string (kw RADIX) *READ-BASE* (kw JUNK-ALLOWED) T)
      (unless integer
	(throw 'parse-number (cl:values nil)))
      (cond
	((= end (LENGTH string))
	 (throw 'parse-number (cl:values integer)))
	((ch= (CHAR string end) 47)
	 (MULTIPLE-VALUE-BIND (denumerator end2)
	     (PARSE-INTEGER string (kw RADIX) *READ-BASE*
			    (kw START) (1+ end) (kw JUNK-ALLOWED) T)
	   (when (and denumerator (= end2 (LENGTH string)))
	     (cl:values (cl:/ integer denumerator)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-reader.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-filenames.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 19, Filenames.

(IN-PACKAGE "EMACS-CL")

(define-storage-layout pathname (host device directory name type version))

(defun PATHNAME (pathspec)
  (cond
    ((PATHNAMEP pathspec)
     pathspec)
    ((STRINGP pathspec)
     ;; TODO: parse logical pathnames
     (cl:values (PARSE-NAMESTRING pathspec)))
    ((STREAMP pathspec)
     (PATHNAME (FILE-STREAM-filename pathspec)))
    (t
     (type-error pathspec '(OR PATHNAME STRING STREAM)))))

(defun mkpathname (host device directory name type version)
  (vector 'PATHNAME host device directory name type version))

(defun mklogpathname (host device directory name type version)
  (vector 'LOGICAL-PATHNAME host device directory name type version))

(cl:defun MAKE-PATHNAME (&KEY HOST DEVICE DIRECTORY NAME
			      TYPE VERSION DEFAULTS CASE)
  (unless DEFAULTS
    (setq DEFAULTS (mkpathname (PATHNAME-HOST *DEFAULT-PATHNAME-DEFAULTS*)
			       nil nil nil nil nil)))
  (when (eq DIRECTORY (kw WILD))
    (setq DIRECTORY `(,(kw ABSOLUTE) ,(kw WILD-INFERIORS))))
  (MERGE-PATHNAMES (mkpathname HOST DEVICE DIRECTORY NAME TYPE VERSION)
		   DEFAULTS nil))

(defun PATHNAMEP (object)
  (and (vectorp object)
       (or (eq (aref object 0) 'PATHNAME)
	   (eq (aref object 0) 'LOGICAL-PATHNAME))))

(defun PATHNAME-HOST (pathname-designator)
  (pathname-host (PATHNAME pathname-designator)))

(defun PATHNAME-DEVICE (pathname-designator)
  (pathname-device (PATHNAME pathname-designator)))

(defun PATHNAME-DIRECTORY (pathname-designator)
  (pathname-directory (PATHNAME pathname-designator)))

(defun PATHNAME-NAME (pathname-designator)
  (pathname-name (PATHNAME pathname-designator)))

(defun PATHNAME-TYPE (pathname-designator)
  (pathname-type (PATHNAME pathname-designator)))

(defun PATHNAME-VERSION (pathname-designator)
  (pathname-version (PATHNAME pathname-designator)))

;;; TODO: LOAD-LOGICAL-PATHNAME-TRANSLATIONS

(defvar *logical-pathname-translations* (make-hash-table))

(defun LOGICAL-PATHNAME-TRANSLATIONS (host)
  (gethash host *logical-pathname-translations*))

(defsetf LOGICAL-PATHNAME-TRANSLATIONS (host) (trans)
  `(puthash ,host ,trans *logical-pathname-translations*))

(DEFSETF LOGICAL-PATHNAME-TRANSLATIONS (host) (trans)
  `(puthash ,host ,trans *logical-pathname-translations*))

;;; *DEFAULT-PATHNAME-DEFAULTS* defined below.

(defun maybe-empty (component)
  (cond
    ((null component)			"")
    ((eq component (kw UNSPECIFIC))	"")
    ((eq component (kw NEWEST))		"")
    ((eq component (kw WILD))		"*")
    ((eq component (kw PREVIOUS))	"~")
    ((INTEGERP component)		(format ".~%d~" component))
    (t					component)))

(defun NAMESTRING (pathname-designator)
  (let* ((pathname (PATHNAME pathname-designator))
	 (dir (DIRECTORY-NAMESTRING pathname))
	 (name (FILE-NAMESTRING pathname))
	 (type (maybe-empty (PATHNAME-TYPE pathname)))
	 (ver (maybe-empty (PATHNAME-VERSION pathname))))
    (concat
     dir
     name
     (if (zerop (length type)) "" ".")
     type ver)))

(defun FILE-NAMESTRING (pathname-designator)
  (let* ((pathname (PATHNAME pathname-designator)))
    (maybe-empty (PATHNAME-NAME pathname))))

(defun DIRECTORY-NAMESTRING (pathname-designator)
  (let* ((pathname (PATHNAME pathname-designator))
	 (dir (PATHNAME-DIRECTORY pathname))
	 (string (cond
		   ((null dir) nil)
		   ((atom dir) (error "error"))
		   ((eq (first dir) (kw ABSOLUTE)) "/")
		   ((equal dir (list (kw RELATIVE))) "./")
		   (t ""))))
    (dolist (x (rest dir) string)
      (setq string
	    (concat string
		    (cond
		      ((STRINGP x)	x)
		      ((eq x (kw UP))	"..")
		      ((eq x (kw WILD))	"*")
		      ((eq x (kw WILD-INFERIORS))
					"**")
		      ((eq x (kw BACK))	(ERROR 'ERROR))
		      (t		(type-error
					 x `(OR STRING
					     (MEMBER
					      ,(kw BACK) ,(kw WILD) ,(kw UP)
					      ,(kw WILD-INFERIORS))))))
		    "/")))))

(defun HOST-NAMESTRING (pathname-designator)
  (let* ((pathname (PATHNAME pathname-designator)))
    (maybe-empty (PATHNAME-HOST pathname))))

(defun dir-subtract (dir1 dir2)
  (cond
    ((null dir1)
     (cons (kw RELATIVE) dir2))
    ((null dir2)
     nil)
    ((EQUAL (first dir1) (first dir2))
     (dir-subtract (rest dir1) (rest dir2)))))

(cl:defun ENOUGH-NAMESTRING (pathname-designator &OPTIONAL
			     (defaults *DEFAULT-PATHNAME-DEFAULTS*))
  ;; It is required that
  ;;   (merge-pathnames (enough-namestring pathname defaults) defaults)
  ;;   == (merge-pathnames (parse-namestring pathname nil defaults) defaults)
  (let ((pathname (PATHNAME pathname-designator)))
    (let ((candidates (list (NAMESTRING pathname)))
	  (shortest nil))
      (when (and (EQUAL (PATHNAME-HOST pathname) (PATHNAME-HOST defaults))
		 (EQUAL (PATHNAME-DEVICE pathname) (PATHNAME-DEVICE defaults))
		 (consp (PATHNAME-DIRECTORY pathname))
		 (eq (first (PATHNAME-DIRECTORY pathname)) (kw ABSOLUTE)))
	(let ((dir (dir-subtract (PATHNAME-DIRECTORY defaults)
				 (PATHNAME-DIRECTORY pathname))))
	  (when dir
	    (push (NAMESTRING (MAKE-PATHNAME (kw DIRECTORY) dir
					     (kw DEFAULTS) pathname))
		  candidates))))
      (FIND-IF (lambda (len)
		 (or (null shortest)
		     (when (< len shortest)
		       (setq shortest len))))
	       candidates (kw KEY) #'LENGTH))))

(defun slashp (char)
  (ch= char 47))

(defun parse-dir (string)
  (when string
    (when (ch= (CHAR string 0) 126)
      (setq string (expand-file-name string)))
    (let* ((start 0)
	   (dir (if (slashp (CHAR string 0))
		    (progn (incf start)
			   (list (kw ABSOLUTE)))
		    (list (kw RELATIVE)))))
      (do ((i start)
	   (j 1 (1+ j)))
	  ((eq j (LENGTH string))
	   (when (> j i)
	     (push (SUBSEQ string i j) dir)))
	(when (slashp (CHAR string j))
	  (let ((component (SUBSEQ string i j)))
	    (cond
	      ((STRING= component "*")		(push (kw WILD) dir))
	      ((STRING= component "**")		(push (kw WILD-INFERIORS) dir))
	      ((STRING= component "..")		(push (kw UP) dir))
	      ((STRING= component "."))		;Nothing.
	      (t				(push component dir))))
	  (setq i (1+ j))))
      (nreverse dir))))

(DEFVAR *DEFAULT-PATHNAME-DEFAULTS*
  (mkpathname nil nil (parse-dir default-directory) nil nil nil))

(defun parse-ver (name string)
  (if (STRING= name "")
      nil
      (cond
	((STRING= string "")		(kw NEWEST))
	((STRING= string "~")		(kw PREVIOUS))
	((and (ch= (CHAR string 0) 46)
	      (ch= (CHAR string 1) 126)
	      (ch= (CHAR string (1- (LENGTH string))) 126))
					(PARSE-INTEGER string (kw START) 2
						       (kw JUNK-ALLOWED) t))
	(t				(error "invalid version")))))

(defun maybe-wild (string)
  (cond
    ((null string)		nil)
    ((STRING= string "")	nil)
    ((STRING= string "*")	(kw WILD))
    (t				string)))

(cl:defun PARSE-NAMESTRING (thing &OPTIONAL host
			    (default *DEFAULT-PATHNAME-DEFAULTS*)
			    &KEY (START 0) END JUNK-ALLOWED)
  (cond
    ((STREAMP thing)
     (PARSE-NAMESTRING (FILE-STREAM-filename thing) host default
		       (kw START) START (kw END) END
		       (kw JUNK-ALLOWED) JUNK-ALLOWED))
    ((PATHNAMEP thing)
     (if (EQUAL (PATHNAME-HOST thing) host)
	 (cl:values thing START)
	 (ERROR 'ERROR)))
    ((STRINGP thing)
     ;; TODO: parse logical pathnames
     (let* ((string (SUBSEQ thing START END))
	    (dir (parse-dir (file-name-directory string)))
	    (name+ver (file-name-nondirectory string))
	    (name-ver (file-name-sans-versions name+ver))
	    (ver (parse-ver name-ver (substring name+ver (length name-ver))))
	    (name (maybe-wild (file-name-sans-extension name-ver)))
	    (type (maybe-wild (file-name-extension name+ver))))
;        (FORMAT T "~&dir=~S name+ver=~S name-ver=~S ver=~S name=~S type=~S"
; 	       dir name+ver name-ver ver name type)
       (cond
	 ((string= name+ver ".")
	  (when (null dir)
	    (setq dir (list (kw RELATIVE))))
	  (setq name nil))
	 ((string= name+ver "..")
	  (setq dir (if (null dir)
			(list (kw RELATIVE) (kw UP))
			(append dir (list (kw UP)))))
	  (setq name nil))
	 ((null name)
	  (setq name name-ver
		type nil))
	 ((null type)
	  (unless (string= name-ver "*.")
	    (setq name name-ver))))
       (when (string= name "")
	 (setq name nil))
       (cl:values (mkpathname nil nil dir name type ver)
		  (or END (LENGTH thing)))))
    (t
     (type-error thing '(OR PATHNAME STRING STREAM)))))

(cl:defun WILD-PATHNAME-P (pathname-designator &OPTIONAL field)
  (let ((pathname (PATHNAME pathname-designator)))
    (cond
      ((eq field (kw HOST))
       (eq (PATHNAME-HOST pathname) (kw WILD)))
      ((eq field (kw DEVICE))
       (eq (PATHNAME-DEVICE pathname) (kw WILD)))
      ((eq field (kw DIRECTORY))
       (or (memq (kw WILD) (PATHNAME-DIRECTORY pathname))
	   (memq (kw WILD-INFERIORS) (PATHNAME-DIRECTORY pathname))))
      ((eq field (kw NAME))
       (eq (PATHNAME-NAME pathname) (kw WILD)))
      ((eq field (kw TYPE))
       (eq (PATHNAME-TYPE pathname) (kw WILD)))
      ((eq field (kw VERSION))
       (eq (PATHNAME-VERSION pathname) (kw WILD)))
      ((null field)
       (some (lambda (f) (WILD-PATHNAME-P pathname f))
	     `(,(kw HOST) ,(kw DEVICE) ,(kw DIRECTORY)
	       ,(kw NAME) ,(kw TYPE) ,(kw VERSION))))
      (t
       (type-error field `(MEMBER NULL ,(kw HOST) ,(kw DEVICE) ,(kw DIRECTORY)
			          ,(kw NAME) ,(kw TYPE) ,(kw VERSION)))))))

(defmacro wild-test (fn pathname wildcard)
  `(or (eq (,fn ,wildcard) (kw WILD))
       (equal (,fn ,wildcard) (,fn ,pathname))
       ,@(when (or (eq fn 'PATHNAME-NAME) (eq fn 'PATHNAME-TYPE))
	   `((and (or (null (,fn ,pathname)) (string= (,fn ,pathname) ""))
	          (string= (,fn ,wildcard) ""))))))

(defun directories-match-p (pathname wildcard)
  (cond
    ((null pathname)
     (null wildcard))
    ((null wildcard)
     nil)
    ((eq (first wildcard) (kw WILD-INFERIORS))
     (if (null (rest wildcard))
	 T
	 (some (lambda (p) (directories-match-p p (rest wildcard)))
	       (maplist #'IDENTITY pathname))))
    ((wild-test first pathname wildcard)
     (directories-match-p (rest pathname) (rest wildcard)))))

(defvar *wild-pathname* (mkpathname (kw WILD) (kw WILD) (kw WILD)
				    (kw WILD) (kw WILD) (kw WILD)))

(defun PATHNAME-MATCH-P (pathname-designator wildcard)
  (let ((pathname (PATHNAME pathname-designator))
	(wildcard (MERGE-PATHNAMES wildcard *wild-pathname* (kw WILD))))
    (and (wild-test PATHNAME-HOST pathname wildcard)
	 (wild-test PATHNAME-DEVICE pathname wildcard)
	 (or (wild-test PATHNAME-DIRECTORY pathname wildcard)
	     (directories-match-p (PATHNAME-DIRECTORY pathname)
				  (PATHNAME-DIRECTORY wildcard)))
	 (wild-test PATHNAME-NAME pathname wildcard)
	 (wild-test PATHNAME-TYPE pathname wildcard)
	 (wild-test PATHNAME-VERSION pathname wildcard))))

;;; TODO: TRANSLATE-LOGICAL-PATHNAME

(defun wild-or-nil (x y)
  (if (or (null x) (eq x (kw WILD)))
      y
      x))

(defun translate-dir (source from to)
  (cond
    ((null to)
     nil)
    ((eq (first to) (kw WILD))
     (let ((pos (position (kw WILD) from)))
       (if pos
	   (cons (nth pos source)
		 (translate-dir (nthcdr (incf pos) source)
				(nthcdr pos from) (rest to)))
	   (ERROR 'ERROR))))
    (t
     (cons (first to) (translate-dir source from (rest to))))))

(defun TRANSLATE-PATHNAME (source from-wildcard to-wildcard)
  (let ((source (PATHNAME source))
	(from-wildcard (PATHNAME from-wildcard))
	(to-wildcard (PATHNAME to-wildcard)))
    (mkpathname
     (wild-or-nil (PATHNAME-HOST to-wildcard) (PATHNAME-HOST source))
     (wild-or-nil (PATHNAME-DEVICE to-wildcard) (PATHNAME-DEVICE source))
     (translate-dir (PATHNAME-DIRECTORY source)
		    (PATHNAME-DIRECTORY from-wildcard)
		    (PATHNAME-DIRECTORY to-wildcard))
     (wild-or-nil (PATHNAME-NAME to-wildcard) (PATHNAME-NAME source))
     (wild-or-nil (PATHNAME-TYPE to-wildcard) (PATHNAME-TYPE source))
     (wild-or-nil (PATHNAME-VERSION to-wildcard) (PATHNAME-VERSION source)))))

(defun merge-directories (dir1 dir2)
  ;; TODO: Proper handling of directory component.
  (cond
    ((null dir1)
     dir2)
    ((and (consp dir1) (eq (first dir1) (kw RELATIVE))
	  (consp dir2) (eq (first dir2) (kw ABSOLUTE)))
     (append dir2 (rest dir1)))
    (t
     dir1)))

(cl:defun MERGE-PATHNAMES (pathname-designator
			   &OPTIONAL
			   (default-designator *DEFAULT-PATHNAME-DEFAULTS*)
			   (default-version (kw NEWEST)))
  (let ((pathname (PATHNAME pathname-designator))
	(default (PATHNAME default-designator)))
    ;; TODO: read spec more closely.
    (mkpathname (or (PATHNAME-HOST pathname) (PATHNAME-HOST default))
		(or (PATHNAME-DEVICE pathname) (PATHNAME-DEVICE default))
		(merge-directories (PATHNAME-DIRECTORY pathname) (PATHNAME-DIRECTORY default))
		(or (PATHNAME-NAME pathname) (PATHNAME-NAME default))
		(or (PATHNAME-TYPE pathname) (PATHNAME-TYPE default))
		(or (PATHNAME-VERSION pathname)
		    (if (PATHNAME-NAME pathname)
			default-version
			(or (PATHNAME-VERSION default) default-version))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-filenames.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-loop.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements the LOOP macro from chapter 6, Iteration.

(IN-PACKAGE "EMACS-CL")

(cl:defmacro LOOP (&rest forms)
  (if (every #'consp forms)
      `(DO () (nil) ,@forms)
      (expand-extended-loop forms)))

(defstruct (loop-state
	     (:conc-name nil)
	     (:constructor make-loop-state
			   (loop-forms &optional loop-accumulator)))
  loop-forms
  loop-name
  loop-bindings
  loop-accumulator
  loop-prologue
  loop-tests
  loop-setters
  loop-body
  loop-steps
  loop-epilogue
  loop-result)

(defun copy-loop-state (state)
  (let ((s (make-loop-state (loop-forms state))))
    (setf (loop-accumulator s) (loop-accumulator state))
    s))

(defun merge-loop-states (s1 s2)
  (setf (loop-forms s1)    (loop-forms s2))
  (when (loop-name s2)     (setf (loop-name s1) (loop-name s2)))
  (setf (loop-bindings s1) (append (loop-bindings s2) (loop-bindings s1)))
  (when (loop-accumulator s2)
    (setf (loop-accumulator s1) (loop-accumulator s2)))
  (setf (loop-prologue s1) (append (loop-prologue s2) (loop-prologue s1)))
  (setf (loop-tests s1)    (append (loop-tests s2) (loop-tests s1)))
  (setf (loop-setters s1)  (append (loop-setters s2) (loop-setters s1)))
  (setf (loop-steps s1)    (append (loop-steps s2) (loop-steps s1)))
  (setf (loop-epilogue s1) (append (loop-epilogue s2) (loop-epilogue s1)))
  (when (loop-result s2)   (setf (loop-result s1) (loop-result s2)))
  s1)

(defmacro merge-loop-state (state)
  `(progn
    (setq forms (loop-forms ,state))
    (when (loop-name ,state)
      (setq name (loop-name ,state)))
    (setq bindings
     (append (loop-bindings ,state) bindings))
    (when (loop-accumulator ,state)
      (setq accumulator (loop-accumulator ,state)))
    (setq prologue
     (append (loop-prologue ,state) prologue))
    (setq tests
     (append (loop-tests ,state) tests))
    (setq setters
     (append (loop-setters ,state) setters))
    (setq steps
     (append (loop-steps ,state) steps))
    (setq epilogue
     (append (loop-epilogue ,state) epilogue))
    (when (loop-result ,state)
      (setq result (loop-result ,state)))))

(defvar *loop-clause-handlers* (make-hash-table :test #'equal))

(defmacro* define-loop-clause (names () &body body)
  (with-gensyms (state)
    `(dolist (name ',(ensure-list names))
      (setf (gethash name *loop-clause-handlers*)
            (lambda (,state)
	      (let ((forms		(loop-forms ,state))
		    (name		(loop-name ,state))
		    (bindings		(loop-bindings ,state))
		    (accumulator	(loop-accumulator ,state))
		    (prologue		(loop-prologue ,state))
		    (tests		(loop-tests ,state))
		    (setters		(loop-setters ,state))
		    (body		(loop-body ,state))
		    (steps		(loop-steps ,state))
		    (epilogue		(loop-epilogue ,state))
		    (result		(loop-result ,state)))
		,@body
		(setf (loop-forms ,state) forms)
		(setf (loop-name ,state) name)
		(setf (loop-bindings ,state) bindings)
		(setf (loop-accumulator ,state) accumulator)
		(setf (loop-prologue ,state) prologue)
		(setf (loop-tests ,state) tests)
		(setf (loop-setters ,state) setters)
		(setf (loop-body ,state) body)
		(setf (loop-steps ,state) steps)
		(setf (loop-epilogue ,state) epilogue)
		(setf (loop-result ,state) result)
		,state))))))

(defmacro peek= (&rest names)
  `(member (symbol-name (first forms)) ',names))

(defmacro next= (&rest names)
  `(member (symbol-name (pop forms)) ',names))

(define-loop-clause "NAMED" ()
  (setq name (pop forms)))

(define-loop-clause "WITH" ()
  (let ((bs nil)
	(more t))
    (while more
      (let* ((var (pop forms))
	     (val (when (peek= "=")
		    (pop forms)
		    (pop forms))))
	(push `(,var ,val) bs))
      (setq more (prog1 (peek= "AND")))
      (when more
	(pop forms)))
    (push bs bindings)))

;; (defun destructuring-setq-form (lambda-list form)
;;   `(MULTIPLE-VALUE-SETQ ,(lambda-list-variables lambda-list)
;;      (DESTRUCTURING-BIND ,lambda-list ,form
;;        (VALUES ,@(lambda-list-variables lambda-list)))))

(defun assignment-form (var form)
  (cond
    ((null var)
     nil)
    ((symbolp var)
     `(SETQ ,var ,form))
    ((consp var)
     (let ((val (gensym)))
       `(LET ((,val ,form))
	  ,@(do ((forms nil)
		 (vars var (rest vars)))
		((atom vars)
		 (unless (null vars)
		   (push `(SETQ ,vars ,val) forms))
		 (nreverse forms))
	      (push (assignment-form (first vars) `(POP ,val)) forms)))))
;;        `(LET ((,val ,form))
;; 	 ,(assignment-form (car var) `(CAR ,val))
;; 	 ,(assignment-form (cdr var) `(CDR ,val)))))
;;      `(MULTIPLE-VALUE-SETQ ,(lambda-list-variables lambda-list)
;;         (DESTRUCTURING-BIND ,lambda-list ,form
;; 	  (VALUES ,@(lambda-list-variables lambda-list)))))
    (t
     (type-error var '(OR SYMBOL CONS)))))

(define-loop-clause ("FOR" "AS") ()
  (let* ((var (pop forms))
	 (form (pop forms))
	 (k (symbol-name form)))
    (cond
      ((member k '("FROM" "UPFROM" "TO" "UPTO" "BELOW" "DOWNTO"
		   "ABOVE" "DOWNFROM"))
       (parse-for-arithmetic var form))
       
      ((string= k "IN")
       (let ((list (gensym))
	     (form (pop forms))
	     (by-fn '(FUNCTION CDR)))
	 (when (and forms
		    (symbolp (first forms))
		    (equal (symbol-name (first forms)) "BY"))
	   (pop forms)
	   (setq by-fn (pop forms)))
	 (push `((,list ,form)
		 ,@(if (atom var) (list var) (lambda-list-variables var)))
	       bindings)
	 (push `(ENDP ,list) tests)
	 (push (assignment-form var `(CAR ,list)) setters)
;; 	 (push (if (atom var)
;; 		   `(SETQ ,var (CAR ,list))
;; 		   (destructuring-setq-form var `(CAR ,list)))
;; 	       setters)
	 (push `(SETQ ,list (FUNCALL ,by-fn ,list)) steps)))

      ((string= k "ON")
       (let ((list (gensym))
	     (form (pop forms))
	     (by-fn '(FUNCTION CDR)))
	 (when (and forms
		    (symbolp (first forms))
		    (equal (symbol-name (first forms)) "BY"))
	   (pop forms)
	   (setq by-fn (pop forms)))
	 (push `((,list ,form)
		 ,@(if (atom var) (list var) (lambda-list-variables var)))
	       bindings)
	 (push `(ATOM ,list) tests)
	 (push (assignment-form var list) setters)
;; 	 (push (if (atom var)
;; 		   `(SETQ ,var ,list)
;; 		   (destructuring-setq-form var list))
;; 	       setters)
	 (push `(SETQ ,list (FUNCALL ,by-fn ,list)) steps)))
;; 	 (push `((,var ,form)) bindings)
;; 	 (push `(ATOM ,var) tests)
;; 	 (push `(SETQ ,var (FUNCALL ,by-fn ,var)) steps)))

      ((string= k "=")
       (let ((form1 (pop forms)))
	 (cond
	   ((peek= "THEN")
	    (pop forms)
	    (let ((form2 (pop forms)))
	      (push `((,var ,form1)) bindings)
	      (push (assignment-form var form2) steps)))
;; 	      (push (if (atom var)
;; 			`(SETQ ,var ,form2)
;; 			(destructuring-setq-form var form2))
;; 		    steps)))
	   (t
	    (push (if (atom var) (list var) (lambda-list-variables var))
		  bindings)
	    (push (assignment-form var form1) setters)))))
;; 	    (push (if (atom var)
;; 		      `(SETQ ,var ,form1)
;; 		      (destructuring-setq-form var form1))
;; 		  setters)))))

      ((string= k "ACROSS")
       (with-gensyms (vector index length)
	 (push `(,var (,index 0) (,vector ,(pop forms))) bindings)
	 (push `((,length (just-one (ARRAY-DIMENSIONS ,vector)))) bindings)
	 (push `(EQ ,index ,length) tests)
	 (push (assignment-form var `(AREF ,vector ,index)) setters)
;;	 (push `(SETQ ,var (AREF ,vector ,index)) setters)
	 (push `(INCF ,index) steps)))

      ((string= k "BEING")
       (setq k (symbol-name (pop forms)))
       (when (or (string= k "THE") (string= k "EACH"))
	 (setq k (symbol-name (pop forms))))
       (let ((k (symbol-name (pop forms))))
	 (unless (or (string= k "IN") (string= k "OF"))
	   (ERROR "Unknown LOOP BEING keyword: ~A" k)))
       (let ((list (gensym)))
	 (push `((,list (package-symbols
			 (OR (FIND-PACKAGE ,(pop forms))
			     (ERROR (QUOTE PACKAGE-ERROR)))
			 ,(cond
			   ((or (string= k "SYMBOL")
				(string= k "SYMBOLS"))
			    `(QUOTE (,kw:EXTERNAL
				     ,kw:INTERNAL
				     ,kw:INHERITED)))
			   ((or (string= k "PRESENT-SYMBOL")
				(string= k "PRESENT-SYMBOLS"))
			    `(QUOTE (,kw:EXTERNAL
				     ,kw:INTERNAL)))
			   ((or (string= k "EXTERNAL-SYMBOL")
				(string= k "EXTERNAL-SYMBOLS"))
			    `(QUOTE (,kw:EXTERNAL)))
			   (t
			    (ERROR "Invalid LOOP keyword: ~A" k)))))
		 ,var)
	       bindings)
	 (push `(NULL ,list) tests)
	 (push `(SETQ ,var (CAAR ,list)) setters)
	 (push `(SETQ ,list (CDR ,list)) steps)))))
  ;; TODO: this is a gross hack!
  (when (and forms
	     (symbolp (first forms))
	     (string= (symbol-name (first forms)) "AND"))
    (pop forms)
    (push 'FOR forms)))

(defun parse-for-arithmetic (var k)
  (let ((start-key nil)
	(start-form nil)
	(end-key nil)
	(end-form nil)
	(by-form 1)
	(step-fn nil)
	(test-fn nil)
	(incf (INTERN "INCF" *cl-package*))
	(decf (INTERN "DECF" *cl-package*))
	(lt (INTERN "<" *cl-package*))
	(gt (INTERN ">" *cl-package*))
	(le (INTERN "<=" *cl-package*))
	(ge (INTERN ">=" *cl-package*)))
    (flet ((parse-preposition (form)
	     (let ((k (symbol-name form)))
	       (cond
		 ((string= k "FROM")
		  (when start-key
		    (ERROR "Only one of FROM, DOWNFROM, or UPFROM allowed"))
		  (setq start-form (pop forms))
		  (setq start-key k))
		 ((string= k "DOWNFROM")
		  (when start-key
		    (ERROR "Only one of FROM, DOWNFROM, or UPFROM allowed"))
		  (if (eq step-fn incf)
		      (ERROR "DOWNFROM implies decrementing stepping")
		      (setq step-fn decf))
		  (setq start-form (pop forms))
		  (setq start-key k))
		 ((string= k "UPFROM")
		  (when start-key
		    (ERROR "Only one of FROM, DOWNFROM, or UPFROM allowed"))
		  (if (eq step-fn decf)
		      (ERROR "UPFROM implies incrementing stepping")
		      (setq step-fn incf))
		  (setq start-key k)
		  (setq start-form (pop forms)))
		 ((string= k "TO")
		  (when end-key
		    (ERROR
		     "Only one of TO, DOWNTO, UPTO, BELOW, or ABOVE allowed"))
		  (setq end-form (pop forms))
		  (setq end-key k))
		 ((string= k "DOWNTO")
		  (when end-key
		    (ERROR
		     "Only one of TO, DOWNTO, UPTO, BELOW, or ABOVE allowed"))
		  (if (eq step-fn incf)
		      (ERROR "UPTO implies incrementing stepping")
		      (setq step-fn decf))
		  (setq test-fn lt)
		  (setq end-form (pop forms))
		  (setq end-key k))
		 ((string= k "UPTO")
		  (when end-key
		    (ERROR
		     "Only one of TO, DOWNTO, UPTO, BELOW, or ABOVE allowed"))
		  (if (eq step-fn decf)
		      (ERROR "UPTO implies incrementing stepping")
		      (setq step-fn incf))
		  (setq test-fn gt)
		  (setq end-form (pop forms))
		  (setq end-key k))
		 ((string= k "BELOW")
		  (when end-key
		    (ERROR
		     "Only one of TO, DOWNTO, UPTO, BELOW, or ABOVE allowed"))
		  (if (eq step-fn decf)
		      (ERROR "BELOW implies incrementing stepping")
		      (setq step-fn incf))
		  (setq test-fn ge)
		  (setq end-form (pop forms))
		  (setq end-key k))
		 ((string= k "ABOVE")
		  (when end-key
		    (ERROR
		     "Only one of TO, DOWNTO, UPTO, BELOW, or ABOVE allowed"))
		  (if (eq step-fn incf)
		      (ERROR "BELOW implies decremental stepping")
		      (setq step-fn decf))
		  (setq test-fn le)
		  (setq end-form (pop forms))
		  (setq end-key k))
		 ((string= k "BY")
		  (setq by-form (pop forms))
		  t)
		 (t
		  (push form forms)
		  nil)))))
      (parse-preposition k)
      (while (and forms (parse-preposition (pop forms))))
      (setq step-fn (or step-fn incf))
      (setq start-form (or start-form (when (eq step-fn incf) 0)))
      (unless test-fn
	(setq test-fn (if (string= start-key "DOWNFROM") lt gt)))
      ;(print (format "%s %s %s %s %s BY %s %s" start-key start-form end-key test-fn end-form step-fn by-form))
      ;(FORMAT T "~S ~S ~S ~S ~S BY ~S ~S" start-key start-form end-key test-fn end-form step-fn by-form)
      (when end-form
	(with-gensyms (end)
	  (push `((,end ,end-form)) bindings)
	  (push `(,test-fn ,var ,end) tests)))
      (push `((,var ,start-form)) bindings)
      (push `(,step-fn ,var ,by-form) steps))))

(defvar *loop-collect* (gensym))
(defvar *loop-append* (gensym))
(defvar *loop-nconc* (gensym))
(defvar *loop-count* (gensym))
(defvar *loop-sum* (gensym))
(defvar *loop-max* (gensym))
(defvar *loop-min* (gensym))

(define-loop-clause ("COLLECT" "COLLECTING") ()
  (let ((form (pop forms)))
    (if accumulator
	(unless (eq accumulator *loop-collect*)
	  (ERROR "LOOP accumulator error"))
	(setq accumulator *loop-collect*))
    (push `((,accumulator nil)) bindings)
    (push `(PUSH ,form ,accumulator) body)
    (setf result `(NREVERSE ,accumulator))))

(define-loop-clause ("APPEND" "APPENDING") ()
  (let ((form (pop forms)))
    (if accumulator
	(unless (eq accumulator *loop-append*)
	  (ERROR "LOOP accumulator error"))
	(setq accumulator *loop-append*))
    (push `((,accumulator nil)) bindings)
    (push `(SETQ ,accumulator (APPEND ,accumulator ,form)) body)
    (setf result accumulator)))

(define-loop-clause ("NCONC" "NCONCING") ()
  (let ((form (pop forms)))
    (if accumulator
	(unless (eq accumulator *loop-nconc*)
	  (ERROR "LOOP accumulator error"))
	(setq accumulator *loop-nconc*))
    (push `((,accumulator nil)) bindings)
    (push `(SETQ ,accumulator (NCONC ,accumulator ,form)) body)
    (setf result accumulator)))

(define-loop-clause ("COUNT" "COUNTING") ()
  (let ((form (pop forms)))
    (if accumulator
	(unless (eq accumulator *loop-count*)
	  (ERROR "LOOP accumulator error"))
	(setq accumulator *loop-count*))
    (push `((,accumulator 0)) bindings)
    (push `(WHEN ,form (INCF ,accumulator)) body)
    (setf result accumulator)))

(define-loop-clause ("SUM" "SUMMING") ()
  (let ((form (pop forms)))
    (if accumulator
	(unless (eq accumulator *loop-sum*)
	  (ERROR "LOOP accumulator error"))
	(setq accumulator *loop-sum*))
    (push `((,accumulator 0)) bindings)
    (push `(INCF ,accumulator ,form) body)
    (setf result accumulator)))

(define-loop-clause ("MAXIMIZE" "MAXIMIZING") ()
  (let ((form (pop forms))
	(val (gensym)))
    (if accumulator
	(unless (eq accumulator *loop-max*)
	  (ERROR "LOOP accumulator error"))
	(setq accumulator *loop-max*))
    (push `((,accumulator nil)) bindings)
    (push `(SETQ ,accumulator (LET ((,val ,form))
				(IF ,accumulator
				    (MAX ,accumulator ,val)
				    ,val)))
	  body)
    (setf result accumulator)))

(define-loop-clause ("MINIMIZE" "MINIMIZING") ()
  (let ((form (pop forms))
	(val (gensym)))
    (if accumulator
	(unless (eq accumulator *loop-min*)
	  (ERROR "LOOP accumulator error"))
	(setq accumulator *loop-min*))
    (push `((,accumulator nil)) bindings)
    (push `(SETQ ,accumulator (LET ((,val ,form))
				(IF ,accumulator
				    (MIN ,accumulator ,val)
				    ,val)))
	  body)
    (setf result accumulator)))

(define-loop-clause "REPEAT" ()
  (let ((end (pop forms))
	(var (gensym)))
    (push `((,var 0)) bindings)
    (push `(,(INTERN ">=") ,var ,end) tests)
    (push `(SETQ ,var (,(INTERN "1+" *cl-package*) ,var)) setters)))

(define-loop-clause "ALWAYS" ()
  (let ((val (gensym)))
    (setf result T)
    (push `(UNLESS ,(pop forms) (RETURN-FROM ,name nil)) body)))

(define-loop-clause "NEVER" ()
  (let ((val (gensym)))
    (setf result T)
    (push `(WHEN ,(pop forms) (RETURN-FROM ,name nil)) body)))

(define-loop-clause "THEREIS" ()
  (let ((val (gensym)))
    (setf result nil)
    (push `(LET ((,val ,(pop forms)))
	    (WHEN ,val (RETURN-FROM ,name ,val)))
	  body)))

(define-loop-clause "WHILE" ()
  (push `(NOT ,(pop forms)) tests))

(define-loop-clause "UNTIL" ()
  (push (pop forms) tests))

(define-loop-clause ("IF" "WHEN") ()
  (let ((condition (pop forms))
	(then nil)
	(else nil))
   (let ((s (parse-loop-clause (make-loop-state forms accumulator))))
     (setq then (loop-body s))
     (setf (loop-body s) nil)
     (merge-loop-state s))
   (when (string= (symbol-name (first forms)) "ELSE")
     (pop forms)
     (let ((s (parse-loop-clause (make-loop-state forms accumulator))))
       (setq else (loop-body s))
       (setf (loop-body s) nil)
       (merge-loop-state s)))
   (push `(IF ,condition
	      (PROGN ,@then)
	      ,(when else `(PROGN ,@else)))
	 body)))

(define-loop-clause "UNLESS" ()
  (let ((form (pop forms)))
    (setq forms `(WHEN (NOT ,form) ,@forms))))

(define-loop-clause ("DO" "DOING") ()
  (while (consp (first forms))
    (push (pop forms) body)))

(define-loop-clause "RETURN" ()
  (push `(RETURN-FROM ,name ,(pop forms)) body))

(define-loop-clause "INITIALLY" ()
  (while (consp (first forms))
    (push (pop forms) prologue)))

(define-loop-clause "FINALLY" ()
  (while (consp (first forms))
    (push (pop forms) epilogue)))

(defun parse-loop-clause (state)
  (let* ((k (symbol-name (pop (loop-forms state))))
	 (fn (gethash k *loop-clause-handlers*)))
    (if fn
	(funcall fn state)
	(ERROR "Unknown LOOP keyword: ~A" k))))

(defun expand-extended-loop (forms)
  (let ((state (make-loop-state forms))
	(start (gensym))
	(end (gensym)))
    (do ()
	((null (loop-forms state)))
      (setq state (parse-loop-clause state)))
    `(BLOCK ,(loop-name state)
      ,(expand-bindings (nreverse (loop-bindings state))
        `(PROGN
	  ,@(nreverse (loop-prologue state))
	  (CATCH (QUOTE ,end)
	    (MACROLET ((LOOP-FINISH () (QUOTE (THROW (QUOTE ,end) nil))))
	      (TAGBODY
		 ,start
		 ,@(when (loop-tests state)
		     `((WHEN (OR ,@(loop-tests state)) (LOOP-FINISH))))
		 ,@(nreverse (loop-setters state))
		 ,@(nreverse (loop-body state))
		 ,@(nreverse (loop-steps state))
		 (GO ,start))))
	  ,@(nreverse (loop-epilogue state))
	  ,(loop-result state))))))

(defun expand-bindings (bindings body)
  (cond
    ((null bindings)
     body)
    ((or (atom (caar bindings))
	 (atom (caaar bindings)))
     `(LET ,(first bindings)
        ,(expand-bindings (rest bindings) body)))
    (t
     `(DESTRUCTURING-BIND ,(caaar bindings) ,(cadaar bindings)
        ,(expand-bindings (rest bindings) body)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-loop.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-numbers.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 12, Numbers.

(IN-PACKAGE "EMACS-CL")

;;; System Class NUMBER
;;; System Class COMPLEX
;;; System Class REAL
;;; System Class FLOAT
;;; Type SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT
;;; System Class RATIONAL
;;; System Class RATIO
;;; System Class INTEGER
;;; Type SIGNED-BYTE
;;; Type UNSIGNED-BYTE
;;; Type Specifier MOD
;;; Type BIT
;;; Type FIXNUM
;;; Type BIGNUM

(define-storage-layout ratio (num den))

(define-storage-layout complex (real imag))

(define-storage-layout random-state (x))

(defun cl:= (number &rest numbers)
  (every (lambda (n) (binary= number n)) numbers))

(defun binary= (num1 num2)
  ;; TODO: This doesn't work for all possible pairs of numbers.
  (cond
    ((and (or (integerp num1) (floatp num1))
	  (or (integerp num2) (floatp num2)))
     (= num1 num2))
    ((or (COMPLEXP num1) (COMPLEXP num2))
     (and (binary= (REALPART num1) (REALPART num2))
	  (binary= (IMAGPART num1) (IMAGPART num2))))
    ((or (ratiop num1) (ratiop num2))
     (and (binary= (NUMERATOR num1) (NUMERATOR num2))
	  (binary= (DENOMINATOR num1) (DENOMINATOR num2))))
    ((and (bignump num1) (bignump num2))
     (and (= (length num1) (length num2))
	  (every #'eql num1 num2)))
    ((and (NUMBERP num1) (NUMBERP num2))
     nil)
    (t
     (unless (NUMBERP num1)
       (type-error num1 'NUMBER))
     (type-error num2 'NUMBER))))

(defun cl:/= (number &rest numbers)
  (if (null numbers)
      T
      (and (not (some (lambda (num) (binary= number num)) numbers))
	   (apply #'cl:/= (first numbers) (rest numbers)))))

(defun cl:< (number &rest numbers)
  (if (null numbers)
      T
      (and (binary< number (first numbers))
	   (apply #'cl:< (first numbers) (rest numbers)))))

(defun binary< (num1 num2)
  (cond
    ((and (or (integerp num1) (floatp num1))
	  (or (integerp num2) (floatp num2)))
     (< num1 num2))
    ((or (ratiop num1) (ratiop num2))
     ;; TODO
     (< (/ (FLOAT (NUMERATOR num1)) (FLOAT (DENOMINATOR num1)))
	(/ (FLOAT (NUMERATOR num2)) (FLOAT (DENOMINATOR num2)))))
    ((or (bignump num1) (bignump num2))
     (MINUSP (binary- num1 num2)))
    (t
     (unless (REALP num1)
       (type-error num1 'REAL))
     (type-error num2 'REAL))))

(defun cl:> (number &rest numbers)
  (if (null numbers)
      T
      (and (binary< (first numbers) number)
	   (apply #'cl:> (first numbers) (rest numbers)))))

(defun cl:<= (number &rest numbers)
  (if (null numbers)
      T
      (and (binary<= number (first numbers))
	   (apply #'cl:<= (first numbers) (rest numbers)))))

(defun binary<= (num1 num2)
  (cond
    ((and (or (integerp num1) (floatp num1))
	  (or (integerp num2) (floatp num2)))
     (<= num1 num2))
    ((or (ratiop num1) (ratiop num2))
     ;; TODO
     (<= (/ (FLOAT (NUMERATOR num1)) (FLOAT (DENOMINATOR num1)))
	 (/ (FLOAT (NUMERATOR num2)) (FLOAT (DENOMINATOR num2)))))
    ((or (bignump num1) (bignump num2))
     (let ((diff (binary- num1 num2)))
       (or (MINUSP diff) (ZEROP diff))))
    (t
     (unless (REALP num1)
       (type-error num1 'REAL))
     (type-error num2 'REAL))))

(defun cl:>= (number &rest numbers)
  (if (null numbers)
      t
      (and (binary<= (first numbers) number)
	   (apply #'cl:>= (first numbers) (rest numbers)))))

(defun MAX (&rest numbers)
  (if (null numbers)
      (error "")
      (reduce (lambda (num1 num2) (if (cl:>= num1 num2) num1 num2)) numbers)))

(defun MIN (&rest numbers)
  (if (null numbers)
      (error "")
      (reduce (lambda (num1 num2) (if (cl:<= num1 num2) num1 num2)) numbers)))

(defun MINUSP (num)
  (cond
    ((or (integerp num) (floatp num))
     (minusp num))
    ((bignump num)
     (minusp (aref num (1- (length num)))))
    ((ratiop num)
     (MINUSP (NUMERATOR num)))
    (t
     (type-error num 'REAL))))

(defun PLUSP (num)
  (cond
    ((or (integerp num) (floatp num))
     (plusp num))
    ((bignump num)
     (>= (aref num (1- (length num))) 0))
    ((ratiop num)
     (PLUSP (NUMERATOR num)))
    (t
     (type-error num 'REAL))))

(defun ZEROP (num)
  (cond
    ((or (integerp num) (floatp num))
     (zerop num))
    ((ratiop num)
     (ZEROP (NUMERATOR num)))
    ((COMPLEXP num)
     (and (ZEROP (REALPART num)) (ZEROP (IMAGPART num))))
    ((bignump num)
     nil)
    (t
     (type-error num 'NUMBER))))

(defconst fixnum-bits (1+ (round (log most-positive-fixnum 2))))

(defun integer-truncate (x y)
  (cond
    ((and (integerp x) (integerp y))
     (if (and (eql x MOST-NEGATIVE-FIXNUM) (eql y -1))
	 (cl:values (vector 'BIGNUM MOST-NEGATIVE-FIXNUM 0) nil)
	 (cl:values (/ x y) (not (zerop (% x y))))))
    ((and (INTEGERP x) (INTEGERP y))
     (let ((sign 1)
	   (q 0)
	   (r 0)
	   (i (1- (if (integerp x) fixnum-bits (* fixnum-bits (1- (length x)))))))
       (when (MINUSP x)
	 (setq x (cl:- x) sign -1))
       (when (MINUSP y)
	 (setq y (cl:- y) sign (- sign)))
       (while (>= i 0)
;	 (print (format "x=%s y=%s q=%s r=%s" x y q r))
	 (setq r (ASH r 1))
	 (when (LOGBITP i x)
	   (setq r (LOGIOR r 1)))
	 (setq q (ASH q 1))
	 (when (cl:>= r y)
	   (setq q (LOGIOR q 1))
	   (setq r (binary- r y)))
	 (decf i))
       (cl:values (binary* sign q) (not (ZEROP r)))))
    (t
     (unless (INTEGERP x)
       (type-error x 'INTEGER))
     (type-error y 'INTEGER))))

(defconst two^fixnum-bits
    (* 2 (1+ (float most-positive-fixnum))))

(defun floor-to-bignum (float)
  (let ((list nil))
    (while (or (>= float 1.0) (<= float -1.0))
      (let ((residue (mod float two^fixnum-bits)))
	(push (truncate (if (> residue most-positive-fixnum)
			    (- residue two^fixnum-bits)
			    residue))
	      list))
      (setq float (/ float two^fixnum-bits)))
    (canonical-bignum (nreverse list))))

(cl:defun FLOOR (number &OPTIONAL (divisor 1))
  (let (quotient remainder)
    (cond
      ((or (floatp number) (floatp divisor))
       (setq number (FLOAT number)
	     divisor (FLOAT divisor)
	     quotient
	     (condition-case condition
		 (floor number divisor)
	       (range-error (floor-to-bignum (/ number divisor))))))
      ((or (ratiop number) (ratiop divisor))
       (MULTIPLE-VALUE-SETQ (quotient remainder)
	 (integer-truncate
	  (binary* (NUMERATOR number) (DENOMINATOR divisor))
	  (binary* (DENOMINATOR number) (NUMERATOR divisor)))))
      ((and (INTEGERP number) (INTEGERP divisor))
       (MULTIPLE-VALUE-SETQ (quotient remainder)
	 (integer-truncate number divisor)))
      (t
       (unless (REALP number)
	 (type-error number 'REAL))
       (type-error divisor 'REAL)))
    (when (and remainder (or (MINUSP quotient)
			     (and (ZEROP quotient) (MINUSP number))))
      (setq quotient (binary- quotient 1)))
    (cl:values quotient (binary- number (binary* quotient divisor)))))

(cl:defun FFLOOR (number &OPTIONAL (divisor 1))
  (MULTIPLE-VALUE-BIND (quotient remainder) (FLOOR number divisor)
    (cl:values (FLOAT quotient) remainder)))

(defun ceiling-to-bignum (float)
  (cl:- (floor-to-bignum (- float))))

(cl:defun CEILING (number &OPTIONAL (divisor 1))
  (let (quotient remainder)
    (cond
      ((or (floatp number) (floatp divisor))
       (setq number (FLOAT number)
	     divisor (FLOAT divisor)
	     quotient
	     (condition-case condition
		 (ceiling number divisor)
	       (range-error (ceiling-to-bignum (/ number divisor))))))
      ((or (ratiop number) (ratiop divisor))
       (MULTIPLE-VALUE-SETQ (quotient remainder)
	 (integer-truncate
	  (binary* (NUMERATOR number) (DENOMINATOR divisor))
	  (binary* (DENOMINATOR number) (NUMERATOR divisor)))))
      ((and (INTEGERP number) (INTEGERP divisor))
       (MULTIPLE-VALUE-SETQ (quotient remainder)
	 (integer-truncate number divisor)))
      (t
       (unless (REALP number)
	 (type-error number 'REAL))
       (type-error divisor 'REAL)))
    (when (and remainder (or (PLUSP quotient)
			     (and (ZEROP quotient) (PLUSP number))))
      (setq quotient (binary+ quotient 1)))
    (cl:values quotient (binary- number (binary* quotient divisor)))))

(cl:defun FCEILING (number &OPTIONAL (divisor 1))
  (MULTIPLE-VALUE-BIND (quotient remainder) (CEILING number divisor)
    (cl:values (FLOAT quotient) remainder)))

(defun truncate-to-bignum (float)
  (if (minusp float)
      (ceiling-to-bignum float)
      (floor-to-bignum float)))

(condition-case c
    (progn
      (truncate 1 2)
      (defmacro trunc2 (num div) `(truncate ,num ,div)))
  (wrong-number-of-arguments
   (defmacro trunc2 (num div) `(truncate (/ ,num ,div)))))

(cl:defun TRUNCATE (number &OPTIONAL (divisor 1))
  (let (quotient)
    (cond
      ((or (floatp number) (floatp divisor))
       (setq number (FLOAT number)
	     divisor (FLOAT divisor)
	     quotient
	     (condition-case c
		 (trunc2 number divisor)
	       (range-error (truncate-to-bignum (/ number divisor))))))
      ((or (ratiop number) (ratiop divisor))
       (setq quotient (integer-truncate
		       (binary* (NUMERATOR number) (DENOMINATOR divisor))
		       (binary* (DENOMINATOR number) (NUMERATOR divisor)))))
      ((and (INTEGERP number) (INTEGERP divisor))
       (setq quotient (integer-truncate number divisor)))
      (t
       (unless (REALP number)
	 (type-error number 'REAL))
       (type-error divisor 'REAL)))
    (cl:values quotient (binary- number (binary* quotient divisor)))))

(cl:defun FTRUNCATE (number &OPTIONAL (divisor 1))
  (MULTIPLE-VALUE-BIND (quotient remainder) (TRUNCATE number divisor)
    (cl:values (FLOAT quotient) remainder)))

(cl:defun ROUND (number &OPTIONAL (divisor 1))
  (MULTIPLE-VALUE-BIND (quotient remainder)
      ;; TODO: proper rounding
      (TRUNCATE (binary+ number .5) divisor)
    (cl:values quotient remainder)))

(cl:defun FROUND (number &OPTIONAL (divisor 1))
  (MULTIPLE-VALUE-BIND (quotient remainder) (ROUND number divisor)
    (cl:values (FLOAT quotient) remainder)))

(defun SIN (x)
  (cond
    ((REALP x)		(sin (FLOAT x)))
    ((COMPLEXP x)	(error "TODO"))
    (t			(type-error x 'NUMBER))))

(defun COS (x)
  (cond
    ((REALP x)		(cos (FLOAT x)))
    ((COMPLEXP x)	(error "TODO"))
    (t			(type-error x 'NUMBER))))

(defun TAN (x)
  (cond
    ((REALP x)		(tan (FLOAT x)))
    ((COMPLEXP x)	(error "TODO"))
    (t			(type-error x 'NUMBER))))

(defun ASIN (x)
  (cond
    ((REALP x)		(asin (FLOAT x)))
    ((COMPLEXP x)	(error "TODO"))
    (t			(type-error x 'NUMBER))))

(defun ACOS (x)
  (cond
    ((REALP x)		(acos (FLOAT x)))
    ((COMPLEXP x)	(error "TODO"))
    (t			(type-error x 'NUMBER))))

(defun ATAN (x &optional y)
  (when y (error "TODO"))
  (cond
    ((REALP x)		(atan (FLOAT x)))
    ((COMPLEXP x)	(error "TODO"))
    (t			(type-error x 'NUMBER))))

(DEFCONSTANT PI 3.141592653589793)

(defun SINH (x)
  (binary* 0.5 (binary- (EXP x) (EXP (cl:- x)))))

(defun COSH (x)
  (binary* 0.5 (binary+ (EXP x) (EXP (cl:- x)))))

(defun TANH (x)
  (binary/ (binary- (EXP x) (EXP (cl:- x)))
	   (binary+ (EXP x) (EXP (cl:- x)))))

(defun ASINH (x)
  (LOG (binary+ x (SQRT (1+ (binary* x x))))))

(defun ACOSH (x)
  (binary* 2 (LOG (binary+ (SQRT (binary* 0.5 (1+ x)))
			   (SQRT (binary* 0.5 (1- x)))))))

(defun ATANH (x)
  (binary* 0.5 (binary- (LOG (1+ x)) (LOG (binary- 1 x)))))

(defun cl:* (&rest numbers)
  (reduce #'binary* numbers :initial-value 1))

(defconst multiplication-limit (floor (sqrt most-positive-fixnum)))

(defun binary* (x y)
  (cond
    ((and (integerp x) (integerp y))
     (if (and (< x multiplication-limit)
	      (> x (- multiplication-limit))
	      (< y multiplication-limit)
	      (> y (- multiplication-limit)))
	 (* x y)
	 (bignum* (vector 'BIGNUM x (if (minusp x) -1 0))
		  (vector 'BIGNUM y (if (minusp y) -1 0)))))
    ((or (COMPLEXP x) (COMPLEXP y))
     (COMPLEX (binary- (binary* (REALPART x) (REALPART y))
		       (binary* (IMAGPART x) (IMAGPART y)))
	      (binary+ (binary* (REALPART x) (IMAGPART y))
		       (binary* (IMAGPART x) (REALPART y)))))
    ((floatp x)
     (* x (FLOAT y)))
    ((floatp y)
     (* (FLOAT x) y))
    ((or (ratiop x) (ratiop y))
     (make-ratio (binary* (NUMERATOR x) (NUMERATOR y))
		 (binary* (DENOMINATOR x) (DENOMINATOR y))))
    ((or (INTEGERP x) (INTEGERP y))
     (when (integerp x)
       (setq x (vector 'BIGNUM x (if (minusp x) -1 0))))
     (when (integerp y)
       (setq y (vector 'BIGNUM y (if (minusp y) -1 0))))
     (bignum* x y))
    (t
     (unless (NUMBERP x)
       (type-error x 'NUMBER))
     (type-error y 'NUMBER))))

(defun bignum* (x y)
  (cond
    ((equal x [BIGNUM 1 0])
     (canonical-bignum y))
    ((equal x [BIGNUM -1 -1])
     (cl:- (canonical-bignum y)))
    ((equal y [BIGNUM 10 0])
     (setq x (canonical-bignum x))
;    (print (format "(bignum* %s %s)" x y))
     (let* ((2x (binary+ x x))
	    (4x (binary+ 2x 2x))
	    (5x (binary+ 4x x)))
;      (print (format "%s %s %s" 2x 4x 5x))
       (binary+ 5x 5x)))
    (t
     (setq x (canonical-bignum x))
     (setq y (canonical-bignum y))
     (let ((sign 1)
	   (z 0))
       (when (MINUSP x)
	 (setq x (cl:- x) sign -1))
       (when (MINUSP y)
	 (setq y (cl:- y) sign (- sign)))
       (while (PLUSP x)
	 (when (LOGBITP 0 x)
	   (setq z (binary+ z y)))
	 (setq y (ASH y 1))
	 (setq x (ASH x -1)))
       (binary* sign z)))))

(defun cl:+ (&rest numbers)
  (reduce #'binary+ numbers :initial-value 0))

(defun binary+ (x y)
  (cond
    ((and (integerp x) (integerp y))
     (let ((sum (+ x y)))
       (cond
	 ((and (>= x 0) (>= y 0) (minusp sum))
	  (vector 'BIGNUM sum 0))
	 ((and (minusp x) (minusp y) (>= sum 0))
	  (vector 'BIGNUM sum -1))
	 (t
	  sum))))
    ((or (COMPLEXP x) (COMPLEXP y))
     (COMPLEX (binary+ (REALPART x) (REALPART y))
	      (binary+ (IMAGPART x) (IMAGPART y))))
    ((floatp x)
     (+ x (FLOAT y)))
    ((floatp y)
     (+ (FLOAT x) y))
    ((or (ratiop x) (ratiop y))
     (make-ratio (binary+ (binary* (NUMERATOR x) (DENOMINATOR y))
			  (binary* (DENOMINATOR x) (NUMERATOR y)))
		 (binary* (DENOMINATOR x) (DENOMINATOR y))))
    ((or (bignump x) (bignump y))
;    (print (format "%s %s" x y))
     (cond
       ((integerp x)	(bignum+fixnum y x))
       ((integerp y)	(bignum+fixnum x y))
       (t		(bignum+bignum x y))))
    (t
     (error "error"))))

(defun bignum+fixnum (x y)
  (let* ((x0 (aref x 1))
	 (sum (+ x0 y))
	 (new (copy-sequence x)))
    (aset new 1 sum)
;   (print x0)
;   (print y)
;   (print sum)
    (cond
      ;; negative + positive -> positive: carry
      ((and (minusp x0) (>= y 0) (>= sum 0))
       (bignum+bignum new [BIGNUM 0 1]))
      ;; positive + negative -> negative: borrow
      ((and (>= x0 0) (minusp y) (minusp sum))
       (bignum+bignum new [BIGNUM 0 -1]))
      ;; positive + positive -> negative: no overflow
      ;; negative + negative -> positive: no overflow
      (t
       (canonical-bignum new)))))

(defun bignum+bignum (x y)
  (canonical-bignum (bignum+ (bignum-list x) (bignum-list y))))

(cl:defun bignum-list (num &OPTIONAL (index 1))
  (if (= index (length num))
      nil
      (cons (aref num index) (bignum-list num (1+ index)))))

(defun canonical-bignum (object)
  (cond
    ((bignump object)
     (canonical-bignum (bignum-list object)))
    ((listp object)
     (setq object (truncate-sign-extension object))
     (if (eql (length object) 1)
	 (first object)
	 (let ((bignum (make-vector (1+ (length object)) 'BIGNUM))
	       (i 0))
	   (dolist (n object)
	     (aset bignum (incf i) n))
	   bignum)))
    (t
     (error "error"))))

(defun truncate-sign-extension (list &optional prev)
  (if (null list)
      nil
      (let ((rest (truncate-sign-extension (rest list) (first list))))
	(setf (cdr list) rest)
	(if (null rest)
	    (let ((this (first list)))
	      (cond
		((and (zerop this) prev (>= prev 0))
		 nil)
		((and (eql this -1) prev (minusp prev))
		 nil)
		(t
		 list)))
	    list))))

(cl:defun bignum+ (x y &OPTIONAL (carry 0))
; (print (format "(bignum+ %s %s %s)" x y carry))
  (cond
    ((null x)
     (if (zerop carry)
	 y
	 (bignum+ y (list carry))))
    ((null y)
     (if (zerop carry)
	 x 
	 (bignum+ x (list carry))))
    (t
     (let* ((x0 (car x))
	    (y0 (car y))
	    (sum (+ x0 y0 carry)))
;      (print (format "x0=%s y0=%s sum=%s" x0 y0 sum))
       (if (and (null (rest x)) (null (rest y))
		(>= x0 0) (>= y0 0) (minusp sum))
	   ;; Last number wrapped from positive to negative.
	   ;; Need a final zero.
	   (cons sum '(0))
	   (cons sum
		 (bignum+
		  (rest x)
		  (rest y)
		  (if (or (and (minusp x0) (>= y0 0) (>= sum 0) (rest x))
			  (and (>= x0 0) (minusp y0) (>= sum 0) (rest y))
			  (and (minusp x0) (minusp y0) (rest x) (rest y)))
		      1 0))))))))

(defun cl:- (number &rest numbers)
  (if (null numbers)
      (cond
	((or (integerp number) (floatp number))
	 (if (eql number MOST-NEGATIVE-FIXNUM)
	     (vector 'BIGNUM number 0)
	     (- number)))
	((ratiop number)
	 (vector 'RATIO (cl:- (NUMERATOR number)) (DENOMINATOR number)))
	((COMPLEXP number)
	 (vector 'COMPLEX (cl:- (REALPART number)) (cl:- (IMAGPART number))))
	((bignump number)
	 (bignum+fixnum (LOGNOT number) 1))
	(t
	 (error "error")))
      (dolist (num numbers number)
	(setq number (binary- number num)))))

(defun binary- (x y)
  (binary+ x (cl:- y)))

(defun cl:/ (number &rest numbers)
  (if (null numbers)
      (cond
	((integerp number)
	 (vector 'RATIO 1 number))
	((floatp number)
	 (/ 1.0 number))
	((bignump number)
	 (vector 'RATIO 1 number))
	((ratiop number)
	 (make-ratio (DENOMINATOR number) (NUMERATOR number)))
	((COMPLEXP number)
	 (let* ((r (REALPART number))
		(i (IMAGPART number))
		(x (binary- (binary* r r) (binary* i i))))
	   (COMPLEX (binary/ r x) (cl:- (binary/ i x)))))
	(t
	 (error "error")))
      (dolist (num numbers number)
	(setq number (binary/ number num)))))

(defun binary/ (x y)
  (cond
    ((and (INTEGERP x) (INTEGERP y))
     (make-ratio x y))
    ((or (COMPLEXP x) (COMPLEXP y))
     (let* ((rx (REALPART x))
	    (ry (REALPART y))
	    (ix (IMAGPART x))
	    (iy (IMAGPART y))
	    (div (binary+ (binary* ry ry) (binary* iy iy))))
       (COMPLEX (binary/ (binary+ (binary* rx ry) (binary* ix iy)) div)
		(binary/ (binary- (binary* ix ry) (binary* rx iy)) div))))
    ((floatp x)
     (/ x (FLOAT y)))
    ((floatp y)
     (/ (FLOAT x) y))
    ((or (RATIONALP x) (RATIONALP y))
     (make-ratio (binary* (NUMERATOR x) (DENOMINATOR y))
		 (binary* (DENOMINATOR x) (NUMERATOR y))))
    (t
     (unless (NUMBERP x)
       (type-error x 'NUMBER))
     (type-error y 'NUMER))))
  
(defun cl:1+ (number)
  (binary+ number 1))

(defun cl:1- (number)
  (binary- number 1))

(defun ABS (number)
  (cond
    ((integerp number)
     (if (eql number MOST-NEGATIVE-FIXNUM)
	 (vector 'BIGNUM number 0)
	 (abs number)))
    ((floatp number)
     (abs number))
    ((ratiop number)
     (vector 'RATIO (ABS (NUMERATOR number)) (DENOMINATOR number)))
    ((COMPLEXP number)
     (let ((r (FLOAT (REALPART number)))
	   (i (FLOAT (IMAGPART number))))
       (sqrt (+ (* r r) (* i i)))))
    ((bignump number)
     (if (MINUSP number)
	 (cl:- number)
	 number))
    (t
     (type-error number 'NUMBER))))

(defun EVENP (num)
  (if (INTEGERP num)
      (not (LOGBITP 0 num))
      (type-error num 'INTEGER)))

(defun ODDP (num)
  (if (INTEGERP num)
      (LOGBITP 0 num)
      (type-error num 'INTEGER)))

(defun EXP (num)
  (cond
    ((REALP num)	(exp (FLOAT num)))
    ((COMPLEXP num)	(error "TODO"))
    (t			(type-error num 'NUMBER))))

(defun EXPT (base power)
  (cond
    ((and (RATIONALP base) (INTEGERP power))
     (exact-expt base power))
    ((and (REALP base) (REALP power))
     (expt (FLOAT base) (FLOAT power)))
    ((and (NUMBERP base) (NUMBERP power))
     (error "TODO"))
    (t
     (unless (NUMBERP base)
       (type-error base 'NUMBER))
     (type-error power 'NUMBER))))

(defun exact-expt (base power)
  (cond
    ((ZEROP power)
     1)
    ((MINUSP power)
     (exact-expt (make-ratio (DENOMINATOR base) (NUMERATOR base))
		 (cl:- power)))
    (t
     (let ((result 1))
       (while (PLUSP power)
	 (when (LOGBITP 0 power)
	   (setq result (binary* result base)))
	 (setq base (binary* base base))
	 (setq power (ASH power -1)))
       result))))

(defun GCD (&rest numbers)
  (reduce #'binary-gcd numbers :initial-value 0))

(defun binary-gcd (x y)
  (cond
    ((or (eq x 1) (eq y 1))
     1)
    ((and (integerp x) (integerp y))
     (when (> y x)
       (psetq x y y x))
     (while (not (zerop y))
       (psetq y (% x y) x y))
     (abs x))
    (t
     (when (cl:> y x)
       (psetq x y y x))
     (while (not (ZEROP y))
       (psetq y (REM x y) x y))
     (ABS x))))

(cl:defmacro INCF (place &optional delta)
  (unless delta
    (setq delta 1))
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place nil) ;TODO: &environment
    `(LET* (,@(MAPCAR #'list temps values)
	    (,(first variables)
	     ,(if (eq delta 1)
		  `(,(INTERN "1+" *cl-package*) ,getter)
		  `(,(INTERN "+" *cl-package*) ,getter ,delta))))
       ,setter)))

(cl:defmacro DECF (place &optional delta)
  (unless delta
    (setq delta 1))
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION place nil) ;TODO: &environment
    `(LET* (,@(MAPCAR #'list temps values)
	    (,(first variables)
	     ,(if (eq delta 1)
		  `(,(INTERN "1-" *cl-package*) ,getter)
		  `(,(INTERN "-" *cl-package*) ,getter ,delta))))
       ,setter)))

(defun LCM (&rest numbers)
  (if (null numbers)
      1
      (reduce #'binary-lcm numbers)))

(defun binary-lcm (x y)
  (if (or (ZEROP x) (ZEROP y))
      0
      (integer-truncate (ABS (binary* x y)) (GCD x y))))

(cl:defun LOG (number &OPTIONAL (base (exp 1)))
  (cond
    ((and (REALP number) (REALP base))
     (log (FLOAT number) (FLOAT base)))
    ((and (NUMBERP number) (NUMBERP base))
     (error "TODO"))
    (t
     (unless (NUMBERP number)
       (type-error number 'NUMBER))
     (type-error base 'NUMBER))))
  
(defun MOD (number divisor)
  (NTH-VALUE 1 (FLOOR number divisor)))

(defun REM (number divisor)
  (NTH-VALUE 1 (TRUNCATE number divisor)))

(defun SIGNUM (number)
  (cond
    ((RATIONALP number) (cond ((PLUSP number)	1)
			      ((ZEROP number)	0)
			      ((MINUSP number)	-1)))
    ((floatp number)	(cond ((plusp number)	1.0)
			      ((zerop number)	0.0)
			      ((minusp number)	-1.0)))
    ((COMPLEXP number)	(if (ZEROP number)	number
						(binary/ number (ABS number))))
    (t			(type-error number 'NUMBER))))

(defun SQRT (number)
  (cond
    ((REALP number)	(sqrt (FLOAT number)))
    ((COMPLEXP number)	(error "TODO"))
    (t			(type-error number 'NUMBER))))

;;; http://www.embedded.com/98/9802fe2.htm
(defun ISQRT (number)
  (unless (and (INTEGERP number) (not (MINUSP number)))
    (type-error number '(INTEGER 0 *)))
  (do ((rem 0)
       (root 0)
       (i (LOGAND (INTEGER-LENGTH number) -2) (- i 2)))
      ((minusp i)
       (ASH root -1))
    (setq root (binary+ root root)
	  rem (binary+ (ASH rem 2) (LOGAND (ASH number (- i)) 3))
	  root (binary+ root 1))
    (if (binary<= root rem)
	(setq rem (binary- rem root)
	      root (binary+ root 1))
	(setq root (binary+ root -1)))))
; (defun ISQRT (number)
;   (do ((rem 0)
;        (root 0)
;        divisor
;        (i (LOGAND (INTEGER-LENGTH number) -2) (- i 2)))
;       ((minusp i)
;        root)
;     (setq root (binary+ root root)
; 	  rem (binary+ (ASH rem 2) (LOGAND (ASH number (- i)) 3))
; 	  divisor (binary+ (binary+ root root) 1))
;     (when (binary<= divisor rem)
;       (setq rem (binary- rem divisor)
; 	    root (binary+ root 1)))))

;;; System Class RANDOM-STATE

(defun MAKE-RANDOM-STATE (&optional state)
  (vector
   'RANDOM-STATE
   (cond
     ((null state)		0)
     ((eq state T)		(random-state-x *RANDOM-STATE*))
     ((RANDOM-STATE-P state)	(random-state-x state))
     (t				(type-error state
					    '(OR BOOLEAN RANDOM-STATE))))))

(defun RANDOM (limit &optional state)
  ;; TODO: use state
  (cond
    ((integerp limit)
     (random limit))
    ((floatp limit)
     (/ (* limit (random MOST-POSITIVE-FIXNUM)) MOST-POSITIVE-FIXNUM))
    ((bignump limit)
     ;; TODO
     0)))

(defun RANDOM-STATE-P (object)
  (vector-and-typep object 'RANDOM-STATE))

(DEFVAR *RANDOM-STATE* (MAKE-RANDOM-STATE))

(defun NUMBERP (object)
  (or (numberp object)
      (and (vectorp object)
	   (let ((type (aref object 0)))
	     (or (eq type 'BIGNUM)
		 (eq type 'RATIO)
		 (eq type 'COMPLEX))))))

(defun CIS (x)
  (unless (REALP x)
    (type-error x 'REAL))
  (EXP (vector 'COMPLEX 0 x)))

(cl:defun COMPLEX (realpart &OPTIONAL (imagpart 0))
  (cond
    ((floatp realpart)
     (setq imagpart (float imagpart)))
    ((floatp imagpart)
     (setq realpart (float realpart))))
  (if (eq imagpart 0)
      realpart
      (vector 'COMPLEX realpart imagpart)))

(defun COMPLEXP (object)
  (vector-and-typep object 'COMPLEX))

(defun CONJUGATE (num)
  (vector 'COMPLEX (REALPART num) (cl:- (IMAGPART num))))

(defun PHASE (num)
  (ATAN (IMAGPART num) (REALPART num)))

(defun REALPART (num)
  (if (COMPLEXP num)
      (complex-real num)
      num))

(defun IMAGPART (num)
  (if (COMPLEXP num)
      (complex-imag num)
      0))

(defun UPGRADED-COMPLEX-PART-TYPE (typespec &optional env)
  'REAL)

(defun REALP (num)
  (or (RATIONALP num) (floatp num)))

(defun make-ratio (num den)
  (unless (INTEGERP num)
    (type-error num 'INTEGER))
  (unless (INTEGERP den)
    (type-error den 'INTEGER))
  (when (ZEROP den)
    (ERROR 'DIVISION-BY-ZERO))
  (if (and (eq num MOST-NEGATIVE-FIXNUM) (eq den -1))
      (vector 'BIGNUM MOST-NEGATIVE-FIXNUM 0)
      (let* ((gcd (GCD num den))
	     (num (integer-truncate num gcd))
	     (den (integer-truncate den gcd)))
	(cl:values
	 (cond
	   ((eq den 1)
	    num)
	   ((MINUSP den)
	    (vector 'RATIO (cl:- num) (cl:- den)))
	   (t
	    (vector 'RATIO num den)))))))

(defun ratiop (num)
  (vector-and-typep num 'RATIO))

(defun NUMERATOR (num)
  (if (ratiop num)
      (ratio-num num)
      num))

(defun DENOMINATOR (num)
  (if (ratiop num)
      (ratio-den num)
      1))

(defun RATIONAL (num)
  (cond
    ((floatp num)
     (MULTIPLE-VALUE-BIND (significand exp sign) (INTEGER-DECODE-FLOAT num)
       (let ((bits (max exp 53)))
	 (cl:values
	  (make-ratio (ROUND (SCALE-FLOAT significand bits))
		      (EXPT 2 (- bits exp)))))))
    ((RATIONALP num)
     num)
    (t
     (type-error num 'REAL))))

(defun* RATIONALIZE (num)
  (cond
    ((floatp num)
     ;; Algorithm from Gareth McCaughan.
     (let ((p 0) (q 1) (r 1) (s 0))
       (while (binary< s max-rationalize-denominator)
	 (MULTIPLE-VALUE-BIND (intpart fracpart) (FLOOR num)
	   (setq p (prog1 r
		     (setq r (binary+ p (binary* intpart r)))))
	   (setq q (prog1 s
		     (setq s (binary+ q (binary* intpart s)))))
	   (when (< fracpart 1e-14)
	     (return-from RATIONALIZE (cl:values (make-ratio r s))))
	   (setq num (binary/ 1 fracpart))))
       (cl:values (make-ratio p q))))
    ((RATIONALP num)
     num)
    (t
     (type-error num 'REAL))))

(defun RATIONALP (num)
  (or (INTEGERP num) (ratiop num)))

(defun ASH (num shift)
  (cond
    ((ZEROP shift)
     num)
    ((MINUSP shift)
     (cond
       ((integerp num)
	(ash num shift))
       ((bignump num)
	(let ((new (copy-sequence num)))
	  (while (MINUSP shift)
	    (shift-right new)
	    (incf shift))
	  (canonical-bignum new)))
       (t
	(error "error"))))
    (t
     (while (> shift 0)
       (setq num (binary+ num num)
	     shift (1- shift)))
     num)))

(defun shift-right (num)
  (let ((i (1- (length num)))
	(first t)
	(carry 0))
    (while (plusp i)
      (let ((n (aref num i)))
	(aset num i (if first
			(ash n -1)
			(logior (lsh n -1) (ash carry (1- fixnum-bits)))))
	(setq carry (logand n 1)
	      first nil))
      (decf i))))

(defun INTEGER-LENGTH (num)
  (when (MINUSP num)
    (setq num (cl:- num)))
  (cond
    ((eq num 0)		0)
    ((integerp num)	(1+ (logb num)))
    ((bignump num)	(let* ((len (1- (length num)))
			       (last (aref num len)))
			  (+ (* fixnum-bits (1- len))
			     (if (zerop last)
				 0
				 (1+ (logb last))))))
    (t			(type-error num 'INTEGER))))

(defun bignump (num)
  (vector-and-typep num 'BIGNUM))

(defun INTEGERP (num)
  (or (integerp num) (bignump num)))

(cl:defun PARSE-INTEGER (string &KEY (START 0) (END (LENGTH string))
			             (RADIX 10) JUNK-ALLOWED)
  (let ((sign 1)
	(integer 0)
	(i START)
	char digit)
    (catch 'PARSE-INTEGER
      (while (whitespacep (CHAR string i))
	(incf i)
	(when (= i END)
	  (if JUNK-ALLOWED
	      (throw 'PARSE-INTEGER (cl:values nil i))
	      (ERROR 'PARSE-ERROR))))
      (setq char (CHAR string i))
      (when (FIND char "+-")
	(when (ch= char 45)
	  (setq sign -1))
	(incf i)
	(when (= i END)
	  (if JUNK-ALLOWED
	      (throw 'PARSE-INTEGER (cl:values nil i))
	      (ERROR 'PARSE-ERROR)))
	(setq char (CHAR string i)))
      (unless (DIGIT-CHAR-P char RADIX)
	(if JUNK-ALLOWED
	    (throw 'PARSE-INTEGER (cl:values nil i))
	    (ERROR 'PARSE-ERROR)))
      (while (setq digit (DIGIT-CHAR-P char RADIX))
	(setq integer (cl:+ (cl:* integer RADIX) digit))
	(incf i)
	(when (= i END)
	  (throw 'PARSE-INTEGER (cl:values (cl:* sign integer) i)))
	(setq char (CHAR string i)))
      (if JUNK-ALLOWED
	  (cl:values (cl:* sign integer) i)
	  (do ((i i (1+ i)))
	      ((= i END)
	       (cl:values (cl:* sign integer) i))
	    (unless (whitespacep (CHAR string i))
	      (ERROR 'PARSE-ERROR)))))))

(DEFCONSTANT BOOLE-1		 1)
(DEFCONSTANT BOOLE-2		 2)
(DEFCONSTANT BOOLE-AND		 3)
(DEFCONSTANT BOOLE-ANDC1	 4)
(DEFCONSTANT BOOLE-ANDC2	 5)
(DEFCONSTANT BOOLE-C1		 6)
(DEFCONSTANT BOOLE-C2		 7)
(DEFCONSTANT BOOLE-CLR		 8)
(DEFCONSTANT BOOLE-EQV		 9)
(DEFCONSTANT BOOLE-IOR		10)
(DEFCONSTANT BOOLE-NAND		11)
(DEFCONSTANT BOOLE-NOR		12)
(DEFCONSTANT BOOLE-ORC1		13)
(DEFCONSTANT BOOLE-ORC2		14)
(DEFCONSTANT BOOLE-SET		15)
(DEFCONSTANT BOOLE-XOR		16)

(defun BOOLE (op integer1 integer2)
  (ecase op
    (1	integer1)
    (2	integer2)
    (3	(LOGAND integer1 integer2))
    (4	(LOGANDC1 integer1 integer2))
    (5	(LOGANDC2 integer1 integer2))
    (6	(LOGNOT integer1))
    (7	(LOGNOT integer2))
    (8	0)
    (9	(LOGEQV integer1 integer2))
    (10	(LOGIOR integer1 integer2))
    (11	(LOGNAND integer1 integer2))
    (12	(LOGNOR integer1 integer2))
    (13	(LOGORC1 integer1 integer2))
    (14	(LOGORC2 integer1 integer2))
    (15	-1)
    (16	(LOGXOR integer1 integer2))))
    
(defun LOGNOT (num)
  (cond
    ((integerp num)
     (lognot num))
    ((bignump num)
     ;; TODO: may need one more element in result.
     (let ((new (make-vector (length num) 'BIGNUM)))
       (dotimes (i (1- (length num)))
	 (aset new (1+ i) (lognot (aref num (1+ i)))))
       new))
    (t
     (type-error num 'INTEGER))))

(defun LOGAND (&rest numbers)
  (reduce #'binary-logand numbers :initial-value -1))

(defun binary-logand (x y)
  (cond
    ((and (integerp x) (integerp y))
     (logand x y))
    ((and (bignump x) (integerp y))
     (if (minusp y)
	 (let ((new (copy-sequence x)))
	   (aset new 1 (logand (aref x 1) y))
	   new)
	 (logand (aref x 1) y)))
    ((and (bignump y) (integerp x))
     (if (minusp x)
	 (let ((new (copy-sequence y)))
	   (aset new 1 (logand (aref y 1) x))
	   new)
	 (logand (aref y 1) x)))
    ((and (bignump x) (bignump y))
     (bignum-logand x y))
    (t
     (unless (INTEGERP x)
       (type-error x 'INTEGER))
     (type-error y 'INTEGER))))

(defun bignum-logand (x y)
  (setq x (bignum-list x))
  (setq y (bignum-list y))
  (when (< (length x) (length y))
    (psetq x y y x))
  (when (< (length y) (length x))
    (setq y (nreverse y))
    (while (< (length y) (length x))
      (push (if (MINUSP (first y)) -1 0) y))
    (setq y (nreverse y)))
  (canonical-bignum (MAPCAR (lambda (n m) (logand n m)) x y)))

(defun LOGIOR (&rest numbers)
  (reduce #'binary-logior numbers :initial-value 0))

(defun binary-logior (x y)
  (cond
    ((and (integerp x) (integerp y))
     (logior x y))
    ((and (bignump x) (integerp y))
     (if (minusp y)
	 (logior (aref x 1) y)
	 (let ((new (copy-sequence x)))
	   (aset new 1 (logior (aref x 1) y))
	   new)))
    ((and (bignump y) (integerp x))
     (if (minusp x)
	 (logior (aref y 1) x)
	 (let ((new (copy-sequence y)))
	   (aset new 1 (logior (aref y 1) x))
	   new)))
    ((and (bignump x) (bignump y))
     (bignum-logior x y))
    (t
     (unless (INTEGERP x)
       (type-error x 'INTEGER))
     (type-error y 'INTEGER))))

(defun bignum-logior (x y)
  (setq x (bignum-list x))
  (setq y (bignum-list y))
  (when (< (length x) (length y))
    (psetq x y y x))
  (when (< (length y) (length x))
    (setq y (nreverse y))
    (while (< (length y) (length x))
      (push (if (MINUSP (first y)) -1 0) y))
    (setq y (nreverse y)))
  (canonical-bignum (MAPCAR (lambda (n m) (logior n m)) x y)))

(defun LOGNAND (x y)
  (LOGNOT (LOGAND x y)))

(defun LOGANDC1 (x y)
  (LOGAND (LOGNOT x) y))

(defun LOGANDC2 (x y)
  (LOGAND x (LOGNOT y)))

(defun LOGNOR (x y)
  (LOGNOT (LOGIOR x y)))

(defun LOGORC1 (x y)
  (LOGIOR (LOGNOT x) y))

(defun LOGORC2 (x y)
  (LOGIOR x (LOGNOT y)))

(defun LOGEQV (&rest numbers)
  (LOGNOT (apply #'LOGXOR numbers)))

(defun LOGXOR (&rest numbers)
  (reduce #'binary-logxor numbers :initial-value 0))

(defun binary-logxor (x y)
  (cond
    ((and (integerp x) (integerp y))
     (logxor x y))
    ((and (bignump x) (integerp y))
     (let ((new (copy-sequence x)))
       (aset new 1 (logxor (aref x 1) y))
       (when (minusp y)
	 (dotimes (i (- (length x) 2))
	   (aset new (+ i 2) (lognot (aref new (+ i 2))))))
       new))
    ((and (bignump y) (integerp x))
     (let ((new (copy-sequence y)))
       (aset new 1 (logior (aref y 1) x))
       (when (minusp x)
	 (dotimes (i (- (length y) 2))
	   (aset new (+ i 2) (lognot (aref new (+ i 2))))))
       new))
    ((and (bignump x) (bignump y))
     (bignum-logxor x y))
    (t
     (unless (INTEGERP x)
       (type-error x 'INTEGER))
     (type-error y 'INTEGER))))

(defun bignum-logxor (x y)
  (setq x (bignum-list x))
  (setq y (bignum-list y))
  (when (< (length x) (length y))
    (psetq x y y x))
  (when (< (length y) (length x))
    (setq y (nreverse y))
    (while (< (length y) (length x))
      (push (if (MINUSP (first y)) -1 0) y))
    (setq y (nreverse y)))
  (canonical-bignum (MAPCAR (lambda (n m) (logxor n m)) x y)))


(defun LOGBITP (index integer)
  (unless (integerp index)
    (error "TODO"))
  (when (minusp index)
    (type-error index '(INTEGER 0 *)))
  (cond
    ((integerp integer)
     (if (>= index fixnum-bits)
	 (minusp integer)
	 (not (zerop (logand integer (ash 1 index))))))
    ((bignump integer)
     (if (>= index (* fixnum-bits (1- (length integer))))
	 (MINUSP integer)
	 (let ((i (1+ (/ index fixnum-bits)))
	       (j (% index fixnum-bits)))
	   (not (zerop (logand (aref integer i) (ash 1 j)))))))
    (t
     (type-error integer 'INTEGER))))

(defconst max-rationalize-denominator (exact-expt 2 52))

(defun LOGCOUNT (num)
  (when (MINUSP num)
    (setq num (cl:- num)))
  (let ((len 0))
    (cond
      ((integerp num)
       (dotimes (i fixnum-bits)
	 (when (LOGBITP i num)
	   (incf len))))
      (t
       (dotimes (i (1- (length num)))
	 (dotimes (j fixnum-bits)
	   (when (LOGBITP i num)
	     (incf len))))))
    len))

(defun LOGTEST (num1 num2)
  (NOT (ZEROP (LOGAND num1 num2))))

(defun BYTE (size pos)
  (list size pos))

(defun BYTE-SIZE (bytespec)
  (first bytespec))

(defun BYTE-POSITION (bytespec)
  (second bytespec))

(defun DEPOSIT-FIELD (newbyte bytespec integer)
  (LOGIOR (LOGAND integer (LOGNOT (DPB -1 bytespec 0)))
	  (MASK-FIELD bytespec newbyte)))

(defun DPB (newbyte bytespec integer)
  (let ((mask (cl:1- (ASH 1 (BYTE-SIZE bytespec)))))
    (LOGIOR (LOGANDC2 integer (ASH mask (BYTE-POSITION bytespec)))
	    (ASH (LOGAND newbyte mask) (BYTE-POSITION bytespec)))))

(defun LDB (bytespec integer)
  (LOGAND (ASH integer (cl:- (BYTE-POSITION bytespec)))
	  (cl:1- (ASH 1 (BYTE-SIZE bytespec)))))

(DEFINE-SETF-EXPANDER LDB (bytespec integer)
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION integer nil) ;TODO: environment
    (let ((byte (gensym))
	  (value (gensym)))
      (values (cons byte temps)
	      (cons bytespec values)
	      (list value)
	      `(let ((,(first variables) (DPB ,value ,byte ,getter)))
		,setter
		,value)
	      `(LDB ,byte ,getter)))))

(defun LDB-TEST (bytespec integer)
  (NOT (ZEROP (LDB bytespec integer))))

(defun MASK-FIELD (bytespec integer)
  (LOGAND integer (DPB -1 bytespec 0)))

(DEFCONSTANT MOST-POSITIVE-FIXNUM most-positive-fixnum)

(DEFCONSTANT MOST-NEGATIVE-FIXNUM most-negative-fixnum)

(DEFINE-SETF-EXPANDER MASK-FIELD (bytespec integer)
  (MULTIPLE-VALUE-BIND (temps values variables setter getter)
      (GET-SETF-EXPANSION integer nil) ;TODO: &environment
    (let ((byte (gensym))
	  (value (gensym)))
    (values (cons byte temps)
	    (cons bytespec values)
	    (list value)
	    `(let ((,(first variables) (DEPOSIT-FIELD ,value ,byte ,getter)))
	      ,setter
	      ,value)
	    `(MASK-FIELD ,byte ,getter)))))

(defun DECODE-FLOAT (float)
  (unless (floatp float)
    (type-error float 'FLOAT))
  (if (zerop float)
      (cl:values 0.0 0 1.0)
      (let ((exponent (1+ (logb float))))
	(cl:values (* (abs float) (expt 2 (- (float exponent))))
		   exponent
		   (if (minusp float) -1.0 1.0)))))

(defun SCALE-FLOAT (float integer)
  (unless (floatp float)
    (type-error float 'FLOAT))
  (unless (INTEGERP integer)
    (type-error integer 'INTEGER))
  (* float (expt 2.0 (FLOAT integer))))

(defun FLOAT-RADIX (float)
  (unless (floatp float)
    (type-error float 'FLOAT))
  2)

(cl:defun FLOAT-SIGN (float1 &OPTIONAL (float2 1.0))
  (if (minusp float1)
      (- float2)
      float2))

(defun FLOAT-DIGITS (float)
  (unless (floatp float)
    (type-error float 'FLOAT))
  53)

(defun FLOAT-PRECISION (float)
  (unless (floatp float)
    (type-error float 'FLOAT))
  (if (zerop float)
      0
      ;; TODO: return number of significant digits in denormals.
      53))

(defun INTEGER-DECODE-FLOAT (float)
  (unless (floatp float)
    (type-error float 'FLOAT))
  (if (zerop float)
      (cl:values 0.0 0 1)
      (let ((exponent (1+ (logb float))))
	(cl:values (* (abs float) (expt 2 (- (float exponent))))
		   exponent
		   (if (minusp float) 1 1)))))

(defun bignum-float (num)
  (do ((i 1 (1+ i))
       (w 1.0 (* w two^fixnum-bits))
       (x 0.0)
       (len (1- (length num))))
      ((eq i len)
       (+ x (* w (aref num i))))
    (let ((y (aref num i)))
      (incf x (* w (if (minusp y) (+ two^fixnum-bits y) y))))))

(defun FLOAT (num &optional prototype)
  (cond
    ((integerp num)
     (float num))
    ((floatp num)
     num)
    ((ratiop num)
     (/ (FLOAT (NUMERATOR num)) (FLOAT (DENOMINATOR num))))
    ((bignump num)
     (bignum-float num))
    (t
     (type-error num 'REAL))))

(fset 'FLOATP (symbol-function 'floatp))

(DEFCONSTANT MOST-POSITIVE-SHORT-FLOAT 0.0)
(DEFCONSTANT LEAST-POSITIVE-SHORT-FLOAT 0.0)
(DEFCONSTANT LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT 0.0)
(DEFCONSTANT MOST-POSITIVE-DOUBLE-FLOAT 0.0)
(DEFCONSTANT LEAST-POSITIVE-DOUBLE-FLOAT 0.0)
(DEFCONSTANT LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT 0.0)
(DEFCONSTANT MOST-POSITIVE-LONG-FLOAT 0.0)
(DEFCONSTANT LEAST-POSITIVE-LONG-FLOAT 0.0)
(DEFCONSTANT LEAST-POSITIVE-NORMALIZED-LONG-FLOAT 0.0)
(DEFCONSTANT MOST-POSITIVE-SINGLE-FLOAT 0.0)
(DEFCONSTANT LEAST-POSITIVE-SINGLE-FLOAT 0.0)
(DEFCONSTANT LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT 0.0)
(DEFCONSTANT MOST-NEGATIVE-SHORT-FLOAT 0.0)
(DEFCONSTANT LEAST-NEGATIVE-SHORT-FLOAT 0.0)
(DEFCONSTANT LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT 0.0)
(DEFCONSTANT MOST-NEGATIVE-SINGLE-FLOAT 0.0)
(DEFCONSTANT LEAST-NEGATIVE-SINGLE-FLOAT 0.0)
(DEFCONSTANT LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT 0.0)
(DEFCONSTANT MOST-NEGATIVE-DOUBLE-FLOAT 0.0)
(DEFCONSTANT LEAST-NEGATIVE-DOUBLE-FLOAT 0.0)
(DEFCONSTANT LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT 0.0)
(DEFCONSTANT MOST-NEGATIVE-LONG-FLOAT 0.0)
(DEFCONSTANT LEAST-NEGATIVE-LONG-FLOAT 0.0)
(DEFCONSTANT LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT 0.0)

(DEFCONSTANT SHORT-FLOAT-EPSILON 0.0)
(DEFCONSTANT SHORT-FLOAT-NEGATIVE-EPSILON 0.0)
(DEFCONSTANT SINGLE-FLOAT-EPSILON 0.0)
(DEFCONSTANT SINGLE-FLOAT-NEGATIVE-EPSILON 0.0)
(DEFCONSTANT DOUBLE-FLOAT-EPSILON 0.0)
(DEFCONSTANT DOUBLE-FLOAT-NEGATIVE-EPSILON 0.0)
(DEFCONSTANT LONG-FLOAT-EPSILON 0.0)
(DEFCONSTANT LONG-FLOAT-NEGATIVE-EPSILON 0.0)

;;; Defined in cl-conditions.el: ARITHMETIC-ERROR,
;;; ARITHMETIC-ERROR-OPERANDS, ARITHMETIC-ERROR-OPERATION,
;;; DIVISION-BY-ZERO, FLOATING-POINT-INVALID-OPERATION,
;;; FLOATING-POINT-INEXACT, FLOATING-POINT-OVERFLOW,
;;; FLOATING-POINT-UNDERFLOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-numbers.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./populate.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file does the final work of setting up packages.

(IN-PACKAGE "EMACS-CL")

(defun populate-packages ()
  (let ((emacs-cl-table (make-hash-table :test 'equal)))
    (aset *emacs-cl-package* 3 nil)	;shadowing symbols
    (aset *emacs-cl-package* 6 emacs-cl-table) ;hash table
    (aset *emacs-cl-package* 7 nil)	;exported symbols

    (dolist (sym '(&ALLOW-OTHER-KEYS &AUX &BODY &ENVIRONMENT &KEY &OPTIONAL
&REST &WHOLE *BREAK-ON-SIGNALS* *COMPILE-FILE-PATHNAME* *COMPILE-FILE-TRUENAME*
*COMPILE-PRINT* *COMPILE-VERBOSE* *DEBUG-IO* *DEBUGGER-HOOK*
*DEFAULT-PATHNAME-DEFAULTS* *ERROR-OUTPUT* *FEATURES* *GENSYM-COUNTER*
*LOAD-PATHNAME* *LOAD-PRINT* *LOAD-TRUENAME* *LOAD-VERBOSE* *MACROEXPAND-HOOK*
*MODULES* *PACKAGE* *PRINT-ARRAY* *PRINT-BASE* *PRINT-CASE* *PRINT-CIRCLE*
*PRINT-ESCAPE* *PRINT-GENSYM* *PRINT-LENGTH* *PRINT-LEVEL* *PRINT-LINES*
*PRINT-MISER-WIDTH* *PRINT-PPRINT-DISPATCH* *PRINT-PRETTY* *PRINT-RADIX*
*PRINT-READABLY* *PRINT-RIGHT-MARGIN* *QUERY-IO* *RANDOM-STATE* *READ-BASE*
*READ-DEFAULT-FLOAT-FORMAT* *READ-EVAL* *READ-SUPPRESS* *READTABLE*
*STANDARD-INPUT* *STANDARD-OUTPUT* *TERMINAL-IO* *TRACE-OUTPUT* ABORT ABS ACONS
ACOS ACOSH ADD-METHOD ADJOIN ADJUST-ARRAY ADJUSTABLE-ARRAY-P ALLOCATE-INSTANCE
ALPHA-CHAR-P ALPHANUMERICP AND APPEND APPLY APROPOS APROPOS-LIST AREF
ARITHMETIC-ERROR ARITHMETIC-ERROR-OPERANDS ARITHMETIC-ERROR-OPERATION ARRAY
ARRAY-DIMENSION ARRAY-DIMENSION-LIMIT ARRAY-DIMENSIONS ARRAY-DISPLACEMENT
ARRAY-ELEMENT-TYPE ARRAY-HAS-FILL-POINTER-P ARRAY-IN-BOUNDS-P ARRAY-RANK
ARRAY-RANK-LIMIT ARRAY-ROW-MAJOR-INDEX ARRAY-TOTAL-SIZE ARRAY-TOTAL-SIZE-LIMIT
ARRAYP ASH ASIN ASINH ASSERT ASSOC ASSOC-IF ASSOC-IF-NOT ATAN ATANH ATOM
BASE-CHAR BASE-STRING BIGNUM BIT BIT-AND BIT-ANDC1 BIT-ANDC2 BIT-EQV BIT-IOR
BIT-NAND BIT-NOR BIT-NOT BIT-ORC1 BIT-ORC2 BIT-VECTOR BIT-VECTOR-P BIT-XOR
BLOCK BOOLE BOOLE-1 BOOLE-2 BOOLE-AND BOOLE-ANDC1 BOOLE-ANDC2 BOOLE-C1 BOOLE-C2
BOOLE-CLR BOOLE-EQV BOOLE-IOR BOOLE-NAND BOOLE-NOR BOOLE-ORC1 BOOLE-ORC2
BOOLE-SET BOOLE-XOR BOOLEAN BOTH-CASE-P BOUNDP BREAK BROADCAST-STREAM
BROADCAST-STREAM-STREAMS BUILT-IN-CLASS BUTLAST BYTE BYTE-POSITION BYTE-SIZE
CAAAAR CAAADR CAAAR CAADAR CAADDR CAADR CAAR CADAAR CADADR CADAR CADDAR CADDDR
CALL-ARGUMENTS-LIMIT CADDR CADR CAR CASE CATCH CCASE CDAAAR CDAADR CDAAR CDADAR
CDADDR CDADR CDAR CDDAAR CDDADR CDDAR CDDDAR CDDDDR CDDDR CDDR CDR CEILING
CELL-ERROR CELL-ERROR-NAME CERROR CHAR CHAR-CODE CHAR-CODE-LIMIT CHAR-DOWNCASE
CHAR-EQUAL CHAR-GREATERP CHAR-INT CHAR-LESSP CHAR-NAME CHAR-NOT-EQUAL
CHAR-NOT-GREATERP CHAR-NOT-LESSP CHAR-UPCASE CHAR/= CHAR< CHAR<= CHAR= CHAR>
CHAR>= CHARACTER CHARACTERP CHECK-TYPE CIS CLASS CLASS-NAME CLASS-OF
CLEAR-INPUT CLEAR-OUTPUT CLOSE CLRHASH CODE-CHAR COERCE COMPILATION-SPEED
COMPILE COMPILE-FILE COMPILE-FILE-PATHNAME COMPILED-FUNCTION
COMPILED-FUNCTION-P COMPILER-MACRO COMPILER-MACRO-FUNCTION COMPLEMENT COMPLEX
COMPLEXP COMPUTE-RESTARTS CONCATENATE CONCATENATED-STREAM
CONCATENATED-STREAM-STREAMS COND CONDITION CONJUGATE CONS CONSP CONSTANTLY
CONSTANTP CONTINUE CONTROL-ERROR COPY-ALIST COPY-LIST COPY-PPRINT-DISPATCH
COPY-READTABLE COPY-SEQ COPY-STRUCTURE COPY-SYMBOL COPY-TREE COS COSH COUNT
COUNT-IF COUNT-IF-NOT CTYPECASE DEBUG DECF DECLAIM DECLARATION DECLARE
DECODE-FLOAT DECODE-UNIVERSAL-TIME DEFCONSTANT DEFCLASS DEFGENERIC
DEFINE-COMPILER-MACRO DEFINE-CONDITION DEFINE-MODIFY-MACRO DEFINE-SETF-EXPANDER
DEFINE-SYMBOL-MACRO DEFMACRO DEFMETHOD DEFPACKAGE DEFPARAMETER DEFSETF
DEFSTRUCT DEFTYPE DEFUN DEFVAR DELETE DELETE-DUPLICATES DELETE-FILE DELETE-IF
DELETE-IF-NOT DELETE-PACKAGE DENOMINATOR DEPOSIT-FIELD DESCRIBE DESCRIBE-OBJECT
DESTRUCTURING-BIND DIGIT-CHAR DIGIT-CHAR-P DIRECTORY DIRECTORY-NAMESTRING
DISASSEMBLE DIVISION-BY-ZERO DO DO* DO-ALL-SYMBOLS DO-EXTERNAL-SYMBOLS
DO-SYMBOLS DOCUMENTATION DOLIST DOTIMES DOUBLE-FLOAT DOUBLE-FLOAT-EPSILON
DOUBLE-FLOAT-NEGATIVE-EPSILON DPB DYNAMIC-EXTENT ECASE ECHO-STREAM
ECHO-STREAM-INPUT-STREAM ECHO-STREAM-OUTPUT-STREAM ED EIGHTH ELT
ENCODE-UNIVERSAL-TIME END-OF-FILE ENDP ENOUGH-NAMESTRING
ENSURE-DIRECTORIES-EXIST ENSURE-GENERIC-FUNCTION EQ EQL EQUAL EQUALP ERROR
ETYPECASE EVAL EVAL-WHEN EVENP EVERY EXP EXPORT EXPT EXTENDED-CHAR FBOUNDP
FCEILING FDEFINITION FFLOOR FIFTH FILE-AUTHOR FILE-ERROR FILE-ERROR-PATHNAME
FILE-LENGTH FILE-NAMESTRING FILE-POSITION FILE-STREAM FILE-STRING-LENGTH
FILE-WRITE-DATE FILL FILL-POINTER FIND FIND-CLASS FIND-IF FIND-IF-NOT
FIND-ALL-SYMBOLS FIND-PACKAGE FIND-RESTART FIND-SYMBOL FINISH-OUTPUT FIRST
FIXNUM FLET FLOAT FLOAT-DIGITS FLOAT-PRECISION FLOAT-RADIX FLOAT-SIGN
FLOATING-POINT-INEXACT FLOATING-POINT-INVALID-OPERATION FLOATING-POINT-OVERFLOW
FLOATING-POINT-UNDERFLOW FLOATP FLOOR FMAKUNBOUND FORCE-OUTPUT FORMAT FORMATTER
FOURTH FRESH-LINE FROUND FTRUNCATE FTYPE FUNCALL FUNCTION
FUNCTION-LAMBDA-EXPRESSION FUNCTIONP GCD GENERIC-FUNCTION GENSYM GENTEMP GET
GET-DECODED-TIME GET-DISPATCH-MACRO-CHARACTER GET-INTERNAL-REAL-TIME
GET-INTERNAL-RUN-TIME GET-MACRO-CHARACTER GET-SETF-EXPANSION
GET-OUTPUT-STREAM-STRING GET-PROPERTIES GET-UNIVERSAL-TIME GETF GETHASH GO
GRAPHIC-CHAR-P HANDLER-BIND HANDLER-CASE HASH-TABLE HASH-TABLE-COUNT
HASH-TABLE-P HASH-TABLE-REHASH-SIZE HASH-TABLE-REHASH-THRESHOLD HASH-TABLE-SIZE
HASH-TABLE-TEST HOST-NAMESTRING IDENTITY IF IGNORE IGNORE-ERRORS IGNORABLE
IMAGPART IMPORT IN-PACKAGE INCF INITIALIZE-INSTANCE INLINE INPUT-STREAM-P
INTEGER INTEGER-DECODE-FLOAT INTEGER-LENGTH INTEGERP INTERACTIVE-STREAM-P
INTERN INTERNAL-TIME-UNITS-PER-SECOND INTERSECTION INVALID-METHOD-ERROR
INVOKE-DEBUGGER INVOKE-RESTART ISQRT KEYWORD KEYWORDP LABELS LAMBDA
LAMBDA-LIST-KEYWORDS LAMBDA-PARAMETERS-LIMIT LAST LCM LDB LDB-TEST LDIFF
LEAST-NEGATIVE-DOUBLE-FLOAT LEAST-NEGATIVE-LONG-FLOAT
LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT
LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT
LEAST-NEGATIVE-SHORT-FLOAT LEAST-NEGATIVE-SINGLE-FLOAT
LEAST-POSITIVE-DOUBLE-FLOAT LEAST-POSITIVE-LONG-FLOAT
LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT LEAST-POSITIVE-NORMALIZED-LONG-FLOAT
LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT
LEAST-POSITIVE-SHORT-FLOAT LEAST-POSITIVE-SINGLE-FLOAT LENGTH LET LET*
LISP-IMPLEMENTATION-TYPE LISP-IMPLEMENTATION-VERSION LIST LIST*
LIST-ALL-PACKAGES LIST-LENGTH LISTEN LISTP LOAD-TIME-VALUE LOAD LOCALLY LOG
LOGAND LOGANDC1 LOGANDC2 LOGBITP LOGCOUNT LOGEQV LOGICAL-PATHNAME
LOGICAL-PATHNAME-TRANSLATIONS LOGIOR LOGNAND LOGNOR LOGNOT LOGORC1 LOGORC2
LOGTEST LOGXOR LONG-FLOAT LONG-FLOAT-EPSILON LONG-FLOAT-NEGATIVE-EPSILON
LONG-SITE-NAME LOOP LOOP-FINISH LOWER-CASE-P MACHINE-INSTANCE MACHINE-TYPE
MACHINE-VERSION MACRO-FUNCTION MACROEXPAND MACROEXPAND-1 MACROLET MAKE-ARRAY
MAKE-BROADCAST-STREAM MAKE-CONCATENATED-STREAM MAKE-CONDITION
MAKE-DISPATCH-MACRO-CHARACTER MAKE-ECHO-STREAM MAKE-INSTANCE MAKE-HASH-TABLE
MAKE-LIST MAKE-LOAD-FORM MAKE-LOAD-FORM-SAVING-SLOTS MAKE-PACKAGE MAKE-PATHNAME
MAKE-RANDOM-STATE MAKE-SEQUENCE MAKE-STRING MAKE-SYMBOL
MAKE-STRING-INPUT-STREAM MAKE-STRING-OUTPUT-STREAM MAKE-SYNONYM-STREAM
MAKE-TWO-WAY-STREAM MAKUNBOUND MAP MAP-INTO MAPC MAPCAN MAPCAR MAPCON MAPHASH
MAPL MAPLIST MASK-FIELD MAX MEMBER MEMBER-IF MEMBER-IF-NOT MERGE
MERGE-PATHNAMES METHOD-COMBINATION-ERROR MIN MINUSP METHOD METHOD-COMBINATION
MISMATCH MOD MOST-NEGATIVE-DOUBLE-FLOAT MOST-NEGATIVE-FIXNUM
MOST-NEGATIVE-LONG-FLOAT MOST-NEGATIVE-SHORT-FLOAT MOST-NEGATIVE-SINGLE-FLOAT
MOST-POSITIVE-DOUBLE-FLOAT MOST-POSITIVE-FIXNUM MOST-POSITIVE-LONG-FLOAT
MOST-POSITIVE-SHORT-FLOAT MOST-POSITIVE-SINGLE-FLOAT MUFFLE-WARNING
MULTIPLE-VALUE-BIND MULTIPLE-VALUE-CALL MULTIPLE-VALUE-LIST
MULTIPLE-VALUE-PROG1 MULTIPLE-VALUE-SETQ MULTIPLE-VALUES-LIMIT NAME-CHAR
NAMESTRING NBUTLAST NCONC NINTERSECTION NINTH NO-APPLICABLE-METHOD NOT NOTANY
NOTEVERY NOTINLINE NRECONC NREVERSE NSET-DIFFERENCE NSET-EXCLUSIVE-OR
NSTRING-CAPITALIZE NSTRING-DOWNCASE NSTRING-UPCASE NSUBLIS NSUBST NSUBST-IF
NSUBST-IF-NOT NSUBSTITUTE NSUBSTITUTE-IF NSUBSTITUTE-IF-NOT NTH NTH-VALUE
NTHCDR NULL NUMBER NUMBERP NUMERATOR NUNION ODDP OPEN OPEN-STREAM-P OPTIMIZE OR
OTHERWISE OUTPUT-STREAM-P PACKAGE PACKAGE-ERROR PACKAGE-ERROR-PACKAGE
PACKAGE-NAME PACKAGE-NICKNAMES PACKAGE-SHADOWING-SYMBOLS PACKAGE-USE-LIST
PACKAGE-USED-BY-LIST PACKAGEP PAIRLIS PARSE-ERROR PARSE-INTEGER
PARSE-NAMESTRING PATHNAME PATHNAME-HOST PATHNAME-DEVICE PATHNAME-DIRECTORY
PATHNAME-MATCH-P PATHNAME-NAME PATHNAME-TYPE PATHNAME-VERSION PATHNAMEP
PEEK-CHAR PHASE PI PLUSP POP POSITION POSITION-IF POSITION-IF-NOT PPRINT
PPRINT-DISPATCH PRIN1 PRIN1-TO-STRING PRINC PRINC-TO-STRING PRINT
PRINT-NOT-READABLE PRINT-NOT-READABLE-OBJECT PRINT-OBJECT
PRINT-UNREADABLE-OBJECT PROBE-FILE PROCLAIM PROG PROG* PROG1 PROG2 PROGN
PROGRAM-ERROR PROVIDE PSETF PSETQ PROGV PUSH PUSHNEW QUOTE RANDOM RANDOM-STATE
RANDOM-STATE-P RASSOC RASSOC-IF RASSOC-IF-NOT RATIO RATIONAL RATIONALIZE
RATIONALP READ READ-BYTE READ-CHAR READ-CHAR-NO-HANG READ-DELIMITED-LIST
READ-FROM-STRING READ-LINE READ-PRESERVING-WHITESPACE READ-SEQUENCE
READER-ERROR READTABLE READTABLE-CASE READTABLEP REAL REALP REALPART REDUCE REM
REMF REMHASH REMOVE REMOVE-DUPLICATES REMOVE-IF REMOVE-IF-NOT REMPROP
RENAME-FILE RENAME-PACKAGE REPLACE REQUIRE REST RESTART RESTART-BIND
RESTART-NAME RETURN RETURN-FROM REVAPPEND REVERSE ROOM ROTATEF ROUND
ROW-MAJOR-AREF RPLACA RPLACD SAFETY SATISFIES SBIT SCALE-FLOAT SCHAR SEARCH
SECOND SEQUENCE SERIOUS-CONDITION SET SET-DIFFERENCE
SET-DISPATCH-MACRO-CHARACTER SET-EXCLUSIVE-OR SET-MACRO-CHARACTER
SET-PPRINT-DISPATCH SET-SYNTAX-FROM-CHAR SETF SETQ SEVENTH SHADOW
SHADOWING-IMPORT SHARED-INITIALIZE SHIFTF SHORT-FLOAT SHORT-FLOAT-EPSILON
SHORT-FLOAT-NEGATIVE-EPSILON SHORT-SITE-NAME SIGNAL SIGNED-BYTE SIGNUM
SIMPLE-ARRAY SIMPLE-BASE-STRING SIMPLE-BIT-VECTOR SIMPLE-BIT-VECTOR-P
SIMPLE-CONDITION SIMPLE-CONDITION-FORMAT-ARGUMENTS
SIMPLE-CONDITION-FORMAT-CONTROL SIMPLE-ERROR SIMPLE-STRING SIMPLE-STRING-P
SIMPLE-TYPE-ERROR SIMPLE-VECTOR SIMPLE-VECTOR-P SIMPLE-WARNING SIN SINGLE-FLOAT
SINGLE-FLOAT-EPSILON SINGLE-FLOAT-NEGATIVE-EPSILON SINH SIXTH SLEEP SLOT-BOUNDP
SLOT-EXISTS-P SLOT-MAKUNBOUND SLOT-MISSING SLOT-UNBOUND SLOT-VALUE
SOFTWARE-TYPE SOFTWARE-VERSION SOME SORT SPACE SPECIAL SPECIAL-OPERATOR-P SPEED
SQRT STABLE-SORT STANDARD-CHAR STANDARD-CHAR-P STANDARD-CLASS STANDARD-OBJECT
STANDARD-GENERIC-FUNCTION STANDARD-METHOD STEP STORAGE-CONDITION STORE-VALUE
STREAM STREAM-ELEMENT-TYPE STREAM-ERROR STREAM-ERROR-STREAM
STREAM-EXTERNAL-FORMAT STREAMP STRING STRING-CAPITALIZE STRING-DOWNCASE
STRING-EQUAL STRING-GREATERP STRING-LEFT-TRIM STRING-LESSP STRING-NOT-EQUAL
STRING-NOT-GREATERP STRING-NOT-LESSP STRING-RIGHT-TRIM STRING-STREAM
STRING-TRIM STRING-UPCASE STRING/= STRING< STRING<= STRING= STRING> STRING>=
STRINGP STRUCTURE STRUCTURE-CLASS STRUCTURE-OBJECT STYLE-WARNING SUBLIS SUBSEQ
SUBSETP SUBST SUBST-IF SUBST-IF-NOT SUBSTITUTE SUBSTITUTE-IF SUBSTITUTE-IF-NOT
SUBTYPEP SVREF SXHASH SYMBOL SYMBOL-FUNCTION SYMBOL-MACROLET SYMBOL-NAME
SYMBOL-PACKAGE SYMBOL-PLIST SYMBOL-VALUE SYMBOLP SYNONYM-STREAM
SYNONYM-STREAM-SYMBOL T TAGBODY TAILP TAN TANH TENTH TERPRI THE THIRD THROW
TIME TRACE TRANSLATE-PATHNAME TREE-EQUAL TRUENAME TRUNCATE TWO-WAY-STREAM
TWO-WAY-STREAM-INPUT-STREAM TWO-WAY-STREAM-OUTPUT-STREAM TYPE TYPE-ERROR
TYPE-ERROR-DATUM TYPE-ERROR-EXPECTED-TYPE TYPE-OF TYPECASE TYPEP UNBOUND-SLOT
UNBOUND-SLOT-INSTANCE UNBOUND-VARIABLE UNDEFINED-FUNCTION UNEXPORT UNINTERN
UNION UNLESS UNREAD-CHAR UNSIGNED-BYTE UNTRACE UNUSE-PACKAGE UNWIND-PROTECT
UPGRADED-ARRAY-ELEMENT-TYPE UPGRADED-COMPLEX-PART-TYPE UPPER-CASE-P USE-PACKAGE
USE-VALUE USER-HOMEDIR-PATHNAME VALUES VALUES-LIST VARIABLE VECTOR VECTOR-POP
VECTOR-PUSH VECTOR-PUSH-EXTEND VECTORP WARN WARNING WHEN WILD-PATHNAME-P
WITH-COMPILATION-UNIT WITH-HASH-TABLE-ITERATOR WITH-INPUT-FROM-STRING
WITH-OPEN-FILE WITH-OPEN-STREAM WITH-OUTPUT-TO-STRING WITH-PACKAGE-ITERATOR
WITH-STANDARD-IO-SYNTAX WRITE WRITE-BYTE WRITE-CHAR WRITE-LINE WRITE-SEQUENCE
WRITE-STRING WRITE-TO-STRING Y-OR-N-P YES-OR-NO-P ZEROP))
      (setf (gethash (SYMBOL-NAME sym) emacs-cl-table) sym)
      (setf (SYMBOL-PACKAGE sym) *emacs-cl-package*))

    ;; NIL is a special case, because its Emacs Lisp symbol-name isn't
    ;; equal to its Common Lisp SYMBOL-NAME.
    (setf (gethash "NIL" emacs-cl-table) nil)
    (setf (SYMBOL-PACKAGE nil) *emacs-cl-package*)

    ;; * is treated specially because it's used in type specifiers,
    ;; and it must be distinct from EMACS-LISP:*.
    (setf (gethash "*" emacs-cl-table) star)
    (setf (SYMBOL-PACKAGE star) *emacs-cl-package*)
    (set star nil)
    (fset star (symbol-function 'cl:*))
    (MULTIPLE-VALUE-BIND (symbol found)	(FIND-SYMBOL "*" *cl-package*)
      (when found
	(UNINTERN symbol *cl-package*)))
    (MULTIPLE-VALUE-BIND (symbol found) (FIND-SYMBOL "*" *emacs-cl-package*)
      (when found
	(IMPORT (list symbol) *cl-package*)
	(EXPORT (list symbol) *cl-package*)))

    ;; Symbols prefixed with "cl:" in Emacs Lisp.
    (dolist (name '("=" "/=" "<" ">" "<=" ">=" "*" "+" "-" "/" "1+" "1-"))
      (let ((to (INTERN name *emacs-cl-package*))
	    ;;(to (make-symbol name))
	    (from (intern (concat "cl:" name))))
	;;(setf (gethash name emacs-cl-table) to)
	;;(setf (SYMBOL-PACKAGE to) *emacs-cl-package*)
	(if (boundp from)
	    (set to (symbol-value from)))
	(fset to (symbol-function from))))

    (dolist (sym '(** *** ++ +++ // ///))
      (setf (gethash (symbol-name sym) emacs-cl-table) sym)
      (setf (SYMBOL-PACKAGE sym) *emacs-cl-package*)
      (set sym nil))

    ;; Internal symbols.
    (dolist (sym '(BACKQUOTE COMMA COMMA-AT COMMA-DOT))
      (setf (gethash (SYMBOL-NAME sym) emacs-cl-table) sym)
      (setf (SYMBOL-PACKAGE sym) *emacs-cl-package*))

    ;; Additional external symbols.
    (dolist (sym '(INTERPRETED-FUNCTION INTERPRETED-FUNCTION-P INTERACTIVE))
      (setf (gethash (SYMBOL-NAME sym) emacs-cl-table) sym)
      (setf (SYMBOL-PACKAGE sym) *emacs-cl-package*)
      (EXPORT sym *emacs-cl-package*))

    ;; External MOP symbols.
    (let ((table (aref *mop-package* 6)))
      (dolist (sym '(ADD-DIRECT-SUBCLASS CLASS-DIRECT-SUBCLASSES
		     CLASS-DIRECT-SUPERCLASSES CLASS-SLOTS CLASS-FINALIZED-P
		     COMPUTE-DISCRIMINATING-FUNCTION ENSURE-CLASS-USING-CLASS
		     ENSURE-GENERIC-FUNCTION-USING-CLASS EQL-SPECIALIZER
		     EQL-SPECIALIZER-OBJECT EXTRACT-LAMBDA-LIST
		     EXTRACT-SPECIALIZER-NAMES FINALIZE-INHERITANCE
		     FUNCALLABLE-STANDARD-CLASS
		     FUNCALLABLE-STANDARD-INSTANCE-ACCESS
		     FUNCALLABLE-STANDARD-OBJECT GENERIC-FUNCTION-LAMBDA-LIST
		     GENERIC-FUNCTION-METHODS INTERN-EQL-SPECIALIZER METAOBJECT
		     METHOD-SPECIALIZERS SET-FUNCALLABLE-INSTANCE-FUNCTION
		     SLOT-DEFINITION SLOT-NAME SPECIALIZER
		     STANDARD-ACCESSOR-METHOD STANDARD-READER-METHOD
		     STANDARD-WRITER-METHOD))
	(setf (gethash (SYMBOL-NAME sym) table) sym)
	(setf (SYMBOL-PACKAGE sym) *mop-package*)
	(EXPORT sym *mop-package*)))

    (dolist (sym '(&ALLOW-OTHER-KEYS &AUX &BODY &ENVIRONMENT &KEY &OPTIONAL
&REST &WHOLE ** *** *BREAK-ON-SIGNALS* *COMPILE-FILE-PATHNAME*
*COMPILE-FILE-TRUENAME* *COMPILE-PRINT* *COMPILE-VERBOSE* *DEBUG-IO*
*DEBUGGER-HOOK* *DEFAULT-PATHNAME-DEFAULTS* *ERROR-OUTPUT* *FEATURES*
*GENSYM-COUNTER* *LOAD-PATHNAME* *LOAD-PRINT* *LOAD-TRUENAME* *LOAD-VERBOSE*
*MACROEXPAND-HOOK* *MODULES* *PACKAGE* *PRINT-ARRAY* *PRINT-BASE* *PRINT-CASE*
*PRINT-CIRCLE* *PRINT-ESCAPE* *PRINT-GENSYM* *PRINT-LENGTH* *PRINT-LEVEL*
*PRINT-LINES* *PRINT-MISER-WIDTH* *PRINT-PPRINT-DISPATCH* *PRINT-PRETTY*
*PRINT-RADIX* *PRINT-READABLY* *PRINT-RIGHT-MARGIN* *QUERY-IO* *RANDOM-STATE*
*READ-BASE* *READ-DEFAULT-FLOAT-FORMAT* *READ-EVAL* *READ-SUPPRESS* *READTABLE*
*STANDARD-INPUT* *STANDARD-OUTPUT* *TERMINAL-IO* *TRACE-OUTPUT* + ++ +++ - / //
/// /= 1+ 1- < <= = > >= ABORT ABS ACONS ACOS ACOSH ADD-METHOD ADJOIN
ADJUST-ARRAY ADJUSTABLE-ARRAY-P ALLOCATE-INSTANCE ALPHA-CHAR-P ALPHANUMERICP
AND APPEND APPLY APROPOS APROPOS-LIST AREF ARITHMETIC-ERROR
ARITHMETIC-ERROR-OPERANDS ARITHMETIC-ERROR-OPERATION ARRAY ARRAY-DIMENSION
ARRAY-DIMENSION-LIMIT ARRAY-DIMENSIONS ARRAY-DISPLACEMENT ARRAY-ELEMENT-TYPE
ARRAY-HAS-FILL-POINTER-P ARRAY-IN-BOUNDS-P ARRAY-RANK ARRAY-RANK-LIMIT
ARRAY-ROW-MAJOR-INDEX ARRAY-TOTAL-SIZE ARRAY-TOTAL-SIZE-LIMIT ARRAYP ASH ASIN
ASINH ASSERT ASSOC ASSOC-IF ASSOC-IF-NOT ATAN ATANH ATOM BASE-CHAR BASE-STRING
BIGNUM BIT BIT-AND BIT-ANDC1 BIT-ANDC2 BIT-EQV BIT-IOR BIT-NAND BIT-NOR BIT-NOT
BIT-ORC1 BIT-ORC2 BIT-VECTOR BIT-VECTOR-P BIT-XOR BLOCK BOOLE BOOLE-1 BOOLE-2
BOOLE-AND BOOLE-ANDC1 BOOLE-ANDC2 BOOLE-C1 BOOLE-C2 BOOLE-CLR BOOLE-EQV
BOOLE-IOR BOOLE-NAND BOOLE-NOR BOOLE-ORC1 BOOLE-ORC2 BOOLE-SET BOOLE-XOR
BOOLEAN BOTH-CASE-P BOUNDP BREAK BROADCAST-STREAM BROADCAST-STREAM-STREAMS
BUILT-IN-CLASS BUTLAST BYTE BYTE-POSITION BYTE-SIZE CAAAAR CAAADR CAAAR CAADAR
CAADDR CAADR CAAR CADAAR CADADR CADAR CADDAR CADDDR CADDR CADR
CALL-ARGUMENTS-LIMIT CALL-METHOD CALL-NEXT-METHOD CAR CASE CATCH CCASE CDAAAR
CDAADR CDAAR CDADAR CDADDR CDADR CDAR CDDAAR CDDADR CDDAR CDDDAR CDDDDR CDDDR
CDDR CDR CEILING CELL-ERROR CELL-ERROR-NAME CERROR CHANGE-CLASS CHAR CHAR-CODE
CHAR-CODE-LIMIT CHAR-DOWNCASE CHAR-EQUAL CHAR-GREATERP CHAR-INT CHAR-LESSP
CHAR-NAME CHAR-NOT-EQUAL CHAR-NOT-GREATERP CHAR-NOT-LESSP CHAR-UPCASE CHAR/=
CHAR< CHAR<= CHAR= CHAR> CHAR>= CHARACTER CHARACTERP CHECK-TYPE CIS CLASS
CLASS-NAME CLASS-OF CLEAR-INPUT CLEAR-OUTPUT CLOSE CLRHASH CODE-CHAR COERCE
COMPILATION-SPEED COMPILE COMPILE-FILE COMPILE-FILE-PATHNAME COMPILED-FUNCTION
COMPILED-FUNCTION-P COMPILER-MACRO COMPILER-MACRO-FUNCTION COMPLEMENT COMPLEX
COMPLEXP COMPUTE-APPLICABLE-METHODS COMPUTE-RESTARTS CONCATENATE
CONCATENATED-STREAM CONCATENATED-STREAM-STREAMS COND CONDITION CONJUGATE CONS
CONSP CONSTANTLY CONSTANTP CONTINUE CONTROL-ERROR COPY-ALIST COPY-LIST
COPY-PPRINT-DISPATCH COPY-READTABLE COPY-SEQ COPY-STRUCTURE COPY-SYMBOL
COPY-TREE COS COSH COUNT COUNT-IF COUNT-IF-NOT CTYPECASE DEBUG DECF DECLAIM
DECLARATION DECLARE DECODE-FLOAT DECODE-UNIVERSAL-TIME DEFCLASS DEFCONSTANT
DEFGENERIC DEFINE-COMPILER-MACRO DEFINE-CONDITION DEFINE-METHOD-COMBINATION
DEFINE-MODIFY-MACRO DEFINE-SETF-EXPANDER DEFINE-SYMBOL-MACRO DEFMACRO DEFMETHOD
DEFPACKAGE DEFPARAMETER DEFSETF DEFSTRUCT DEFTYPE DEFUN DEFVAR DELETE
DELETE-DUPLICATES DELETE-FILE DELETE-IF DELETE-IF-NOT DELETE-PACKAGE
DENOMINATOR DEPOSIT-FIELD DESCRIBE DESCRIBE-OBJECT DESTRUCTURING-BIND
DIGIT-CHAR DIGIT-CHAR-P DIRECTORY DIRECTORY-NAMESTRING DISASSEMBLE
DIVISION-BY-ZERO DO DO* DO-ALL-SYMBOLS DO-EXTERNAL-SYMBOLS DO-SYMBOLS
DOCUMENTATION DOLIST DOTIMES DOUBLE-FLOAT DOUBLE-FLOAT-EPSILON
DOUBLE-FLOAT-NEGATIVE-EPSILON DPB DRIBBLE DYNAMIC-EXTENT ECASE ECHO-STREAM
ECHO-STREAM-INPUT-STREAM ECHO-STREAM-OUTPUT-STREAM ED EIGHTH ELT
ENCODE-UNIVERSAL-TIME END-OF-FILE ENDP ENOUGH-NAMESTRING
ENSURE-DIRECTORIES-EXIST ENSURE-GENERIC-FUNCTION EQ EQL EQUAL EQUALP ERROR
ETYPECASE EVAL EVAL-WHEN EVENP EVERY EXP EXPORT EXPT EXTENDED-CHAR FBOUNDP
FCEILING FDEFINITION FFLOOR FIFTH FILE-AUTHOR FILE-ERROR FILE-ERROR-PATHNAME
FILE-LENGTH FILE-NAMESTRING FILE-POSITION FILE-STREAM FILE-STRING-LENGTH
FILE-WRITE-DATE FILL FILL-POINTER FIND FIND-ALL-SYMBOLS FIND-CLASS FIND-IF
FIND-IF-NOT FIND-METHOD FIND-PACKAGE FIND-RESTART FIND-SYMBOL FINISH-OUTPUT
FIRST FIXNUM FLET FLOAT FLOAT-DIGITS FLOAT-PRECISION FLOAT-RADIX FLOAT-SIGN
FLOATING-POINT-INEXACT FLOATING-POINT-INVALID-OPERATION FLOATING-POINT-OVERFLOW
FLOATING-POINT-UNDERFLOW FLOATP FLOOR FMAKUNBOUND FORCE-OUTPUT FORMAT FORMATTER
FOURTH FRESH-LINE FROUND FTRUNCATE FTYPE FUNCALL FUNCTION FUNCTION-KEYWORDS
FUNCTION-LAMBDA-EXPRESSION FUNCTIONP GCD GENERIC-FUNCTION GENSYM GENTEMP GET
GET-DECODED-TIME GET-DISPATCH-MACRO-CHARACTER GET-INTERNAL-REAL-TIME
GET-INTERNAL-RUN-TIME GET-MACRO-CHARACTER GET-OUTPUT-STREAM-STRING
GET-PROPERTIES GET-SETF-EXPANSION GET-UNIVERSAL-TIME GETF GETHASH GO
GRAPHIC-CHAR-P HANDLER-BIND HANDLER-CASE HASH-TABLE HASH-TABLE-COUNT
HASH-TABLE-P HASH-TABLE-REHASH-SIZE HASH-TABLE-REHASH-THRESHOLD HASH-TABLE-SIZE
HASH-TABLE-TEST HOST-NAMESTRING IDENTITY IF IGNORABLE IGNORE IGNORE-ERRORS
IMAGPART IMPORT IN-PACKAGE INCF INITIALIZE-INSTANCE INLINE INPUT-STREAM-P
INSPECT INTEGER INTEGER-DECODE-FLOAT INTEGER-LENGTH INTEGERP
INTERACTIVE-STREAM-P INTERN INTERNAL-TIME-UNITS-PER-SECOND INTERSECTION
INVALID-METHOD-ERROR INVOKE-DEBUGGER INVOKE-RESTART
INVOKE-RESTART-INTERACTIVELY ISQRT KEYWORD KEYWORDP LABELS LAMBDA
LAMBDA-LIST-KEYWORDS LAMBDA-PARAMETERS-LIMIT LAST LCM LDB LDB-TEST LDIFF
LEAST-NEGATIVE-DOUBLE-FLOAT LEAST-NEGATIVE-LONG-FLOAT
LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT
LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT
LEAST-NEGATIVE-SHORT-FLOAT LEAST-NEGATIVE-SINGLE-FLOAT
LEAST-POSITIVE-DOUBLE-FLOAT LEAST-POSITIVE-LONG-FLOAT
LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT LEAST-POSITIVE-NORMALIZED-LONG-FLOAT
LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT
LEAST-POSITIVE-SHORT-FLOAT LEAST-POSITIVE-SINGLE-FLOAT LENGTH LET LET*
LISP-IMPLEMENTATION-TYPE LISP-IMPLEMENTATION-VERSION LIST LIST*
LIST-ALL-PACKAGES LIST-LENGTH LISTEN LISTP LOAD
LOAD-LOGICAL-PATHNAME-TRANSLATIONS LOAD-TIME-VALUE LOCALLY LOG LOGAND LOGANDC1
LOGANDC2 LOGBITP LOGCOUNT LOGEQV LOGICAL-PATHNAME LOGICAL-PATHNAME-TRANSLATIONS
LOGIOR LOGNAND LOGNOR LOGNOT LOGORC1 LOGORC2 LOGTEST LOGXOR LONG-FLOAT
LONG-FLOAT-EPSILON LONG-FLOAT-NEGATIVE-EPSILON LONG-SITE-NAME LOOP LOOP-FINISH
LOWER-CASE-P MACHINE-INSTANCE MACHINE-TYPE MACHINE-VERSION MACRO-FUNCTION
MACROEXPAND MACROEXPAND-1 MACROLET MAKE-ARRAY MAKE-BROADCAST-STREAM
MAKE-CONCATENATED-STREAM MAKE-CONDITION MAKE-DISPATCH-MACRO-CHARACTER
MAKE-ECHO-STREAM MAKE-HASH-TABLE MAKE-INSTANCE MAKE-INSTANCES-OBSOLETE
MAKE-LIST MAKE-LOAD-FORM MAKE-LOAD-FORM-SAVING-SLOTS MAKE-METHOD MAKE-PACKAGE
MAKE-PATHNAME MAKE-RANDOM-STATE MAKE-SEQUENCE MAKE-STRING
MAKE-STRING-INPUT-STREAM MAKE-STRING-OUTPUT-STREAM MAKE-SYMBOL
MAKE-SYNONYM-STREAM MAKE-TWO-WAY-STREAM MAKUNBOUND MAP MAP-INTO MAPC MAPCAN
MAPCAR MAPCON MAPHASH MAPL MAPLIST MASK-FIELD MAX MEMBER MEMBER-IF
MEMBER-IF-NOT MERGE MERGE-PATHNAMES METHOD METHOD-COMBINATION
METHOD-COMBINATION-ERROR METHOD-QUALIFIERS MIN MINUSP MISMATCH MOD
MOST-NEGATIVE-DOUBLE-FLOAT MOST-NEGATIVE-FIXNUM MOST-NEGATIVE-LONG-FLOAT
MOST-NEGATIVE-SHORT-FLOAT MOST-NEGATIVE-SINGLE-FLOAT MOST-POSITIVE-DOUBLE-FLOAT
MOST-POSITIVE-FIXNUM MOST-POSITIVE-LONG-FLOAT MOST-POSITIVE-SHORT-FLOAT
MOST-POSITIVE-SINGLE-FLOAT MUFFLE-WARNING MULTIPLE-VALUE-BIND
MULTIPLE-VALUE-CALL MULTIPLE-VALUE-LIST MULTIPLE-VALUE-PROG1
MULTIPLE-VALUE-SETQ MULTIPLE-VALUES-LIMIT NAME-CHAR NAMESTRING NBUTLAST NCONC
NEXT-METHOD-P NIL NINTERSECTION NINTH NO-APPLICABLE-METHOD NO-NEXT-METHOD NOT
NOTANY NOTEVERY NOTINLINE NRECONC NREVERSE NSET-DIFFERENCE NSET-EXCLUSIVE-OR
NSTRING-CAPITALIZE NSTRING-DOWNCASE NSTRING-UPCASE NSUBLIS NSUBST NSUBST-IF
NSUBST-IF-NOT NSUBSTITUTE NSUBSTITUTE-IF NSUBSTITUTE-IF-NOT NTH NTH-VALUE
NTHCDR NULL NUMBER NUMBERP NUMERATOR NUNION ODDP OPEN OPEN-STREAM-P OPTIMIZE OR
OTHERWISE OUTPUT-STREAM-P PACKAGE PACKAGE-ERROR PACKAGE-ERROR-PACKAGE
PACKAGE-NAME PACKAGE-NICKNAMES PACKAGE-SHADOWING-SYMBOLS PACKAGE-USE-LIST
PACKAGE-USED-BY-LIST PACKAGEP PAIRLIS PARSE-ERROR PARSE-INTEGER
PARSE-NAMESTRING PATHNAME PATHNAME-DEVICE PATHNAME-DIRECTORY PATHNAME-HOST
PATHNAME-MATCH-P PATHNAME-NAME PATHNAME-TYPE PATHNAME-VERSION PATHNAMEP
PEEK-CHAR PHASE PI PLUSP POP POSITION POSITION-IF POSITION-IF-NOT PPRINT
PPRINT-DISPATCH PPRINT-EXIT-IF-LIST-EXHAUSTED PPRINT-FILL PPRINT-INDENT
PPRINT-LINEAR PPRINT-LOGICAL-BLOCK PPRINT-NEWLINE PPRINT-POP PPRINT-TAB
PPRINT-TABULAR PRIN1 PRIN1-TO-STRING PRINC PRINC-TO-STRING PRINT
PRINT-NOT-READABLE PRINT-NOT-READABLE-OBJECT PRINT-OBJECT
PRINT-UNREADABLE-OBJECT PROBE-FILE PROCLAIM PROG PROG* PROG1 PROG2 PROGN
PROGRAM-ERROR PROGV PROVIDE PSETF PSETQ PUSH PUSHNEW QUOTE RANDOM RANDOM-STATE
RANDOM-STATE-P RASSOC RASSOC-IF RASSOC-IF-NOT RATIO RATIONAL RATIONALIZE
RATIONALP READ READ-BYTE READ-CHAR READ-CHAR-NO-HANG READ-DELIMITED-LIST
READ-FROM-STRING READ-LINE READ-PRESERVING-WHITESPACE READ-SEQUENCE
READER-ERROR READTABLE READTABLE-CASE READTABLEP REAL REALP REALPART REDUCE
REINITIALIZE-INSTANCE REM REMF REMHASH REMOVE REMOVE-DUPLICATES REMOVE-IF
REMOVE-IF-NOT REMOVE-METHOD REMPROP RENAME-FILE RENAME-PACKAGE REPLACE REQUIRE
REST RESTART RESTART-BIND RESTART-CASE RESTART-NAME RETURN RETURN-FROM
REVAPPEND REVERSE ROOM ROTATEF ROUND ROW-MAJOR-AREF RPLACA RPLACD SAFETY
SATISFIES SBIT SCALE-FLOAT SCHAR SEARCH SECOND SEQUENCE SERIOUS-CONDITION SET
SET-DIFFERENCE SET-DISPATCH-MACRO-CHARACTER SET-EXCLUSIVE-OR
SET-MACRO-CHARACTER SET-PPRINT-DISPATCH SET-SYNTAX-FROM-CHAR SETF SETQ SEVENTH
SHADOW SHADOWING-IMPORT SHARED-INITIALIZE SHIFTF SHORT-FLOAT
SHORT-FLOAT-EPSILON SHORT-FLOAT-NEGATIVE-EPSILON SHORT-SITE-NAME SIGNAL
SIGNED-BYTE SIGNUM SIMPLE-ARRAY SIMPLE-BASE-STRING SIMPLE-BIT-VECTOR
SIMPLE-BIT-VECTOR-P SIMPLE-CONDITION SIMPLE-CONDITION-FORMAT-ARGUMENTS
SIMPLE-CONDITION-FORMAT-CONTROL SIMPLE-ERROR SIMPLE-STRING SIMPLE-STRING-P
SIMPLE-TYPE-ERROR SIMPLE-VECTOR SIMPLE-VECTOR-P SIMPLE-WARNING SIN SINGLE-FLOAT
SINGLE-FLOAT-EPSILON SINGLE-FLOAT-NEGATIVE-EPSILON SINH SIXTH SLEEP SLOT-BOUNDP
SLOT-EXISTS-P SLOT-MAKUNBOUND SLOT-MISSING SLOT-UNBOUND SLOT-VALUE
SOFTWARE-TYPE SOFTWARE-VERSION SOME SORT SPACE SPECIAL SPECIAL-OPERATOR-P SPEED
SQRT STABLE-SORT STANDARD STANDARD-CHAR STANDARD-CHAR-P STANDARD-CLASS
STANDARD-GENERIC-FUNCTION STANDARD-METHOD STANDARD-OBJECT STEP
STORAGE-CONDITION STORE-VALUE STREAM STREAM-ELEMENT-TYPE STREAM-ERROR
STREAM-ERROR-STREAM STREAM-EXTERNAL-FORMAT STREAMP STRING STRING-CAPITALIZE
STRING-DOWNCASE STRING-EQUAL STRING-GREATERP STRING-LEFT-TRIM STRING-LESSP
STRING-NOT-EQUAL STRING-NOT-GREATERP STRING-NOT-LESSP STRING-RIGHT-TRIM
STRING-STREAM STRING-TRIM STRING-UPCASE STRING/= STRING< STRING<= STRING=
STRING> STRING>= STRINGP STRUCTURE STRUCTURE-CLASS STRUCTURE-OBJECT
STYLE-WARNING SUBLIS SUBSEQ SUBSETP SUBST SUBST-IF SUBST-IF-NOT SUBSTITUTE
SUBSTITUTE-IF SUBSTITUTE-IF-NOT SUBTYPEP SVREF SXHASH SYMBOL SYMBOL-FUNCTION
SYMBOL-MACROLET SYMBOL-NAME SYMBOL-PACKAGE SYMBOL-PLIST SYMBOL-VALUE SYMBOLP
SYNONYM-STREAM SYNONYM-STREAM-SYMBOL T TAGBODY TAILP TAN TANH TENTH TERPRI THE
THIRD THROW TIME TRACE TRANSLATE-LOGICAL-PATHNAME TRANSLATE-PATHNAME TREE-EQUAL
TRUENAME TRUNCATE TWO-WAY-STREAM TWO-WAY-STREAM-INPUT-STREAM
TWO-WAY-STREAM-OUTPUT-STREAM TYPE TYPE-ERROR TYPE-ERROR-DATUM
TYPE-ERROR-EXPECTED-TYPE TYPE-OF TYPECASE TYPEP UNBOUND-SLOT
UNBOUND-SLOT-INSTANCE UNBOUND-VARIABLE UNDEFINED-FUNCTION UNEXPORT UNINTERN
UNION UNLESS UNREAD-CHAR UNSIGNED-BYTE UNTRACE UNUSE-PACKAGE UNWIND-PROTECT
UPDATE-INSTANCE-FOR-DIFFERENT-CLASS UPDATE-INSTANCE-FOR-REDEFINED-CLASS
UPGRADED-ARRAY-ELEMENT-TYPE UPGRADED-COMPLEX-PART-TYPE UPPER-CASE-P USE-PACKAGE
USE-VALUE USER-HOMEDIR-PATHNAME VALUES VALUES-LIST VARIABLE VECTOR VECTOR-POP
VECTOR-PUSH VECTOR-PUSH-EXTEND VECTORP WARN WARNING WHEN WILD-PATHNAME-P
WITH-ACCESSORS WITH-COMPILATION-UNIT WITH-CONDITION-RESTARTS
WITH-HASH-TABLE-ITERATOR WITH-INPUT-FROM-STRING WITH-OPEN-FILE WITH-OPEN-STREAM
WITH-OUTPUT-TO-STRING WITH-PACKAGE-ITERATOR WITH-SIMPLE-RESTART WITH-SLOTS
WITH-STANDARD-IO-SYNTAX WRITE WRITE-BYTE WRITE-CHAR WRITE-LINE WRITE-SEQUENCE
WRITE-STRING WRITE-TO-STRING Y-OR-N-P YES-OR-NO-P ZEROP))
      (MULTIPLE-VALUE-BIND (symbol found)
	  (FIND-SYMBOL (SYMBOL-NAME sym) *cl-package*)
	(when found
	  (UNINTERN symbol *cl-package*)))
      (MULTIPLE-VALUE-BIND (symbol found)
	  (FIND-SYMBOL (SYMBOL-NAME sym) *emacs-cl-package*)
	(when found
	  (IMPORT (list symbol) *cl-package*)
	  (EXPORT (list symbol) *cl-package*))))))


;;; Local variables:
;;; fill-column: 79
;;; End:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./populate.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-eval.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements EVAL and environment objects.

;;; Possible future implementation:
;;; (defun EVAL (form)
;;;   (funcall (cl:values (COMPILE nil `(LAMBDA () ,form)))))

(IN-PACKAGE "EMACS-CL")

(defvar *special-operator-evaluators* (make-hash-table))

(defmacro* define-special-operator (name (&rest args) env &body body)
  `(progn
     (unless (fboundp ',name)
       (fset ',name nil))
     (setf (gethash ',name *special-operator-evaluators*)
           (function* (lambda (,env ,@args) ,@body)))))

(defun eval-body (forms env)
  (let ((lastval nil))
    (cl:values nil)
    (dolist (form forms lastval)
      (setq lastval (eval-with-env form env)))))

;;; Definitions for all special operators follows.

(define-special-operator BLOCK (tag &rest forms) env
  (let* ((catch-tag (gensym))
	 (new-env (augment-environment env :block (cons tag catch-tag))))
    (catch catch-tag
      (eval-body forms new-env))))

(define-special-operator CATCH (tag &rest forms) env
  (catch (eval-with-env tag env)
    (eval-body forms env)))

(define-special-operator EVAL-WHEN (situations &body body) env
  (when (or (memq (kw EXECUTE) situations) (memq 'EVAL situations))
    (eval-body body env)))

(define-special-operator FLET (fns &rest forms) env
  (let ((new-env (augment-environment env :function (mapcar #'first fns))))
    (dolist (fn fns)
      (setf (lexical-function (first fn) new-env)
	    (MULTIPLE-VALUE-BIND (body decls doc) (parse-body (cddr fn) t)
	      (enclose `(LAMBDA ,(second fn)
			  ,@(when decls `((DECLARE ,@decls)))
			  (BLOCK ,(function-block-name (first fn))
			    ,@body))
		       env (first fn) doc))))
    (MULTIPLE-VALUE-BIND (body declarations) (parse-body forms)
      (eval-body body new-env))))

(defun lexical-or-global-function (name env)
  (multiple-value-bind (type localp decl) (function-information name env)
    (if (eq type :function)
	(if localp (lexical-function name env) (FDEFINITION name))
	(ERROR 'UNDEFINED-FUNCTION (kw NAME) name))))

(define-special-operator FUNCTION (form) env
  (cl:values
    (cond
      ((SYMBOLP form)		(lexical-or-global-function form env))
      ((ATOM form)		(not-function-name-error form))
      ((case (first form)
	 (LAMBDA		(enclose form env form))
	 (SETF			(lexical-or-global-function form env))
	 (t			(not-function-name-error form)))))))

(define-special-operator GO (tag) env
  (let ((info (tagbody-information tag env)))
    (if info
	(throw info tag)
	(ERROR 'PROGRAM-ERROR))))

(define-special-operator IF (condition then &optional else) env
  (if (eval-with-env condition env)
      (eval-with-env then env)
      (eval-with-env else env)))

(define-special-operator LABELS (fns &rest forms) env
  (let ((new-env (augment-environment env :function (mapcar #'first fns))))
    (dolist (fn fns)
      (setf (lexical-function (first fn) new-env)
	    (MULTIPLE-VALUE-BIND (body decls doc) (parse-body (cddr fn) t)
	      (enclose `(LAMBDA ,(second fn)
			  ,@(when decls `((DECLARE ,@decls)))
			  (BLOCK ,(function-block-name (first fn))
			    ,@body))
		       new-env (first fn) doc))))
    (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
      (eval-body body new-env))))

(defun lexical-variable-p (var env)
  (eq (nth-value 0 (variable-information var env)) :lexical))

(defun special-variable-p (var env)
  (not (lexical-variable-p var env)))

;;; TODO: let* bindings shouldn't be evaluated in an environment where
;;; succeeding bindings exist.
(defun eval-let (bindings forms env old-env)
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let* ((vars (mapcar #'first-or-identity bindings))
	   (new-env (augment-environment env :variable vars :declare decls))
	   (oldvals nil))
      (dolist (binding bindings)
	(multiple-value-bind (var val) (if (symbolp binding)
					   (values binding nil)
					   (values (first binding)
						   (eval-with-env
						    (second binding)
						    (or old-env new-env))))
	  (if (lexical-variable-p var new-env)
	      (setf (lexical-value var new-env) val)
	      (progn
		(push (if (boundp var) (symbol-value var) unbound) oldvals)
		(setf (symbol-value var) val)))))
      (unwind-protect
	   (eval-body body new-env)
	(setq oldvals (nreverse oldvals))
	(dolist (binding bindings)
	  (let ((var (if (symbolp binding) binding (first binding))))
	    (unless (lexical-variable-p var new-env)
	      (let ((val (pop oldvals)))
		(if (eq val unbound)
		    (makunbound var)
		    (setf (symbol-value var) val))))))))))

(define-special-operator LET (bindings &rest forms) env
  (eval-let bindings forms env env))

(define-special-operator LET* (bindings &rest forms) env
  (eval-let bindings forms env nil))

(define-special-operator LOAD-TIME-VALUE (form &optional read-only-p) env
  (cl:values (eval-with-env form nil)))

(define-special-operator LOCALLY (&rest forms) env
  (MULTIPLE-VALUE-BIND (body declarations) (parse-body forms)
    (eval-body body env)))

(defun macro-body (lambda-list decls body fvar form)
  (if (eq (first lambda-list) '&WHOLE)
      (unless (eql (length lambda-list) 2)
	(push (gensym) (cddr lambda-list)))
      (setq form `(CDR ,fvar)))
  (unless (null lambda-list)
    (setq body `((DESTRUCTURING-BIND ,lambda-list ,form
		   ,@(when decls `((DECLARE ,@decls)))
		   ,@body))))
  (cl:values body form))

(defun compiler-macro-body (lambda-list decls body fvar form)
  (let ((wvar nil))
    (when (eq (first lambda-list) '&WHOLE)
      (setq wvar (second lambda-list))
      (setq lambda-list (cddr lambda-list)))
    (unless (null lambda-list)
      (setq body `((SETQ ,fvar
		         (IF (EQ (CAR ,fvar) (QUOTE FUNCALL))
			     (CDDR ,fvar)
			     (CDR ,fvar)))
		   (DESTRUCTURING-BIND ,lambda-list ,fvar
		     ,@(when decls `((DECLARE ,@decls)))
		     ,@body))))
    (when wvar
      (setq body `((LET ((,wvar ,fvar)) ,@body))))
    (cl:values body form)))

(defun* make-macro-function (name lambda-list forms &optional env
			     &key type)
  (with-gensyms (fvar evar)
    (let ((e (memq '&ENVIRONMENT lambda-list))
	  (form fvar))
      (when e
	(when (null (cdr e))
	  (ERROR 'PROGRAM-ERROR))
	(setq evar (second e))
	(let ((x lambda-list))
	  (while x
	    (when (eq (cadr x) '&ENVIRONMENT)
	      (setf (cdr x) (cdddr x)))
	    (setq x (cdr x)))))
      (MULTIPLE-VALUE-BIND (body decls doc) (parse-body forms t)
	(MULTIPLE-VALUE-SETQ (body form)
	  (if (eq type 'COMPILER-MACRO)
	      (compiler-macro-body lambda-list decls body fvar form)
	      (macro-body lambda-list decls body fvar form)))
	(setq body `(,@(when doc (list doc))
		     (BLOCK ,name ,@body)))
	(let ((fn `(LAMBDA (,fvar ,evar) ,@body)))
	  (when env
	    (setq fn (enclose fn env name)))
	  fn)))))

(defun env-with-macros (env macros decls)
  (let ((new-env (augment-environment env :macro (mapcar #'first macros)
				      :declare decls)))
    (dolist (macro macros)
      (destructuring-bind (name lambda-list &rest body) macro
	(setf (MACRO-FUNCTION name new-env)
	      (make-macro-function name lambda-list body env))))
    new-env))

(define-special-operator MACROLET (macros &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let ((new-env (env-with-macros env macros decls)))
      (eval-body body new-env))))

(define-special-operator MULTIPLE-VALUE-CALL (fn &rest forms) env
  (let ((values nil))
    (dolist (form forms)
      (setq values (append values
			   (MULTIPLE-VALUE-LIST (eval-with-env form env)))))
    (APPLY (eval-with-env fn env) values)))

(define-special-operator MULTIPLE-VALUE-PROG1 (form &rest forms) env
  (let ((values (MULTIPLE-VALUE-LIST (eval-with-env form env))))
    (eval-body forms env)
    (VALUES-LIST values)))

(define-special-operator PROGN (&rest forms) env
  (eval-body forms env))

(defmacro defun-do-progv ()
  (with-gensyms (sym syms vals fn temp state)
  `(defun do-progv (,syms ,vals ,fn)
    (let ((,state nil))
      (unwind-protect
	   (progn
	     (dolist (,sym ,syms)
	       (let ((,temp (boundp ,sym)))
		 (when ,temp
		   (push (symbol-value ,sym) ,state))
		 (push ,temp ,state))
	       (if (null ,vals)
		   (makunbound ,sym)
		   (set ,sym (pop ,vals))))
	     (FUNCALL ,fn))
	(dolist (,sym ,syms)
	  (if (pop ,state)
	      (set ,sym (pop ,state))
	      (makunbound ,sym))))))))

(defun-do-progv)

(define-special-operator PROGV (symbols values &rest forms) env
  (do-progv (eval-with-env symbols env)
            (eval-with-env values env)
	    (enclose `(LAMBDA () ,@forms) env)))

(define-special-operator QUOTE (form) env
  (cl:values form))

(define-special-operator RETURN-FROM (tag &optional form) env
  (let ((info (block-information tag env)))
    (if info
	(throw (cdr info) (eval-with-env form env))
	(ERROR 'PROGRAM-ERROR))))

(define-special-operator SETQ (&rest forms) env
  (when (oddp (length forms))
    (ERROR "Odd number of forms in SETQ"))
  (do ((lastval nil)
       (forms forms (cddr forms)))
      ((null forms)
       (cl:values lastval))
    (let* ((var (first forms))
	   (vals (MULTIPLE-VALUE-LIST (eval-with-env (second forms) env)))
	   (val (first vals)))
      (unless (symbolp var)
	(ERROR "Setting non-symbol ~S" var))
      (setq lastval
	    (ecase (nth-value 0 (variable-information var env))
	      (:lexical		(setf (lexical-value var env) val))
	      (:special		(set var val))
	      ((nil)		(WARN "Setting undefined variable ~S" var)
				(set var val))
	      (:symbol-macro	(eval-with-env
				 `(SETF ,(MACROEXPAND var env)
				        (VALUES-LIST (QUOTE ,vals)))
				 env))
	      (:constant	(ERROR "Setting constant ~S" var)))))))

(define-special-operator SYMBOL-MACROLET (macros &rest forms) env
  (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
    (let ((new-env (augment-environment env :symbol-macro
					(mapcar #'first macros))))
      (dolist (macro macros)
	(setf (lexical-value (first macro) new-env)
	      (enclose `(LAMBDA (form env) (QUOTE ,(second macro)))
		       env (first macro))))
      (eval-body body new-env))))

(defun go-tag-p (object)
  (or (INTEGERP object) (symbolp object)))

(define-special-operator TAGBODY (&rest forms) env
  (let* ((catch-tag (gensym))
	 (new-env (augment-environment
		   env :tagbody
		   (cons catch-tag (remove-if-not #'go-tag-p forms))))
	 (exe forms)
	 (no-tag 0.0))
    (while exe
      (let ((form (first exe)))
	(cond
	  ((go-tag-p form)
	   (setq exe (rest exe)))
	  ((consp form)
	   (let ((tag (catch catch-tag
			(eval-with-env form new-env)
			no-tag)))
	     (if (eq tag no-tag)
		 (setq exe (rest exe))
		 (setq exe (member tag forms)))))
	  (t
	   (ERROR "Syntax error: ~S in tagbody is neither a go tag ~
		   nor a compound expression" form))))))
  (cl:values nil))

(define-special-operator THE (type form) env
  (eval-with-env form env))

(define-special-operator THROW (tag form) env
  (throw (eval-with-env tag env) (eval-with-env form env)))

(define-special-operator UNWIND-PROTECT (protected &rest cleanups) env
  (let (ntmp mtmp)
    (prog1 (unwind-protect (prog1 (eval-with-env protected env)
			     (setq ntmp nvals mtmp mvals))
	     (eval-body cleanups env))
      (setq nvals ntmp mvals mtmp))))



(defun variable-information (var &optional env)
  (unless env
    (setq env *global-environment*))
  (values
    (let ((info (assoc var (aref env 1))))
      (cond
	(info			(cdr info))
	((boundp var)		(if (CONSTANTP var env) :constant :special))
	((memq var *constants*)	:constant)
	((memq var *specials*)	:special)))
    (member var (aref env 2))
    nil))

(defun lexical-value (var env)
  (cdr (assq var (aref env 3))))

(defsetf lexical-value (var env) (val)
  `(setf (cdr (assq ,var (aref ,env 3))) ,val))

(defun function-information (fn &optional env)
  (unless env
    (setq env *global-environment*))
  (values
   (let ((info (assoc fn (aref env 4))))
     (if info
	 (cdr info)
	 (cond
	   ((gethash fn *macro-functions*)	:macro)
	   ((FBOUNDP fn)			:function)
	   ((and (symbolp fn)
		 (SPECIAL-OPERATOR-P fn))	:special-operator))))
   (member fn (aref env 5))
   nil))

(defun lexical-function (name env)
  (cdr-safe (assoc name (aref env 6))))

(defsetf lexical-function (name env) (fn)
  `(let ((cons (assoc ,name (aref ,env 6))))
     (if cons
	 (setf (cdr cons) ,fn)
	 (progn (aset ,env 6 (acons ,name ,fn (aref ,env 6)))
		,fn))))

(defun block-information (tag env)
  (when env
    (assoc tag (aref env 7))))

(defun tagbody-information (tag env)
  (when env
    (let ((tagbody (find-if (lambda (x) (member tag (rest x))) (aref env 8))))
      (first tagbody))))

(defun* augment-environment (env &key variable symbol-macro function
				      macro declare block tagbody)
  (unless env
    (setq env *global-environment*))
  (let ((lexicals (remove-if (lambda (var) (memq var *specials*)) variable))
	(var-info (aref env 1))
	(var-local (aref env 2))
	(var-storage (aref env 3))
	(fn-info (aref env 4))
	(fn-local (aref env 5))
	(fn-storage (aref env 6))
	(block-info (aref env 7))
	(tagbody-info (aref env 8)))
    (setq var-local (append lexicals symbol-macro var-local))
    (dolist (decl declare)
      (when (eq (first decl) 'SPECIAL)
	(dolist (var (rest decl))
	  (setq var-info (acons var :special var-info))
	  (setq lexicals (delq var lexicals)))))
    (setq var-info (reduce (lambda (env var) (acons var :lexical env))
			   lexicals
			   :initial-value var-info))
    (setq var-info (reduce (lambda (env var) (acons var :symbol-macro env))
			   symbol-macro
			   :initial-value var-info))
    (dolist (var lexicals)
      (push (cons var nil) var-storage))
    (dolist (var symbol-macro)
      (push (cons var nil) var-storage))
    (setq fn-info (reduce (lambda (env fn) (acons fn :function env))
			  function
			  :initial-value fn-info))
    (setq fn-info (reduce (lambda (env mac) (acons mac :macro env))
			  macro
			  :initial-value fn-info))
    (setq fn-local (append function macro fn-local))
    (dolist (fn function)
      (push (cons fn nil) fn-storage))
    (dolist (fn macro)
      (push (cons fn nil) fn-storage))
    (setq block-info (cons block block-info))
    (setq tagbody-info (cons tagbody tagbody-info))
  (vector 'environment var-info var-local var-storage
	               fn-info fn-local fn-storage
	               block-info tagbody-info)))

(cl:defun enclose (lambda-exp &OPTIONAL env (name ""))
  (unless env
    (setq env *global-environment*))
  (MULTIPLE-VALUE-BIND (body decls doc) (parse-body (cddr lambda-exp) t)
    (vector 'INTERPRETED-FUNCTION
	    `(LAMBDA ,(second lambda-exp) (DECLARE ,@decls) ,@body)
	    env name doc)))

(defun INTERPRETED-FUNCTION-P (object)
  (vector-and-typep object 'INTERPRETED-FUNCTION))

(defun function-name (fn)
  (cond
    ((INTERPRETED-FUNCTION-P fn)
     (interp-fn-name fn))
    ((byte-code-function-p fn)
     "")
    ((subrp fn)
     (let ((string (prin1-to-string fn)))
       (substring string 7 (1- (length string)))))
    ((listp fn)
     "")
    (t
     (type-error fn 'FUNCTION))))

(defsetf function-name set-function-name)

(DEFSETF function-name set-function-name)

(defun set-function-name (fn name)
  (cond
    ((INTERPRETED-FUNCTION-P fn)
     (setf (interp-fn-name fn) name))
    ((byte-code-function-p fn)
     name)
    ((subrp fn)
     name)
    ((listp fn)
     name)
    (t
     (type-error fn 'FUNCTION))))



(defun* parse-body (forms &optional doc-allowed)
  (do ((decl nil)
       (doc nil)
       (body forms (rest body)))
      ((null forms)
       (cl:values nil decl doc))
    (flet ((done () (return-from parse-body (cl:values body decl doc))))
      (let ((form (first body)))
	(cond
	  ((STRINGP form)
	   (if (and doc-allowed (not doc) (rest body))
	       (setq doc form)
	       (done)))
	  ((and (consp form) (eq (first form) 'DECLARE))
	   (dolist (d (rest form))
	     (push d decl)))
	  (t
	   (done)))))))

(defun set-local-macro (name fn env)
  (augment-environment env :macro (list name))
  (setf (lexical-function name env) fn))

(defun eval-lambda-expr (lambda-expr args old-env)
  (MULTIPLE-VALUE-BIND (body decls doc) (parse-body (cddr lambda-expr) t)
    (let* ((lambda-list (second lambda-expr))
	   (new-env
	    (augment-environment old-env
	      :variable (lambda-list-variables lambda-list)
	      :declare decls))
	  (other-keys-p nil)
	  (allow-other-keys-p (memq '&ALLOW-OTHER-KEYS lambda-list)))
      (do-lambda-list (((key var) default supplied) kind lambda-list)
	;; TODO: special variables.
	(case kind
	  (:required
	   (unless args
	     (ERROR "No value for required parameter."))
	   (setf (lexical-value var new-env) (pop args)))
	  (&OPTIONAL
	   (when supplied
	     (setf (lexical-value supplied new-env) args))
	   (setf (lexical-value var new-env)
		 (if args
		     (pop args)
		     (eval-with-env default env))))
	  (&REST
	   (setf (lexical-value var new-env) (copy-list args))
	   (unless (memq '&KEY lambda-list)
	     (setq args nil)))
	  (&KEY
	   (let ((arg (memq key args)))
	     (cond
	       (arg
		(setf (lexical-value var new-env) (second arg))
		(when supplied (setf (lexical-value supplied new-env) 'T))
		(remf args (first arg)))
	       (t
		(setf (lexical-value var new-env) (eval-with-env default env))
		(when supplied (setf (lexical-value supplied new-env) nil))))))
	  (&AUX
	   (setf (lexical-value var new-env)
		 (eval-with-env default new-env)))))
      (when (and args (not (or allow-other-keys-p
			       (memq (kw ALLOW-OTHER-KEYS) args))))
	(ERROR 'PROGRAM-ERROR))
      (eval-body body new-env))))

(defun eval-forms (forms env)
  (mapcar (lambda (form) (eval-with-env form env))
	  forms))

(defun eval-with-env (form env)
  (unless env
    (setq env *global-environment*))
  (setq form (cl:values (MACROEXPAND form env)))
  (cond
    ((SYMBOLP form)
     (ecase (nth-value 0 (variable-information form env))
       ((nil)		(ERROR 'UNBOUND-VARIABLE (kw NAME) form))
       (:special	(SYMBOL-VALUE form))
       (:lexical	(lexical-value form env))
       (:symbol-macro	(error "shouldn't happen"))
       (:constant	(SYMBOL-VALUE form))))
    ((ATOM form)
     form)
    ((consp (car form))
     (if (eq (caar form) 'LAMBDA)
	 (eval-lambda-expr (first form) (eval-forms (rest form) env) env)
	 (ERROR 'PROGRAM-ERROR)))
    (t
     (let* ((name (first form))
	    (fn (gethash name *special-operator-evaluators*)))
       (cond
	 (fn
	  (apply fn env (rest form)))
	 ((setq fn (lexical-or-global-function name env))
	  (let ((args (eval-forms (rest form) env)))
	    (setq nvals 1 mvals nil)
	    (if (listp fn)
		;; Special hack for interpreted Emacs Lisp function.
		(apply fn args)
		(APPLY fn args)))))))))

(defun cl-debugger (&optional error args)
  (unless (eval-when-compile (featurep 'xemacs))
    (incf num-nonmacro-input-events))
  ;; error:
  ;;   lambda - function entry, debug-on-next-call
  ;;   debug - function entry, breakpoint
  ;;   t - evaluation of list form
  ;;   exit - exit of marked stack frame
  ;;   error - error or quit signalled
  ;;   nil - enter explicitly
  (unless (eq error 'error)
    (debug))
  (case (car args)
    (quit	(debug))
    (range-error
		nil)
    (void-variable
		(ERROR 'UNBOUND-VARIABLE))
    ((void-function invalid-function)
		(ERROR 'UNDEFINED-FUNCTION))
    ((wrong-type-argument)
		(ERROR 'TYPE-ERROR))
    (no-catch
		(ERROR 'CONTROL-ERROR))
    ((wrong-number-of-arguments no-catch wrong-type-argument)
		(ERROR 'PROGRAM-ERROR))
    (setting-constant
		(ERROR 'ERROR))
    (error	(ERROR "~A." (cadr args)))
    (t		(ERROR "Error: ~A ~S" error args))))

(defun EVAL (form)
  (let ((debug-on-error t)
	(debug-on-quit t)
	(debug-on-signal t)
	(debug-ignored-errors nil)
	(debugger 'cl-debugger))
    (eval-with-env form nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-eval.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-streams.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 21, Streams.

(IN-PACKAGE "EMACS-CL")

(DEFSTRUCT (STREAM (:predicate STREAMP) (:copier nil))
  (openp T)
  (ELEMENT-TYPE 'CHARACTER)
  direction
  content
  (position 0)
  end
  fresh-line-p
  (unread-buffer nil)
  read-fn
  write-fn)

(defmacro defstream (name &rest slots)
  `(DEFSTRUCT (,name (:include STREAM)
	             (:predicate nil)
	             (:copier nil)
	             (:constructor
		      ,(intern (concat "mk-" (symbol-name name)))))
     ,@slots))

(defstream BROADCAST-STREAM STREAMS)
(defstream CONCATENATED-STREAM STREAMS)
(defstream ECHO-STREAM INPUT-STREAM OUTPUT-STREAM)
(defstream FILE-STREAM filename)
(defstream STRING-STREAM string)
(defstream SYNONYM-STREAM SYMBOL)
(defstream TWO-WAY-STREAM INPUT-STREAM OUTPUT-STREAM)

(defun stream-error (stream)
  (ERROR 'STREAM-ERROR (kw STREAM) stream))

(DEFVAR *STANDARD-INPUT* nil)

(DEFVAR *STANDARD-OUTPUT* nil)

(DEFVAR *TERMINAL-IO* nil)

(defun input-stream (designator)
  (case designator
    ((nil)	*STANDARD-INPUT*)
    ((t)	*TERMINAL-IO*)
    (t		designator)))

(defun output-stream (designator)
  (case designator
    ((nil)	*STANDARD-OUTPUT*)
    ((t)	*TERMINAL-IO*)
    (t		designator)))

(defun INPUT-STREAM-P (stream)
  (let ((direction (STREAM-direction stream)))
    (or (eq direction (kw INPUT))
	(eq direction (kw IO)))))

(defun OUTPUT-STREAM-P (stream)
  (let ((direction (STREAM-direction stream)))
    (or (eq direction (kw OUTPUT))
	(eq direction (kw IO)))))

(defun INTERACTIVE-STREAM-P (stream)
  (ecase (TYPE-OF stream)
    (STREAM			T)
    (BROADCAST-STREAM		nil)
    (CONCATENATED-STREAM	nil)
    (ECHO-STREAM		(INTERACTIVE-STREAM-P
				 (ECHO-STREAM-INPUT-STREAM stream)))
    (FILE-STREAM		nil)
    (STRING-STREAM		nil)
    (SYNONYM-STREAM		(INTERACTIVE-STREAM-P
				 (symbol-value
				  (SYNONYM-STREAM-SYMBOL stream))))
    (TWO-WAY-STREAM		(and
				 (INTERACTIVE-STREAM-P
				  (TWO-WAY-STREAM-INPUT-STREAM stream))
				 (INTERACTIVE-STREAM-P
				  (TWO-WAY-STREAM-OUTPUT-STREAM stream))))))

(defun OPEN-STREAM-P (stream)
  (STREAM-openp stream))

;;; STREAM-ELEMENT-TYPE defined by defstruct.

;;; STREAMP defined by defstruct.

(defun READ-BYTE (&rest args)
  (CHAR-CODE (apply #'READ-CHAR args)))

(defun WRITE-BYTE (byte &rest args)
  (apply #'WRITE-CHAR (CHAR-CODE byte) args))

(defun* PEEK-CHAR (&optional peek-type stream (eof-error-p T)
			     eof-value recursive-p)
  (loop
   (let ((char (READ-CHAR stream eof-error-p eof-value recursive-p)))
     (cond
       ((EQL char eof-value)
	(return-from PEEK-CHAR eof-value))
       ((or (eq peek-type nil)
	    (and (eq peek-type T) (not (whitespacep char)))
	    (and (not (eq peek-type T)) (CHAR= char peek-type)))
	(UNREAD-CHAR char stream)
	(return-from PEEK-CHAR char))))))

(defun* READ-CHAR (&optional stream-designator (eof-error-p T)
			     eof-value recursive-p)
  (let ((stream (input-stream stream-designator)))
    (when (STREAM-unread-buffer stream)
      (return-from READ-CHAR (pop (STREAM-unread-buffer stream))))
    (let* ((fn (STREAM-read-fn stream))
	   (char (funcall (or fn (stream-error stream)) stream)))
      (if (eq char :eof)
	  (if eof-error-p
	      (ERROR 'END-OF-FILE (kw STREAM) stream)
	      eof-value)
	  char))))

(cl:defun read-char-exclusive-ignoring-arg (arg)
  (let ((char (read-char-exclusive)))
    (cl-char (if (eq char 13) 10 char))))

(cl:defun READ-CHAR-NO-HANG (&OPTIONAL stream-designator (eof-error-p T)
				       eof-value recursive-p)
  (let ((stream (input-stream stream-designator)))
    (if (eq (STREAM-read-fn stream)
	    (cl:function read-char-exclusive-ignoring-arg))
	(when (LISTEN stream)
	  (READ-CHAR stream stream eof-error-p eof-value recursive-p))
	(READ-CHAR stream stream eof-error-p eof-value recursive-p))))

(cl:defun TERPRI (&OPTIONAL stream-designator)
  (let ((stream (output-stream stream-designator)))
    (WRITE-CHAR (ch 10) stream))
  nil)

(cl:defun FRESH-LINE (&OPTIONAL stream-designator)
  (let ((stream (output-stream stream-designator)))
    (unless (if (bufferp (STREAM-content stream))
		(with-current-buffer (STREAM-content stream)
		  (eql (char-before (point)) (ch 10)))
		(STREAM-fresh-line-p stream))
      (TERPRI stream))))

(cl:defun UNREAD-CHAR (char &OPTIONAL stream-designator)
  (let ((stream (input-stream stream-designator)))
    (if (STREAM-position stream)
	(when (> (STREAM-position stream) 0)
	  (decf (STREAM-position stream)))
	(push char (STREAM-unread-buffer stream)))))

(cl:defun WRITE-CHAR (char &OPTIONAL stream-designator)
  (let* ((stream (output-stream stream-designator))
	 (fn (STREAM-write-fn stream)))
    (unless fn
      (stream-error stream))
    (funcall fn char stream)
    (setf (STREAM-fresh-line-p stream) (ch= char 10))
    char))

(cl:defun READ-LINE (&OPTIONAL stream-designator (eof-error-p T)
			       eof-value recursive-p)
  (let ((stream (input-stream stream-designator))
	(line ""))
    (catch 'READ-LINE
      (loop
       (let ((char (READ-CHAR stream eof-error-p eof-value recursive-p)))
	 (cond
	   ((EQL char eof-value)
	    (throw 'READ-LINE
	      (cl:values (if (= (length line) 0) eof-value line) t)))
	   ((ch= char 10)
	    (throw 'READ-LINE (cl:values line nil))))
	 (setq line (concat line (list (CHAR-CODE char)))))))))

(cl:defun WRITE-STRING (string &OPTIONAL stream-designator &KEY (START 0) END)
  (unless END
    (setq END (LENGTH string)))
  (do ((stream (output-stream stream-designator))
       (i START (1+ i)))
      ((>= i END) string)
    (WRITE-CHAR (CHAR string i) stream)))

(cl:defun WRITE-LINE (string &OPTIONAL stream-designator &KEY (START 0) END)
  (let ((stream (output-stream stream-designator)))
    (WRITE-STRING string stream (kw START) START (kw END) END)
    (TERPRI stream)
    string))

(cl:defun READ-SEQUENCE (seq stream &KEY (START 0) END)
  (unless END
    (setq END (LENGTH seq)))
  (catch 'READ-SEQUENCE
    (do ((i START (1+ i)))
	((eq i END)
	 i)
      (let ((char (READ-CHAR stream nil)))
	(if (null char)
	    (throw 'READ-SEQUENCE i)
	    (setf (ELT seq i) char))))))

(cl:defun WRITE-SEQUENCE (seq stream &KEY (START 0) END)
  (unless END
    (setq END (LENGTH seq)))
  (do ((i START (1+ i)))
      ((eq i END)
       seq)
    (WRITE-CHAR (ELT seq i) stream)))

(defun FILE-LENGTH (stream)
  (unless (TYPEP stream 'FILE-STREAM)
    (type-error stream 'FILE-STREAM))
  (let ((len (file-attributes (FILE-STREAM-filename stream))))
    (cond
      ((integerp len)	len)
      ((null len)	nil)
      ;; TODO: return integer
      ((floatp len)	len)
      (t		(error "?")))))

(defun FILE-POSITION (stream &optional position)
  (if position
      ;; TODO: implement setting position
      (progn
	(setf (STREAM-position stream))
	T)
      (STREAM-position stream)))

(defun FILE-STRING-LENGTH (stream object)
  (LENGTH (let ((s (MAKE-STRING-OUTPUT-STREAM)))
	    (unwind-protect
		 (PRINT object s)
	      (CLOSE s)))))

(cl:defun OPEN (filespec &KEY (DIRECTION (kw INPUT)) (ELEMENT-TYPE 'CHARACTER)
		              IF-EXISTS IF-DOES-NOT-EXIST
			      (EXTERNAL-FORMAT (kw DEFAULT)))
  (setq filespec (NAMESTRING (MERGE-PATHNAMES filespec)))
  (mk-FILE-STREAM
   (kw direction) DIRECTION
   (kw filename) filespec
   (kw content) (let ((buffer (create-file-buffer filespec)))
		  (when (eq DIRECTION (kw INPUT))
		    (save-current-buffer
		      (set-buffer buffer)
		      (insert-file-contents-literally filespec)))
		  buffer)
   (kw position) 0
   (kw read-fn)
     (lambda (stream)
       (save-current-buffer
	 (set-buffer (STREAM-content stream))
	 (if (= (STREAM-position stream) (buffer-size))
	     :eof
	     (cl-char (char-after (incf (STREAM-position stream)))))))
   (kw write-fn)
     (lambda (char stream)
       (with-current-buffer (STREAM-content stream)
	 (goto-char (incf (STREAM-position stream)))
	 (insert (el-char char))))))

(defun STREAM-EXTERNAL-FORMAT (stream)
  (kw DEFAULT))

(defmacro* WITH-OPEN-FILE ((stream filespec &rest options) &body body)
  `(WITH-OPEN-STREAM (,stream (OPEN ,filespec ,@options))
     ,@body))

(cl:defmacro WITH-OPEN-FILE ((stream filespec &rest options) &body body)
  `(WITH-OPEN-STREAM (,stream (OPEN ,filespec ,@options))
     ,@body))

(cl:defun CLOSE (stream &KEY ABORT)
  (cond
    ((STREAM-openp stream)
     (when (and (TYPEP stream 'FILE-STREAM)
		(OUTPUT-STREAM-P stream))
       (save-current-buffer
	 (set-buffer (STREAM-content stream))
	 (write-region 1 (1+ (buffer-size)) (FILE-STREAM-filename stream))))
     (when (bufferp (STREAM-content stream))
       (kill-buffer (STREAM-content stream)))
     (setf (STREAM-openp stream) nil)
     T)
    (t
     nil)))

(cl:defmacro WITH-OPEN-STREAM ((var stream) &body body)
  `(LET ((,var ,stream))
     (UNWIND-PROTECT
	  (PROGN ,@body)
       (CLOSE ,var))))

(defmacro* WITH-OPEN-STREAM ((var stream) &body body)
  `(let ((,var ,stream))
     (unwind-protect
	  (progn ,@body)
       (CLOSE ,var))))

(cl:defun LISTEN (&OPTIONAL stream-designator)
  (let ((stream (input-stream stream-designator)))
     (if (eq (STREAM-read-fn stream)
	     (cl:function read-char-exclusive-ignoring-arg))
	 (not (sit-for 0))
	 (not (eq (PEEK-CHAR nil stream :eof) :eof)))))

(defun CLEAR-INPUT (&optional stream-designator)
  (let ((stream (input-stream stream-designator)))
    (when (eq (STREAM-read-fn stream)
	      (cl:function read-char-exclusive-ignoring-arg))
      (while (LISTEN stream)
	(READ-CHAR stream)))))

(defun FINISH-OUTPUT (&optional stream-designator)
  (let ((stream (output-stream stream-designator)))
    nil))

(defun FORCE-OUTPUT (&optional stream-designator)
  (let ((stream (output-stream stream-designator)))
    nil))

(defun CLEAR-OUTPUT (&optional stream-designator)
  (let ((stream (output-stream stream-designator)))
    nil))

(defun Y-OR-N-P (&optional format &rest args)
  (when format
    (FRESH-LINE *QUERY-IO*)
    (apply #'FORMAT *QUERY-IO* format args))
  (catch 'Y-OR-N-P
    (loop
     (let ((char (READ-CHAR *QUERY-IO*)))
       (cond
	 ((CHAR-EQUAL char (ch 89))
	  (throw 'Y-OR-N-P T))
	 ((CHAR-EQUAL char (ch 78))
	  (throw 'Y-OR-N-P nil))
	 (t
	  (WRITE-LINE "Please answer 'y' or 'n'. ")))))))

(defun YES-OR-NO-P (&optional format &rest args)
  (when format
    (FRESH-LINE *QUERY-IO*)
    (apply #'FORMAT *QUERY-IO* format args))
  (catch 'YES-OR-NO-P
    (loop
     (let ((line (READ-LINE *QUERY-IO*)))
       (cond
	 ((STRING-EQUAL line "yes")
	  (throw 'YES-OR-NO-P T))
	 ((STRING-EQUAL line "no")
	  (throw 'YES-OR-NO-P nil))
	 (t
	  (WRITE-LINE "Please answer 'yes' or 'no'. ")))))))

(defun MAKE-SYNONYM-STREAM (symbol)
  (mk-SYNONYM-STREAM
   (kw SYMBOL) symbol
   (kw read-fn)
     (lambda (stream)
       (let ((s (symbol-value (SYNONYM-STREAM-SYMBOL stream))))
	 (funcall (STREAM-read-fn s) s)))
   (kw write-fn)
     (lambda (char stream)
       (let ((s (symbol-value (SYNONYM-STREAM-SYMBOL stream))))
	 (funcall (STREAM-read-fn s) char s)))))

;;; SYNONYM-STREAM-SYMBOL defined by defstruct.

;;; BROADCAST-STREAM-STREAMS defined by defstruct.

(defun MAKE-BROADCAST-STREAM (&rest streams)
  (mk-BROADCAST-STREAM
   (kw direction) (kw OUTPUT)
   (kw STREAMS) streams
   (kw write-fn) (lambda (char stream)
		   (dolist (s (BROADCAST-STREAM-STREAMS stream))
		     (WRITE-CHAR char s)))))

(defun MAKE-TWO-WAY-STREAM (input output)
  (mk-TWO-WAY-STREAM
   (kw direction) (kw IO)
   (kw INPUT-STREAM) input
   (kw OUTPUT-STREAM) output
   (kw read-fn)
     (lambda (stream)
       (READ-CHAR (TWO-WAY-STREAM-INPUT-STREAM stream)))
   (kw write-fn)
     (lambda (char stream)
       (WRITE-CHAR char (TWO-WAY-STREAM-OUTPUT-STREAM stream)))))

;;; TWO-WAY-STREAM-INPUT-STREAM and TWO-WAY-STREAM-OUTPUT-STREAM
;;; defined by defstruct.

;;; ECHO-STREAM-INPUT-STREAM and ECHO-STREAM-OUTPUT-STREAM defined
;;; by defstruct.

(defun MAKE-ECHO-STREAM (input output)
  (mk-ECHO-STREAM
   (kw direction) (kw INPUT)
   (kw INPUT-STREAM) input
   (kw OUTPUT-STREAM) output
   (kw read-fn)
     (lambda (stream)
       (let ((char (READ-CHAR (ECHO-STREAM-INPUT-STREAM stream))))
	 (WRITE-CHAR char (ECHO-STREAM-OUTPUT-STREAM stream))
	 char))))

;;; CONCATENATED-STREAM-STREAMS defined by defstruct.

(defun MAKE-CONCATENATED-STREAM (&rest streams)
  (mk-CONCATENATED-STREAM
   (kw direction) (kw INPUT)
   (kw STREAMS) streams
   (kw read-fn)
     (lambda (stream)
       (let ((streams (CONCATENATED-STREAM-STREAMS stream)))
	 (if (null streams)
	     :eof
	     (let ((char (READ-CHAR (first streams) nil)))
	       (if (null char)
		   (progn
		     (pop (CONCATENATED-STREAM-STREAMS stream))
		     (funcall (STREAM-read-fn stream) stream))
		   char)))))))

(defun GET-OUTPUT-STREAM-STRING (stream)
  (STRING-STREAM-string stream))

(cl:defun MAKE-STRING-INPUT-STREAM (string &OPTIONAL (start 0) end)
  (mk-STRING-STREAM
   (kw direction) (kw INPUT)
   (kw string) (if (> (length string) 0) string :eof)
   (kw position) start
   (kw end) (or end (LENGTH string))
   (kw read-fn)
     (lambda (stream)
       (cond
	 ((eq (STRING-STREAM-string stream) :eof)
	  :eof)
	 ((= (STREAM-position stream) (STREAM-end stream))
	  (setf (STRING-STREAM-string stream) :eof))
	 (t
	  (CHAR (STRING-STREAM-string stream)
		(1- (incf (STREAM-position stream)))))))))

(cl:defun MAKE-STRING-OUTPUT-STREAM (&KEY (ELEMENT-TYPE 'CHARACTER))
  (mk-STRING-STREAM
   (kw direction) (kw OUTPUT)
   (kw string) ""
   (kw write-fn)
     (lambda (char stream)
       (setf (STRING-STREAM-string stream)
	     (concat (STRING-STREAM-string stream)
		     (list (el-char char)))))))

(cl:defmacro WITH-INPUT-FROM-STRING ((var string &key INDEX START END)
				     &body body)
  ;; TODO: INDEX
  (when (null START)
    (setq START 0))
  `(WITH-OPEN-STREAM (,var (MAKE-STRING-INPUT-STREAM ,string ,START ,END))
     ,@body))

(defmacro* WITH-OUTPUT-TO-STRING ((var &optional string) &body body)
  (if string
      `(WITH-OPEN-STREAM (,var (make-fill-pointer-output-stream ,string))
	 ,@body)
      `(WITH-OPEN-STREAM (,var (MAKE-STRING-OUTPUT-STREAM))
	 ,@body
	 (GET-OUTPUT-STREAM-STRING ,var))))

(cl:defmacro WITH-OUTPUT-TO-STRING ((var &optional string &key ELEMENT-TYPE)
				    &body body)
  (when (null ELEMENT-TYPE)
    (setq ELEMENT-TYPE '(QUOTE CHARACTER)))
  (if string
      `(WITH-OPEN-STREAM (,var (make-fill-pointer-output-stream ,string))
	 ,@body)
      `(WITH-OPEN-STREAM (,var (MAKE-STRING-OUTPUT-STREAM
				,(kw ELEMENT-TYPE) ,ELEMENT-TYPE))
	 ,@body
	 (GET-OUTPUT-STREAM-STRING ,var))))

(DEFVAR *DEBUG-IO* nil)
(DEFVAR *ERROR-OUTPUT* nil)
(DEFVAR *QUERY-IO* nil)
;;; *STANDARD-INPUT* defined above.
;;; *STANDARD-OUTPUT* defined above.
(DEFVAR *TRACE-OUTPUT* nil)
;;; *TERMINAL-IO* defined above.

;;; STREAM-ERROR, STREAM-ERROR-STREAM, and END-OF-FILE defined by
;;; cl-conditions.el.


(defun make-buffer-output-stream (buffer)
  (MAKE-STREAM (kw direction) (kw OUTPUT)
	       (kw content) buffer
	       (kw write-fn) (lambda (char stream)
			       (with-current-buffer (STREAM-content stream)
				 (insert (el-char char)))
			       (when (ch= char 10)
				 (sit-for 0)))))

(defun make-buffer-input-stream (buffer)
  (MAKE-STREAM
   (kw direction) (kw INPUT)
   (kw content) buffer
   (kw position) 0
   (kw read-fn)
   (lambda (stream)
     (with-current-buffer (STREAM-content stream)
       (if (= (STREAM-position stream) (buffer-size))
	   :eof
	   (cl-char (char-after (incf (STREAM-position stream)))))))))

(defun make-read-char-exclusive-input-stream ()
  (MAKE-STREAM (kw direction) (kw INPUT)
	       (kw read-fn) (cl:function read-char-exclusive-ignoring-arg)
	       (kw position) nil))

(defun make-fill-pointer-output-stream (string)
  (mk-STRING-STREAM (kw direction) (kw OUTPUT)
		    (kw string) string
		    (kw write-fn) (lambda (char stream)
				    (VECTOR-PUSH-EXTEND
				     char
				     (STRING-STREAM-string stream)))))

(defun make-princ-stream ()
  (MAKE-STREAM (kw direction) (kw OUTPUT)
	       (kw write-fn)
	         (lambda (char stream) (princ (string (el-char char))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-streams.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-subtypep.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;;
;;; This file implements the SUBTYPEP function from chapter 4, Types
;;; and Classes.  The implementation is based on Henry Baker's paper A
;;; Decision Procedure for Common Lisp's SUBTYPEP Predicate.

(IN-PACKAGE "EMACS-CL")

(defvar *types*
  '(nil COMPLEX KEYWORD SYMBOL CONS CHARACTER FUNCTION COMPILED-FUNCTION))

(defvar *subtypep-objects*
  (list (COMPLEX 0 1) nil T (make-symbol "") (cons nil nil)
	;; Should really be an uninterned keyword.
	(kw really-unlikely-keyword-name)
	;; This guarantees unique character objects.
	(vector 'CHARACTER 0)
	(vector 'CHARACTER 65)
	(vector 'INTERPRETED-FUNCTION '(LAMBDA ()) nil nil nil)
	(byte-compile '(lambda ()))))

(defvar *type-val* (make-hash-table :test 'equal))

(defun object-val (object)
  (ASH 1 (position object *subtypep-objects*)))

(defun register-object (object)
  ;(FORMAT T "~&Object #<~S>:" (TYPE-OF object))
  (dolist (type *types*)
    (when (TYPEP object type)
      ;(FORMAT T " ~S" type)
      (setf (gethash type *type-val*)
	    (LOGIOR (gethash type *type-val*) (object-val object))))))

(dolist (type *types*)
  (setf (gethash type *type-val*) 0))

(dolist (object *subtypep-objects*)
  (register-object object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun integer-endpoint (x high)
  (cond
    ((eq x star)
     x)
    ((INTEGERP x)
     x)
    ((REALP x)
     (cl:values (if high (FLOOR x) (CEILING x))))
    ((consp x)
     (setq x (car x))
     (if (INTEGERP x)
	 (if high (cl:1- x) (cl:1+ x))
	 (cl:values (if high
			(cl:1- (CEILING x))
			(cl:1+ (FLOOR x))))))
    (t
     (type-error x `(OR REAL (CONS REAL NULL) (EQL ,star))))))

(defun* simplify-integer-range (range &optional high)
  (unless (null range)
    (let ((x (integer-endpoint (pop range) high)))
      (cond
	((eq x star)
	 (cons x (simplify-integer-range range (not high))))
	((and high
	      (not (null range))
	      (EQL (cl:1+ x)
		   (integer-endpoint (first range) (not high))))
	 (pop range)
	 (simplify-integer-range range high))
	((and (not high)
	      (not (eq (first range) star))
	      (cl:> x (integer-endpoint (first range) high)))
	 (when (= (length range) 1)
	   (return-from simplify-integer-range nil))
	 (pop range)
	 (simplify-integer-range range high))
	(t
	 (cons x (simplify-integer-range range (not high))))))))

(defun not-integer-p (x)
  (or (ratiop x)
      (and (floatp x)
	   (not (zerop (NTH-VALUE 1 (TRUNCATE x)))))))

(defun* simplify-ratio-range (range &optional high)
  (unless (null range)
    (let ((x (pop range)))
      (cond
	((eq x star)
	 (cons x (simplify-ratio-range range (not high))))
	((and (EQUAL x (first range))
	      (if high
		  (or (and (consp x) (INTEGERP (car x)))
		      (not-integer-p x))
		  (or (and (consp x) (not-integer-p (car x)))
		      (INTEGERP x))))
	 ;; For example, leave (* (1/2) (1/2) *) alone,
	 ;; but merge (* (0) (0) *) into (* *).
	 (pop range)
	 (simplify-ratio-range range high))
	(t
	 (cons x (simplify-ratio-range range (not high))))))))

(defun range-complement (range)
  (cond
    ((null range)
     (list star star))
    (t
     (setq range (if (eq (first range) star)
		     (rest range)
		     (cons star range)))
     (setq range (if (eq (first (last range)) star)
		     (butlast range)
		     (append range (list star))))
     (mapcar (lambda (x)
	       (cond
		 ((eq x star) star)
		 ((consp x) (first x))
		 (t (list x))))
	     range))))

(defun ll<= (x y)
  (cond
    ((eq x star))
    ((eq y star) nil)
    ((consp x)
     (if (consp y)
	 (binary<= (first x) (first y))
	 (binary< (first x) y)))
    ((consp y)
     (binary<= x (first y)))
    ((binary<= x y))))

(defun lh> (x y)
  (cond
    ((eq x star) nil)
    ((eq y star) nil)
    ((consp x)
     (if (consp y)
	 (if (and (ratiop x) (ratiop y))
	     (binary< (first y) (first x))
	     (binary<= (first y) (first x)))
	 (binary< y (first x))))
    ((consp y)
     (binary< (first y) x))
    ((binary< y x))))

(defun hh<= (x y)
  (cond
    ((eq x star) nil)
    ((eq y star))
    ((consp x)
     (if (consp y)
	 (binary<= (first x) (first y))
	 (binary<= (first x) y)))
    ((consp y)
     (binary< x (first y)))
    ((binary<= x y))))

(defun range-union (ranges1 ranges2)
  (when (and ranges1 ranges2 (ll<= (first ranges2) (first ranges1)))
    (psetq ranges1 ranges2
	   ranges2 ranges1))
;   (print (format "union %s %s" ranges1 ranges2))
  (let ((low1 (first ranges1))
	(low2 (first ranges2))
	(high1 (second ranges1))
	(high2 (second ranges2)))
    (cond
      ((null ranges1)
       ranges2)
      ((null ranges2)
       ranges1)
      ((lh> low2 high1)
;        (print
; 	(let ((standard-output (lambda (ch) nil)))
; 	 (format "A: (%s %s ...)   %s + %s -> %s\n=> %s"
; 		 low1 high1 (cddr ranges1) ranges2
; 		 (range-union (cddr ranges1) ranges2)
; 		 (list* low1 high1 (range-union (cddr ranges1) ranges2)))))
       (list* low1 high1 (range-union (cddr ranges1) ranges2)))
      ((hh<= high1 high2)
;        (print
; 	(let ((standard-output (lambda (ch) nil)))
; 	 (format "B: (%s %s ...)   %s + %s -> %s\n=> %s"
; 		 low1 high2 (cddr ranges1) ranges2
; 		 (range-union (cddr ranges1) ranges2)
; 		 (let ((u (range-union (cddr ranges1) ranges2)))
; 		   (when (and u (hh<= high2 (second u)))
; 		     (setq high2 (second u)))
; 		   (list* low1 high2 (cddr u))))))
       (let ((u (range-union (cddr ranges1) ranges2)))
	 (when (and u (hh<= high2 (second u)))
	   (setq high2 (second u)))
	 (list* low1 high2 (cddr u))))
      (t
;        (print
; 	(let ((standard-output (lambda (ch) nil)))
; 	 (format "C: (%s %s ...)   %s + %s -> %s"
; 		 low1 high1 ranges1 (cddr ranges2)
; 		 (range-union ranges1 (cddr ranges2)))))
       (list* low1 high1 (cddr (range-union ranges1 (cddr ranges2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun integer-union (r1 r2)
  (simplify-integer-range (range-union r1 r2)))

(defun ratio-union (r1 r2)
  (simplify-ratio-range (range-union r1 r2)))

(defun range-intersection (r1 r2)
  (range-complement (range-union (range-complement r1)
				 (range-complement r2))))

(defun integer-intersection (r1 r2)
  (integer-complement (integer-union (integer-complement r1)
				     (integer-complement r2))))

(defun type-union (v1 v2)
  `(,(discrete-union (first v1) (first v2))
    ,(real-union (second v1) (second v2))
    ,(array-union (third v1) (third v2))))

(defun discrete-union (t1 t2)
  (LOGIOR t1 t2))

(defun real-union (r1 r2)
  `(,(integer-union (first r1) (first r2))
    ,(ratio-union (second r1) (second r2))
    ,(range-union (third r1) (third r2))))

(defun array-union (t1 t2)
  (MAPCAR #'LOGIOR t1 t2))

(defun type-intersection (v1 v2)
  `(,(discrete-intersection (first v1) (first v2))
    ,(real-intersection (second v1) (second v2))
    ,(array-intersection (third v1) (third v2))))

(defun discrete-intersection (t1 t2)
  (LOGAND t1 t2))

(defun real-intersection (r1 r2)
  `(,(integer-intersection (first r1) (first r2))
    ,(range-intersection (second r1) (second r2))
    ,(range-intersection (third r1) (third r2))))

(defun array-intersection (t1 t2)
  (MAPCAR #'LOGAND t1 t2))

(defun type-complement (type)
  `(,(discrete-complement (first type))
    ,(real-complement (second type))
    ,(array-complement (third type))))

(defun discrete-complement (type)
  (LOGNOT type))

(defun real-complement (type)
  `(,(integer-complement (first type))
    ,(range-complement (second type))
    ,(range-complement (third type))))

(defun integer-complement (range)
  (simplify-integer-range (range-complement range)))

(defun array-complement (type)
  (mapcar #'LOGNOT type))

(defun discrete-type-val (type)
  (or (gethash type *type-val*)
      (let ((val 0))
	(dolist (object *subtypep-objects*)
	  (when (TYPEP object type)
	    (setq val (LOGIOR val (object-val object)))))
	val)))

(defun real-type-val (type)
  (if (atom type)
      (case type
	(INTEGER
		`((,star ,star) () ()))
	(RATIONAL
		`((,star ,star) (,star ,star) ()))
	(SINGLE-FLOAT
		`(() () (,star ,star)))
	(t
		`(() () ())))
      (case (first type)
	(INTEGER
		(let ((range (rest type)))
		  (while (< (length range) 2)
		    (setq range (append range (list star))))
		  `(,(simplify-integer-range range) () ())))
	(RATIONAL
		(let ((range (rest type)))
		  (while (< (length range) 2)
		    (setq range (append range (list star))))
		  `(,(simplify-integer-range range) ,range ())))
	(SINGLE-FLOAT
		(let ((range (rest type)))
		  (while (< (length range) 2)
		    (setq range (append range (list star))))
		  `(() () ,range)))
	(EQL
	 (let ((obj (second type)))
	   (cond
	     ((INTEGERP obj)
		`((,obj ,obj) () ()))
	     ((ratiop obj)
		`(() (,obj ,obj) ()))
	     ((FLOATP obj)
		`(() () (,obj ,obj)))
	     (t
		'(() () ())))))
	((ARRAY COMPLEX CONS SIMPLE-ARRAY)
		`(() () ()))
	(t	(ERROR "Uknown type specifier: ~S." type)))))

(defun stars (n)
  (with-collector collect
    (dotimes (i n)
      (collect star))))

(defun iterate-over-dimensions (dimensions rank i w pos dims)
  (cond
    ((null dimensions)
     ;(FORMAT T "~& i=~D w=~D pos=~S dims=~S" i w pos dims)
     (let ((v t))
       (dotimes (j rank)
	 ;(FORMAT T "~&   ~S ~S" (nth j pos) (nth j dims))
	 (unless (or (eq (nth j dims) star)
		     (eql (nth j pos) (nth j dims)))
	   (setq v nil)))
       ;(FORMAT T "~&   v=~S" v)
       (when v
	 (setq *val* (LOGIOR *val* (ASH 1 i))))))
    (t
     (let ((w2 (* w (1+ (length (first dimensions))))))
       (dolist (d (append (first dimensions) (list star)))
	 (iterate-over-dimensions
	  (rest dimensions) rank i w2 (append pos (list d)) dims)
	 (incf i w))))))

(defun array-type-val (type)
  (setq type (ensure-list type))
  (cond
    ((or (eq (first type) 'ARRAY)
	 (eq (first type) 'SIMPLE-ARRAY))
     (while (< (length type) 3)
       (setq type (append type (list star))))
     (let* ((*val* (if (eq (first type) 'ARRAY) 1 0))
	    (element-type (second type))
	    (dims (third type))
	    (rank dims)
	    (n 1)
	    (pos 0))
       (cond
	 ((eq dims star)
	  (setq *val* (logior *val* -2)))
	 ((integerp dims)
	  (setq dims (stars dims)))
	 (t
	  (setq rank (length dims))))
       (unless (eq dims star)
	 (iterate-over-dimensions
	  (gethash rank *array-bounds*) rank
	  (gethash rank *rank-index*) 1 nil dims))
;	 (FORMAT T "~&~S dimensions ~S => " (first type) dims)
;	 (WRITE *val* (kw BASE) 2)
       (if (eq element-type star)	`(,*val* ,*val* ,*val* ,*val*)
	   (case (UPGRADED-ARRAY-ELEMENT-TYPE element-type)
	     ((nil)			`(,*val* 0 0 0))
	     (BIT			`(0 ,*val* 0 0))
	     (CHARACTER			`(0 0 ,*val* 0))
	     (T				`(0 0 0 ,*val*))))))
    (t
     '(0 0 0 0))))

(defun type-val (type)
  (setq type (expand-type type nil))
  (cond
    ((eq type 'T)
     `(-1 ((,star ,star) (,star ,star) (,star ,star)) (-1 -1 -1)))
    ;((memq type *types*)
     ;`(,(gethash type *type-val*) (() () ()) ,(array-type-val type)))
    ((atom type)
     `(,(discrete-type-val type) ,(real-type-val type) ,(array-type-val type)))
    (t
     (case (first type)
       ((FUNCTION SATISFIES VALUES)
	(throw 'SUBTYPEP (cl:values nil nil)))
       (AND
	(if (null (rest type))
	    (type-val 'T)
	    (reduce #'type-intersection (rest type) :key #'type-val)))
       (OR
	(if (null (rest type))
	    (type-val nil)
	    (reduce #'type-union (rest type) :key #'type-val)))
       (NOT
	(type-complement (type-val (second type))))
       (t
	`(,(discrete-type-val type)
	  ,(real-type-val type)
	  ,(array-type-val type)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *array-bounds*)
(defvar *rank-index*)
(defvar *highest-rank*)

(defun do-array-stuff (dims)
  (unless (eq dims star)
    (when (integerp dims)
      (setq dims (stars dims)))
    (let ((n (length dims)))
      (when (> n *highest-rank*)
	(setq *highest-rank* n))
      (when (null (gethash n *array-bounds*))
	(setf (gethash n *array-bounds*)
	      (make-list n nil)))
      (dotimes (i n)
	(unless (eq (nth i dims) star)
	  (pushnew (nth i dims)
		   (nth i (gethash n *array-bounds*))
		   :test #'eql))))))

(defun find-new-objects (type env)
  (setq type (expand-type type env))
  (when (consp type)
    (case (first type)
      ((AND OR NOT)
       (mapc #'find-new-objects (rest type)))
      (EQL
       (push type *types*)
       (pushnew (second type) *subtypep-objects*))
      ((ARRAY SIMPLE-ARRAY)
       (let ((dims star))
	 (when (> (length type) 2)
	   (setq dims (third type)))
	 (do-array-stuff dims))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun* SUBTYPEP (type1 type2 &optional env)
  (when (null type1)
    (return-from SUBTYPEP (cl:values 'T 'T)))
  (when (eq type2 'T)
    (return-from SUBTYPEP (cl:values 'T 'T)))
  (let ((*types* *types*)
	(*type-val* *type-val*)
	(*subtypep-objects* *subtypep-objects*)
	(*array-bounds* (make-hash-table :test #'eql))
	(*rank-index* (make-hash-table :test #'eql))
	(*highest-rank* 0))
    (find-new-objects type1 env)
    (find-new-objects type2 env)
    (let ((bit-index 1))
      (dotimes (i (1+ *highest-rank*))
	(setf (gethash i *rank-index*) bit-index)
	(let ((bits 1))
	  (dotimes (j (length (gethash i *array-bounds*)))
	    (setq bits
		  (* bits (1+ (length (nth j (gethash i *array-bounds*))))))
	    (setf (nth j (gethash i *array-bounds*))
		  (sort (nth j (gethash i *array-bounds*)) #'binary<)))
	  (incf bit-index bits))))
;   (dotimes (i (1+ *highest-rank*))
;     (FORMAT T "~&Rank ~D: ~S, index ~D"
;	      i (gethash i *array-bounds*) (gethash i *rank-index*)))
    (dolist (type *types*)
      (setf (gethash type *type-val*) 0))
    (dolist (object *subtypep-objects*)
      (register-object object))
    (catch 'SUBTYPEP
;     (FORMAT T "~&Type ~S = ~S => ~S"
;	     type1 (expand-type type1 nil) (type-val type1))
;     (FORMAT T "~&Type ~S = ~S => ~S"
;	     type2 (expand-type type2 nil) (type-val type2))
;     (PRINT (type-val `(AND ,type1 (NOT ,type2))))
     (let* ((val (type-val `(AND ,type1 (NOT ,type2))))
	    (ranges (second val)))
       (cl:values (and (ZEROP (first val))
		       (null (first ranges))
		       (null (second ranges))
		       (null (third ranges))
		       (every #'ZEROP (third val)))
		  'T)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-subtypep.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./batch.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; Batch-mode REPL.  This code is used by the "emacs-cl" script.

(setq *STANDARD-OUTPUT* (make-princ-stream)
      *ERROR-OUTPUT* *STANDARD-OUTPUT*
      *TRACE-OUTPUT* *STANDARD-OUTPUT*)
(setq *STANDARD-INPUT* (make-read-char-exclusive-input-stream))
(setq *TERMINAL-IO* (MAKE-TWO-WAY-STREAM *STANDARD-INPUT* *STANDARD-OUTPUT*)
      *QUERY-IO* *TERMINAL-IO*
      *DEBUG-IO* *TERMINAL-IO*)

(defun batch-repl ()
  (loop
   (FORMAT T "~%~A> " (PACKAGE-NAME *PACKAGE*))
   (dolist (x (emacs-cl-eval-interactively (READ)))
     (PPRINT x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./batch.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-characters.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.
;;; This file implements operators in chapter 13, Characters.

(IN-PACKAGE "EMACS-CL")

;;; System Class CHARACTER
;;; Type BASE-CHAR
;;; Type STANDARD-CHAR
;;; Type EXTENDED-CHAR

(unless use-character-type-p
  (define-storage-layout char (code)))

(defun CHAR= (&rest chars)
  (apply #'cl:= (mapcar #'CHAR-CODE chars)))

(defun CHAR/= (&rest chars)
  (apply #'cl:/= (mapcar #'CHAR-CODE chars)))

(defun CHAR< (&rest chars)
  (apply #'cl:< (mapcar #'CHAR-CODE chars)))

(defun CHAR> (&rest chars)
  (apply #'cl:> (mapcar #'CHAR-CODE chars)))

(defun CHAR<= (&rest chars)
  (apply #'cl:<= (mapcar #'CHAR-CODE chars)))

(defun CHAR>= (&rest chars)
  (apply #'cl:>= (mapcar #'CHAR-CODE chars)))

(defun char-upcase-code (char)
  (CHAR-CODE (CHAR-UPCASE char)))

(defun CHAR-EQUAL (&rest chars)
  (apply #'cl:= (mapcar #'char-upcase-code chars)))

(defun CHAR-NOT-EQUAL (&rest chars)
  (apply #'cl:/= (mapcar #'char-upcase-code chars)))

(defun CHAR-LESSP (&rest chars)
  (apply #'cl:< (mapcar #'char-upcase-code chars)))

(defun CHAR-GREATERP (&rest chars)
  (apply #'cl:> (mapcar #'char-upcase-code chars)))

(defun CHAR-NOT-GREATERP (&rest chars)
  (apply #'cl:<= (mapcar #'char-upcase-code chars)))

(defun CHAR-NOT-LESSP (&rest chars)
  (apply #'cl:>= (mapcar #'char-upcase-code chars)))

(defun CHARACTER (x)
  (cond
    ((CHARACTERP x)			x)
    ((and (STRINGP x) (= (LENGTH x) 1))	(AREF x 0))
    ((SYMBOLP x)			(CHARACTER (SYMBOL-NAME x)))
    (t
     (error "invalid character designator"))))

(if use-character-type-p
    (fset 'CHARACTERP (symbol-function 'characterp))
    (defun CHARACTERP (char)
      (vector-and-typep char 'CHARACTER)))

(defun ALPHA-CHAR-P (char)
  (or (cl:<= 65 (CHAR-CODE char) 90)
      (cl:<= 97 (CHAR-CODE char) 122)))

(defun ALPHANUMERICP (char)
  (or (DIGIT-CHAR-P char) (ALPHA-CHAR-P char)))

(defun* DIGIT-CHAR (weight &optional (radix 10))
  (when (cl:< weight radix)
    (CODE-CHAR (if (< weight 10)
		   (+ 48 weight)
		   (+ 65 weight -10)))))

(defun* DIGIT-CHAR-P (char &optional (radix 10))
  (let* ((code (CHAR-CODE char))
	 (n (cond
	      ((cl:<= 48 code 57) (- code 48))
	      ((cl:<= 65 code 90) (- code 65 -10))
	      ((cl:<= 95 code 122) (- code 95 -10))
	      (t 99))))
    (if (< n radix) n nil)))

(defun GRAPHIC-CHAR-P (char)
  (let ((code (CHAR-CODE char)))
    (and (>= code 32) (<= code 126))))

(defconst standard-chars
    "\n abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!$\"'(),_-./:;?+<=>#%&*@[\\]{|}`^~")

(if use-character-type-p
    (defun STANDARD-CHAR-P (char)
      (find char standard-chars))
    (defun STANDARD-CHAR-P (char)
      (find (CHAR-CODE char) standard-chars)))

(defun CHAR-UPCASE (char)
  (if (LOWER-CASE-P char)
      (CODE-CHAR (- (CHAR-CODE char) 32))
      char))

(defun CHAR-DOWNCASE (char)
  (if (UPPER-CASE-P char)
      (CODE-CHAR (+ (CHAR-CODE char) 32))
      char))

(defun UPPER-CASE-P (char)
  (cl:<= 65 (CHAR-CODE char) 90))

(defun LOWER-CASE-P (char)
  (cl:<= 97 (CHAR-CODE char) 122))

(defun BOTH-CASE-P (char)
  (or (UPPER-CASE-P char) (LOWER-CASE-P char)))

(if use-character-type-p
    (fset 'CHAR-CODE (symbol-function 'char-to-int))
    (defun CHAR-CODE (char)
      (char-code char)))

(fset 'CHAR-INT (symbol-function 'CHAR-CODE))

(if use-character-type-p
    (defun CODE-CHAR (code)
      (if (and (integerp code) (< code CHAR-CODE-LIMIT))
	  (int-char code)
	  nil))
    (defun CODE-CHAR (code)
      (if (and (integerp code) (< code CHAR-CODE-LIMIT))
	  (vector 'CHARACTER code)
	  nil)))

(DEFCONSTANT CHAR-CODE-LIMIT 256)

(defun NAME-CHAR (name)
  (let ((string (STRING name)))
    (cond
      ((equalp string "Backspace")	(ch 8))
      ((equalp string "Tab")		(ch 9))
      ((equalp string "Newline")	(ch 10))
      ((equalp string "Linefeed")	(ch 10))
      ((equalp string "Page")		(ch 12))
      ((equalp string "Return")		(ch 13))
      ((equalp string "Space")		(ch 32))
      ((equalp string "Rubout")		(ch 127)))))

(defun CHAR-NAME (char)
  (case (CHAR-CODE char)
    (8		"Backspace")
    (9		"Tab")
    (10		"Newline")
    (12		"Page")
    (13		"Return")
    (32		"Space")
    (127	"Rubout")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-characters.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-evaluation.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 3, Evaluation and Compilation.

(IN-PACKAGE "EMACS-CL")

;;; Assigned later in populate-packages.
(defvar *global-environment* nil)

(defvar *compiler-macro-functions* (make-hash-table :test 'equal))

(defvar *macro-functions* (make-hash-table))

(defvar *symbol-macro-functions* (make-hash-table))

(defun COMPILER-MACRO-FUNCTION (name &optional env)
  (gethash name *compiler-macro-functions*))

(defsetf COMPILER-MACRO-FUNCTION (name &optional env) (fn)
  `(setf (gethash ,name *compiler-macro-functions*) ,fn))

;; DEFINE-COMPILER-MACRO defined later.

;;; Redefined later in cl-eval.el.
(defun lexical-function (name env)
  nil)

(defun MACRO-FUNCTION (name &optional env)
  (when (null env)
    (setq env *global-environment*))
  (multiple-value-bind (type localp decl) (function-information name env)
    (when (eq type :macro)
      (if localp
	  (lexical-function name env)
	  (gethash name *macro-functions*)))))

(defsetf MACRO-FUNCTION (name &optional env) (fn)
  `(if (null ,env)
       (setf (gethash ,name *macro-functions*) ,fn)
       (set-local-macro ,name ,fn ,env)))

(defun make-macro-el-function (name lambda-list body)
  (with-gensyms (fvar evar)
    (let ((e (memq '&environment lambda-list))
	  (form fvar))
      (when e
	(when (null (cdr e))
	  (ERROR 'PROGRAM-ERROR))
	(setq evar (second e))
	(let ((x lambda-list))
	  (while x
	    (when (eq (cadr x) '&environment)
	      (setf (cdr x) (cdddr x)))
	    (setq x (cdr x)))))
      (if (eq (first lambda-list) '&whole)
	  (unless (= (length lambda-list) 2)
	    (push (gensym) (cddr lambda-list)))
	  (setq form `(cdr ,fvar)))
      (unless (null lambda-list)
	(setq body `((destructuring-bind ,lambda-list ,form ,@body))))
      `(lambda (,fvar ,evar) ,@body))))

(defmacro* cl:defmacro (name lambda-list &body body)
  (when byte-compile-warnings
    (byte-compile-log-1 (format "cl:defmacro %s" name)))
  `(progn
     (unless (fboundp ',name)
       (fset ',name nil))
     (setf (MACRO-FUNCTION ',name)
           ,(make-macro-el-function name lambda-list body))
    ',name))

(cl:defmacro DEFMACRO (name lambda-list &body body)
  `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
     (fset (QUOTE ,name) nil)
     (SETF (MACRO-FUNCTION (QUOTE ,name))
           ,(make-macro-function name lambda-list body))
     (QUOTE ,name)))

(cl:defmacro DEFINE-COMPILER-MACRO (name lambda-list &body body)
  `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
     (SETF (COMPILER-MACRO-FUNCTION (QUOTE ,name))
           ,(make-macro-function name lambda-list body nil
				 :type 'COMPILER-MACRO))
     (QUOTE ,name)))

(cl:defmacro LAMBDA (lambda-list &body body)
  `(FUNCTION (LAMBDA ,lambda-list ,@body)))

;;; COMPILE is defined in cl-compile.el.

(defun MACROEXPAND-1 (form &optional env)
  (cond
    ((and (consp form)
	  (symbolp (car form)))
     (let ((fn (MACRO-FUNCTION (car form) env)))
       (if fn
	   (let ((new (FUNCALL *MACROEXPAND-HOOK* fn form env)))
	     (cl:values new (not (eq form new))))
	   (cl:values form nil))))
    ((symbolp form)
     (multiple-value-bind (type localp decls) (variable-information form env)
       (if (eq type :symbol-macro)
	   (if localp
	       (let ((fn (lexical-value form env)))
		 (cl:values (funcall *MACROEXPAND-HOOK* fn form env) T))
	       (let ((fn (gethash form *symbol-macro-functions*)))
		 (if fn
		     (cl:values (funcall *MACROEXPAND-HOOK* fn form env) T)
		     (cl:values form nil))))
	   (cl:values form nil))))
    (t
     (cl:values form nil))))

(defun* MACROEXPAND (form &optional env)
  (let ((form form) (expanded-p nil) exp)
    (loop
     (MULTIPLE-VALUE-SETQ (form exp) (MACROEXPAND-1 form env))
     (if exp
	 (setq expanded-p T)
	 (return-from MACROEXPAND (cl:values form expanded-p))))))

(defmacro* DEFINE-SYMBOL-MACRO (symbol expansion)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',symbol *symbol-macro-functions*)
           (cl:lambda (form env) ',expansion))
     ',symbol))

(cl:defmacro DEFINE-SYMBOL-MACRO (symbol expansion)
  `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
     (puthash (QUOTE ,symbol) (LAMBDA (form env) (QUOTE ,expansion))
              *symbol-macro-functions*)
     (QUOTE ,symbol)))

(defvar *MACROEXPAND-HOOK* 'FUNCALL)

(defvar *declarations*
  '(IGNORE IGNORABLE DYNAMIC-EXTENT TYPE INLINE
    NOTINLINE FTYPE DECLARATION OPTIMIZE SPECIAL
    ;; Emacs Common Lisp extensions:
    INTERACTIVE)
  "A list of valid declaration identifiers.")

(defun valid-declaration-identifier-p (object)
  (or (memq object *declarations*)
      (gethash object *atomic-typespecs*)
      (gethash object *deftype-expanders*)
      (classp object)))

(defun PROCLAIM (declaration)
  (unless (and (consp declaration)
	       (valid-declaration-identifier-p (car declaration)))
    (type-error declaration `(CONS (MEMBER ,@*declarations*) LIST)))
  (case (first declaration)
    (SPECIAL
     (dolist (var (rest declaration))
       (pushnew var *specials*)))
    (INLINE)
    (NOTINLINE)
    (DECLARATION
     (dolist (name (rest declaration))
       (pushnew name *declarations*))))
  nil)

(cl:defmacro DECLAIM (&rest declarations)
  `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
     ,@(mapcar (lambda (decl) `(PROCLAIM (QUOTE ,decl)))
	       declarations)))

;;; THE setf expansion defined in cl-flow.el.

(defun SPECIAL-OPERATOR-P (symbol)
  (unless (symbolp symbol)
    (type-error symbol 'SYMBOL))
  (memq symbol
	'(BLOCK CATCH EVAL-WHEN FLET FUNCTION GO IF LABELS LET LET*
	  LOAD-TIME-VALUE LOCALLY MACROLET MULTIPLE-VALUE-CALL
	  MULTIPLE-VALUE-PROG1 PROGN PROGV QUOTE RETURN-FROM SETQ
	  SYMBOL-MACROLET TAGBODY THE THROW UNWIND-PROTECT)))

(defun quoted-object-p (object)
  (and (consp object)
       (eq (car object) 'QUOTE)))

(defun CONSTANTP (object &optional env)
  (unless env
    (setq env *global-environment*))
  (cond
    ((KEYWORDP object))
    ((symbolp object)
     (memq object *constants*))
    ((atom object))
    ((quoted-object-p object))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-evaluation.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-files.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 20, Files.

(IN-PACKAGE "EMACS-CL")

(defun file-error (pathname)
  (ERROR 'FILE-ERROR (kw PATHNAME) pathname))

(defun wild-directories (name dir pathname files)
  (if (null dir)
      (nconc (DIRECTORY (MERGE-PATHNAMES name pathname)) files)
      (let ((component (first dir)))
	(setq dir (rest dir))
	(cond
	  ((eq component (kw WILD))
	   (dolist (file (directory-files name) files)
	     (unless (or (string= file ".") (string= file ".."))
	       (setq file (concat name file "/"))
	       (when (file-directory-p file)
		 (setq files (wild-directories file dir pathname files))))))
	  ((eq component (kw WILD-INFERIORS))
	   (setq files (wild-directories name dir pathname files))
	   (dolist (file (directory-files name) files)
	     (unless (or (string= file ".") (string= file ".."))
	       (setq file (concat name file "/"))
	       (when (file-directory-p file)
		 (setq files (wild-directories
			      file (cons (kw WILD-INFERIORS) dir)
			      pathname files))))))
	  ((eq component (kw UP))
	   (wild-directories (concat name "../") dir pathname files))
	  ((eq component (kw BACK))
	   (ERROR ":BACK isn't supported"))
	  (t
	   (let ((file (concat name component "/")))
	     (if (file-directory-p file)
		 (wild-directories file dir pathname files)
		 files)))))))

(defun DIRECTORY (pathname-designator)
  (let ((pathname (MERGE-PATHNAMES pathname-designator)))
    (if (WILD-PATHNAME-P pathname (kw DIRECTORY))
	(let* ((dir (PATHNAME-DIRECTORY pathname))
	       (x (pop dir))
	       (name (cond
		       ((eq x (kw ABSOLUTE)) "/")
		       ((or (null x) (eq x (kw RELATIVE))) "./")
		       (t (error "error")))))
	  (wild-directories name dir pathname nil))
	(let ((result nil)
	      (dir (MAKE-PATHNAME (kw DIRECTORY)
				  (PATHNAME-DIRECTORY pathname))))
	  (dolist (file (directory-files (DIRECTORY-NAMESTRING pathname)))
	    (setq file (MERGE-PATHNAMES file dir))
	    (when (PATHNAME-MATCH-P file pathname)
	      (push file result)))
	  result))))

(defun PROBE-FILE (pathname-designator)
  (let ((pathname (MERGE-PATHNAMES pathname-designator)))
    (when (file-exists-p (NAMESTRING pathname))
      (TRUENAME pathname))))

(cl:defun ENSURE-DIRECTORIES-EXIST (pathname-designator &KEY VERBOSE)
  (let* ((pathname (MERGE-PATHNAMES pathname-designator))
	 (dir (DIRECTORY-NAMESTRING pathname)))
    (when (or (eq (PATHNAME-HOST pathname) (kw WILD))
	      (eq (PATHNAME-DEVICE pathname) (kw WILD))
	      (or (memq (kw WILD) (PATHNAME-DIRECTORY pathname))
		  (memq (kw WILD-INFERIORS) (PATHNAME-DIRECTORY pathname))))
      (ERROR 'FILE-ERROR))
    (cl:values pathname-designator
	       (unless (file-exists-p dir)
		 (make-directory dir t)
		 T))))

(defun TRUENAME (pathname-designator)
  (let ((pathname (MERGE-PATHNAMES pathname-designator)))
    (PATHNAME (file-truename (NAMESTRING pathname)))))

(defun FILE-AUTHOR (pathname-designator)
  (let ((pathname (MERGE-PATHNAMES pathname-designator)))
    (user-login-name (nth 2 (file-attributes (NAMESTRING pathname))))))

(defun FILE-WRITE-DATE (pathname-designator)
  (let* ((pathname (MERGE-PATHNAMES pathname-designator))
	 (filename (NAMESTRING pathname))
	 (x (nth 5 (file-attributes filename)))
	 (y (first x))
	 (z (second x)))
    (when (null x)
      (file-error pathname))
    (cl:+ (binary* y 65536) z universal-time-offset)))

(defun RENAME-FILE (old-pathname-designator new-pathname-designator)
  (let* ((old-pathname (MERGE-PATHNAMES old-pathname-designator))
	 (new-pathname (MERGE-PATHNAMES new-pathname-designator old-pathname)))
    (rename-file (NAMESTRING old-pathname) (NAMESTRING new-pathname) t)
    (cl:values new-pathname (TRUENAME old-pathname) (TRUENAME new-pathname))))

(defun DELETE-FILE (pathname-designator)
  (let* ((pathname (MERGE-PATHNAMES pathname-designator))
	 (filename (NAMESTRING pathname)))
    (if (file-exists-p filename)
	(delete-file filename)
	(file-error pathname))
    T))

;;; FILE-ERROR and FILE-ERROR-PATHNAME are defined in cl-conditions.el.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-files.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-hash.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 18, Hash Tables.

(IN-PACKAGE "EMACS-CL")

(if (eval-when-compile (eq (type-of (make-hash-table)) 'hash-table))
    (progn
      (cl:defun MAKE-HASH-TABLE (&KEY TEST (SIZE 10) REHASH-SIZE
				 REHASH-THRESHOLD)
	(make-hash-table :test (el-test TEST) :size SIZE))

      (when (eval-when-compile (fboundp 'define-hash-table-test))
	(define-hash-table-test 'EQL #'EQL #'sxhash)
	(define-hash-table-test 'EQUAL #'EQUAL #'sxhash)
	(define-hash-table-test 'EQUALP #'EQUALP #'equalp-hash)

	(defun equalp-hash (object)
	  (cond
	    ((CHARACTERP object)	(sxhash (CHAR-UPCASE object)))
	    ((REALP object)		(sxhash (FLOAT object)))
	    ((NUMBERP object)		(+ (sxhash (FLOAT (REALPART object)))
					   (sxhash (FLOAT (IMAGPART object)))))
	    ((consp object)		(+ (equalp-hash (car object))
					   (equalp-hash (cdr object))))
	    ((STRINGP object)		(sxhash (STRING-UPCASE object)))
	    (t				(ERROR "TODO: equalp-hash")))))

      (defun el-test (fn)
	(cond
	  ((null fn)				'EQL)
	  ((eq fn 'EQ)				'eq)
	  ((eq fn (symbol-function 'EQ))	'eq)
	  ((eq fn 'EQL)			'EQL)
	  ((eq fn (symbol-function 'EQL))	'EQL)
	  ((eq fn 'EQUAL)			'EQUAL)
	  ((eq fn (symbol-function 'EQUAL))	'EQUAL)
	  ((eq fn 'EQUALP)			'EQUALP)
	  ((eq fn (symbol-function 'EQUALP))	'EQUALP)
	  (t
	   (ERROR "Unknown hash table test function"))))

      (defmacro htab (hash)
	hash)

      (cl:defmacro HTAB (hash)
	hash)

      (defun HASH-TABLE-P (object)
	(hash-table-p object))

      (defun HASH-TABLE-TEST (hash)
	(hash-table-test hash)))

    ;; If there isn't a real hash-table type, make one using defstruct.
    (progn
      (DEFSTRUCT (HASH-TABLE (:copier nil) (:constructor mkhash (TABLE TEST)))
        TABLE TEST)

      (cl:defun MAKE-HASH-TABLE (&KEY (TEST #'EQL) (SIZE 10) REHASH-SIZE
				 REHASH-THRESHOLD)
	(mkhash (make-hash-table :test TEST :size SIZE) TEST))

      (defun htab (hash)
	(HASH-TABLE-TABLE hash))

      (cl:defmacro HTAB (hash)
	`(HASH-TABLE-TABLE ,hash))))

(defun HASH-TABLE-COUNT (hash)
  (hash-table-count (htab hash)))

(defun HASH-TABLE-REHASH-SIZE (hash)
  ;; TODO
  0)

(defun HASH-TABLE-REHASH-THRESHOLD (hash)
  ;; TODO
  0)

(defun HASH-TABLE-SIZE (hash)
  ;; TODO
  0)

(defun GETHASH (key hash &optional default)
  (let ((object (gethash key (htab hash) not-found)))
    (if (eq object not-found)
	(cl:values default nil)
	(cl:values object T))))

(DEFINE-SETF-EXPANDER GETHASH (key hash &optional default)
  (with-gensyms (keytemp hashtemp val)
    (cl:values (list keytemp hashtemp)
	       (list key hash)
	       (list val)
	       `(puthash ,keytemp ,val (HTAB ,hashtemp))
	       `(GETHASH ,keytemp ,hashtemp))))

(defun REMHASH (key hash)
  (remhash key (htab hash)))

(defun MAPHASH (fn hash)
  (maphash (el-function fn) (htab hash))
  nil)

(defun hashlist (hash)
  (let ((list nil))
    (maphash (lambda (k v) (push (cons k v) list)) hash)
    list))

(cl:defmacro WITH-HASH-TABLE-ITERATOR ((name hash) &body body)
  (with-gensyms (list)
    `(LET ((,list (hashlist ,hash)))
       (MACROLET ((,name ()
		    (QUOTE (IF (NULL ,list) (cl:values nil nil nil)
			       (LET ((cons (POP ,list)))
				 (cl:values T (CAR cons) (CDR cons)))))))
	 ,@body))))

(defun CLRHASH (hash)
  (clrhash (htab hash))
  hash)

(if (eval-when-compile (fboundp 'sxhash))
    (fset 'SXHASH (symbol-function 'sxhash))
    (defun SXHASH (object) 42))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-hash.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-iteration.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 6, Iteration.

(IN-PACKAGE "EMACS-CL")

(defun var-inits (vars)
  (mapcar (lambda (var)
	    (if (symbolp var)
		var
		`(,(first var) ,(second var))))
	  vars))

(defun var-steps (vars)
  (mappend (lambda (var)
	     (when (and (consp var) (= (length var) 3))
	       `(,(first var) ,(third var))))
	   vars))

(defun expand-do (let setq vars test result forms)
  (with-gensyms (start)
    (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
      (let ((block `(TAGBODY
		      ,start
		      ,@(when test `((WHEN ,test (RETURN (PROGN ,@result)))))
		      ,@(if decls
		    	    `((LOCALLY (DECLARE ,@decls) ,@body))
		    	    body)
		      ,@(when vars `((,setq ,@(var-steps vars))))
		      (GO ,start))))
	`(BLOCK nil
	   ,(cond
	     (vars	`(,let ,(var-inits vars)
			   ,@(when decls `((DECLARE ,@decls))) ,block))
	     (decls	`(LOCALLY (DECLARE ,@decls) ,block))
	     (t		block)))))))

(cl:defmacro DO (vars (test &rest result) &body body)
  (expand-do 'LET 'PSETQ vars test result body))

(cl:defmacro DO* (vars (test &rest result) &body body)
  (expand-do 'LET* 'SETQ vars test result body))

(cl:defmacro DOTIMES ((var count &optional result) &body body)
  (with-gensyms (end)
    `(DO ((,var 0 (,(INTERN "1+" "CL") ,var))
	  (,end ,count))
         ((,(INTERN ">=" *cl-package*) ,var ,end)
	  (LET ((,var (MAX ,count 0)))
	    ,result))
       ,@body)))

(cl:defmacro DOLIST ((var list &optional result) &body forms)
  (with-gensyms (glist)
    (MULTIPLE-VALUE-BIND (body decls) (parse-body forms)
      `(DO (,var
	    (,glist ,list (CDR ,glist)))
	  ((NULL ,glist)
	   (LET ((,var nil))
	     ,result))
	 (DECLARE ,@decls)
	 (SETQ ,var (CAR ,glist))
	 ,@body))))

;;; LOOP and LOOP-FINISH are implemented in cl-loop.el.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-iteration.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-strings.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 16, Strings.

(IN-PACKAGE "EMACS-CL")

;;; System Class STRING

;;; Type BASE-STRING

;;; Type SIMPLE-STRING

;;; Type SIMPLE-BASE-STRING

(defun SIMPLE-STRING-P (object)
  (stringp object))

(defun CHAR (string index)
  (cond
    ((SIMPLE-STRING-P string)
     (SCHAR string index))
    ((STRINGP string)
     (SCHAR (aref string 2) index))
    (t
     (type-error string 'STRING))))

(defsetf CHAR (string index) (char)
  `(cond
    ((SIMPLE-STRING-P ,string)
     (setf (SCHAR ,string ,index) ,char))
    ((STRINGP ,string)
     (setf (SCHAR (aref ,string 2) ,index) ,char))
    (t
     (type-error ,string 'STRING))))

(DEFINE-SETF-EXPANDER CHAR (string index)
  (let ((obj (gensym))
	(stemp (gensym))
	(itemp (gensym)))
    (cl:values (list stemp itemp)
	       (list string index)
	       (list obj)
	       `(SET-CHAR ,obj ,stemp ,itemp)
	       `(CHAR ,stemp ,itemp))))

(defun SET-CHAR (char string index)
  (cond
    ((SIMPLE-STRING-P string)
     (SET-SCHAR char string index))
    ((STRINGP string)
     (SET-SCHAR char (aref string 2) index))
    (t
     (type-error string 'STRING))))

(if use-character-type-p
    (defun SCHAR (string index)
      (aref string index))
    (defun SCHAR (string index)
      (CODE-CHAR (aref string index))))

(if use-character-type-p
    (defsetf SCHAR (string index) (char)
      `(aset ,string ,index ,char))
    (defsetf SCHAR (string index) (char)
      (let ((temp (gensym)))
	`(let ((,temp ,char))
	   (aset ,string ,index (CHAR-CODE ,temp))
	   ,temp))))
	

(DEFINE-SETF-EXPANDER SCHAR (string index)
  (let ((obj (gensym))
	(stemp (gensym))
	(itemp (gensym)))
    (cl:values (list stemp itemp)
	       (list string index)
	       (list obj)
	       `(SET-SCHAR ,obj ,stemp ,itemp)
	       `(SCHAR ,stemp ,itemp))))

(if use-character-type-p
    (defun SET-SCHAR (char string index)
      (aset string index char))
    (defun SET-SCHAR (char string index)
      (aset string index (CHAR-CODE char))
      char))

(defun STRING (x)
  (cond
    ((STRINGP x)	x)
    ((SYMBOLP x)	(SYMBOL-NAME x))
    ((CHARACTERP x)	(MAKE-STRING 1 (kw INITIAL-ELEMENT) x))
    (t			(type-error x '(OR STRING SYMBOL CHARACTER)))))

(cl:defun STRING-UPCASE (string &KEY (START 0) END)
  (NSTRING-UPCASE (COPY-SEQ (STRING string)) (kw START) START (kw END) END))

(cl:defun STRING-DOWNCASE (string &KEY (START 0) END)
  (NSTRING-DOWNCASE (COPY-SEQ (STRING string)) (kw START) START (kw END) END))

(cl:defun STRING-CAPITALIZE (string &KEY (START 0) (END (LENGTH string)))
  (NSTRING-CAPITALIZE (COPY-SEQ (STRING string))
		      (kw START) START (kw END) END))

(cl:defun NSTRING-UPCASE (string &KEY (START 0) END)
  (setq string (STRING string))
  (unless END
    (setq END (LENGTH string)))
  (do ((i START (1+ i)))
      ((eq i END) string)
    (setf (CHAR string i) (CHAR-UPCASE (CHAR string i)))))

(cl:defun NSTRING-DOWNCASE (string &KEY (START 0) END)
  (setq string (STRING string))
  (unless END
    (setq END (LENGTH string)))
  (do ((i START (1+ i)))
      ((eq i END) string)
    (setf (CHAR string i) (CHAR-DOWNCASE (CHAR string i)))))

(cl:defun NSTRING-CAPITALIZE (string &KEY (START 0) (END (LENGTH string)))
  (setq string (STRING string))
  (do* ((i START (1+ i))
	(in-word-p nil))
       ((eq i END)
	string)
    (let* ((char (CHAR string i))
	   (alnump (ALPHANUMERICP char)))
      (when alnump
	(setf (CHAR string i)
	      (if in-word-p (CHAR-DOWNCASE char) (CHAR-UPCASE char))))
      (setq in-word-p alnump))))

(defun STRING-TRIM (chars string)
  (STRING-LEFT-TRIM chars (STRING-RIGHT-TRIM chars string)))

(defun STRING-LEFT-TRIM (chars string)
  (setq string (STRING string))
  (let ((i 0)
	(len (LENGTH string)))
    (while (and (< i len) (FIND (CHAR string i) chars))
      (incf i))
    (SUBSEQ string i)))

(defun STRING-RIGHT-TRIM (chars string)
  (setq string (STRING string))
  (let* ((i (1- (LENGTH string))))
    (while (and (>= i 0) (FIND (CHAR string i) chars))
      (decf i))
    (SUBSEQ string 0 (1+ i))))

(cl:defun STRING= (string1 string2 &KEY (START1 0) END1 (START2 0) END2)
  (setq string1 (STRING string1))
  (setq string2 (STRING string2))
  (string= (substring string1 START1 END1)
	   (substring string2 START2 END2)))

(cl:defun STRING/= (string1 string2 &KEY (START1 0) END1 (START2 0) END2)
  (not (STRING= string1 string2 (kw START1) START1 (kw END1) END1
				(kw START2) START2 (kw END2) END2)))

(defun cl-string-cmp (string1 string2)
  (let ((len1 (LENGTH string1))
	(len2 (LENGTH string2))
	(i 0))
    (loop
     (when (= i len1)
       (return (cl:values i (if (= i len2) 0 -1))))
     (when (= i len2)
       (return (cl:values i 1)))
     (let ((c1 (CHAR string1 i))
	   (c2 (CHAR string2 i)))
       (cond
	 ((CHAR< c1 c2) (return (cl:values i -1)))
	 ((CHAR> c1 c2) (return (cl:values i 1)))
	 (t (incf i)))))))

(cl:defun STRING< (string1 string2 &KEY (START1 0) END1 (START2 0) END2)
  (MULTIPLE-VALUE-BIND (index cmp)
      (cl-string-cmp (SUBSEQ (STRING string1) START1 END1)
		     (SUBSEQ (STRING string2) START2 END2))
    (when (minusp cmp)
      index)))

(cl:defun STRING> (string1 string2 &KEY (START1 0) END1 (START2 0) END2)
  (MULTIPLE-VALUE-BIND (index cmp)
      (cl-string-cmp (SUBSEQ (STRING string1) START1 END1)
		     (SUBSEQ (STRING string2) START2 END2))
    (when (plusp cmp)
      index)))

(cl:defun STRING<= (string1 string2 &KEY (START1 0) END1 (START2 0) END2)
  (MULTIPLE-VALUE-BIND (index cmp)
      (cl-string-cmp (SUBSEQ (STRING string1) START1 END1)
		     (SUBSEQ (STRING string2) START2 END2))
    (when (not (plusp cmp))
      index)))

(cl:defun STRING>= (string1 string2 &KEY (START1 0) END1 (START2 0) END2)
  (MULTIPLE-VALUE-BIND (index cmp)
      (cl-string-cmp (SUBSEQ (STRING string1) START1 END1)
		     (SUBSEQ (STRING string2) START2 END2))
    (when (not (minusp cmp))
      index)))

(cl:defun STRING-EQUAL (string1 string2 &KEY (START1 0) END1 (START2 0) END2)
  (string= (substring (STRING-UPCASE string1) START1 END1)
	   (substring (STRING-UPCASE string2) START2 END2)))

(cl:defun STRING-NOT-EQUAL (string1 string2 &KEY (START1 0) END1
			                         (START2 0) END2)
  (not (STRING-EQUAL string1 string2 (kw START1) START1 (kw END1) END1
				     (kw START2) START2 (kw END2) END2)))

(cl:defun STRING-LESSP (string1 string2 &KEY (START1 0) END1 (START2 0) END2)
  (STRING< (substring (STRING-UPCASE string1) START1 END1)
	   (substring (STRING-UPCASE string2) START1 END1)))

(cl:defun STRING-GREATERP (string1 string2 &KEY (START1 0) END1
						(START2 0) END2)
  (STRING> (substring (STRING-UPCASE string1) START1 END1)
	   (substring (STRING-UPCASE string2) START1 END1)))

(cl:defun STRING-NOT-GREATERP (string1 string2 &KEY (START1 0) END1
						    (START2 0) END2)
  (STRING<= (substring (STRING-UPCASE string1) START1 END1)
	    (substring (STRING-UPCASE string2) START1 END1)))

(cl:defun STRING-NOT-LESSP (string1 string2 &KEY (START1 0) END1
						 (START2 0) END2)
  (STRING>= (substring (STRING-UPCASE string1) START1 END1)
	    (substring (STRING-UPCASE string2) START1 END1)))

(defun STRINGP (object)
  (or (stringp object)
      (vector-and-typep object 'STRING)))

(if use-character-type-p
    (cl:defun MAKE-STRING (size &KEY INITIAL-ELEMENT ELEMENT-TYPE)
      (make-string size (or INITIAL-ELEMENT ?\000)))
    (cl:defun MAKE-STRING (size &KEY INITIAL-ELEMENT ELEMENT-TYPE)
      (make-string size (if INITIAL-ELEMENT (CHAR-CODE INITIAL-ELEMENT) 0))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-strings.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-symbols.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 10, Symbols.

(IN-PACKAGE "EMACS-CL")

;;; Note that the Emacs Lisp symbol nil doubles as the Common Lisp
;;; symbol NIL.  This requires special attention in SYMBOL-NAME.

;;; The SYMBOL system class is built in.

(fset 'SYMBOLP (symbol-function 'symbolp))

(defun KEYWORDP (sym)
  (and (SYMBOLP sym)
       (eq (SYMBOL-PACKAGE sym) *keyword-package*)))

(defun MAKE-SYMBOL (string)
  (unless (STRINGP string)
    (type-error string 'STRING))
  (make-symbol string))

(defun COPY-SYMBOL (sym &optional copy-properties)
  (let ((new (make-symbol (SYMBOL-NAME sym))))
    (when copy-properties
      (when (boundp sym)
	(setf (symbol-value new) (symbol-value sym)))
      (when (fboundp sym)
	(setf (symbol-function new) (symbol-function sym)))
      (setf (symbol-plist new) (copy-list (symbol-plist sym))))
    new))

(cl:defun GENSYM (&OPTIONAL (x "G"))
  (multiple-value-bind (prefix suffix)
      (cond
	((STRINGP x)	(values x (prog1 *GENSYM-COUNTER*
				      (setq *GENSYM-COUNTER*
					    (binary+ *GENSYM-COUNTER* 1)))))
	((INTEGERP x)	(values "G" x))
	(t		(type-error x '(OR STRING INTEGER))))
    (MAKE-SYMBOL (FORMAT nil "~A~D" prefix suffix))))

(DEFVAR *GENSYM-COUNTER* 1)

(defvar *gentemp-counter* 1)

(cl:defun GENTEMP (&OPTIONAL (prefix "T") (package *PACKAGE*))
  (catch 'GENTEMP
    (loop
      (MULTIPLE-VALUE-BIND (symbol found)
	  (INTERN (FORMAT nil "~A~D" prefix *gentemp-counter*) package)
	(unless found
	  (throw 'GENTEMP (cl:values symbol)))
	(incf *gentemp-counter*)))))

; (defun SYMBOL-FUNCTION (symbol)
;   (unless (symbolp symbol)
;     (type-error symbol 'SYMBOL))
;   (unless (fboundp symbol)
;     (ERROR 'UNDEFINED-FUNCTION (kw NAME) symbol))
;   (symbol-function symbol))

; (DEFSETF SYMBOL-FUNCTION (symbol) (fn)
;   `(fset ,symbol ,fn))

(defun SYMBOL-FUNCTION (symbol)
  (unless (symbolp symbol)
    (type-error symbol 'SYMBOL))
  (unless (fboundp symbol)
    (ERROR 'UNDEFINED-FUNCTION (kw NAME) symbol))
  (let ((fn (symbol-function symbol)))
    (cond
      ((and (consp fn)
	    (eq (car fn) 'macro))
       nil)
      ((and (consp fn)
	    (consp (third fn))
	    (eq (first (third fn)) 'APPLY))
       (let ((ifn (second (third fn))))
	 (if (INTERPRETED-FUNCTION-P ifn) ifn fn)))
      ((and (consp fn)
	    (consp (fourth fn))
	    (eq (first (fourth fn)) 'APPLY))
       (let ((ifn (second (fourth fn))))
	 (if (INTERPRETED-FUNCTION-P ifn) ifn fn)))
      (t fn))))

(defsetf SYMBOL-FUNCTION set-symbol-function)

(DEFSETF SYMBOL-FUNCTION set-symbol-function)

(defun interactive-stuff (forms)
  (some (lambda (form)
	  (and (consp form)
	       (eq (car form) 'DECLARE)
	       (consp (cdr form))
	       (or (when (eq (cadr form) 'INTERACTIVE)
		     '((interactive)))
		   (when (and (consp (cadr form))
			      (eq (caadr form) 'INTERACTIVE))
		     `((interactive ,@(cdadr form)))))))
	forms))

(defun el-function (fn)
  (if (vectorp fn)
      `(lambda (&rest args)
	,@(interactive-stuff
	   (cddr (cl:values (FUNCTION-LAMBDA-EXPRESSION fn))))
	(APPLY ,fn args))
      fn))

(defun set-symbol-function (symbol fn)
  (fset symbol
	(cond
	  ((INTERPRETED-FUNCTION-P fn)	(el-function fn))
	  ((FUNCTIONP fn)		fn)
	  (t				(type-error fn 'FUNCTION)))))

(defun SYMBOL-NAME (symbol)
  (if symbol
      (symbol-name symbol)
      "NIL"))

(defvar *symbol-package-table* (make-hash-table :test 'eq :weakness t))

(defun SYMBOL-PACKAGE (sym)
  (or (gethash sym *symbol-package-table*)
      (when (interned-p sym) *emacs-lisp-package*)))

(defsetf SYMBOL-PACKAGE (sym) (package)
  `(if (null ,package)
       (progn (remhash ,sym *symbol-package-table*) ,package)
       (setf (gethash ,sym *symbol-package-table*) ,package)))

(fset 'SYMBOL-PLIST (symbol-function 'symbol-plist))

(DEFSETF SYMBOL-PLIST (symbol) (plist)
  `(setplist ,symbol ,plist))

(fset 'SYMBOL-VALUE (symbol-function 'symbol-value))

(defsetf SYMBOL-VALUE (symbol) (val)
  `(set ,symbol ,val))

(DEFSETF SYMBOL-VALUE (symbol) (val)
  `(SET ,symbol ,val))

(defun GET (symbol property &optional default)
  (let ((val (member property (symbol-plist symbol))))
    (if val
	(cadr val)
	default)))

(DEFSETF GET (symbol property &optional default) (val)
  `(put ,symbol ,property ,val))

(defun REMPROP (symbol indicator)
  (setplist symbol (delete-property (symbol-plist symbol) indicator)))

(defun BOUNDP (symbol)
  (unless (symbolp symbol)
    (type-error symbol 'SYMBOL))
  (boundp symbol))

(defun MAKUNBOUND (symbol)
  (unless (symbolp symbol)
    (type-error symbol 'SYMBOL))
  (makunbound symbol))

(fset 'SET (symbol-function 'set))

;;; UNBOUND-VARIABLE in cl-conditions.el.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-symbols.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-system.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 24, System Construction.

(IN-PACKAGE "EMACS-CL")

;;; COMPILE-FILE is defined in cl-compile.el.

(defun elc-file (filename)
  (MERGE-PATHNAMES (MAKE-PATHNAME (kw TYPE) "elc") filename))

(cl:defun COMPILE-FILE-PATHNAME (input-file
				 &KEY (OUTPUT-FILE (elc-file input-file))
				 &ALLOW-OTHER-KEYS)
  (let* ((input (MERGE-PATHNAMES input-file)))
    (MERGE-PATHNAMES OUTPUT-FILE input)))

(defun file-has-elc-magic-p (pathname)
  (WITH-OPEN-FILE (stream pathname)
    (let ((magic (make-string 4 0)))
      (and (eq (READ-SEQUENCE magic stream) 4)
	   (STRING= magic ";ELC")))))

(cl:defun LOAD (file &KEY (VERBOSE *LOAD-VERBOSE*)
		     	  (PRINT *LOAD-PRINT*)
		     	  (IF-DOES-NOT-EXIST T)
		     	  (EXTERNAL-FORMAT (kw DEFAULT)))
  (let* ((*PACKAGE* *PACKAGE*)
	 (*READTABLE* *READTABLE*)
	 (*LOAD-PATHNAME* (MERGE-PATHNAMES file))
	 (*LOAD-TRUENAME* (TRUENAME *LOAD-PATHNAME*)))
    (cond
      ((STREAMP file)
       (let ((eof (gensym)))
	 (do ((form (READ file nil eof) (READ file nil eof)))
	     ((eq form eof)
	      (cl:values T))
	   (let ((val (EVAL form)))
	     (when PRINT
	       (PRINT val))))))
      ((or (STRINGP file) (PATHNAMEP file))
       (when VERBOSE
	 (FORMAT T "~&;Loading ~A" (NAMESTRING *LOAD-PATHNAME*)))
       (if (or (STRING= (PATHNAME-TYPE *LOAD-PATHNAME*) "elc")
	       (file-has-elc-magic-p *LOAD-PATHNAME*))
	   (load (NAMESTRING *LOAD-PATHNAME*))
	   (WITH-OPEN-FILE (stream *LOAD-PATHNAME*)
	     (LOAD stream (kw PRINT) PRINT)))
       (cl:values T))
      (t
       (type-error file '(OR PATHNAME STRING STREAM))))))

(DEFVAR *compilation-unit* nil)
(DEFVAR *deferred-compilation-actions* nil)

(cl:defmacro WITH-COMPILATION-UNIT ((&key OVERRIDE) &body body)
  `(PROGN
     (LET ((*compilation-unit* T))
       ,@body)
     (WHEN (OR ,OVERRIDE (NOT *compilation-unit*))
       (DOLIST (fn (NREVERSE *deferred-compilation-actions*))
	 (FUNCALL fn))
       (SETQ *deferred-compilation-actions* nil))))

(defmacro* WITH-COMPILATION-UNIT ((&key OVERRIDE) &body body)
  `(progn
     (let ((*compilation-unit* T))
       ,@body)
     (when (or ,OVERRIDE (not *compilation-unit*))
       (dolist (fn (nreverse *deferred-compilation-actions*))
	 (FUNCALL fn))
       (setq *deferred-compilation-actions* nil))))

(DEFVAR *FEATURES* (list ;; TODO: (kw ANSI-CL)
			 (kw EMACS-CL)
		         (kw COMMON-LISP)))

(let ((cons (ASSOC (emacs-version)
		   `(("GNU Emacs" .	,(kw GNU-EMACS))
		     ("XEmacs" .	,(kw XEMACS))
		     ("Hemlock" .	,(kw HEMLOCK)))
		   (kw TEST) (lambda (version string)
			       (STRING= version string
					(kw END1) (LENGTH string))))))
  (push (if cons (cdr cons) (kw UNKNOWN-EMACS)) *FEATURES*))

(DEFVAR *COMPILE-FILE-PATHNAME* nil)
(DEFVAR *COMPILE-FILE-TRUENAME* nil)

(DEFVAR *LOAD-PATHNAME* nil)
(DEFVAR *LOAD-TRUENAME* nil)

(DEFVAR *COMPILE-PRINT* nil)
(DEFVAR *COMPILE-VERBOSE* nil)

(DEFVAR *LOAD-PRINT* nil)
(DEFVAR *LOAD-VERBOSE* nil)

(DEFVAR *MODULES* nil)

(defun PROVIDE (name)
  (let ((string (STRING name)))
    (pushnew string *MODULES* :test #'STRING=)
    string))

(defun REQUIRE (name &optional pathnames)
  (let ((string (STRING name)))
    (unless (find string *MODULES* :test #'STRING=)
      (do-list-designator (file pathnames)
	(LOAD file)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-system.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-typep.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements the TYPEP function from chapter 4, Types and Classes.

(IN-PACKAGE "EMACS-CL")

(defvar *atomic-typespecs* (make-hash-table))
(defvar *compound-typespecs* (make-hash-table))

(defun ensure-type (name predicate)
  (setf (gethash name *atomic-typespecs*) predicate))

;;; Implements TYPEP for "typespec".
(defmacro* define-typep ((var typespec env &optional compound-only) &body body)
  (if (consp typespec)
      `(progn
	 (setf (gethash ',(first typespec) *compound-typespecs*)
	       (function* (lambda (,var ,env ,@(rest typespec)) ,@body)))
	,@(unless compound-only
	   `((ensure-type ',(first typespec)
	                  (function* (lambda (,var ,env ,@(rest typespec))
			               ,@body))))))
      `(ensure-type ',typespec (function* (lambda (,var ,env) ,@body)))))

(defun in-range (num low high)
  "Check that NUM is in the range specified by the interval designators
   LOW and HIGH."
  (let* ((low-exclusive (consp low))
	 (low (if low-exclusive (car low) low))
	 (high-exclusive (consp high))
	 (high (if high-exclusive (car high) high)))
    (and (cond
	   ((eq low star) t)
	   (low-exclusive (cl:< low num))
	   (t (cl:<= low num)))
	 (cond
	   ((eq high star) t)
	   (high-exclusive (cl:< num high))
	   (t (cl:<= num high))))))

(defvar star (INTERN "*" "EMACS-CL"))

(defmacro star-or (type &rest forms)
  `(or (eq ,type star) ,@forms))


;;; Definitions for all type specifiers recognized by TYPEP follows.

(define-typep (object (AND &rest types) env :compound-only)
  (every (lambda (type) (TYPEP object type env)) types))

(define-typep (object (ARRAY &optional (type star) (dims star)) env)
  (and (ARRAYP object)
       (star-or type (eq (UPGRADED-ARRAY-ELEMENT-TYPE type)
			 (ARRAY-ELEMENT-TYPE object)))
       (cond
	 ((eq dims star)	'T)
	 ((INTEGERP dims)	(eql (ARRAY-RANK object) dims))
	 (t			(dims-match dims (ARRAY-DIMENSIONS object))))))

(defun dims-match (d1 d2)
  (cond
    ((null d1)	(null d2))
    ((null d2)	nil)
    (t		(and (or (eq (first d1) star)
			 (eq (first d2) star)
			 (eql (first d1) (first d2)))
		     (dims-match (rest d1) (rest d2))))))

(define-typep (object CHARACTER env)
  (CHARACTERP object))

(define-typep (object COMPILED-FUNCTION env)
  (COMPILED-FUNCTION-P object))

(define-typep (object (COMPLEX &optional (type star)) env)
  (and (COMPLEXP object)
       (star-or
	type
	(unless (cl:values (SUBTYPEP type 'REAL))
	  (ERROR "(COMPLEX ~S) is not a valid type specifier." type))
	'T)))

(define-typep (object (CONS &optional (car-type star) (cdr-type star)) env)
  (and (consp object)
       (star-or car-type (TYPEP (car object) car-type env))
       (star-or cdr-type (TYPEP (cdr object) cdr-type env))))

(define-typep (obj1 (EQL obj2) env :compound-only)
  (EQL obj1 obj2))

(define-typep (object FUNCTION env)
  (FUNCTIONP object))

(define-typep (object (FUNCTION &rest args) env :compound-only)
  (ERROR "TYPEP does not accept a compound FUNCTION type specifier."))

(define-typep (object HASH-TABLE env)
  (HASH-TABLE-P object))

(define-typep (object (INTEGER &optional (low star) (high star)) env)
  (and (INTEGERP object) (in-range object low high)))

(define-typep (object INTERPRETED-FUNCTION env)
  (INTERPRETED-FUNCTION-P object))

(define-typep (object KEYWORD env)
  (KEYWORDP object))

(define-typep (object LOGICAL-PATHNAME env)
  (vector-and-typep object 'LOGICAL-PATHNAME))

(define-typep (object nil env)
  nil)

(define-typep (object (NOT type) env :compound-only)
  (not (TYPEP object type env)))

(define-typep (object (OR &rest types) env :compound-only)
  (some (lambda (type) (TYPEP object type env)) types))

(define-typep (object PACKAGE env)
  (PACKAGEP object))

(define-typep (object PATHNAME env)
  (PATHNAMEP object))

(define-typep (object RANDOM-STATE env)
  (RANDOM-STATE-P object))

(define-typep (object (RATIONAL &optional (low star) (high star)) env)
  (and (RATIONALP object) (in-range object low high)))

(define-typep (object (SATISFIES fn) env :compound-only)
  (unless (symbolp fn)
    (type-error fn '(CONS (EQL SATISFIES) (CONS SYMBOL NULL))))
  (funcall fn object))

(define-typep (object (SIMPLE-ARRAY &optional (type star) (dims star)) env)
  (and (or (bit-vector-p object)
	   (stringp object)
	   (SIMPLE-VECTOR-P object))
       (star-or type
		(eq (UPGRADED-ARRAY-ELEMENT-TYPE type)
		    (ARRAY-ELEMENT-TYPE object)))
       (star-or dims
		(eql dims 1)
		(equal dims (list star)))))

(define-typep (object (SINGLE-FLOAT &optional (low star) (high star)) env)
  (and (floatp object) (in-range object low high)))

(define-typep (object STANDARD-CHAR env)
  (and (CHARACTERP object)
       (STANDARD-CHAR-P object)))

(define-typep (object SYMBOL env)
  (SYMBOLP object))

(define-typep (object T env)
  T)

(define-typep (object (VALUES &rest args) env :compound-only)
  (ERROR "TYPEP does not accept a VALUES type specifier."))



(defun TYPEP (object type &optional env)
  (setq type (expand-type type env))
  (cond
    ((consp type)
     (let ((fn (gethash (first type) *compound-typespecs*)))
       (if fn
	   (APPLY fn object env (rest type))
	   (error "invalid typespec: %s" type))))
    ((symbolp type)
     (let ((fn (gethash type *atomic-typespecs*)))
       (if fn
	   (FUNCALL fn object env)
	   (ERROR "Invalid typespec: ~A" type))))
    (t
     (type-error type '(OR SYMBOL CONS CLASS)))))

;;; Bootstrap issue.  Redefined later.
(defun INTERPRETED-FUNCTION-P (fn)
  nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-typep.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./cl-types.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file implements operators in chapter 4, Types and Classes.

(IN-PACKAGE "EMACS-CL")

;;; TODO: GENERIC-FUNCTION
;;; TODO: STANDARD-GENERIC-FUNCTION
;;; TODO: CLASS
;;; TODO: BUILT-IN-CLASS
;;; TODO: STRUCTURE-CLASS
;;; TODO: STANDARD-CLASS
;;; TODO: METHOD
;;; TODO: STANDARD-METHOD
;;; TODO: STRUCTURE-OBJECT
;;; TODO: STANDARD-OBJECT
;;; TODO: METHOD-COMBINATION

(unless (fboundp 'puthash)
  (defun puthash (key value table)
    (setf (gethash key table) value)))

(defun COERCE (object type)
  (cond
    ((or (eq type 'T) (TYPEP object type))
     object)
    ((null type)
     (ERROR 'SIMPLE-TYPE-ERROR
	    (kw format) "~S can't be coerced to type ~S."
	    (kw args) (list object type)))
    ((cl:values (SUBTYPEP type 'SEQUENCE))
     (when (consp type)
       (let ((n (second type)))
	 (when (and (eq (first type) 'ARRAY)
		    (listp n))
	   (unless (eql (length n) 1)
	     (ERROR 'TYPE-ERROR))
	   (setq n (first n)))
	 (unless (or (eq n star)
		     (eql n (LENGTH object)))
	   (ERROR 'TYPE-ERROR))))
     (MAP type #'IDENTITY object))
    ((eq type 'CHARACTER)
     (CHARACTER object))
    ((eq type 'COMPLEX)
     (cond
       ((RATIONALP object)	object)
       ((FLOATP object)		(COMPLEX object 0.0))
       (t			(type-error object 'NUMBER))))
    ((cl:values (SUBTYPEP type 'FLOAT))
     (FLOAT object))
    ((eq type 'FUNCTION)
     (if (lambda-expr-p object)
	 (cl:values (COMPILE nil object))
	 (FDEFINITION object)))
    (t
     (ERROR 'SIMPLE-TYPE-ERROR
	    (kw format) "~S can't be coerced to type ~S."
	    (kw args) (list object type)))))

(defvar *deftype-expanders* (make-hash-table))

(defmacro* cl:deftype (name lambda-list &body body)
  `(progn
     (puthash ',name
	      ,(make-macro-el-function name lambda-list body)
	      *deftype-expanders*)
     ',name))

(cl:defmacro DEFTYPE (name lambda-list &body body &environment env)
  `(EVAL-WHEN (,(kw COMPILE-TOPLEVEL) ,(kw LOAD-TOPLEVEL) ,(kw EXECUTE))
     (puthash (QUOTE ,name)
	      ,(make-macro-function name lambda-list body env)
	      *deftype-expanders*)
     (QUOTE ,name)))

;;; Redefined later.
(defun classp (x)
  nil)

(defun expand-type (orig-type env)
  (if (classp orig-type)
      (CLASS-NAME orig-type)
      (let* ((type (ensure-list orig-type))
	     (fn (gethash (first type) *deftype-expanders*)))
	(if fn
	    (expand-type (FUNCALL fn type env) env)
	    orig-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl:deftype ATOM ()
  '(NOT CONS))

(cl:deftype BASE-CHAR ()
  'CHARACTER)

(cl:deftype BASE-STRING (&optional n)
  (unless n (setq n star))
  `(STRING ,n))

(cl:deftype BIGNUM ()
  '(AND INTEGER (NOT FIXNUM)))

(cl:deftype BIT ()
  '(INTEGER 0 1))

(cl:deftype BIT-VECTOR (&optional n)
  (unless n (setq n star))
  `(ARRAY BIT (,n)))

(cl:deftype BOOLEAN ()
  '(MEMBER nil T))

(cl:deftype DOUBLE-FLOAT (&optional m n)
  (unless m (setq m star))
  (unless n (setq n star))
  `(SINGLE-FLOAT ,m ,n))

(cl:deftype EXTENDED-CHAR ()
  '(AND CHARACTER (NOT BASE-CHAR)))

(cl:deftype FIXNUM ()
  (eval-when-compile `(INTEGER ,most-negative-fixnum ,most-positive-fixnum)))

(cl:deftype FLOAT (&optional m n)
  (unless m (setq m star))
  (unless n (setq n star))
  `(SINGLE-FLOAT ,m ,n))

(cl:deftype LIST ()
  '(OR NULL CONS))

(cl:deftype LONG-FLOAT (&optional m n)
  (unless m (setq m star))
  (unless n (setq n star))
  `(SINGLE-FLOAT ,m ,n))

(cl:deftype MEMBER (&rest objects)
  `(OR ,@(mapcar (curry #'list 'EQL) objects)))

(cl:deftype MOD (n)
  `(INTEGER 0 ,(binary+ n -1)))

(cl:deftype NULL ()
  '(EQL nil))

(cl:deftype NUMBER ()
  '(OR REAL COMPLEX))

(cl:deftype RATIO ()
  '(AND RATIONAL (NOT INTEGER)))

(cl:deftype REAL (&optional m n)
  (unless m (setq m star))
  (unless n (setq n star))
  `(OR (RATIONAL ,m ,n) (SINGLE-FLOAT ,m ,n)))

(cl:deftype SEQUENCE ()
  '(OR LIST VECTOR))

(cl:deftype SHORT-FLOAT (&optional m n)
  (unless m (setq m star))
  (unless n (setq n star))
  `(SINGLE-FLOAT ,m ,n))

(cl:deftype SIGNED-BYTE (&optional n)
  (unless n (setq n star))
  (let ((m n))
    (unless (eq n star)
      (setq n (EXPT 2 (cl:1- n)))
      (setq m (cl:- n))
      (setq n (cl:1- n)))
    `(INTEGER ,m ,n)))

(cl:deftype SIMPLE-BASE-STRING (&optional n)
  (unless n (setq n star))
  `(SIMPLE-STRING (,n)))

(cl:deftype SIMPLE-BIT-VECTOR (&optional n)
  (unless n (setq n star))
  `(SIMPLE-ARRAY BIT (,n)))

(cl:deftype SIMPLE-STRING (&optional n)
  (unless n (setq n star))
  `(OR (SIMPLE-ARRAY CHARACTER (,n))
       (SIMPLE-ARRAY nil (,n))))

(cl:deftype SIMPLE-VECTOR (&optional n)
  (unless n (setq n star))
  `(SIMPLE-ARRAY T (,n)))

(cl:deftype STRING (&optional n)
  (unless n (setq n star))
  `(OR (ARRAY CHARACTER (,n))
       (ARRAY nil (,n))))

(cl:deftype UNSIGNED-BYTE (&optional n)
  (unless n (setq n star))
  (unless (eq n star)
    (setq n (cl:1- (EXPT 2 n))))
  `(INTEGER 0 ,n))

(cl:deftype VECTOR (&whole w &optional type n)
  (when (null (rest w)) (setq type star))
  (unless n (setq n star))
  `(ARRAY ,type (,n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun TYPE-OF (object)
  (case object
    ((nil)		'NULL)
    (T			'BOOLEAN)
    ((0 1)		'BIT)
    (t
     (ecase (type-of object)
       ;; This is supposed to be an exhaustive enumeration of all
       ;; possible return values for Emacs Lisp type-of.
       ((bit-vector bool-vector)
			`(SIMPLE-BIT-VECTOR ,(length object)))
       (subr
			'COMPILED-FUNCTION)
       (compiled-function
			(let ((info (gethash object *funcallable-objects*)))
			  (if info
			      (car info)
			      'COMPILED-FUNCTION)))
       (character	'CHARACTER)
       (cons		'CONS)
       (float		'SINGLE-FLOAT)
       (hash-table	'HASH-TABLE)
       (integer		(if (minusp object)
			    'FIXNUM
			    `(INTEGER 0 ,MOST-POSITIVE-FIXNUM)))
       (string		`(SIMPLE-STRING ,(length object)))
       (symbol		(if (eq (SYMBOL-PACKAGE object) *keyword-package*)
			    'KEYWORD
			    'SYMBOL))
       (vector
	(case (aref object 0)
	  (ARRAY	`(ARRAY T ,(array-dims object)))
	  (BIGNUM	(if (MINUSP object)
			    'BIGNUM
			    `(INTEGER ,(binary+ MOST-POSITIVE-FIXNUM 1))))
	  (bit-array	`(ARRAY BIT ,(array-dims object)))
	  (BIT-VECTOR	`(BIT-VECTOR ,(vector-size object)))
	  (char-array	`(ARRAY CHARACTER ,(array-dims object)))
	  (CHARACTER	'CHARACTER)
	  (COMPLEX	'COMPLEX)
	  (INTERPRETED-FUNCTION
			'INTERPRETED-FUNCTION)
	  (RATIO	'RATIO)
	  (SIMPLE-VECTOR
			`(SIMPLE-VECTOR ,(1- (length object))))
	  (STRING	`(STRING ,(vector-size object)))
	  (VECTOR	`(VECTOR T ,(vector-size object)))
	  (t		(aref object 0))))
       ;; For now, throw an error on these.
       ((buffer char-table frame marker overlay process
	 subr window window-configuration)
			(error "Unknown type: %s" (type-of object)))))))

;;; TYPEP defined in cl-typep.el.

;;; TYPE-ERROR, TYPE-ERROR-DATUM, TYPE-ERROR-EXPECTED-TYPE, and
;;; SIMPLE-TYPE-ERROR defined in cl-conditions.el.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./cl-types.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./func.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file provides cl:lambda, cl:function, and cl:defun.

(defmacro cl:function (name)
  (let ((fn (FDEFINITION name)))
    (if (subrp fn)
	;; Have to return symbol since #<subr ...> in .elc isn't readable.
	`',name
	fn)))

(defmacro cl:defun (name lambda-list &rest body)
  (when byte-compile-warnings
    (byte-compile-log-1 (format "cl:defun %s" name)))
  `(progn
     (fset ',name (cl:lambda ,lambda-list ,@body))
     ',name))

(defmacro parse-parameter (p kind k v i s err)
  `(cond
    ((or (symbolp ,p) (not (memq ,kind '(&OPTIONAL &KEY &AUX))))
     (when (and ',k (eq ,kind '&KEY) (not (listp ,p)))
       (setq ,k (keyword (symbol-name ,p))))
     (when ',v (setq ,v ,p)))
    ((and (consp ,p) (<= (length ,p) 3))
     (cond
       ((or (symbolp (first ,p)) (not (eq ,kind '&KEY)))
	(when (and ',k (eq ,kind '&KEY))
	  (setq ,k (keyword (symbol-name (first ,p)))))
	(when ',v (setq ,v (first ,p))))
       ((and (consp (first ,p)) (= (length (first ,p)) 2))
	(when ',k (setq ,k (first (first ,p))))
	(when ',v (setq ,v (second (first ,p)))))
       (t
	,err))
     (when ',i (setq ,i (second ,p)))
     (when ',s (setq ,s (third ,p))))
    (t
     ,err)))

(defmacro* do-lambda-list ((var kind lambda-list &optional result
			    &key (keywords 'LAMBDA-LIST-KEYWORDS))
			   &body body)
  (let ((keyword-var nil)
	(var-var nil)
	(default-var nil)
	(supplied-var nil)
	(v (gensym)))
    (parse-parameter var '&KEY keyword-var var-var default-var supplied-var
		     (error "Syntax error in do-lambda-list."))
    (with-gensyms (list)
      `(do ((,kind :required)
	    (,list ,lambda-list (rest ,list)))
	   ((atom ,list)
	    (unless (null ,list)
	      (setq ,kind '&REST)
	      (let (,@(when keyword-var `((,keyword-var nil)))
		    ,@(when var-var `((,var-var ,list)))
		    ,@(when default-var `((,default-var nil)))
		    ,@(when supplied-var `((,supplied-var nil))))
		,@body))
	    ,result)
	 (let ((,v (car ,list)))
	   (if (memq ,v ,keywords)
	       (setq ,kind ,v)
	       (let ,(remove nil (list var-var keyword-var
				       default-var supplied-var))
		 (parse-parameter ,v ,kind ,keyword-var ,var-var ,default-var
				  ,supplied-var (ERROR 'PROGRAM-ERROR))
		 ,@body
		 (when (memq ,kind '(&ENVIRONMENT &WHOLE))
		   (setq ,kind :required)))))))))

;;; Allowed lambda list keywords:
;;; Ordinary		&optional &rest &key &allow-other-keys &aux
;;; Generic Function	&optional &rest &key &allow-other-keys
;;; Specialized		&optional &rest &key &allow-other-keys &aux
;;; Macro		&whole &optional &rest &body &key &allow-other-keys
;;;			&aux &environment
;;; Destructuring	&whole &optional &rest &body &key &allow-other-keys
;;;			&aux
;;; Boa			Same as Ordinary.
;;; Defsetf		&optional &rest &key &allow-other-keys &environment
;;; Deftype		Same as Macro.
;;; Define-modify-macro	&optional &rest
;;; Define-method-combination
;;;			&whole &optional &rest &key &allow-other-keys &aux

(defvar rest-sym (make-symbol "rest"))

(defvar unbound (make-symbol "unbound"))

(defun* simplify-lambda-list (lambda-list &optional env)
  (let ((result nil)
	(push-optional t))
    (do-lambda-list ((var default supp) kind lambda-list)
      (case kind
        (&OPTIONAL
         (when push-optional
	   (push '&optional result)
	   (setq push-optional nil)))
        ((&REST &KEY)
         (push '&rest result)
         (push rest-sym result)
         (return-from simplify-lambda-list (nreverse result)))
	(&AUX
         (return-from simplify-lambda-list (nreverse result))))
      (when (or default supp)
         (when (eq (car result) '&optional)
           (pop result))
         (push '&rest result)
         (push rest-sym result)
         (return-from simplify-lambda-list (nreverse result)))
      (push (if env (compile-variable var env) var)
	    result))
    (nreverse result)))

(defun* lambda-list-bindings (lambda-list env)
  (let ((bindings nil)
	(optional-rest nil))
    (do-lambda-list ((var default supp) kind lambda-list)
      (when env
	(setq var (compile-variable var env))
	(when supp
	  (setq supp (compile-variable supp env))))
      (case kind
	(&OPTIONAL
	 (when (or default supp)
	   (setq optional-rest t))
	 (when optional-rest
	   (when supp
	     (push `(,supp ,rest-sym) bindings))
	   (when env
	     (setq default (compile-form default env)))
	   (push `(,var (if ,rest-sym (pop ,rest-sym) ,default))
		 bindings)))
	(&REST
	 (push `(,var ,rest-sym) bindings))
	(&KEY
	 (push `(,var ',unbound) bindings)
	 (when supp
	   (push `(,supp nil) bindings)))
	(&AUX
	 (push var bindings))))
    (nreverse bindings)))

(defun lambda-list-keys (lambda-list)
  (with-collector collect
    (do-lambda-list (((key var)) kind lambda-list)
      (when (eq kind '&KEY)
	(collect key)))))

(defun lambda-list-keyword-vars (lambda-list env &optional include-supplied)
  (with-collector collect
    (do-lambda-list ((var nil supp) kind lambda-list)
      (when (eq kind '&KEY)
	(collect (if env (lexical-value var env) var))
	(when (and supp include-supplied)
	  (collect (if env (lexical-value supp env) supp)))))))

(defun lambda-list-keyword-defaults (lambda-list)
  (with-collector collect
    (do-lambda-list ((var default) kind lambda-list)
      (when (eq kind '&KEY)
	(collect default)))))

(defun load-time-symbol (sym)
  (if (or (not (boundp '*keyword-package*))
	  (eq (SYMBOL-PACKAGE sym) *keyword-package*))
      `(keyword ,(symbol-name sym))
      `(INTERN ,(symbol-name sym) ,(PACKAGE-NAME (SYMBOL-PACKAGE sym)))))

(defun keyword-assignments (lambda-list env)
  (let ((allow-other-keys-p (memq '&ALLOW-OTHER-KEYS lambda-list))
	(temp (gensym))
	(allow (gensym))
	(val (gensym))
	(keys (lambda-list-keys lambda-list))
	(vars (lambda-list-keyword-vars lambda-list env))
	(defaults (lambda-list-keyword-defaults lambda-list)))
    (when keys
      (let* ((list `(list ,@(mapcar #'load-time-symbol keys)))
	     (keyword-list
	      (if (eval-when-compile (featurep 'xemacs))
		  list
		  `(load-time-value ,list)))
	     (body
	      `((while ,rest-sym
		  (let ((,temp (position (pop ,rest-sym) ,keyword-list)) ,val)
		    ,@(unless allow-other-keys-p
		       `((unless (or ,temp ,allow) (ERROR 'PROGRAM-ERROR))))
		    (when (null ,rest-sym)
		      (ERROR 'PROGRAM-ERROR))
		    (setq ,val (pop ,rest-sym))
		    (when ,temp
		      (set (nth ,temp ',vars) ,val))))
		,@(mappend (lambda (var default)
			     `((when (eq ,var ',unbound)
				 (setq ,var
				       ,(if env
					    (compile-form default env)
					    default)))))
			   vars defaults))))
	(unless allow-other-keys-p
	  (setq body
		`((let ((,allow (cadr (memq (kw ALLOW-OTHER-KEYS) ,rest-sym))))
		    ,@body))))
	body))))

(defun aux-assignments (lambda-list env)
  (let ((bindings nil))
    (do-lambda-list ((var default) kind lambda-list)
      (when (and (eq kind '&AUX)
		 default)
	(when env
	  (setq var (compile-variable var env)
		default (compile-form default env)))
	(push `(,var ,default) bindings)))
    (when bindings
      `((setq ,@(nreverse bindings))))))

(defun translate-lambda-list (lambda-list env)
  (mapcar (lambda (x)
	    (let ((cons (assq x '((&OPTIONAL . &optional) (&REST . &rest)))))
	      (cond
		(cons	(cdr cons))
		(env	(compile-variable x env))
		(t	x))))
	  lambda-list))

(defun lambda-list-variables (lambda-list)
  (let ((result nil))
    (do-lambda-list ((var default supp) kind lambda-list)
      (if (symbolp var)
	  (push var result)
	  (setq result (nconc result (lambda-list-variables var))))
      (when supp (push supp result)))
    result))

(defun expand-lambda (lambda-list body &optional env)
  (if (and (every 'symbolp lambda-list)
	   (notany (lambda (x) (memq x '(&KEY &AUX))) lambda-list))
      ;; Easy case: no defaults, suppliedp, keyword, or aux parameters.
      `(lambda ,(translate-lambda-list lambda-list env) ,@body)
      ;; Difficult case:
      `(lambda ,(simplify-lambda-list lambda-list env)
	(let* ,(lambda-list-bindings lambda-list env)
;; 	  ,@(unless (or (memq '&REST lambda-list) (memq '&KEY lambda-list))
;; 	      `((when ,rest-sym
;; 		  (ERROR 'PROGRAM-ERROR))))
	  ,@(keyword-assignments lambda-list env)
	  ,@(aux-assignments lambda-list env)
	  ,@body))))

(defmacro cl:lambda (lambda-list &rest body)
;  (byte-compile
   (expand-lambda lambda-list body));)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./func.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./interaction.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; A major mode implementing an Emacs Common Lisp listener.

(defvar emacs-cl-prompt-marker nil
  "Position of last prompt.")

(defvar emacs-cl-history '("")
  "Common Lisp listener command history.")

(defvar emacs-cl-history-index 0
  "Common Lisp listener command history index.")

(defun emacs-cl ()
  "Starts a Common Lisp listener."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*Emacs Common Lisp*"))
  (emacs-cl-mode)
  (setq *STANDARD-OUTPUT* (make-buffer-output-stream (current-buffer))
	*ERROR-OUTPUT* *STANDARD-OUTPUT*
	*TRACE-OUTPUT* *STANDARD-OUTPUT*)
  (setq *STANDARD-INPUT*
	(MAKE-ECHO-STREAM (make-read-char-exclusive-input-stream)
			  *STANDARD-OUTPUT*))
  (setq *TERMINAL-IO* (MAKE-TWO-WAY-STREAM *STANDARD-INPUT* *STANDARD-OUTPUT*)
	*QUERY-IO* *TERMINAL-IO*)
  (setq standard-output
	(if use-character-type-p
	    (lambda (char) (WRITE-CHAR char *STANDARD-OUTPUT*))
	    (lambda (char) (WRITE-CHAR (CODE-CHAR char) *STANDARD-OUTPUT*))))
  (insert (PACKAGE-NAME *PACKAGE*) "> ")
  (setq emacs-cl-prompt-marker (point-marker)))

(defun emacs-cl-mode ()
  "Major mode for an Emacs Common Lisp listener.

  \\[emacs-cl-newline]		Process current line
  \\[emacs-cl-beginning-of-line]		Go to start of current line
  \\[emacs-cl-history-previous]		Previous line in history
  \\[emacs-cl-history-next]		Next line in history
  \\[emacs-cl-beginning-of-line]		Go to start of current line"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'emacs-cl-mode)
  (setq mode-name "Emacs Common Lisp")
  (use-local-map emacs-cl-mode-map)
  (make-variable-buffer-local 'emacs-cl-prompt-marker)
  (make-variable-buffer-local 'emacs-cl-history)
  (make-variable-buffer-local 'emacs-cl-history-index)
  (run-hooks 'emacs-cl-mode-hooks))

(defvar emacs-cl-mode-map nil
  "Local keymap for Emacs Common Lisp listener buffers.")

(unless emacs-cl-mode-map
  (setq emacs-cl-mode-map (make-keymap))
  (define-key emacs-cl-mode-map "\C-m" 'emacs-cl-newline)
  (define-key emacs-cl-mode-map "\C-a" 'emacs-cl-beginning-of-line)
  (define-key emacs-cl-mode-map "\M-p" 'emacs-cl-history-previous)
  (define-key emacs-cl-mode-map "\M-n" 'emacs-cl-history-next))

(defun* emacs-cl-eval-interactively (form)
  (save-current-buffer
    (set (INTERN "-" "CL") form)
    (let ((*-sym (INTERN "*" "CL"))
	  (/-sym (INTERN "/" "CL"))
	  (+-sym (INTERN "+" "CL"))
	  (values
	   (restart-bind ((ABORT (lambda ()
				   (return-from emacs-cl-eval-interactively))))
	     (MULTIPLE-VALUE-LIST (EVAL form)))))
      (setq +++ ++ ++ (SYMBOL-VALUE +-sym))
      (set +-sym form)
      (setq /// // // (SYMBOL-VALUE /-sym))
      (set /-sym values)
      (setq *** ** ** (SYMBOL-VALUE *-sym))
      (set *-sym (first values))
      values)))

(defun emacs-cl-get-line ()
  (let ((line (buffer-substring emacs-cl-prompt-marker (point))))
    (setf (nth 0 emacs-cl-history) line)
    (HANDLER-BIND
	((END-OF-FILE
	  (lambda (c)
	    (insert "\n")
	    (dotimes (i (+ (length (PACKAGE-NAME *PACKAGE*)) 2))
	      (insert " "))
	    (throw 'read-continue nil)))
	 (ERROR
	  (lambda (c)
	    (FORMAT T "~%Error: ~S" c)
	    (throw 'read-error nil))))
      (READ-FROM-STRING line))))

(defun emacs-cl-newline ()
  (interactive)
  (catch 'read-continue
    (if (< (point) emacs-cl-prompt-marker)
	(insert "\n")
	(goto-char (point-max))
	(when (> (point) emacs-cl-prompt-marker)
	  (dolist (x (catch 'read-error
		       (emacs-cl-eval-interactively (emacs-cl-get-line))))
	    (let* ((start (1+ (point)))
		   (ignore (PPRINT x))
		   (end (point)))
	      (put-text-property start end 'mouse-face 'modeline)
	      ;(put-text-property start end 'keymap ...)
	      (put-text-property start end 'emacs-cl-object x))))
	(insert "\n" (PACKAGE-NAME *PACKAGE*) "> ")
	(setq emacs-cl-prompt-marker (point-marker))
	(push "" emacs-cl-history)
	(setq emacs-cl-history-index 0))))

(defun emacs-cl-history-previous ()
  (interactive)
  (when (and (>= (point) emacs-cl-prompt-marker)
	     (< emacs-cl-history-index (1- (length emacs-cl-history))))
    (when (zerop emacs-cl-history-index)
      (setf (nth 0 emacs-cl-history)
	    (buffer-substring emacs-cl-prompt-marker (point))))
    (goto-char (point-max))
    (delete-region emacs-cl-prompt-marker (point))
    (incf emacs-cl-history-index)
    (insert (nth emacs-cl-history-index emacs-cl-history))))

(defun emacs-cl-history-next ()
  (interactive)
  (when (and (>= (point) emacs-cl-prompt-marker)
	     (plusp emacs-cl-history-index))
    (goto-char (point-max))
    (delete-region emacs-cl-prompt-marker (point))
    (decf emacs-cl-history-index)
    (insert (nth emacs-cl-history-index emacs-cl-history))))

(defun emacs-cl-beginning-of-line ()
  (interactive)
  (if (< (point) emacs-cl-prompt-marker)
      (beginning-of-line)
      (progn
	(beginning-of-line)
	(when (< (point) emacs-cl-prompt-marker)
	  (goto-char emacs-cl-prompt-marker)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./interaction.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./load-cl.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; Functions for loading and compiling the whole system.
;;; Loading this file also loads the system as a side effect.

(require 'cl)
(require 'byte-compile "bytecomp")

(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

;;; Fake IN-PACKAGE and FIND-PACKAGE until they are defined properly
;;; in cl-packages.el.
(defmacro IN-PACKAGE (name) nil)
(defun FIND-PACKAGE (name) nil)

(defvar *cl-files*
  '("utils"
    "func"

    "cl-evaluation"
    "cl-flow"
    "cl-numbers"
    "cl-conses"
    "cl-characters"
    "cl-strings"
    "cl-arrays"
    "cl-sequences"
    "cl-structures"
    "cl-iteration"

    "cl-symbols"
    "cl-packages"

    "cl-types"
    "cl-typep"
    "cl-subtypep"

    "cl-hash"
    "cl-streams"
    "cl-reader"
    "cl-printer"
    "cl-environment"
    "cl-filenames"
    "cl-files"
    "interaction"
    "cl-eval"
    "cl-system"

    "cl-loop"
    "cl-format"
    "cl-compile"
    "cl-objects"
    "cl-conditions"

    "populate"))

(defun load-cl ()
  (interactive)
  (let ((load-path (cons (file-name-directory load-file-name) load-path))
	(debug-on-error t)
	(byte-compile-warnings nil))
    (mapc #'load *cl-files*)
    (populate-packages)
    (garbage-collect)))

(defun compile-cl ()
  (interactive)
  (let ((byte-compile-warnings '(not cl-functions)))
    (dolist (file *cl-files*)
      (byte-compile-file (concat file ".el")))))

(when (string-match "^19" emacs-version)
  (dolist (x '(:required :optional-rest :weakness :type :read-only
	       :constituent :whitespace :single-escape
	       :multiple-escape :terminating-macro
	       :non-terminating-macro :eof :special-operator
	       :lexical :special :macro :symbol-macro))
    (set x x)))

(setq *global-environment*
      (vector 'environment
	      ;; Variable information
	      nil nil nil
	      ;; Function information
	      nil nil nil
	      ;; Block and tagbody information
	      nil nil))

(load-cl)
(IN-PACKAGE "CL-USER")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./load-cl.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./tests.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003 Lars Brinkhoff.

;;; Various test cases for bignum addition.
(defun bignum-test ()
  (loop for (x y z) in
	'((67108864		67108864		[BIGNUM -134217728 0])
	  (134217727		1			[BIGNUM -134217728 0])
	  (-134217728		-1			[BIGNUM 134217727 -1])
	  (-134217728		-134217728		[BIGNUM 0 -1])
	  ([BIGNUM -1 0]	[BIGNUM -1 0]		[BIGNUM -2 1])
	  ([BIGNUM 0 -1]	[BIGNUM 0 -1]		[BIGNUM 0 -2])
	  ([BIGNUM 0 2]		[BIGNUM 0 -1]		[BIGNUM 0 1])
	  ([BIGNUM 0 -1]	[BIGNUM 0 2]		[BIGNUM 0 1])
	  ([BIGNUM 0 1]		[BIGNUM 0 -2]		[BIGNUM 0 -1])
	  ([BIGNUM 0 -2]	[BIGNUM 0 1]		[BIGNUM 0 -1])
	  ([BIGNUM 2 2]		[BIGNUM -1 -3]		1)
	  ([BIGNUM 2 2]		[BIGNUM -3 -3]		-1)
	  ([BIGNUM -54323701 6]	[BIGNUM 16292363 17]	[BIGNUM -38031338 23])
	  ([BIGNUM 119720045 12408]
				[BIGNUM 38283770 30621]
						    [BIGNUM -110431641 43029])
	  ([BIGNUM -134217728 2] -1			[BIGNUM 134217727 2])
	  ([BIGNUM 0 100000000]	[BIGNUM 0 100000000]	[BIGNUM 0 -68435456 0])
	  ([BIGNUM -24181363 103035877]
				[BIGNUM -24181363 103035877]
					       [BIGNUM -48362726 -62363701 0]))
	do (unless (equal (cl:+ x y) z)
	     (princ (format "%s + %s /= %s\n" x y z)))))

(defun funcall-test ()
  (let ((foo
	 (EVAL (READ-FROM-STRING
		"(defun foo (a &optional b (c 100) (d 101 dp)
		               &key f (g 102) (h 103 hp) ((i j) 104 jp))
		   (list a b c d dp f g h hp j jp))")))
	(data
	 `(((1) . (1 nil 100 101 nil nil 102 103 nil 104 nil))
	   ((1 2) . (1 2 100 101 nil nil 102 103 nil 104 nil))
	   ((1 2 3) . (1 2 3 101 nil nil 102 103 nil 104 nil))
	   ((1 2 3 4) . (1 2 3 4 t nil 102 103 nil 104 nil))
	   ((1 2 3 4 ,(kw f) 5) . (1 2 3 4 t 5 102 103 nil 104 nil)))))
    (dolist (x data)
      (let* ((args (car x))
	     (good (cdr x))
	     (result (APPLY foo args)))
	(dolist (i '(4 9 11))
	  (setf (nth i result) (not (null (nth i result)))))
	(unless (equal result good)
	  (princ (format "Interpreted FOO %s /= %s\n" args result)))))
    (COMPILE foo nil)
    (dolist (x data)
      (let* ((args (car x))
	     (good (cdr x))
	     (result (APPLY foo args)))
	(dolist (i '(4 9 11))
	  (setf (nth i result) (not (null (nth i result)))))
	(unless (equal result good)
	  (princ (format "Compiled FOO %s = %s\n" args (APPLY foo args)))
	  (princ (format "Compiled FOO %s /= %s\n" args result)))))))

(defun test-cl ()
  (bignum-test)
  ;(funcall-test)
  (LOAD "tests.lisp")
  (princ "\nAll tests completed.\n\n"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./tests.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./utils.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; -*- emacs-lisp -*-
;;;
;;; Copyright (C) 2003, 2004 Lars Brinkhoff.
;;; This file provides various small utilities.

(defun map-to-gensyms (list)
  (mapcar (lambda (x) (gensym)) list))

(defmacro* with-gensyms (syms &body body)
  ;;`(let ,(mapcar #'list syms '#1=((gensym) . #1#))
  `(let ,(mapcar (lambda (sym) `(,sym ',(gensym))) syms)
     ,@body))

(defun cl:string (x)
  (cond
    ((stringp x)	x)
    ((symbolp x)	(symbol-name x))
    (t			(error "type error"))))

(defun strcat (&rest string-designators)
  (apply #'concat (mapcar #'cl:string string-designators)))

(defun symcat (&rest string-designators)
  (let ((sym (intern (apply #'strcat string-designators))))
    (when (fboundp 'SYMBOL-PACKAGE)
      (setf (SYMBOL-PACKAGE sym) *PACKAGE*))
    sym))

(defun cl:symcat (&rest string-designators)
  (let ((sym (INTERN (apply #'strcat string-designators))))
    ;(when (fboundp 'SYMBOL-PACKAGE)
      (setf (SYMBOL-PACKAGE sym) *PACKAGE*);)
    sym))

(defun just-one (list)
  (cond
    ((atom list)	list)
    ((cdr list)		(error "error"))
    (t			(car list))))

(defun mappend (fn &rest lists)
  (apply #'append
   (if (null (cdr lists))
       (mapcar fn (car lists))
       (cl-mapcar-many fn lists))))

(defun vector-and-typep (object type)
  (and (vectorp object)
       (eq (aref object 0) type)))

(defun curry (fn &rest args1)
  `(lambda (&rest args2)
     (apply ',fn ,@(mapcar (lambda (x) (list 'quote x)) args1) args2)))

(defun rcurry (fn &rest args2)
  `(lambda (&rest args1) (apply ',fn (append args1 ',args2))))

(defmacro compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
	    (fns (butlast fns)))
	`(lambda (&rest args)
	  ,(reduce (lambda (f1 f2) `(,f1 ,f2)) fns
		   :from-end t :initial-value `(apply ',fn1 args))))
      #'identity))

(defun ensure-list (object)
  (if (listp object)
      object
      (list object)))

(defmacro* do-list-designator ((var list &optional result) &body body)
  `(dolist (,var (ensure-list ,list) ,result)
     ,@body))

(defmacro* do-plist ((prop val plist &optional result) &body body)
  (with-gensyms (list)
    `(do ((,list ,plist)
	  ,prop ,val)
         ((null ,list)
	  ,result)
      (setq ,prop (pop ,list) ,val (pop ,list))
      ,@body)))

(defun el-keyword (symbol)
  (intern (concat ":" (symbol-name symbol))))

;;; Bootstrap magic: this list of symbols will later be imported into
;;; the KEYWORD package.
(defvar *initial-keywords* nil)

;;; Initially, this function pushes all created symbols onto
;;; *initial-keywords*.  Later, it will be redefined to intern symbols
;;; into the KEYWORD package directly.
(defun keyword (name)
  (let ((sym (find name *initial-keywords* :key 'symbol-name :test 'string=)))
    (or sym
	(let ((sym (make-symbol name)))
	  (push sym *initial-keywords*)
	  (set sym sym)
	  sym))))

(if (eval-when-compile (featurep 'xemacs))
    (defmacro kw (name) `(keyword ,(symbol-name name)))
    (defmacro kw (name) `(load-time-value (keyword ,(symbol-name name)))))

(defun type-error (datum type)
  (ERROR 'TYPE-ERROR (kw DATUM) datum (kw EXPECTED-TYPE) type))

(defconst use-character-type-p (eq (type-of ?A) 'character))

(if use-character-type-p
    (progn
      (defmacro ch (code) (int-char code))
      (defmacro ch= (char code) `(char= ,char ,(int-char code)))
      (defmacro cl-char (char) char)
      (defmacro el-char (char) char))
    (progn
      (defmacro ch (code) (vector 'CHARACTER code))
      (defmacro ch= (char code) `(eq (aref ,char 1) ,code))
      (defmacro cl-char (char) `(vector 'CHARACTER ,char))
      (defmacro el-char (char) `(aref ,char 1))))

(defmacro define-storage-layout (type slots)
  (let ((index 0))
    `(progn
       ,@(mapcar (lambda (slot)
		   `(defmacro ,(symcat type "-" slot) (object)
		      (list 'aref object ,(incf index))))
		 slots)
       ',type)))

;;; This macro can be used instead of VALUES.
(defmacro cl:values (&rest vals)
  (let ((n (length vals)))
    (case n
      (0	`(setq nvals 0 mvals nil))
      (1	`(prog1 ,(car vals) (setq nvals 1 mvals nil)))
      (t	`(prog1
		   ,(car vals)
		   (setq nvals ,n mvals (list ,@(cdr vals))))))))

(defun expand-tagbody-forms (body start end)
  (do ((clauses nil)
       (clause (list (list start)))
       (forms body (cdr forms)))
      ((null forms)
       (setq clause (append clause (list (list 'go end))))
       (setq clauses (append clauses `(,clause)))
       clauses)
    (let ((form (first forms)))
      (cond
	((atom form)
	 (setq clause (append clause `((go ,form))))
	 (setq clauses (append clauses `(,clause)))
	 (setq clause `((,form))))
	(t
	 (setq clause (append clause `(,form))))))))

(defmacro* tagbody (&body body)
  (let ((pc (gensym))
	(start (gensym))
	(end (gensym))
	(throw-tag (gensym)))
    `(let ((,pc ',start))
      (macrolet ((go (tag)
		   (list 'throw
			 (list 'quote ',throw-tag)
			 (list 'quote tag))))
	(while (not (eq ,pc ',end))
	  (setq ,pc
		(catch ',throw-tag
		  (case ,pc
		    ,@(expand-tagbody-forms body start end))))))
      nil)))

;(defun tagbody-blocks (body start)
;  (do ((n 0)
;       (blocks nil)
;       (block (list start))
;       (forms body (cdr forms)))
;      ((null forms)
;       (setq block (append block (list -1)))
;       (setq blocks (append blocks `(,block)))
;       blocks)
;    (let ((form (first forms)))
;      (cond
;	((atom form)
;	 (incf n)
;	 (setq block (append block `(,n)))
;	 (setq blocks (append blocks `(,block)))
;	 (setq block (list form)))
;	(t
;	 (setq block (append block `(,form))))))))

;(defun tagbody-functions (blocks)
;  (let ((tags (do ((blocks blocks (cdr blocks))
;		   (tags nil)
;		   (n 0))
;		  ((null blocks) tags)
;		(push (cons (pop (car blocks)) n) tags)
;		(incf n)))
;	(catch (gensym)))
;    (mapcar (lambda (block)
;	      `(lambda ()
;		(macrolet ((go (tag)
;			     (list 'throw
;				   (list 'quote ',catch)
;				   (list 'quote (cdr (assq tag ',tags))))))
;		  (catch ',catch
;		    ,@block))))
;	    blocks)))

;(defmacro* tagbody (&body body)
;  (let* ((pc (gensym))
;	 (start (if (atom (first body)) (pop body) (gensym)))
;	 (blocks (tagbody-blocks body start)))
;    `(let ((,pc 0))
;	(while (not (minusp ,pc))
;	  (setq ,pc (funcall (aref (eval-when-compile
;				    (vector ,@(tagbody-functions blocks)))
;				   ,pc)))))))

(defun mapcar2 (fn list)
  (when list
    (cons (funcall fn (first list) (second list))
	  (mapcar2 fn (cddr list)))))

(defun tree-count (object tree) ; &KEY TEST KEY
  (cond
    ((eq object tree)	1)
    ((atom tree)	0)
    (t			(+ (tree-count object (car tree))
			   (tree-count object (cdr tree))))))

(defmacro destructuring-lambda (lambda-list &rest body)
  (with-gensyms (args)
    `(lambda (&rest ,args)
       (destructuring-bind ,lambda-list ,args ,@body))))

(defmacro* define-case (name &key test)
  (setq test (if (and (consp test) (eq (car test) 'function))
		 (cdr test)
		 (cons 'funcall (cdr test))))
  `(progn
     (defmacro ,name (object &rest clauses)
       (with-gensyms (value)
	 (let ((fn ',test))
	   `(let ((,value ,object))
	      (cond
		,@(mapcar (destructuring-lambda ((x &rest forms))
			    `((cl:values (,@fn ,value ',x))
			      (progn ,@forms)))
			  clauses))))))
;;      (defmacro ,(intern (concat "e" (symbol-name name))) (object &rest clauses)
;;        (with-gensyms (value)
;; 	 (let ((,value ,object))
;; 	   `(,name ,value
;; 	      ,@clauses
;; 	      (t (ERROR "No match for ~S in ~S." ,value ',name))))))
     ',name))

(define-case subtypecase :test #'SUBTYPEP)

;; (defmacro with-collector (name &rest body)
;;   (with-gensyms (result end)
;;     `(let* ((,result (list nil))
;; 	    (,end ,result))
;;        (macrolet ((,name (x)
;; 		    (list 'setq ',end (list 'setcdr ',end (list 'list x)))))
;; 	 ,@body
;; 	 (cdr ,result)))))

(defmacro with-collector (name &rest body)
  (with-gensyms (result)
    `(let ((,result nil))
       (macrolet ((,name (x) (list 'push x ',result)))
	 ,@body
	 (nreverse ,result)))))

(defun interned-p (symbol)
  (and (symbolp symbol)
       (eq (intern-soft (symbol-name symbol)) symbol)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./utils.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




