;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                              predicate routines


(in-package 'lisp)
(export '(deftype typep subtypep coerce))


(in-package 'system)


(proclaim '(optimize (safety 2) (space 3)))

;;; DEFTYPE macro.
(defmacro deftype (name lambda-list &rest body)
  ;; Replace undefaultized optional parameter X by (X '*).
  (do ((l lambda-list (cdr l))
       (m nil (cons (car l) m)))
      ((null l))
    (when (member (car l) lambda-list-keywords)
	  (unless (eq (car l) '&optional) (return nil))
	  (setq m (cons '&optional m))
	  (setq l (cdr l))
	  (do ()
	      ((or (null l) (member (car l) lambda-list-keywords)))
	    (if (symbolp (car l))
		(setq m (cons (list (car l) ''*) m))
		(setq m (cons (car l) m)))
	    (setq l (cdr l)))
	  (setq lambda-list (nreconc m l))
	  (return nil)))
  `(progn (setf (get ',name 'deftype-form)
	   '(deftype ,name ,lambda-list ,@body))
          (setf (get ',name 'deftype-definition)
	   #'(lambda ,lambda-list ,@body))
          (setf (get ',name 'type-documentation)
	   ,(find-documentation body))
          ',name))


;;; Some DEFTYPE definitions.
(deftype fixnum ()
  `(integer ,most-negative-fixnum ,most-positive-fixnum))
(deftype bit () '(integer 0 1))
(deftype mod (n)
  `(integer 0 ,(1- n)))
(deftype signed-byte (&optional s)
  (if (eq s '*)
      `(integer * *)
      `(integer ,(- (expt 2 (1- s))) ,(1- (expt 2 (1- s))))))
(deftype unsigned-byte (&optional s)
  (if (eq s '*)
      `(integer 0 *)
      `(integer 0 ,(1- (expt 2 s)))))

(deftype vector (&optional element-type size)
  `(array ,element-type (,size)))
(deftype string (&optional size)
  `(vector string-char ,size))
(deftype bit-vector (&optional size)
  `(vector bit ,size))

(deftype simple-vector (&optional size)
  `(simple-array t (,size)))
(deftype simple-string (&optional size)
  `(simple-array string-char (,size)))
(deftype simple-bit-vector (&optional size)
  `(simple-array bit (,size)))



(defun simple-array-p (x)
  (and (arrayp x)
       (not (adjustable-array-p x))
       (not (array-has-fill-pointer-p x))
       (not (sys:displaced-array-p x))))))


(dolist (l '((null . null)
	     (symbol . symbolp)
	     (keyword . keywordp)
	     (atom . atom)
	     (cons . consp)
	     (list . listp)
	     (number . numberp)
	     (character . characterp)
	     (package . packagep)
	     (stream . streamp)
	     (pathname . pathnamep)
	     (readtable . readtablep)
	     (hash-table . hash-table-p)
	     (random-state . random-state-p)
	     (structure . sys:structurep)
	     (function . functionp)
	     (compiled-function . compiled-function-p)
	     (common . commonp)
	     ))
  (setf (get (car l) 'type-predicate) (cdr l)))


;;; TYPEP predicate.
(defun typep (object type &aux tp i)
  (if (atom type)
      (setq tp type i nil)
      (setq tp (car type) i (cdr type)))
  (let ((f (get tp 'TYPE-PREDICATE)))
    (when f (return-from typep (funcall f object))))
  (case tp
    (MEMBER (member object i))
    (NOT (not (typep object (car i))))
    (OR (dolist (e i)
	  (when (typep object e) (return t))))
    (AND (dolist (e i t)
	   (unless (typep object e) (return nil))))
    (SATISFIES (funcall (car i) object))
    ((T) t)
    ((NIL) nil)
    (FIXNUM (eq (type-of object) 'fixnum))
    (BIGNUM (eq (type-of object) 'bignum))
    (RATIO (eq (type-of object) 'ratio))
    (STANDARD-CHAR
     (and (characterp object) (standard-char-p object)))
    (STRING-CHAR
     (and (characterp object) (string-char-p object)))
    (INTEGER
     (and (integerp object) (in-interval-p object i)))
    (RATIONAL
     (and (rationalp object) (in-interval-p object i)))
    (FLOAT
     (and (floatp object) (in-interval-p object i)))
    ((SINGLE-FLOAT SHORT-FLOAT)
     (and (eq (type-of object) 'SHORT-FLOAT) (in-interval-p object i)))
    ((DOUBLE-FLOAT LONG-FLOAT)
     (and (eq (type-of object) 'LONG-FLOAT) (in-interval-p object i)))
    (COMPLEX
     (and (complexp object)
          (or (null i)
	      (and (typep (realpart object) (car i))
		   ;;wfs--should only have to check one.
		   ;;Illegal to mix real and imaginary types!
		   (typep (imagpart object) (car i))))
	   ))
    (SEQUENCE (or (listp object) (vectorp object)))
    (STRING
     (and (stringp object)
          (or (null i) (match-dimensions (array-dimensions object) i))))
    (BIT-VECTOR
     (and (bit-vector-p object)
          (or (null i) (match-dimensions (array-dimensions object) i))))
    (SIMPLE-STRING
     (and (simple-string-p object)
          (or (null i) (match-dimensions (array-dimensions object) i))))
    (SIMPLE-BIT-VECTOR
     (and (simple-bit-vector-p object)
          (or (null i) (match-dimensions (array-dimensions object) i))))
    (SIMPLE-VECTOR
     (and (simple-vector-p object)
          (or (null i) (match-dimensions (array-dimensions object) i))))
    (SIMPLE-ARRAY
     (and (simple-array-p object)
          (or (endp i) (eq (car i) '*)
              (equal (array-element-type object) (car i)))
          (or (endp (cdr i)) (eq (cadr i) '*)
              (match-dimensions (array-dimensions object) (cadr i)))))
    (ARRAY
     (and (arrayp object)
          (or (endp i) (eq (car i) '*)
              ;; Or the element type of object should be EQUAL to (car i).
              ;; Is this too strict?
              (equal (array-element-type object) (car i)))
          (or (endp (cdr i)) (eq (cadr i) '*)
              (match-dimensions (array-dimensions object) (cadr i)))))
    (t
     (cond #+clos
	   ((sys:instancep object)
	    ;; Follow the inheritance chain
	    (locally (declare (notinline find-class))
	       (and (find-class type nil)
		    (subclassp (sys:instance-class object)
			       (find-class type nil)))))
	   #-clos
	   ((get tp 'IS-A-STRUCTURE)
            (when (sys:structurep object)
	      ;; Follow the chain of structure-include.
	      (do ((stp (sys:structure-name object)
			(get stp 'STRUCTURE-INCLUDE)))
		  ((eq tp stp) t)
		(when (null (get stp 'STRUCTURE-INCLUDE))
		  (return nil)))))
           ((get tp 'DEFTYPE-DEFINITION)
            (typep object
                   (apply (get tp 'DEFTYPE-DEFINITION) i)))))))

#+clos
(defun subclassp (low high)
  (or (eq low high)
      (dolist (class (sys:instance-ref low 1)) ; (class-superiors low)
	(when (subclassp class high) (return t)))))

;;; NORMALIZE-TYPE normalizes the type using the DEFTYPE definitions.
;;; The result is always a list.
(defun normalize-type (type &aux tp i fd)
  ;; Loops until the car of type has no DEFTYPE definition.
  (loop
    (if (atom type)
        (setq tp type i nil)
        (setq tp (car type) i (cdr type)))
    (if (setq fd (get tp 'DEFTYPE-DEFINITION))
        (setq type (apply fd i))
        (return-from normalize-type (if (atom type) (list type) type)))))


;;; KNOWN-TYPE-P answers if the given type is a known base type.
;;; The type may not be normalized.
(defun known-type-p (type)
  (when (consp type) (setq type (car type)))
  (if (or (member type
                  '(T NIL NULL SYMBOL KEYWORD ATOM CONS LIST SEQUENCE
                    NUMBER INTEGER BIGNUM RATIONAL RATIO FLOAT
                    SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT COMPLEX
                    CHARACTER STANDARD-CHAR STRING-CHAR
                    PACKAGE STREAM PATHNAME READTABLE HASH-TABLE RANDOM-STATE
                    STRUCTURE ARRAY SIMPLE-ARRAY FUNCTION COMPILED-FUNCTION))
	  #+clos
	  (locally (declare (notinline find-class))
		   (find-class type nil))
          (get type 'IS-A-STRUCTURE))
      t
      nil))

;;; Dummy version before CLOS is loaded
#+clos
(defun find-class (n &optional err env) (declare (ignore n err env)) nil)
#+clos
(proclaim '(notinline find-class))

;;; SUBTYPEP predicate.
(defun subtypep (type1 type2 &aux t1 t2 i1 i2 ntp1 ntp2)
    (setq type1 (normalize-type type1))
    (setq type2 (normalize-type type2))  
    (when (equal type1 type2)
      (return-from subtypep (values t t)))
    (setq t1 (car type1) t2 (car type2))
    (setq i1 (cdr type1) i2 (cdr type2))
    (case t1
      (MEMBER (dolist (e i1)
		(unless (typep e type2) (return-from subtypep (values nil t))))
	      (return-from subtypep (values t t)))
      (OR (dolist (tt i1)
	    (multiple-value-bind (tv flag) (subtypep tt type2)
	      (unless tv (return-from subtypep (values tv flag)))))
	  (return-from subtypep (values t t)))
      (AND (dolist (tt i1)
	     (let ((tv (subtypep tt type2)))
	       (when tv (return-from subtypep (values t t)))))
	   (return-from subtypep (values nil nil)))
      (NOT (multiple-value-bind (tv flag) (subtypep (car i1) type2)
	     (return-from subtypep (values (not tv) flag)))))
    (case t2
      (MEMBER (return-from subtypep (values nil nil)))
      (OR (dolist (tt i2)
	    (let ((tv (subtypep type1 tt)))
	      (when tv (return-from subtypep (values t t)))))
	  (return-from subtypep (values nil nil)))
      (AND (dolist (tt i2)
	     (multiple-value-bind (tv flag) (subtypep type1 tt)
	       (unless tv (return-from subtypep (values tv flag)))))
	   (return-from subtypep (values nil nil)))
      (NOT (multiple-value-bind (tv flag) (subtype type1 (car i2))
	     (return-from subtypep (values (not tv) flag)))))
    (setq ntp1 (known-type-p type1) ntp2 (known-type-p type2))
    (flet ((is-class-type (x)
	     ;; these are the build-in classes of CLOS:
	     (and (not (member x '(ARRAY SEQUENCE LIST CONS STRING VECTOR
				   BIT-VECTOR CHARACTER NUMBER COMPLEX FLOAT
				   RATIONAL INTEGER RATIO SYMBOL NULL KEYWORD)
			       :test #'eq))
		  (find-class x nil))))
      (cond ((or (eq t1 'nil) (eq t2 't) (eq t2 'COMMON)) (values t t))
	    ((eq t2 'NIL) (values nil ntp1))
	    ((eq t1 'T) (values nil ntp2))
	    ((eq t1 'COMMON) (values nil ntp2))
	    ((eq t2 'LIST)
	     (cond ((member t1 '(NULL CONS LIST) :test #'eq) (values t t))
		   (t (values nil ntp1))))
	    ((eq t2 'SEQUENCE)
	     (cond ((member t1 '(NULL CONS LIST SEQUENCE) :test #'eq)
		    (values t t))
		   ((eq t1 'ARRAY)
		    (if (and (cdr i1) (consp (cadr i1)) (null (cdadr i1)))
			(values t t)
			(values nil t)))
		   (t (values nil ntp1))))
	    ((eq t1 'LIST) (values nil ntp2))
	    ((eq t1 'SEQUENCE) (values nil ntp2))
	    ((eq t2 'ATOM)
	     (cond ((member t1 '(CONS LIST) :test #'eq) (values nil t))
		   (ntp1 (values t t))
		   (t (values nil nil))))
	    ((eq t1 'ATOM) (values nil ntp2))
	    ((eq t2 'SYMBOL)
	     (if (member t1 '(SYMBOL KEYWORD NULL) :test #'eq)
		 (values t t)
		 (values nil ntp1)))
	    ((eq t2 'KEYWORD)
	     (if (eq t1 'KEYWORD) (values t t) (values nil ntp1)))
	    ((eq t2 'NULL)
	     (if (eq t1 'NULL) (values t t) (values nil ntp1)))
	    ((eq t2 'NUMBER)
	     (cond ((member t1 '(BIGNUM INTEGER RATIO RATIONAL FLOAT
				 SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT
				 LONG-FLOAT COMPLEX NUMBER)
			    :test #'eq)
		    (values t t))
		   (t (values nil ntp1))))
	    ((eq t1 'NUMBER) (values nil ntp2))
	    ((eq t2 'STRUCTURE)
	     (if (or (eq t1 'STRUCTURE)
		     #-clos
		     (get t1 'IS-A-STRUCTURE)
		     #+clos
		     (subclassp (find-class t1 nil)
				(find-class 'STRUCTURE-OBJECT)))
		 (values t t)
		 (values nil ntp1)))
	    ((eq t1 'STRUCTURE) (values nil ntp2))
	    #-clos
	    ((get t1 'IS-A-STRUCTURE)
	     (if (get t2 'IS-A-STRUCTURE)
		 (do ((tp1 t1 (get tp1 'STRUCTURE-INCLUDE)) (tp2 t2))
		     ((null tp1) (values nil t))
		   (when (eq tp1 tp2) (return (values t t))))
		 (values nil ntp2)))
	    #+clos
	    ((is-class-type t1)
	     (if (is-class-type t2)
		 (values (subclassp (find-class t1)
				    (find-class t2)) t)
		 (values nil ntp1)))	 
	    #+clos
	    ((is-class-type t2) (values nil ntp1))
	    ((get t2 'IS-A-STRUCTURE) (values nil ntp1))
	    (t
	     (case t1
	       (BIGNUM
		(case t2
		  (bignum (values t t))
		  ((integer rational)
		   (if (sub-interval-p '(* *) i2)
		       (values t t)
		       (values nil t)))
		  (t (values nil ntp2))))
	       (RATIO
		(case t2
		  (ratio (values t t))
		  (rational
		   (if (sub-interval-p '(* *) i2) (values t t) (values nil t)))
		  (t (values nil ntp2))))
	       (STANDARD-CHAR
		(if (member t2 '(STANDARD-CHAR STRING-CHAR CHARACTER)
			    :test #'eq)
		    (values t t)
		    (values nil ntp2)))
	       (STRING-CHAR
		(if (member t2 '(STRING-CHAR CHARACTER) :test #'eq)
		    (values t t)
		    (values nil ntp2)))
	       (INTEGER
		(if (member t2 '(INTEGER RATIONAL) :test #'eq)
		    (values (sub-interval-p i1 i2) t)
		    (values nil ntp2)))
	       (RATIONAL
		(if (eq t2 'RATIONAL)
		    (values (sub-interval-p i1 i2) t)
		    (values nil ntp2)))
	       (FLOAT
		(if (eq t2 'FLOAT)
		    (values (sub-interval-p i1 i2) t)
		    (values nil ntp2)))
	       ((SINGLE-FLOAT SHORT-FLOAT)
		(if (member t2 '(SHORT-FLOAT FLOAT) :test #'eq)
		    (values (sub-interval-p i1 i2) t)
		    (values nil ntp2)))
	       ((DOUBLE-FLOAT LONG-FLOAT)
		(if (member t2 '(SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT FLOAT)
			    :test #'eq)
		    (values (sub-interval-p i1 i2) t)
		    (values nil ntp2)))
	       (COMPLEX
		(if (eq t2 'COMPLEX)
		    (subtypep (or (car i1) t) (or (car i2) t))
		    (values nil ntp2)))
	       (SIMPLE-ARRAY
		(cond ((or (eq t2 'SIMPLE-ARRAY) (eq t2 'ARRAY))
		       (if (or (endp i1) (eq (car i1) '*))
			   (unless (or (endp i2) (eq (car i2) '*))
			     (return-from subtypep (values nil t)))
			   (unless (or (endp i2) (eq (car i2) '*))
			     (unless (equal (car i1) (car i2))
			       ;; Unless the element type matches,
			       ;;  return NIL T.
			       ;; Is this too strict?
			       (return-from subtypep
				 (values nil t)))))
		       (when (or (endp (cdr i1)) (eq (cadr i1) '*))
			 (if (or (endp (cdr i2)) (eq (cadr i2) '*))
			     (return-from subtypep (values t t))
			     (return-from subtypep (values nil t))))
		       (when (or (endp (cdr i2)) (eq (cadr i2) '*))
			 (return-from subtypep (values t t)))
		       (values (match-dimensions (cadr i1) (cadr i2)) t))
		      (t (values nil ntp2))))
	       (ARRAY
		(cond ((eq t2 'ARRAY)
		       (if (or (endp i1) (eq (car i1) '*))
			   (unless (or (endp i2) (eq (car i2) '*))
			     (return-from subtypep (values nil t)))
			   (unless (or (endp i2) (eq (car i2) '*))
			     (unless (equal (car i1) (car i2))
			       (return-from subtypep
				 (values nil t)))))
		       (when (or (endp (cdr i1)) (eq (cadr i1) '*))
			 (if (or (endp (cdr i2)) (eq (cadr i2) '*))
			     (return-from subtypep (values t t))
			     (return-from subtypep (values nil t))))
		       (when (or (endp (cdr i2)) (eq (cadr i2) '*))
			 (return-from subtypep (values t t)))
		       (values (match-dimensions (cadr i1) (cadr i2)) t))
		      (t (values nil ntp2))))
	       (t (if ntp1 (values (eq t1 t2) t) (values nil nil))))))))


(defun sub-interval-p (i1 i2)
  (let (low1 high1 low2 high2)
    (if (endp i1)
        (setq low1 '* high1 '*)
        (if (endp (cdr i1))
            (setq low1 (car i1) high1 '*)
            (setq low1 (car i1) high1 (cadr i1))))
    (if (endp i2)
        (setq low2 '* high2 '*)
        (if (endp (cdr i2))
            (setq low2 (car i2) high2 '*)
            (setq low2 (car i2) high2 (cadr i2))))
    (cond ((eq low1 '*)
	   (unless (eq low2 '*)
	           (return-from sub-interval-p nil)))
          ((eq low2 '*))
	  ((consp low1)
	   (if (consp low2)
	       (when (< (car low1) (car low2))
		     (return-from sub-interval-p nil))
	       (when (< (car low1) low2)
		     (return-from sub-interval-p nil))))
	  ((if (consp low2)
	       (when (<= low1 (car low2))
		     (return-from sub-interval-p nil))
	       (when (< low1 low2)
		     (return-from sub-interval-p nil)))))
    (cond ((eq high1 '*)
	   (unless (eq high2 '*)
	           (return-from sub-interval-p nil)))
          ((eq high2 '*))
	  ((consp high1)
	   (if (consp high2)
	       (when (> (car high1) (car high2))
		     (return-from sub-interval-p nil))
	       (when (> (car high1) high2)
		     (return-from sub-interval-p nil))))
	  ((if (consp high2)
	       (when (>= high1 (car high2))
		     (return-from sub-interval-p nil))
	       (when (> high1 high2)
		     (return-from sub-interval-p nil)))))
    (return-from sub-interval-p t)))

(defun in-interval-p (x interval)
  (let (low high)
    (if (endp interval)
        (setq low '* high '*)
        (if (endp (cdr interval))
            (setq low (car interval) high '*)
            (setq low (car interval) high (cadr interval))))
    (cond ((eq low '*))
          ((consp low)
           (when (<= x (car low)) (return-from in-interval-p nil)))
          ((when (< x low) (return-from in-interval-p nil))))
    (cond ((eq high '*))
          ((consp high)
           (when (>= x (car high)) (return-from in-interval-p nil)))
          ((when (> x high) (return-from in-interval-p nil))))
    (return-from in-interval-p t)))

(defun match-dimensions (dim pat)
  (if (null dim)
      (null pat)
      (and (or (eq (car pat) '*)
	       (eq (car dim) (car pat)))
	   (match-dimensions (cdr dim) (cdr pat)))))



;;; COERCE function.
(defun coerce (object type)
  (when (typep object type)
        ;; Just return as it is.
        (return-from coerce object))
  (setq type (normalize-type type))
  (case (car type)
    (LIST
     (do ((l nil (cons (elt object i) l))
          (i (1- (length object)) (1- i)))
         ((< i 0) l)
       (declare (fixnum i))))
    ((ARRAY SIMPLE-ARRAY)
     (unless (or (endp (cdr type))
                 (endp (cddr type))
                 (eq (caddr type) '*)
                 (endp (cdr (caddr type))))
             (error "Cannot coerce to an multi-dimensional array."))
     (do* ((l (length object))
	   (seq (make-sequence type l))
	   (i 0 (1+ i)))
	  ((>= i l) seq)
       (declare (fixnum i l))
       (setf (elt seq i) (coerce (elt object i) (cadr type)))))
    ((CHARACTER STRING-CHAR) (character object))
    (FLOAT (float object))
    ((SINGLE-FLOAT SHORT-FLOAT) (float object 0.0S0))
    ((DOUBLE-FLOAT LONG-FLOAT) (float object 0.0L0))
    (COMPLEX
     (if (or (null (cdr type)) (null (cadr type)) (eq (cadr type) '*))
         (complex (realpart object) (imagpart object))
         (complex (coerce (realpart object) (cadr type))
                  (coerce (imagpart object) (cadr type)))))
    (t (error "Cannot coerce ~S to ~S." object type))))
