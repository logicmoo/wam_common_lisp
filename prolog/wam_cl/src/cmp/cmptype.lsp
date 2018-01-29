;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPTYPE  Type information.

(in-package 'compiler)

;;; CL-TYPE is any valid type specification of Common Lisp.
;;;
;;; TYPE is a representation type used by ECL.  TYPE is one of:
;;;
;;;				T(BOOLEAN)
;;;
;;;	FIXNUM  CHARACTER  SHORT-FLOAT  LONG-FLOAT
;;;	(VECTOR T)  STRING  BIT-VECTOR  (VECTOR FIXNUM)
;;;	(VECTOR SHORT-FLOAT)  (VECTOR LONG-FLOAT)
;;;	(ARRAY T)  (ARRAY STRING-CHAR)  (ARRAY BIT)
;;;	(ARRAY FIXNUM)
;;;	(ARRAY SHORT-FLOAT)  (ARRAY LONG-FLOAT)
;;;	STANDARD-OBJECT STRUCTURE-OBJECT
;;;	UNKNOWN
;;;
;;;				NIL
;;;
;;;
;;; immediate-type:
;;;	FIXNUM		int
;;;	CHARACTER	char
;;;	SHORT-FLOAT	float
;;;	LONG-FLOAT	double


;;; Check if THING is an object of the type TYPE.
;;; Depends on the implementation of TYPE-OF.
(defun object-type (thing)
  (let ((type (type-of thing)))
    (case type
      ((FIXNUM SHORT-FLOAT LONG-FLOAT) type)
      ((STRING-CHAR STANDARD-CHAR CHARACTER) 'CHARACTER)
      ((STRING BIT-VECTOR) type)
      (VECTOR (list 'VECTOR (array-element-type thing)))
      (ARRAY (list 'ARRAY (array-element-type thing)))
      #+clos
      (STANDARD-OBJECT 'STANDARD-OBJECT)
      #+clos
      (STRUCTURE-OBJECT 'STRUCTURE-OBJECT)
      (t 'UNKNOWN))))

(defun type-filter (type)
  (case type
        ((FIXNUM CHARACTER SHORT-FLOAT LONG-FLOAT) type)
        (SINGLE-FLOAT 'SHORT-FLOAT)
        (DOUBLE-FLOAT 'LONG-FLOAT)
        ((SIMPLE-STRING STRING) 'STRING)
        ((SIMPLE-BIT-VECTOR BIT-VECTOR) 'BIT-VECTOR)
	((NIL T) t)
        (t
	 (cond
	  #+clos
	  ((subtypep type 'STANDARD-OBJECT) type)
	  #+clos
	  ((subtypep type 'STRUCTURE-OBJECT) type)
	  (t
	   (let ((type (sys::normalize-type type)))
             (case (car type)
	       ((SIMPLE-ARRAY ARRAY)
		(if (endp (cdr type))
		    (list 'ARRAY t) ; Beppe
		    (let ((element-type (cadr type)))
		      (setq element-type
			    (cond
			      ((or (subtypep element-type 'STRING-CHAR)
				   (subtypep element-type 'STANDARD-CHAR)
				   (subtypep element-type 'CHARACTER))
			       'STRING-CHAR)
			      ((subtypep element-type 'BIT) 'BIT)
			      ((subtypep element-type 'FIXNUM) 'FIXNUM)
			      ((or (subtypep element-type 'SHORT-FLOAT)
				   (subtypep element-type 'SINGLE-FLOAT))
			       'SHORT-FLOAT)
			      ((or (subtypep element-type 'LONG-FLOAT)
				   (subtypep element-type 'DOUBLE-FLOAT))
			       'LONG-FLOAT)
			      (t t)))
		      (if (and (cddr type)
			       (not (eq (third type) '*))
			       (= (length (third type)) 1))
			  (case element-type
			    (STRING-CHAR 'STRING)
			    (BIT 'BIT-VECTOR)
			    (t (list 'VECTOR element-type)))
			  (list 'ARRAY element-type)))))
	       (INTEGER
		(if (sys::sub-interval-p (cdr type)
					'#.(list most-negative-fixnum
						 most-positive-fixnum))
		    'FIXNUM
		    t))
	       ((SHORT-FLOAT SINGLE-FLOAT) 'SHORT-FLOAT)
	       ((LONG-FLOAT DOUBLE-FLOAT) 'LONG-FLOAT)
	       ((STREAM) 'STREAM)	; Beppe
	       (t (cond ((subtypep type 'FIXNUM) 'FIXNUM)
			((subtypep type 'CHARACTER) 'CHARACTER)
			((subtypep type 'SHORT-FLOAT) 'SHORT-FLOAT)
			((subtypep type 'LONG-FLOAT) 'LONG-FLOAT)
			((subtypep type '(VECTOR T)) '(VECTOR T))
			((subtypep type 'STRING) 'STRING)
			((subtypep type 'BIT-VECTOR) 'BIT-VECTOR)
			((subtypep type '(VECTOR FIXNUM)) '(VECTOR FIXNUM))
			((subtypep type '(VECTOR SHORT-FLOAT))
			 '(VECTOR SHORT-FLOAT))
			((subtypep type '(VECTOR LONG-FLOAT))
			 '(VECTOR LONG-FLOAT))
			((subtypep type '(ARRAY STRING-CHAR))
			 '(ARRAY STRING-CHAR))
			((subtypep type '(ARRAY BIT)) '(ARRAY BIT))
			((subtypep type '(ARRAY FIXNUM)) '(ARRAY FIXNUM))
			((subtypep type '(ARRAY SHORT-FLOAT))
			 '(ARRAY SHORT-FLOAT))
			((subtypep type '(ARRAY LONG-FLOAT))
			 '(ARRAY LONG-FLOAT))
			((subtypep type '(ARRAY T)) '(ARRAY T)) ; Beppe
			((eq (car type) 'VALUES)
			 (if (null (cddr type))
			     (list 'VALUES (type-filter (second type)))
			     t))
			((and (eq (car type) 'SATISFIES) ; Beppe
			      (symbolp (second type))
			      (get (second type) 'TYPE-FILTER)))
			(t t)))
	       )))))))

;;; The algebra of types should be more complete. Beppe
(defun type-and (type1 type2)
  (cond ((equal type1 type2) type1)
        ((eq type1 t) type2)
	((eq type1 '*) type2)		; optional args
	((eq type1 'OBJECT) type2)	; Beppe
	((eq type2 'OBJECT) type1)	; Beppe
        ((eq type2 t) type1)
	((eq type2 '*) type1)		; optional args
	((and (consp type2) (eq (car type2) 'VALUES))
	 (type-and type1 (second type2)))
        ((consp type1)
         (case (car type1)
               (ARRAY
                (case (cadr type1)
                      (STRING-CHAR
		       (if (eq type2 'STRING)
			   type2
			 (if (and (consp type2)	; Beppe
				  (member (car type2) '(ARRAY VECTOR))
				  (null (cddr type2))
				  (type>= 'STRING-CHAR (cadr type2)))
			     type1 nil)))
                      (BIT (if (eq type2 'BIT-VECTOR) type2 nil))
		      ;; Beppe:
                      (t (case type2
			   (STRING
			    (when (type-and (cadr type1) 'STRING-CHAR)
			      '(ARRAY STRING-CHAR)))
			   (BIT-VECTOR
			    (when (type-and (cadr type1) 'BIT)
			      '(ARRAY BIT)))
			   ((ARRAY VECTOR) type1)
			   (t (if (and (consp type2)
				       (member (car type2) '(ARRAY VECTOR)))
				  (cond ((type>= (cadr type1) (cadr type2))
					 type2)
					((type>= (cadr type2) (cadr type1))
					 type1))))))))
               (VECTOR
                (case (cadr type1)	; Beppe
                      (STRING-CHAR
		       (if (eq type2 'STRING)
			   type2
			 (if (and (consp type2)	; Beppe
				  (member (car type2) '(ARRAY VECTOR))
				  (null (cddr type2))
				  (type>= 'STRING-CHAR (cadr type2)))
			     type1 nil)))
                      (BIT (if (eq type2 'BIT-VECTOR) type2 nil))
		      ;; Beppe
                      (t (case type2
			   (STRING
			    (when (eq (cadr type1) 'STRING-CHAR) type1))
			   (BIT-VECTOR
			    (when (eq (cadr type1) 'BIT) type1))
			   ((ARRAY VECTOR) type1)
			   (t (if (and (consp type2)
				       (member (car type2) '(ARRAY VECTOR)))
				  (cond ((type>= (cadr type1) (cadr type2))
					 type2)
					((type>= (cadr type2) (cadr type1))
					 type1))))))))
	       (values (type-and (second type1) type2)))
	 )
	((and (consp type2) (eq (car type2) 'OR)) ; Beppe
	 (do ((types (cdr type2) (cdr types))
	      (res))
	     ((null types) nil)
	     (when (setq res (type-and type1 (car types)))
	       (return res))))
        (t (case type1
                 (STRING
                  (if (and (consp type2) (eq (car type2) 'ARRAY)
                           (eq (cadr type2) 'STRING-CHAR))
                      type1 nil))
                 (BIT-VECTOR
                  (if (and (consp type2) (eq (car type2) 'ARRAY)
                           (eq (cadr type2) 'BIT))
                      type1 nil))
                 (FIXNUM-FLOAT
                  (if (member type2 '(FIXNUM FLOAT SHORT-FLOAT LONG-FLOAT))
                      type2 nil))
                 (FLOAT
                  (if (member type2 '(SHORT-FLOAT LONG-FLOAT))
                      type2 nil))
                 ((LONG-FLOAT SHORT-FLOAT)
                  (if (member type2 '(FIXNUM-FLOAT FLOAT))
                      type1 nil))
;;;		 ((SIGNED-CHAR UNSIGNED-CHAR SIGNED-SHORT)
;;;		  (if (eq type2 'FIXNUM) type1 nil))
;;;		 ((UNSIGNED-SHORT)
;;;		  (if (subtypep type1 type2) type1 nil))
                 (FIXNUM
		  (case type2
		    (FIXNUM-FLOAT 'FIXNUM)
;;;		    ((SIGNED-CHAR UNSIGNED-CHAR SIGNED-SHORT BIT)
;;;		     type2)
;;;		    ((UNSIGNED-SHORT)
;;;		     (if (subtypep type2 type1) type2 nil))
		    ))
		 #+clos
		 (STANDARD-OBJECT
                  (if (subtypep type2 'STANDARD-OBJECT) type2 nil))
		 #+clos
		 (STRUCTURE-OBJECT
                  (if (subtypep type2 'STRUCTURE-OBJECT) type2 nil))))))

(defun type>= (type1 type2)
  (equal (type-and type1 type2) type2))

(defun reset-info-type (info)
  (if (info-type info)
      (let ((info1 (copy-info info)))
           (setf (info-type info1) t)
           info1)
      info))

;;;
;;; and-form-type
;;;   returns a copy of form whose type is the type-and of type and the form's
;;;   type
;;;
(defun and-form-type (type form original-form)
  (let* ((type2 (info-type (cadr form)))
	 (type1 (or (type-and type type2)
		    (when (subtypep type2 type) type2)))) ; class types. Beppe
    (if type1
	(if (eq type1 type2)
	    form
	    (let ((info (copy-info (cadr form))))
	      (setf (info-type info) type1)
	      (list* (car form) info (cddr form))))
	(progn
	  (cmpwarn "The type of the form ~s is not ~s." original-form type)
	  form))))

(defun check-form-type (type form original-form)
  (when (null (type-and type (info-type (cadr form))))
        (cmpwarn "The type of the form ~s is not ~s." original-form type)))

(defun default-init (type)
  (case type
    (FIXNUM (cmpwarn "The default value of NIL is not FIXNUM.")
	    (c1constant-value 0 nil))	; use #. in final version
    (CHARACTER (cmpwarn "The default value of NIL is not CHARACTER.")
	       (c1constant-value #\space nil))
    (LONG-FLOAT (cmpwarn "The default value of NIL is not LONG-FLOAT.")
		(c1constant-value 0.0L1 nil))
    (SHORT-FLOAT (cmpwarn "The default value of NIL is not SHORT-FLOAT.")
		(c1constant-value 0.0S1 nil))
    (t (c1nil))))
