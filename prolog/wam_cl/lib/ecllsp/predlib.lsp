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


(in-package "CL")
(export '(DEFTYPE TYPEP SUBTYPEP COERCE #+clos subclassp))

(in-package "SYSTEM")

;;; DEFTYPE macro.
(defmacro deftype (name lambda-list &rest body)
  "Syntax: (deftype name lambda-list {decl | doc}* {form}*)
Defines a new type-specifier abbreviation in terms of an 'expansion' function
	(lambda lambda-list1 {DECL}* {FORM}*)
where LAMBDA-LIST1 is identical to LAMBDA-LIST except that all optional
parameters with no default value specified in LAMBDA-LIST defaults to the
symbol '*', but not to NIL.  When the type system of ECL encounters a type
specifier (NAME arg1 ... argn), it calls the expansion function with the
arguments ARG1 ... ARGn, and uses the returned value instead of the original
type specifier.  When the symbol NAME is used as a type specifier, the
expansion function is called with no argument.
The doc-string DOC, if supplied, is saved as a TYPE doc and can be retrieved
by (documentation 'NAME 'type)."
  (multiple-value-bind (body doc)
      (remove-documentation body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
          (put-sysprop ',name 'DEFTYPE-FORM '(DEFTYPE ,name ,lambda-list ,@body))
          (put-sysprop ',name 'DEFTYPE-DEFINITION #'(LAMBDA ,lambda-list ,@body))
	  ,@(si::expand-set-documentation name 'type doc)
          ',name)))


;;; Some DEFTYPE definitions.
(deftype boolean ()
  "A BOOLEAN is an object which is either NIL or T."
  `(member nil t))

(deftype fixnum ()
  "A FIXNUM is an integer between MOST-NEGATIVE-FIXNUM (= - 2^29 in ECL) and
MOST-POSITIVE-FIXNUM (= 2^29 - 1 in ECL) inclusive.  Other integers are
bignums."
  `(INTEGER #.most-negative-fixnum #.most-positive-fixnum))

(deftype byte8 () `(INTEGER 0 255))
(deftype integer8 () `(INTEGER -128 127))

(deftype real (&rest foo) '(OR RATIONAL FLOAT))

(deftype single-float (&rest args)
  (if args
      `(short-float ,@args)
      'short-float))

(deftype double-float (&rest args)
  (if args
      `(long-float ,@args)
      'long-float))

(deftype bit ()
  "A BIT is either integer 0 or 1."
  '(INTEGER 0 1))

(deftype mod (n)
  `(INTEGER 0 ,(1- n)))

(deftype compiled-function () 'FUNCTION)

(deftype signed-byte (&optional s)
  "As a type specifier, (SIGNED-BYTE n) specifies those integers that can be
represented with N bits in 2's complement representation."
  (if (or (null s) (eq s '*))
      '(INTEGER * *)
      `(INTEGER ,(- (expt 2 (1- s))) ,(1- (expt 2 (1- s))))))

(deftype unsigned-byte (&optional s)
  "As a type specifier, (UNSIGNED-BYTE n) specifies non-negative integers that
can be represented with N bits."
  (if (or (null s) (eq s '*))
      '(INTEGER 0 *)
      `(INTEGER 0 ,(1- (expt 2 s)))))

(deftype sequence () '(OR CONS NULL (ARRAY * (*))))
(deftype list ()
  "As a type specifier, LIST is used to specify the type consisting of NIL and
cons objects.  In our ordinary life with Lisp, however, a list is either NIL
or a cons whose cdr is a list, and is notated by its elements surrounded with
parentheses.
The backquote macro is sometimes useful to construct a complicated list
structure.  When evaluating `(...)
	,form embeds the value of FORM,
	,@form and ,.form embed all elements of the list value of FORM,
	and other things embed itself
into the structure at their position.  For example,
	`(a b ,c d e) expands to (list* 'a 'b c '(d e))
	`(a b ,@c d e) expands to (list* 'a 'b (append c '(d e)))
	`(a b ,.c d e) expands to (list* 'a 'b (nconc c '(d e)))"
  '(OR CONS NULL))

(deftype atom ()
  "An ATOM is an object that is not a CONS."
  '(NOT CONS))
;(deftype null () '(MEMBER NIL))

(deftype vector (&optional (element-type '*) (size '*))
  "A vector is a one-dimensional array.  Strings and bit-vectors are kinds of
vectors.  Other vectors are called general vectors and are notated as
	#(elem ... elem)
Some vectors may be displaced to another array, may have a fill-pointer, or
may be adjustable.  Other vectors are called simple-vectors."
  `(array ,element-type (,size)))

(deftype extended-char ()
  '(and character (not base-char)))

(deftype string (&optional size)
  "A string is a vector of characters.  A string is notated by surrounding the
characters with double quotes.  Some strings may be displaced to another
string, may have a fill-pointer, or may be adjustable.  Other strings are
called simple-strings."
  (if size `(array character (,size)) '(array character (*))))

(deftype base-string (&optional size)
  (if size `(array base-char (,size)) '(array base-char (*))))
(deftype bit-vector (&optional size)
  (if size `(array bit (,size)) '(array bit (*))))

(deftype simple-vector (&optional size)
  "A simple-vector is a vector that is not displaced to another array, has no
fill-pointer, and is not adjustable."
  (if size `(simple-array t (,size)) '(simple-array t (*))))

(deftype simple-string (&optional size)
  "A simple-string is a string that is not displaced to another array, has no
fill-pointer, and is not adjustable."
  (if size `(simple-array character (,size)) '(simple-array character (*))))

(deftype simple-base-string (&optional size)
  (if size `(simple-array base-char (,size)) '(simple-array base-char (*))))

(deftype simple-bit-vector (&optional size)
  "A simple-bit-vector is a bit-vector that is not displaced to another array,
has no fill-pointer, and is not adjustable."
  (if size `(simple-array bit (,size)) '(simple-array bit (*))))

;;************************************************************
;;			TYPEP
;;************************************************************

(defun constantly-t (&rest foo)
  t)

(defun constantly-nil (&rest foo)
  nil)

(defun simple-array-p (x)
  (and (arrayp x)
       (not (adjustable-array-p x))
       (not (array-has-fill-pointer-p x))
       (not (array-displacement x))))

(dolist (l '((ARRAY . ARRAYP)
	     (ATOM . ATOM)
	     (EXTENDED-CHAR . CONSTANTLY-NIL)
	     (BASE-CHAR . CHARACTERP)
	     (CHARACTER . CHARACTERP)
	     (COMMON . COMMONP)
	     (COMPILED-FUNCTION . COMPILED-FUNCTION-P)
	     (COMPLEX . COMPLEXP)
	     (CONS . CONSP)
	     (DISPATCH-FUNCTION . DISPATCH-FUNCTION-P)
	     (FLOAT . FLOATP)
	     (FUNCTION . FUNCTIONP)
	     (HASH-TABLE . HASH-TABLE-P)
	     (INTEGER . INTEGERP)
	     (KEYWORD . KEYWORDP)
	     (LIST . LISTP)
	     (LOGICAL-PATHNAME . LOGICAL-PATHNAME-P)
	     (NIL . CONSTANTLY-NIL)
	     (NULL . NULL)
	     (NUMBER . NUMBERP)
	     (PACKAGE . PACKAGEP)
	     (RANDOM-STATE . RANDOM-STATE-P)
	     (RATIONAL . RATIONALP)
	     (PATHNAME . PATHNAMEP)
	     (READTABLE . READTABLEP)
	     (REAL . REALP)
	     (SIMPLE-ARRAY . SIMPLE-ARRAY-P)
	     (SIMPLE-STRING . SIMPLE-STRING-P)
	     (SIMPLE-VECTOR . SIMPLE-VECTOR-P)
	     (STREAM . STREAMP)
	     (STRING . STRINGP)
	     (STRUCTURE . SYS:STRUCTUREP)
	     (SYMBOL . SYMBOLP)
	     (T . CONSTANTLY-T)
	     (VECTOR . VECTORP)
	     ))
  (put-sysprop (car l) 'TYPE-PREDICATE (cdr l)))

(defconstant +upgraded-array-element-types+
  '(BIT BASE-CHAR BYTE8 INTEGER8 FIXNUM SHORT-FLOAT LONG-FLOAT T))

(defun upgraded-array-element-type (element-type &optional env)
  (dolist (v +upgraded-array-element-types+ 'T)
    (when (subtypep element-type v)
      (return v))))

(defun upgraded-complex-part-type (real-type &optional env)
  (dolist (v '(INTEGER RATIO RATIONAL SHORT-FLOAT LONG-FLOAT FLOAT)
	   (error "~S is not a valid part type for a complex." real-type))
    (when (subtypep real-type v)
      (return v))))

(defun in-interval-p (x interval)
  (declare (si::c-local))
  (let* (low high)
    (if (endp interval)
        (setq low '* high '*)
        (if (endp (cdr interval))
            (setq low (car interval) high '*)
            (setq low (car interval) high (second interval))))
    (cond ((eq low '*))
          ((consp low)
           (when (<= x (car low)) (return-from in-interval-p nil)))
          ((when (< x low) (return-from in-interval-p nil))))
    (cond ((eq high '*))
          ((consp high)
           (when (>= x (car high)) (return-from in-interval-p nil)))
          ((when (> x high) (return-from in-interval-p nil))))
    (return-from in-interval-p t)))

(defun error-type-specifier (type)
  (declare (si::c-local))
  (error "~S is not a valid type specifier." type))

(defun match-dimensions (array pat)
  (declare (si::c-local))
  (or (eq pat '*)
      (let ((rank (array-rank array)))
	(cond ((numberp pat) (= rank pat))
	      ((listp pat)
	       (dotimes (i rank (null pat))
		 (unless (and (consp pat)
			      (or (eq (car pat) '*)
				  (eql (array-dimension array i) (car pat))))
		   (return nil))
		 (setq pat (cdr pat))))
	      ((atom pat)
	       (error "~S does not describe array dimensions." pat))))))

(defun typep (object type &optional env &aux tp i c)
  "Args: (object type)
Returns T if X belongs to TYPE; NIL otherwise."
  (declare (ignore env))
  (cond ((symbolp type)
	 (let ((f (get-sysprop type 'TYPE-PREDICATE)))
	   (cond (f (return-from typep (funcall f object)))
		 ((eq (type-of object) type) (return-from typep t))
		 (t (setq tp type i nil)))))
	((consp type)
	 (setq tp (car type) i (cdr type)))
	#+clos
	((sys:instancep type)
	 (return-from typep (subclassp (class-of object) type)))
	(t
	 (error-type-specifier type)))
  (case tp
    ((EQL MEMBER) (and (member object i) t))
    (NOT (not (typep object (car i))))
    (OR (dolist (e i)
	  (when (typep object e) (return t))))
    (AND (dolist (e i t)
	   (unless (typep object e) (return nil))))
    (SATISFIES (funcall (car i) object))
    ((T) t)
    ((NIL) nil)
    (FIXNUM (eq (type-of object) 'FIXNUM))
    (BIGNUM (eq (type-of object) 'BIGNUM))
    (RATIO (eq (type-of object) 'RATIO))
    (STANDARD-CHAR
     (and (characterp object) (standard-char-p object)))
    (INTEGER
     (and (integerp object) (in-interval-p object i)))
    (RATIONAL
     (and (rationalp object) (in-interval-p object i)))
    (FLOAT
     (and (floatp object) (in-interval-p object i)))
    (REAL
     (and (or (rationalp object) (floatp object)) (in-interval-p object i)))
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
    (CONS (and (consp object)
	       (or (endp i) (typep (car object) (first i)))
	       (or (endp (cdr i)) (typep (cdr object) (second i)))))
    (STRING
     (and (stringp object)
          (or (null i) (match-dimensions object i))))
    (BIT-VECTOR
     (and (bit-vector-p object)
          (or (null i) (match-dimensions object i))))
    (SIMPLE-STRING
     (and (simple-string-p object)
          (or (null i) (match-dimensions object i))))
    (SIMPLE-BIT-VECTOR
     (and (simple-bit-vector-p object)
          (or (null i) (match-dimensions object i))))
    (SIMPLE-VECTOR
     (and (simple-vector-p object)
          (or (null i) (match-dimensions object i))))
    (SIMPLE-ARRAY
     (and (simple-array-p object)
          (or (endp i) (eq (car i) '*)
	      ;; (car i) needs expansion
	      (eq (array-element-type object)
		  (upgraded-array-element-type (car i))))
          (or (endp (cdr i)) (match-dimensions object (second i)))))
    (ARRAY
     (and (arrayp object)
          (or (endp i) (eq (car i) '*)
              ;; Or the element type of object should be EQUAL to (car i).
              ;; Is this too strict?
              (eq (array-element-type object)
		  (upgraded-array-element-type (car i))))
          (or (endp (cdr i)) (match-dimensions object (second i)))))
    (t
     (cond
           ((get-sysprop tp 'DEFTYPE-DEFINITION)
            (typep object (apply (get-sysprop tp 'DEFTYPE-DEFINITION) i)))
	   ((consp i)
	    (error-type-specifier type))
           #+clos
	   ((setq c (find-class type nil))
	    ;; Follow the inheritance chain
	    (subclassp (class-of object) c))
	   #-clos
	   ((get-sysprop tp 'IS-A-STRUCTURE)
            (when (sys:structurep object)
	      ;; Follow the chain of structure-include.
	      (do ((stp (sys:structure-name object)
			(get-sysprop stp 'STRUCTURE-INCLUDE)))
		  ((eq tp stp) t)
		(when (null (get-sysprop stp 'STRUCTURE-INCLUDE))
		  (return nil)))))
	   (t
	    (error-type-specifier type))))))

#+clos
(defun subclassp (low high)
  (or (eq low high)
      (dolist (class (sys:instance-ref low 1)) ; (class-superiors low)
	(when (subclassp class high) (return t)))))

#+(and clos ecl-min)
(defun clos::classp (foo)
  (declare (ignore foo))
  nil)

;;************************************************************
;;			NORMALIZE-TYPE
;;************************************************************
;; NORMALIZE-TYPE normalizes the type using the DEFTYPE definitions.
;; The result is a pair of values
;;  VALUE-1 = normalized type name or object
;;  VALUE-2 = normalized type arguments or nil
(defun normalize-type (type &aux tp i fd)
  ;; Loops until the car of type has no DEFTYPE definition.
  (cond ((symbolp type)
	 (if (setq fd (get-sysprop type 'DEFTYPE-DEFINITION))
	   (normalize-type (funcall fd))
	   (values type nil)))
	#+clos
	((clos::classp type) (values type nil))
	((atom type)
	 (error-type-specifier type))
	((progn
	   (setq tp (car type) i (cdr type))
	   (setq fd (get-sysprop tp 'DEFTYPE-DEFINITION)))
	 (normalize-type (apply fd i)))
	((and (eq tp 'INTEGER) (consp (cadr i)))
	 (values tp (list (car i) (1- (caadr i)))))
	(t (values tp i))))

(defun expand-deftype (type)
  (cond ((symbolp type)
	 (let ((fd (get-sysprop type 'DEFTYPE-DEFINITION)))
	   (if fd
	       (expand-deftype (funcall fd))
	       type)))
	((and (consp type)
	      (symbolp type))
	 (let ((fd (get-sysprop (first type) 'DEFTYPE-DEFINITION)))
	   (if fd
	       (expand-deftype (funcall fd (rest type)))
	       type)))
	(t
	 type)))

;;************************************************************
;;			COERCE
;;************************************************************

(defun error-coerce (object type)
  (error "Cannot coerce ~S to type ~S." object type))

(defun coerce (object type &aux aux)
  "Args: (x type)
Coerces X to an object of the specified type, if possible.  Signals an error
if not possible."
  (when (typep object type)
    ;; Just return as it is.
    (return-from coerce object))
  (setq type (expand-deftype type))
  (cond ((atom type)
	 (case type
	   ((T) object)
	   (LIST
	    (do ((l nil (cons (elt object i) l))
		 (i (1- (length object)) (1- i)))
		((< i 0) l)
	      (declare (fixnum i))))
	   ((CHARACTER BASE-CHAR) (character object))
	   (FLOAT (float object))
	   ((SINGLE-FLOAT SHORT-FLOAT) (float object 0.0S0))
	   ((DOUBLE-FLOAT LONG-FLOAT) (float object 0.0L0))
	   (COMPLEX (complex (realpart object) (imagpart object)))
	   (FUNCTION (coerce-to-function object))
	   ((VECTOR SIMPLE-VECTOR SIMPLE-STRING STRING BIT-VECTOR SIMPLE-BIT-VECTOR)
	    (concatenate type object))
	   (t
	    (if (or (listp object) (vector object))
		(concatenate type object)
		(error-coerce object type)))))
	((eq (setq aux (first type)) 'COMPLEX)
	 (if type
	     (complex (coerce (realpart object) (second type))
		      (coerce (imagpart object) (second type)))
	     (complex (realpart object) (imagpart object))))
	((member aux '(SINGLE-FLOAT SHORT-FLOAT DOUBLE-FLOAT LONG-FLOAT FLOAT))
	 (setq aux (coerce object aux))
	 (unless (typep object type)
	   (error-coerce object type)))
	((eq aux 'AND)
	 (coerce object (second type))
	 (unless (typep object type)
	   (error-coerce object type)))
	((or (listp object) (vector object))
	 (concatenate type object))
	(t
	 (error-coerce object type))))
	    
;;************************************************************
;;			SUBTYPEP
;;************************************************************
;;
;; TYPES LATTICE (Following Henry Baker's paper)
;;
;; The algorithm works as follows. Types are identified with sets. Some sets
;; are elementary, in the sense that other types may be expressed as
;; combination of them. We partition these sets into FAMILIES
;;
;;	Built-in objects --- Hash tables, etc
;;	Intervals --- (INTEGER a b), (REAL a b), etc
;;	Arrays --- (ARRAY * (2)), etc
;;	Classes
;;
;; When passed a type specifier, ECL canonicalizes it: it decomposes the
;; type into the most elementary sets, assigns a unique bit pattern (TAG) to
;; each of these sets, and builds a composite tag for the type by LOGIOR.
;; Operations between these sets reduce to logical operations between these
;; bit patterns. Given types T1, T2 and a function which produces tags f(T)
;;
;;	f((AND T1 T2)) = (LOGIAND f(T1) f(T2))
;;	f((OR T1 T2)) = (LOGIOR f(T1) f(T2))
;;	f((NOT T1)) = (LOGNOT f(T2))
;;
;; However, tags are not permanent: whenever a new type is registered, the
;; tag associated to a type may be changed (for instance, because new
;; elementary sets are discovered, which also belong to existing types).

(defparameter *save-types-database* nil)

(defparameter *highest-type-tag*
  #+ecl-min #B1
  #-ecl-min '#.*highest-type-tag*)

(defparameter *member-types*
  #+ecl-min NIL
  #-ecl-min '#.*member-types*)

(defparameter *intervals-mask* #B1)

(defparameter *elementary-types*
  #+ecl-min
  '()
  #-ecl-min
  '#.*elementary-types*)

(defun new-type-tag ()
  (declare (si::c-local))
  (prog1 *highest-type-tag*
    (setq *highest-type-tag* (ash *highest-type-tag* 1))))

;; Find out the tag for a certain type, if it has been already registered.
;;
(defun find-registered-tag (type &optional (test #'equal))
  (declare (si::c-local))
  (let* ((pos (assoc type *elementary-types* :test test)))
    (and pos (cdr pos))))

;; We are going to make changes in the types database. Save a copy if this
;; will cause trouble.
;;
(defun maybe-save-types ()
  (declare (si::c-local))
  (when *save-types-database*
    (setf *save-types-database* nil
	  *elementary-types* (copy-tree *elementary-types*)
	  *member-types* (copy-tree *member-types*))))

;; We have created and tagged a new type (NEW-TAG). However, there are
;; composite and synonym types registered around which are supertypes of
;; this type and need to be tagged. TYPE-MASK is a bit pattern which helps
;; us in recognizing these supertypes.
;;
(defun update-types (type-mask new-tag)
  (declare (si::c-local))
  (maybe-save-types)
  (dolist (i *elementary-types*)
    (unless (zerop (logand (cdr i) type-mask))
      (setf (cdr i) (logior new-tag (cdr i))))))

;; FIND-TYPE-BOUNDS => (VALUES TAG-SUPER TAG-SUB)
;;
;; This function outputs two values: TAG-SUB, the tag for the union-type of all
;; types which are subtypes of the supplied one; and TAG-SUPER, which is either
;; the tag for the union-type of all types which a supertype of the supplied
;; one (MINIMIZE-SUPER = NIL) or the tag for the smallest type which is a
;; supertype of the given one (MINIMIZE-SUPER = TRUE). The search process is
;; restricted to types in the same family class.
;;
;; A value of MINIMIZE-SUPER = TRUE only makes sense for some families (such
;; as semi-infinite intervals), for which (SUBTYPEP T1 T2) = T and (SUBTYPEP T1
;; T3) = T implies either (SUBTYPEP T2 T3) = T or (SUBTYPEP T3 T2) = T.
;;
(defun find-type-bounds (type in-our-family-p type-<= minimize-super)
  (declare (si::c-local))
  (let* ((subtype-tag 0)
	 (disjoint-tag 0)
	 (supertype-tag (if minimize-super -1 0)))
    (dolist (i *elementary-types*)
      (let ((other-type (car i))
	    (other-tag (cdr i)))
	(when (funcall in-our-family-p other-type)
	  (cond ((funcall type-<= type other-type)
		 (if minimize-super
		     (when (zerop (logandc2 other-tag supertype-tag))
		       (setq supertype-tag other-tag))
		     (setq supertype-tag (logior other-tag supertype-tag))))
		((funcall type-<= other-type type)
		 (setq subtype-tag (logior other-tag subtype-tag)))
		(t
		 (setq disjoint-tag (logior disjoint-tag other-tag)))))))
    (values (if (= supertype-tag -1) 0
		(logandc2 supertype-tag (logior disjoint-tag subtype-tag)))
	    subtype-tag)))

;; A new type is to be registered, which is not simply a composition of
;; previous types. A new tag has to be created, and all supertypes are to be
;; tagged. Here we have to distinguish two possibilities: first, a supertype
;; may belong to the same family (intervals, arrays, etc); second, some
;; supertypes may be basic types (NUMBER is a supertype for (INTEGER 0 2),
;; for instance). The first possibility is detected with the comparison
;; procedure, TYPE-<=; the second possibility is detected by means of tags.
;;
(defun register-type (type in-our-family-p type-<=)
  (declare (si::c-local))
  (or (find-registered-tag type)
      (multiple-value-bind (tag-super tag-sub)
	  (find-type-bounds type in-our-family-p type-<= nil)
	(let ((tag (logior (new-type-tag) tag-sub)))
	  (update-types (logandc2 tag-super tag-sub) tag)
	  (push (cons type tag) *elementary-types*)
	  tag))))

;;----------------------------------------------------------------------
;; MEMBER types. We register this object in a separate list, *MEMBER-TYPES*,
;; and tag all types to which it belongs.
;;
(defun register-member-type (object)
  (declare (si::c-local))
  (let ((pos (assoc object *member-types*)))
    (or (and pos (cdr pos))
	;; We convert number into intervals, so that (AND INTEGER (NOT
	;; (EQL 10))) is detected as a subtype of (OR (INTEGER * 9)
	;; (INTEGER 11 *)).
	(and (numberp object)
	     (let* ((base-type (if (integerp object) 'INTEGER (type-of object)))
		    (type (list base-type object object)))
	       (or (find-registered-tag type)
		   (register-interval-type type))))
	(let* ((tag (new-type-tag)))
	  (maybe-save-types)
	  (setq *member-types* (acons object tag *member-types*))
	  (dolist (i *elementary-types*)
	    (let ((type (car i)))
	      (when (typep object type)
		(setf (cdr i) (logior tag (cdr i))))))
	  tag))))

;;----------------------------------------------------------------------
;; SATISFIES types. Here we should signal some error which is caught
;; somewhere up, to denote failure of the decision procedure.
;;
(defun register-satisfies-type (type)
  (declare (si::c-local))
  (throw '+canonical-type-failure+ 'satisfies))

;;----------------------------------------------------------------------
;; CLOS classes and structures.
;;
(defun register-class (class)
  (declare (si::c-local))
  (or (find-registered-tag class)
      ;; We do not need to register classes which belong to the core type
      ;; system of LISP (ARRAY, NUMBER, etc).
      (let* ((name (class-name class)))
	(and name
	     (eq class (find-class name 'nil))
	     (if (eq name 'T) -1 (find-registered-tag name))))
      (register-type class
		     #'(lambda (c) (or (si::instancep c) (symbolp c)))
		     #'(lambda (c1 c2)
			 (when (symbolp c1)
			   (setq c1 (find-class c1 nil)))
			 (when (symbolp c2)
			   (setq c2 (find-class c2 nil)))
			 (and c1 c2 (subclassp c1 c2))))))

;;----------------------------------------------------------------------
;; ARRAY types.
;;
(defun register-array-type (type)
  (declare (si::c-local))
  (multiple-value-bind (array-class elt-type dimensions)
      (parse-array-type type)
    (cond ((eq elt-type '*)
	   (canonical-type `(OR ,@(mapcar #'(lambda (type) `(,array-class ,type ,dimensions))
					  +upgraded-array-element-types+))))
	  ((find-registered-tag (setq type (list array-class elt-type dimensions))))
	  (t
	   #+nil
	   (when (and (consp dimensions) (> (count-if #'numberp dimensions) 1))
	     (dotimes (i (length dimensions))
	       (when (numberp (elt dimensions i))
		 (let ((dims (make-list (length dimensions) :initial-element '*)))
		   (setf (elt dims i) (elt dimensions i))
		   (register-type (list array-class elt-type dims)
				  #'array-type-p #'array-type-<=)))))
	   (register-type type #'array-type-p #'array-type-<=)))))

;;
;; We look for the most specialized type which is capable of containing
;; this object. LIST always contains 'T, so that this procedure never
;; fails. It is faster than UPGRADED-... because we use the tags of types
;; that have been already registered.
;;
(defun fast-upgraded-array-element-type (type)
  (declare (si::c-local))
  (if (eql type '*)
      '*
      (let* ((tag (or (canonical-type type) -1)))
	(dolist (other-type +upgraded-array-element-types+ 'T)
	  (when (zerop (logand tag (lognot (canonical-type other-type))))
	    (return other-type))))))

;;
;; This canonicalizes the array type into the form
;;	({ARRAY | SIMPLE-ARRAY} {elt-type | '*} {'* | (['*]*)})
;;
;; ELT-TYPE is the upgraded element type of the input.
;;
(defun parse-array-type (input)
  (declare (si::c-local))
  (let* ((type input)
	 (name (pop type))
	 (elt-type (fast-upgraded-array-element-type (if type (pop type) '*)))
	 (dims (if type (pop type) '*)))
    (when type
      (error "Wrong array type designator ~S." input))
    (cond ((numberp dims)
	   (unless (< -1 dims array-rank-limit)
	     (error "Wrong rank size array type ~S." input))
	   (setq dims (nthcdr (- array-rank-limit dims)
			      '#.(make-list array-rank-limit :initial-element '*))))
	  ((consp dims)
	   (dolist (i dims)
	     (unless (or (eq i '*)
			 (and (integerp i) (< -1 i array-dimension-limit)))
	       (error "Wrong dimension size in array type ~S." input)))))
    (values name elt-type dims)))

;;
;; This function checks whether the array type T1 is a subtype of the array
;; type T2.
;;
(defun array-type-<= (t1 t2)
  (unless (and (or (eq (first t1) (first t2))
		   (eq (first t2) 'ARRAY))
	       (eq (second t1) (second t2)))
    (return-from array-type-<= nil))
  (let ((dim (third t1))
	(pat (third t2)))
    (cond ((eq pat '*) t)
	  ((eq dim '*) nil)
	  (t (do ((a dim (cdr a))
		  (b pat (cdr b)))
		 ((or (endp a)
		      (endp b)
		      (not (or (eq (car b) '*)
			       (eql (car b) (car a)))))
		  (and (null a) (null b)))
	       )))))

(defun array-type-p (type)
  (and (consp type)
       (member (first type) '(ARRAY SIMPLE-ARRAY))))

;;----------------------------------------------------------------------
;; INTERVALS:
;;
;; Arbitrary intervals may be defined as the union or intersection of
;; semi-infinite intervals, of the form (number-type b *), where B is
;; either a real number, a list with one real number or *.
;; Any other interval, may be defined using these. For instance
;;  (INTEGER 0 2) = (AND (INTEGER 0 *) (NOT (INTEGER (2) *)))
;;  (SHORT-FLOAT (0.2) (2)) = (AND (SHORT-FLOAT (0.2) *) (NOT (SHORT-FLOAT 2 *)))

(defun register-elementary-interval (type b)
  (declare (si::c-local))
  (setq type (list type b))
  (or (find-registered-tag type #'equalp)
      (multiple-value-bind (tag-super tag-sub)
	  (find-type-bounds type
			    #'(lambda (other-type)
				(and (consp other-type)
				     (null (cddr other-type))))
			    #'(lambda (i1 i2)
				(and (eq (first i1) (first i2))
				     (bounds-<= (second i2) (second i1))))
			    t)
	(let ((tag (new-type-tag)))
	  (update-types (logandc2 tag-super tag-sub) tag)
	  (setq tag (logior tag tag-sub))
	  (push (cons type tag) *elementary-types*)
	  tag))))

(defun register-interval-type (interval)
  (declare (si::c-local))
  (let* ((i interval)
	 (type (pop i))
	 (low (if i (pop i) '*))
	 (high (if i (pop i) '*))
	 (tag-high (cond ((eq high '*)
			  0)
			 ((eq type 'INTEGER)
			  (setq high (if (consp high)
					 (ceiling (first high))
					 (floor (1+ high))))
			  (register-elementary-interval type high))
			 ((consp high)
			  (register-elementary-interval type (first high)))
			 (t
			  (register-elementary-interval type (list high)))))
	 (tag-low (register-elementary-interval type
		    (cond ((or (eq '* low) (not (eq type 'INTEGER)) (integerp low))
			   low)
			  ((consp low)
			   (floor (1+ (first low))))
			  (t
			   (ceiling low)))))
	 (tag (logandc2 tag-low tag-high)))
    (unless (eq high '*)
      (push (cons interval tag) *elementary-types*))
    tag))

;; All comparisons between intervals operations may be defined in terms of
;;
;;	(BOUNDS-<= b1 b2)	and	(BOUNDS-< b1 b2)
;;
;; The first one checks whether (REAL b2 *) is contained in (REAL b1 *). The
;; second one checks whether (REAL b2 *) is strictly contained in (REAL b1 *)
;; (i.e., (AND (REAL b1 *) (NOT (REAL b2 *))) is not empty).
;;
(defun bounds-<= (b1 b2)
  (cond ((eq b1 '*) t)
	((eq b2 '*) nil)
	((consp b1)
	 (if (consp b2)
	     (<= (first b1) (first b2))
	     (< (first b1) b2)))
	((consp b2)
	 (<= b1 (first b2)))
	(t
	 (<= b1 b2))))

(defun bounds-< (b1 b2)
  (cond ((eq b1 '*) (not (eq b2 '*)))
	((eq b2 '*) nil)
	((consp b1)
	 (if (consp b2)
	     (< (first b1) (first b2))
	     (< (first b1) b2)))
	((consp b2)
	 (<= b1 (first b2)))
	(t
	 (< b1 b2))))

;;----------------------------------------------------------------------
;; COMPLEX types. We do not need to register anything, because all
;; possibilities have been covered by the definitions above. We only have to
;; bring the type to canonical form, which is a union of all specialized
;; complex types that can store an element of the corresponding type.
;;
(defun canonical-complex-type (real-type)
  (declare (si::c-local))
  (canonical-type `(COMPLEX ,(upgraded-complex-part-type (or real-type 'REAL)))))

;;----------------------------------------------------------------------
;; CONS types. Only (CONS T T) and variants, as well as (CONS NIL *), etc
;; are strictly supported.
;;
(defun register-cons-type (&optional (car-type '*) (cdr-type '*))
  (let ((car-tag (if (eq car-type '*) -1 (canonical-type car-type)))
	(cdr-tag (if (eq cdr-type '*) -1 (canonical-type cdr-type))))
    (cond ((or (zerop car-tag) (zerop cdr-tag))
	   0)
	  ((and (= car-tag -1) (= cdr-tag -1))
	   (canonical-type 'CONS))
	  (t
	   (throw '+canonical-type-failure+ 'cons)))))		     

;;----------------------------------------------------------------------
;; (CANONICAL-TYPE TYPE)
;;
;; This function registers all types mentioned in the given expression,
;; and outputs a code corresponding to the represented type. This
;; function has side effects: it destructively modifies the content of
;; *ELEMENTARY-TYPES* and *MEMBER-TYPES*.
;;
(defun canonical-type (type)
  (declare (notinline clos::classp)
	   (si::cl-local))
  (cond ((find-registered-tag type))
	((eq type 'T) -1)
	((eq type 'NIL) 0)
        ((symbolp type)
	 (let ((expander (get-sysprop type 'DEFTYPE-DEFINITION)))
	   (if expander
	       (canonical-type (funcall expander))
	       (let ((class (find-class type nil)))
		 (if class
		     (register-class class)
		     (throw '+canonical-type-failure+ nil))))))
	((consp type)
	 (case (first type)
	   (AND (apply #'logand (mapcar #'canonical-type (rest type))))
	   (OR (apply #'logior (mapcar #'canonical-type (rest type))))
	   (NOT (lognot (canonical-type (second type))))
	   ((EQL MEMBER) (apply #'logior (mapcar #'register-member-type (rest type))))
	   (SATISFIES (register-satisfies-type type))
	   ((INTEGER SHORT-FLOAT LONG-FLOAT RATIO)
	    (register-interval-type type))
	   ((FLOAT)
	    (canonical-type `(OR (SHORT-FLOAT ,@(rest type))
			      (LONG-FLOAT ,@(rest type)))))
	   ((REAL)
	    (canonical-type `(OR (INTEGER ,@(rest type))
			      (RATIO ,@(rest type))
			      (SHORT-FLOAT ,@(rest type))
			      (LONG-FLOAT ,@(rest type)))))
	   ((RATIONAL)
	    (canonical-type `(OR (INTEGER ,@(rest type))
			      (RATIO ,@(rest type)))))
	   (COMPLEX (canonical-complex-type (second type)))
	   (CONS (apply #'register-cons-type (rest type)))
	   ((ARRAY SIMPLE-ARRAY) (register-array-type type))
	   (t (let ((expander (get-sysprop (first type) 'DEFTYPE-DEFINITION)))
		(if expander
		    (canonical-type (apply expander (rest type)))
		    (unless (assoc (first type) *elementary-types*)
		      (throw '+canonical-type-failure+ nil)))))))
	((clos::classp type)
	 (register-class type))
	(t
	 (error-type-specifier type))))

(defun safe-canonical-type (type)
  (declare (si::c-local))
  (catch '+canonical-type-failure+
    (canonical-type type)))

(defun subtypep (t1 t2 &optional env)
  (when (eq t1 t2)
    (return-from subtypep (values t t)))
  (let* ((*highest-type-tag* *highest-type-tag*)
	 (*save-types-database* t)
	 (*member-types* *member-types*)
	 (*elementary-types* *elementary-types*)
	 (tag1 (safe-canonical-type t1))
	 (tag2 (safe-canonical-type t2)))
    (cond ((and (numberp tag1) (numberp tag2))
	   (values (zerop (logandc2 (safe-canonical-type t1)
				    (safe-canonical-type t2)))
		   t))
	  #+nil
	  ((null tag1)
	   (error "Unknown type specifier ~S." t1))
	  #+nil
	  ((null tag2)
	   (error "Unknown type specifier ~S." t2))
	  (t
	   (values nil nil)))))

;;----------------------------------------------------------------------
;; BOOTSTRAP
;;
#+ecl-min
(progn
  (defun canonicalize (type)
    (let ((*highest-type-tag* *highest-type-tag*)
	  (*save-types-database* t)
	  (*member-types* *member-types*)
	  (*elementary-types* *elementary-types*))
      (let ((tag (canonical-type type))
	    (out))
	(setq tag (canonical-type type))
	(print-types-database *elementary-types*)
	(print-types-database *member-types*)
	(dolist (i *member-types*)
	(unless (zerop (logand (cdr i) tag))
	  (push (car i) out)))
	(when out
	  (setq out `((MEMBER ,@out))))
	(dolist (i *elementary-types*)
	  (unless (zerop (logand (cdr i) tag))
	    (print (list tag (cdr i) (logand tag (cdr i))))
	    (push (car i) out)))
	(values tag `(OR ,@out)))))

  (defun print-types-database (types)
    (format t "~%-------------------------")
    (dolist (i types)
      (format t "~%~20A~%~79,' B" (car i) (cdr i))))

  (defun extend-type-tag (tag minimal-supertype-tag)
    (dolist (type *elementary-types*)
      (let ((other-tag (cdr type)))
	(when (zerop (logandc2 minimal-supertype-tag other-tag))
	  (setf (cdr type) (logior tag other-tag))))))

  (dolist (i '((SYMBOL)
	       (KEYWORD NIL SYMBOL)
	       (PACKAGE)
	       (FUNCTION)
	       (COMPILED-FUNCTION NIL FUNCTION)
	       (DISPATCH-FUNCTION NIL FUNCTION)

	       (INTEGER (INTEGER * *))
	       (SHORT-FLOAT (SHORT-FLOAT * *))
	       (LONG-FLOAT (LONG-FLOAT * *))
	       (RATIO (RATIO * *))

	       (RATIONAL (OR INTEGER RATIO))
	       (FLOAT (OR SHORT-FLOAT LONG-FLOAT))
	       (REAL (OR INTEGER SHORT-FLOAT LONG-FLOAT RATIO))
	       ((COMPLEX SHORT-FLOAT))
	       ((COMPLEX LONG-FLOAT))
	       ((COMPLEX INTEGER))
	       ((COMPLEX RATIO))
	       ((COMPLEX RATIONAL) (OR (COMPLEX INTEGER) (COMPLEX RATIO)))
	       ((COMPLEX FLOAT) (OR (COMPLEX SHORT-FLOAT) (COMPLEX LONG-FLOAT)))
	       ((COMPLEX REAL) (OR (COMPLEX RATIONAL) (COMPLEX FLOAT)))
	       (COMPLEX (COMPLEX REAL))

	       (NUMBER (OR REAL COMPLEX))

	       (FIXNUM (INTEGER #.MOST-NEGATIVE-FIXNUM #.MOST-POSITIVE-FIXNUM))
	       (BIGNUM (AND INTEGER (NOT FIXNUM)))
	       (BIT (INTEGER 0 1))
	       (BYTE8 (INTEGER 0 255))
	       (INTEGER8 (INTEGER -128 127))

	       (CHARACTER)
	       (BASE-CHAR CHARACTER)
	       (STANDARD-CHAR NIL BASE-CHAR)

	       (CONS)
	       (NULL (MEMBER NIL))
	       (LIST (OR CONS NULL))

	       ((ARRAY BYTE8 *))
	       ((ARRAY INTEGER8 *))
	       ((ARRAY FIXNUM *))
	       ((ARRAY CHARACTER *))
	       ((ARRAY SHORT-FLOAT *))
	       ((ARRAY LONG-FLOAT *))
	       ((ARRAY T *))
	       (ARRAY (ARRAY * *))
;; 	       ((SIMPLE-ARRAY BYTE8 *) NIL (ARRAY BYTE8 *))
;; 	       ((SIMPLE-ARRAY INTEGER8 *) NIL (ARRAY INTEGER8 *))
;; 	       ((SIMPLE-ARRAY FIXNUM *) NIL (ARRAY FIXNUM *))
;; 	       ((SIMPLE-ARRAY CHARACTER *) NIL (ARRAY CHARACTER *))
;; 	       ((SIMPLE-ARRAY SHORT-FLOAT *) NIL (ARRAY SHORT-FLOAT *))
;; 	       ((SIMPLE-ARRAY LONG-FLOAT *) NIL (ARRAY LONG-FLOAT *))
;; 	       ((SIMPLE-ARRAY T *) NIL (ARRAY T *))
 	       (SIMPLE-ARRAY (SIMPLE-ARRAY * *))
	       (SIMPLE-VECTOR (SIMPLE-ARRAY T (*)))
	       (SIMPLE-BIT-VECTOR (SIMPLE-ARRAY BIT (*)))
	       (VECTOR (ARRAY * (*)))
	       ((VECTOR BIT) (ARRAY BIT (*)))
	       ((VECTOR BASE-CHAR) (ARRAY BASE-CHAR (*)))
	       ((VECTOR BYTE8) (ARRAY BYTE8 (*)))
	       ((VECTOR INTEGER8) (ARRAY INTEGER8 (*)))
	       ((VECTOR FIXNUM) (ARRAY FIXNUM (*)))
	       ((VECTOR SHORT-FLOAT) (ARRAY SHORT-FLOAT (*)))
	       ((VECTOR LONG-FLOAT) (ARRAY LONG-FLOAT (*)))
	       ((VECTOR T) (ARRAY T (*)))
	       (STRING (ARRAY CHARACTER (*)))
	       (SIMPLE-STRING (SIMPLE-ARRAY CHARACTER (*)))
	       (BIT-VECTOR (VECTOR BIT))

	       (SEQUENCE (OR LIST VECTOR))

	       (HASH-TABLE)
	       (PATHNAME)
	       (LOGICAL-PATHNAME NIL PATHNAME)

	       (BROADCAST-STREAM)
	       (CONCATENATED-STREAM)
	       (ECHO-STREAM)
	       (FILE-STREAM)
	       (STRING-STREAM)
	       (SYNONYM-STREAM)
 	       (TWO-WAY-STREAM)
	       (STREAM (OR BROADCAST-STREAM CONCATENATED-STREAM ECHO-STREAM
			   FILE-STREAM STRING-STREAM SYNONYM-STREAM TWO-WAY-STREAM))

	       (READTABLE)
	       ))
    (let* ((type (first i))
	   (alias (second i))
	   (strict-supertype (or (third i) 'T))
	   (tag))
      (if alias
	  (setq tag (canonical-type alias))
	  (let* ((strict-supertype-tag (canonical-type strict-supertype)))
	    (setq tag (new-type-tag))
	    (unless (eq strict-supertype 't)
	      (extend-type-tag tag strict-supertype-tag))))
      (push (let ((*print-base* 2)) (print (cons type tag))) *elementary-types*)
      ))
  #+nil
  (let ((tag (new-type-tag)))
    (extend-type-tag tag (canonical-type 'symbol))
    (setq *member-types* (acons 'NIL tag *member-types*))
    (push (cons 'NULL tag) *elementary-types*))
  (print-types-database *elementary-types*)
  (format t "~%~70B" *highest-type-tag*)
); ngorp
