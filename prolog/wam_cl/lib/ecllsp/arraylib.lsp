;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;    arraylib.lsp
;;;;
;;;;                            array routines

(in-package "SYSTEM")

(defun make-array (dimensions
		   &key (element-type t)
			(initial-element nil initial-element-supplied-p)
			(initial-contents nil initial-contents-supplied-p)
			adjustable fill-pointer
			displaced-to (displaced-index-offset 0))
  "Args: (dimensions &key (element-type t) initial-element (initial-contents nil)
		    (adjustable nil) (fill-pointer nil) (displaced-to nil)
		    (displaced-index-offset 0) (static nil))
Creates an array of the specified DIMENSIONS.  DIMENSIONS is a list of
non-negative integers each representing the length of the corresponding
dimension.  It may be an integer for vectors, i.e., one-dimensional arrays.
ELEMENT-TYPE specifies the type of array elements.  INITIAL-ELEMENT specifies
the initial value for all elements.  Its default value depends on ELEMENT-
TYPE.  INITIAL-CONTENTS specifies each element in terms of sequences.
ADJUSTABLE specifies whether or not the array is adjustable (see ADJUST-
ARRAY).  FILL-POINTER is meaningful only for vectors.  It specifies whether
the vector has fill-pointer or not, and if it has, the initial value of the
fill-pointer.  Possible values are NIL (no fill-pointer), T (the length of the
vector), or an integer.  See VECTOR-PUSH and VECTOR-POP.  DISPLACED-TO, if
non-NIL, must be an array and specifies that the new array is displaced to the
given array.  DISPLACED-INDEX-OFFSET is meaningful only when DISPLACED-TO is
non-NIL and specifies that the reference to the I-th element of the new array
in raw-major indexing is actually the reference to the (I + DISPLACED-INDEX-
OFFSET)th element of the given array.If the STATIC argument is supplied
with a non-nil value, then the body of the array is allocated as a
contiguous block."
  (setq element-type (upgraded-array-element-type element-type))

  (if (or (integerp dimensions)
	  (when (= (length dimensions) 1)
	    (setq dimensions (first dimensions))))
      (let ((x (sys:make-vector element-type dimensions
			       adjustable fill-pointer
			       displaced-to displaced-index-offset)))
	(declare (vector x))
	(when initial-element-supplied-p
	  (do ((n dimensions)
	       (i 0 (1+ i)))
	      ((>= i n))
	    (declare (fixnum n i))
	    (sys:aset initial-element x i)))
	(when initial-contents-supplied-p
	  (do ((n dimensions)
	       (i 0 (1+ i)))
	      ((>= i n))
	    (declare (fixnum n i))
	    (sys:aset (elt initial-contents i) x i)))
	x)
      (if fill-pointer
	  (error ":FILL-POINTER may not be specified for an array of rank ~D"
		 (length dimensions))
	  (let ((x (apply #'sys:make-pure-array
			  element-type adjustable
			  displaced-to displaced-index-offset
			  dimensions)))
	    (declare (array x))
	    (unless (member 0 dimensions)
	      (when initial-element-supplied-p
		(do ((cursor
		      (make-list (length dimensions)
				 :initial-element 0)))
		    (nil)
		  (apply #'aset initial-element x cursor)
		  (when (increment-cursor cursor dimensions)
		    (return nil))))
	      (when initial-contents-supplied-p
		(do ((cursor
		      (make-list (length dimensions)
				 :initial-element 0)))
		    (nil)
		  (apply #'aset (sequence-cursor initial-contents
						 cursor)
			 x cursor)
		  (when (increment-cursor cursor dimensions)
		    (return nil)))))
	    x))))

(defun increment-cursor (cursor dimensions)
  (declare (si::c-local))
  (if (null cursor)
      t
      (let ((carry (increment-cursor (cdr cursor) (cdr dimensions))))
	(if carry
	    (cond ((>= (the fixnum (1+ (the fixnum (car cursor))))
	               (the fixnum (car dimensions)))
		   (rplaca cursor 0)
		   t)
		  (t
		   (rplaca cursor
		           (the fixnum (1+ (the fixnum (car cursor)))))
		   nil))
	    nil))))


(defun sequence-cursor (sequence cursor)
  (declare (si::c-local))
  (if (null cursor)
      sequence
      (sequence-cursor (elt sequence (the fixnum (car cursor)))
                       (cdr cursor))))


(defun vector (&rest objects)
  "Args: (&rest objects)
Creates and returns a simple-vector, with the N-th OBJECT being the N-th
element."
  (make-array (list (length objects))
	      :element-type t
	      :initial-contents objects))


(defun array-dimensions (array)
  "Args: (array)
Returns a list whose N-th element is the length of the N-th dimension of ARRAY."
  (do ((i (array-rank array))
       (d nil))
      ((= i 0) d)
    (setq i (1- i))
    (setq d (cons (array-dimension array i) d))))


(defun array-in-bounds-p (array &rest indices &aux (r (array-rank array)))
  "Args: (array &rest indexes)
Returns T if INDEXes are valid indexes of ARRAY; NIL otherwise.  The number of
INDEXes must be equal to the rank of ARRAY."
  (when (/= r (length indices))
        (error "The rank of the array is ~R,~%~
               ~7@Tbut ~R ~:*~[indices are~;index is~:;indices are~] ~
               supplied."
               r (length indices)))
  (do ((i 0 (1+ i))
       (s indices (cdr s)))
      ((>= i r) t)
    (when (or (< (car s) 0)
              (>= (car s) (array-dimension array i)))
          (return nil))))


(defun array-row-major-index (array &rest indices)
  "Args: (array &rest indexes)
Returns the non-negative integer that represents the location of the element
of ARRAY specified by INDEXes, assuming all elements of ARRAY are aligned in
row-major order."
  (do ((i 0 (1+ i))
       (j 0 (+ (* j (array-dimension array i)) (car s)))
       (s indices (cdr s)))
      ((null s) j)))


(defun bit (bit-array &rest indices)
  "Args: (bit-array &rest indexes)
Returns the bit of BIT-ARRAY specified by INDEXes."
  (apply #'aref bit-array indices))


(defun sbit (bit-array &rest indices)
  "Args: (simple-bit-array &rest subscripts)
Returns the specified bit in SIMPLE-BIT-ARRAY."
  (apply #'aref bit-array indices))


(defun bit-and (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise AND of BIT-ARRAY1 and BIT-ARRAY2.  Puts the results
into a new bit-array if RESULT is NIL, into BIT-ARRAY1 if RESULT is T, or into
RESULT if RESULT is a bit-array."
  (bit-array-op boole-and bit-array1 bit-array2 result-bit-array))


(defun bit-ior (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise INCLUSIVE OR of BIT-ARRAY1 and BIT-ARRAY2.  Puts the
results into a new bit-array if RESULT is NIL, into BIT-ARRAY1 if RESULT is T,
or into RESULT if RESULT is a bit-array."
  (bit-array-op boole-ior bit-array1 bit-array2 result-bit-array))


(defun bit-xor (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise EXCLUSIVE OR of BIT-ARRAY1 and BIT-ARRAY2.  Puts the
results into a new bit-array if RESULT is NIL, into BIT-ARRAY1 if RESULT is T,
or into RESULT if RESULT is a bit-array."
  (bit-array-op boole-xor bit-array1 bit-array2 result-bit-array))


(defun bit-eqv (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise EQUIVALENCE of BIT-ARRAY1 and BIT-ARRAY2.  Puts the
results into a new bit-array if RESULT is NIL, into BIT-ARRAY1 if RESULT is T,
or into RESULT if RESULT is a bit-array."
  (bit-array-op boole-eqv bit-array1 bit-array2 result-bit-array))

    
(defun bit-nand (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise NOT of {the element-wise AND of BIT-ARRAY1 and BIT-
ARRAY2}.  Puts the results into a new bit-array if RESULT is NIL, into BIT-
ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array."
  (bit-array-op boole-nand bit-array1 bit-array2 result-bit-array))

    
(defun bit-nor (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise NOT of {the element-wise INCLUSIVE OR of BIT-ARRAY1
and BIT-ARRAY2}.  Puts the results into a new bit-array if RESULT is NIL, into
BIT-ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array."
  (bit-array-op boole-nor bit-array1 bit-array2 result-bit-array))

    
(defun bit-andc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise AND of {the element-wise NOT of BIT-ARRAY1} and BIT-
ARRAY2.  Puts the results into a new bit-array if RESULT is NIL, into BIT-
ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array."
  (bit-array-op boole-andc1 bit-array1 bit-array2 result-bit-array))

    
(defun bit-andc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise AND of BIT-ARRAY1 and {the element-wise NOT of BIT-
ARRAY2}.  Puts the results into a new bit-array if RESULT is NIL, into BIT-
ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array."
  (bit-array-op boole-andc2 bit-array1 bit-array2 result-bit-array))

    
(defun bit-orc1 (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise INCLUSIVE OR of {the element-wise NOT of BIT-ARRAY1}
and BIT-ARRAY2.  Puts the results into a new bit-array if RESULT is NIL, into
BIT-ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array."
  (bit-array-op boole-orc1 bit-array1 bit-array2 result-bit-array))

    
(defun bit-orc2 (bit-array1 bit-array2 &optional result-bit-array)
  "Args: (bit-array1 bit-array2 &optional (result nil))
Returns the element-wise INCLUSIVE OR of BIT-ARRAY1 and {the element-wise NOT
of BIT-ARRAY2}.  Puts the results into a new bit-array if RESULT is NIL, into
BIT-ARRAY1 if RESULT is T, or into RESULT if RESULT is a bit-array."
  (bit-array-op boole-orc2 bit-array1 bit-array2 result-bit-array))

    
(defun bit-not (bit-array &optional result-bit-array)
  "Args: (bit-array &optional (result nil))
Returns the element-wise NOT of BIT-ARRAY.  Puts the results into a new bit-
array if RESULT is NIL, into BIT-ARRAY if RESULT is T, or into RESULT if
RESULT is a bit-array."
  (bit-array-op boole-c1 bit-array bit-array result-bit-array))


(defun vector-push (new-element vector)
  "Args: (item vector)
Replaces ITEM for the element of VECTOR that is pointed to by the fill-pointer
of VECTOR and then increments the fill-pointer by one.  Returns NIL if the new
value of the fill-pointer becomes too large.  Otherwise, returns the new fill-
pointer as the value."
  (let ((fp (fill-pointer vector)))
    (declare (fixnum fp))
    (cond ((< fp (the fixnum (array-dimension vector 0)))
           (sys:aset new-element vector fp)
           (sys:fill-pointer-set vector (the fixnum (1+ fp)))
	   fp)
	  (t nil))))


(defun vector-push-extend (new-element vector &optional extension)
  "Args: (item vector &optional (n (length vector)))
Replaces ITEM for the element of VECTOR that is pointed to by the fill-pointer
of VECTOR and then increments the fill-pointer by one.  If the new value of
the fill-pointer becomes too large, extends VECTOR for N more elements.
Returns the new value of the fill-pointer."
  (let ((fp (fill-pointer vector)))
    (declare (fixnum fp))
    (cond ((< fp (the fixnum (array-dimension vector 0)))
	   (sys:aset new-element vector fp)
	   (sys:fill-pointer-set vector (the fixnum (1+ fp)))
	   fp)
	  (t
	   (adjust-array vector
	                 (list (+ (array-dimension vector 0)
				  (or extension
				      (if (> (array-dimension vector 0)  0)
					  (array-dimension vector 0)
					5))))
	                 :element-type (array-element-type vector)
			 :fill-pointer fp)
	   (sys:aset new-element vector fp)
	   (sys:fill-pointer-set vector (the fixnum (1+ fp)))
	   fp))))


(defun vector-pop (vector)
  "Args: (vector)
Decrements the fill-pointer of VECTOR by one and returns the element pointed
to by the new fill-pointer.  Signals an error if the old value of the fill-
pointer is 0 already."
  (let ((fp (fill-pointer vector)))
    (declare (fixnum fp))
    (when (= fp 0)
          (error "The fill pointer of the vector ~S zero." vector))
    (sys:fill-pointer-set vector (the fixnum (1- fp)))
    (aref vector (the fixnum (1- fp)))))


(defun adjust-array (array new-dimensions
                     &rest r
		     &key element-type
			  initial-element
			  initial-contents
			  fill-pointer
			  displaced-to
			  displaced-index-offset)
  "Args: (array dimensions
       &key (element-type (array-element-type array))
            initial-element (initial-contents nil) (fill-pointer nil)
            (displaced-to nil) (displaced-index-offset 0))
Adjusts the dimensions of ARRAY to the given DIMENSIONS.  ARRAY must be an
adjustable array."
  (declare (ignore element-type
                   initial-element
                   initial-contents
                   fill-pointer
                   displaced-index-offset))
  (when (integerp new-dimensions)
        (setq new-dimensions (list new-dimensions)))
  (let ((element-type (array-element-type array)))
    (unless (eq element-type t) (push element-type r)
	    (push :element-type r)))
  (let ((x (apply #'make-array new-dimensions :adjustable t r)))
    (declare (array x))
    (unless displaced-to
      (do ((cursor (make-list (length new-dimensions) :initial-element 0)))
	  (nil)
	(when (apply #'array-in-bounds-p array cursor)
	  (apply #'aset (apply #'aref array cursor) x cursor))
	(when (increment-cursor cursor new-dimensions)
	  (return nil))))
    (sys:replace-array array x)
    ))
