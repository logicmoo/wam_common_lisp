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


(in-package 'lisp)


(export '(make-array vector
          array-element-type array-rank array-dimension
          array-dimensions
          array-in-bounds-p array-row-major-index
          adjustable-array-p
          bit sbit
          bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor
          bit-andc1 bit-andc2 bit-orc1 bit-orc2 bit-not
          array-has-fill-pointer-p fill-pointer
          vector-push vector-push-extend vector-pop
          adjust-array))


(in-package 'system)


(proclaim '(optimize (safety 2) (space 3)))


(defun make-array (dimensions
		   &key (element-type t)
			(initial-element nil initial-element-supplied-p)
			(initial-contents nil initial-contents-supplied-p)
			adjustable fill-pointer
			displaced-to (displaced-index-offset 0)
			static)
  (setq element-type
	(cond ((or (eql t element-type) (null element-type)) T)
	      ((eql element-type 'CHARACTER) 'STRING-CHAR)
	      (t (dolist (v '(BIT STRING-CHAR FIXNUM SHORT-FLOAT LONG-FLOAT) T)
		   (when (subtypep element-type v)
		     (return v))))))

  (if (or (integerp dimensions)
	  (when (= (length dimensions) 1)
	    (setq dimensions (first dimensions))))
      (let ((x (sys:make-vector element-type dimensions
			       adjustable fill-pointer
			       displaced-to displaced-index-offset
			       static)))
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
			  static
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
  (if (null cursor)
      sequence
      (sequence-cursor (elt sequence (the fixnum (car cursor)))
                       (cdr cursor))))


(defun vector (&rest objects)
  (make-array (list (length objects))
	      :element-type t
	      :initial-contents objects))


(defun array-dimensions (array)
  (do ((i (array-rank array))
       (d nil))
      ((= i 0) d)
    (setq i (1- i))
    (setq d (cons (array-dimension array i) d))))


(defun array-in-bounds-p (array &rest indices &aux (r (array-rank array)))
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
  (do ((i 0 (1+ i))
       (j 0 (+ (* j (array-dimension array i)) (car s)))
       (s indices (cdr s)))
      ((null s) j)))


(defun bit (bit-array &rest indices)
  (apply #'aref bit-array indices))


(defun sbit (bit-array &rest indices)
  (apply #'aref bit-array indices))


(defun bit-and (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-and bit-array1 bit-array2 result-bit-array))


(defun bit-ior (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-ior bit-array1 bit-array2 result-bit-array))


(defun bit-xor (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-xor bit-array1 bit-array2 result-bit-array))


(defun bit-eqv (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-eqv bit-array1 bit-array2 result-bit-array))

    
(defun bit-nand (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-nand bit-array1 bit-array2 result-bit-array))

    
(defun bit-nor (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-nor bit-array1 bit-array2 result-bit-array))

    
(defun bit-andc1 (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-andc1 bit-array1 bit-array2 result-bit-array))

    
(defun bit-andc2 (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-andc2 bit-array1 bit-array2 result-bit-array))

    
(defun bit-orc1 (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-orc1 bit-array1 bit-array2 result-bit-array))

    
(defun bit-orc2 (bit-array1 bit-array2 &optional result-bit-array)
  (bit-array-op boole-orc2 bit-array1 bit-array2 result-bit-array))

    
(defun bit-not (bit-array &optional result-bit-array)
  (bit-array-op boole-c1 bit-array bit-array result-bit-array))


(defun vector-push (new-element vector)
  (let ((fp (fill-pointer vector)))
    (declare (fixnum fp))
    (cond ((< fp (the fixnum (array-dimension vector 0)))
           (sys:aset new-element vector fp)
           (sys:fill-pointer-set vector (the fixnum (1+ fp)))
	   fp)
	  (t nil))))


(defun vector-push-extend (new-element vector &optional extension)
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
			  displaced-index-offset
			  static)
  (declare (ignore element-type
                   initial-element
                   initial-contents
                   fill-pointer
                   displaced-to
                   displaced-index-offset
                   static))
  (when (integerp new-dimensions)
        (setq new-dimensions (list new-dimensions)))
  (let ((element-type (array-element-type array)))
    (unless (eq element-type t) (push element-type r)
	    (push :element-type r)))
  (let ((x (apply #'make-array new-dimensions :adjustable t r)))
    (declare (array x))
    (do ((cursor (make-list (length new-dimensions) :initial-element 0)))
        (nil)
      (when (apply #'array-in-bounds-p array cursor)
            (apply #'aset (apply #'aref array cursor)
		   x
		   cursor))
      (when (increment-cursor cursor new-dimensions)
            (return nil)))
    (sys:replace-array array x)
    ))
