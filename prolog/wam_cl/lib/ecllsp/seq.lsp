;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                           sequence routines

(in-package "SYSTEM")

(defun error-sequence-type (type)
  (declare (si::c-local))
  (error 'simple-type-error
	 :datum type
	 :expected-type 'sequence
	 :format-control "~S does not specify a sequence type"
	 :format-arguments (list type)))

(defun error-sequence-length (type size)
  (declare (si::c-local))
  (error 'simple-type-error
	 :format-control
	 "Cannot create a sequnce of size ~S which matches type ~S."
	 :format-arguments (list size type)
	 :expected-type type
	 :datum NIL))

(defun closest-vector-type (type)
  (let (elt-type length name args)
    (if (atom type)
	(setq name type args nil)
	(setq name (first type) args (cdr type)))
    (case name
      ((VECTOR SIMPLE-VECTOR)
       (setq elt-type (if (endp args) '* (first args))
	     length (if (endp (rest args)) '* (second args))))
      ((STRING SIMPLE-STRING)
       (setq elt-type 'BASE-CHAR
	     length (if (endp args) '* (first args))))
      ((BIT-VECTOR SIMPLE-BIT-VECTOR)
       (setq elt-type 'BIT
	     length (if (endp args) '* (first args))))
      ((ARRAY SIMPLE-ARRAY)
       (when (or (endp (rest args))
		 (atom (setq length (second args)))
		 (endp length)
		 (not (endp (rest length))))
	 (error-sequence-type type))
       (setq elt-type (upgraded-array-element-type (first args))
	     length (first (second args))))
      (t
       (dolist (i '((SIMPLE-STRING . BASE-CHAR)
		    (STRING . BASE-CHAR)
		    (BIT-VECTOR . BIT)
		    ((VECTOR BYTE8) . BYTE8)
		    ((VECTOR INTEGER8) . INTEGER8)
		    ((VECTOR FIXNUM) . FIXNUM)
		    ((VECTOR SHORT-FLOAT) . SHORT-FLOAT)
		    ((VECTOR LONG-FLOAT) . LONG-FLOAT)
		    (VECTOR . T))
		(error-sequence-type type))
	  (when (subtypep type (car i))
	    (setq elt-type (cdr i) length '*)
	    (return)))))
    (values elt-type length)))

(defun make-sequence (type size	&key (initial-element nil iesp) &aux sequence)
  "Args: (type length &key initial-element)
Creates and returns a sequence of the given TYPE and LENGTH.  If INITIAL-
ELEMENT is given, then it becomes the elements of the created sequence.  The
default value of INITIAL-ELEMENT depends on TYPE."
  (if (subtypep type 'LIST)
      (progn
	(cond ((subtypep 'LIST type)
	       (make-list size :initial-element initial-element))
	      ((subtypep type 'NIL)
	       (error-sequence-type type))
	      ((subtypep type 'NULL)
	       (unless (zerop size)
		 (error-sequence-length type size)))
	      ((subtypep type 'CONS)
	       (when (zerop size)
		 (error-sequence-length type size))))
	(make-list size :initial-element initial-element))
      (multiple-value-bind (element-type length)
	  (closest-vector-type type)
	(setq sequence (sys:make-vector (if (eq element-type '*) T element-type)
					size nil nil nil nil))
	(unless (or (eql length '*) (eql length size))
	  (error-sequence-length type size))
	(when iesp
	  (do ((i 0 (1+ i))
	       (size size))
	      ((>= i size))
	    (declare (fixnum i size))
	    (setf (elt sequence i) initial-element)))
	sequence)))

(defun concatenate (result-type &rest sequences)
  "Args: (type &rest sequences)
Returns a new sequence of the specified type, consisting of all elements of
SEQUENCEs."
  (do ((x (make-sequence result-type
			 (apply #'+ (mapcar #'length sequences))))
       (s sequences (cdr s))
       (i 0))
      ((null s) x)
    (declare (fixnum i))
    (do ((j 0 (1+ j))
         (n (length (car s))))
        ((>= j n))
      (declare (fixnum j n))
      (setf (elt x i) (elt (car s) j))
      (incf i))))


(defun map (result-type function sequence &rest more-sequences)
  "Args: (type function sequence &rest more-sequences)
Creates and returns a sequence of TYPE with K elements, with the N-th element
being the value of applying FUNCTION to the N-th elements of the given
SEQUENCEs, where K is the minimum length of the given SEQUENCEs."
  (setq more-sequences (cons sequence more-sequences))
  (let ((l (apply #'min (mapcar #'length more-sequences))))
    (if (null result-type)
        (do ((i 0 (1+ i))
             (l l))
            ((>= i l) nil)
          (declare (fixnum i l))
          (apply function (mapcar #'(lambda (z) (elt z i))
                                  more-sequences)))
        (let ((x (make-sequence result-type l)))
          (do ((i 0 (1+ i))
               (l l))
              ((>= i l) x)
            (declare (fixnum i l))
            (setf (elt x i)
                  (apply function (mapcar #'(lambda (z) (elt z i))
                                          more-sequences))))))))


(defun some (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs satisfies PREDICATE;
NIL otherwise."
  (setq more-sequences (cons sequence more-sequences))
  (do ((i 0 (1+ i))
       (l (apply #'min (mapcar #'length more-sequences))))
      ((>= i l) nil)
    (declare (fixnum i l))
    (let ((that-value
           (apply predicate
                  (mapcar #'(lambda (z) (elt z i)) more-sequences))))
      (when that-value (return that-value)))))


(defun every (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if every elements of SEQUENCEs satisfy PREDICATE; NIL otherwise."
  (setq more-sequences (cons sequence more-sequences))
  (do ((i 0 (1+ i))
       (l (apply #'min (mapcar #'length more-sequences))))
      ((>= i l) t)
    (declare (fixnum i l))
    (unless (apply predicate (mapcar #'(lambda (z) (elt z i)) more-sequences))
            (return nil))))


(defun notany (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if none of the elements in SEQUENCEs satisfies PREDICATE; NIL
otherwise."
  (not (apply #'some predicate sequence more-sequences)))


(defun notevery (predicate sequence &rest more-sequences)
  "Args: (predicate sequence &rest more-sequences)
Returns T if at least one of the elements in SEQUENCEs does not satisfy
PREDICATE; NIL otherwise."
  (not (apply #'every predicate sequence more-sequences)))

(defun map-into (result-sequence function &rest sequences)
  (let ((nel (apply #'min (if (vectorp result-sequence)
			      (array-dimension result-sequence 0)
			      (length result-sequence))
		    (mapcar #'length sequences))))
    (declare (fixnum nel))
    ;; Set the fill pointer to the number of iterations
    (when (and (vectorp result-sequence)
	       (array-has-fill-pointer-p result-sequence))
      (setf (fill-pointer result-sequence) nel))
    ;; Perform mapping
    (dotimes (k nel result-sequence)
      (declare (fixnum nel))
      (setf (elt result-sequence k)
	    (apply function (mapcar #'(lambda (v) (elt v k)) sequences))))))
