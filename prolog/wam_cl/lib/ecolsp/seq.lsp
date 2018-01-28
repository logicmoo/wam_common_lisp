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


(in-package #:lisp)

(export '(make-sequence concatenate map some every notany notevery))

(in-package #:system)


(proclaim '(optimize (safety 2) (space 3)))


(defun make-sequence (type size	&key (initial-element nil iesp)
                                &aux element-type sequence)
  (setq element-type
        (cond ((eq type 'LIST)
               (return-from make-sequence
                (if iesp
                    (make-list size :initial-element initial-element)
                    (make-list size))))
              ((or (eq type 'SIMPLE-STRING) (eq type 'STRING)) 'STRING-CHAR)
              ((or (eq type 'SIMPLE-BIT-VECTOR) (eq type 'BIT-VECTOR)) 'BIT)
              ((or (eq type 'SIMPLE-VECTOR) (eq type 'VECTOR)) t)
              (t
               (setq type (normalize-type type))
               (when (eq (car type) 'LIST)
                     (return-from make-sequence
                      (if iesp
                          (make-list size :initial-element initial-element)
                          (make-list size))))
               (unless (or (eq (car type) 'ARRAY)
                           (eq (car type) 'SIMPLE-ARRAY))
                       (error "~S is not a sequence type." type))
               (or (second type) t))))
  (setq sequence (sys:make-vector element-type size nil nil nil nil nil))
  (when iesp
        (do ((i 0 (1+ i))
             (size size))
            ((>= i size))
          (declare (fixnum i size))
          (setf (elt sequence i) initial-element)))
  sequence)


(defun concatenate (result-type &rest sequences)
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
  (setq more-sequences (cons sequence more-sequences))
  (do ((i 0 (1+ i))
       (l (apply #'min (mapcar #'length more-sequences))))
      ((>= i l) t)
    (declare (fixnum i l))
    (unless (apply predicate (mapcar #'(lambda (z) (elt z i)) more-sequences))
            (return nil))))


(defun notany (predicate sequence &rest more-sequences)
  (not (apply #'some predicate sequence more-sequences)))


(defun notevery (predicate sequence &rest more-sequences)
  (not (apply #'every predicate sequence more-sequences)))
