;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;                        list manipulating routines


(in-package 'lisp)

(export '(union nunion intersection nintersection
          set-difference nset-difference set-exclusive-or nset-exclusive-or
          subsetp))

(in-package 'system)

(proclaim '(optimize (safety 2) (space 3)))

(defun union (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (cond ((null list1) list2)
	((apply #'member1 (car list1) list2 rest)
	 (apply #'union (cdr list1) list2 rest))
	(t
	 (cons (car list1)
	       (apply #'union (cdr list1) list2 rest)))))

(defun nunion (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (cond ((null list1) list2)
	((apply #'member1 (car list1) list2 rest)
	 (apply #'nunion (cdr list1) list2 rest))
	(t
	 (rplacd list1
		 (apply #'nunion (cdr list1) list2 rest)))))

(defun intersection (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (cond ((null list1) nil)
	((apply #'member1 (car list1) list2 rest)
	 (cons (car list1)
	       (apply #'intersection (cdr list1) list2 rest)))
	(t (apply #'intersection (cdr list1) list2 rest))))

(defun nintersection (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (cond ((null list1) nil)
	((apply #'member1 (car list1) list2 rest)
	 (rplacd list1
		 (apply #'nintersection (cdr list1) list2 rest)))
	(t (apply #'nintersection (cdr list1) list2 rest))))

(defun set-difference (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (cond ((null list1) nil)
	((not (apply #'member1 (car list1) list2 rest))
	 (cons (car list1)
	       (apply #'set-difference (cdr list1) list2 rest)))
	(t (apply #'set-difference (cdr list1) list2 rest))))

(defun nset-difference (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (cond ((null list1) nil)
	((not (apply #'member1 (car list1) list2 rest))
	 (rplacd list1
		 (apply #'nset-difference (cdr list1) list2 rest)))
	(t (apply #'nset-difference (cdr list1) list2 rest))))

(defun set-exclusive-or (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (append (apply #'set-difference list1 list2 rest)
	  (apply #'set-difference list2 list1 rest)))

(defun nset-exclusive-or (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (nconc (apply #'set-difference list1 list2 rest)
	 (apply #'nset-difference list2 list1 rest)))

(defun subsetp (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (do ((l list1 (cdr l)))
      ((null l) t)
    (if (not (apply #'member1 (car l) list2 rest)) (return nil))))

(defun rassoc-if (pred arg &key key)
  (rassoc pred arg :test #'funcall :key key))
(defun rassoc-if-not (pred arg &key key)
  (rassoc pred arg :test-not #'funcall :key key))

(defun assoc-if (pred arg &key key)
  (assoc pred arg :test #'funcall :key key))
(defun assoc-if-not (pred arg &key key)
  (assoc pred arg :test-not #'funcall :key key))

(defun member-if (pred arg &key key)
  (member pred arg :test #'funcall :key key))
(defun member-if-not (pred arg &key key)
  (member pred arg :test-not #'funcall :key key))

(defun subst-if (new old where &key key)
  (subst new old where :test #'funcall :key key))
(defun subst-if-not (new old where &key key)
  (subst new old where :test-not #'funcall :key key))

(defun nsubst-if (new old where &key key)
  (nsubst new old where :test #'funcall :key key))
(defun nsubst-if-not (new old where &key key)
  (nsubst new old where :test-not #'funcall :key key))
