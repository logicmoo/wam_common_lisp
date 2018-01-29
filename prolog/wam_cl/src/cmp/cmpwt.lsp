;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPWT  Output routines.

(in-package 'compiler)

(defun wt-comment (message &optional symbol)
  (if symbol
    (progn
      (terpri *compiler-output1*)
      (princ "/*	" *compiler-output1*)
      (princ message *compiler-output1*))
    (if (symbol-package message)
	(progn
	  (format *compiler-output1* "~50T/*  ")
	  (setq symbol message))
	;; useless to show gensym's
	(return-from wt-comment)))
  (let* ((s (symbol-name symbol))
         (l (1- (length s)))
         c)
    (declare (string s) (fixnum l) (character c))
    (dotimes (n l)
      (setq c (schar s n))
      (princ c *compiler-output1*)
      (when (and (char= c #\*) (char= (schar s (1+ n)) #\/))
        (princ #\\ *compiler-output1*)))
    (princ (schar s l) *compiler-output1*))
  (format *compiler-output1* "~70T*/")
  nil
  )

(defun wt1 (form)
  (typecase form
    ((or STRING INTEGER CHARACTER)
     (princ form *compiler-output1*))
    ((or LONG-FLOAT SHORT-FLOAT)
     (format *compiler-output1* "~10,,,,,,'eG" form))
    (VAR (wt-var form))
    (t (wt-loc form)))
  nil)

(defun wt-h1 (form)
  (if (consp form)
    (let ((fun (get (car form) 'wt)))
      (if fun
        (apply fun (cdr form))
        (cmperr "The location ~s is undefined." form)))
    (princ form *compiler-output2*))
  nil)

(defun wt-data (expr)
  (let ((*print-radix* nil)
        (*print-base* 10)
        (*print-circle* t)
        (*print-pretty* nil)
        (*print-level* nil)
        (*print-length* nil)
        (*print-case* :downcase)
        (*print-gensym* t)
        (*print-array* t)
	(*read-default-float-format* 'single-float)
        (sys::*print-package* t)
        (sys::*print-structure* t))
    (terpri *compiler-output-data*)
    ;; fix to ANACAD bug
    (if (compiled-function-p expr)
	(prin1 (sys:compiled-function-name expr) *compiler-output-data*)
      (prin1 expr *compiler-output-data*))
    nil))

(defun wt-data-begin ()
  (princ "          " *compiler-output-data*)
  (terpri *compiler-output-data*)
  (princ "#(" *compiler-output-data*)
  nil)

(defun wt-data-end ()
  (terpri *compiler-output-data*)
  (princ ")" *compiler-output-data*)
  ;; write the length of the data
  (format *compiler-output-data* "~%~8D~%"
	  (+ 11 (file-length *compiler-output-data*))))

(defun wt-data-package-operation (form)
  (terpri *compiler-output-data*)
  (princ "#!" *compiler-output-data*)
  (wt-data form))
