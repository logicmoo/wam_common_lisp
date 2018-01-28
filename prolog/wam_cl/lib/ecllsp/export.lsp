;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;                    Exporting external symbols of LISP package

(si::select-package "CL")

(export '(
	  COMMON
	  KYOTO
	  KCL
	  ECL
	  common-lisp
	  common-lisp-user
	  cl
	  cl-user
	  ))

(si::select-package "SI")

;;; ----------------------------------------------------------------------
;;;
(*make-special '*dump-defun-definitions*)
(setq *dump-defun-definitions* nil)
(*make-special '*dump-defmacro-definitions*)
(setq *dump-defmacro-definitions* *dump-defun-definitions*)

(si::fset 'defun
	  #'(lambda-block defun (def env)
	      (let* ((name (second def))
		     (function `#'(lambda-block ,@(cdr def))))
		(when *dump-defun-definitions*
		  (print function)
		  (setq function `(si::bc-disassemble ,function)))
		`(si::fset ',name ,function)))
	  t)

(si::fset 'in-package
	  #'(lambda-block in-package (def env)
	      `(si::select-package ,(string (second def))))
	  t)

(defun eval-feature (x)
  (declare (si::c-local))
  (cond ((symbolp x)
         (member x *features*
                 :test #'(lambda (a b)
                           (or (eql a b)
			       (and (symbolp a) (symbolp b)
				    (string-equal (symbol-name a)
						  (symbol-name b)))))))
	((atom x) (error "~ is not allowed as a feature" x))
        ((eq (car x) 'AND)
         (dolist (x (cdr x) t) (when (not (eval-feature x)) (return nil))))
        ((eq (car x) 'OR)
         (dolist (x (cdr x) nil) (when (eval-feature x) (return t))))
        ((eq (car x) 'NOT)
	 (not (eval-feature (second x))))
	(t (error "~S is not a feature expression." x))))

;;; Revised by G. Attardi
(defun check-no-infix (stream subchar arg)
  (declare (si::c-local))
  (when arg
    (error "Reading from ~S: no number should appear between # and ~A"
	   stream subchar)))

(defun sharp-+-reader (stream subchar arg)
  (check-no-infix stream subchar arg)
  (let ((feature (read stream t nil t)))
    (if (and (not *read-suppress*) (eval-feature feature))
	(read stream t nil t)
	(let ((*read-suppress* t)) (read stream t nil t) (values)))))

(set-dispatch-macro-character #\# #\+ 'sharp-+-reader)
(set-dispatch-macro-character #\# #\+ 'sharp-+-reader
                              (sys::standard-readtable))

(defun sharp---reader (stream subchar arg)
  (check-no-infix stream subchar arg)
  (let ((feature (read stream t nil t)))
    (if (or *read-suppress* (eval-feature feature))
	(let ((*read-suppress* t)) (read stream t nil t) (values))
	(read stream t nil t))))

(set-dispatch-macro-character #\# #\- 'sharp---reader)
(set-dispatch-macro-character #\# #\- 'sharp---reader
                              (sys::standard-readtable))
