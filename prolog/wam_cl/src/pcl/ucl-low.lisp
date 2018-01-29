;;;;
;;;; File ucl-low.l
;;;;
;;;; **************************************************************************
;;;;
;;;; (C) Copyright 1992 by the University of Utah.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted.  Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;; 
;;;; This software is made available AS IS, and the University of Utah makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.
;;;; 
;;;; Suggestions, comments and requests for improvements are welcome.  Please
;;;; address inquiries about this file or Utah Common Lisp to:
;;;;
;;;;   University of Utah Department of Computer Science
;;;;   3440 Merrill Engineering Building
;;;;   Salt Lake City, Utah  84112
;;;;
;;;;   Email: ucl-bugs@cs.utah.edu (for UCL inquiries)
;;;;
;;;; **************************************************************************
;;;; 
;;;; This file contains low-level functions specific to Utah Common Lisp (UCL).
;;;; This file was written by Eric Eide (eeide@cs.utah.edu).
;;;; 

(in-package "PCL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Redefining things in "low.l".
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Structure functions and macros.
;;;

(pushnew :structure-wrapper *features*)
(pushnew :structure-functions *features*)

(defmacro structurep (x)
  `(ucl::structurep ,x))

(defmacro structure-type (x)
  ;; I could have used ucl::structure-type-of here.
  `(svref (the simple-vector (ucl::structure-secret ,x)) 0))

(defun known-structure-type-p (symbol)
  (not (null (ucl::structure-info symbol))))

(defun structure-type-included-type-name (symbol)
  (let ((info (ucl::structure-info symbol)))
    (if info
	(cadr (svref (the simple-vector info) 1))
	nil)))

(defmacro structure-slots (type)
  `(svref (the simple-vector (ucl::structure-info ,type)) 0))

(defmacro structure-slot-name (slot)      `(svref (the simple-vector ,slot) 0))
(defmacro structure-slot-read-only (slot) `(svref (the simple-vector ,slot) 2))
(defmacro structure-slot-accessor (slot)  `(svref (the simple-vector ,slot) 5))

(defun structure-type-slot-description-list (type)
  (let ((include (structure-type-included-type-name type)))
    (if include
	(nthcdr (length (structure-slots include))
		(structure-slots type))
	(structure-slots type))))

(defun structure-slotd-name (slotd)
  (structure-slot-name slotd))

(defun structure-slotd-accessor-symbol (slotd)
  (structure-slot-accessor slotd))

(defun structure-slotd-reader-function (slotd)
  (symbol-function (structure-slot-accessor slotd)))

(defun structure-slotd-writer-function (slotd)
  (unless (structure-slot-read-only slotd)
    (get (structure-slot-accessor slotd) 'ucl::setf-method-expander)))

;;;
;;; Function functions.
;;;

(defun set-function-name-1 (function new-name uninterned-name)
  ;; Only conscode can associate a name with a function.
  (when (ucl::conscode-p function)
    (ucl::setf-conscode-name function new-name))
  function)

;; No way to write FUNCTION-ARGLIST.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Redefining things in "macros.l".
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun printing-random-thing-internal (thing stream)
  (ucl::prin1-address thing stream))

