;;; A portable implementation of locatives (or first-class places).

;;; From the Lisp Machine Manual:
;;;
;;;	A locative is a type of Lisp object used as a pointer to a cell.
;;;	[...]  A cell is a machine word that can hold a (pointer to a) Lisp
;;;	object.  For example, a symbol has five cells: the print name cell,
;;;	the value cell, the function cell, the property list cell, and the
;;;	package cell.  The value cell holds (a pointer to) the binding of
;;;	the symbol, and so on.  [...]  A locative is an object that points
;;;	to a cell: it lets you refer to a cell so that you can examine or
;;;	alter its contents.
;;;
;;; Since standard Common Lisp doesn't provide any way to create a pointer
;;; to a cell, this implementation instead uses the setf place machinery.
;;; This makes locatives more versatile, as they can refer to not just a
;;; cell, but any place, e.g. a single bit or multiple values.  Because of
;;; this, a locative can't be an immediate value like a machine address, so
;;; it also makes locatives more heavy-weight.  In this implementation,
;;; creating a locative involves consing two closures, plus storage to hold
;;; them.

;;; Two additional Lisp machine locative operators, location-boundp and
;;; location-makunbound, can at best only be approximated, so are better
;;; left out completely.

;;; Usage example:
;;;
;;;	(defun foo (list)
;;;	  ;; Return a locative pointing into a list.
;;;	  (locf (nth 2 list)))
;;;
;;;	(defun bar (array)
;;;	  ;; Return a locative pointing into an array.
;;;	  (locf (aref array 3)))
;;;
;;;	(defun frob (loc)
;;;	  ;; Modify the contents of the place.
;;;	  (setf (contents loc) 42))
;;;
;;;	(let ((list (list 1 2 3 4 5))
;;;	      (array (vector 1 2 3 4 5)))
;;;	  (frob (foo list))
;;;	  (frob (bar array))
;;;	  (values list array))



(defpackage #:locatives
  (:use #:common-lisp)
  (:export #:locative #:locativep #:locf #:contents))

(in-package #:locatives)

(eval-when (:compile-toplevel :execute)
  (defconstant +locative-doc+
    "A locative is a type of Lisp object used as a pointer to a place.")
  (defconstant +locativep-doc+
    "Returns true if the object is a locative."))

;;; Three different storage types for locatives are provided:
;;; structure, class, or cons.

#+(and)
(progn
  (defstruct (locative
	       (:predicate locativep)
	       (:constructor make-locative (reader writer))
	       (:copier nil))
    #.+locative-doc+
    (reader nil :type function :read-only t)
    (writer nil :type function :read-only t))
  (setf (documentation 'locativep 'function) #.+locativep-doc+))

#+(or)
(progn
  (defclass locative ()
    ((reader :initarg :reader :type function :reader locative-reader)
     (writer :initarg :writer :type function :reader locative-writer))
    (:documentation #.+locative-doc+))
  (defun locativep (object)
    #.+locativep-doc+
    (typep object 'locative))
  (defun make-locative (reader writer)
    (make-instance 'locative :reader reader :writer writer)))

#+(or)
(progn
  (deftype locative ()
    #.+locative-doc+
    `(cons function function))
  (defun locativep (object)
    #.+locativep-doc+
    (typep object 'locative))
  (defun locative-reader (loc) (car loc))
  (defun locative-writer (loc) (cdr loc))
  (defun make-locative (reader writer) (cons reader writer)))

(when (find-class 'locative nil)
  (defmethod print-object ((object locative) stream)
    (print-unreadable-object (object stream :type t :identity t))
    object))

(defmacro locf (place &environment env)
  "Return a locative for place."
  (multiple-value-bind (temps values variables writer reader)
      (get-setf-expansion place env)
    `(let* ,(mapcar #'list temps values)
       (make-locative (lambda () ,reader) (lambda ,variables ,writer)))))

(defun contents (locative)
  "Returns the contents of the place which the locative points to."
  (funcall (locative-reader locative)))

(define-setf-expander contents (locative &environment env)
  "Modifies the contents of the place which the locative points to."
  (multiple-value-bind (temps values variables writer reader)
      (get-setf-expansion locative env)
    (declare (ignore writer))
    (values temps
	    values
	    variables
	    `(funcall (locative-writer ,reader) ,@variables)
	    `(funcall (locative-reader ,reader)))))
