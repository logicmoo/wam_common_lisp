;; Standalone ANSI bootstrap tests: mutable conses, hash tables, and structures.
;; Run from the repository root:
;;   .\wam_cl.cmd < t\ansi-test-wamcl\bootstrap\03-cons-hash-structures.lisp

(in-package :cl-user)

(defparameter *ab-passed* 0)
(defparameter *ab-failed* 0)
(defparameter *ab-total* 11)
(unless (boundp '*ab-all-passed*) (setq *ab-all-passed* 0))
(unless (boundp '*ab-all-failed*) (setq *ab-all-failed* 0))
(unless (boundp '*ab-all-total*) (setq *ab-all-total* 0))

(defun ab-record (name expected actual)
  (if (equal expected actual)
      (progn
        (setq *ab-passed* (+ *ab-passed* 1))
        (print (list :ansi-bootstrap-test name :pass)))
      (progn
        (setq *ab-failed* (+ *ab-failed* 1))
        (print (list :ansi-bootstrap-test name :fail
                     :expected expected :actual actual))))
  nil)

(defun ab-test (name expected actual)
  (ab-record name expected actual))

(defun ab-summary (phase)
  (let ((failed (- *ab-total* *ab-passed*)))
    (setq *ab-all-passed* (+ *ab-all-passed* *ab-passed*))
    (setq *ab-all-failed* (+ *ab-all-failed* failed))
    (setq *ab-all-total* (+ *ab-all-total* *ab-total*))
    (print (list :ansi-bootstrap-summary phase
                 :passed *ab-passed* :failed failed :total *ab-total*))
    (print (list :ansi-bootstrap-total
                 :passed *ab-all-passed*
                 :failed *ab-all-failed*
                 :total *ab-all-total*)))
  nil)

(ab-test :list-star
         '(1 2 . 3)
         (list* 1 2 3))

(defparameter *ab-alias-left* (list nil))
(defparameter *ab-alias-right* *ab-alias-left*)
(setf (cdr *ab-alias-right*) (list 'entry))

(ab-test :global-cons-identity
         '(t (nil entry) (nil entry))
         (list (if (eq *ab-alias-left* *ab-alias-right*) t nil)
               *ab-alias-left*
               *ab-alias-right*))

(ab-test :push-single-value
         '((1))
         (let ((stack nil))
           (multiple-value-list (push (values 1 2) stack))))

(ab-test :generalized-pop
         '(1 ((2)))
         (let ((place (list (list 1 2))))
           (list (pop (car place)) place)))

(ab-test :vector-and-aref-pop
         '(1 (2))
         (let ((vector (vector '(1 2))))
           (list (pop (aref vector 0))
                 (aref vector 0))))

(ab-test :hash-eq-identity
         '(t t)
         (let* ((key (list 'key))
                (value (list 'value))
                (table (make-hash-table :test 'eq)))
           (setf (gethash key table) value)
           (list (if (eq value (gethash key table)) t nil)
                 (if (null (gethash (list 'key) table)) t nil))))

(ab-test :hash-equal-lookup
         '(value t)
         (let ((table (make-hash-table :test 'equal)))
           (setf (gethash '(a b) table) 'value)
           (multiple-value-bind (value present)
               (gethash (list 'a 'b) table)
             (list value (if present t nil)))))

(ab-test :hash-mutation
         '(2 t 1 t 0)
         (let ((table (make-hash-table)))
           (setf (gethash 'a table) 1)
           (setf (gethash 'b table) 2)
           (let ((before (hash-table-count table))
                 (removed (remhash 'a table))
                 (after (hash-table-count table)))
             (clrhash table)
             (list before (if removed t nil) after
                   (if (= (hash-table-count table) 0) t nil)
                   (hash-table-count table)))))

(defparameter *ab-copy-init-count* 0)

(defstruct ab-entry
  name
  (value (incf *ab-copy-init-count*)))

(ab-test :structure-accessors
         '(name changed)
         (let ((entry (make-ab-entry :name 'name :value 'value)))
           (setf (ab-entry-value entry) 'changed)
           (list (ab-entry-name entry)
                 (ab-entry-value entry))))

(ab-test :structure-copy
         '(1 value copied nil)
         (let* ((entry (make-ab-entry :name 'name))
                (copy (copy-ab-entry entry)))
           (setf (ab-entry-value copy) 'copied)
           (list *ab-copy-init-count*
                 (ab-entry-value entry)
                 (ab-entry-value copy)
                 (if (eq entry copy) t nil))))

(defpackage ab-structure-home (:use common-lisp))
(defpackage ab-copier-home (:use common-lisp))

(defstruct (ab-structure-home::record
            (:copier ab-copier-home::clone-record))
  value)

(ab-test :explicit-copier-package
         '(t nil ab-structure-home::record)
         (list (if (fboundp 'ab-copier-home::clone-record) t nil)
               (if (fboundp 'ab-structure-home::clone-record) t nil)
               (type-of
                (ab-copier-home::clone-record
                 (ab-structure-home::make-record :value 7)))))

(ab-summary 'cons-hash-structures)
