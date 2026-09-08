;; Standalone ANSI bootstrap tests: packages, symbols, and foreign lambda lists.
;; Run from the repository root:
;;   .\wam_cl.cmd < t\ansi-test-wamcl\bootstrap\02-packages-and-lambda-lists.lisp

(in-package :cl-user)

(defparameter *ab-passed* 0)
(defparameter *ab-failed* 0)
(defparameter *ab-total* 10)
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

(defparameter *ab-flat-package*
  (make-package "AB-FLAT-PACKAGE"
                :use '("COMMON-LISP")
                :nicknames '("AB-FLAT")))

(ab-test :make-package-name
         "AB-FLAT-PACKAGE"
         (package-name *ab-flat-package*))

(ab-test :make-package-nickname
         t
         (if (eq *ab-flat-package* (find-package "AB-FLAT")) t nil))

(ab-test :make-package-use-list
         t
         (not (null (member (find-package "COMMON-LISP")
                            (package-use-list *ab-flat-package*)))))

(ab-test :package-used-by-list
         t
         (not (null (member *ab-flat-package*
                            (package-used-by-list "COMMON-LISP")))))

(ab-test :inherited-symbol
         '(t :inherited)
         (multiple-value-bind (symbol status)
             (find-symbol "CAR" *ab-flat-package*)
             (list (if (eq symbol 'car) t nil) status)))

(defparameter *ab-import-package*
  (make-package "AB-IMPORT-PACKAGE" :use nil))

(import (list 'car 'cdr) *ab-import-package*)

(ab-test :import-list
         '(t :internal t :internal)
         (multiple-value-bind (car-symbol car-status)
             (find-symbol "CAR" *ab-import-package*)
           (multiple-value-bind (cdr-symbol cdr-status)
               (find-symbol "CDR" *ab-import-package*)
               (list (if (eq car-symbol 'car) t nil) car-status
                     (if (eq cdr-symbol 'cdr) t nil) cdr-status))))

(export (list 'car 'cdr) *ab-import-package*)

(ab-test :export-list
         '(:external :external)
         (list (nth-value 1 (find-symbol "CAR" *ab-import-package*))
               (nth-value 1 (find-symbol "CDR" *ab-import-package*))))

(defpackage ab-use-a (:use))
(defpackage ab-use-b (:use))
(defpackage ab-definition-package
  (:use ab-use-a)
  (:use ab-use-b)
  (:nicknames ab-definition-a)
  (:nicknames ab-definition-b)
  (:shadow "CAR"))

(ab-test :defpackage-repeated-options
         '(2 t t)
         (list (length (package-use-list "AB-DEFINITION-PACKAGE"))
               (if (eq (find-package "AB-DEFINITION-A")
                       (find-package "AB-DEFINITION-PACKAGE")) t nil)
               (if (eq (find-package "AB-DEFINITION-B")
                       (find-package "AB-DEFINITION-PACKAGE")) t nil)))

(ab-test :defpackage-shadow
         '(t t)
         (let ((symbol (find-symbol "CAR" "AB-DEFINITION-PACKAGE")))
           (list (if (eq (symbol-package symbol)
                         (find-package "AB-DEFINITION-PACKAGE")) t nil)
                 (if (eq symbol
                         (car (package-shadowing-symbols
                               "AB-DEFINITION-PACKAGE"))) t nil))))

(defpackage ab-lambda-package (:use common-lisp))
(in-package ab-lambda-package)

(defun foreign-aux-probe (&aux (value 5))
  value)

(common-lisp-user::ab-test :foreign-package-aux
                           5
                           (foreign-aux-probe))

(common-lisp-user::ab-summary 'packages-and-lambda-lists)
