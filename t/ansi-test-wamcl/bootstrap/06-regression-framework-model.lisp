;; Standalone ANSI bootstrap tests: minimal model of ansi-test/rt.lsp storage.
;; Run from the repository root:
;;   .\wam_cl.cmd < t\ansi-test-wamcl\bootstrap\06-regression-framework-model.lisp

(in-package :cl-user)

(defparameter *ab-passed* 0)
(defparameter *ab-failed* 0)
(defparameter *ab-total* 7)
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

(defstruct ab-rt-entry
  pend
  name
  props
  form
  vals)

(defparameter *ab-rt-entries* (list nil))
(defparameter *ab-rt-tail* *ab-rt-entries*)
(defparameter *ab-rt-table* (make-hash-table :test #'equal))

(defun ab-rt-add-entry (entry)
  (setq entry (copy-ab-rt-entry entry))
  (setf (gethash (ab-rt-entry-name entry) *ab-rt-table*)
        *ab-rt-tail*)
  (setf (cdr *ab-rt-tail*) (cons entry nil))
  (setq *ab-rt-tail* (cdr *ab-rt-tail*))
  (ab-rt-entry-name entry))

(defmacro ab-rt-deftest (name &rest body)
  (let* ((remaining body)
         (properties
          (loop while (keywordp (first remaining))
                append (list (pop remaining) (pop remaining))))
         (form (pop remaining))
         (values remaining))
    `(ab-rt-add-entry
      (make-ab-rt-entry :pend t
                        :name ',name
                        :props ',properties
                        :form ',form
                        :vals ',values))))

(ab-rt-deftest rt-one (+ 1 2) 3)
(ab-rt-deftest rt-two :note bootstrap (values 'a 'b) a b)
(ab-rt-deftest rt-three (list 1 2) (1 2))

(ab-test :registered-entry-count
         '(3 3)
         (list (length (cdr *ab-rt-entries*))
               (hash-table-count *ab-rt-table*)))

(ab-test :registered-entry-order
         '(rt-one rt-two rt-three)
         (mapcar #'ab-rt-entry-name (cdr *ab-rt-entries*)))

(ab-test :hash-predecessor-cell
         t
         (if (eq (gethash 'rt-one *ab-rt-table*)
                 *ab-rt-entries*) t nil))

(ab-test :stored-form-and-values
         '((values 'a 'b) (a b))
         (let ((entry
                (cadr (gethash 'rt-two *ab-rt-table*))))
           (list (ab-rt-entry-form entry)
                 (ab-rt-entry-vals entry))))

(ab-test :stored-properties
         '(:note bootstrap)
         (ab-rt-entry-props
          (cadr (gethash 'rt-two *ab-rt-table*))))

(ab-test :copied-entry-independence
         '(old new nil)
         (let* ((entry (make-ab-rt-entry :name 'old))
                (copy (copy-ab-rt-entry entry)))
           (setf (ab-rt-entry-name copy) 'new)
           (list (ab-rt-entry-name entry)
                 (ab-rt-entry-name copy)
                 (eq entry copy))))

(ab-test :empty-handler-bind
         3
         (handler-bind ()
           (+ 1 2)))

(ab-summary 'regression-framework-model)
