;; Standalone ANSI bootstrap tests: multiple values and non-local control.
;; Run from the repository root:
;;   .\wam_cl.cmd < t\ansi-test-wamcl\bootstrap\04-control-and-values.lisp

(in-package :cl-user)

(defparameter *ab-passed* 0)
(defparameter *ab-failed* 0)
(defparameter *ab-total* 14)
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

(ab-test :zero-values
         nil
         (multiple-value-list (values)))

(ab-test :multiple-values
         '(a b c)
         (multiple-value-list (values 'a 'b 'c)))

(ab-test :multiple-value-bind
         '(1 2 3)
         (multiple-value-bind (a b c) (values 1 2 3)
           (list a b c)))

(ab-test :stale-values-cleared
         '(3)
         (progn
           (values 1 2)
           (multiple-value-list 3)))

(ab-test :block-values
         '(1 2)
         (multiple-value-list
          (block result
            (return-from result (values 1 2)))))

(ab-test :catch-values
         '(1 2)
         (multiple-value-list
          (catch 'result
            (throw 'result (values 1 2)))))

(ab-test :catch-tag-values-cleared
         '(42)
         (multiple-value-list
          (catch (values 'tag 'ignored)
            42)))

(ab-test :ignore-errors-success-values
         '(1 2)
         (multiple-value-list
          (ignore-errors (values 1 2))))

(ab-test :ignore-errors-preserves-throw
         42
         (catch 'ansi-bootstrap
           (ignore-errors
             (throw 'ansi-bootstrap 42))))

(ab-test :ignore-errors-error-values
         '(2 t t t)
         (let ((values
                (multiple-value-list
                 (ignore-errors (error "bootstrap error")))))
           (list (length values)
                 (if (null (car values)) t nil)
                 (if (cadr values) t nil)
                 (if (ignore-errors
                       (typep (cadr values) 'error)) t nil))))

(ab-test :tagbody-go
         2
         (let ((value 0))
           (tagbody
              (setq value 1)
              (go target)
              (setq value 99)
            target
              (setq value (+ value 1)))
           value))

(ab-test :nested-lexical-go
         1
         (block result
           (let (escape)
             (tagbody
                (setq escape (lambda () (go outer-target)))
                (tagbody
                   (funcall escape)
                 outer-target
                   (return-from result 99))
              outer-target
                (return-from result 1)))))

(defun ab-recursive-go (number escape)
  (block done
    (tagbody
     start
       (if escape
           (funcall escape)
           (let ((jump (lambda () (go target))))
             (return-from done (ab-recursive-go 0 jump))))
     target
       (return-from done number))))

(ab-test :recursive-lexical-go
         1
         (ab-recursive-go 1 nil))

(ab-test :unwind-protect-cleanup
         '(:done :cleaned)
         (let ((cleanup nil))
           (let ((result
                  (block done
                    (unwind-protect
                         (return-from done :done)
                      (setq cleanup :cleaned)))))
             (list result cleanup))))

(ab-summary 'control-and-values)
