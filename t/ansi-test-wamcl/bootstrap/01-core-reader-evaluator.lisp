;; Standalone ANSI bootstrap tests: reader, evaluator, macros, and lambda lists.
;; Run from the repository root:
;;   .\wam_cl.cmd < t\ansi-test-wamcl\bootstrap\01-core-reader-evaluator.lisp
;; The seven files can also be concatenated in numeric order; each summary then
;; includes a cumulative :ANSI-BOOTSTRAP-TOTAL.

(in-package :cl-user)

(defparameter *ab-passed* 0)
(defparameter *ab-failed* 0)
(defparameter *ab-total* 12)
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

(ab-test :list-evaluation
         '(a (b c) 3)
         (list 'a (list 'b 'c) (+ 1 2)))

(ab-test :backquote-splicing
         '(1 2 3 4)
         (let ((middle '(2 3))
               (tail 4))
           `(1 ,@middle ,tail)))

(ab-test :uninterned-symbol
         t
         (if (null (symbol-package '#:ansi-bootstrap-uninterned)) t nil))

(ab-test :vector-reader
         '(t 3 2)
         (list (if (vectorp '#(1 2 3)) t nil)
               (length '#(1 2 3))
               (aref '#(1 2 3) 1)))

(ab-test :character-and-string-reader
         '(t t t)
         (list (if (characterp #\A) t nil)
               (if (stringp "ABC") t nil)
               (if (char= #\A (char "ABC" 0)) t nil)))

(defun ab-optional (x &optional (y 2))
  (+ x y))

(ab-test :optional-arguments
         '(5 8)
         (list (ab-optional 3)
               (ab-optional 3 5)))

(defun ab-rest (x &rest rest)
  (cons x rest))

(ab-test :rest-arguments
         '(1 2 3 4)
         (ab-rest 1 2 3 4))

(defun ab-key (&key (x 1) (y 2))
  (+ x y))

(ab-test :keyword-arguments
         '(3 9)
         (list (ab-key)
               (ab-key :y 5 :x 4)))

(defun ab-aux (x &aux (y (+ x 1)))
  y)

(ab-test :auxiliary-arguments
         5
         (ab-aux 4))

(ab-test :destructuring-bind
         '(1 2 3)
         (destructuring-bind (a (b c)) '(1 (2 3))
           (list a b c)))

(ab-test :local-functions
         '(6 120)
         (list (flet ((plus-one (x) (+ x 1)))
                 (plus-one 5))
               (labels ((factorial (n)
                          (if (= n 0)
                              1
                              (* n (factorial (- n 1))))))
                 (factorial 5))))

(defmacro ab-pair (left right)
  `(list ,left ,right))

(ab-test :macro-expansion
         '(left right)
         (ab-pair 'left 'right))

(ab-summary 'core-reader-evaluator)
