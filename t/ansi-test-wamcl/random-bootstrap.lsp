;; Focused tests of the unmodified upstream randomization macros.
(in-package :cl-test)

(cl::is eq 'case
    (car (macroexpand-1 '(random-case :first :second))))

(cl::is eq 'let
    (car (macroexpand-1 '(rcase (1 :first) (2 :second)))))

(cl::is eq :only
    (random-case :only))

(cl::is equal '(:only 1)
    (let ((calls 0))
      (list (rcase (1 (incf calls) :only)) calls)))

(cl::is equal t
    (every #'(lambda (value) (or (eq value :first) (eq value :second)))
           (loop repeat 20 collect (random-case :first :second))))

(cl::is equal t
    (every #'(lambda (value) (or (eq value :first) (eq value :second)))
           (loop repeat 20 collect (rcase (1 :first) (2 :second)))))

(cl::is equal '(1 3 6)
    (let ((sum 0))
      (loop for weight in '(1 2 3) collect (incf sum weight))))

(cl::is eq :only
    (random-from-seq '(:only)))

(cl::is equal '(t t t t)
    (list (consp *universe*) (consp *mini-universe*)
         (random-state-p (car *random-states*))
         (if (> *maximum-random-int-bits* 35) t nil)))
