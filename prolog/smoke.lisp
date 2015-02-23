;; Test macro
(defmacro is (eqf expected actual)
  (let ((a (gensym "a")) (b (gensym "b")))
    `(let ((,a ,expected) (,b ,actual))
       (if (not (,eqf ,a ,b))
         (progn
           (format t "FAILED: when matching ~a and ~a~%" ',expected ',actual)
           (quit 1))
         (format t "OK: ~a is ~a to ~a~%" ',expected ',eqf ',actual)))))

(write-line "Running smoke test!")

(is eq 1 1)
(is equal (list 1 'a 'b) (cons 1 '(a b)))

(is eq 2 (if nil 1 2))

(is eq t (keywordp :k))

(is eq 10 (if t 10 20))

(is eq t (stringp "abc"))
(is equal (subseq "abc" 1) "bc")

(is eq 1 (if t 1 2))
(is eq 2 (if nil 1 2))

(defun accum (r) (if (= 0 r) (list 0) (cons r (accum (- r 1)))))
(is equal (list 4 3 2 1 0) (accum 4))

(defmacro defwrap (name) `(defun ,name () 1))
(defwrap foo)
(is eq 1 (foo))
(is equal (macroexpand-1 '(defwrap foo)) '(defun foo nil 1))

(write-line "PASSED")
(quit 0)
