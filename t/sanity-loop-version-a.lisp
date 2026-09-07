;; Frozen phase-A LOOP fallback.  Keep these tests separate from CL:LOOP so
;; later ANSI work cannot silently change SYS::PRACTICAL-LOOP.

(is equal
    '(1 2 3)
    (sys::practical-loop for i from 1 to 3 collect i))

(is equal
    '(a a a)
    (sys::practical-loop repeat 3 collect 'a))

(is equal
    '(10 20 30)
    (sys::practical-loop for x in '(10 20 30) collect x))

(is eql
    6
    (sys::practical-loop for i from 1 to 3 sum i))

(is equal
    '(2 4)
    (sys::practical-loop for i from 1 to 4
                         when (evenp i) collect i))

(is eql
    4
    (let ((n 0))
      (sys::practical-loop
        (setq n (+ n 1))
        (when (= n 4) (return n)))))

(is equal
    '(1 2 3)
    (sys::practical-loop for i from 1 to 10
                         collect i into values
                         when (= i 3) do (loop-finish)
                         finally (return values)))

(is equal
    '(3 7)
    (sys::practical-loop for (a b) in '((1 2) (3 4))
                         collect (+ a b)))

(is equal
    '(10 20 30)
    (sys::practical-loop for x in '(1 2 3)
                         as y = (* x 10)
                         collect y))

(is eql
    t
    (sys::practical-loop for i from 1 to 4 always (< i 5)))
