;; Practical LOOP subset regression tests.

(is equal
    '(1 2 3)
    (loop for i from 1 to 3 collect i))

(is eql
    :x
    (loop finally (return :x)
          for i from 1 to 3
          collect i))

(is equal
    '(0 0)
    (loop with (i f) of-type (integer fixnum)
          repeat 1
          collect i into integers
          collect f into floats
          finally (return (list (car integers) (car floats)))))

(is equal
    '((1 2 1))
    (loop with (a b) = '(1 2)
          with c = a
          repeat 1
          collect (list a b c)))

(is equal
    '(1 3 5)
    (loop for i from 1 below 6 by 2 collect i))

(is equal
    '(1 3 5)
    (loop for i by 2 below 6 from 1 collect i))

(is equal
    '(1 2 3)
    (loop for i fixnum to 3 from 1 collect i))

(is equal
    '(0 2 4)
    (loop for i by 2
          repeat 3
          collect i))

(is equal
    '(5 3 1)
    (loop for i downfrom 5 above 0 by 2 collect i))

(is equal
    '(5 4 3 2 1)
    (loop for i downfrom 5 to 1 collect i))

(is equal
    '(10 20 30)
    (loop for x in '(10 20 30) collect x))

(is equal
    '((1 2 3) (2 3) (3))
    (loop for tail on '(1 2 3) collect tail))

(is equal
    '(#\a #\b #\c)
    (loop for ch across "abc" collect ch))

(is equal
    '(a a a a)
    (loop repeat 4 collect 'a))

(is equal
    '(1 2)
    (loop repeat 2
          for i from 1
          collect i))

(is equal
    '(1)
    (loop :repeat 1 :collect 1))

(is eql
    15
    (loop for i from 1 to 5 sum i))

(is eql
    3
    (loop for i from 1 to 6 count (oddp i)))

(is equal
    '(1 1 2 2 3 3)
    (loop for i from 1 to 3 append (list i i)))

(is equal
    '(2 4 6)
    (loop for i from 1 to 6 when (evenp i) collect i))

(is equal
    '(1 3 5)
    (loop for i from 1 to 6 unless (evenp i) collect i))

(is equal
    '(1 2 4 8)
    (loop for i = 1 then (* i 2)
          while (< i 10)
          collect i))

(is eql
    4
    (let ((n 0))
      (loop
        (setq n (+ n 1))
        (when (= n 4) (return n)))))

(is equal
    '(1 2 3)
    (loop for i from 1 to 10
          collect i into values
          when (= i 3) do (loop-finish)
          finally (return values)))

(is equal
    '(finished started)
    (loop with events = nil
          repeat 1
          initially (setq events (cons 'started events))
          finally
            (setq events (cons 'finished events))
            (return events)))

(is eql
    3
    (loop named answer
          for i from 1 to 10
          when (= i 3) return i))

(is eql
    t
    (loop for i from 1 to 4 always (< i 5)))

(is eql
    t
    (loop for i from 1 to 4 never (> i 5)))

(is eql
    4
    (loop for i from 1 to 6 thereis (and (evenp i) (> i 2) i)))

(is eql
    9
    (loop for i in '(2 9 4 7) maximize i))

(is eql
    2
    (loop for i in '(2 9 4 7) minimize i))

(is equal
    '(1 2 3)
    (loop with seen = nil
          for i from 1
          do (setq seen (append seen (list i)))
          until (= i 3)
          finally (return seen)))

(is equal
    '(odd even odd)
    (loop for i from 1 to 3
          if (oddp i)
            collect 'odd
          else
            collect 'even
          end))

(is equal
    '(1 2 3 4)
    (loop for tail on '(1 2 3 4) by #'cddr
          append (if (cdr tail)
                     (list (car tail) (cadr tail))
                     (list (car tail)))))

(is eql
    1
    (loop with calls = 0
          for tail on '(1 2 3)
            by (progn
                 (setq calls (+ calls 1))
                 #'cdr)
          finally (return calls)))

(is equal
    '(1 2 3)
    (loop with n = 0
          for x = (setq n (+ n 1))
          repeat 3
          collect x))

(is equal
    '((1 9))
    (let ((a 9))
      (loop with a = 1 and b = a
            repeat 1
            collect (list a b))))

(is equal
    '((1 1))
    (let ((a 9))
      (loop with a = 1
            with b = a
            repeat 1
            collect (list a b))))

(is equal
    '((1 10) (2 11) (3 12))
    (loop for i from 1 to 3
          and j from 10 to 12
          collect (list i j)))

(is equal
    '(3 7)
    (loop for (a b) in '((1 2) (3 4))
          collect (+ a b)))

(is equal
    '(10 20 30)
    (loop for x in '(1 2 3)
          as y = (* x 10)
          collect y))

(is equal
    '((1 nil) (2 2) (3 3))
    (loop for x in '(1 2 3)
          for y = nil then x
          collect (list x y)))

(is equal
    '((1 10) (2 12) (3 15))
    (loop for a = 1 then (+ a 1)
          for b = 10 then (+ b a)
          repeat 3
          collect (list a b)))

(is equal
    '((1 10) (2 11) (3 13))
    (loop for a = 1 then (+ a 1)
          and b = 10 then (+ b a)
          repeat 3
          collect (list a b)))

(is equal
    '((1 9))
    (let ((a 9))
      (loop for a = 1 then a
            and b = a then b
            repeat 1
            collect (list a b))))

(is equal
    '((1 9))
    (let ((a 9))
      (loop for a = 1
            and b = a
            repeat 1
            collect (list a b))))

(is equal
    '(1 2)
    (loop for tail on '(1 2 . 3)
          collect (car tail)))

(is equal
    '(() a b)
    (loop with seen-nil = nil
          for x in '(nil a nil b)
          if (null x)
            unless seen-nil
              collect x
              and do (setq seen-nil t)
            end
          else
            collect x
          end))

(is equal
    '(2 4)
    (loop for x in '(nil 2 nil 4)
          when x collect it))

(is equal
    '(nil)
    (loop repeat 1
          if nil collect 'yes
          else collect it))

(is equal
    '((loop-finish))
    (loop repeat 1 collect `(loop-finish)))

(is equal
    '(1 1 2 2 3 3)
    (loop for i from 1 to 3
          collect i
          append (list i)))

(is eql
    8
    (loop for i from 1 to 3
          sum i
          count (oddp i)))

(is eql
    6
    (loop for i from 1 to 3
          sum i fixnum))

(is eql
    3
    (loop for i from 1 to 5
          count (oddp i) into odds fixnum
          finally (return odds)))

(is eql
    6
    (loop for i from 1 to 3
          sum i t))

(is eql
    3
    (loop for i from 1 to 3
          count i t))

(is eql
    6
    (loop initially (setq n 5)
          repeat 1
          count t into n
          finally (return n)))

(is equal
    '((1 2 3) (1 2 3) (1 2 3) (1 2 3) (1 2 3))
    (loop for outer from 1 to 5
          collect (loop for inner from 1 to 3
                        collect inner)))

(is equal
    '(3 3 3 3 3)
    (loop for outer from 1 to 5
          collect (loop for inner from 1 to 3
                        sum 1)))

;; Runtime helpers exercised by LOOP.
(is equal '(1 2 3) (append '(1) '(2) '(3)))
(is equal '(1 2 . 3) (append '(1 2) 3))
(is equal '(3 2 1) (reverse '(1 2 3)))
(is equal '(3 2 1) (nreverse (list 1 2 3)))

(is equal
    '(1 2)
    (loop for piece in (list (list 1) (list 2))
          nconc piece))

(is equal
    '((1 2) (2))
    (let* ((first (list 1))
           (second (list 2))
           (result (nconc first second)))
      (list result (cdr first))))

(is equal
    '(0)
    (loop with x fixnum
          repeat 1
          collect x))

(is equal
    '(0)
    (loop with x of-type integer
          repeat 1
          collect x))

(is eql
    42
    (loop initially (loop-finish)
          finally (return 42)))

(is eql
    t
    (let* ((first (list 1))
           (second (list 2)))
      (nconc first second)
      (eq (cdr first) second)))

(is eql
    t
    (let ((table (make-hash-table)))
      (sys::puthash 'a table 10)
      (sys::puthash 'b table 20)
      (let ((keys (loop for key being each hash-key of table
                        collect key)))
        (and (= (length keys) 2)
             (not (null (member 'a keys)))
             (not (null (member 'b keys)))))))

(is eql
    'equal
    (hash-table-test (make-hash-table :test #'equal)))

(is equal
    '(nil)
    (let ((iterator (sys::hash-table-iterator (make-hash-table))))
      (multiple-value-list (sys::hash-table-iterate iterator))))

(is eql
    30
    (let ((table (make-hash-table)))
      (sys::puthash 'a table 10)
      (sys::puthash 'b table 20)
      (loop for value being the hash-values in table
            sum value)))

(is eql
    t
    (let ((table (make-hash-table)))
      (sys::puthash 'a table 10)
      (sys::puthash 'b table 20)
      (loop for key being each hash-key of table
            for same = key
            always (eq key same))))

(is eql
    t
    (let ((table (make-hash-table)))
      (sys::puthash 'a table 10)
      (sys::puthash 'b table 20)
      (let ((pairs (loop for key being each hash-key of table
                           using (hash-value value)
                         collect (cons key value))))
        (and (= (length pairs) 2)
             (eql (cdr (assoc 'a pairs)) 10)
             (eql (cdr (assoc 'b pairs)) 20)))))

(is eql
    t
    (let ((table (make-hash-table)))
      (sys::puthash 'a table 10)
      (sys::puthash 'b table 20)
      (let ((pairs (loop for value being each hash-value of table
                           using (hash-key key)
                         collect (cons key value))))
        (and (= (length pairs) 2)
             (eql (cdr (assoc 'a pairs)) 10)
             (eql (cdr (assoc 'b pairs)) 20)))))

(is eql
    t
    (> (loop for symbol being each external-symbol
               of (find-package "COMMON-LISP")
             count symbol)
       900))

(is eql
    t
    (> (loop for symbol being each present-symbol
               in (find-package "COMMON-LISP")
             count symbol)
       900))

(is eql
    t
    (> (loop for symbol being each symbol
               of (find-package "CL-USER")
             count symbol)
       0))

(is eql
    t
    (> (loop for symbol being each symbol
             count symbol)
       0))

;; Keep this reader-sensitive RETURN case last: WAM-CL's current block/reader
;; path reports end-of-input after it, although the assertion itself succeeds.
(is equal
    '(1 2 3 nil)
    (loop with (a b c d) = '(1 2 3)
          return (list a b c d)))
