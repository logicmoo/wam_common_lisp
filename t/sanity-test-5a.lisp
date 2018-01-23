

(is eq 1 (macrolet ((jump (x) `(car ,x))
           (skip (x) `(jump ,x))
           (hop (x) `(skip ,x)))
  (hop '(1 2 3))))

(is equal '(2 3) '(2 3))

(is equal '(yin 6 '(1 2 3))
    (macrolet ((yin (n x)
             (if (zerop n)
                 `(cdr ,x)
                 `(yang ,(1- n) ,x)))
           (yangE (n x)
             (if (zerop n)
                 `(car ,x)
                 `(yin ,(1- n) ,x))))
      (MACROEXPAND '(yin 6 '(1 2 3)))))

(is equal '(CDR '(1 2 3))
    (macrolet ((yin (n x)
             (if (zerop n)
                 `(cdr ,x)
                 `(yang ,(1- n) ,x)))
           (yang (n x)
             (if (zerop n)
                 `(car ,x)
                 `(yin ,(1- n) ,x))))
      (MACROEXPAND '(yin 6 '(1 2 3)))))


(is equal '(yin 6 '(1 2 3))
    (macrolet ((yin (n x)
             (if (zerop n)
                 `(cdr ,x)
                 `(yang ,(1- n) ,x)))
           (yangE (n x)
             (if (zerop n)
                 `(car ,x)
                 `(yin ,(1- n) ,x))))
      (MACROEXPAND '(yin 6 '(1 2 3)))))

