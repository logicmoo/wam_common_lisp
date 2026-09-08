;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jul 27 09:32:46 2004
;;;; Contains: Tests involving PRINT-LINES

(deftest print-lines.1
  *print-lines*
  nil)

;; original test had different expected values, but print margin is in
;; ems and I think there is no definite anwer what this should print.
#+(or)
(deftest print-lines.2
  (with-standard-io-syntax
   (let ((*print-lines* 1)
         (*print-readably* nil)
         (*print-miser-width* nil)
         (*print-pprint-dispatch* (copy-pprint-dispatch)))
     (set-pprint-dispatch '(cons (eql 1) t) 'pprint-fill)
     (apply
      #'values
      (loop
         for i from 1 to 10
         for s in '("(1 ..)"
                    "(1 ..)"
                    "(1 ..)"
                    "(1 ..)"
                    "(1 2 ..)"
                    "(1 2 ..)"
                    "(1 2 3 ..)"
                    "(1 2 3 ..)"
                    "(1 2 3 4 ..)"
                    "(1 2 3 4 ..)")
         collect
           (let ((result
                  (let ((*print-right-margin* i))
                    (subseq
                     (with-output-to-string (*standard-output*)
                       (terpri)
                       (pprint '(1 2 3 4 5 6 7 8 9)))
                     2))))
             (or (equal s result)
                 (list s result)))))))
  T T T T T T T T T T)
