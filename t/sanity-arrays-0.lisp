#|
;; $Id: examples.lisp,v 1.1 2003/10/21 17:30:56 nhabedi Exp $
;;                          EXAMPLES.LISP
;;           Nick Levine, Ravenbrook Limited, 2003-08-14
;; 
;; These are the examples I expect to use in the tutorial on CLOS
;; at the International Lisp Conference 2003.
;; 
;; This document is mainly for my operational convenience. You might
;; want to raid fragments to help you get started when building CLOS
;; into your Common Lisp applications. Nothing useful will happen if
;; you try to cl:load this document into a lisp image.
;;
;; This document is provided "as is", without any express or implied
;; warranty.  In no event will the author be held liable for any
;; damages arising from the use of this document.  You may make and
;; distribute verbatim copies of this document provided that you do
;; not charge a fee for this document or for its distribution.
|#

; #+WAM-CL (prolog-call "cls.")

(defun mapcar-visualize (func l) (if (null l) () (cons (apply func (list (first l))) (mapcar func (rest l)))))

(in-package "CL-USER")

' (load "sanity-util")
'(require 'sanity-util)

(write-line "Running smoke test!")


(is equal #(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL) (write (setf my-array (make-array '(10)))))
(terpri)
(setf (aref my-array 0) 25)
(setf (aref my-array 1) 23)
(setf (aref my-array 2) 45)
(setf (aref my-array 3) 10)
(setf (aref my-array 4) 20)
(setf (aref my-array 5) 17)
(setf (aref my-array 6) 25)
(setf (aref my-array 7) 19)
(setf (aref my-array 8) 67)
(setf (aref my-array 9) 30)
(is equal #(25 23 45 10 20 17 25 19 67 30) (write my-array))

;; Example 2
(setf x (make-array '(3 3) :initial-contents '((0 1 2 ) (3 4 5) (6 7 8))) )
(is equal #2A((0 1 2) (3 4 5) (6 7 8)) (write x))


;; Example 3
(setq a (make-array '(4 3)))
(dotimes (i 4)
   (dotimes (j 3)
      (setf (aref a i j) (list i 'x j '= (* i j))) ))
(dotimes (i 4)  (dotimes (j 3)(print (aref a i j))))
#|

Executing the program....
$clisp main.lisp
(0 X 0 = 0) 
(0 X 1 = 0) 
(0 X 2 = 0) 
(1 X 0 = 0) 
(1 X 1 = 1) 
(1 X 2 = 2) 
(2 X 0 = 0) 
(2 X 1 = 2) 
(2 X 2 = 4) 
(3 X 0 = 0) 
(3 X 1 = 3) 
(3 X 2 = 6) 

|#


(setq myarray (make-array '(3 2 3) 
   :initial-contents 
   '(((a b c) (1 2 3)) 
      ((d e f) (4 5 6)) 
      ((g h i) (7 8 9)) 
   ))
) 
(setq array2 (make-array 4 :displaced-to myarray :displaced-index-offset 2)) 
(is equal #3A(((A B C) (1 2 3)) ((D E F) (4 5 6)) ((G H I) (7 8 9)))  (write myarray))
(terpri)
(is equal #(C 1 2 3) (write array2))



(setq myarray (make-array '(3 2 3) 
   :initial-contents 
   '(((a b c) (1 2 3)) 
      ((d e f) (4 5 6)) 
      ((g h i) (7 8 9)) 
   ))
) 
(setq array2 (make-array '(3 2) :displaced-to myarray :displaced-index-offset 2)) 
(is equal #3A(((A B C) (1 2 3)) ((D E F) (4 5 6)) ((G H I) (7 8 9)))  (write myarray))
(terpri)
(is equal #2A((C 1) (2 3) (D E)) (write array2))



(setq myarray (make-array '(3 2 3) 
   :initial-contents 
   '(((a b c) (1 2 3)) 
      ((d e f) (4 5 6)) 
      ((g h i) (7 8 9)) 
   ))
) 
(setq array2 (make-array '(3 2) :displaced-to myarray :displaced-index-offset 5)) 
(is equal #3A(((A B C) (1 2 3)) ((D E F) (4 5 6)) ((G H I) (7 8 9)))  (write myarray))
(terpri)
(is equal #2A((3 D) (E F) (4 5)) (write array2))





;a one dimensional array with 5 elements, 
;initail value 5
(is equal #(5 5 5 5 5) (write (make-array 5 :initial-element 5)))
(terpri)

;two dimensional array, with initial element a
(is equal #2A((A A A) (A A A)) (write (make-array '(2 3) :initial-element 'a)))
(terpri)

;an array of capacity 14, but fill pointer 5, is 5
(is equal 5 (write(length (make-array 14 :fill-pointer 5))))
(terpri)

;however its length is 14
(is equal (14) (write (array-dimensions (make-array 14 :fill-pointer 5))))
(terpri)

; a bit array with all initial elements set to 1
(is equal #*1111111111 (write(make-array 10 :element-type 'bit :initial-element 1)))
(terpri)

; a character array with all initial elements set to a
; is a string actually
(is equal "aaaaaaaaaa" (write(make-array 10 :element-type 'character :initial-element #\a)) )
(terpri)

; a two dimensional array with initial values a
(setq myarray (make-array '(2 2) :initial-element 'a :adjustable t))
(is equal #2A((A A) (A A)) (write myarray) )
(terpri)

;readjusting the array
(adjust-array myarray '(1 3) :initial-element 'b) 
(is equal #2A((A A B)) (write myarray))

