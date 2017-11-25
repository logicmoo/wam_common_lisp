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



(in-package "CL-USER")


(defun fib (n)
  (if (> n 1)
    (+ (fib (- n 1))
       (fib (- n 2)))
    1))


(DEFUN string_l (x )(COND ((STRINGP x )x )((SYMBOLP x )(symbol-name x ))(T (ERROR "type error" ))))

(TAGBODY 1 (PRINT "hi" ))
 (LET ((val 1 ))NIL )
 (LET ((val 1 ))val )


;; 3.1. Review of defstruct

(defstruct point x y z)

(defstruct point4d x y z t)

(defun distance-from-origin (point)
  (let* ((x (point-x point))
         (y (point-y point))
         (z (point-z point)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

(defun reflect-in-y-axis (point)
  (setf (point-y point)
        (- (point-y point))))

(setf my-point (make-point :x 3 :y 4 :z 12))
(setf my-point2 (make-point :x 3 :y 4 :z 12))
(setf my-point3 #S(POINT :X 3 :Y 4 :Z 12))
(setf my-point4d (make-point4d :x 3 :y 4 :z 12 :t 1))



(point-p my-point)

(type-of my-point)


(progn (print (distance-from-origin my-point)))

(reflect-in-y-axis my-point)

my-point

(setf a-similar-point #s(point :x 3 :y -4 :z 12))

(equal my-point a-similar-point)

(equalp my-point a-similar-point)


