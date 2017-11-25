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



;; 3.2. defclass

(unintern 'point)

(defclass point ()
  (x
   y
   z))

(setf my-point (make-instance 'point))

(type-of my-point)

(defun set-point-values (point x y z)
  (setf (slot-value point 'x) x
        (slot-value point 'y) y
        (slot-value point 'z) z))

(set-point-values my-point 3 4 12)

(defun distance-from-origin (point)
  (with-slots (x y z)
      point
    (sqrt (+ (* x x) (* y y) (* z z)))))


(DISASSEMBLE #'distance-from-origin)


(distance-from-origin my-point)

;; 3.3. classes are objects

(find-class 'point)

(class-name (find-class 'point))

(class-of my-point)

;; #-(or cormanlisp CLISP WAM-CL)
(typep my-point (class-of my-point))

(class-of (class-of my-point))

;; 3.4. you don't need clos to use clos

(let ((the-symbol-class (find-class 'symbol)))
  (values the-symbol-class
          (class-name the-symbol-class)
          (eq the-symbol-class (class-of 'symbol))
          (class-of the-symbol-class)))

(find-class t)

(defstruct foo)

(class-of (make-foo))

;; 3.5 slots

(defclass daft-point ()
  ((x :accessor daft-x :initarg :x)
   (y :accessor daft-y :initform 3.14159)
   (z :reader daft-z :allocation :class)))

(setf (slot-value (make-instance 'daft-point) 'z) 42)

(setf my-daft-point (make-instance 'daft-point :x 19))

(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (daft-z my-daft-point))

(let ((temp (make-instance 'daft-point)))
  (setf (daft-y temp) 999
        (slot-value temp 'z) 0))

(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (daft-z my-daft-point))

;; 3.6 Subclasses and inheritance

(defclass animal ()
  ((legs :reader leg-count :initarg :legs)
   (comes-from :reader comes-from :initarg :comes-from)))

(defclass mammal (animal)
  ((diet :initform 'antelopes :initarg :diet)))

(defclass aardvark (mammal)
  ((cute-p :accessor cute-p :initform nil)))

(#-allegro class-direct-superclasses #+allegro aclmop:class-direct-superclasses
   (find-class 'aardvark))

;; ACL needs to instantiate a class before its precedence-list becomes visible
;; #+allegro
(make-instance 'aardvark)

(#-allegro class-precedence-list #+allegro aclmop:class-precedence-list
   (find-class 'aardvark))

(defclass figurine ()
  ((potter :accessor made-by :initarg :made-by)
   (comes-from :initarg :made-in)))

(defclass figurine-aardvark (aardvark figurine)
  ((name :reader aardvark-name :initarg :aardvark-name)
   (diet :initform nil)))

;; ACL needs to instantiate a class before its precedence-list becomes visible
;; #+allegro 
(make-instance 'figurine-aardvark)

(#-allegro class-precedence-list #+allegro aclmop:class-precedence-list
             (find-class 'figurine-aardvark))

(setf Eric (make-instance 'figurine-aardvark
                          :legs 4
                          :made-by "Jen"
                          :made-in "Brittany"
                          :aardvark-name "Eric"))

(shiftf (cute-p Eric) t)

(slot-value Eric 'diet)

;; 3.7 Changing a class

(list Eric (class-of Eric) (slot-exists-p Eric 'has-tail-p))

(defclass animal ()
  ((legs :reader leg-count :initarg :legs)
   (has-tail-p :reader has-tail-p :initform t)
   (comes-from :reader comes-from :initarg :comes-from)))

(list Eric (class-of Eric) #-(or cormanlisp CLISP WAM-CL) (slot-value Eric 'has-tail-p))

(defclass antelope (mammal)
  ((diet :reader munched-by)))

(change-class Eric 'antelope
              :diet 'greens)

(list (slot-exists-p Eric 'potter) (munched-by Eric))

;; 3.8 Implementation notes: object wrappers

#-(or cormanlisp CLISP WAM-CL)
(#+lispworks clos::wrapper-of #+allegro excl::wrapper-of
             Eric)


;; 4.1 Review - etypecase to drive function dispatch

(defun my-describe (thing)
  (typecase thing
    (cons   (describe-cons thing))
    (symbol (describe-symbol thing))
    (array  (describe-array thing))
    (number (describe-number thing))
    ;; [ etc etc etc ]
    (t      (describe-whatever thing))))

(defun describe-symbol (symbol)
  (let ((package (symbol-package symbol))
        (boundp (boundp symbol)))
    (format t
            "~s is a symbol. ~
             It ~:[~*does not have a home~;is in the ~s~] package. ~
             Its value is ~:[unbound~;~s~]."
            symbol
            package (when package (package-name package))
            boundp (when boundp (symbol-value symbol)))))

(my-describe :foo)

(my-describe '#:foo)

;; 4.2 defmethod

(fmakunbound 'my-describe)

(defmethod my-describe (thing)
  (format t
          "~s could be anything, for all I care."
          thing))

(defmethod my-describe ((animal animal))
  (format t
          "~s is an animal. It has ~d leg~:p ~
           and comes from ~a."
          animal
          (leg-count animal)
          (comes-from animal)))

(my-describe Eric)

(my-describe (make-instance 'figurine))

(mapcar 'class-name
        (#-allegro class-precedence-list #+allegro aclmop:class-precedence-list
	   (class-of Eric)))

;; 4.3 Generic functions and method combination

#'my-describe

(#-allegro generic-function-methods #+allegro aclmop:generic-function-methods
   #'my-describe)

(#-allegro method-generic-function #+allegro aclmop:method-generic-function
   (car *))

(defmethod my-describe ((antelope antelope))
  (if (string= (slot-value antelope 'comes-from)
               "Brittany")
      (format t "Eric? Is that you?")
    (call-next-method)))

(my-describe 
 (make-instance 'antelope :comes-from 'nowhere :legs 4))

(my-describe Eric)

;; 4.5. Other specializers (you still don't need CLOS objects to use CLOS)

(defmethod my-describe ((self #+(or lispworks allegro) structure-object #+(or cormanlisp CLISP WAM-CL) structure))
  (format t "~s is a structure object."
          self))

(my-describe (make-foo))

(defmethod my-describe ((self foo))
  (format t "bar"))

(my-describe (make-foo))

(defmethod my-describe ((self (eql pi)))
  (format t "approximately 22/7"))

(defmethod my-describe ((self float))
  (format t "some float"))

(my-describe pi)

;; 4.6. Qualifiers and method combination

(defmethod my-describe :around (self)
  (call-next-method)
  (values))

(my-describe Eric)

