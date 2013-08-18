
(load "sanity-util")
'(require 'sanity-util)

;; 3.1. Review of defstruct

(is eq 'point4d (defstruct point4d x y z t))

(progn #+WAM-CL (prolog-inline "nop(trace)")(is eq 'point (defstruct point x y z)))
;; (defstruct point x y z)          


(defun distance-from-origin (point)
  (let* ((x (point-x point))
         (y (point-y point))
         (z (point-z point)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

(defun reflect-in-y-axis (point)
  (setf (point-y point)
        (- (point-y point))))

;; (break)

(list (setf my-point (make-point :x 3 :y 4 :z 12)) (setf my-point2 (make-point :x 3 :y 4 :z 12)))
(setf my-point3 #S(POINT :X 3 :Y 4 :Z 12))
(setf my-point4d (make-point4d :x 3 :y 4 :z 12 :t 1))


(is eq t (point-p my-point))

(is eq 'point (type-of my-point))

;; #+IGNORE #+WAM-CL 



(is eql 13 (progn (print (distance-from-origin my-point))))

;; #+CLISP (BREAK)
;; #+WAM-CL (prolog-call "break")

(is = -4 (reflect-in-y-axis my-point))

(is eq my-point my-point)

(setf a-similar-point #s(point :x 3 :y -4 :z 12))


(is eq nil (eq my-point a-similar-point))

(is equalp my-point a-similar-point)

(is eq t (equalp my-point a-similar-point) )


;; 3.2. defclass

(unintern 'point)

(defclass point ()
  (x
   y
   z))

(setf my-point (make-instance 'point))

(is eq 'point (type-of my-point))

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

(is eql 13 (distance-from-origin my-point))


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

(is eq (find-class 'STANDARD-CLASS)
       (class-of (class-of my-point)))

(defstruct foo)

(class-of (make-foo))

;; 3.4. you don't need clos to use clos

(let ((the-symbol-class (find-class 'symbol)))
  (values the-symbol-class
          (class-name the-symbol-class)
          (eq the-symbol-class (class-of 'symbol))
          (class-of the-symbol-class)))

(find-class t)

(is eq 'foo (defstruct foo))

(is eq (find-class 'foo) (class-of (make-foo)))

;; 3.5 slots

(defclass daft-point ()
  ((x :accessor daft-x :initarg :x)
   (y :accessor daft-y :initform 3.14159)
   (z :reader daft-z :allocation :class)))

#+CLASS_ALLOC (setf (slot-value (make-instance 'daft-point) 'z) 42)

(setf my-daft-point (make-instance 'daft-point :x 19))


#+PERFECT 
(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (progn #+WAM-CL (prolog-trace) (daft-z my-daft-point)))

#+CLASS_ALLOC 
 (let ((temp (make-instance 'daft-point)))
  (setf (daft-y temp) 999
        (slot-value temp 'z) 0))

#+PERFECT
(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (daft-z my-daft-point))

;; 3.6 Subclasses and inheritance

(defclass animal ()
  ((legs :reader leg-count :initarg :legs)
   (comes-from :reader comes-from :initarg :comes-from)))

(defparameter my-animal (make-instance 'animal))

(defclass mammal (animal)
  ((diet :initform 'antelopes :initarg :diet)))

(defparameter my-mammal (make-instance 'mammal))

(defclass aardvark (mammal)
  ((cute-p :accessor cute-p :initform nil)))

(#-allegro class-direct-superclasses #+allegro aclmop:class-direct-superclasses
   (find-class 'aardvark))

;; ACL needs to instantiate a class before its precedence-list becomes visible #+allegro

(defparameter my-aardvark (make-instance 'aardvark))


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

#+HAS_SHIFTF
(shiftf (cute-p Eric) t)

(slot-value Eric 'diet)




