
;; 3.3. classes are objects

(find-class 'point)

(class-name (find-class 'point))

(class-of my-point)

;; #-(or cormanlisp CLISP WAM-CL)
(typep my-point (class-of my-point))

(is eq (find-class 'STANDARD-CLASS)
       (class-of (class-of my-point)))

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

(setf (slot-value (make-instance 'daft-point) 'z) 42)

(setf my-daft-point (make-instance 'daft-point :x 19))


#+PERFECT 
(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (progn #+WAM-CL (prolog-trace) (daft-z my-daft-point)))

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

#+HAS_SHIFTF
(shiftf (cute-p Eric) t)

(slot-value Eric 'diet)


