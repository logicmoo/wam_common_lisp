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

#+HAS_SHIFTF (shiftf (cute-p Eric) t)

(slot-value Eric 'diet)




