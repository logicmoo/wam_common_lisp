(defclass foo-meta-class (standard-class) ())

(defclass foo-standard-direct-slot-definition (standard-direct-slot-definition)
  ((foo :initform nil :initarg :foo)))

(defclass foo-standard-effective-slot-definition (standard-effective-slot-definition)
  ((foo :initform nil :initarg :foo)))

(defmethod clos:direct-slot-definition-class ((class foo-meta-class) &rest initargs)
  (find-class 'foo-standard-direct-slot-definition))

(defmethod clos:effective-slot-definition-class ((class foo-meta-class) &rest initargs)
  (find-class 'foo-standard-effective-slot-definition))

(defclass foo ()
  ((a :initarg :a :foo :bar))
  (:metaclass foo-meta-class))

(find-class 'foo)

(class-direct-slots *)

(describe (first *))
