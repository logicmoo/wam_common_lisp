
'(load "sanity-util")
'(require 'sanity-util)


;; 3.5 slots

(defclass daft-point () ((x :accessor daft-x :initarg :x)(y :accessor daft-y :initform 3.14159)(z :reader daft-z :allocation :class)))

(defparameter my-daft-point (make-instance 'daft-point))

(setf (slot-value my-daft-point 'z) 66642)

(defparameter my-daft-point2 (make-instance 'daft-point :x 19))


(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (daft-z my-daft-point))

(let ((temp (make-instance 'daft-point)))
  (setf (daft-y temp) 999
        (slot-value temp 'z) 0))

(list (daft-x my-daft-point)
      (daft-y my-daft-point)
      (daft-z my-daft-point))


