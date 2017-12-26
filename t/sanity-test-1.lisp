
;; 3.1. Review of defstruct

(progn #+WAM-CL (prolog-inline "nop(trace)")(is eq 'point (defstruct point x y z)))
;; (defstruct point x y z)

(is eq 'point4d (defstruct point4d x y z t))

(defun distance-from-origin (point)
  (let* ((x (point-x point))
         (y (point-y point))
         (z (point-z point)))
    (sqrt (+ (* x x) (* y y) (* z z)))))

(defun reflect-in-y-axis (point)
  (setf (point-y point)
        (- (point-y point))))

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

; (is eq t (equal my-point a-similar-point))

(is eq nil (eq my-point a-similar-point))

(equalp my-point a-similar-point)

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

