;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;; GOODIES for defining a-lists
(defun add-to-alist (a-list key datum)
  (let* ((old-value (rest (assoc key a-list)))
         (new-value (if old-value
                      (union (list datum) old-value)
                      (list datum))))
    (if old-value
      (rplacd (assoc key a-list) new-value)
      (setf a-list (acons key new-value a-list)))
      a-list))

(defun print-occus (consat-result)
  (mapcar #'(lambda (x) (if x (print x))) consat-result))

(defmacro map-append (function list)
  `(let ((result nil))
     (dolist (item ,list result)
       (setf result (append result (funcall ,function item))))))

(defun constraint-with-list-back (normal-prove-result)
  (map-append '(lambda (x) (cdar x)) normal-prove-result))


#|
(defun kreuzp (menge1 menge2)
  (let ((result nil))
    (dolist (x1 menge1 (reverse result))
      (setf result (dolist (x2 menge2 result)
        (setf result (append (list (list x1 x2)) result)))))))
|#


(DEF-CLAUSE-SET prolog-stuff
  ((kreuzprodukt _x1 _x2 _set1 _set2)
   <-
   (member _x1 _set1)
   (member _x2 _set2))

  ((get-key-word :true true))
  ((get-key-word :false false))
  ((get-key-word :unknwon unknown))
 
  ((member _x (_x . _tail)))
  ((member _x (_h . _tail)) <- (member _x _tail)))


(DEF-CLAUSE-SET prove-unknown
  ((prove-unknown _arg1 _arg2) <-
   (write "prove-unknown still not implemented !")
   (fail))

  ((prove-unknown _arg1 _arg2 _arg3) <-
   (write "prove-unknown still not implemented !")
   (fail))

  ((prove-unknown _arg1 _arg2 _arg3 _arg4) <-
   (write "prove-unknown still not implemented !")
   (fail)))

;;; ========================================
;;; CONSAT Extensions

(defun def-constraint-net (name primitive-constraint-expressions-list)
  (let ((interface-vars (get-interface-variables primitive-constraint-expressions-list)))
    (eval
     `(DEFCONSTRAINT ,name
        (:TYPE compound)
        (:INTERFACE ,@interface-vars)
        (:CONSTRAINT-EXPRESSIONS ,@primitive-constraint-expressions-list)))))
#|
(DEF-CONSTRAINT-NET 'net17 '((single-room-constraint thomas)
                             (next-door-rooms-constraint thomas monika)
                             (next-door-rooms-constraint thomas hans)
                             (different-rooms-constraint thomas hans)
                             (different-rooms-constraint thomas monika)
                             (different-rooms-constraint monika hans)
))
|#
(defun get-interface-variables (primitive-constraint-expressions-list)
  (reverse (union (map-append #'(lambda (constraint-expression) `@(rest constraint-expression))
          primitive-constraint-expressions-list)
         nil)))

#|
(get-interface-variables '((diff thomas werner)
                           (diff thomas hans)
                           (same thomas)
                           (diff monika werner)
                           (diff monika angi)))
|#

#|
(DEF-CONSTRAINT-NET 'net17 '((single-room-constraint thomas)
                             (next-door-rooms-constraint thomas monika)
                             (next-door-rooms-constraint thomas hans)
                             (different-rooms-constraint thomas hans)
                             (different-rooms-constraint thomas monika)
                             (different-rooms-constraint monika hans)
))
|#



;;; eof

