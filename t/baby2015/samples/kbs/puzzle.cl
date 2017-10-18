;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;; ************* KNOWLEDGE BASE  DECLARATION ***********


(def-kb-instance puzzle puzzlec)


;;;
;;;        NINE PUZZLE
;;;

(defconstraint triangle
  (:type primitive)
  (:interface triangle)
  (:relation
   (:tuple (((d head) (d tail) (s tail))))
   (:tuple (((s tail) (d tail) (d tail))))
   (:tuple (((d tail) (s tail) (d head))))
   
   (:tuple (((d head) (s head) (s tail))))
   (:tuple (((s tail) (d head) (s head))))
   (:tuple (((s head) (s tail) (d head))))
   
   (:tuple (((d head) (t tail) (d tail))))
   (:tuple (((d tail) (d head) (t tail))))
   (:tuple (((t tail) (d tail) (d head))))
   
   (:tuple (((t tail) (s tail) (s head))))
   (:tuple (((s head) (t tail) (s tail))))
   (:tuple (((s tail) (s head) (t tail))))
   
   (:tuple (((d head) (t tail) (t head))))
   (:tuple (((t head) (d head) (t tail))))
   (:tuple (((t tail) (t head) (d head))))
   
   (:tuple (((s head) (d head) (t tail))))
   (:tuple (((t tail) (s head) (d head))))
   (:tuple (((d head) (t tail) (s head))))
   
   (:tuple (((t tail) (d head) (t head))))
   (:tuple (((t head) (t tail) (d head))))
   (:tuple (((d head) (t head) (t tail))))
   
   (:tuple (((t tail) (d head) (d tail))))
   (:tuple (((d tail) (t tail) (d head))))
   (:tuple (((d head) (d tail) (t tail))))
   
   (:tuple (((s head) (t tail) (t head))))
   (:tuple (((t head) (s head) (t tail))))
   (:tuple (((t tail) (t head) (s head))))))

(defconstraint complement
  (:type primitive)
  (:interface label-1 label-2)
  (:relation 
   (:tuple ((d tail) (d head)))
   (:tuple ((d head) (d tail)))
   
   (:tuple ((t tail) (t head)))
   (:tuple ((t head) (t tail)))
   
   (:tuple ((s tail) (s head)))
   (:tuple ((s head) (s tail)))))

(defun complement-p (label-1 label-2)
  (eval `(satisfied-p complement :with
                      label-1 = (:one-of ,label-1)
                      label-2 = (:one-of ,label-2))))


(defconstraint connected-left
  (:type primitive)
  (:interface triangle-1 triangle-2)
  (:relation
   (:pattern (triangle-1 triangle-2)
             :if (complement-p (first triangle-1) (first triangle-2))))
  (:condition (constrained-p triangle-1 triangle-2)))

(defconstraint connected-middle
  (:type primitive)
  (:interface triangle-1 triangle-2)
  (:relation
   (:pattern (triangle-1 triangle-2)
             :if (complement-p (second triangle-1) (second triangle-2))))
  (:condition (constrained-p triangle-1 triangle-2)))

(defconstraint connected-right
  (:type primitive)
  (:interface triangle-1 triangle-2)
  (:relation
   (:pattern (triangle-1 triangle-2)
             :if (complement-p (third triangle-1) (third triangle-2))))
  (:condition (constrained-p triangle-1 triangle-2)))

(defconstraint different-triangles
  (:type primitive)
  (:interface triangle-1 triangle-2)
  (:relation
   (:pattern (triangle-1 triangle-2)
             :if (different-triangles triangle-1 triangle-2)))
  (:condition (constrained-p triangle-1 triangle-2)))

(defun different-triangles (triangle-1 triangle-2)
  (not (or (equal triangle-1 triangle-2)
           (and (equal (first triangle-1) (second triangle-2))
                (equal (second triangle-1) (third triangle-2))
                (equal (third triangle-1) (first triangle-2)))
           (and (equal (first triangle-1) (third triangle-2))
                (equal (second triangle-1) (first triangle-2))
                (equal (third triangle-1) (second triangle-2))))))

(defconstraint nine-puzzle
  (:type compound)
  (:interface position-1 position-2 position-3 position-4
              position-5 position-6 position-7 position-8 position-9)
  (:constraint-expressions
   
   (triangle position-1) (triangle position-2) (triangle position-3)
   (triangle position-4) (triangle position-5) (triangle position-6)
   (triangle position-7) (triangle position-8) (triangle position-9)
   
   (connected-middle position-1 position-3)
   (connected-right position-2 position-3)
   (connected-left position-3 position-4)
   (connected-middle position-4 position-8)
   (connected-right position-5 position-6)
   (connected-left position-6 position-7)
   (connected-right position-7 position-8)
   (connected-left position-8 position-9)
   
   (different-triangles position-1 position-2)
   (different-triangles position-1 position-3)
   (different-triangles position-1 position-4)
   (different-triangles position-1 position-5)
   (different-triangles position-1 position-6)
   (different-triangles position-1 position-7)
   (different-triangles position-1 position-8)
   (different-triangles position-1 position-9)
   
   (different-triangles position-2 position-3)
   (different-triangles position-2 position-4)
   (different-triangles position-2 position-5)
   (different-triangles position-2 position-6)
   (different-triangles position-2 position-7)
   (different-triangles position-2 position-8)
   (different-triangles position-2 position-9)
   
   (different-triangles position-3 position-4)
   (different-triangles position-3 position-5)
   (different-triangles position-3 position-6)
   (different-triangles position-3 position-7)
   (different-triangles position-3 position-8)
   (different-triangles position-3 position-9)
   
   
   (different-triangles position-4 position-5)
   (different-triangles position-4 position-6)
   (different-triangles position-4 position-7)
   (different-triangles position-4 position-8)
   (different-triangles position-4 position-9)
   
   (different-triangles position-5 position-6)
   (different-triangles position-5 position-7)
   (different-triangles position-5 position-8)
   (different-triangles position-5 position-9)
   
   (different-triangles position-6 position-7)
   (different-triangles position-6 position-8)
   (different-triangles position-6 position-9)
   
   (different-triangles position-7 position-8)
   (different-triangles position-7 position-9)
   
   (different-triangles position-8 position-9)))

(instructions 
 (satisfy nine-puzzle :globally 1))

;;; (time (satisfy nine-puzzle :globally 1))

;;; eof

