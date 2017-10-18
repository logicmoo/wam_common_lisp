;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;; ************* KNOWLEDGE BASE  DECLARATION ***********


(def-kb-instance adder adderc)


(defconstraint adder
  (:type primitive)
  (:interface a b c)
  (:relation (:pattern (a b (+ a b)) :if (constrained-p a b))
             (:pattern (a (- c a) c) :if (constrained-p a c))
             (:pattern ((- c b) b c) :if (constrained-p b c)))
  (:condition :or))


(defconstraint  multiplier
  (:type primitive)
  (:interface a b c)
  (:relation (:pattern (a b (* a b)) :if (constrained-p a b))
             (:pattern (a (/ c a) c) :if (and (constrained-p a c) (/= a 0)))
             (:pattern ((/ c b) b c) :if (and (constrained-p b c) (/= b 0)))
             (:pattern (a b 0) :if (or (and (constrained-p a) (= a 0))
                                       (and (constrained-p b) (= b 0)))))
  (:condition (or (constrained-p a b)
                  (constrained-p a c)
                  (constrained-p b c)
                  (and (constrained-p a) (= a 0))
                  (and (constrained-p b) (= b 0)))))


(defconstraint equal
  (:type primitive)
  (:interface x y)
  (:relation (:pattern (x x) :if (constrained-p x))
             (:pattern (y y) :if (constrained-p y)))
  (:condition (not (unconstrained-p x y))))


(defconstraint ampel
  (:type primitive)
  (:interface f1 f2)
  (:relation (:tuple (rot gruen))
             (:tuple (gruen rot))))


(defconstraint same-sum
  (:type compound)
  (:interface a1 a2 b1 b2)
  (:constraint-expressions
   (adder a1 a2 s)
   (adder b1 b2 s)))


(defconstraint same-sum2
  (:type compound)
  (:interface a1 a2 b1 b2)
  (:constraint-expressions
   (adder a1 a2 s1)
   (adder b1 b2 s2)
   (equal s1 s2)))


(instructions t)

;;; eof

