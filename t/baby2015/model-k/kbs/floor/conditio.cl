;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;; ===========================================================================
;;; ===========================================================================
;;; DOMAIN LAYER : CONCEPTS
;;; ===========================================================================
;;; ===========================================================================
;;; here: conditions to satisfy between employees


(DEF-CONCEPT condition
  (SLOTS 
   (primitive-constraint - :POSSIBLE-VALUES :any)))



(DEF-CONCEPT-INSTANCE should-be-in-room OF condition
  WITH 
  primitive-constraint = in-room-constraint)

(DEF-CONCEPT-INSTANCE should-be-in-single-room OF condition
  WITH 
  primitive-constraint = in-single-room-constraint)

(DEF-CONCEPT-INSTANCE should-be-in-next-door-rooms OF condition
  WITH
  primitive-constraint = in-next-door-rooms-constraint)

(DEF-CONCEPT-INSTANCE should-be-in-near-rooms OF condition
  WITH
  primitive-constraint = in-near-rooms-constraint)

(DEF-CONCEPT-INSTANCE should-be-in-different-rooms OF condition
  WITH 
  primitive-constraint = in-different-rooms-constraint)

(DEF-CONCEPT-INSTANCE should-be-in-same-room OF condition
  WITH 
  primitive-constraint = in-same-room-constraint)




;;; ===================================================
;;; Definition of the primitive constraints
;;; (They are defined extensionally, because the system runs faster,
;;; but it is no problem to define them intensionally)


(defconstraint in-room-constraint
  (:type primitive)
  (:interface r1)
  (:relation (:TUPLE (c5-114))
             (:TUPLE (c5-115))
             (:TUPLE (c5-116))
             (:TUPLE (c5-120))
             (:TUPLE (c5-121))))

(defconstraint in-single-room-constraint
  (:type primitive)
  (:interface r1)
  (:relation (:TUPLE (c5-114))
             (:TUPLE (c5-115))
             (:TUPLE (c5-116)))
  )

(defconstraint in-next-door-rooms-constraint
  (:type primitive)
  (:interface r1 r2)
  (:relation (:TUPLE (c5-114 c5-115))
             (:TUPLE (c5-115 c5-114))
             (:TUPLE (c5-115 c5-116))
             (:TUPLE (c5-116 c5-115))
             (:TUPLE (c5-120 c5-121))
             (:TUPLE (c5-121 c5-120)))
  )

(defconstraint in-near-rooms-constraint
  (:type primitive)
  (:interface r1 r2)
  (:relation (:TUPLE (c5-114 c5-115))
             (:TUPLE (c5-114 c5-116))
             (:TUPLE (c5-115 c5-114))
             (:TUPLE (c5-115 c5-116))
             (:TUPLE (c5-116 c5-114))
             (:TUPLE (c5-116 c5-115))
             (:TUPLE (c5-116 c5-120))
             (:TUPLE (c5-120 c5-116))
             (:TUPLE (c5-120 c5-121))
             (:TUPLE (c5-121 c5-120)))
  )

(defconstraint in-different-rooms-constraint
  (:type primitive)
  (:interface r1 r2)
  (:relation (:tuple (C5-120 C5-121))
             (:tuple (C5-120 C5-116)) 
             (:tuple (C5-120 C5-115))
             (:tuple (C5-120 C5-114))
             (:tuple (C5-121 C5-120))
             (:tuple (C5-121 C5-116)) 
             (:tuple (C5-121 C5-115))
             (:tuple (C5-121 C5-114)) 
             (:tuple (C5-116 C5-120))
             (:tuple (C5-116 C5-121))
             (:tuple (C5-116 C5-115))
             (:tuple (C5-116 C5-114)) 
             (:tuple (C5-115 C5-120)) 
             (:tuple (C5-115 C5-121))
             (:tuple (C5-115 C5-116))
             (:tuple (C5-115 C5-114))
             (:tuple (C5-114 C5-120))
             (:tuple (C5-114 C5-121))
             (:tuple (C5-114 C5-116))
             (:tuple (C5-114 C5-115)))
)


(defconstraint in-same-room-constraint
  (:type primitive)
  (:interface r1 r2)
  (:relation (:tuple (C5-120 C5-120))
             (:tuple (C5-121 C5-121)) 
             (:tuple (C5-116 C5-116))
             (:tuple (C5-115 C5-115))
             (:tuple (C5-114 C5-114)))
)



;;; eof




