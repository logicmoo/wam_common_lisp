;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

(USE-KB-INSTANCE floor)
 
;;; ===========================================================================
;;; CONCEPT PART


(DEF-CONCEPT room
  (SLOTS 
         (available-resources - :POSSIBLE-VALUES (:SOME-OF-ENUMERATION resources))
         (belongs-to          - :POSSIBLE-VALUES (:ONE-OF-ENUMERATION groups))
         (has-window          - :POSSIBLE-VALUES :EXTENDED-BOOLEAN)
         (no-of-max-desks     - :POSSIBLE-VALUES :NUMBER)
;                1 desk entspricht ca. 5 qm
         (room-number         - :POSSIBLE-VALUES :NUMBER)
         (type                - :POSSIBLE-VALUES (:ONE-OF-ENUMERATION room-type))
         (ASSUMABLE     (belongs-to no-of-max-desks) :POSSIBLE-VALUES :ANY)))



;;; ===========================================================================
;;; CONCEPT-INSTANCES PART

       
(DEF-CONCEPT-INSTANCE C5-114 OF room
   WITH available-resources = ((:EXACTLY-TRUE macintosh macintosh))
        belongs-to = xps
        has-window = true
        room-number = 114
        no-of-max-desks = 2
        type = single)
       
(DEF-CONCEPT-INSTANCE C5-115 OF room
   WITH available-resources = ((:EXACTLY-TRUE macintosh macintosh))
        belongs-to = xps
        has-window = true
        room-number = 115
        no-of-max-desks = 2
        type = single)
       
(DEF-CONCEPT-INSTANCE C5-116 OF room
   WITH available-resources = ((:EXACTLY-TRUE macintosh))
        belongs-to = xps
        has-window = true
        room-number = 116
        no-of-max-desks = 2
        type = single)

(DEF-CONCEPT-INSTANCE C5-121 OF room
   WITH available-resources = ((:EXACTLY-TRUE macintosh macintosh sun))
        belongs-to = xps
        has-window = true
        room-number = 121
        no-of-max-desks = 4
        type = double)


(DEF-CONCEPT-INSTANCE C5-120 OF room
   WITH available-resources = ((:EXACTLY-TRUE macintosh macintosh qume))
        belongs-to = xps
        has-window = true
        room-number = 120
        no-of-max-desks = 6
        type = three)



(DEF-CONCEPT-INSTANCE hall OF room        ; Hall, für alle noch nicht untergebrachten Mitarbeiter
   WITH type = hall)


