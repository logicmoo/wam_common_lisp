;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

(USE-KB-INSTANCE floor)


(DEF-ENUMERATION resources
  WITH values = ((sun symbolics macintosh siemens qume pc)))

(DEF-ENUMERATION roles
  WITH values = ((head-of-group head-of-subgroup head-of-project secretary
                                researcher mta host student discussion)))

(DEF-ENUMERATION projects
  WITH values = ((bap 
                  diamod 
                  reflect 
                  kriton 
                  tex-b 
                  tex-i 
                  werex 
                  eulisp 
                  qwertz 
                  tasso
                  mlp)))

(DEF-ENUMERATION groups
  WITH values = ((xps his sojus gus pus mmk)))

(DEF-ENUMERATION subgroups
  WITH values = ((cognitive-science
                  expert-system-tools
                  knowledge-acquisition
                  machine-learning
                  planning )))

(DEF-ENUMERATION themes
  WITH values = ((basic-research
                  cognitive-science
                  common-sense-reasoning
                  computers-and-society
                  constraints 
                  expert-system-tools 
                  interfaces
                  knowledge-acquisition 
                  knowledge-modeling
                  knowledge-representation
                  linguistics
                  machine-learning
                  natural-language-processing
                  planning
                  programming
                  qualitative-reasoning
                  robotics
                  software-architecture
                  theorem-proving)))

(DEF-ENUMERATION hobbies
  WITH values = ((ballgames
                  cars 
                  cooking
                  cycling
                  electronics
                  fishing
                  flowers
                  handicraft
                  home-mechanics
                  literature
                  music 
                  photography
                  sport
                  squash
                  tennis
                  wondering)))

(DEF-ENUMERATION room-type
  WITH values = ((single double three mass hall)))

(DEF-ENUMERATION nerve-racking-things
  WITH values = ((smoking-employees frequented-employees)))


;;; ===========================================================================
;;; RELATION PART
;;; contains the relations:
#|
(DEF-RELATION next-door-rooms OF relation            ; computable (room-number)
(DEF-RELATION near-rooms OF relation                 ; partially computable (room-number)
(DEF-RELATION different-rooms OF relation            ; computable (name)
(DEF-RELATION same-rooms OF relation                 ; computable (name)
(DEF-RELATION is-room OF relation                    ; computable (name)
(DEF-RELATION is-single-room OF relation             ; computable (name)
|#


(DEF-RELATION next-door-rooms OF relation            ; computable (room-number)
   WITH properties = ((symetric))
        domains    = ((room room))
        assumable  = false)

(DEF-CLAUSE-SET next-door-rooms
  ((next-door-rooms _room1 _room2) <-
   (room _room1)
   (is-room _room1)
   (is _number1 (<- _room1 :get-true 'room-number))
   (room _room2)
   (is-room _room2)
   (is _number2 (<- _room2 :get-true 'room-number))
   (lisp (equal (+ 1 '_number1) '_number2))))


(DEF-RELATION near-rooms OF relation                 ; partially computable (room-number)
   WITH properties = ((symetric))
        domains    = ((room room))
        assumable  = false)

(DEF-CLAUSE-SET near-rooms
  ((near-rooms _room1 _room2) <- (next-door-rooms _room1 _room2))
  ((near-rooms c5-114 c5-116))
  ((near-rooms c5-120 c5-116)))


(DEF-RELATION different-rooms OF relation             ; computable (name)
   WITH domains    = ((room room))
        assumable  = false)

(DEF-CLAUSE-SET different-rooms
  ((different-rooms _r1 _r2)
   <- 
   (room _r1)
   (room _r2)
   (/== _r1 _r2)))


(DEF-RELATION same-rooms OF relation             ; computable (name)
   WITH properties = ((reflexive))
        domains    = ((room room))
        assumable  = false)

(DEF-CLAUSE-SET same-rooms
  ((same-rooms _room1 _room2)
   <-
   (NOT (different-rooms _room1 _room2))))
  

(DEF-RELATION is-room OF relation             ; computable (name)
   WITH domains    = ((room))
        assumable  = false)

(DEF-CLAUSE-SET is-room
  ((is-room _room)
   <-
   (room _room)
   (IS _room-type (<- _room :get 'domains))
   (/== _room-type hall)))


(DEF-RELATION is-single-room OF relation             ; computable (name)
   WITH domains    = ((room))
        assumable  = false)

(DEF-CLAUSE-SET is-single-room
  ((is-single-room _room)
   <-
   (is-room _room)
   (lisp (equal (<- _room :get 'domains) 'single))))


;;; eof

