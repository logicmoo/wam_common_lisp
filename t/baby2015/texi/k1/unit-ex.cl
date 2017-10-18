;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")


;;; adopted to non SYMBOLICS machines for BABYLON 2.1 by Juergen Walther 10.5.89


(def-kb-instance unit-kb-examples k1c :pkg :ks)

;; the kb uses the physical-unit and functional-unit definition of unit-kb

(do ()
    ((member 'unit-kb *known-knowledge-bases*)
     (progn
       ($send unit-kb-examples :make-yourself-current)
       (send-kb :send-if-handles :import-kb unit-kb)
       "unit-kb imported"))
  (if (not (member 'unit-kb *known-knowledge-bases*))
    (cerror "Load << unit-kb >> before proceeding!"
            "Unknown Knowledge Base ~S" 'unit-kb)))

;; _________________________________________________________________________

;; defining the compound object AKW-ANLAGE
;; an AKW-ANLAGE consists of a PRIMAERKREIS-SYSTEM


(DEFFRAME AKW-ANLAGE
  (SUPERS PHYSICAL-UNIT)
  (SLOTS
   (PRIMAERKREIS-SYSTEM -
                        :IS-A PRIMAERKREIS-SYSTEM
                        :DOC "Ein Teilsystem der Reaktor-Anlage")
   (ANDERES-UNTERSYSTEM -
                        :DOC "Ein weiteres, hier nicht definiertes Untersystem")
   ;; hier koennen weitere slots definiert werden
   ))

;; _________________________________________________________________________

;; the compound object PRIMAERKREIS-SYSTEM
;; a PRIMAERKREIS-SYSTEM consists of a REAKTOR and a DND-MESSKREISLAUF


(DEFFRAME PRIMAERKREIS-SYSTEM
   (SUPERS PHYSICAL-UNIT)
   (SLOTS
     (REAKTOR - :IS-A REAKTOR)
     (DND-MESSKREISLAUF - :IS-A DND-MESSKREISLAUF)))

;; __________________________________________________________________________

;; some "basic" Physical Units

;;(DEFFRAME DRUCKMESSER
;;  (SUPERS PHYSICAL-UNIT)
;;  (SLOTS (REGION -)
;;	 (MESSGROESSEN -)))

(DEFFRAME DRUCKMESSER
  (SUPERS PHYSICAL-UNIT)
  (SLOTS (REGION -)
         (MESSGROESSEN -)
         (LEITUNG - :DOC "Dieser slot wird als Relation benutzt.")))

(defbehavior (DRUCKMESSER :after :initialize)	; for test purposes
  (ignore)
  (declare (ignore ignore))
  (<- self :set-trace t))

;;(DEFFRAME PUMPE (SUPERS PHYSICAL-UNIT))

(DEFFRAME PUMPE
  (SUPERS PHYSICAL-UNIT)
  (SLOTS (LEITUNG - :DOC "Dieser slot wird als Relation benutzt.")
         (STATE not-active :DOC "for test purposes")))


(defbehavior (PUMPE :after :initialize)		; for test purposes
  (ignore)
  (declare (ignore ignore))
  (<- self :set-trace t))

;;(DEFFRAME LECKDETEKTOR (SUPERS PHYSICAL-UNIT))

(DEFFRAME LECKDETEKTOR
  (SUPERS PHYSICAL-UNIT)
  (SLOTS (LEITUNG - :DOC "Dieser slot wird als Relation benutzt.")))


;; __________________________________________________________________________

;; the compound object REAKTOR
;; the REAKTOR consists of a DRUCKMESSER


(DEFFRAME REAKTOR
  (SUPERS PHYSICAL-UNIT)
  (SLOTS (DRUCKMESSER - :IS-A DRUCKMESSER)))

;; _________________________________________________________________________

;; the compound object DND-MESSKREISLAUF
;; has 4 DRUCKMESSER, 2 PUMPEN and a LECKDETEKTOR

(DEFFRAME DND-MESSKREISLAUF
  (SUPERS PHYSICAL-UNIT)
  (SLOTS
   (DRUCKMESSER - :IS-A-LIST-OF DRUCKMESSER :NAMES 4)	; has 4 DRUCKMESSER
   (PUMPEN - :IS-A-LIST-OF PUMPE :NAMES 2)	; has 2 PUMPEN
   (LECKDETEKTOR - :IS-A LECKDETEKTOR)))	; has a LECKDETEKTOR


;; __________________________________________________________________________

;; this is an instance of AKW-ANLAGE (with no other specifications)
;; in  general the (physical or functional) structure of a compound
;; object can be displayed with the messages
;;
;; try (<- KRAFTWERK1 :describe-physical-tree)
;; or  (<- KRAFTWERK1 :describe-functional-tree)
;;
;; sorry on Lispm only!!!!!!

(DEFINSTANCE KRAFTWERK1 OF AKW-ANLAGE)

;; __________________________________________________________________________

;; KRAFTWERK2 is a AKW-ANLAGE in which the REAKTOR of the 
;; PRIMAERKREIS-SYSTEM is called REAKTOR1 and its DRUCKMESSER DRUCKMESSER1 

(DEFINSTANCE KRAFTWERK2 OF AKW-ANLAGE
  WITH PRIMAERKREIS-SYSTEM = 
  (:NAME (:STANDARD
          WITH REAKTOR =
          (:NAME (REAKTOR1 
                  WITH DRUCKMESSER = (:NAME DRUCKMESSER1))))))

;; __________________________________________________________________________

;; KRAFTWERK3 is a AKW-ANLAGE with special names for the subparts
;; and a specified initialisation of DRUCKMESSER-P1

(DEFINSTANCE DRUCKMESSER-P1 OF DRUCKMESSER
  WITH MESSGROESSEN = ((YC11P1))
  REGION = :F1)


(DEFINSTANCE REAKTOR-R1 OF REAKTOR
  WITH  DRUCKMESSER = (:name (DRUCKMESSER-P1)))


(DEFINSTANCE KRAFWTERK3 OF AKW-ANLAGE
  WITH PRIMAERKREIS-SYSTEM = 
  (:NAME (:STANDARD
          WITH REAKTOR = (:name (REAKTOR-R1)))))

;; __________________________________________________________________________

;; these are acces-functions for the (physical subcomponents)

;; (<- KRAFTWERK1 :GET-PHYSICAL-SUBCOMPONENTS :TERMINAL)

;; (<- KRAFTWERK1 :GET-PHYSICAL-SUBCOMPONENTS :ALL)

;; __________________________________________________________________________

;; this illustrates the use of relations (as slots)






;; ___________________________________________________________________________



;; the MESSKREISLAUF consists of the DRUCKMESSER-P1, the DRUCKMESSER-P2
;; the PUMPE-D1 and the PUMPE-D2
;; the connections (relations) are as follows
;;
;; ------------------------------------------------------
;; |                                                    |
;; |                     <leitung>                      | this is the structure
;; |                                                    | of a MESSKREISLAUF
;; |   DRUCKMESSER-P1 <-------------> PUMPE-D1          |
;; |                                     ^              |
;; |                                     |              |
;; |                                     |  <leitung>   |
;; |                                     |              |
;; |                                     v              |
;; |   DRUCKMESSER-P2 <-------------> PUMPE-D2          |
;; |                                                    |
;; |                     <leitung>                      |
;; |                                                    |                                                   
;; |                                                    |
;; ------------------------------------------------------


(DEFFRAME MESSKREISLAUF
  (SUPERS PHYSICAL-UNIT)
  (SLOTS
   (DRUCKMESSER-P1 -
                   :IS-A DRUCKMESSER
                   :RELATION (LEITUNG PUMPE-D1)
                   :DOC "Der Slot LEITUNG des Druckmessers enthaelt den Wert von PUMPE-D1.")
   (DRUCKMESSER-P2 -
                   :IS-A DRUCKMESSER
                   :RELATION (LEITUNG PUMPE-D2))
   (PUMPE-D1 -
             :IS-A PUMPE
             :RELATION (LEITUNG DRUCKMESSER-P1 PUMPE-D2))     
   (PUMPE-D2 -
             :IS-A PUMPE
             :RELATION (LEITUNG DRUCKMESSER-P2 PUMPE-D1))
   (LECKDETEKTOR - :IS-A LECKDETEKTOR)))


(DEFFRAME MESSKREISLAUF-R
  (SUPERS PHYSICAL-UNIT)
  (SLOTS
   (DRUCKMESSER-P1 -
                   :IS-A DRUCKMESSER
                   :RELATION (LEITUNG PUMPE-D1)
                   :DOC "Der Slot LEITUNG des Druckmessers enthaelt den Wert von PUMPE-D1.")
   (DRUCKMESSER-P2 -
                   :IS-A DRUCKMESSER
                   :RELATION (LEITUNG PUMPE-D2))
   (PUMPE-D1 -
             :IS-A PUMPE
             :RELATION (LEITUNG PUMPE-D2))     
   (PUMPE-D2 -
             :IS-A PUMPE
             :RELATION (LEITUNG PUMPE-D3))
   (PUMPE-D3 -
             :IS-A PUMPE
             :RELATION (LEITUNG PUMPE-D4))
   (PUMPE-D4 -
             :IS-A PUMPE
             :RELATION (LEITUNG PUMPE-D2 PUMPE-D5))
   (PUMPE-D5 -
             :IS-A PUMPE
             :RELATION (LEITUNG PUMPE-D6))
   (PUMPE-D6 -
             :IS-A PUMPE
             :RELATION (LEITUNG DRUCKMESSER-P2 PUMPE-D1))
   (LECKDETEKTOR - :IS-A LECKDETEKTOR))) 

;; we define an instance


(definstance mk1 of MESSKREISLAUF)

(definstance mk3 of MESSKREISLAUF-R)

;; the user can access all components DRUCKMESSER-P2 of mk1 is
;; connected with (over LEITUNG) using the message :connected-components

;; try (<- mk1.DRUCKMESSER-P1 :connected-components 'LEITUNG)
;;>(MK1.PUMPE-D1 MK1.PUMPE-D2 MK1.DRUCKMESSER-P2)

;; to check if two components are related use :is-connected-with

;; try (<- mk1.DRUCKMESSER-P1 :is-connected-with mk1.DRUCKMESSER-P2 'LEITUNG)
;;>T
;; we have also access to the path from PUMPE-D2 to DRUCKMESSER-P1

;; try (<- mk1.DRUCKMESSER-P1 :get-path mk1.PUMPE-D2 'LEITUNG)
;;>(MK1.DRUCKMESSER-P1 MK1.PUMPE-D1 MK1.PUMPE-D2)


;; __________________________________________________________________________

;; if we define another instance mk2 of MESSKREISLAUF we are able
;; to connect the pumps of mk2 to the  DRUCKMESSERs of mk1 in the
;; following way

(definstance mk2 of MESSKREISLAUF)

;; try (<- mk2.pumpe-d1 :ADD-TO-RELATION 'leitung mk1.DRUCKMESSER-P1)
;;>(MK1.DRUCKMESSER-P1 MK2.PUMPE-D2 MK2.DRUCKMESSER-P1)
;; try (<- mk2.pumpe-d2 :ADD-TO-RELATION 'leitung mk1.DRUCKMESSER-P2)
;;>(MK1.DRUCKMESSER-P2 MK2.PUMPE-D1 MK2.DRUCKMESSER-P2)

;; and get the following


;; --------------------------------------------------------
;; |                                                       |
;; |                                                       |  this is the mk2
;; |                                                       |  
;; |      DRUCKMESSER-P1             DRUCKMESSER-P2        |
;; |             ^                          ^              |
;; |             |                          |              |
;; |             |                          |   <leitung>  |
;; |             |                          |              |
;; |             v                          v              |
;; |        PUMPE-D1  <---------------> PUMPE-D2           |
;; |             |                          |              |
;; |             |          <leitung>       |              |
;; |             |                          |              |
;; --------------------------------------------------------
;;               |                          |   <leitung>
;; --------------------------------------------------------
;; |             |                          |              |
;; |             |                          |              |  this is the mk1
;; |             v                          v              |  
;; |      DRUCKMESSER-P1             DRUCKMESSER-P2        |
;; |             ^                          ^              |
;; |             |                          |              |
;; |             |                          |   <leitung>  |
;; |             |                          |              |
;; |             v                          v              |
;; |        PUMPE-D1  <---------------> PUMPE-D2           |
;; |                                                       |
;; |                      <leitung>                        |
;; |                                                       |
;; --------------------------------------------------------


;; notice that :ADD-TO-RELATION is "not" symmetric

;; try (<- mk1.DRUCKMESSER-P1 :connected-components 'LEITUNG)
;;>(MK1.PUMPE-D1 MK1.PUMPE-D2 MK1.DRUCKMESSER-P2)
;; and (<- mk2.DRUCKMESSER-P1 :connected-components 'LEITUNG)
;;>(MK2.PUMPE-D1 MK1.DRUCKMESSER-P1 MK1.PUMPE-D1 MK1.PUMPE-D2 MK1.DRUCKMESSER-P2 
;;  MK2.PUMPE-D2 MK2.DRUCKMESSER-P2)
;; and (<- mk1.DRUCKMESSER-P1 :get-path mk2.pumpe-d2 'LEITUNG)
;;>NIL
;; and (<- mk2.DRUCKMESSER-P1 :get-path mk1.pumpe-d2 'LEITUNG)
;;>(MK2.DRUCKMESSER-P1 MK2.PUMPE-D1 MK1.DRUCKMESSER-P1 MK1.PUMPE-D1 MK1.PUMPE-D2)
   
;; since there exist more than one path from mk2.DRUCKMESSER-P1 to mk2.pumpe-d2
;; all the paths are accesable via the message :get-all-paths

;; try (<- mk2.DRUCKMESSER-P1 :get-all-paths mk2.pumpe-d2 'LEITUNG)
;;>((MK2.DRUCKMESSER-P1 MK2.PUMPE-D1 MK2.PUMPE-D2) (MK2.DRUCKMESSER-P1 MK2.PUMPE-D1 
;;   MK2.DRUCK MESSER-P1 MK2.PUMPE-D1 MK2.PUMPE-D2))

;; the user can also give an extra argument to the messages concerning  the
;; search for a relation. in every case it is interpreted (and must be)  as
;; the name of  a behavior  with one  argument. for  instance we can access
;; only the paths from mk2.DRUCKMESSER-P1 to mk1.pumpe-d2 for which all
;; pumps are active so we define

(defbehavior (physical-unit :is-active-pump) (this-arg-must-be-given)
  (if (null this-arg-must-be-given)
    t					  ; nil means there is no predecessor
    (if (<- self :type 'pumpe)	          ; if its a pump
      (if (eq ($value 'state) 'active)     ; t only if active
        t
        nil)
      t)))				  ; if not a pump t


;; and look for the apropriate paths with
;; (<- mk2.DRUCKMESSER-P1 :get-all-paths mk1.pumpe-d2 'LEITUNG :is-active-pump)
;; since all pumps are by default not active no path is generated.

;; if we "activate" all pumps
;;
;; (<- mk1.pumpe-d1 :set 'state 'active)
;; (<- mk1.pumpe-d2 :set 'state 'active)
;; (<- mk2.pumpe-d1 :set 'state 'active)
;; (<- mk2.pumpe-d2 :set 'state 'active)
;;
;; (<- mk2.DRUCKMESSER-P1 :get-all-paths mk1.pumpe-d2 'LEITUNG :is-active-pump)
;; yields the same result as
;; (<- mk2.DRUCKMESSER-P1 :get-all-paths mk1.pumpe-d2 'LEITUNG)

;; if we "deactivte" the pump mk1.pumpe-d2

;; (<- mk2.pumpe-d2 :set 'state 'inactive)

;; (<- mk2.DRUCKMESSER-P1 :get-all-paths mk1.pumpe-d2 'LEITUNG :is-active-pump)
;; yields only one path. if in addition we "inactivate" pump mk2.pumpe-d1

;; (<- mk2.pumpe-d1 :set 'state 'inactive)

;; then there is no path from mk2.DRUCKMESSER-P1 to mk1.pumpe-d2 only over
;; active pumps.


;; the use of the additional parameter (the condition) in other messages
;; (:is-connected-with :connected-components etc.) is similar.

;; eof

