;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

; ========================================================
; =========== REQUIREMENTS                    ============
; ========================================================

;---------------------------------------------------------
; component-specific-requirements

(DEF-CONCEPT component-specific-requirement
  (SLOTS (precondition - :POSSIBLE-VALUES :any)
         (condition     - :POSSIBLE-VALUES (:INSTANCE-OF condition))
         (weight        0 :POSSIBLE-VALUES :NUMBER)
         (lazy          - :POSSIBLE-VALUES :EXTENDED-BOOLEAN)))   ; first not of interest
   

;;; requirements using the definitions above defined with DEF-CONCEPT-INSTANCE

;;; *** component-specific-requirement everyone-should-be-in-room ***

(DEF-CONCEPT-INSTANCE everyone-in-room OF component-specific-requirement
  WITH precondition = ((is-employee _emp))
       weight = 100
       condition = should-be-in-room)


;;; *** component-specific-requirement head-of-group-in-single-room ***

(DEF-CONCEPT-INSTANCE head-of-group-in-single-room OF component-specific-requirement
  WITH precondition = ((is-head-of-group _emp))
       weight = 100
       condition = should-be-in-single-room)


;;; *** component-specific-requirement sectretaries-next-to-head-of-group ***

(DEF-CONCEPT-INSTANCE sectretaries-next-to-head-of-group OF component-specific-requirement
  WITH precondition = ((secretary_and_head-of-group-pair _em1 _em2))
       weight = 50
       condition = should-be-in-next-door-rooms)


;;; *** component-specific-requirement heads-of-projects-near-to-head-of-group ***

(DEF-CONCEPT-INSTANCE heads-of-projects-near-to-head-of-group OF component-specific-requirement
  WITH precondition = ((head-of-project_and_head-of-group-pair _em1 _em2))
       weight = 15
       condition = should-be-in-near-rooms)


;;; *** component-specific-requirement discussion-near-to-head-of-group ***

(DEF-CONCEPT-INSTANCE discussion-near-to-head-of-group OF component-specific-requirement
  WITH precondition = ((discussion_and_head-of-group-pair _em1 _em2))
       condition = should-be-in-near-rooms)


;;; *** component-specific-requirement frequencies-respected ***

(DEF-CONCEPT-INSTANCE frequencies-respected OF component-specific-requirement
  WITH precondition = ((frequented_and_full-time-pair _em1 _em2))
       weight = 2
       condition = should-be-in-different-rooms)


;;; *** component-specific-requirement smoker-and-not-smoker-aversion-respected ***

(DEF-CONCEPT-INSTANCE smoker-and-not-smoker-aversion-respected OF component-specific-requirement
  WITH precondition = ((smoker_and_not-smoker-pair _em1 _em2))
       weight = 10
       condition = should-be-in-different-rooms)


;;; *** component-specific-requirement in-same-project-and-different-rooms ***

(DEF-CONCEPT-INSTANCE same-project-different-room OF component-specific-requirement
  WITH precondition = ((are-in-same-projects _em1 _em2))
       weight = 1
       condition = should-be-in-different-rooms)


;;; *** no-common-themes-but-in-different-rooms ***

(DEF-CONCEPT-INSTANCE no-common-themes-different-room OF component-specific-requirement
  WITH precondition = ((do-not-share-common-themes _em1 _em2))
  ; the relation share-common-themes is assumable !
  ; there must be a new slot "truth-value" which stands for it
       weight = 1
       condition = should-be-in-different-rooms)

;---------------------------------------------------------
; location-specific-requirements

(DEF-CONCEPT location-specific-requirement
  (SLOTS (test-function - :POSSIBLE-VALUES :any)))
   

;; These requirements are defined for a room and the employees in a room

;; A room is overfull, if there are more people in than in the room-type slot
;; specified
(DEF-CONCEPT-INSTANCE room-overfull OF location-specific-requirement
  WITH
  test-function = (((lambda (room emps-in-room)
                    (let ((room-type (<- room :get-true 'type))
                          (nr-persons-in (length emps-in-room)))
                      (if emps-in-room
                        (case room-type
                          (single (> nr-persons-in 1))
                          (double (> nr-persons-in 2))
                          (three  (> nr-persons-in 3)))
                        T))))))

#|
; resources-not-available-p returns T iff there are not enough 
; resources in the room for all employees assigned to this room

(DEF-CONCEPT-INSTANCE resources-not-available-in-room OF location-specific-requirement
  WITH
  test-function = (((lambda (room emps-in-room)
                    (not (let ((still-available (<- room :get-true 'available-resources))
                          (resources-ok T))
                      (when emps-in-room
                        (dolist (one-employee emps-in-room resources-ok)
                          (setf empl-resources (<- one-employee :get-true 'required-resources))
                          (dolist (one-resource empl-resources resources-ok)
                            (if (member one-resource still-available)
                              (setf still-available (remove one-resource still-available :count 1))
                              (progn 
                                (setf resources-ok NIL)
                                (return resources-ok))))))))))))
|#

;;; eof



