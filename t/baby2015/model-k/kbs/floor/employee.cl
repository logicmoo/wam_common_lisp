;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;; ===========================================================================
;;; ===========================================================================
;;; DOMAIN LAYER : CONCEPTS
;;; ===========================================================================
;;; ===========================================================================
;;; here: employees


(DEF-CONCEPT employee
  (SLOTS 
         (employee-preferences   - :POSSIBLE-VALUES (:SOME-OF-INSTANCES employee))
         (frequented             - :POSSIBLE-VALUES :EXTENDED-BOOLEAN )
         (full-time              - :POSSIBLE-VALUES :EXTENDED-BOOLEAN)
         (group                  - :POSSIBLE-VALUES (:ONE-OF-ENUMERATION groups))
         (hobbies                - :POSSIBLE-VALUES (:SOME-OF-ENUMERATION hobbies))
         (projects               - :POSSIBLE-VALUES (:SOME-OF-ENUMERATION projects))
         (required-resources     - :POSSIBLE-VALUES (:SOME-OF-ENUMERATION resources))
         (roles                  - :POSSIBLE-VALUES (:SOME-OF-ENUMERATION roles))
         (room-preferences       - :POSSIBLE-VALUES (:SOME-OF-INSTANCES room))
         (smoker                 - :POSSIBLE-VALUES :EXTENDED-BOOLEAN)
         (subgroup               - :POSSIBLE-VALUES (:ONE-OF-ENUMERATION subgroups))
         (themes                 - :POSSIBLE-VALUES (:SOME-OF-ENUMERATION themes))
         (tolerating             - :POSSIBLE-VALUES (:SOME-OF-ENUMERATION nerve-racking-things))
         (year-of-enlistment     - :POSSIBLE-VALUES :NUMBER)
         (legal-rooms            (c5-121 c5-120 c5-116 c5-115 c5-114) ; used for testing only!
                                :POSSIBLE-VALUES :ANY)
         (ASSUMABLE (employee-preferences frequented full-time group hobbies projects
                                          required-resources room-preferences smoker
                                          subgroup themes tolerating year-of-enlistment)
                    :POSSIBLE-VALUES :any)))


;;; ===========================================================================
;;; CONCEPT-INSTANCES PART


; if "get-frame-name-with-check" is on pay attention to the order of the
; employee-instances (because some have employee-preferences)


(DEF-CONCEPT-INSTANCE Thomas-Christaller OF employee
  WITH frequented         = true
       full-time          = true
       group              = xps
       projects           = ((:EXACTLY-TRUE reflect bap eulisp))
       required-resources = ((:EXACTLY-TRUE macintosh))
       roles              = ((:EXACTLY-TRUE head-of-group head-of-project researcher))
       smoker             = false
       subgroup           = nil
       tolerating         = ((:EXACTLY-FALSE smoking-employees frequented-employees))
       themes             = (((:TRUE expert-system-tools knowledge-modeling programming)))
       year-of-enlistment = 1983)

(DEF-CONCEPT-INSTANCE Monika-Wendel OF employee
  WITH frequented         = true
       full-time          = true
       group              = xps
       roles              = ((:EXACTLY-TRUE secretary))
       smoker             = false
       tolerating         = ((:EXACTLY-FALSE smoking-employees frequented-employees))
;       themes             = (((:TRUE)))
       required-resources = ((:EXACTLY-TRUE macintosh))
       year-of-enlistment = 1978)


(DEF-CONCEPT-INSTANCE Alexander-Horz OF employee
  WITH frequented         = false
       full-time          = true
       group              = xps
       hobbies            = (((:TRUE cooking cycling photography wondering)))
       projects           = ((:EXACTLY-TRUE qwertz tasso))
       required-resources = ((:EXACTLY-TRUE sun macintosh))
       roles              = ((:EXACTLY-TRUE researcher))
       smoker             = false
       subgroup           = planning
       tolerating         = ((:EXACTLY-FALSE smoking-employees frequented-employees))
       themes             = (((:TRUE planning robotics)))
       year-of-enlistment = 1987)

(DEF-CONCEPT-INSTANCE Angi-Voss OF employee
  WITH frequented = false
       full-time = false
       group    = xps
       projects = ((:EXACTLY-TRUE reflect werex))
       required-resources = ((:EXACTLY-TRUE macintosh))
       roles     = ((:EXACTLY-TRUE head-of-subgroup head-of-project researcher))
       smoker = false
       tolerating = ((:EXACTLY-FALSE smoking-employees frequented-employees))
       subgroup = knowledge-acquisition
       themes = (((:TRUE knowledge-modeling)))
       year-of-enlistment = 1986)

(DEF-CONCEPT-INSTANCE Werner-Karbach OF employee
  WITH frequented = false
       full-time  = true
       group = xps
       projects = ((:EXACTLY-TRUE reflect werex))
       required-resources = ((:EXACTLY-TRUE sun macintosh))
       roles  = ((:EXACTLY-TRUE researcher))
       smoker = false
       subgroup = knowledge-acquisition
       tolerating = (((:TRUE frequented-employees) (:FALSE smoking-employees)))
       themes = (((:TRUE knowledge-modeling knowledge-acquisition)))
       year-of-enlistment = 1987)

(DEF-CONCEPT-INSTANCE Juergen-Walther OF employee
  WITH frequented = false
       full-time  = true
       group = xps
       projects = ((:EXACTLY-TRUE bap werex))
       required-resources = ((:EXACTLY-TRUE macintosh))
       roles  = ((:EXACTLY-TRUE researcher))
       smoker = true
       subgroup = expert-system-tools
       tolerating = ((:EXACTLY-FALSE frequented-employees))
       themes = (((:TRUE knowledge-representation programming)))
       year-of-enlistment = 1979)


(DEF-CONCEPT-INSTANCE Michael-Schmitz OF employee
  WITH frequented = true
       full-time  = true
       group = xps
       projects = ((:EXACTLY-TRUE bap))
       required-resources = ((:EXACTLY-TRUE macintosh))
       roles  = ((:EXACTLY-TRUE mta researcher))
       smoker = false
       subgroup = expert-system-tools
       tolerating = ((:EXACTLY-FALSE smoking-employees frequented-employees))
       themes = (((:TRUE expert-system-tools programming)))
       year-of-enlistment = 1986)

(DEF-CONCEPT-INSTANCE Hans-Voss OF employee
  WITH frequented = false
       full-time  = true
       group = xps
       projects = ((:EXACTLY-TRUE bap tex-b tex-i))
       required-resources = (((:TRUE macintosh sun)))
       roles  = ((:EXACTLY-TRUE head-of-subgroup head-of-project researcher))
       smoker = false
       subgroup = expert-system-tools
       tolerating = ((:EXACTLY-FALSE smoking-employees frequented-employees))
       themes = (((:TRUE qualitative-reasoning expert-system-tools)))
       year-of-enlistment = 1986)

  
  
;;; ===========================================================================
;;; RELATION PART
;;; contains the relations:
#|
(DEF-RELATION is-employee OF relation
(DEF-RELATION secretary_and_head-of-group-pair OF relation           
(DEF-RELATION head-of-project_and_head-of-group-pair OF relation           
(DEF-RELATION discussion_and_head-of-group-pair OF relation          
(DEF-RELATION frequented_and_full-time-pair OF relation           
(DEF-RELATION smoker_and_not-smoker-pair OF relation           
(DEF-RELATION are-in-same-projects OF relation           
(DEF-RELATION do-not-share-common-themes OF relation
(DEF-RELATION meeting-often OF relation
(DEF-RELATION close-friends OF relation
(DEF-RELATION share-common-themes OF relation
(DEF-RELATION do-not-share-common-themes OF relation
|#

(DEF-RELATION is-employee OF relation
  WITH domains    = ((employee)) 
       assumable  = false)   ; originally it was true

(DEF-CLAUSE-SET is-employee
  ((is-employee _emp)
   <-
   (employee _emp)))

(DEF-RELATION is-head-of-group OF relation
  WITH domains    = ((employee)) 
       assumable  = false)   ; originally it was true

(DEF-CLAUSE-SET is-head-of-group
  ((is-head-of-group _emp)
   <-
   (employee _emp)
   (LISP (member 'head-of-group (<- _emp :get-true 'roles)))))

(DEF-RELATION secretary_and_head-of-group-pair OF relation           ; computable (occupancy)
   WITH properties = ((symetric))
        domains    = ((employee employee))
        assumable  = false)

; the corresponding Prolog-part:

(DEF-CLAUSE-SET secretary-and-head-of-group-pair
  ((secretary_and_head-of-group-pair _em1 _em2) 
   <-
   (employee _em1)
   (LISP (member 'secretary (<- _em1 :get-true 'roles)))
   (employee _em2)
   (LISP (member 'head-of-group (<- _em2 :get-true 'roles)))))


(DEF-RELATION head-of-project_and_head-of-group-pair OF relation           ; computable (occupancy)
   WITH properties = ((symetric))
        domains    = ((employee employee))
        assumable  = false)

; the corresponding Prolog-part:

(DEF-CLAUSE-SET head-of-project_and_head-of-group-pair
  ((head-of-project_and_head-of-group-pair _em1 _em2) 
   <-
   (employee _em1)
   (LISP (member 'head-of-project (<- _em1 :get-true 'roles)))
   (employee _em2)
   (LISP (member 'head-of-group (<- _em2 :get-true 'roles)))))


(DEF-RELATION discussion_and_head-of-group-pair OF relation           ; computable (occupancy)
   WITH properties = ((symetric))
        domains    = ((employee employee))
        assumable  = false)

; the corresponding Prolog-part:

(DEF-CLAUSE-SET discussion_and_head-of-group-pair
  ((discussion_and_head-of-group-pair _em1 _em2) 
   <-
   (employee _em1)
   (LISP (member 'discussion (<- _em1 :get-true 'roles)))
   (employee _em2)
   (LISP (member 'head-of-group (<- _em2 :get-true 'roles)))))


(DEF-RELATION frequented_and_full-time-pair OF relation           ; computable (occupancy)
   WITH properties = ((symetric))
        domains    = ((employee employee))
        assumable  = false)

; the corresponding Prolog-part:

(DEF-CLAUSE-SET frequented_and_full-time-pair
  ((frequented_and_full-time-pair _em1 _em2) 
   <-
   (employee _em1)
   (LISP (equal (<- _em1 :get-true 'frequented) 'true))
   (employee _em2)
   (LISP (member 'frequented-employees (<- _em2 :get-false 'tolerating)))))


(DEF-RELATION smoker_and_not-smoker-pair OF relation           ; computable (employee)
   WITH properties = ((symetric))
        domains    = ((employee employee))
        assumable  = false)

; the corresponding Prolog-part:


(DEF-CLAUSE-SET smoker_and_not-smoker-pair
  ((smoker_and_not-smoker-pair _em1 _em2) 
   <-
   (employee _em1)
   (LISP (equal (<- _em1 :get-true 'smoker) 'true))
   (employee _em2)
   (LISP (member 'smoking-employees (<- _em2 :get-false 'tolerating)))))


(DEF-RELATION meeting-often OF relation          ; close-friends or are-in-same-projects
  WITH properties = ((symetric))
       domains    = ((employee employee)) 
       assumable  = true)

(DEF-CLAUSE-SET meeting-often
  ((meeting-often _em1 _em2 _truth) <- (close-friends _em1 _em2 _truth))  
  ((meeting-often _em1 _em2 _truth) <- (are-in-same-projects _em1 _em2 _truth)))
  
  
(DEF-RELATION are-in-same-projects OF relation           ; computable (employee)
   WITH properties = ((symetric reflexive))
        domains    = ((employee employee))
        assumable  = true)

; the corresponding Prolog-part:

(DEF-CLAUSE-SET are-in-same-projects
  ((are-in-same-projects _em1 _em2 _truth)
   <-
   (employee _em1)
   (is _project1 (<- _em1 :get-true 'projects))
   (/== _project1 nil)
   (employee _em2)
   (lisp (string-lessp '_em1 '_em2))
   (is _project2 (<- _em2 :get-true 'projects))
   (/== _project2 nil)
   (in-same-proj _project1 _project2 _truth))

  ((in-same-proj _project1 _project2 true)
   <-
   (lisp (intersection '_project1 '_project2)))

  ((in-same-proj _project1 _project2 false)
   <-
   (lisp (null (intersection '_project1 '_project2)))))
 

(DEF-RELATION close-friends OF relation
  WITH properties = ((symetric))
       domains    = ((employee employee))
       assumable  = true)

(DEF-CLAUSE-SET close-friends
  ((close-friends angi-voss hans-voss true)))
  

(DEF-RELATION share-common-themes OF relation
  WITH properties = ((symetric reflexive))
       domains    = ((employee employee))
       assumable  = true)

; the corresponding Prolog-part:

(DEF-CLAUSE-SET share-common-themes
  ((share-common-themes _emp1 _emp2 _truth)
   <-
   (employee _emp1)
   (is _themes1 (<- _emp1 :get-true 'themes))
   (/== _themes1 nil)
   (employee _emp2)
   (lisp (string-lessp '_emp1 '_emp2))  ; _emp1 is not equal _emp2
   (is _themes2 (<- _emp2 :get-true 'themes))
   (/== _themes2 nil)
   (test-for-fitting-themes _themes1 _themes2 _truth))
  
  ((test-for-fitting-themes _themes1 _themes2 false)
   <-
   (not (test-for-fitting-themes _themes1 _themes2 true)))
  
  ((test-for-fitting-themes _themes1 _themes2 true)
   <-
   (lisp (intersection '_themes1 '_themes2)))
  
  ((test-for-fitting-themes _themes1 _themes2 true)
   <-
   (member _t1 _themes1)
   (member _t2 _themes2)
   (sym related-themes _t1 _t2)))         ; the relation is symmetric



(DEF-RELATION do-not-share-common-themes OF relation
  WITH properties = ((symetric reflexive))
       domains    = ((employee employee))
       assumable  = false)

; the corresponding Prolog-part:

(DEF-CLAUSE-SET do-not-share-common-themes
  ((do-not-share-common-themes _emp1 _emp2)
   <-
   (share-common-themes _emp1 _emp2 false)))

;;; eof

