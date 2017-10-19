;*******************************************************************************
;
; Daydreamer
; Version 3.5
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; This file contains:
; Daydreamer commonsense knowledge base
;
; 1/24/86: Added major-type definitions
; 1/26/86: Added some failure reversal rules
; 7/22/86: Added new rules
; 9/24/86: Took out flavors
; 11/7/86: Added minimization
; 9/11/88: Added patrol dream
;
;*******************************************************************************

;
; Type definitions
;

; Objects
(ty$create 'OBJECT nil nil)
(ty$create 'FREE-OBJ '(OBJECT) nil)

; Physical objects
(ty$create 'PHYS-OBJ '(OBJECT) '(nil (name mcontents) (mcontents)))
(ty$major-type 'PHYS-OBJ)
(ty$create 'FREE-PHYS-OBJ '(FREE-OBJ PHYS-OBJ) nil)
(ty$create 'FIXED-PHYS-OBJ '(PHYS-OBJ) nil)

; Fixed objects
(ty$create 'BUILDING '(FIXED-PHYS-OBJ) '(prop (name) ()))
(ty$create 'BAR '(BUILDING) '(prop (name) ()))
(ty$create 'THEATER '(BUILDING) '(prop (name) ()))
(ty$create 'MUSEUM '(BUILDING) '(prop (name) ()))

(ty$create 'COMPUTER '(FIXED-PHYS-OBJ) '(prop (name) ()))
(ty$create 'CASH '(FIXED-PHYS-OBJ) '(prop (name) ()))
(ty$create 'DRAWING '(FIXED-PHYS-OBJ) '(prop (name) ()))
(ty$create 'LAMPSHADE '(FIXED-PHYS-OBJ) '(prop (name) ()))

; States
(ty$create 'STATE nil nil)

; Free objects
(ty$create 'MAIL '(FREE-PHYS-OBJ) '(prop (contents) ()))
(ty$create 'MAGAZINE '(FREE-PHYS-OBJ) '(prop (name) ()))
(ty$create 'NO-VARIZE-OBJ '(FREE-PHYS-OBJ) nil)
(ty$create 'ALUMNI-DIR '(NO-VARIZE-OBJ) '(prop (obj)))
(ty$create 'EDIBLE '(FREE-PHYS-OBJ) '(prop (name) ()))
(ty$create 'CHINESE-FOOD '(EDIBLE) '(prop (name) ()))
(ty$create 'RESUME '(FREE-PHYS-OBJ) '(prop (obj) ()))
(ty$create 'SUNGLASSES '(FREE-PHYS-OBJ) '(prop (name) ()))
(ty$create 'NEWSPAPER '(FREE-PHYS-OBJ) '(prop (name) ()))
(ty$create 'CLOTHES '(FREE-PHYS-OBJ) '(prop (obj) ()))
(ty$create 'FASHIONABLE-CLOTHES '(CLOTHES) '(prop (obj) ()))
(ty$create 'RAINCOAT '(CLOTHES) '(prop (obj) ()))
(ty$create 'NECKLACE '(FREE-PHYS-OBJ) '(prop () ()))

; Conceptual objects
(ty$create 'ACCESS '(STATE) '(prop (actor obj) ()))
(ty$create 'INFO-OBJ '(OBJECT) '(prop (name) ()))
(ty$create 'MOVIES '(OBJECT) '(prop (obj) ()))
(ty$major-type 'INFO-OBJ)
(ty$create 'INSURANCE '(OBJECT) '(prop (name) ()))

; Comparisons
(ty$create 'GREATER-THAN '(STATE) '(prop (obj) ()))

; Mental states
(ty$create 'MENTAL-STATE '(STATE) nil)
(ty$create 'KNOW '(MENTAL-STATE) '(prop (actor obj) ()))
(ty$create 'BELIEVE '(MENTAL-STATE) '(prop (actor obj strength) (strength)))

; Attitudes
(ty$create 'ATTITUDE '(MENTAL-STATE) nil)
(ty$create 'POS-ATTITUDE '(ATTITUDE) '(prop (obj) ()))
(ty$create 'NEG-ATTITUDE '(ATTITUDE) '(prop (obj) ()))
(ty$create 'ROMANTIC-INTEREST '(POS-ATTITUDE) '(prop (obj) ()))

; Mental objects
(ty$create 'SMALLTALK '(MENTAL-STATE) nil)
(ty$create 'MENTAL-OBJ '(MENTAL-STATE) nil)
(ty$create 'MOVIE '(MENTAL-OBJ) '(prop (name) ()))
(ty$create 'INTRODUCTION '(MENTAL-OBJ) nil)
(ty$create 'KNOWABLE '(MENTAL-OBJ) nil)

; Personal attributes
(ty$create 'PERSONAL-ATTRIBUTE '(KNOWABLE) nil)
(ty$create 'RICH '(PERSONAL-ATTRIBUTE) '(prop (actor) ()))
(ty$create 'FAMOUS '(PERSONAL-ATTRIBUTE) '(prop (actor) ()))
; You can't tell someone you are cute...
(ty$create 'ATTRACTIVE '(STATE) '(prop (actor) ()))
(ty$create 'MOVIE-STAR '(PERSONAL-ATTRIBUTE) '(prop (actor) ()))
(ty$create 'STAR '(PERSONAL-ATTRIBUTE) '(prop (actor level) ()))
(ty$create 'EXECUTIVE '(PERSONAL-ATTRIBUTE) '(prop (actor) ()))
(ty$create 'WELL-DRESSED '(PERSONAL-ATTRIBUTE) '(prop (actor) ()))
(ty$create 'DATING-SERVICE-MEMBER '(PERSONAL-ATTRIBUTE)
  '(prop (actor level) ()))
(ty$create 'WEARING '(PERSONAL-ATTRIBUTE) '(prop (actor obj) ()))
(ty$create 'COLLEGE '(PERSONAL-ATTRIBUTE) '(prop (actor obj) ()))

; Locations
; Note: locations must always have a name to differentiate them from
; other locations.
(ty$create 'LOCATION '(KNOWABLE) '(prop (name) ()))
; There is only one AT assertion per object--locations are unique.
(ty$create 'AT '(STATE) '(prop (actor obj) ()))
(ty$create 'LATER '(STATE) '(prop (obj) ()))
(ty$create 'CITY '(STATE) '(prop (name) ()))

; Other states
(ty$create 'POSS '(STATE) '(prop (actor obj) ()))
(ty$create 'TELNO '(KNOWABLE) '(prop (actor obj) ()))
(ty$create 'ADDRESS '(KNOWABLE) '(prop (actor obj) ()))
(ty$create 'TIME-OF-DAY '(KNOWABLE) nil)
(ty$create 'VPROX '(STATE) '(prop (actor) ()))
(ty$create 'FRIDAY-NIGHT '(STATE) nil)
(ty$create 'NIGHT '(STATE) nil)
(ty$create 'WAIT '(STATE) nil)
(ty$create 'PRESERVATION '(STATE) '(prop (obj threat) ()))
(ty$create 'RAINING '(STATE) '(prop (obj) ()))
(ty$create 'EARTHQUAKE-ONSET '(STATE) '(prop (obj) ()))
(ty$create 'EARTHQUAKE '(STATE) '(prop (obj) ()))
(ty$create 'UNDER-DOORWAY '(STATE) '(prop (actor) ()))
(ob$add ^EARTHQUAKE 'predictor ^EARTHQUAKE-ONSET)

; Goal failure states
(ty$create 'WET '(STATE) '(prop (actor) ()))
(ty$create 'HURT '(STATE) '(prop (actor) ()))

; Actions
(ty$create 'ACTION nil nil)
(ty$create 'PTRANS '(ACTION) '(prop (actor from to obj) ()))
(ty$create 'PTRANS1 '(ACTION) '(prop (actor from to obj) ()))
(ty$create 'MTRANS '(ACTION) '(prop (actor from to obj) ()))
(ty$create 'ATRANS '(ACTION) '(prop (actor from to obj) ()))
(ty$create 'GRAB '(ACTION) '(prop (actor obj) ()))
(ty$create 'M-PHONE '(ACTION) '(prop (actor to) ()))
(ty$create 'M-SMILE '(ACTION) '(prop (actor to) ()))
(ty$create 'M-PUTON '(ACTION) '(prop (actor obj) ()))
(ty$create 'M-LOGIN '(ACTION) '(prop (actor obj) ()))
(ty$create 'M-EAT '(ACTION) '(prop (actor obj) ()))
(ty$create 'M-WORK '(ACTION) '(prop (actor obj) ()))
(ty$create 'M-BREAK-UP '(ACTION) '(prop (actor obj) ()))
(ty$create 'M-KISS '(ACTION) '(prop (actor) ()))
(ty$create 'M-SEX '(ACTION) '(prop (actor) ()))
(ty$create 'M-STUDY '(ACTION) '(prop (actor obj)))
(ty$create 'M-BEAT-UP '(ACTION) '(prop (actor obj)))

(ty$create 'ACTIVITY '(STATE) '(prop (actor) ()))
(ty$create 'M-DATE '(ACTIVITY) nil)
(ty$create 'M-MOVIE '(ACTIVITY) nil)
(ty$create 'M-RESTAURANT '(ACTIVITY) nil)
(ty$create 'M-PARTY '(ACTIVITY) nil)
(ty$create 'M-PURCHASE '(ACTIVITY) '(prop (actor obj from) ()))
(ty$create 'M-PAINT '(ACTIVITY) '(prop (actor obj) ()))
(ty$create 'M-AGREE '(ACTIVITY) '(prop (actor obj) ()))
(ty$create 'M-CONVERSATION '(ACTIVITY) '(prop (actor) ()))

(ty$create 'MTRANS-ACCEPTABLE '(STATE) '(prop (actor) ()))
(ty$create 'ENABLE-FUTURE-VPROX '(STATE) '(prop (actor) ()))

; Goal objectives
(ty$create 'DD-GOAL-OBJ '(STATE) nil)
(ty$create 'PERSONAL-GOAL-OBJ '(STATE) nil)
(ty$create 'REALISTIC-GOAL-OBJ '(DD-GOAL-OBJ) nil)
(ty$create 'FANCIFUL-GOAL-OBJ '(DD-GOAL-OBJ) nil)
(ty$create 'SKIPINDEX '(DD-GOAL-OBJ) nil)
(ty$create 'RATIONALIZATION '(FANCIFUL-GOAL-OBJ) '(prop (obj) ()))
(ty$create 'REVENGE '(FANCIFUL-GOAL-OBJ) '(prop (actor to obj) ()))
(ty$create 'ROVING '(FANCIFUL-GOAL-OBJ) '(prop (obj) ()))
(ty$create 'RECOVERY '(REALISTIC-GOAL-OBJ) '(prop (obj) ()))
(ty$create 'REHEARSAL '(REALISTIC-GOAL-OBJ SKIPINDEX) '(prop (obj) ()))
(ty$create 'REVERSAL '(REALISTIC-GOAL-OBJ SKIPINDEX) '(prop (obj) ()))
(ty$create 'REPERCUSSIONS '(REALISTIC-GOAL-OBJ SKIPINDEX) '(prop (obj) ()))

; Needs
(ty$create 'NEED '(PERSONAL-GOAL-OBJ) '(prop (strength) ()))

(ty$create 'FOOD '(NEED) nil)
(ty$create 'ENTERTAINMENT '(NEED) nil)

(ty$create 'EMPLOYMENT-SUBSUMPTION '(NEED) nil)
(ty$create 'MONEY '(EMPLOYMENT-SUBSUMPTION) nil)
(ty$create 'POSSESSIONS '(EMPLOYMENT-SUBSUMPTION) nil)

(ty$create 'LOVERS-SUBSUMPTION '(NEED) nil)
(ty$create 'SEX '(LOVERS-SUBSUMPTION) nil)
(ty$create 'LOVE-GIVING '(LOVERS-SUBSUMPTION) nil)
(ty$create 'LOVE-RECEIVING '(LOVERS-SUBSUMPTION) nil)
(ty$create 'FRIENDS-SUBSUMPTION '(LOVERS-SUBSUMPTION) nil)
(ty$create 'COMPANIONSHIP '(FRIENDS-SUBSUMPTION) nil)

; Long-term states
(ty$create 'LONG-TERM-STATE '(STATE) nil)
(ty$create 'RPROX '(LONG-TERM-STATE) '(prop (actor location) ()))
(ty$create 'INSURED '(LONG-TERM-STATE PERSONAL-GOAL-OBJ) '(prop (actor) ()))

; Goals
(ty$create 'GOAL '(MENTAL-STATE) '(prop (obj top-level-goal seq-next strength)
                                       (strength)))
(ty$create 'ACTIVE-GOAL '(GOAL) nil)
(ty$create 'TERMINATED-GOAL '(GOAL) nil)
(ty$create 'FAILED-GOAL '(TERMINATED-GOAL) nil)
(ty$create 'SUCCEEDED-GOAL '(TERMINATED-GOAL) nil)
(ty$create 'P-GOAL '(GOAL) nil)
(ty$create 'ACTIVE-P-GOAL '(P-GOAL ACTIVE-GOAL) nil)

; Emotions
(ty$create 'EMOTION nil nil)
(ty$create 'OVERALL-EMOTION '(EMOTION) '(prop (strength) ())) ; for gen only
(ty$create 'POS-EMOTION '(EMOTION) '(prop (strength to) ()))
(ty$create 'SURPRISE '(EMOTION) '(prop (strength to) ()))
(ty$create 'NEG-EMOTION '(EMOTION) '(prop (strength to) ()))
(ty$create 'POS-SURPRISE '(SURPRISE POS-EMOTION) '(prop (strength to) ()))
(ty$create 'NEG-SURPRISE '(SURPRISE NEG-EMOTION) '(prop (strength to) ()))
(ty$create 'INWARD-NEG-EMOTION '(NEG-EMOTION) '(prop (strength to) ()))
(ty$create 'NET-EMOTION nil nil)
; (ty$create 'ANGER '(NEG-EMOTION) '(prop (to strength) ()))

; Links
(ty$create 'LINK nil '(nil (linked-from linked-to) ()))
(ty$create 'CAUSAL-LINK '(LINK) nil)
(ty$create 'INTENDS '(CAUSAL-LINK) '(nil (linked-from linked-to rule seq?)
                                        ()))
(ty$create 'DEPENDENCY '(CAUSAL-LINK) '(nil (linked-from linked-to weight
                                                        offset decay rule)
                                           ()))
(ty$create 'ACTION-LINK '(CAUSAL-LINK) nil)

; Misc types
(ty$create 'LEADTO '(STATE) '(nil (ante conseq) ()))
(ty$create 'MINIMIZATION '(STATE) '(nil (obj) ()))
(ty$create 'SELLS '(STATE) '(nil (actor obj) ()))

; Relationships
(ty$create 'RELATIONSHIP '(STATE) nil)
(ty$create 'POS-RELATIONSHIP '(RELATIONSHIP) nil)
(ty$create 'NEG-RELATIONSHIP '(RELATIONSHIP) nil)
(ty$create 'ACQUAINTED '(RELATIONSHIP) '(prop (actor) ()))
(ty$create 'LOVERS '(POS-RELATIONSHIP PERSONAL-GOAL-OBJ) '(prop (actor) ()))
(ty$create 'FRIENDS '(POS-RELATIONSHIP PERSONAL-GOAL-OBJ) '(prop (actor) ()))
(ty$create 'ENEMIES '(NEG-RELATIONSHIP) '(prop (actor) ()))
(ty$create 'EMPLOYMENT '(POS-RELATIONSHIP PERSONAL-GOAL-OBJ)
                 '(nil (actor organization strength) (strength)))
(ty$create 'ACTING-EMPLOY '(EMPLOYMENT) nil)

; People
(ty$create 'PERSON '(OBJECT FREE-OBJ) nil)
(ty$major-type 'PERSON)
(ty$create 'MALE-PERSON '(PERSON) '(prop (first-name last-name) ()))
(ty$create 'FEMALE-PERSON '(PERSON) '(prop (first-name last-name) ()))
(ty$create 'ACTOR '(PERSON) '(prop (first-name last-name) ()))
(ty$create 'ART-CRITIC '(PERSON) '(prop (first-name last-name) ()))
(ty$create 'MALE-ACTOR '(ACTOR MALE-PERSON) nil)
(ty$create 'FEMALE-ACTOR '(ACTOR FEMALE-PERSON) nil)

; Organizations
(ty$create 'ORGANIZATION '(FIXED-PHYS-OBJ) '(prop (name) ()))
(ty$major-type 'ORGANIZATION)
(ty$create 'UNIVERSITY '(ORGANIZATION) '(prop (name) ()))
(ty$create 'DATING-SERVICE '(ORGANIZATION) nil)
(ty$create 'STORE '(ORGANIZATION) '(prop (name) ()))
(ty$create 'RESTAURANT '(ORGANIZATION) '(prop (name) ()))
(ty$create 'INSURANCE-COMPANY '(ORGANIZATION) '(prop (name) ()))

; Misc
(ty$create 'LIST nil '(prop (first rest)))
(ty$create 'HYPOTHESIZE nil '(prop (state)))

; Type definitions for wppfiles
(ty$create 'TYPE nil '(nil (slot-name1 slot-name2 parent) ()))
(ty$create 'THE-TYPE nil '(prop (slot-name1 slot-name2 type parent) ()))
(ty$create 'PROX nil '(prop (actor loc) ()))
(ty$create 'LIVES-IN nil '(prop (actor loc) ()))
(ty$create 'DRIVE-VEHICLE '(ACTION) '(prop (actor from to vehicle) ()))
(ty$create 'VEHICLE '(FREE-PHYS-OBJ) '(prop (obj) ()))
(ty$create 'LOC '(LOCATION) '(prop (obj) ()))
(ty$create 'SUBGOAL '(GOAL) '(prop (obj) ()))
(ty$create 'WANT '(GOAL) '(prop (actor obj) ()))
(ty$create 'RESIDENCE '(KNOWABLE) '(prop (obj) ()))
(ob$create '(PERSON obname Person1))
(ob$create '(PERSON obname Person2))
(ob$create '(PERSON obname Harrison))
(ob$create '(PERSON obname Debra))
(ob$create '(PERSON obname John1))
(ob$create '(PERSON obname John))
(ob$create '(PERSON obname Mary1))
(ob$create '(STORE obname Store1))
(ob$create '(LOCATION obname Location1))
(ob$create '(STATE obname Fact))

;
; Definitions of Me, the daydreamer himself/herself
;

(setq *gender* 'female)

(setq *me-ob*
      (cond
       ((eq? *gender* 'female)
        (ob$create '(FEMALE-PERSON first-name "Sarah" obname Me)))
       ((eq? *gender* 'male)
        (ob$create '(MALE-PERSON first-name "Tom" obname Me)))))

(defun me? (x) (eq? x *me-ob*))

(defun not-me? (x) (neq? x *me-ob*))

(setq *me-belief-path* (list *me-ob*))

(setq *empty-me-bd* (planner-empty-bd *me-belief-path*))

;
; Global ob definitions
;

(setq *succeeded-goal-ob* ^succeeded-goal)
(setq *active-goal-ob* ^active-goal)
(setq *active-p-goal-ob* ^active-p-goal)
(setq *failed-goal-ob* ^failed-goal)

(setq *pos-emotion-ob* ^pos-emotion)
(setq *neg-emotion-ob* ^neg-emotion)

(setq *believe-ob* ^believe)

(setq *person-ob* ^person)
(setq *male-person-ob* ^male-person)
(setq *female-person-ob* ^female-person)

(setq *intends-ob* ^intends)
(setq *dependency-ob* ^dependency)

(setq *mtrans-ob* ^mtrans)
(setq *ptrans-ob* ^ptrans)
(setq *atrans-ob* ^atrans)

(setq *not-ob* ^not)

;
; Initial facts for need states (with strength = 1.0 by default)
;

(setq *needs*
  (list
   (ob$fcreate '(ENTERTAINMENT))
   (ob$fcreate '(LOVE-RECEIVING))
   (ob$fcreate '(FOOD obname Food-Need))
   (ob$fcreate '(MONEY obname Money-Need))
   (ob$fcreate '(POSSESSIONS))
   (ob$fcreate '(SEX))
   (ob$fcreate '(LOVE-GIVING))
   (ob$fcreate '(COMPANIONSHIP))))

(defun need-init (context)
  (yloop (yfor need in *needs*)
         (ydo (set-strength need 1.0)))
  (set-strength (car *needs*) 0.1)
  (set-strength (cadr *needs*) 0.1)
  (if (memq? 'employment1 *gate-load-options*)
      (progn
       (set-strength ^money-need 0.1)
       (set-strength ^food-need 0.4)))
  (if (memq? 'food-need *gate-load-options*)
      (set-strength ^food-need 0.1))
  (no-gen
   (yloop (yfor need in *needs*)
          (ydo (cx$assert context need)))))

;
; Non-compilable stuff from dd_mutation
;

(setq *location-var* '?:Location)
(setq *person-var* '?:Person)
(setq *phys-obj-var* '?:Phys-Obj)
(setq *mental-obj-var* '?:Mental-Obj)
(setq *emot-var* '?:Emotion)

; Retrieve returns list?
(defun object->location (obj context)
  (let ((bd (cx$retrieve context (ob$fcreate `(AT ,obj ?Location)))))
       (bd-lookup 'Location (car bd))))

; Not unique. Just returns an arbitrary object at that location.
(defun location->object (location context)
  (let ((bds (cx$retrieve context (ob$fcreate `(AT ?Object ,location)))))
       (bd-lookup 'Object (car bds))))

;
; Non-compilable stuff from dd_gen
;

(setq *pos-rel-mtrans*
      (ob$create '(MTRANS ?Person1 ?Person1 ?Person2 
                          (BELIEVE ?Person1
                                   (ACTIVE-GOAL
                                    (UAND ?Pos-Relationship
                                          (NOTYPE actor ?Person1
                                                  actor ?Person2)))))))

(setq *neg-rel-mtrans*
      (ob$create '(MTRANS ?Person1 ?Person1 ?Person2 
                          (NOT (BELIEVE ?Person1
                                   (ACTIVE-GOAL
                                      (UOR
                                         (M-RESTAURANT ?Person1 ?Person2)
                                         (UAND ?Pos-Relationship
                                               (NOTYPE actor ?Person1
                                                       actor ?Person2)))))))))

;
; Phrase definitions
;

; Todo: Make it select randomly from a collection of male movie stars.
(cond
 ((eq? *gender* 'female)
  (define-initial-fact nil (ROMANTIC-INTEREST (MALE-ACTOR "Harrison" "Ford"
                                                       obname Movie-Star1))))
  ; Note: 'male mode is no longer supported.
 ((eq? *gender* 'male)
  (define-initial-fact nil (ROMANTIC-INTEREST (FEMALE-ACTOR "Nastassja"
                                                             "Kinski"
                                                             obname
                                                             Movie-Star1)))))

(ob$create '(MALE-ACTOR "Robert" "Redford" obname Movie-Star2))

(define-initial-fact (all exp2)
  (ROMANTIC-INTEREST Movie-Star2))

(define-initial-fact (all exp2)
  (POS-ATTITUDE Movie-Star2))

(define-initial-fact (all exp2)
  (RICH Movie-Star2))

(define-phrase "robert redford is at ucla shooting a film."
  (AT Movie-Star2 (LOCATION name "UCLA" obname UCLA-Location)))

(define-initial-fact (all exp2)
  (KNOW Me UCLA-Location))

(define-initial-fact (all always)
  (AT (THEATER "the Nuart" obname Nuart-Theater)
      (LOCATION "the Nuart" obname Nuart-Location)))

(define-phrase "nastassja kinski is at the nuart."
  (AT Movie-Star1 Nuart-Location))

(define-phrase "harrison ford is at the nuart."
  (AT Movie-Star1 Nuart-Location))

;(define-phrase "he chats with me."
;  (MTRANS Movie-Star1 Movie-Star1 Me (SMALLTALK)))

(define-phrase "he introduces himself to me."
  (MTRANS Movie-Star1 Movie-Star1 Me (INTRODUCTION)))

(define-initial-fact (unused) (AT Movie-Star1 Nuart-Location))

(define-phrase "she turns me down."
  (MTRANS Movie-Star1 Movie-Star1 Me
          (NOT (BELIEVE Movie-Star1 (ACTIVE-GOAL (LOVERS Movie-Star1 Me))))))

(define-phrase "she accepts."
  (MTRANS Movie-Star1 Movie-Star1 Me
          (BELIEVE Movie-Star1 (ACTIVE-GOAL (LOVERS Movie-Star1 Me)))))

(define-phrase "he declines."
  (MTRANS Movie-Star1 Movie-Star1 Me
          (NOT (BELIEVE Movie-Star1 (ACTIVE-GOAL (M-RESTAURANT
                                                  Movie-Star1 Me))))))

(define-phrase "he accepts."
  (MTRANS Movie-Star1 Movie-Star1 Me
          (BELIEVE Movie-Star1 (ACTIVE-GOAL (M-RESTAURANT Movie-Star1 Me)))))

;
; Store guy episode
;

(ob$create '(MALE-PERSON first-name "Guy" obname Guy1))

(ob$create '(EDIBLE name "Boston Lettuce" obname Edible1))

(ob$fcreate '(STORE name "Westward Ho" obname Westward-Ho))

(ob$fcreate '(LOCATION name "Westward Ho" obname Westward-Ho-Loc))

(define-phrase "a cute guy buys some boston lettuce."
  (AT Guy1 Westward-Ho-Loc)
  (ATTRACTIVE Guy1)
  (M-PURCHASE Guy1 Edible1 Westward-Ho))

(define-phrase "guy introduces himself to me."
  (MTRANS Guy1 Guy1 Me (INTRODUCTION)))

(define-initial-fact (all lovers)
  (POS-ATTITUDE Edible1))

(define-rule Pos-Attitude-Inf1 (all lovers)
  (RULE subgoal (M-PURCHASE (UAND ?Person (UDIST ?Self)) ?Phys-Obj ?Store)
        goal (BELIEVE ?Person (POS-ATTITUDE ?Phys-Obj))
        is 'inference-only
        inf-comments '(if "person M-PURCHASE object from store"
                       then "person has POS-ATTITUDE toward object")
        plausibility 1.0))

(define-phrase "he smiles at me."
  (M-SMILE Guy1 Me))

(define-phrase "guy accepts."
  (MTRANS Guy1 Guy1 Me
          (BELIEVE Guy1 (ACTIVE-GOAL (M-RESTAURANT Guy1 Me)))))

(define-phrase "guy gives me his address."
  (MTRANS Guy1 Guy1 Me (ADDRESS Guy1 (LOCATION "Guy's house" obname
                                               Guy-Home))))

(define-phrase "it is friday night."
  (FRIDAY-NIGHT))

(define-phrase "guy agrees."
  (MTRANS Guy1 Guy1 Me
          (BELIEVE Guy1 (ACTIVE-GOAL (LOVERS Guy1 Me)))))

(define-phrase "guy dumps me."
  (MTRANS Guy1 Guy1 Me
          (BELIEVE Guy1 (NOT (ACTIVE-GOAL (LOVERS Me Guy1))))))

;
; syl-ep
;
; Todo: Have to add rules so that DDer tells it no?
;

(ob$create '(MALE-PERSON first-name "Sylvester" obname Sylvester))

(define-phrase "Sylvester calls you on the phone and asks you out."
  (M-PHONE Sylvester Me)
  (MTRANS Sylvester Sylvester Me
          (BELIEVE Sylvester (ACTIVE-GOAL (LOVERS Sylvester Me)))))

;
; Jerry ep
;

(ob$create '(MALE-PERSON "Jerry" obname Jerry))

(define-initial-fact (jerry-ep) (ROMANTIC-INTEREST Jerry))

(ob$create '(FEMALE-PERSON obname Jerry-Gf))
(define-initial-fact (jerry-ep) (LOVERS Jerry Jerry-Gf))
(define-initial-fact (jerry-ep) (ACQUAINTED Jerry))
(define-initial-fact (jerry-ep) (KNOW Me (TELNO Jerry)))

; Should be able to retract facts too.
(define-phrase "jerry broke up with his girlfriend."
  (NOT (LOVERS Jerry Jerry-Gf)))

(define-phrase "jerry turns me down."
  (MTRANS Jerry Jerry Me
          (NOT (BELIEVE Jerry (ACTIVE-GOAL (LOVERS Jerry Me))))))

(define-phrase "jerry accepts."
  (MTRANS Jerry Jerry Me
          (BELIEVE Jerry (ACTIVE-GOAL (LOVERS Jerry Me)))))

;
; Earthquake example
;

(define-phrase "there is an earthquake in mexico city."
  (EARTHQUAKE (CITY obname Mexico-City name "Mexico City")))

(define-initial-fact (all earthquake-alone)
  (EARTHQUAKE Mexico-City))

;
; Carol Burnett example
;
; Note that EXPERIENCE1 could also be done by having the state (AT Mo-St1 Nuart)
; already in the initial database. What different behavior do we get (as
; far as REVERSAL) if we do this? Also, can we also do the Carol ep in the
; input way?
; Actually, this is different, cause remember, anything in the database is
; considered to a belief of the dder.
;

(define-phrase "carol burnett went to ucla."
  (COLLEGE (FEMALE-ACTOR first-name "Carol" last-name "Burnett"
                         obname Carol)
           (UNIVERSITY name "UCLA" obname UCLA)))

; An alumni book from my college is in my mailbox.
(define-initial-fact (all always)
  (AT (MAIL contents (ALUMNI-DIR obj UCLA obname Alumni-Dir1) obname Mail1)
      (LOCATION name "outside" obname Outside)))

(define-initial-fact (all always) (KNOW Me Outside))

(define-phrase "carol's telephone number is in the alumni directory."
  (KNOW Alumni-Dir1 (TELNO Carol)))

(define-rule Poss-Mail-Inf (all recovery3)
  (RULE subgoal (POSS ?Self (MAIL ?Phys-Obj))
        goal (POSS ?Self ?Phys-Obj)
        is 'inference-only
        inf-comments '(if "self POSS MAIL containing object"
                       then "self POSS object")
        plausibility 1.0))

; Why would this be needed? Everything in the db is a bit of knowledge.
;(define-rule College-Plan (all recovery3)
;  (RULE subgoal (COLLEGE ?Person ?University)
;        goal (KNOW ?Self (COLLEGE ?Person ?University))
;        plausibility 1.0))

(define-rule Inverse-College-Plan (all recovery3)
  (RULE subgoal (KNOW ?Self (COLLEGE ?Person))
        goal (COLLEGE ?Person ?University)
        is 'plan-only
        plan-comments '(if
                        "ACTIVE-GOAL for COLLEGE of person to be a university"
                        then "ACTIVE-GOAL for self to KNOW COLLEGE of person")
        plausibility 1.0))

;
; Object phrases
;

(define-phrase "computer." (COMPUTER))
(define-phrase "money." (CASH))
(define-phrase "cash." (CASH))
(define-phrase "drawing." (DRAWING))
(define-phrase "lampshade." (LAMPSHADE))

;
; Generic rules
;

;
; 'Always' rules and initial facts
;

(define-initial-fact (unused)
  (AT Me Nuart-Location))

(ob$create '(LOCATION name "home" obname Home))

(define-initial-fact (all always)
  (AT Me Home))

(define-initial-fact (all always) (ADDRESS Me Home))

(define-initial-fact (all always) (KNOW Me Home))

;
; Basic States and Actions
;

;
; ATRANS
;

(define-rule Atrans-Plan1 (all mut5)
  (RULE subgoal (RSEQ (AT ?Person1 ?Location)
                      (AT ?Person2 ?Location)
                      (BELIEVE ?Person1 (ACTIVE-GOAL (POSS ?Person2
                                                           ?Phys-Obj))))
        goal (ATRANS ?Person1 ?Person1 ?Person2 ?Phys-Obj)
        initial (ROR (AT ?Person1 ?Location) (RTRUE))
        is 'action-plan
        plan-comments '(if "ACTIVE-GOAL for person1 to ATRANS object to person2"
                       then "ACTIVE-GOAL for person1 et person2 to be AT"
                       "same location and"
                       "ACTIVE-GOAL for person1 to have ACTIVE-GOAL"
                       "for person2 to POSS object")
        plausibility 1.0))

; Atrans plans should have (POSS ?Person ?Object) as a precondition?!
(define-rule Atrans-Plan2 (all purchase oseren earthquake employment1)
  (RULE subgoal (RSEQ (AT ?Other ?Location)
                      (AT ?Self ?Location))
        goal (ATRANS ?Other ?Other ?Self ?Object)
        initial (ROR (AT ?Other ?Location) (RTRUE))
        ; must be ?Object and not ?Phys-Obj so that (INSURANCE) matches.
        is 'action-plan
        plan-comments '(if "ACTIVE-GOAL for person to ATRANS object to self"
                       then "ACTIVE-GOAL for self et person to be AT"
                       "same location")
        plausibility 1.0))

(define-rule Atrans-Plan3 (all purchase oseren)
  (RULE subgoal (RSEQ (AT ?Other ?Location)
                      (AT ?Self ?Location))
        goal (ATRANS ?Self ?Self ?Other ?Object)
        initial (ROR (AT ?Other ?Location) (RTRUE))
        is 'action-plan
        plan-comments '(if "ACTIVE-GOAL for self to ATRANS object to person"
                       then "ACTIVE-GOAL for self et person to be AT"
                       "same location")
        plausibility 1.0))

(define-rule Atrans-Plan4 (all purchase oseren)
  (RULE subgoal (RSEQ (AT ?Person2 ?Location)
                      (AT ?Person1 ?Location))
        goal (ATRANS ?Person2 ?Person2 ?Person1 ?Object)
        initial (ROR (AT ?Person2 ?Location) (RTRUE))
        is 'action-plan
        plan-comments '(if "ACTIVE-GOAL for person2 to ATRANS object to person1"
                       then "ACTIVE-GOAL for person1 et person2 to be AT"
                       "same location")
        plausibility 0.8))

;
; M-Purchase
;

(ob$fcreate '(NEWSPAPER name "the Los Angeles Times" 
                        obname Los-Angeles-Times))

(define-initial-fact (all employment1)
  (SELLS Westward-Ho Los-Angeles-Times))

(ob$fcreate '(EDIBLE name "food" obname Food1))

(define-initial-fact (all food-need)
  (SELLS Westward-Ho Food1))

(ob$create '(MALE-PERSON first-name "Joe" obname Checker1))

(define-initial-fact (all purchase)
  (EMPLOYMENT actor Checker1
              organization Westward-Ho))

(define-initial-fact (all purchase)
  (AT Checker1 Westward-Ho-Loc))

(define-phrase "he gives me a newspaper."
  (ATRANS Checker1 Checker1 Me Los-Angeles-Times))

(define-phrase "he gives me groceries."
  (ATRANS Checker1 Checker1 Me Food1))

(define-initial-fact (all purchase)
  (KNOW Me Westward-Ho-Loc))

(define-rule M-Purchase-Plan (all purchase)
  (RULE subgoal (RSEQ (SELLS ?Store ?Phys-Obj)
                      (EMPLOYMENT actor ?Person organization ?Store)
                      (ATRANS ?Self ?Self ?Person (CASH))
                      (ATRANS ?Person ?Person ?Self ?Phys-Obj))
        goal (M-PURCHASE ?Self ?Phys-Obj ?Store)
        plan-comments '(if
                        "ACTIVE-GOAL for self to M-PURCHASE object from store"
                        then "ACTIVE-GOAL for self to ATRANS CASH to that store"
                        "and ACTIVE-GOAL for store to ATRANS object to self")
        is 'plan-only
        plausibility 1.0))

;
; Personal Goals
;

;
; Food
;

; Personal goals are ordered:
; LOVERS > EMPLOYMENT > FRIENDS > MONEY > FOOD > ENTERTAINMENT

(define-rule Food-Theme (all food-need)
  (RULE subgoal (UAND (FOOD) (UPROC 'Less-Need-Thresh?))
        goal (ACTIVE-GOAL (FOOD strength (UPROC 'Need-Satisfied?)))
        is 'inference-only
        emotion (POS-EMOTION strength 0.7)
        inf-comments '(if "level of satisfaction of FOOD need below"
                          "threshold"
                        then "ACTIVE-GOAL for FOOD")
        plausibility 1.0))

(setq *need-thresh* 0.4)

(defun less-need-thresh? (s)
  (fl< (strength s) *need-thresh*))

(defun need-satisfied? (s)
  (fl>= s *need-thresh*))

(define-rule Food-Plan (all food-need)
  (RULE subgoal (M-EAT ?Self)
        goal (FOOD)
        is 'plan-only-no-auto
        plan-comments '(if "ACTIVE-GOAL for FOOD"
                        then "ACTIVE-GOAL for self to M-EAT")
        plausibility 1.0))

(define-rule Food-Inf (all food-need)
  (RULE subgoal (M-EAT ?Self)
        goal (FOOD)
        is 'inference-only
        inf-comments '(if "self M-EAT"
                        then "FOOD need satisfied")
        plausibility 1.0))

(define-rule M-Eat-Plan (all food-need lovers)
  (RULE subgoal (POSS ?Person ?Edible)
        goal (M-EAT ?Person)
        is 'action-plan
        plan-comments '(if "ACTIVE-GOAL for self to M-EAT"
                        then "ACTIVE-GOAL for self to POSS food")
        plausibility 1.0))

(define-rule Poss-Edible-Plan (all food-need)
  (RULE subgoal (M-PURCHASE ?Person ?Edible ?Store)
        goal (POSS ?Person ?Edible)
        plan-comments '(if "ACTIVE-GOAL for self to POSS food"
                        then
                        "ACTIVE-GOAL for self to M-PURCHASE food from store")
        inf-comments '(if "self M-PURCHASE food from store"
                        then "self POSS food")
        plausibility 1.0))

(ob$fcreate '(RESTAURANT name "Chan Dara" obname Restaurant1))
(ob$fcreate '(LOCATION name "Chan Dara" obname Restaurant1-Loc))
(ob$fcreate '(FEMALE-PERSON first-name "Rebecca" obname Waiter1))

(define-phrase "guy goes to his house."
  (PTRANS Guy1 Restaurant1-Loc Guy-Home Guy1))

(define-initial-fact (all lovers)
  (AT Restaurant1 Restaurant1-Loc))

(define-initial-fact (all lovers)
  (KNOW Me Restaurant1-Loc))

(define-initial-fact (all lovers)
  (KNOW Guy1 Restaurant1-Loc))

(define-initial-fact (all lovers)
  (KNOW Guy1 Guy-Home))

(define-initial-fact (all lovers)
  (EMPLOYMENT actor Waiter1 organization Restaurant1))

(define-initial-fact (all lovers)
  (AT Waiter1 Restaurant1-Loc))

(ob$fcreate '(EDIBLE name "Thai food" obname Food2))

(ob$fcreate '(EDIBLE name "Thai food" obname Food3))

;(define-initial-fact (all lovers)
;  (SELLS Restaurant1 Food2))

(define-initial-fact (all lovers)
  (POSS Waiter1 Food2))

(define-initial-fact (all lovers)
  (POSS Waiter1 Food3))

; Temp phrases

(define-phrase "guy goes to chan dara."
  (PTRANS Guy1 Guy-Home Restaurant1-Loc Guy1))

(define-phrase "guy goes to chan dara1."
  (PTRANS Guy1 Westward-Ho-Loc Restaurant1-Loc Guy1))

(define-phrase "the waiter serves guy."
  (ATRANS Waiter1 Waiter1 Guy1 Food2))

(define-phrase "the waiter serves me."
  (ATRANS Waiter1 Waiter1 Me Food3))

(define-phrase "guy eats."
  (M-EAT Guy1))

; end temp

; Todo: A 'group' entity might help the below, just as for M-MOVIE.
(define-rule M-Restaurant-Plan (all lovers patrol)
  (RULE subgoal (RSEQ (AT ?Restaurant ?Location)
                      (EMPLOYMENT actor ?Person organization ?Restaurant)
                      (AT ?Other ?Location)
                      (AT ?Self ?Location)
                      (POSS ?Person ?Edible1)
                      (ATRANS ?Person ?Person ?Other ?Edible1)
                      (LATER (POSS ?Person ?Edible2))
                      (ATRANS ?Person ?Person ?Self ?Edible2)
                      (M-EAT ?Other)
                      (M-EAT ?Self)
                      (ATRANS ?Self ?Self ?Person (CASH)))
        goal (M-RESTAURANT ?Self ?Other)
        is 'plan-only
        script 't
        plan-comments '(if "ACTIVE-GOAL for M-RESTAURANT with self et person"
                        then "ACTIVE-GOAL for self et person to be AT location"
                        "of restaurant and"
                 "ACTIVE-GOAL for waiter to ATRANS food to self et person and"
                        "ACTIVE-GOAL for self et person to M-EAT and"
                        "ACTIVE-GOAL for self to ATRANS CASH to waiter")
        plausibility 1.0))

;
; Entertainment
;

(define-rule Entertainment-Theme (all lovers1)
  (RULE subgoal (UAND (ENTERTAINMENT) (UPROC 'Less-Need-Thresh?))
        goal (ACTIVE-GOAL (ENTERTAINMENT strength (UPROC 'Need-Satisfied?)))
        is 'inference-only
        emotion (POS-EMOTION strength 0.6)
        inf-comments '(if "level of satisfaction of ENTERTAINMENT need below"
                          "threshold"
                       then "ACTIVE-GOAL for ENTERTAINMENT")
        plausibility 1.0))

(define-rule Entertainment-Plan (all lovers)
  (RULE subgoal (M-MOVIE ?Self ?Other)
        goal (ENTERTAINMENT)
        plan-comments '(if "ACTIVE-GOAL for ENTERTAINMENT"
                        then "ACTIVE-GOAL for M-MOVIE with self et person")
        is 'plan-only-no-auto
        plausibility 1.0))

(define-rule Entertainment-Plan1 (all lovers1)
  (RULE subgoal (M-MOVIE ?Self)
        goal (ENTERTAINMENT)
        plan-comments '(if "ACTIVE-GOAL for ENTERTAINMENT"
                        then "ACTIVE-GOAL for M-MOVIE with self")
        is 'plan-only-no-auto
        plausibility 1.0))

(define-rule Entertainment-Inf1 (all lovers1)
  (RULE subgoal (M-MOVIE ?Self)
        goal (ENTERTAINMENT)
        inf-comments '(if "M-MOVIE with self"
                       then "ENTERTAINMENT need satisfied")
        is 'inference-only
        plausibility 1.0))

(define-initial-fact (all rain) (RAINING Outside))

; (define-initial-fact (all) (AT (NEWSPAPER obname Newspaper1) Outside))

(define-rule Entertainment-Plan3 (unused)
  (RULE subgoal (MTRANS ?Self ?Newspaper ?Self ?Info) ; see below for mods
        goal (ENTERTAINMENT)
        plan-comments '(if "ACTIVE-GOAL for ENTERTAINMENT"
                        then "ACTIVE-GOAL to MTRANS from newspaper to self")
        is 'plan-only-no-auto
        plausibility 1.0))

; Since there is currently no uniquification, ?Phys-Obj1 must be so
; to avoid conflict with ?Phys-Obj in Grab-Plan.
(define-rule Entertainment-Plan2 (all rain recovery3)
  (RULE subgoal (RSEQ (POSS ?Self (MAIL ?Phys-Obj1))
                      (KNOW ?Phys-Obj1 ?Knowable)
                      (MTRANS ?Self ?Phys-Obj1 ?Self ?Knowable))
        goal (ENTERTAINMENT)
        plan-comments '(if "ACTIVE-GOAL for ENTERTAINMENT"
                        then "ACTIVE-GOAL to MTRANS from MAIL to self")
        plan-no-gen '(nil t t)
        is 'plan-only-no-auto
        plausibility 1.0))

(define-rule Entertainment-Inf2 (all rain recovery3)
  (RULE subgoal (RAND (POSS ?Self (MAIL ?Phys-Obj1))
                      (MTRANS ?Self ?Phys-Obj1 ?Self ?Knowable))
        goal (ENTERTAINMENT)
        is 'inference-only
        inf-comments '(if "MTRANS from MAIL to self"
                          then "ENTERTAINMENT need satisfied")
        plausibility 1.0))

;
; The rule below needs work. A flaw is that we cannot say ROR LOVERS.
; MTRANS can't handle multiple actors. Do we need a 'group' entity?
;
(define-rule M-Movie-Plan (all lovers patrol)
  (RULE subgoal (RSEQ (FRIENDS ?Self ?Other)
                      (MTRANS actor ?Self ?Other from ?Theater
                              to ?Self ?Other obj (MOVIE)))
        goal (M-MOVIE ?Self ?Other)
        script 't
        plan-comments '(if "ACTIVE-GOAL for M-MOVIE with self et person"
                        then "ACTIVE-GOAL for FRIENDS with person and"
                        "ACTIVE-GOAL for self et person to MTRANS MOVIE from"
                        "theater to self et person")
        is 'plan-only
        plausibility 1.0))

;
; This is 'compiled' to speed preconditions (and to enable it possible to
; return home)
;
(define-rule M-Movie-Alone-Plan (all lovers1)
  (RULE subgoal (RSEQ (AT ?Theater ?Location2)
                      (PTRANS ?Self ?Location1 ?Location2 ?Self)
                      (MTRANS ?Self ?Theater ?Self (MOVIE))
                      (PTRANS ?Self ?Location2 ?Location1 ?Self))
        goal (M-MOVIE ?Self)
        initial (AT ?Self ?Location1)
        is 'plan-only
        plan-no-gen '(t t nil t)
        plan-comments '(if "ACTIVE-GOAL for M-MOVIE with self"
                        then "ACTIVE-GOAL to PTRANS to theater and"
                        "ACTIVE-GOAL to MTRANS MOVIE from theater to self and"
                        "ACTIVE-GOAL to PTRANS back to original location")
        plausibility 1.0))

(define-rule Mtrans-Movie-Plan (all lovers1)
  (RULE subgoal (RSEQ (AT ?Phys-Obj ?Location)
                      (AT ?Self ?Location))
        goal (MTRANS ?Self ?Phys-Obj ?Self (MOVIE))
        is 'action-plan
        plan-comments '(if "ACTIVE-GOAL to MTRANS MOVIE from theater to self"
                        then "ACTIVE-GOAL to be AT location of theater")
        plausibility 1.0))

(define-initial-fact (all lovers1) (KNOW Me Nuart-Location))

;
; Friends
;

(define-rule Friends-Theme (all friends)
  (RULE subgoal (RAND (POS-ATTITUDE ?Other)
                      (RNOT (FRIENDS ?Self ?Other))
                      (RNOT (LOVERS ?Self ?Other))
                      (RNOT (ACTIVE-GOAL (LOVERS ?Self ?Other))))
        goal (ACTIVE-GOAL (FRIENDS ?Self ?Other))
        is 'inference-only
        inf-comments '(if "POS-ATTITUDE toward someone with whom not FRIENDS or"
                          "LOVERS"
                       then "ACTIVE-GOAL for FRIENDS with person")
        emotion (POS-EMOTION strength 0.8)
        plausibility 1.0))

(define-rule Friends-Plan (all friends)
  (RULE subgoal (RSEQ (ACQUAINTED ?Self ?Other)
                      (POS-ATTITUDE ?Other)
                      (BELIEVE ?Other (POS-ATTITUDE ?Self)))
        goal (FRIENDS ?Self ?Other)
        plan-no-gen '(nil nil nil nil nil nil nil)
        plan-comments '(if "ACTIVE-GOAL for FRIENDS with person"
                    then "ACTIVE-GOAL for self et person to be ACQUAINTED and"
                    "ACTIVE-GOAL to have POS-ATTITUDE toward person and"
                    "ACTIVE-GOAL for person to have POS-ATTITUDE toward self")
        inf-comments '(if "self et person ACQUAINTED and"
                        "POS-ATTITUDE toward person and"
                        "person has POS-ATTITUDE toward self"
                        then "FRIENDS with person")
        plausibility 0.95))

; Need friends maintenance rules which involve doing activities
; together.

;
; Employment
;

(define-rule Employment-Theme (all employment1)
  (RULE subgoal (RNOT (EMPLOYMENT actor ?Self))
; The below should not be necessary. The above EMPLOYMENT state should
; tell the whole story.
;                     (FAILED-GOAL (EMPLOYMENT actor ?Self)))
; Took the below out because of problems with spurious activation in daydreaming
; mode. See also Lovers-Theme.
; Alterns: + copy needs in each dd-goal base ctxt -- no decay.
;          + use top-level-goal (UNOT ?Dd-Goal-Obj)
;                      (ROR (UAND ?Employment-Subsumption
;                                 (UPROC 'Less-Need-Thresh?)))
        goal (ACTIVE-GOAL (EMPLOYMENT actor ?Self actor ?Other
                                      organization ?Organization))
        is 'inference-only
        inf-comments '(if "self not have EMPLOYMENT with anyone and"
                          "satisfaction level of MONEY or POSSESSIONS need"
                          "below threshold"
                       then "ACTIVE-GOAL for EMPLOYMENT with person")
        emotion (POS-EMOTION strength 0.85)
        plausibility 1.0))

(define-rule Employment-Plan (all employment1)
  (RULE subgoal (RSEQ (BELIEVE ?Other (ACTIVE-GOAL
                                       (EMPLOYMENT actor 'qualified
                                                   actor ?Other
                                                   organization ?Organization)))
                      (BELIEVE ?Other (ACTIVE-GOAL
                                       (EMPLOYMENT actor ?Self
                                                   actor ?Other
                                                   organization ?Organization)))
;                      (POS-ATTITUDE (EMPLOYMENT actor ?Self actor ?Other
;                                                organization ?Organization))
                      (M-AGREE actor ?Other actor ?Self
                               obj (EMPLOYMENT actor ?Self actor ?Other
                                               organization ?Organization)))
        goal (EMPLOYMENT actor ?Self actor ?Other organization ?Organization)
        plan-comments '(if "ACTIVE-GOAL for EMPLOYMENT with person"
                        then "ACTIVE-GOAL for person to have ACTIVE-GOAL of"
                        "employing someone and"
            "ACTIVE-GOAL for person to have ACTIVE-GOAL of employing self and"
            "ACTIVE-GOAL for self et person to M-AGREE to EMPLOYMENT of self"
                        "with person")
        is 'plan-only
        plausibility 1.0))

(define-phrase "a job opening is listed in the newspaper."
  (KNOW Los-Angeles-Times
        (BELIEVE (MALE-PERSON first-name "James" obname James)
                 (ACTIVE-GOAL (EMPLOYMENT actor 'qualified
                                          actor James
                                          organization
                                          (ORGANIZATION
                                           name "the Broadway"
                                           obname The-Broadway))))))

(define-phrase "he offers me a job."
  (MTRANS James James Me
          (BELIEVE James
                   (ACTIVE-GOAL (EMPLOYMENT actor Me actor James
                                            organization The-Broadway)))))

(define-initial-fact (all employment1)
  (AT James (LOCATION name "the Broadway"
                      obname The-Broadway-Loc)))

(define-initial-fact (all employment1)
  (KNOW Me The-Broadway-Loc))

(define-rule Opening-Plan (all employment1)
  (RULE subgoal (MTRANS ?Self ?Newspaper ?Self
                        (BELIEVE ?Other
                                 (ACTIVE-GOAL (EMPLOYMENT actor 'qualified
                                                          actor ?Other
                                                          organization
                                                          ?Organization))))
        goal (BELIEVE ?Other (ACTIVE-GOAL
                              (EMPLOYMENT actor 'qualified
                                          actor ?Other
                                          organization ?Organization)))
        plan-comments '(if
          "ACTIVE-GOAL for person to have ACTIVE-GOAL of employing someone"
                        then "ACTIVE-GOAL to MTRANS from newspaper to"
                        "self that person has ACTIVE-GOAL of employing someone")
        inf-comments '(if "self MTRANS from newspaper to self that person has"
                        "ACTIVE-GOAL of employing someone"
                        then "person has ACTIVE-GOAL of employing someone")
        plausibility 1.0))

(define-rule Poss-Newspaper-Plan (all employment1)
  (RULE subgoal (M-PURCHASE ?Self ?Newspaper ?Store)
        goal (POSS ?Self ?Newspaper)
        plan-comments '(if "ACTIVE-GOAL for self to POSS newspaper"
                        then "ACTIVE-GOAL for self to M-PURCHASE newspaper from"
                        "store")
        inf-comments '(if "self M-PURCHASE newspaper from store"
                        then "self POSS newspaper")
        plausibility 1.0))

(define-rule Money-Theme (all employment1)
  (RULE subgoal (RAND (UAND (MONEY) (UPROC 'Less-Need-Thresh?))
                      (EMPLOYMENT actor ?Self actor ?Person
                                  organization ?Organization))
        goal (ACTIVE-GOAL (MONEY strength (UPROC 'Need-Satisfied?)))
        inf-comments '(if "satisfaction level of MONEY need below threshold"
                          "and self has EMPLOYMENT with person"
                       then "ACTIVE-GOAL for MONEY")
        emotion (POS-EMOTION strength 0.75)
        plausibility 1.0
        is 'inference-only))

(define-rule Money-Plan (all employment1)
  (RULE subgoal (M-WORK ?Self ?Other)
        goal (MONEY)
        plan-comments '(if "ACTIVE-GOAL for MONEY"
                        then "ACTIVE-GOAL for self to M-WORK for person")
        is 'plan-only-no-auto
        plausibility 1.0))

(define-rule Money-Inf (all employment1)
  (RULE subgoal (UAND ?M-Work (M-WORK ?Self ?Other))
        goal (MONEY)
        delete ?M-Work ; fix to problem of it thinking action is
                       ; already performed?
        inf-comments '(if "self M-WORK for person"
                        then "MONEY need satisfied")
        is 'inference-only
        plausibility 1.0))

(define-rule M-Work-Plan (all employment1)
  (RULE subgoal (RSEQ (EMPLOYMENT actor ?Self actor ?Other
                                  organization ?Organization)
                      (AT ?Other ?Location)
                      (AT ?Self ?Location))
        goal (M-WORK ?Self ?Other)
        plan-comments '(if "ACTIVE-GOAL for self to M-WORK for person"
                        then
                        "ACTIVE-GOAL for self to have EMPLOYMENT with person"
                        "and ACTIVE-GOAL to be AT location of person")
        is 'action-plan
        plausibility 1.0))

(define-initial-fact (all employment1)
  (POSS Me (RESUME Me)))

(define-rule Pos-Att-Employ-Plan2 (all employment1)
  (RULE subgoal (POSS (UAND ?Other (UDIST ?Self)) (RESUME ?Self))
        goal (BELIEVE ?Other
                      (POS-ATTITUDE (EMPLOYMENT actor ?Other actor ?Self
                                                organization ?Organization)))
        top-level-goal (EMPLOYMENT)
        is 'plan-only
        plan-comments '(if "ACTIVE-GOAL for person to have POS-ATTITUDE toward"
                           "EMPLOYMENT of self with person"
                           then "ACTIVE-GOAL for person to POSS RESUME of self")
        plausibility 0.8))

;
; Lovers
;

(define-rule Lovers-Theme (all lovers1 lovers)
  ; (RULE subgoal (UAND (ENTERTAINMENT) (UPROC 'Less-Need-Thresh?))
  (RULE subgoal (RAND (ROR (RNOT (LOVERS ?Self ?Other))
                           (FAILED-GOAL (LOVERS ?Self ?Other)))
; Took the below out because of problems with spurious activation in daydreaming
; mode. See also Employment-Theme.
;                      (UAND ?Lovers-Subsumption
;                            (UPROC 'Less-Need-Thresh?))
                      (RNOT (ACTIVE-GOAL (LOVERS ?Self ?Male-Person))))
        goal (ACTIVE-GOAL (LOVERS ?Self ?Male-Person))
        is 'inference-only
        initial-status 'halted  ; for debugging, for now.
        self-type FEMALE-PERSON
        inf-comments '(if "self not LOVERS with anyone"
                       then "ACTIVE-GOAL for LOVERS with some person")
        emotion (POS-EMOTION strength 0.9) ; was 0.8, was 0.9
        plausibility 1.0))

(define-rule Lovers-Plan (all lovers1 mut oseren-alone)
  (RULE subgoal (RSEQ (ACQUAINTED ?Self ?Other)
                      (ROMANTIC-INTEREST ?Other)
                      (BELIEVE ?Other (ACTIVE-GOAL (LOVERS ?Self ?Other)))
                      (M-DATE ?Self ?Other)
                      (M-AGREE actor ?Self actor ?Other
                               obj (LOVERS ?Self ?Other)))
        goal (LOVERS ?Self ?Other)
        plan-no-gen '(nil nil nil nil nil nil nil)
        plan-comments '(if "ACTIVE-GOAL for LOVERS with person"
                       then "ACTIVE-GOAL for ACQUAINTED with person and"
                       "ACTIVE-GOAL for ROMANTIC-INTEREST in person and"
         "ACTIVE-GOAL for person to have ACTIVE-GOAL of LOVERS with self and"
                       "ACTIVE-GOAL for M-DATE with self et person and"
                       "ACTIVE-GOAL for self et person to M-AGREE to LOVERS")
        is 'plan-only
        plausibility 0.95))

(define-rule Lovers-Subsumption-Theme (all lovers)
  (RULE subgoal (RAND (UAND ?Lovers-Subsumption
                            (UPROC 'Less-Need-Thresh?))
                      (LOVERS ?Self ?Person))
        goal (ACTIVE-GOAL (UCODE 'LOVERS-SUBSUMPTION-CODE))
        inf-comments '(if "SEX or LOVE-GIVING or LOVE-RECEIVING or"
                          "COMPANIONSHIP need below threshold and"
                          "LOVERS with person"
                        then "ACTIVE-GOAL to satisfy need")
        is 'inference-only
        plausibility 1.0))

(defun lovers-subsumption-code ()
  (let ((result (ob$create-empty)))
    (ob$set result 'type (ob$ty (bd-lookup 'Lovers-Subsumption *ob-bindings*)))
    (ob$set result 'strength (ob$fcreate '(UPROC proc 'Need-Satisfied?)))
    result))

(define-rule Sex-Plan (all lovers)
  (RULE subgoal (M-SEX ?Self ?Other)
        goal (SEX)
        plan-comments '(if "ACTIVE-GOAL for SEX"
                        then "ACTIVE-GOAL for M-SEX with person")
        inf-comments '(if "M-SEX with person"
                        then "SEX need satisfied")
        plausibility 1.0))

(define-rule M-Sex-Plan (all lovers)
  (RULE subgoal (LOVERS ?Self ?Other)
        goal (M-SEX ?Self ?Other)
        plan-comments '(if "ACTIVE-GOAL for M-SEX with person"
                        then "ACTIVE-GOAL for LOVERS with person")
        is 'action-plan
        plausibility 1.0))

(define-rule Love-Giving-Plan (all lovers)
  (RULE subgoal (M-KISS ?Self ?Other)
        goal (LOVE-GIVING)
        plan-comments '(if "ACTIVE-GOAL for LOVE-GIVING"
                        then "ACTIVE-GOAL to M-KISS person")
        inf-comments '(if "M-KISS person"
                        then "LOVE-GIVING need satisfied")
        plausibility 1.0))

(define-rule Love-Receiving-Plan (all lovers)
  (RULE subgoal (M-KISS ?Other ?Self)
        goal (LOVE-RECEIVING)
        plan-comments '(if "ACTIVE-GOAL for LOVE-RECEIVING"
                        then "ACTIVE-GOAL for person to M-KISS self")
        inf-comments '(if "person M-KISS self"
                        then "LOVE-RECEIVING need satisfied")
        plausibility 1.0))

(define-rule M-Kiss-Plan1 (all lovers)
  (RULE subgoal (RTRUE)
        goal (M-KISS ?Self ?Other)
        initial (ACTIVE-GOAL (M-DATE ?Self ?Other))
        plan-comments '(if "ACTIVE-GOAL to M-KISS person while on M-DATE"
                           then "ACTIVE-GOAL for RTRUE")
        is 'action-plan
        plausibility 1.0))

(define-rule M-Kiss-Plan2 (all lovers)
  (RULE subgoal (LOVERS ?Self ?Other)
        goal (M-KISS ?Self ?Other)
        initial (RNOT (ACTIVE-GOAL (M-DATE ?Self ?Other)))
        plan-comments '(if "ACTIVE-GOAL to M-KISS person"
                        then "ACTIVE-GOAL for LOVERS with person")
        is 'action-plan
        plausibility 1.0))

(define-rule Companionship-Inf (all lovers)
  (RULE subgoal (UAND ?Activity (NOTYPE actor ?Self ?Other)
                      (UNOT (M-AGREE))
                      (UNOT (M-CONVERSATION)))
        goal (COMPANIONSHIP)
        inf-comments '(if "self et person perform some activity together"
                        then "COMPANIONSHIP need satisfied")
        is 'inference-only
        plausibility 1.0))

(define-rule Companionship-Plan (all lovers)
  (RULE subgoal (M-MOVIE ?Self ?Other)
        goal (COMPANIONSHIP)
        plan-comments '(if "ACTIVE-GOAL for COMPANIONSHIP"
                        then "ACTIVE-GOAL for M-MOVIE with self et person")
        inf-comments '(if "M-MOVIE with self et person"
                        then "COMPANIONSHIP need satisfied")
        plausibility 1.0))

;
; Acquaintance planning
;

(define-rule Acquainted-Plan (all lovers1 mut oseren-alone)
  (RULE subgoal (M-CONVERSATION ?Person1 ?Person2)
        goal (ACQUAINTED ?Person1 ?Person2)
        inf-no-gen '(t)
        plan-no-gen '(activate)
        inf-comments '(if "M-CONVERSATION with person"
                       then "ACQUAINTED with person")
        plan-comments '(if "ACTIVE-GOAL to be ACQUAINTED with person"
                       then "ACTIVE-GOAL for M-CONVERSATION with person")
        plausibility 1.0))

(define-rule M-Conversation-Plan (all lovers1 oseren-alone)
  (RULE subgoal (RSEQ (MTRANS-ACCEPTABLE ?Person1 ?Person2)
                      (MTRANS ?Person1 ?Person1 ?Person2 (INTRODUCTION))
                      (MTRANS ?Person2 ?Person2 ?Person1 (INTRODUCTION)))
        ; was (SMALLTALK)
        ; Todo: Add planning for MTRANSing about personality traits,
        ; interests, etc.
        goal (M-CONVERSATION ?Person1 ?Person2)
        plan-no-gen '(activate t t)
        inf-no-gen '(t)
        plan-comments '(if "ACTIVE-GOAL for M-CONVERSATION between person1 et"
                           "person2"
                        then "ACTIVE-GOAL for MTRANS-ACCEPTABLE between"
                        "person1 et person2 and"
                "ACTIVE-GOAL for person1 to MTRANS INTRODUCTION to person2"
                        "and person2 to MTRANS INTRODUCTION to person1")
        inf-comments '(if "MTRANS-ACCEPTABLE between person1 et person2"
                          "and person1 MTRANS INTRODUCTION to person1 and"
                          "person2 MTRANS INTRODUCTION to person1"
                        then "M-CONVERSATION between person1 et person2")
        plausibility 1.0))

(define-rule Mtrans-Unacceptable-Inf (all)
  (RULE subgoal (RAND (RNOT (MTRANS-ACCEPTABLE ?Person1 ?Person2))
                      (MTRANS ?Person1 ?Person1 ?Person2 ?Anything))
        goal (BELIEVE ?Person2 (NEG-ATTITUDE ?Person1))
        inf-comments '(if "not MTRANS-ACCEPTABLE between person1 et"
                          "person2 and person1 MTRANS anything to person2"
                        then "person2 has NEG-ATTITUDE toward person1")
        is 'inference-only
        plausibility 1.0))

(define-rule Mtrans-Acceptable-Inf1 (all lovers1 oseren-alone)
  (RULE subgoal (ACQUAINTED ?Self ?Other)
        goal (MTRANS-ACCEPTABLE ?Self ?Other)
        inf-comments '(if "self et person ACQUAINTED"
                        then "MTRANS-ACCEPTABLE between self et person")
        is 'inference-only
        plausibility 1.0))

(define-rule Mtrans-Acceptable-Inf2 (all lovers1 oseren-alone)
  (RULE subgoal (MTRANS ?Self ?Self ?Other
                        (ACTIVE-GOAL (KNOW ?Self (TIME-OF-DAY))))
        goal (MTRANS-ACCEPTABLE ?Self ?Other)
        plan-no-gen '(t)
        inf-no-gen '(t)
        plan-comments '(if
                    "ACTIVE-GOAL for MTRANS-ACCEPTABLE between self et person"
                        then
                    "ACTIVE-GOAL for self to MTRANS to person that self"
                        "has ACTIVE-GOAL to KNOW the time")
        inf-comments '(if "self MTRANS to person that self has"
                       "ACTIVE-GOAL to KNOW the time"
                       then "MTRANS-ACCEPTABLE between self et person")
        plausibility .95))

(define-rule Mtrans-Acceptable-Inf3 (all lovers1 oseren-alone)
  (RULE subgoal (MTRANS ?Other ?Other ?Self ?Anything)
        goal (MTRANS-ACCEPTABLE ?Self ?Other)
        inf-comments '(if "person MTRANS anything to self"
                        then "MTRANS-ACCEPTABLE between self et person")
        is 'inference-only
        plausibility 1.0))

(define-rule Mtrans-Acceptable-Inf4 (all)
  (RULE subgoal (RSEQ (BELIEVE ?Other (ACTIVE-GOAL (ACQUAINTED ?Self ?Other)))
                      (MTRANS ?Self ?Self ?Other (INTRODUCTION)))
        goal (MTRANS-ACCEPTABLE ?Self ?Other)
        inf-comments '(if "person has ACTIVE-GOAL for self et person to be"
                          "ACQUAINTED and self MTRANS INTRODUCTION to person"
                       then "MTRANS-ACCEPTABLE between self et person")
        is 'inference-only
        plausibility 1.0))

(define-rule M-Party-Inf (all)
  (RULE subgoal (M-PARTY ?Self ?Other)
        goal (BELIEVE ?Other (ACTIVE-GOAL (ACQUAINTED ?Self ?Other)))
        plan-comments '(if
                   "ACTIVE-GOAL for person to have ACTIVE-GOAL for self et"
                   "person to be ACQUAINTED"
                        then "ACTIVE-GOAL for M-PARTY with self et person")
        inf-comments '(if "M-PARTY with self et person"
                        then "person has ACTIVE-GOAL for self et person"
                        "to be ACQUAINTED")
        plausibility 1.0))

(define-rule M-Smile-Inf1 (all lovers)
  (RULE subgoal (M-SMILE ?Other ?Self)
        goal (BELIEVE ?Other (ROMANTIC-INTEREST ?Self))
        is 'inference-only
        inf-comments '(if "person M-SMILE at self"
                       then "person has ROMANTIC-INTEREST in self")
        plausibility 0.4))

(define-rule M-Smile-Inf2 (all lovers)
  (RULE subgoal (M-SMILE ?Other ?Self)
        goal (BELIEVE ?Other (ACTIVE-GOAL (ACQUAINTED ?Self ?Other)))
        plan-comments '(if
                  "ACTIVE-GOAL for person to have ACTIVE-GOAL for self et"
                           "person to be ACQUAINTED"
                        then "ACTIVE-GOAL for person to M-SMILE at self")
        inf-comments '(if "person M-SMILE at self"
                        then "person has ACTIVE-GOAL for self et person"
                        "to be ACQUAINTED")
        plausibility 0.8))

; Note: any rules involving BELIEF should be stated in terms of ?Self
; and ?Other. When are rules NOT in terms of self and person?

(define-rule M-Agree-Plan (all lovers1 oseren-alone employment1)
  (RULE subgoal (RSEQ (MTRANS ?Self ?Self ?Other 
                              (BELIEVE ?Self (ACTIVE-GOAL ?Mutual-Obj)))
                      (MTRANS ?Other ?Other ?Self
                              (BELIEVE ?Other (ACTIVE-GOAL ?Mutual-Obj))))
        goal (M-AGREE actor ?Self actor ?Other obj ?Mutual-Obj)
        plan-no-gen '(t t)
        plan-comments '(if "ACTIVE-GOAL for self et person to M-AGREE to thing"
                        then
                        "ACTIVE-GOAL for self to MTRANS to person that self"
                        "has ACTIVE-GOAL for that something and"
                  "ACTIVE-GOAL for person to MTRANS to self that person have"
                        "ACTIVE-GOAL for thing")
        inf-comments '(if "self MTRANS to person that self have"
                        "ACTIVE-GOAL for thing and"
                        "person MTRANS to self that person has ACTIVE-GOAL"
                        "for thing"
                        then "self et person M-AGREE to that something")
        plausibility 1.0))

(define-rule Lovers-Theme-Plan (all lovers1 oseren-alone)
  (RULE subgoal (RSEQ (ROMANTIC-INTEREST ?Female-Person)
                      (NOT (LOVERS ?Self))) ; was RNOT, was .. ?:Female-Person
        goal (ACTIVE-GOAL (LOVERS ?Self ?Female-Person))
        is 'plan-only
;        self-type MALE-PERSON
        plan-comments '(if
             "ACTIVE-GOAL for self to have ACTIVE-GOAL of LOVERS with person"
                       then "ACTIVE-GOAL for ROMANTIC-INTEREST in person"
                       "and ACTIVE-GOAL for not LOVERS with anyone")
        plan-no-gen '(t t)
        plausibility 1.0))

; Note also that maintenance activities are similar to date activities.
; Can we collapse these?
; Possible activities: M-DINNER, M-MOVIE
;
; Everywhere that (M-RESTAURANT ?Self ?Other) appears in the below,
; used to be ?Activity. In the future, need generalized planning.
(define-rule M-Date-Plan (all lovers1 oseren-alone)
  (RULE subgoal (RSEQ ;(BELIEVE ?Other (POS-ATTITUDE
                                       ;(M-RESTAURANT ?Self ?Other)))
                      ;(POS-ATTITUDE (M-RESTAURANT ?Self ?Other))
                      (M-AGREE actor ?Self actor ?Other
                               obj (M-RESTAURANT ?Self ?Other))
                      (ENABLE-FUTURE-VPROX ?Self ?Other)
                      (FRIDAY-NIGHT)
                      (AT ?Other ?Location2)
                      (AT ?Self ?Location2)
; Todo: To put this back (needed for oseren?), you must modify instantiate
; to give ?Activity as the result when ?Activity is unbound; since UNOT
; is a special, it doesn't count for the UAND accumulation.
;                      (UAND ?Activity
;                            (UNOT (M-DATE ?Self ?Other))
;                            (UNOT (M-CONVERSATION ?Self ?Other)))
                      (M-RESTAURANT ?Self ?Other)
                      (LATER (AT ?Other ?Location2))
                      (LATER (AT ?Self ?Location2))
                      (M-KISS ?Self ?Other)
; We do NOT want the below--would make the M-AGREE which follows difficult.
;                      (AT ?Self ?Location1)
                      ) ; end RSEQ
        goal (M-DATE ?Self ?Other)
        plan-no-gen '(nil nil t t t)
        plan-comments '(if "ACTIVE-GOAL for M-DATE with self et person"
                        then "ACTIVE-GOAL for self et person to M-AGREE to"
                        "M-RESTAURANT with self et person and"
       "ACTIVE-GOAL for self et person to ENABLE-FUTURE-VPROX and"
                        "ACTIVE-GOAL for it to be FRIDAY-NIGHT and"
                        "ACTIVE-GOAL for self to be AT location of person and"
                        "ACTIVE-GOAL for M-RESTAURANT with self et person and"
       "ACTIVE-GOAL for self et person to be AT initial location of person and"
                        "ACTIVE-GOAL for self et person to M-KISS and"
       "ACTIVE-GOAL for self to be AT initial location of self")
        is 'plan-only
        plausibility 1.0))

; Todo: The problem is this will only work once for any given goal.
; We need to have a GC mechanism.
(define-rule Later-Plan (all lovers)
  (RULE subgoal ?Obj
        goal (LATER ?Obj)
        is 'plan-only
        plausibility 1.0))

(define-rule Enable-Future-Vprox-Plan1 (all lovers1 oseren-alone)
  (RULE subgoal (KNOW ?Person1 (TELNO (UAND ?Person2 (UDIST ?Person1))))
        goal (ENABLE-FUTURE-VPROX ?Person1 ?Person2)
        plan-no-gen '(t)
        inf-no-gen '(t)
        plan-comments '(if
                 "ACTIVE-GOAL for person1 et person2 to ENABLE-FUTURE-VPROX"
                        then "ACTIVE-GOAL for person1 to KNOW TELNO of person2")
        inf-comments '(if "person1 KNOW TELNO of person2"
                        then "person1 et person2 ENABLE-FUTURE-VPROX")
        plausibility 1.0))

(define-rule Enable-Future-Vprox-Plan2 (all lovers1 oseren-alone)
  (RULE subgoal (KNOW ?Person1 (ADDRESS (UAND ?Person2 (UDIST ?Person1))
                                        ?Location))
        goal (ENABLE-FUTURE-VPROX ?Person1 ?Person2)
        plan-no-gen '(t)
        inf-no-gen '(t)
        plan-comments '(if
                 "ACTIVE-GOAL for person1 et person2 to ENABLE-FUTURE-VPROX"
                        then
                 "ACTIVE-GOAL for person1 to KNOW ADDRESS of person2")
        inf-comments '(if "person1 KNOW ADDRESS of person2"
                        then "person1 et person2 ENABLE-FUTURE-VPROX")
        plausibility 1.0))

;(define-rule Enable-Future-Vprox-Plan3 (all lovers1 oseren-alone)
;  (RULE subgoal (RSEQ (ADDRESS ?Person2 ?Location)
;                      (KNOW ?Person1 ?Location))
;        goal (ENABLE-FUTURE-VPROX ?Person1 ?Person2)
;        plan-comments '(if
;              "ACTIVE-GOAL for person1 et person2 to ENABLE-FUTURE-VPROX"
;                        then
;              "ACTIVE-GOAL for person1 to KNOW ADDRESS of person2")
;        inf-comments '(if "person1 KNOW ADDRESS of person2"
;                        then "person1 et person2 ENABLE-FUTURE-VPROX")
;        plausibility 1.0))

;(define-rule Enable-Future-Vprox-Plan4 (all lovers1 oseren-alone)
;  (RULE subgoal (RSEQ (ADDRESS ?Person1 ?Location)
;                      (KNOW ?Person2 ?Location))
;        goal (ENABLE-FUTURE-VPROX ?Person1 ?Person2)
;        plan-comments '(if
;               "ACTIVE-GOAL for person1 et person2 to ENABLE-FUTURE-VPROX"
;                        then
;               "ACTIVE-GOAL for person2 to KNOW ADDRESS of person1")
;        inf-comments '(if "person2 KNOW ADDRESS of person1"
;                        then "person1 et person2 ENABLE-FUTURE-VPROX")
;        plausibility 1.0))

;(define-rule Repeat-Until-Plan1 (all)
;  (RULE subgoal (RSEQ ?obj1
;                     (REPEAT-UNTIL ?obj1 ?obj2))
;       goal (REPEAT-UNTIL ?obj1 ?obj2)))

;(define-rule Repeat-Until-Plan1 (all)
;  (RULE subgoal (RAND (ACTIVE-GOAL (REPEAT-UNTIL ?obj1 ?obj2))
;                     ?obj2)
;       goal (REPEAT-UNTIL ?obj1 ?obj2)
;       is 'inference-only))

;(define-rule M-Edate-Plan (all)
;  (RULE subgoal (RSEQ (OPTIONAL-WAIT)
;                     (AT ?Other ?Loc)
;                     (AT ?Self ?Loc)
;                     (M-KISS ?Self ?Loc)
;                     (M-DRINK ?Self ?Other)
;                     (M-MUSIC ?Self ?Other)
;                     (M-TELEVISION ?Self ?Other)
;                     (M-SEX ?Self ?Loc)
;                     (M-SLEEP ?Self ?Other))
;       goal (M-EDATE ?Self ?Other)))

(define-rule Friday-Night-Plan (all lovers1 oseren-alone lovers)
  (RULE subgoal (WAIT)
        goal (FRIDAY-NIGHT)
        plan-no-gen '(t)
        plan-comments '(if "ACTIVE-GOAL for it to be FRIDAY-NIGHT"
                        then "ACTIVE-GOAL to WAIT and suspend task")
        is 'plan-only
        halt? 't
        plausibility 1.0))

(define-rule At-Home-Inf (all lovers)
  (RULE subgoal (RAND (ACTIVE-GOAL (WAIT))
                      (AT (UAND ?Person (UDIST Me))
                          ?Location1)
                      (ADDRESS ?Person (UAND ?Location2
                                             (UDIST ?Location1))))
        goal (AT ?Person ?Location2)
        delete (AT ?Person ?Location1)
        inf-comments '(if "ACTIVE-GOAL to WAIT and"
                          "person is AT location1 and"
                          "ADDRESS of person is location2"
                          then "person is AT location2 and"
                          "delete person is AT location1")
        is 'inference-only
        plausibility 1.0))

(define-rule Know-Address-Loc-Inf (all lovers)
  (RULE subgoal (KNOW ?Self (ADDRESS ?Other ?Location))
        ; No, you are not seeing double! Two goals!
        goal (KNOW ?Self ?Location)
        goal (ADDRESS ?Other ?Location)
        is 'inference-only
        inf-comments '(if "self KNOW ADDRESS of person is location"
                          then "self KNOW location and"
                          "ADDRESS of person is location")
        plausibility 1.0))

(define-rule Night-Plan (unused)
  (RULE subgoal (WAIT)
        goal (NIGHT)
        plan-comments '(if "ACTIVE-GOAL for it to be NIGHT"
                        then "ACTIVE-GOAL to WAIT and suspend task")
        is 'plan-only
        halt? 't
        plausibility 1.0))

(define-rule Lovers-P-Goal1 (all lovers)
  (RULE subgoal (RAND (LOVERS ?Self ?Other)
                      (ROR (UAND ?Threat
                                 (M-SEX ?Other (UAND (NOT ?Self) ?Person)))
                           (UAND ?Threat
                                 (M-SEX ?Self (UAND (NOT ?Other) ?Person)))))
        goal (ACTIVE-GOAL (PRESERVATION (LOVERS ?Self ?Other) ?Threat))
        inf-comments '(if "self LOVERS with person and person have M-SEX with"
                          "someone else or self have M-SEX with someone"
                          "else"
                        then "ACTIVE-GOAL PRESERVATION on LOVERS")
        is 'inference-only
        plausibility 1.0))

(define-rule Lovers-P-Goal2 (all lovers)
  (RULE subgoal (RAND (LOVERS ?Self ?Other)
                      (RNOT (ROMANTIC-INTEREST ?Other)))
        goal (ACTIVE-GOAL (PRESERVATION (LOVERS ?Self ?Other)
                                        (NOT (ROMANTIC-INTEREST ?Other))))
        inf-comments '(if "self LOVERS with person and not ROMANTIC-INTEREST"
                          "in person"
                        then "ACTIVE-GOAL PRESERVATION on LOVERS")
        is 'inference-only
        plausibility 1.0))

(define-rule Lovers-P-Goal3 (all lovers)
  (RULE subgoal (RAND (LOVERS ?Self ?Other)
                      (RNOT (BELIEVE ?Other (ROMANTIC-INTEREST ?Self))))
        goal (ACTIVE-GOAL (PRESERVATION (LOVERS ?Self ?Other)
                                        (NOT (BELIEVE ?Other
                                                      (ROMANTIC-INTEREST
                                                       ?Self)))))
        inf-comments '(if "self LOVERS with person and person not have"
                          "ROMANTIC-INTEREST in self"
                        then "ACTIVE-GOAL PRESERVATION on LOVERS")
        is 'inference-only
        plausibility 1.0))

; Inappropriately is same as a similar rule for revenge?
(define-rule Rel-Failure1 (all lovers employment1)
  (RULE subgoal (RSEQ (UAND ?Pos-Relationship (NOTYPE actor ?Self actor ?Other))
                      (MTRANS ?Other ?Other ?Self
                              (BELIEVE ?Other
                                       (NOT (ACTIVE-GOAL ?Pos-Relationship)))))
        goal (FAILED-GOAL ?Pos-Relationship)
        delete ?Pos-Relationship
        plan-comments '(if
              "ACTIVE-GOAL for FAILED-GOAL of POS-RELATIONSHIP with person"
                        then
              "ACTIVE-GOAL for person to MTRANS to self that person"
              "has ACTIVE-GOAL not to be in POS-RELATIONSHIP with self")
        inf-comments '(if "person MTRANS to self that person has ACTIVE-GOAL"
                          "not to be in POS-RELATIONSHIP with self"
                        then "FAILED-GOAL of POS-RELATIONSHIP with person and"
                        "delete POS-RELATIONSHIP with person")
        plausibility 1.0))

(define-rule Rel-Failure2 (all lovers)
  (RULE subgoal (MTRANS ?Self ?Self ?Other
                        (BELIEVE ?Self
                                 (NOT (ACTIVE-GOAL (UAND ?Pos-Relationship
                                                         (NOTYPE ?Self
                                                                 ?Other))))))
        goal (FAILED-GOAL ?Pos-Relationship)
        plan-comments '(if
              "ACTIVE-GOAL for FAILED-GOAL of relationship with person"
                        then
              "ACTIVE-GOAL for self to MTRANS to person that self"
                        "has ACTIVE-GOAL not to be in relationship with person")
        inf-comments '(if "self MTRANS to person that self has ACTIVE-GOAL"
                          "not to be in relationship with person"
                        then "FAILED-GOAL of relationship with person")
        plausibility 1.0))

(define-rule Not-Lovers-Plan1 (all revenge1 mut4)
  (RULE subgoal (M-BREAK-UP ?Person) ; was ... (LOVERS ?Person ?Other)
        goal (BELIEVE ?Person (NOT (LOVERS ?Person))) ; was ... ?Other
        delete (LOVERS ?Person ?Other)
        initial (ROR (ACTIVE-GOAL (REVENGE))
                     (MUTATION))
        plan-comments '(if "ACTIVE-GOAL for person not to be LOVERS with anyone"
                           then "ACTIVE-GOAL for person to M-BREAK-UP")
        inf-comments '(if "person M-BREAK-UP"
                          then "person not LOVERS with anyone and"
                          "delete person LOVERS with someone")
        plausibility 1.0))

; Todo: A plan is needed for this to be MTRANSable.
(define-rule M-Break-Up-Plan1 (all revenge1 reversal1)
  (RULE subgoal (BELIEVE ?Person (M-SEX ?Other ?Person1))
        goal (M-BREAK-UP ?Person (LOVERS ?Person ?Other))
        top-level-goal (REVERSAL)
        is 'action-plan
        plan-comments '(if "ACTIVE-GOAL for person1 to M-BREAK-UP with person2"
                           then "ACTIVE-GOAL for person1 to BELIEVE person2"
                           "has M-SEX with person3")
        plausibility 1.0))

; Relax this instead? Actions aren't relaxed.
(define-rule M-Break-Up-Plan2 (all revenge1 reversal1 mut4)
  (RULE subgoal (RTRUE)
        goal (M-BREAK-UP ?Person)
        initial (ROR (ACTIVE-GOAL (REVENGE))
                     (MUTATION))
        is 'action-plan
        plan-comments '(if "ACTIVE-GOAL for person to M-BREAK-UP"
                           then "ACTIVE-GOAL for RTRUE")
        plausibility 1.0))

; The below are taken care of through need states now.
;(define-rule Rel-Maintain-P-Goal (all)
;  (RULE subgoal (UAND ?Pos-Relationship
;                    (NOTYPE actor ?Self ?Person))
;       goal (ACTIVE-GOAL (PRESERVATION ?Pos-Relationship
;                                       'regular-maintenance))
;       is 'inference-only))
;
;(define-rule Lovers-Maintenance-Plan1 (all)
;  (RULE subgoal (M-SEX ?Self ?Person)
;       goal (PRESERVATION (LOVERS ?Self ?Person)
;                          'regular-maintenance)))
;
;(define-rule Employment-Maintenance-Plan1 (all)
;  (RULE subgoal (M-WORK ?Self ?Person)
;       goal (PRESERVATION (EMPLOYMENT ?Self ?Person)
;                          'regular-maintenance)))

;
; End Lovers
;

(define-rule At-Plan (all lovers1 mut oseren-alone earthquake employment1
                      rationalization2 purchase patrol)
  (RULE subgoal (PTRANS ?Person ?Location1 ?Location2 ?Person)
        goal (AT ?Person ?Location2)
        delete (AT ?Person ?Location1)
        initial (AT ?Person ?Location1)
        inf-comments '(if "person PTRANS from location1 to location2"
                       then "person AT location2 and"
                       "delete person AT location1")
        plan-comments '(if "ACTIVE-GOAL for person to be AT location"
                        then "ACTIVE-GOAL for person to PTRANS to location")
        plan-no-gen '(t)
        plausibility 1.0))

; Paradigm for English descriptions.
(define-rule Sample-At-Plan (all diss)
  (RULE subgoal (PTRANS ?Person ?Location1 ?Location2 ?Person)
        goal (AT ?Person ?Location2)
        plan-comments '(if "ACTIVE-GOAL for person to be AT location"
                       then "ACTIVE-GOAL for person to PTRANS to location")
        is 'plan-only
        no-pp-all 't
        plausibility 1.0))

(define-rule Undo-Causes-Examp (all diss)
  (RULE subgoal (PTRANS)
        goal (AT)
        inf-comments '(if "person is RICH or ACTIVE-GOAL for person to"
                          "be RICH and"
                          "person is AT location or ACTIVE-GOAL for person"
                          "to be AT location and"
                          "self PTRANS to location or ACTIVE-GOAL for self"
                          "to PTRANS to location and"
                          "self MTRANS to person or ACTIVE-GOAL for self"
                          "to MTRANS to person and"
                          "ROMANTIC-INTEREST toward person or ACTIVE-GOAL"
                          "for ROMANTIC-INTEREST toward person"
                       then "ACTIVE-GOAL PRESERVATION UID.1")
        plan-comments '(if "ACTIVE-GOAL PRESERVATION UID.1"
                        then "ACTIVE-GOAL for self to be WELL-DRESSED")
        no-pp-all 't
        plausibility 1.0))

(define-rule Ptrans-Plan (all lovers1 mut oseren-alone earthquake employment1
                          purchase rationalization2)
  (RULE subgoal (KNOW ?Person ?Location2)
        goal (PTRANS ?Person ?Location1 ?Location2 ?Free-Obj)
        is 'action-plan
        plan-comments '(if "ACTIVE-GOAL for person to PTRANS to location"
                        then "ACTIVE-GOAL for person to KNOW location")
        plausibility 1.0))

; Believe other planning for the next clause...

(define-rule Believe-Plan1 (all lovers1 mut4 oseren-alone employment1-revenge
                            patrol)
  (RULE subgoal (MTRANS ?Person1 ?Person1 ?Person2 ?Mental-State)
        goal (BELIEVE ?Person2 (BELIEVE (UAND ?Person1 (UDIST ?Person2))
                                        ?Mental-State))
        plan-comments '(if "ACTIVE-GOAL for person to BELIEVE self mental state"
                       then "ACTIVE-GOAL to MTRANS mental state to person")
        inf-comments '(if "MTRANS mental state to person"
                       then "person BELIEVE self mental state")
        inf-no-gen '(t)
        plan-no-gen '(t)
        plausibility 0.9))

(define-rule Believe-Plan2 (all lovers1 mut4 oseren-alone employment1-revenge)
  (RULE subgoal (MTRANS ?Person1 ?Person1 ?Person2 (NOT ?Mental-State))
        goal (BELIEVE ?Person2 (BELIEVE (UAND ?Person1 (UDIST ?Person2))
                                        (NOT ?Mental-State)))
        inf-no-gen '(t)
        plan-no-gen '(t)
        plan-comments '(if
             "ACTIVE-GOAL for person to BELIEVE self NOT mental state"
                       then "ACTIVE-GOAL to MTRANS NOT mental state to person")
        inf-comments '(if "MTRANS NOT mental state to person"
                       then "person BELIEVE self NOT mental state")
        plausibility 0.9))

(define-rule Mtrans-Plan2 (all lovers1 mut oseren-alone employment1)
  (RULE subgoal (VPROX ?Person2 ?Person1)
        goal (MTRANS ?Person1 ?Person1 ?Person2
                     (UOR (UAND ?Mental-State (UOR (UPROC 'is-var?)
                                                   (UNOT ?Knowable)))
                          (NOT (UAND ?Mental-State
                                     (UOR (UPROC 'is-var?) (UNOT ?Knowable))))))
        is 'action-plan
        plan-comments '(if "ACTIVE-GOAL to MTRANS mental state to person"
                       then "ACTIVE-GOAL to be VPROX person")
        plausibility 1.0))

(define-rule Mtrans-Plan3 (all employment1)
  (RULE subgoal (RSEQ (POSS ?Person ?Phys-Obj)
                      (KNOW ?Phys-Obj (BELIEVE ?Other ?State)))
        goal (MTRANS ?Person ?Phys-Obj ?Person (BELIEVE ?Other ?State))
        is 'action-plan
        plan-comments '(if
                "ACTIVE-GOAL for person MTRANS from object to person that"
                           "person BELIEVE state"
                           then "ACTIVE-GOAL for person to POSS object and"
                "ACTIVE-GOAL for object to KNOW that person BELIEVE state")
        plausibility 1.0))

; Todo: In order for the (VPROX Movie-Star1 Karen) not to be relaxed in
; mut4, the below must be rewritten not to be in terms of ?Self
; (or add another rule).
(define-rule Vprox-Plan1 (all lovers1 mut oseren-alone employment1)
  (RULE subgoal (RSEQ (AT (UAND ?Person (UDIST ?Self)) ?Location)
                      (AT ?Self ?Location))
        goal (VPROX ?Self ?Person)
; Self must be first because unify does not backtrack.
        plan-comments '(if "ACTIVE-GOAL to be VPROX person"
                                then "ACTIVE-GOAL to be AT location of person")
        inf-comments '(if "AT location of person"
                               then "VPROX person")
        inf-no-gen '(t)
        plan-no-gen '(t t)
        plausibility 0.95))

; Check to make sure the below works.
(define-rule Vprox-Inf (all lovers1 mut oseren-alone)
  (RULE subgoal (RAND (AT ?Self ?Location)
                      (VPROX ?Self ?Other)
                      (AT ?Other (UNOT ?Location)))
        delete (VPROX ?Self ?Other)
        inf-comments '(if "self AT location and self VPROX person and person"
                          "not AT location"
                        then "delete VPROX self et person")
        is 'inference-only
        plausibility 1.0))

(define-rule Pos-Attitude-Plan1 (all mutx lovers)
  (RULE subgoal (RSEQ (BELIEVE ?Other (POS-ATTITUDE ?Object))
                      (POS-ATTITUDE ?Object))
        goal (POS-ATTITUDE ?Other)
        plan-comments '(if "ACTIVE-GOAL to have POS-ATTITUDE toward person"
                       then "ACTIVE-GOAL for person to have POS-ATTITUDE toward"
                       "something that self has POS-ATTITUDE toward")
        inf-comments '(if "person has POS-ATTITUDE toward something that"
                          "self has POS-ATTITUDE toward"
                       then "POS-ATTITUDE toward person")
        plausibility 0.6))

(define-initial-fact (all mutx) (POS-ATTITUDE (CHINESE-FOOD)))

(define-rule Pos-Attitude-Plan2 (all lovers1 oseren-alone)
  (RULE subgoal (BELIEVE ?Other (POS-ATTITUDE (MOVIES ?Self)))
        goal (POS-ATTITUDE ?Other)
        top-level-goal (LOVERS)
        plan-no-gen '(t)
        self-type ACTOR
        plan-comments '(if "ACTIVE-GOAL to have POS-ATTITUDE toward person"
                        then "ACTIVE-GOAL for person to have"
                             "POS-ATTITUDE toward self MOVIES")
        inf-comments '(if "person has POS-ATTITUDE toward self MOVIES"
                       then "POS-ATTITUDE toward person")
        plausibility 0.75))

; The below for reverse-alternatives.
(define-rule Pos-Attitude-Plan3 (all lovers1 oseren-alone)
  (RULE subgoal (BELIEVE ?Other (POS-ATTITUDE (CLOTHES ?Self)))
        goal (POS-ATTITUDE ?Other)
        top-level-goal (LOVERS)
        plan-no-gen '(t)
        plan-comments '(if "ACTIVE-GOAL to have POS-ATTITUDE toward person"
                        then "ACTIVE-GOAL for person to have"
                             "POS-ATTITUDE toward self clothes")
        inf-comments '(if "person has POS-ATTITUDE toward self clothes"
                       then "POS-ATTITUDE toward person")
        plausibility 0.7))

(define-rule Pos-Attitude-Plan4 (all mut4)
  (RULE subgoal (RSEQ (RICH ?Self)
                      (RICH ?Other))
        goal (POS-ATTITUDE ?Other)
        initial (RICH ?Self)
        is 'plan-only
        plan-comments '(if "ACTIVE-GOAL to have positive attitude toward person"
                           then "ACTIVE-GOAL for self to be RICH and"
                           "ACTIVE-GOAL for person to be RICH")
        plausibility 0.8))

(define-initial-fact (all lovers1 oseren-alone)
  (STAR Movie-Star1 'some-level))

(define-rule Romantic-Interest-Plan1 (all lovers1 mut oseren-alone)
  (RULE subgoal (RSEQ (ATTRACTIVE ?Other)
                      (POS-ATTITUDE ?Other))
        goal (ROMANTIC-INTEREST ?Other)
        top-level-goal (UNOT (REVENGE))
        plan-no-gen '(t t)
        plan-comments '(if "ACTIVE-GOAL to have ROMANTIC-INTEREST in person"
                       then "ACTIVE-GOAL to have POS-ATTITUDE toward person"
                       "and person to be ATTRACTIVE")
        inf-comments '(if "person ATTRACTIVE and have POSITIVE-ATTITUDE"
                          "toward person"
                       then "ROMANTIC-INTEREST in person")
        plausibility 0.9))

(define-rule Romantic-Interest-Inf (all lovers)
  (RULE subgoal (LOVERS ?Self ?Other)
        goal (BELIEVE ?Other (ROMANTIC-INTEREST ?Self))
        goal (BELIEVE ?Other (POS-ATTITUDE ?Self))
        is 'inference-only
        inf-comments '(if "LOVERS with person"
                          then "person has ROMANTIC-INTEREST toward self and"
                          "person has POS-ATTITUDE toward self")
        plausibility 0.9))

(define-rule Romantic-Interest-Plan2 (all revenge1)
  (RULE subgoal (RSEQ (STAR ?Self ?Level)
                      (STAR (UAND ?Other (UDIST ?Self))
                            (GREATER-THAN ?Level)))
        goal (ROMANTIC-INTEREST ?Other)
        top-level-goal (REVENGE)
        plan-comments '(if "ACTIVE-GOAL to have POS-ATTITUDE toward person"
                           "and self is STAR"
                        then "ACTIVE-GOAL for person to be greater STAR")
        inf-comments '(if "person is greater STAR than self"
                       then "POS-ATTITUDE toward person")
        plausibility 0.7))

(define-rule Star-Plan (all revenge1)
  (RULE subgoal (M-STUDY ?Self ACTOR)
        goal (STAR ?Self ?Level)
        plan-comments '(if "ACTIVE-GOAL for self to be STAR"
                        then "ACTIVE-GOAL for self to M-STUDY to be an ACTOR")
        inf-comments '(if "self M-STUDY to be an ACTOR"
                        then "self is STAR")
        is 'plan-only
        ; Otherwise, ?Level is unbound upon inf.
        plausibility 0.7))

(define-rule M-Study-Plan (all revenge1)
  (RULE subgoal (RTRUE)
        goal (M-STUDY ?Self ACTOR)
        is 'action-plan
        plan-comments '(if "ACTIVE-GOAL for self to M-STUDY to be an ACTOR"
                           then "ACTIVE-GOAL for RTRUE")
        plausibility 1.0))

;
; Roving
;

(define-rule Roving-Theme (all roving1)
  (RULE subgoal (DEPENDENCY ?Failed-Goal
                            (UAND ?Neg-Emotion
                                  (UPROC 'Greater-Rat-Activate?)))
        goal (ACTIVE-GOAL (ROVING))
        emotion ?Neg-Emotion (NEG-EMOTION strength 0.04)
        is 'inference-only
        inf-comments '(if "NEG-EMOTION of sufficient strength"
                          "resulting from a FAILED-GOAL"
                       then "ACTIVE-GOAL for ROVING")
        plausibility 1.0))

(define-rule Roving-Plan1 (all roving1)
  (RULE subgoal (RCODE '(lambda (goal context top-level-goal rule bd)
                          (roving-plan1 goal context top-level-goal rule bd)))
        goal (ROVING)
        is 'plan-only
        plan-comments '(if "ACTIVE-GOAL for ROVING"
                       then "recall pleasant episode")
        plausibility 1.0))

(setq *roving-episodes* nil)

(defun roving-plan1 (goal context top-level-goal rule bd)
  (ndbg-roman-nl *gate-dbg* rule "Roving plan for ~A in ~A" goal context)
  (let ((sprouted-context (cx$sprout context))
        (intends (ob$fcreate `(INTENDS linked-from ,goal
                                       linked-to (SUCCEEDED-GOAL
                                                  obj (RTRUE))
                                       rule Roving-Plan1))))
       (delay-dbgs sprouted-context
          (rule-fire-msg rule "coded plan" context bd sprouted-context goal)
          (cx$assert sprouted-context intends)
          (epmem-reminding (random-element *roving-episodes*) nil nil)
          (set-ordering sprouted-context 1.0)
          (ndbg-roman-nl *gate-dbg* rule-xtra "Done with roving-plan1")
          nil)
       (list sprouted-context)))

(defun coded-plan-template-plan1 (goal context top-level-goal rule bd)
  (ndbg-roman-nl *gate-dbg* rule "Your message here")
  (let ((sprouted-context (cx$sprout context))
        (intends (ob$fcreate `(INTENDS linked-from ,goal
                                       linked-to (SUCCEEDED-GOAL obj (RTRUE))
                                       rule Your-rule-here))))
       (delay-dbgs sprouted-context
          (rule-fire-msg rule "coded plan" context bd sprouted-context goal)
          (cx$assert sprouted-context intends)
          ; Your code here.
          nil)
       (list sprouted-context)))

;
; 'Rationalization'
;

(ob$create '(CITY "Los Angeles" obname Los-Angeles))

(ob$create '(CITY "Paris" obname Paris))

(define-initial-fact (rationalization1-alone)
  (FAILED-GOAL (LOVERS Me Movie-Star1)
               strength 0.9
               obname Failed-Lovers1))

(define-initial-fact (rationalization1-alone)
  (NEG-EMOTION strength 0.9 obname Upset1))

(define-initial-fact (rationalization1-alone)
  (DEPENDENCY Failed-Lovers1 Upset1 weight 1.0 offset 0.0))

; No longer needed with new PTRANS1
;(define-initial-fact (rationalization1) (KNOW Me Paris))
;(define-initial-fact (rationalization1) (KNOW Me Los-Angeles))
;(define-initial-fact (rationalization1) (BELIEVE Movie-Star1 (KNOW Movie-Star1 Paris)))

(define-rule Rationalization-Theme (all rationalization1 rationalization2
                                    rationalization3)
  (RULE subgoal (DEPENDENCY ?Failed-Goal
                            (UAND ?Neg-Emotion
;                                  (UNOT (NEG-EMOTION to ?To))
                                  (UPROC 'Greater-Rat-Activate?)))
        goal (ACTIVE-GOAL (RATIONALIZATION ?Failed-Goal))
;        delete (RATIONALIZATION ?Failed-Goal)
        emotion ?Neg-Emotion (NEG-EMOTION strength 0.06)
        is 'inference-only
        inf-comments '(if "NEG-EMOTION of sufficient strength"
                          "resulting from a FAILED-GOAL"
                       then "ACTIVE-GOAL for RATIONALIZATION"
                       "of failure")
        plausibility 1.0))

(defun greater-rat-activate? (emotion)
  (fl> (strength emotion) 0.7))

(defun less-rat-success? (emotion)
  (fl< (strength emotion) 0.3))

;(defun not-zero-strength? (emotion)
;  (not (fl= (strength emotion) 0.0)))

(define-rule Rationalization-Plan1 (all rationalization1)
  (RULE subgoal (LEADTO (SUCCEEDED-GOAL ?State)
                        (FAILED-GOAL))
        goal (RATIONALIZATION (UAND (FAILED-GOAL ?State)
                                    (UPROC 'Not-Inferred-Top-Level-Goal?)))
        is 'plan-only-no-auto
        plan-no-gen '(t)
        plan-comments '(if "ACTIVE-GOAL for RATIONALIZATION of failure"
                        then "ACTIVE-GOAL for success to LEADTO"
                             "failure")
        plausibility 0.99))

(define-rule Rationalization-Inf1 (all rationalization1 rationalization2
                                   rationalization3)
  (RULE subgoal (RAND (ACTIVE-GOAL (RATIONALIZATION ?Failed-Goal))
                      (DEPENDENCY ?Failed-Goal
                                  (UOR ?Pos-Emotion
                                       (UAND ?Neg-Emotion
                                             (UPROC 'Less-Rat-Success?)))))
        goal (RATIONALIZATION ?Failed-Goal)
        is 'inference-only
        reality-subgoal 'T
        inf-comments '(if "NEG-EMOTION associated with failure less than"
              "a certain strength or POS-EMOTION associated with failure"
                       then "RATIONALIZATION of failure")
        plausibility 1.0))

(define-rule Rationalization-Plan2 (all rationalization2)
  (RULE subgoal (LEADTO (FAILED-GOAL ?State)
                        (SUCCEEDED-GOAL))
        goal (RATIONALIZATION (UAND (FAILED-GOAL ?State)
                                    (UPROC 'Not-Inferred-Top-Level-Goal?)))
        is 'plan-only-no-auto
        plan-no-gen '(t)
        plan-comments '(if "ACTIVE-GOAL for RATIONALIZATION of failure"
                                then "ACTIVE-GOAL for failure to LEADTO"
                             "success")
        plausibility 0.98))

(define-rule Leadto-Plan1 (all rationalization1)
  (RULE subgoal (RCODE '(lambda (goal context top-level-goal rule bd)
                          (leadto-plan1 goal context top-level-goal rule
                                        bd)))
        goal (LEADTO (SUCCEEDED-GOAL ?State)
                     (FAILED-GOAL))
        is 'plan-only-no-auto
        plan-comments '(if "ACTIVE-GOAL for success to LEADTO failure"
                       then "hypothesize success et explore consequences")
        plausibility 1.0))

(defun leadto-plan1 (goal context top-level-goal rule bd)
  (leadto-plan1a goal context top-level-goal rule bd (var-value '?State)))

(defun leadto-plan1a (goal context top-level-goal rule bd state)
  (ndbg-roman-nl *gate-dbg* rule "Leadto plan for ~A in ~A" goal context)
  (let ((sprouted-context (cx$sprout context))
        (intends (ob$fcreate `(INTENDS linked-from ,goal
                                       linked-to (SUCCEEDED-GOAL obj (RTRUE))
                                       rule Leadto-Plan1))))
       (delay-dbgs sprouted-context
          (rule-fire-msg rule "coded plan" context bd sprouted-context goal)
          (set-ordering sprouted-context 1.0)
          (set-altern sprouted-context)
          (gen-future-assumption
           (no-gen (cx$assert sprouted-context intends))
           (cx$assert sprouted-context state)
           (no-gen (cx$assert sprouted-context
                              (ob$fcreate `(SUCCEEDED-GOAL obj ,state)))))
          (ob$set sprouted-context 'gen-switches '((tense conditional)))
          ; conditional tense is used for future.
          (ndbg-roman-nl *gate-dbg* rule-xtra
           "Run inferences for leadto state change")
          (run-inferences sprouted-context top-level-goal *me-belief-path*)
          (ndbg-roman-nl *gate-dbg* rule-xtra "Done with leadto-plan1")
          nil)
       (list sprouted-context)))

; Could have a corresponding UWLPATH Leadto-Inf1, but for rationalization
; to succeed we don't really need it:
(define-rule Leadto-Inf1 (unused)
  (RULE subgoal (RAND (UAND ?Failed-Goal (FAILED-GOAL ?State))
                      (UAND ?Succeeded-Goal
                            (SUCCEEDED-GOAL obj (UWLPATH ?State DEPENDENCY
                                                         Forward))))
        goal (LEADTO ?Succeeded-Goal ?Failed-Goal)
        is 'inference-only
        plausibility 1.0))

(define-rule Other-Rule1 (all rationalization1)
  (RULE subgoal (LOVERS ?Self ?Other)
        goal (OTHER ?Other)
        is 'inference-only
        inf-comments '(if "LOVERS with person"
                       then "initiate forward vicarious planning for person")
        plausibility 1.0))

; Hard to write this for other mode, since it will fire no matter what
; since RNOT will be true of any negation and it fires on Me.
(define-rule Acting-Job-Theme (all rationalization1)
  (RULE subgoal (RAND (OTHER ?Actor)
                      (RNOT (ACTING-EMPLOY actor ?Actor actor ?:Organization)))
        goal (BELIEVE ?Actor (ACTIVE-GOAL (ACTING-EMPLOY actor ?Actor
                                                      actor ?:Organization)))
        is 'inference-only
        inf-comments '(if "actor does not have an ACTING-EMPLOY"
                       then "actor has ACTIVE-GOAL to have an ACTING-EMPLOY")
        plausibility 1.0))

; Is substituted by episode.
(define-rule Acting-Job-Plan (unused)
  (RULE subgoal (RPROX ?Self Paris)
        goal (ACTING-EMPLOY actor ?Self actor ?Other)
        is 'plan-only
        plausibility 1.0))

(ob$create '(ORGANIZATION obname My-Job name "the May Company"))

(ob$create '(FEMALE-PERSON obname My-Boss first-name "Agatha"))

(define-initial-fact (all employment1)
  (AT My-Boss (LOCATION name "the May Company" obname May-Company-Loc)))

(define-initial-fact (all employment1)
  (KNOW Me May-Company-Loc))

(define-phrase "my boss fires me."
  (MTRANS My-Boss My-Boss Me
          (BELIEVE My-Boss
                   (NOT (ACTIVE-GOAL (EMPLOYMENT actor Me actor My-Boss
                                                 organization My-Job))))))

(define-initial-fact (all rationalization1 employment1)
  (EMPLOYMENT actor Me actor My-Boss organization My-Job))

(define-initial-fact (all rationalization1)
  (RPROX My-Job Los-Angeles))

(define-initial-fact (all rationalization1 earthquake)
  (RPROX Me Los-Angeles))

(define-initial-fact (all rationalization1) (RPROX Movie-Star1 Los-Angeles))

(define-initial-fact (all rationalization1) (BELIEVE Movie-Star1
                                   (RPROX Movie-Star1 Los-Angeles)))

(define-rule Rprox-plan (all rationalization1)
  (RULE subgoal (PTRANS1 ?Person ?City1 ?City2 ?Person)
        goal (RPROX ?Person ?City2)
        delete (RPROX ?Person ?City1)
        initial (RPROX ?Person ?City1)
        plan-no-gen '(t)
        plan-comments '(if "ACTIVE-GOAL to be RPROX city"
                        then "ACTIVE-GOAL to PTRANS1 to city")
        inf-comments '(if "PTRANS1 from city1 to city2"
                       then "RPROX city2 and delete RPROX"
                            "city1")
        plausibility 1.0))

(define-rule Ptrans1-Plan (all rationalization1)
  (RULE subgoal (RTRUE)
        goal (PTRANS1 ?Person ?City1 ?City2 ?Person)
        is 'action-plan
       plan-comments '(if "ACTIVE-GOAL to PTRANS1 to city"
                      then "ACTIVE-GOAL for RTRUE")
        plausibility 1.0))

(define-rule Job-Failure (all rationalization1)
  (RULE subgoal (RAND (EMPLOYMENT actor ?Self actor ?Other
                                  organization ?Organization)
                      (RPROX ?Organization ?City)
                      (RNOT (RPROX ?Self ?City)))
        goal (FAILED-GOAL (EMPLOYMENT actor ?Self actor ?Other
                                      organization ?Organization
                                      strength 0.4))
; 0.4 for now so that rationalization doesn't succeed on this
; alone (later with addition of reality it should be 0.85)
        is 'inference-only
        inf-comments '(if "have EMPLOYMENT with organization which"
                          "is RPROX city and self is not"
                          "RPROX city"
                       then "FAILED-GOAL of EMPLOYMENT with organization")
        plausibility 0.9))

; Why is the above a goal failure and this an active p-goal? This is
; what we mean by 'ad hoc'.
(define-rule Lovers-P-Goal (all rationalization1)
  (RULE subgoal (RAND (LOVERS ?Self ?Other)
                      (RPROX ?Other ?City)
                      (RNOT (RPROX ?Self ?City)))
        goal (ACTIVE-P-GOAL (LOVERS ?Self ?Other))
        is 'inference-only
        inf-comments '(if "LOVERS with person who"
                      "is RPROX city and self is not"
                      "RPROX city"
                   then "ACTIVE-P-GOAL of LOVERS with person")
        plausibility 1.0))

;
; Rationalization3
;

(define-initial-fact (all rationalization3) (WEARING Me (NECKLACE)))

(define-rule Well-Dressed-Plan2 (all rationalization3)
  (RULE subgoal (WEARING ?Self (NECKLACE))
        goal (WELL-DRESSED ?Self)
        inf-no-gen '(t)
        inf-comments '(if "self WEARING necklace"
                          then "self is WELL-DRESSED")
        is 'inference-only
        plausibility 0.1))

(define-rule Rationalization-Plan3 (all rationalization3)
  (RULE subgoal (MINIMIZATION ?Failed-Goal)
        goal (RATIONALIZATION (UAND ?Failed-Goal
                                    (UPROC 'Inferred-Top-Level-Goal?)))
        is 'plan-only-no-auto
        plan-no-gen '(t)
        plan-comments '(if "ACTIVE-GOAL for RATIONALIZATION of failure"
                           then "ACTIVE-GOAL for MINIMIZATION of failure")
        plausibility 0.98))

(defun inferred-top-level-goal? (x)
  (neq? x (ob$get x 'top-level-goal)))

(defun not-inferred-top-level-goal? (x)
  (eq? x (ob$get x 'top-level-goal)))

(define-rule Minimization-Plan (all rationalization3)
  (RULE subgoal (RCODE '(lambda (goal context top-level-goal rule bd)
                          (minimization-plan goal context top-level-goal rule
                        bd)))
        goal (MINIMIZATION ?Failed-Goal)
        is 'plan-only-no-auto
        plan-comments '(if "ACTIVE-GOAL for MINIMIZATION of failure"
                       then "negate et justify antecedents of failure")
        plausibility 1.0))

(defun minimization-plan (goal context top-level-goal rule bd)
  (minimization-plana goal context top-level-goal rule bd
                      (var-value '?Failed-Goal)))

(defun minimization-plana (goal context top-level-goal rule bd failed-goal)
  (ndbg-roman-nl *gate-dbg* rule "Minimization plan for ~A in ~A" goal context)
  (let ((sprouted-context (cx$sprout context))
        (intends (ob$fcreate `(INTENDS linked-from ,goal
                                       linked-to (SUCCEEDED-GOAL obj (RTRUE))
                                       rule Minimization-Plan))))
       (delay-dbgs sprouted-context
          (rule-fire-msg rule "coded plan" context bd sprouted-context goal)
          (set-ordering sprouted-context 1.0)
          (no-gen (cx$assert sprouted-context intends))
          (if (null? (minimize failed-goal context))
              (setq sprouted-context nil)) ; not quite right?
          (ndbg-roman-nl *gate-dbg* rule-xtra "Done with minimization-plan")
          nil)
       (if sprouted-context
           (list sprouted-context)
           nil)))

(setq *minim-negated-att-str* 0.7)

(defun minimize (failed-goal context)
  (yloop
   (initial (retrieved nil) (fact-negation nil)
            (result nil))
   (yfor fact in (get-leaf-causes failed-goal context))
   (ydo
    (setq fact-negation nil)
    (cond
     ; Fixed the below.
     ((setq retrieved (cx$retrieve context (negation-of fact)))
      (setq result t)
      (setq fact-negation (car (car retrieved)))
      (minimize-rescale fact fact-negation context)
      ; E.g., "Anyway, I was well dressed because I was wearing a necklace."
      ; Todo: this inf doesn't fire?
      (generate1 fact-negation
                 (cons '(justify? t)
                       (cons '(tongue-in-cheek t)
                             (cons '(tense past) *global-switches*)))
                 context *me-belief-path*))
     ((ty$instance? fact 'attitude)
      (setq result t)
      (setq fact-negation (negate-attitude fact))
      (set-strength fact-negation *minim-negated-att-str*)
      (minimize-rescale fact fact-negation context)
      ; E.g., "Anyway, I don't think much of him."
      (generate1 fact-negation (cons '(tongue-in-cheek t) *global-switches*)
                 context *me-belief-path*)))
    ; E.g., "I feel only slightly embarrassed."
    (if fact-negation
        (yloop
         (yfor emotion in (get-result-emotions fact context))
         ; Todo: above must be done in *reality*? Will it work?
         (ydo 
          (generate1 emotion *global-switches* context
                     *me-belief-path*)))))
   (yresult result)))

(defun negate-attitude (fact)
  (if (ty$instance? fact 'pos-attitude)
      (ob$fcreate `(NEG-ATTITUDE obj ,(ob$get fact 'obj)))
      (ob$fcreate `(POS-ATTITUDE obj ,(ob$get fact 'obj)))))

(defun minimize-rescale (fact fact-negation context)
  (let ((total-strength (fl+ (strength fact) (strength fact-negation))))
       (hyper-set-strength fact (fl/ (strength fact) total-strength)
                           context)
       (hyper-set-strength fact-negation (fl/ (strength fact-negation)
                                              total-strength)
                           context)))

;
; 'Revenge'
;
; Note: ROMANTIC-INTEREST will come from a relaxed RICH goal.
;

(define-initial-fact (revenge1-alone) (FAILED-GOAL (LOVERS Me Movie-Star1)
                                                    obname Failed-Lovers1))

(define-initial-fact (revenge1-alone)
  (NEG-EMOTION to Movie-Star1 obname Anger1))

(define-initial-fact (revenge1-alone) (DEPENDENCY Failed-Lovers1 Anger1))

(define-rule Revenge-Theme (all revenge1 employment1-revenge)
  (RULE subgoal (DEPENDENCY ?Failed-Goal (UAND ?Neg-Emotion
                                               (NEG-EMOTION to ?Other)))
        goal (ACTIVE-GOAL (REVENGE ?Self ?Other ?Failed-Goal))
        emotion ?Neg-Emotion (NEG-EMOTION strength 0.05)
        is 'inference-only
        inf-comments '(if "NEG-EMOTION toward person resulting from"
                          "a FAILED-GOAL"
                       then "ACTIVE-GOAL to gain REVENGE"
                       "against person")
        plausibility 1.0))

; Below doesn't work as inference yet.
(define-rule Revenge-Plan1 (all revenge1 employment1-revenge)
  (RULE subgoal (RSEQ (BELIEVE ?Other (ACTIVE-GOAL ?Pos-Relationship))
                      (MTRANS ?Other ?Other ?Self
                              (BELIEVE ?Other (ACTIVE-GOAL ?Pos-Relationship)))
                      (BELIEVE ?Other (FAILED-GOAL ?Pos-Relationship)))
        goal (REVENGE ?Self ?Other (FAILED-GOAL ?Pos-Relationship))
        plan-no-gen '(nil t t)
        is 'plan-only
        plan-comments '(if "ACTIVE-GOAL to gain REVENGE against person for"
                           "causing self a failed POS-RELATIONSHIP goal"
                       then "ACTIVE-GOAL for person to have failure of"
                       "same POS-RELATIONSHIP")
        plausibility 1.0))

; Below clashes with normal planning mechanisms? Also, it's a bit weird
; from an inferencing standpoint.
(define-rule Failed-Rel-Goal-Plan1 (all revenge1 employment1-revenge)
  (RULE subgoal (RSEQ (ACTIVE-GOAL ?Pos-Relationship)
                      (BELIEVE ?Other (NOT (ACTIVE-GOAL ?Pos-Relationship))))
        goal (FAILED-GOAL (UAND ?Pos-Relationship (NOTYPE actor ?Self
                                                          actor ?Other)))
        is 'plan-only
        plan-no-gen '(t nil)
        plan-comments '(if "ACTIVE-GOAL for person to have FAILED-GOAL of"
                           "POS-RELATIONSHIP"
                       then "ACTIVE-GOAL for person to have ACTIVE-GOAL of"
              "POS-RELATIONSHIP with person and then BELIEVE that person"
                       "does not have ACTIVE-GOAL of POS-RELATIONSHIP")
        plausibility 1.0))

; The same as above without needed believe other mode.
(comment
(define-rule Failed-Lovers-Goal-Plan1 (all revenge1)
  (RULE subgoal (RSEQ (BELIEVE ?Other (ACTIVE-GOAL (LOVERS ?Self ?Other)))
                      (BELIEVE ?Other
                               (BELIEVE ?Self
                                        (NOT (ACTIVE-GOAL
                                              (LOVERS ?Self ?Other))))))
        goal (BELIEVE ?Other (FAILED-GOAL (LOVERS ?Self ?Other)))
        delete (BELIEVE ?Other (ACTIVE-GOAL (LOVERS ?Self ?Other)))
        is 'plan-only
        plausibility 1.0))
)

(define-initial-fact (all revenge1) (MOVIE-STAR Movie-Star1))

(define-rule Lovers-Default-Inf (unused) ; was revenge1--try without....
                                      ; cause this fires spuriously later
  (RULE subgoal (RAND (MOVIE-STAR ?Person)
                      (RNOT (NOT (LOVERS ?Person ?Other)))
                      (RNOT (LOVERS ?Person ?Other)))
        goal (LOVERS ?Person (PERSON))
        is 'inference-only
        inf-comments '(if "person is movie star and person is not"
                          "known to be LOVERS with anyone and not"
                          "known NOT to be LOVERS with anyone"
                       then "assume person is LOVERS with someone")
        plausibility 0.8))

;(define-rule Belief-Pers-Attr (all lovers1 revenge1)
;  (RULE subgoal (UAND ?Personal-Attribute (NOTYPE actor ?Person))
;        goal (BELIEVE ?Person ?Personal-Attribute)
;        plausibility 1.0))

(define-rule Belief-Pers-Attr-Plan1 (all lovers1 revenge1)
  (RULE subgoal (STAR ?Person ?Level)
        goal (BELIEVE ?Person (STAR ?Person ?Level))
        inf-comments '(if "person is STAR"
                       then "person BELIEVE person is STAR")
        inf-no-gen '(t)
        plausibility 1.0))

(define-rule Belief-Pers-Attr-Plan2 (all mut4)
  (RULE subgoal (RICH ?Person)
        goal (BELIEVE ?Person (RICH ?Person))
        inf-comments '(if "person is RICH"
                       then "person BELIEVE person is RICH")
        inf-no-gen '(t)
        plausibility 1.0))

(define-rule Belief-Pers-Attr-Plan3 (all lovers1 revenge1)
  (RULE subgoal (STAR ?Person2 ?Level)
        goal (BELIEVE ?Person1 (STAR (UAND ?Person2 (UDIST ?Person1))
                                     ?Level))
        is 'plan-only
        plan-comments '(if "ACTIVE-GOAL for person1 to BELIEVE that person2"
                           "is STAR"
                           then "ACTIVE-GOAL for person2 to be STAR")
        plausibility 1.0))

(define-rule Belief-Pers-Attr-Plan4 (all mut4)
  (RULE subgoal (ATTRACTIVE ?Person2)
        goal (BELIEVE ?Person1 (ATTRACTIVE (UAND ?Person2 (UDIST ?Person1))))
        is 'plan-only
        plan-comments '(if "ACTIVE-GOAL for person1 to BELIEVE that person2"
                           "is ATTRACTIVE"
                           then "ACTIVE-GOAL for person2 to be ATTRACTIVE")
        plausibility 1.0))

(define-rule Belief-Pers-Attr-Plan5 (all mut4)
  (RULE subgoal (RICH ?Person2)
        goal (BELIEVE ?Person1 (RICH (UAND ?Person2 (UDIST ?Person1))))
        is 'plan-only
        plan-comments '(if "ACTIVE-GOAL for person1 to BELIEVE that person2"
                           "is RICH"
                           then "ACTIVE-GOAL for person2 to be RICH")
        plausibility 1.0))

(define-rule Know-Telno-Rule1 (all revenge1) ; was lovers1?
  (RULE subgoal (RSEQ (STAR (UAND ?Other (UDIST ?Self))
                            ?Level1)
                      (STAR ?Self ?Level2))
        goal (KNOW ?Self (TELNO ?Other))
        inf-comments '(if "person is STAR and self is STAR"
                       then "KNOW TELNO of person")
        is 'inference-only
        plausibility 0.8))

(define-rule Know-Telno-Rule2 (all lovers1 revenge1)
  (RULE subgoal (RSEQ (STAR (UAND ?Other (UDIST ?Self))
                            ?Level1)
                      (STAR ?Self ?Level2))
        goal (KNOW ?Other (TELNO ?Self))
        inf-comments '(if "person is STAR and self is STAR"
                       then "person KNOW TELNO of self")
        is 'inference-only
        plausibility 0.8))

(define-rule Know-Telno-Rule3 (all mut4)
  (RULE subgoal (RSEQ (RICH ?Person1)
                      (RICH ?Person2))
        goal (KNOW ?Person1 (TELNO ?Person2))
        plan-comments '(if "ACTIVE-GOAL for person1 to KNOW TELNO of person2"
                       then "ACTIVE-GOAL for person1 to be RICH and"
                       "ACTIVE-GOAL for person2 to be RICH")
        is 'plan-only ; else use UDIST in subgoal
        initial (RICH ?Person1)
        plausibility 0.8))

(comment
; who uses this rule?
(define-rule Objective-Attr (all lovers1 mut)
  (RULE subgoal ?Attribute
        goal (BELIEVE ?Person1
                      (UAND ?Attribute (NOTYPE actor ?Person2)))
        is 'plan-only
        plausibility 1.0))
)

;
; Job revenge
;

(define-rule Employment-Theme-Plan (all employment1-revenge employment1)
  (RULE subgoal (POS-ATTITUDE (EMPLOYMENT actor ?Self actor ?Other
                                          organization ?Organization))
        goal (ACTIVE-GOAL (EMPLOYMENT actor ?Self actor ?Other
                                      organization ?Organization))
        plan-comments '(if "ACTIVE-GOAL for ACTIVE-GOAL of EMPLOYMENT"
                           "of person"
                           then "ACTIVE-GOAL for POS-ATTITUDE toward"
                           "EMPLOYMENT of person")
        is 'plan-only
        plausibility 1.0))

(define-rule Pos-Att-Employ-Plan1 (all employment1-revenge employment1)
  (RULE subgoal (EXECUTIVE ?Other)
        goal (POS-ATTITUDE (EMPLOYMENT actor (UAND ?Self (UDIST Me))
                                       actor ?Other
                                       organization ?Organization))
        top-level-goal (REVENGE)
        plan-comments '(if "ACTIVE-GOAL for POS-ATTITUDE toward EMPLOYMENT"
                           "of person"
                           then "ACTIVE-GOAL for person to be EXECUTIVE")
        is 'plan-only
        plausibility 1.0))

(define-rule Revenge-Plan2 (all employment1-revenge patrol)
  (RULE subgoal (BELIEVE ?Other (FAILED-GOAL obj (NOT (HURT ?Other))))
        goal (REVENGE ?Self ?Other ?Anything)
        plan-no-gen '(t)
        is 'plan-only
        plan-comments '(if "ACTIVE-GOAL for REVENGE against person"
                           then "ACTIVE-GOAL for person to have FAILED-GOAL"
                           "of NOT being HURT")
        plausibility 1.0))


(define-rule Believe-Hurt-Plan (all employment1-revenge patrol)
  (RULE subgoal (HURT ?Person)
        goal (BELIEVE ?Person (FAILED-GOAL obj (NOT (HURT ?Person))))
        plan-comments '(if "ACTIVE-GOAL for person to have FAILED-GOAL"
                           "of NOT being HURT"
                           then "ACTIVE-GOAL for person to be HURT")
        inf-comments '(if "person is HURT"
                          then "person have FAILED-GOAL"
                          "of NOT being HURT")
        plausibility 0.9))

(define-rule Hurt-Plan (all employment1-revenge)
  (RULE subgoal (M-BEAT-UP Me ?Person)
        goal (HURT (UAND ?Person (UDIST ?Person Me)))
        plan-comments '(if "ACTIVE-GOAL for person to be HURT"
                           then "ACTIVE-GOAL for me to M-BEAT-UP person")
        inf-comments '(if "me M-BEAT-UP person"
                          then "person HURT")
        plausibility 0.9))

(define-rule Hurt-Plan (all patrol)
  (RULE subgoal (M-BEAT-UP Me ?Person)
        goal (HURT ?Person)
        plan-comments '(if "ACTIVE-GOAL for person to be HURT"
                           then "ACTIVE-GOAL for me to M-BEAT-UP person")
        inf-comments '(if "me M-BEAT-UP person"
                          then "person HURT")
        plausibility 0.9))

;
; 'Reversal'
;

; Takes advantage of rep invariant: If a dependency is asserted, so
; are its components (the items to which it is linked).
;
; Supplemental emotions will be used to order control goals once
; they are all in.
(define-rule Reversal-Theme (all reversal)
  (RULE subgoal (RAND (DEPENDENCY (UAND ?Failed-Goal
                                        (UPROC 'Inferred-Top-Level-Goal?))
                                  ; For now, only do inferred tlgfs.
                                  (UAND ?Neg-Emotion
                                        (UPROC 'Greater-Reversal-Activate?)
                                        (UPROC 'Emot-Less-Learn-Thresh?)))
;                     (RNOT
;                      (DEPENDENCY (UAND ?:Failed-Goal
;                                        (UDIST ?Failed-Goal))
;                                  (UAND ?:Neg-Emotion
;                                        (UPROC 'Greater-Reversal-Activate?))))
                      ) ; end RAND
        goal (ACTIVE-GOAL (REVERSAL ?Failed-Goal))
        emotion ?Neg-Emotion (NEG-EMOTION strength 0.03)
        is 'inference-only
        inf-comments '(if "NEG-EMOTION resulting from a FAILED-GOAL"
                       then "ACTIVE-GOAL for REVERSAL"
                            "of failure")
        plausibility 1.0))

(define-rule Reversal-Theme2 (unused)
  (RULE subgoal (RAND (DEPENDENCY ?Failed-Goal1
                                  (UAND ?Neg-Emotion1
                                        (UPROC 'Greater-Reversal-Activate?)
                                        (UPROC 'Emot-Less-Learn-Thresh?)))
                      (DEPENDENCY (UAND ?Failed-Goal2
                                        (UDIST ?Failed-Goal1))
                                  (UAND ?Neg-Emotion2
                                        (UPROC 'Greater-Reversal-Activate?))))
        goal (ACTIVE-GOAL (REVERSAL obj ?Failed-Goal1
                                    obj ?Failed-Goal2))
        emotion ?Neg-Emotion1 ?Neg-Emotion2 (NEG-EMOTION strength 0.03)
        is 'inference-only
        inf-comments '(if "NEG-EMOTIONs resulting from a FAILED-GOALs"
                       then "activate daydreaming goal for REVERSAL"
                            "of failures")
        plausibility 1.0))

(defun greater-reversal-activate? (emotion)
  (fl> (strength emotion) 0.5))

(setq *learn-thresh* 2.0)

(defun emot-less-learn-thresh? (x)
  (fl< (abs *overall-emotional-state*) 2.0))

; Add to rule checking: a rule with a coded subgoal must be a plan-only.
(define-rule Reversal-Plan (all reversal)
  (RULE subgoal (RCODE '(lambda (goal context top-level-goal rule bd)
                          (do-reversal goal context
                                       top-level-goal rule bd)))
        goal ?Reversal
        is 'plan-only
        plan-comments '(if "ACTIVE-GOAL for REVERSAL of failure"
                       then "attempt various REVERSAL strategies") 
        plausibility 1.0))

(defun do-reversal (goal context top-level-goal rule bd)
  (reversal goal (var-value '?Reversal) context top-level-goal rule bd))

; This could be a plan too I guess.
(define-rule Neg-Attitude-Inf (all lovers1)
  (RULE subgoal (RAND (RICH ?Other)
                      (AT ?Other ?Location)
                      (AT ?Self ?Location)
                      (MTRANS ?Self ?Self ?Other ?Anything)
                      (RNOT (WELL-DRESSED ?Self)))
        goal (BELIEVE ?Other (NEG-ATTITUDE ?Self))
        is 'inference-only
        inf-comments '(if "person is RICH and self is AT same location"
                          "as person and self not WELL-DRESSED"
                       then "person has NEG-ATTITUDE toward self")
        plausibility 0.9))

(define-initial-fact (all lovers1) (POS-ATTITUDE Movie-Star1))
(define-initial-fact (all lovers1 test-fop) (RICH Movie-Star1))

(define-rule Social-Esteem-Failure (all lovers1)
  (RULE subgoal (RAND (BELIEVE ?Other (NEG-ATTITUDE ?Self))
                      (ROMANTIC-INTEREST ?Other)) ; was POS-ATTITUDE
        ; It has to be ROMANTIC-INTEREST so that Reversal works right.
        goal (FAILED-GOAL (BELIEVE ?Other (POS-ATTITUDE ?Self))
                          strength 0.7)
        is 'inference-only
        inf-no-gen '(t)
        inf-comments '(if "person has NEG-ATTITUDE toward self and"
                          "self has ROMANTIC-INTEREST toward person"
                       then "FAILED-GOAL of SOCIAL-ESTEEM goal for person"
                       "to have POS-ATTITUDE toward self")
        plausibility 1.0))

(define-initial-fact (all always) (AT (FASHIONABLE-CLOTHES obname Chic-Outfit1)
                                      Home))

(define-rule Well-Dressed-Plan1 (all lovers1)
  (RULE subgoal (WEARING ?Self ?Fashionable-Clothes)
        goal (WELL-DRESSED ?Self)
        plan-no-gen '(t)
        inf-no-gen '(t)
        plan-comments '(if "ACTIVE-GOAL to be WELL-DRESSED"
                       then "ACTIVE-GOAL to be WEARING FASHIONABLE-CLOTHES")
        plausibility 1.0))

(define-rule Wearing-Plan (all lovers1 rain)
  (RULE subgoal (M-PUTON ?Self ?Clothes)
        goal (WEARING ?Self ?Clothes)
        plan-no-gen '(t)
        inf-no-gen '(t)
        plan-comments '(if "ACTIVE-GOAL to be WEARING clothes"
                       then "ACTIVE-GOAL to M-PUTON clothes")
        plausibility 1.0))

(define-initial-fact (all always) (AT (RAINCOAT obname Raincoat1) Home))

(define-rule M-Puton-Plan (all lovers1 rain)
  (RULE subgoal (POSS ?Self ?Clothes)
        goal (M-PUTON ?Self ?Clothes)
        is 'action-plan
        plan-comments '(if "ACTIVE-GOAL to M-PUTON clothes"
                           then "ACTIVE-GOAL for self to POSS clothes")
        plausibility 1.0))

(define-rule Poss-Clothes-Plan (unused)
  (RULE subgoal (M-PURCHASE ?Self (FASHIONABLE-CLOTHES) ?Store)
        goal (POSS ?Self (FASHIONABLE-CLOTHES))
        plan-comments '(if "ACTIVE-GOAL for self to POSS FASHIONABLE-CLOTHES"
                        then "ACTIVE-GOAL for self to M-PURCHASE from store")
        inf-comments '(if "self M-PURCHASE FASHIONABLE-CLOTHES from store"
                        then "self POSS FASHIONABLE-CLOTHES")
        plausibility 1.0))

;
; Earthquake example
;

(define-rule Hurt-Inf (all earthquake)
  (RULE subgoal (RAND (RNOT (UNDER-DOORWAY ?Self))
                      (RPROX ?Self ?City)
                      (EARTHQUAKE ?City))
        goal (HURT ?Self)
        is 'inference-only
        inf-comments '(if "self not UNDER-DOORWAY and"
                          "self RPROX city and EARTHQUAKE in city"
                       then "self is HURT")
        plausibility 0.9))

(define-rule Not-Hurt-Failure (all earthquake employment1-revenge)
  (RULE subgoal (HURT ?Self)
        goal (FAILED-GOAL (NOT (HURT ?Self))
                          strength 0.9)
        inf-comments '(if "self is HURT"
                       then "FAILED-GOAL not to be HURT")
        is 'inference-only
        plausibility 1.0))

; The below is needed because strength locks out the use of
; the above as a plan.
(define-rule Failed-Not-Hurt-Plan (all employment1-revenge)
  (RULE subgoal (HURT ?Self)
        goal (FAILED-GOAL (NOT (HURT ?Self)))
        is 'plan-only
        plan-comments '(if "ACTIVE-GOAL for FAILED-GOAL not to be hurt"
                           then "self is HURT")
        plausibility 1.0))

(define-initial-fact (all earthquake) (LOCATION name "the doorway"
                                                obname Doorway-Location))

(define-initial-fact (all earthquake) (KNOW Me Doorway-Location))

(define-rule Under-Doorway-Plan (all earthquake)
  (RULE subgoal (AT ?Self Doorway-Location)
        goal (UNDER-DOORWAY ?Self)
        plan-comments '(if "ACTIVE-GOAL to be UNDER-DOORWAY"
                       then "ACTIVE-GOAL to be AT doorway")
       inf-comments '(if "self AT doorway"
                       then "self UNDER-DOORWAY")
        plausibility 1.0))

(define-rule Possessions-Failure (all earthquake)
  (RULE subgoal (RAND (RNOT (INSURED ?Self))
                      (RPROX ?Self ?City)
                      (EARTHQUAKE ?City))
        goal (FAILED-GOAL (POSSESSIONS)
                          strength 0.7)
        is 'inference-only
        inf-comments '(if "self not INSURED and"
                          "self RPROX city and EARTHQUAKE in city"
                       then "FAILED-GOAL of POSSESSIONS")
        plausibility 0.9))

(define-rule Insured-Plan (all earthquake)
  (RULE subgoal (M-PURCHASE ?Self (INSURANCE) ?Insurance-Company)
        goal (INSURED ?Self)
        plan-comments '(if "ACTIVE-GOAL to be INSURED"
                       then "ACTIVE-GOAL to M-PURCHASE insurance")
        is 'plan-only
        plausibility 1.0))

(define-initial-fact (all always earthquake)
  (EMPLOYMENT actor (FEMALE-PERSON first-name "Carmelita" obname Carmelita1)
              organization (INSURANCE-COMPANY name "State Farm")))

(define-phrase "carmelita gives me the insurance."
  (ATRANS Carmelita1 Carmelita1 Me (INSURANCE)))

(define-initial-fact (all earthquake)
  (AT Carmelita1 (LOCATION name "State Farm" obname State-Farm-Loc)))

(define-initial-fact (all earthquake)
  (KNOW Me State-Farm-Loc))

(define-rule Purchase-Insurance-Plan (all earthquake)
  (RULE subgoal (RSEQ (EMPLOYMENT actor ?Person
                                  organization ?Insurance-Company)
                      (ATRANS ?Self ?Self ?Person (CASH))
                      (ATRANS ?Person ?Person ?Self (INSURANCE)))
        goal (M-PURCHASE ?Self (INSURANCE) ?Insurance-Company)
        is 'plan-only
        plan-comments '(if "ACTIVE-GOAL to M-PURCHASE insurance"
                           then "ACTIVE-GOAL to ATRANS CASH to employee of"
                           "insurance company and"
                           "ACTIVE-GOAL for employee to ATRANS INSURANCE"
                           "to self")
        plausibility 1.0))

; Never mind the below. It will just fire over and over.
(define-rule Money-Decrement (unused) ; was (all atrans)
  (RULE subgoal (ATRANS ?Self ?Self ?Other (CASH))
        goal (RCODE '(lambda () (money--)))
        is 'inference-only
        plausibility 1.0))

(defun money-- ()
  (ndbg-roman-nl *gate-dbg* rule "Money supply decremented.")
  (let ((money-need (ob$name->ob 'Money-Need)))
       (set-strength money-need (fl- (strength money-need) 0.1))))

;
; Rain example
;

(define-rule Wet-Inf (all rain)
  (RULE subgoal (RAND (RNOT (WEARING ?Self ?Raincoat))
                      (AT ?Self ?Location)
                      (RAINING ?Location))
        goal (WET ?Self)
        is 'inference-only
        inf-comments '(if "self not WEARING RAINCOAT and"
                          "self AT location and"
                          "it is RAINING at that location"
                       then "self is WET")
        plausibility 0.9))

(define-rule Not-Wet-Failure (all rain)
  (RULE subgoal (WET ?Self)
        goal (FAILED-GOAL (NOT (WET ?Self))
                          strength 0.7)
        is 'inference-only
        inf-comments '(if "self is WET"
                       then "FAILED-GOAL not to be WET")
        plausibility 1.0))

;
; For testing reversal-leafs?
;

; This is to assess the attitudes of another. The generator will
; turn the first MTRANS into a question, I think.
(define-rule Believe-Plan3 (all lovers1-v1)
  (RULE subgoal (RSEQ (MTRANS ?Self ?Self ?Other
                              (ACTIVE-GOAL (BELIEVE ?Other
                                                    (POS-ATTITUDE
                                                     ?Object))))
                      (MTRANS ?Other ?Other ?Self (POS-ATTITUDE ?Object)))
        goal (BELIEVE ?Other (POS-ATTITUDE ?Object))
        is 'plan-only
        plan-comments '(if "ACTIVE-GOAL for person to have POS-ATTITUDE toward"
                           "object"
                       then "ACTIVE-GOAL to MTRANS to person that self has"
                       "ACTIVE-GOAL to BELIEVE that person has POS-ATTITUDE"
                       "toward object and person MTRANS that person has"
                       "POS-ATTITUDE toward object")
        plausibility 0.8))

;
; 'Am' rules and initial facts
;

(define-initial-fact (all mut)
  (AT Movie-Star1 (LOCATION "where he lives"
                            obname Movie-Star1-Home)))

(define-rule Vprox-Plan2 (all mut revenge1 oseren recovery3)
  (RULE subgoal (M-PHONE ?Self ?Person)
        goal (VPROX ?Self ?Person)
        plan-comments '(if "ACTIVE-GOAL to be VPROX person"
                       then "ACTIVE-GOAL to M-PHONE person")
        inf-comments '(if "M-PHONE person"
                       then "VPROX person")
        plausibility 0.96))

(define-rule Vprox-Plan4 (all revenge1 oseren)
  (RULE subgoal (M-PHONE ?Person ?Self)
        goal (VPROX ?Self ?Person)
        plan-comments '(if "ACTIVE-GOAL to be VPROX person"
                       then "ACTIVE-GOAL for person to M-PHONE self")
        inf-comments '(if "person M-PHONE self"
                       then "VPROX person")
        plausibility 0.955))

(define-rule M-Phone-Plan1 (all mut revenge1 oseren recovery3)
  (RULE subgoal (KNOW ?Self (TELNO ?Person))
        goal (M-PHONE ?Self ?Person)
        is 'action-plan
        plan-comments '(if "ACTIVE-GOAL to M-PHONE person"
                       then "ACTIVE-GOAL to KNOW TELNO of person")
        plausibility 1.0))

(define-rule M-Phone-Plan2 (all mut revenge1 oseren)
  (RULE subgoal (KNOW ?Person (TELNO ?Self))
        goal (M-PHONE ?Person ?Self)
        is 'action-plan
        plan-comments '(if "ACTIVE-GOAL for person to M-PHONE self"
                       then "ACTIVE-GOAL for person to KNOW TELNO of self")
        plausibility 1.0))

;(define-initial-fact (all mut) (KNOW ?Person9
;                                    (TELNO ?Person9)))

(define-initial-fact (all mut lovers1) (KNOW Movie-Star1 (TELNO Movie-Star1)))

; Superseded by Vprox-Reflexive-Plan
;(define-initial-fact (all mut) (VPROX ?Person9 ?Person9))

; Is not necessary with new Believe-believe inferences?
;(define-rule Believe-Reduce (all mut)
;  (RULE subgoal ?Mental-Object
;        goal (BELIEVE ?Person (BELIEVE ?Person ?Mental-Object))
;        is 'plan-only
;        plausibility 1.0))

(define-rule Know-Plan (all oseren-alone recovery3)
;  (RULE subgoal (MTRANS ?Object2 ?Object1 ?Person1 ?Knowable))
  (RULE subgoal (MTRANS ?Person1 ?Phys-Obj1 ?Person1 ?Knowable)
        goal (KNOW ?Person1 ?Knowable)
        plan-comments '(if "ACTIVE-GOAL for person to KNOW info"
                       then "ACTIVE-GOAL for person to MTRANS info from"
                       "some object to person")
        inf-comments '(if "person MTRANS info from some object to person"
                       then "person KNOW info")
        plausibility 0.8))

(define-rule Know-Plan2 (all oseren-alone recovery3)
  (RULE subgoal (MTRANS ?Object2 ?Object1 ?Person1 ?Knowable)
        goal (KNOW ?Person1 ?Knowable)
        plan-comments '(if "ACTIVE-GOAL for person to KNOW info"
                       then "ACTIVE-GOAL for someone to MTRANS info from"
                       "some object to person")
        inf-comments '(if "someone MTRANS info from some object to person"
                       then "person KNOW info")
        plausibility 0.7))

(define-rule Know-Inf (all lovers)
  (RULE subgoal (MTRANS ?Object2 ?Object1 ?Person1 ?Knowable)
        goal (KNOW ?Person1 ?Knowable)
        is 'inference-only
        inf-comments '(if "object2 MTRANS info from object1 to person"
                          then "person KNOW info")
        plausibility 1.0))

; The UDIST is needed to prevent spurious serendipities having to
; do with (MTRANS Me Me Me (TELNO ...)).
(define-rule Know-Plan2 (all mut lovers1)
  (RULE subgoal (MTRANS ?Person4 ?Person4 ?Person3 (TELNO ?Person4))
        goal (KNOW ?Person3 (TELNO (UAND ?Person4 (UDIST ?Person3))))
        plan-no-gen '(t)
        inf-no-gen '(t)
        plan-comments '(if "ACTIVE-GOAL for person1 to KNOW TELNO of person2"
                       then "ACTIVE-GOAL for person2 to MTRANS TELNO"
                       "to person1")
        inf-comments '(if "person2 MTRANS TELNO to person1"
                          then "person1 KNOW TELNO of person2")
        plausibility 1.0))

; More general version of Know-Plan2; maybe I should have specification
; links to rule out more general rules.
(define-rule Know-Plan3 (all mut lovers1)
  (RULE subgoal (MTRANS ?Person6 ?Person6 ?Person5 (TELNO ?Person7))
        goal (KNOW ?Person5 (TELNO (UAND ?Person7 (UDIST ?Person5))))
        plan-comments '(if "ACTIVE-GOAL for person1 to KNOW TELNO of person2"
                       then "ACTIVE-GOAL for person3 to MTRANS TELNO of person2"
                       "to person1")
        initial (MUTATION) ; hack hack
        plausibility 0.9))

; The following rule subsumes the following cases:
; - Someone reads the info to self from a book
; - Self reads the knowable info in a book (?Person = ?Self)
; - Someone tells self the info (?Person = ?Object)
;
; The BELIEVEs come before the VPROXes so that mut4 will work
; (Karen gets instantiated first). But this means that no planning
; loop will occur in the initial phases of RECOVERY because it will
; not get to it.
(define-rule Mtrans-Plan1 (all lovers1 mut rain recovery3 oseren-alone)
  (RULE subgoal (RSEQ (KNOW ?Object ?Knowable)
                      (BELIEVE ?Person2 (ACTIVE-GOAL (KNOW ?Person1
                                                           ?Knowable)))
                      (BELIEVE ?Person2
                               (BELIEVE ?Person1
                                        (ACTIVE-GOAL
                                         (KNOW ?Person1 ?Knowable))))
                      (VPROX ?Person1 ?Person2)
                      (VPROX ?Person2 ?Object)
                      ) ; end RSEQ
        goal (MTRANS ?Person2 ?Object ?Person1 ?Knowable)
        is 'action-plan
        plan-no-gen '(t t t t t)
        plan-comments '(if "ACTIVE-GOAL for person2 to MTRANS info from object to"
                           "person1"
                       then "ACTIVE-GOAL for object to KNOW info and"
"ACTIVE-GOAL for person1 to be VPROX person2 and ACTIVE-GOAL for person2 to be"
"VPROX object and ACTIVE-GOAL for person2 to BELIEVE that person1"
"has ACTIVE-GOAL to KNOW info and ACTIVE-GOAL for person2 to"
"have ACTIVE-GOAL for person1 to KNOW info")
        plausibility 1.0))

; The below is used for loading, so that the correct chaining
; will be created. I assume this is correct.
(defun concrete? (x) t)

(define-rule Vprox-Reflexive-Plan (all mut rain recovery3 lovers1 oseren-alone)
  (RULE subgoal (RTRUE)
        goal (VPROX (UAND ?Person (UPROC 'Concrete?)) 
                    (UAND ?Person (UPROC 'Concrete?)))
        plan-no-gen '(t)
        is 'plan-only
        plan-comments '(if "ACTIVE-GOAL for person to be VPROX person"
                           then "ACTIVE-GOAL for RTRUE")
        plausibility 1.0))

(defun concrete? (x)
  (and (ob? x) (not (var? x))))

(define-rule Believe-Believe-Inf (all mut rain recovery3 lovers1 oseren-alone)
  (RULE subgoal (ACTIVE-GOAL (MTRANS ?Self ?Object ?Self ?Knowable))
        goal
        (BELIEVE ?Self (BELIEVE ?Self (ACTIVE-GOAL (KNOW ?Self ?Knowable))))
        (BELIEVE ?Self (ACTIVE-GOAL (KNOW ?Self ?Knowable)))
        is 'inference-only
        inf-no-gen '(t)
        inf-comments '(if "self has ACTIVE-GOAL to MTRANS info from object"
                          then "self BELIEVE self BELIEVE ACTIVE-GOAL and"
                          "self BELIEVE ACTIVE-GOAL")
        plausibility 1.0))

(define-initial-fact (mut-alone recovery3-alone)
  (FAILED-GOAL (LOVERS Me Movie-Star1) obname Failed-Lovers1))

(define-initial-fact (mut-alone recovery3-alone)
  (NEG-EMOTION strength 0.9 obname Upset1))

(define-initial-fact (mut-alone recovery3-alone)
  (DEPENDENCY Failed-Lovers1 Upset1 1.0 0.0))

(define-initial-fact (mut-alone recovery3-alone)
  (ACQUAINTED Me Movie-Star1))

(define-rule Recovery-Theme (all mut mut-alone recovery3-alone)
  (RULE subgoal (RAND (DEPENDENCY ?Failed-Goal
                                  (UAND ?Neg-Emotion
                                        (UPROC 'Greater-Recovery-Activate?)
                                        (UPROC 'Emot-Less-Learn-Thresh?)))
                      ) ; end RAND
        goal (ACTIVE-GOAL (RECOVERY ?Failed-Goal))
        emotion ?Neg-Emotion (NEG-EMOTION strength 0.02)
        is 'inference-only
        inf-comments '(if "NEG-EMOTION resulting from a FAILED-GOAL"
                       then "ACTIVE-GOAL for RECOVERY"
                       "of failure")
        plausibility 1.0))

(defun greater-recovery-activate? (emotion)
  (fl> (strength emotion) 0.5))

; Should we also include waiting tasks here?
(defun halted-task? (x)
  (and (top-level-goal? x)
       (eq? (ob$get x 'status) 'halted)))

;  (RULE subgoal (RAND (ROR (UAND (UPROC 'Halted-Task?)
;                                 (UPROC 'Vars-In?)
;                                 ?Active-Goal1
;                                 ?Active-Goal2)
;                           (ACTIVE-GOAL obj (WAIT)
;                                        top-level-goal
;                                        (UAND (UPROC 'No-Vars-In?)
;                                              ?Active-Goal1)
;                                        seq-next ?Active-Goal2)
;                           ) ; end ROR
;                      (DEPENDENCY (UAND ?Pos-Emotion
;                                        (UPROC 'Greater-Rehearsal-Activate?)
;                                        (UPROC 'Emot-Less-Learn-Thresh?))
;                                  ?Active-Goal1))
;  was       goal (ACTIVE-GOAL (REHEARSAL ?Active-Goal2))

(defun no-vars-in? (x)
  (not (vars-in? x)))

(define-rule Rehearsal-Theme (all lovers) ; was mut mut-alone
  (RULE subgoal (RAND (ACTIVE-GOAL obj (WAIT)
                                   top-level-goal
                                   (UAND ?Active-Goal1
                                         (ACTIVE-GOAL
                                          (UAND ?Obj (UPROC 'No-Vars-In?)))))
                      (DEPENDENCY (UAND ?Pos-Emotion
                                        (UPROC 'Greater-Rehearsal-Activate?)
                                        (UPROC 'Emot-Less-Learn-Thresh?))
                                  ?Active-Goal1))
        goal (ACTIVE-GOAL (REHEARSAL ?Obj))
        emotion ?Pos-Emotion (POS-EMOTION strength 0.01)
        is 'inference-only
        inf-comments '(if "subgoal to WAIT and"
                          "corresponding top-level goal contains"
                          "no variables and"
                          "POS-EMOTION of sufficient strength connected to"
                          "top-level goal"
                       then "ACTIVE-GOAL for REHEARSAL"
                       "of top-level goal")
        plausibility 1.0))

(define-rule Rehearsal-Plan (all mut oseren-alone lovers)
  (RULE subgoal ?Obj
        goal (REHEARSAL ?Obj)
        is 'plan-only
        plan-comments '(if "ACTIVE-GOAL for REHEARSAL of a goal"
                       then "ACTIVE-GOAL to achieve goal")
        plausibility 1.0))

(define-rule Test-Theme (all oseren-alone)
  (RULE subgoal (RTRUE)
        goal (ACTIVE-GOAL (REHEARSAL (ACTIVE-GOAL (LOVERS ?Self ?Male-Person))))
        emotion (POS-EMOTION strength 0.9)
        is 'inference-only
        inf-comments '(if "testing object serendipities"
                       then "activate daydreaming goal for REHEARSAL"
                       "of LOVERS")
        plausibility 1.0))

(defun greater-rehearsal-activate? (emotion)
  (fl> (strength emotion) 0.5))

; This is just for now. Later this can be done via serendipity.
(define-rule Repercussions-Theme1 (all earthquake)
  (RULE subgoal (EARTHQUAKE Mexico-City)
        goal (ACTIVE-GOAL (REPERCUSSIONS (LIST (EARTHQUAKE-ONSET Los-Angeles)
                                             (EARTHQUAKE Los-Angeles))))
        emotion (NEG-EMOTION strength 0.7)
        is 'inference-only
        inf-comments '(if "someone else has a goal failure caused by an event"
                       then "ACTIVE-GOAL for REPERCUSSIONS"
                       "of failure")
        plausibility 1.0))

(define-rule Repercussions-Plan1 (all earthquake)
  (RULE subgoal (RSEQ (HYPOTHESIZE ?State) (REPERCUSSIONS ?List-or-state))
        goal (REPERCUSSIONS (LIST ?State ?List-or-state))
        plan-comments '(if "ACTIVE-GOAL for REPERCUSSIONS of list"
                       then "ACTIVE-GOAL to HYPOTHESIZE first element of list"
                       "and ACTIVE-GOAL for REPERCUSSIONS of remainder of list")
        is 'plan-only ; not really necessary?
        plausibility 1.0))

(define-rule Repercussions-Plan2 (all earthquake)
  (RULE subgoal (HYPOTHESIZE ?State)
        goal (REPERCUSSIONS ?State)
        plan-comments '(if "ACTIVE-GOAL for REPERCUSSIONS of state"
                       then "ACTIVE-GOAL to HYPOTHESIZE state")
        is 'plan-only ; not really necessary?
        plausibility 1.0))

(define-rule Hypothesize-Plan (all earthquake)
  (RULE subgoal (RCODE '(lambda (goal context top-level-goal rule bd)
                                (hypothesize-plan1 goal context top-level-goal
                                                   rule bd)))
        goal (HYPOTHESIZE ?State)
        plan-comments '(if "ACTIVE-GOAL to HYPOTHESIZE state"
                       then "ACTIVE-GOAL to assert state in context")
        is 'plan-only
        plausibility 1.0))

(defun hypothesize-plan1 (goal context top-level-goal rule bd)
  (ndbg-roman-nl *gate-dbg* rule "Hypothesize plan")
  (let ((sprouted-context (cx$sprout context))
        (intends (ob$fcreate `(INTENDS linked-from ,goal
                                       linked-to (SUCCEEDED-GOAL obj (RTRUE))
                                       rule Hypothesize-Plan))))
       (delay-dbgs sprouted-context
          (rule-fire-msg rule "coded plan" context bd sprouted-context goal)
          (cx$assert sprouted-context intends)
          (cx$assert sprouted-context (var-value '?State))
          nil)
       (list sprouted-context)))

(comment
(define-rule Recovery-Plan (all mut)
  (RULE subgoal ?Obj
        goal (RECOVERY (FAILED-GOAL ?Obj))
        is 'plan-only
        plan-comments '(if "ACTIVE-GOAL for RECOVERY from a failure"
                       then "ACTIVE-GOAL to achieve goal that failed")
        plausibility 1.0))
)

(define-rule Recovery-Plan (all mut-alone recovery3-alone)
  (RULE subgoal (MTRANS ?Self ?Self ?Person
                        (ACTIVE-GOAL (LOVERS ?Self ?Person)))
        goal (RECOVERY (FAILED-GOAL (LOVERS ?Self ?Person)))
        is 'plan-only
;        halt? 't  for recovery3
; Note: halt? does not take effect anymore for dd goals. So that
; Rehersal of lovers works.
        plan-comments '(if "ACTIVE-GOAL for RECOVERY from a LOVERS failure"
                        then "ACTIVE-GOAL to MTRANS to person that self"
                        "has ACTIVE-GOAL of LOVERS with person")
        plausibility 1.0))

(comment
(define-rule Recovery-Inf (all mut)
  (RULE subgoal (SUCCEEDED-GOAL ?Obj)
        goal (RECOVERY (FAILED-GOAL ?Obj))
        is 'inference-only
        inf-comments '(if "ACTIVE-GOAL succeeds"
                       then "RECOVERY for goal")
        plausibility 1.0))
) ; want to make recovery-plan be plan-only-no-auto?

;
; 'Am4' rules and initial facts
;

(define-rule Want-Telno-Plan (all mut4)
  (RULE subgoal (BELIEVE ?Person1 (ACTIVE-GOAL (LOVERS ?Person1 ?Person2)))
        goal (BELIEVE ?Person1
                      (ACTIVE-GOAL (KNOW ?Person2 (TELNO ?Person1))))
        plan-comments '(if "ACTIVE-GOAL for person to have ACTIVE-GOAL to KNOW"
                           "TELNO of another person"
                       then "ACTIVE-GOAL for person to have ACTIVE-GOAL of"
                       "LOVERS with person")
        inf-comments '(if "person has ACTIVE-GOAL of LOVERS with person"
                       then "person has ACTIVE-GOAL to KNOW TELNO of person")
        plausibility 0.8))

(define-rule Friends-Help-Plan (all mut4)
  (RULE subgoal (RSEQ (FRIENDS ?Person1 ?Person2)
                      (BELIEVE ?Person1
                               (BELIEVE ?Person2
                                        (ACTIVE-GOAL (KNOW ?Person2
                                                           ?Knowable)))))
        goal (BELIEVE ?Person1 (ACTIVE-GOAL (KNOW ?Person2 ?Knowable)))
        initial (RNOT (RICH ?Person1)) ; i.e., does not apply to Movie-Star1
        plan-comments '(if
    "ACTIVE-GOAL for person1 to have ACTIVE-GOAL for person2"
                           "to KNOW info"
                       then "ACTIVE-GOAL for person1 et person2 to be FRIENDS"
                       "and for person1 to BELIEVE that person2 has an"
                       "ACTIVE-GOAL to KNOW info")
        inf-comments '(if "person1 et person2 are FRIENDS"
                       "and person1 BELIEVE that person2 has an"
                       "ACTIVE-GOAL to KNOW info"
                       then "person1 has ACTIVE-GOAL for person2"
                           "to KNOW info")
        plausibility 0.95))
        
(define-initial-fact (all mut4) (RICH (FEMALE-PERSON "Karen" obname Karen)))

(define-initial-fact (all mut4) (ATTRACTIVE Karen))

(define-initial-fact (all mut4) (FRIENDS Me Karen))

(define-initial-fact (all mut4) (KNOW Me (TELNO Karen)))

;
; 'Am5' rules and initial facts
;

(define-initial-fact (all mut5) (KNOW (PHYS-OBJ mcontents ?Knowable)
                                     ?Knowable))

(define-rule Vprox-Plan3 (all mut5 ac2 recovery3 employment1)
  (RULE subgoal (POSS ?Person ?Phys-Obj)
        goal (VPROX ?Person ?Phys-Obj)
        plan-comments '(if "ACTIVE-GOAL for person to be VPROX an object"
                       then "ACTIVE-GOAL for person to POSS that object")
        inf-comments '(if "person POSS object"
                       then "person VPROX that object")
        plausibility 0.95))

(define-rule Poss-Plan1 (all mut5 employment1 lovers)
  (RULE subgoal (ATRANS ?Person1 ?Person1 ?Person2 ?Phys-Obj)
        goal (POSS ?Person2 ?Phys-Obj)
        delete (POSS ?Person1 ?Phys-Obj)
        initial (POSS ?Person1 ?Phys-Obj)
        plan-comments '(if
             "ACTIVE-GOAL for person2 to POSS object which person1"
                           "POSS"
                       then
             "ACTIVE-GOAL for person1 to ATRANS object to person2")
        inf-comments '(if "person1 ATRANS object to person2"
                          then "person2 POSS object and"
                          "delete person1 POSS object")
        plausibility 1.0))

(define-rule Poss-Plan2 (all rain recovery3 reversal1)
  (RULE subgoal (GRAB ?Person1 ?Phys-Obj)
        goal (POSS ?Person1 ?Phys-Obj)
        plan-comments '(if "ACTIVE-GOAL for person to POSS object"
                       then "ACTIVE-GOAL to GRAB object")
        inf-comments '(if "person GRAB object"
                          then "person POSS object")
        plausibility 1.0))

(define-rule Grab-Plan (all rain recovery3 reversal1)
  (RULE subgoal (RSEQ (AT ?Phys-Obj ?Location)
                      (AT ?Self ?Location))
        goal (GRAB ?Self ?Phys-Obj)
        is 'action-plan
        plan-comments '(if "ACTIVE-GOAL to GRAB object"
                       then "ACTIVE-GOAL to be AT location of object")
        plausibility 1.0))

;
; Episode definitions
;

(define-initial-fact (all remind roving1)
  (RESTAURANT name "Gulliver's" obname Gullivers))

(define-initial-fact (all remind roving1)
  (CITY name "the Marina" obname Marina))

(define-initial-fact (all always)
  (CITY name "Venice Beach" obname Venice))

(setq *roving-episodes*
  (cons
; begin first roving episode
; Todo: must add emotions to such episodes the "right" way (attached
; to top-level goal). But note that with the current retrieval algorithm,
; this isn't required. It assumes that any emotions as indices are
; associated with the top-level goal and sets up the dependency itself.
(define-episode (all roving1 remind)
  (Gullivers Marina Steve1 (POS-EMOTION)) nil nil
  ((BELIEVE (MALE-PERSON first-name "Steve" obname Steve1)
            (POS-ATTITUDE Me))
   induce-rule nil
   ((MTRANS Steve1 Steve1 Me
            (BELIEVE Steve1 (POS-ATTITUDE Me))))
   ((AT Me Gullivers))
   ((AT Steve1 Gullivers))))
; end first roving episode
   *roving-episodes*))

(define-episode (all remind) ((EMPLOYMENT) Marina Venice) 1 1
  ((EMPLOYMENT actor Me
               organization
               (ORGANIZATION name "UCLA Sailing Club" obname UCLA-Sail))
   induce-rule nil
   ((RPROX UCLA-Sail Marina))))

; This episode requires intersection of the indices from above.
(define-episode (all remind) (Venice Steve1) 2 2
  ((M-PURCHASE actor Me actor Steve1 obj (SUNGLASSES))
   induce-rule nil
   ((RPROX Me Venice))))

(ob$create '(MALE-PERSON first-name "Peter" obname Peter))

; No reminding of this should occur. Except perhaps when Nuart-Theater
; is activated as index. Could up to 2 2 and somehow get Peter to be
; active.
(define-episode (all remind) (Peter Nuart-Theater) 1 1
  ((M-MOVIE actor Me actor Peter)
   induce-rule nil
   ((AT Me Nuart-Theater))))

;
; Other
;

(define-episode (all recovery3) nil nil nil
  ((KNOW Me (COLLEGE (FEMALE-ACTOR first-name "Brooke" last-name "Shields"
                                   obname Brooke)))
   Know-Plan take-from-rule
   ((MTRANS Me (MAGAZINE name "People magazine" obname People-Magazine)
            Me (COLLEGE Brooke))
    induce-rule (t nil)
    ((KNOW People-Magazine (COLLEGE Brooke)))
    ((POSS Me People-Magazine)
     induce-rule (t)
     ((M-PURCHASE Me People-Magazine Westward-Ho))))))

(define-initial-fact (all recovery3)
  (SELLS Westward-Ho People-Magazine))

;
; Trw-Credit
;

(define-episode (all ac1) nil nil nil
  ((KNOW Me (TELNO (MALE-PERSON first-name "Rich" obname Rich1)))
   Know-Plan2 take-from-rule
   ((MTRANS (MALE-PERSON first-name "Alex" obname Alex)
            (INFO-OBJ name "the TRW credit database"
                      obname Trw-Credit-File)
            Me (TELNO Rich1))
    induce-rule (t nil)
    ((KNOW Trw-Credit-File (TELNO Rich1)))
    ((ACCESS Alex Trw-Credit-File)
     induce-rule nil
     ((M-LOGIN Alex Trw-Credit-File))))))

;
; Unlisted phone directory episode
;

; Should not match goal of (KNOW Me (LOC Harrison))
(define-episode (all ac2) nil nil nil
  ((KNOW (FEMALE-PERSON first-name "Sally" obname Tel-Emp1)
         (TELNO (MALE-PERSON first-name "Harry" obname Harry)))
   induce-rule nil
   ; if we instead used Know-plan here, ?person2 and ?person1
   ; would be the different, and when analogically applying this,
   ; Tel-Emp1 would be instantiated as an 'episodic detail'.
   ; We chose induce plan here since we chose Know-plan above.
   ((MTRANS Tel-Emp1 (PHYS-OBJ name "the unlisted telephone directory"
                               obname Unlisted-Dir)
            Tel-Emp1 (TELNO Harry))
    Mtrans-Plan1 take-from-rule
    ((KNOW Unlisted-Dir (TELNO Harry)))
    ((BELIEVE Tel-Emp1 (BELIEVE Tel-Emp1
                                (ACTIVE-GOAL (KNOW Tel-Emp1 (TELNO Harry))))))
    ((BELIEVE Tel-Emp1 (ACTIVE-GOAL (KNOW Tel-Emp1 (TELNO Harry)))))
    ((VPROX Tel-Emp1 Tel-Emp1))
    ((VPROX Tel-Emp1 Unlisted-Dir)
     Vprox-Plan3 take-from-rule
     ((POSS Tel-Emp1 Unlisted-Dir)
      induce-rule nil
      ((EMPLOYMENT actor Tel-Emp1 organization
                   (ORGANIZATION name "the telephone company"
                                 obname Tel-Co))))))))

;
; Vicente shopping episode
;

(define-initial-fact (unused)
  (AT (ORGANIZATION "Vicente Foods" obname Vicente-Foods)
      (LOCATION "Vicente Foods" obname Vicente-Location)))

(ob$create '(LOCATION obname Some-Location))

(define-episode (unused) nil nil nil
  ((AT (ACTOR obname Movie-Star2)
       Vicente-Location)
   At-Plan take-from-rule
   ((PTRANS Movie-Star2 Some-Location Vicente-Location Movie-Star2))))

;
; 'Debra goes to France' episode
;

(ob$create '(CITY "Cairo" obname Cairo))

; The time Harrison Ford had a job with Paramount Pictures by
; being in Cairo.
(define-episode (all rationalization1) nil nil nil
  ((ACTING-EMPLOY actor Movie-Star1 actor (ORGANIZATION "Paramount Pictures"
                                         obname Paramount))
   induce-rule nil
   ((RPROX Movie-Star1 Cairo))))

;
; Lovers Hidden Blessing Episode
;

(ob$create '(MALE-PERSON "Chris" obname Chris))

(ob$create '(MALE-PERSON "Irving" obname Irving))

(ob$create '(BAR name "Mom's" obname Bar1))

(define-initial-fact (all rationalization2) (AT Bar1 (LOCATION name "Mom's"
                                                      obname Bar1-Loc)))

(define-initial-fact (all rationalization2) (KNOW Me Bar1-Loc))

(define-initial-fact (all rationalization2) (KNOW Chris Bar1-Loc)) ;?

; The time Irving turning me down led to going out with Chris by
; me going out with Chris. I was going out with Chris by me being
; at a bar and Chris being at a bar.  I was at a bar by going to a bar.
(define-episode (all rationalization2) nil nil nil
  ((LEADTO (FAILED-GOAL (LOVERS Me Irving))
           (SUCCEEDED-GOAL))
   induce-rule nil
   ((LEADTO (FAILED-GOAL (LOVERS Me Irving))
            (SUCCEEDED-GOAL (LOVERS Me Chris)))
    induce-rule (t)
    ((LOVERS Me Chris)
     induce-rule nil
     ((AT Me Bar1-Loc)
      At-Plan take-from-rule
      ((PTRANS Me Home Bar1-Loc Me)))
     ((AT Chris Bar1-Loc))))))

;
; Episodes for object-based serendipitous remindings
;

; The time I was acquainted with Harold. It was acceptable to talk to him
; because I was a member of the computer dating service and he was a member
; of the computer dating service. I was a member of the computer dating
; service because I gave the computer dating service some money.
;
; Gen heuristic: Take top-level goal and any subgoals with induced rules.
;
; Note: since the ATRANS plan below for DATING-SERVICE-MEMBER will
; be connected to DSM Me as well as DSM Harold, rip-path->episode1
; will use the rule equally for DSM Harold, since it does not
; worry about whether that particular application is in the ep, as
; long as that rule applies. If you know what I mean.
;
(define-episode (all oseren) ((COMPUTER) (CASH)) 3 2
  ((ACQUAINTED Me (MALE-PERSON first-name "Harold" obname Harold))
   Acquainted-Plan take-from-rule
   ((M-CONVERSATION Me Harold)
    M-Conversation-Plan take-from-rule
    ((MTRANS-ACCEPTABLE Me Harold)
     induce-rule nil
     ((DATING-SERVICE-MEMBER Me)
      induce-rule nil
      ((ATRANS Me Me (MALE-PERSON first-name "the dating service employee"
                                        obname Dating-Service1-Emp) (CASH))))
     ((DATING-SERVICE-MEMBER Harold)))
    ((MTRANS Me Me Harold (INTRODUCTION))
     Mtrans-Plan2 take-from-rule
     ((VPROX Me Harold)
      Vprox-Plan2 take-from-rule
      ((M-PHONE Me Harold)
       M-Phone-Plan1 take-from-rule
       ((KNOW Me (TELNO Harold))
        Know-Plan2 take-from-rule
        ((MTRANS Dating-Service1-Emp Dating-Service1-Emp Me (TELNO Harold)))))))
    ((MTRANS Harold Harold Me (INTRODUCTION))))))
; Note: this whole second half of the ep is not used?!

(define-initial-fact (all oseren)
  (AT Dating-Service1-Emp
      (LOCATION "the dating service" obname Dating-Service1-Location)))

(define-initial-fact (all oseren) (KNOW Me Dating-Service1-Location))

; Unfortunately, this doesn't work, because non-me-belief-path episodes
; can't be represented in the current implementation?
; In addition, serendipity does not try possible believe person paths.
; So, for now we'll do it direct to the subgoal of Lovers-Plan via
; induced rules.
(define-episode (all oseren) ((LAMPSHADE)) 3 2
  ((BELIEVE (FEMALE-PERSON first-name "a member of the audience"
                           obname Audience-Member)
;            (ROMANTIC-INTEREST (MALE-PERSON first-name "a comedian"
;                                            obname Comedian1))
            (ACTIVE-GOAL (LOVERS Audience-Member
                                 (MALE-PERSON first-name "a comedian"
                                              obname Comedian1))))
   induce-rule nil
   ((BELIEVE Audience-Member (POS-ATTITUDE Comedian1))
    induce-rule nil
    ((WEARING Comedian1 (LAMPSHADE))))
   ((BELIEVE Audience-Member (ATTRACTIVE Comedian1)))))

; PATROL DREAM

; Definitions for patrol dream
(ty$create 'M-RANGER '(ACTIVITY) '(prop (actor from to obj) ()))
(ty$create 'RANGER-STATION '(LOCATION) '(prop (name) ()))
(ty$create 'M-GET-READY '(ACTIVITY) '(prop (actor obj)))
(ty$create 'WELL-PREPARED '(PERSONAL-ATTRIBUTE) '(prop (actor to obj) ()))
(ty$create 'WELL-PREPARED-FOR '(PERSONAL-ATTRIBUTE) '(prop (actor to obj) ()))
(ty$create 'M-WALK-ON-PATROL '(ACTIVITY) '(prop (actor from to)))
(ty$create 'M-RETURN-TO-STATION '(ACTIVITY) '(prop (actor obj)))
(ty$create 'M-DEFEND-HOUSE '(ACTIVITY) '(prop (actor from to obj) ()))
(ty$create 'SHOTGUN '(FREE-PHYS-OBJ) '(prop () ()))
(ty$create 'ADVANTAGE '(RELATIONSHIP) '(prop (actor obj) ()))
(ty$create 'GROUP '(RELATIONSHIP) '(nil (actor nonmembers) ()))
(ty$create 'ACADEMIC-SUCCESS '(STATE) '(prop (actor obj) ()))
(ty$create 'M-COURSE '(ACTIVITY) '(prop (actor) ()))
(ty$create 'UNDO-CAUSES '(STATE) '(prop (obj) ()))
(ty$create 'TEACHER '(PERSON) '(prop (first-name last-name) ()))
(ty$create 'FEMALE-TEACHER '(TEACHER FEMALE-PERSON) nil)

; Rules for patrol dream
(define-rule M-Ranger-Plan (patrol)
  (RULE subgoal (RSEQ (M-GET-READY ?Self (M-RANGER ?Self ?Location1 ?Location2 ?Ranger-Station))
                      (M-WALK-ON-PATROL ?Self ?Location1 ?Location2)
                      (M-RETURN-TO-STATION ?Self ?Ranger-Station))
        goal (M-RANGER ?Self ?Location1 ?Location2 ?Ranger-Station)
        script 't
        plan-no-gen '(nil nil nil)
        is 'plan-only
        plausibility 1.0))

(define-rule M-Get-Ready-Plan (patrol)
  (RULE subgoal (WELL-PREPARED ?Self ?Person ?Activity)
        goal (M-GET-READY ?Self ?Activity)
        is 'plan-only
        plausibility 1.0))

(define-rule Well-Prepared-Plan (patrol)
  (RULE subgoal (WELL-PREPARED-FOR ?Self ?Person ?Activity)
        goal (WELL-PREPARED ?Self ?Person ?Activity)
        plausibility 1.0))

(define-rule Well-Prepared-Ranger-Plan (patrol)
  (RULE subgoal (POSS ?Self (SHOTGUN))
        goal (WELL-PREPARED-FOR ?Self ?Person (M-RANGER ?Self))
        plausibility 1.0))

(define-rule M-Walk-On-Patrol-Plan (patrol)
  (RULE subgoal (RSEQ (PTRANS ?Self ?Location1 ?Location2)
                      (PTRANS ?Self ?Location2 ?Location1))
        goal (M-WALK-ON-PATROL ?Self ?Location1 ?Location2)
        is 'plan-only
        plausibility 1.0))

(define-rule M-Return-To-Station-Plan (patrol)
  (RULE subgoal (RSEQ (AT ?Self ?Ranger-Station)
                      (GROUP actor ?Self ?Male-Person1
                             ?Male-Person2 ?Male-Person3))
        goal (M-RETURN-TO-STATION ?Self ?Ranger-Station)
        is 'plan-only
        plausibility 1.0))

(define-rule Revenge-Plan3 (patrol)
  (RULE subgoal (RSEQ (BELIEVE ?Other
                               (ACTIVE-GOAL (BELIEVE ?Self
                                                     (POS-ATTITUDE ?Other))))
                      (BELIEVE ?Other
                               (FAILED-GOAL (BELIEVE ?Self
                                                     (POS-ATTITUDE ?Other)))))
        goal (REVENGE ?Self ?Other
                      (FAILED-GOAL (BELIEVE ?Other (POS-ATTITUDE ?Self))))
        plan-no-gen '(nil t t)
        is 'plan-only
        plausibility 1.0))

(define-rule Failed-Social-Esteem-Plan1 (patrol)
  (RULE subgoal (RSEQ (ADVANTAGE ?Self ?Other)
                      (BELIEVE ?Other (BELIEVE ?Self (NEG-ATTITUDE ?Other))))
        goal (BELIEVE ?Other (FAILED-GOAL (BELIEVE ?Self
                                                   (POS-ATTITUDE ?Other))))
        delete (BELIEVE ?Other (FAILED-GOAL (BELIEVE ?Self
                                                     (POS-ATTITUDE ?Other))))
        is 'plan-only
        plausibility 1.0))

(define-rule Advantage-Plan1 (patrol)
  (RULE subgoal (GROUP actor ?Person1 ?Male-Person1
                       ?Male-Person2 ?Male-Person3
                       nonmembers ?Person2)
        goal (ADVANTAGE ?Person1 ?Person2)
        plausibility 1.0))

(define-rule Group-Plan (patrol)
  (RULE subgoal (PTRANS ?Person1 ?Location2 ?Location1)
        goal (GROUP actor ?Person1 ?Male-Person1
                       ?Male-Person2 ?Male-Person3
                       nonmembers ?Person2)
        plausibility 1.0))

; Below is handled by Believe-Plan1
;(define-rule Believe-Neg-Att-Plan1 (patrol)
;  (RULE subgoal (MTRANS ?Self ?Other (BELIEVE ?Self (NEG-ATTITUDE ?Other)))
;        goal (BELIEVE ?Other (BELIEVE ?Self (NEG-ATTITUDE ?Other)))
;        plausibility 1.0))

(define-rule Believe-Neg-Att-Plan2 (patrol)
  (RULE subgoal (M-BEAT-UP ?Self ?Other)
        goal (BELIEVE ?Other (BELIEVE ?Self (NEG-ATTITUDE ?Other)))
        plausibility 1.0))

(define-rule Reversal-Plan1 (patrol)
  (RULE subgoal (UNDO-CAUSES (FAILED-GOAL (ACADEMIC-SUCCESS ?Self ?Teacher)))
        goal (REVERSAL (FAILED-GOAL (ACADEMIC-SUCCESS ?Self ?Teacher)))
        is 'plan-only
        plausibility 1.0))

(define-rule Undo-Causes-Plan1 (patrol)
  (RULE subgoal (BELIEVE ?Teacher (POS-ATTITUDE ?Self))
        goal (UNDO-CAUSES (FAILED-GOAL (ACADEMIC-SUCCESS ?Self ?Teacher)))
        is 'plan-only
        plausibility 1.0))

(define-rule Teacher-Pos-Att-Plan1 (patrol)
  (RULE subgoal (WELL-PREPARED ?Self ?Teacher (M-COURSE ?Self))
        goal (BELIEVE ?Teacher (POS-ATTITUDE ?Self))
        is 'plan-only
        plausibility 1.0))

(ob$create '(FEMALE-TEACHER first-name "Beth" obname Teacher1))

(setq *test-reversal-goal*
  (ob$create '(ACTIVE-GOAL (REVERSAL
               (FAILED-GOAL (ACADEMIC-SUCCESS Me Teacher1))))))

(setq *test-revenge-goal*
  (ob$create '(ACTIVE-GOAL
               (REVENGE Me Teacher1
                        (FAILED-GOAL (BELIEVE Teacher1 (POS-ATTITUDE Me)))))))

(setq *test-ranger-goal*
  (ob$create '(ACTIVE-GOAL
               (M-RANGER Me (LOCATION) (LOCATION) (RANGER-STATION)))))

; Todo: For general mutations, must add (UAND ?Person2 (UNOT ?Person1))
(setq *mutations* (list
  (list (ob$create `(MTRANS ?Person7 ?Person7
                            ?Person8 ?Something))
        (ob$create `(MTRANS ?Person7 ?Person7
                            (UCODE '(lambda () (mut1-code)))
                            ?Something)))
                   ))

(defun mut1-code ()
  (ob$fcreate `(UAND obj ?Person9
                     obj (UDIST obj ,(var-value '?Person7))
                     obj (UDIST obj ,(var-value '?Person8)))))

(setq *any-pattern* '?Anything)

; End of file.
