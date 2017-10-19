;*******************************************************************************
;
; Sample use of Daydreamer
;
;*******************************************************************************

; Load Gate and Daydreamer.

(setq *gate-load-options* '(sample))
(load "gate_get")
(load "dd_get")

; Define types and objects.

(ty$fcreate 'FILE '(OBJECT) '(name))
(ty$fcreate 'HACK '(NEED) '(strength))
(ty$fcreate 'FTP '(ACTION) '(actor obj))
(ob$fcreate '(FILE name "the Daydreamer source code" obname File1))

; Define needs.

(setq *needs* (list (ob$fcreate '(HACK)) (ob$fcreate '(ENTERTAINMENT))))

; Define concern initiation, planning, and action effect rules.

(define-rule Hack-Theme (sample)
  (RULE subgoal (UAND (HACK) (UPROC 'Less-Need-Thresh?))
        goal (ACTIVE-GOAL (HACK strength (UPROC 'Need-Satisfied?)))
        is 'inference-only
        emotion (POS-EMOTION strength 0.6)
        inf-comments '(if "level of satisfaction of HACK need below"
                          "threshold"
                       then "ACTIVE-GOAL to HACK")
        plausibility 1.0))

(define-rule Hack-Plan (sample)
  (RULE subgoal (FTP actor ?Self obj File1)
        goal (HACK)
        plan-comments '(if "ACTIVE-GOAL to HACK"
                        then "ACTIVE-GOAL to FTP Daydreamer source code")
        is 'plan-only
        plausibility 1.0))

(define-rule Ftp-Plan (sample)
  (RULE subgoal (RTRUE)
        goal (FTP actor ?Self obj ?File)
        plan-comments '(if "ACTIVE-GOAL to FTP"
                        then "ACTIVE-GOAL for RTRUE")
        is 'action-plan
        plausibility 1.0))

; Define English generation rules.

(define-gen HACK nil
  (gen-need-obj con stream switches context bp 'hack " some code"))

(define-gen FTP nil
  (let ((subject (ob$gets con 'actor)))
     (gen-subject subject stream switches context bp)
     (gen-verb 'download subject stream switches (neg? con))
     (gen (ob$get con 'obj) stream switches context bp)
     subject))

; Run Daydreamer.

(daydreamer)
