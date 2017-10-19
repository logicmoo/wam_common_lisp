;*******************************************************************************
;
; GATE
; Version 2.3
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
;*******************************************************************************

;
; Type definitions for specials
;
(ty$create 'UVAR nil '(prop (name unifies-with) ()))
(ty$create 'USPECIAL nil nil)
(ty$create 'UAND '(USPECIAL) '(prop (obj) ()))
(ty$create 'UOR '(USPECIAL) '(prop (obj) ()))
(ty$create 'UNOT '(USPECIAL) '(prop (obj) ()))
(ty$create 'UDIST '(USPECIAL) '(prop (obj) ())) ; 'distinct'
(ty$create 'UPROC '(USPECIAL) '(prop (proc) ()))
(ty$create 'UEMPTY-SLOTS '(USPECIAL) '(prop (slots) ()))
(ty$create 'UIGNORE-SLOTS '(USPECIAL) '(prop (slots pattern) ()))
(ty$create 'UPATH '(USPECIAL) '(prop (path pattern) ()))
(ty$create 'UOLPATH '(USPECIAL) '(prop (link direction pattern) ()))
(ty$create 'UEVAL '(USPECIAL) '(prop (proc) ()))
;
; The below are used mostly for instantiation.
;
(ty$create 'USELECT '(USPECIAL) '(prop (pattern slot) ()))
(ty$create 'UCODE '(USPECIAL) '(prop (proc) ()))
(ty$create 'UBIND! '(USPECIAL) '(prop (var pattern) ()))

(setq *special-priorities*
  (list ^UOR ^UAND ^UNOT ^UDIST ^UPROC ^UEMPTY-SLOTS
        ^UIGNORE-SLOTS ^UPATH ^UOLPATH))

(ty$create 'PRULE nil '(nil (subgoal goal) ()))

(ty$create 'RULEOPER nil '(prop (obj) ()))
(ty$create 'RAND '(RULEOPER) nil)
(ty$create 'RSEQ '(RULEOPER) nil)
(ty$create 'ROR '(RULEOPER) nil)
(ty$create 'RNOT '(RULEOPER) nil)
(ty$create 'RTRUE '(RULEOPER) nil)
(ty$create 'RFALSE '(RULEOPER) nil)
(ty$create 'RCODE '(RULEOPER) nil)

; End of file.
