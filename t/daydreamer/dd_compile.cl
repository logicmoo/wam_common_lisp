;*******************************************************************************
;
; Daydreamer
; Version 3.5
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
;*******************************************************************************

(setq *question-mark-atom* '?)
(load "compat")
(load "loop")
(load "gate_macros")
(load "dd_macros")


#|
(compile-file "compat")
(compile-file "loop")
(compile-file "gate_macros")
(compile-file "dd_macros")
|#

(compile-file "dd_cntrl")
;;(break)
(compile-file "dd_epis")
;; #- :abcl (compile-file "dd_gen")
;; #+ :wamcl 
(compile-file "dd_gen")
(compile-file "dd_mutation")
(compile-file "dd_night")
(compile-file "dd_reversal")
(compile-file "dd_reversal2")
(compile-file "dd_ri")
(compile-file "dd_utils")
(compile-file "dd_rule1")
(compile-file "dd_rule2")
(compile-file "dd_rule3")
(compile-file "dd_rule4")

; End of file.
