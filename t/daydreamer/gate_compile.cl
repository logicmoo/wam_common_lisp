;*******************************************************************************
;
; GATE
; Version 2.3
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
;*******************************************************************************

(setq *question-mark-atom* '?)

(load "compat")
(load "loop")
(load "gate_macros")

(compile-file "compat")
(compile-file "loop")
(compile-file "gate_macros")
#-abcl (compile-file "gate_cx")
(compile-file "gate_instan")
(compile-file "gate_instan2")
(compile-file "gate_main")
(compile-file "gate_prove")
(compile-file "gate_read_pr")
(compile-file "gate_ty")
(compile-file "gate_utils")
(compile-file "gate_unify")

; End of file.
