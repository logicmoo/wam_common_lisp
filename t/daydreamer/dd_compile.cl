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
(load "compat.cl")
(load "loop.cl")
(load "gate_macros.cl")
(load "dd_macros.cl")

(compile-file "dd_cntrl.cl")
(compile-file "dd_epis.cl")
(compile-file "dd_gen.cl")
(compile-file "dd_mutation.cl")
(compile-file "dd_night.cl")
(compile-file "dd_reversal.cl")
(compile-file "dd_ri.cl")
(compile-file "dd_utils.cl")
(compile-file "dd_rule1.cl")
(compile-file "dd_rule2.cl")

; End of file.
