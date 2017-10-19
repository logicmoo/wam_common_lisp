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

(load "compat.cl")
(load "loop.cl")
(load "gate_macros.cl")

(compile-file "compat.cl")
(compile-file "loop.cl")
(compile-file "gate_macros.cl")
(compile-file "gate_cx.cl")
(compile-file "gate_instan.cl")
(compile-file "gate_main.cl")
(compile-file "gate_prove.cl")
(compile-file "gate_read_pr.cl")
(compile-file "gate_ty.cl")
(compile-file "gate_utils.cl")
(compile-file "gate_unify.cl")

; End of file.
