;*******************************************************************************
;
; Daydreamer
; Version 3.5
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
;*******************************************************************************

(progn
 (setq *dd-version* "DAYDREAMER 3.5, Common Lisp version of 2004-12-20")
 (format t "=======================~%")
 (format t "Loading ~A...~%" *dd-version*)
 (format t "=======================~%")
 nil)

(load "dd_macros")
(load "dd_cntrl")
(load "dd_epis")
(load "dd_mutation")
(load "dd_night")
(load "dd_reversal")
(load "dd_ri")
(load "dd_rule1")
(load "dd_rule2")
(load "dd_utils")

(do-interest #'interest)

(setq *subsets* *gate-load-options*)

(cond
  ((memq? 'lovers3 *gate-load-options*)
   (setq *gate-input* (open "inputlovers3.txt")))
  ((memq? 'lovers2 *gate-load-options*)
   (setq *gate-input* (open "inputlovers2.txt")))
  ((memq? 'lovers1 *gate-load-options*)
   (setq *gate-input* (open "inputlovers1.txt")))
  ((memq? 'employment1 *gate-load-options*)
   (setq *gate-input* (open "inputemployment1.txt")))
  ((memq? 'recovery3-alone *gate-load-options*)
   (setq *gate-input* (open "inputrecovery3.txt"))))

(epmem-init)

(load "dd_kb")
(load "dd_gen")

(setq *gen-stream* (make-gen-stream *gate-dbg*))

(format t "=======================~%")
(format t "Welcome to ~A~%" *dd-version*)
(format t "=======================~%")

; End of file.
