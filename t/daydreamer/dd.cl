;*******************************************************************************
;
; Daydreamer
; Version 3.5
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
;*******************************************************************************

;
; Traces in book:
; lovers1 - "I want to be going out with someone..."
; revenge1 - "I am a movie star..."
; rationalization1 - "He would go to Cairo..."
; rationalization2 - "I remember the time my being turned down by Irving..."
; rationalization3 - "Anyway, I was well dressed..."
; roving1 - "I remember the time Steve told me..."
; recovery2 = oseren? - "I have to call him..."
; recovery3 - "I have the UCLA Alumni directory..."
; revenge3 - "I remember the time I got even with..."
; computer-serendipity - "I remember the time Harold and I broke the ice..."
;
; Additional traces in dissertation:
; employment1 (dissertation only) - "I want to have enough money..."
;
;
; all -- every rule in a complete DAYDREAMER run
; always -- rule should always be loaded in any run
; employment1-revenge
; mut -- action mutations
; mut-alone -- action mutations only
; mut4 -- action mutation example 4
; mut5 -- action mutation example 5
; rain --- Can be used in conjunction with recovery3
; unused -- rule is currently never used
;

(setq *gate-load-options* '(always
                            lovers1
                            rationalization1
                            rationalization2
                            rationalization3
                            revenge1))

(load "gate_get.cl")
(load "dd_get.cl")
(daydreamer)

; End of file.
