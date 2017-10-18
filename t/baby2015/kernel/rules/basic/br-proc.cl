;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     uralt
;; AUTHORS:  Franco di Primio, Juergen Walther

;; This file depends on:  common>*
;;                        rules>basic>data
;;                        rules>basic>rules
;;                        rules>basic>br-inter


;;-----------------------------------------------------------------------------
;;                   FLAVOR BASIC-RULE-PROCESSOR 
;;-----------------------------------------------------------------------------

(def$flavor basic-rule-processor
	   ()
	   (rule-interpreter)
  (:documentation "This flavor provides the functionality of rule-interpreter
and the standard methods for processors."))


(def$method (basic-rule-processor :reset-proc) ()
  "Reset the processor to initial state."
  ($send self :reset-data-base))

(def$method (basic-rule-processor :print) (&optional (stream *default-dialog-stream*))
  "Unparse rules and rulesets on <stream>."
  ($send self :unparse-rules stream))

(def$method (basic-rule-processor :kb-inform) (&optional (stream *default-dialog-stream*))
  "Print statistics about rules and rulesets on <stream>."
  ($send self :rule-statistics stream))


#-:FMCS(compile-$flavor-$methods basic-rule-processor)


;;; eof

