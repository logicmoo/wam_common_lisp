;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     uralt
;; AUTHOR:   Eckehard Gross

;; This file depends on:  common>*
;;                        prolog>basic>axioms
;;                        prolog>basic>ax-sc
;;                        prolog>basic>bp-inter

;; Contents: a minimal version of a prolog processor

;;-------------------------------------------------------------------------------
;;                   FLAVOR BASIC-PROLOG-PROCESSOR
;;-------------------------------------------------------------------------------

(def$flavor basic-prolog-processor
	   ()
	(prolog-interpreter)
  :settable-instance-variables
  (:documentation "This flavor represents a minimal version of a prolog processor."))


(def$method (basic-prolog-processor :babylon-format) (&rest args)
  (lexpr-$send meta-processor :babylon-format args))

(def$method (basic-prolog-processor :babylon-read) (key-list)
  ($send meta-processor :babylon-read key-list))


(def$method (basic-prolog-processor :reset-proc) ()
  ($send self :reset-axiom-sets))

#-:FMCS(compile-$flavor-$methods basic-prolog-processor)

;;; eof

