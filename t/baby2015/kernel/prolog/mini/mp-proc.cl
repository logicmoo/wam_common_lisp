;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     uralt
;; AUTHORS:  Eckehard Gross 

;; This file depends on:  common>*
;;                        prolog>basic>*
;;                        prolog>mini>mp-trace

;; contents: a flavor combining basic-prolog-processor with a component
;;           which provides trace facilities.

;;--------------------------------------------------------------------------
;;                  FLAVOR MINI-PROLOG-PROCESSOR
;;--------------------------------------------------------------------------

(def$flavor mini-prolog-processor
	   ()
	(prolog-trace-mixin
         basic-prolog-processor)
  :settable-instance-variables
  (:documentation "this flavor combines basic-prolog-processor with a component
which provides trace facilities."))


;;--------------------------------------------------------------------------
;;                  FLAVOR MINI-GOALBOX
;;--------------------------------------------------------------------------

(def$flavor mini-goalbox
           ()
	(goalbox-trace-mixin
         basic-goalbox)
  (:documentation "this flavor combines goalbox-core with a component
which provides trace facilities."))

#-:FMCS(compile-$flavor-$methods mini-prolog-processor mini-goalbox)

;;; eof

