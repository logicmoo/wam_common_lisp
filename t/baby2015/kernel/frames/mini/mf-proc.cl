;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     April 1987
;; AUTHORS:  Franco di Primio, Eckehard Gross

;; This file depends on:  common>*
;;                        frames>basic>*
;;                        frames>mini>mf-proc
;;                        
;; Contents: a specialization of basic-frame-processor generating frames
;;           which allow that possible values are specified for the
;;           :value property of their slots.


;;--------------------------------------------------------------------
;;                   MINI-FRAME-PROCESSOR 
;;--------------------------------------------------------------------

(def$flavor mini-frame-processor
	()
	(basic-frame-processor)
  (:documentation "specialization of basic-frame-processor generating frames
with possible value feature."))


(def$method (mini-frame-processor :after :init) (&rest plist)
  (declare (ignore plist))
  (setf frame-type 'poss-val-frame-core))

;;--------------------------------------------------------------------
;;                   BASE FLAVOR POSS-VAL-FRAME-CORE 
;;--------------------------------------------------------------------


(def$flavor poss-val-frame-core
	()
	(poss-val-mixin frame-core)
  (:documentation "flavor to be used as base flavor of each frame
instead of frame-core, if possible values are to be supported."))



#-:FMCS(compile-$flavor-$methods  mini-frame-processor)

;;; eof

