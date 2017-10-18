;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; BASE: 10. ;Package: BABYLON -*-


(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     April 1987
;; AUTHORS:  Franco di Primio, Eckehard Gross

;; This file depends on:  common>*
;;                        frames>basic>frames
;;                        frames>basic>bf-mixin
;;                        frames>mini>mf-proc
;;                        

;; contents: a mixin making the facilities of mini-frame-processor available
;;           for a knowledge base.
;;           mini-frame-processor is a specialization of basic-frame-processor
;;           generating frames which allow that possible values are specified
;;           for the :value property of their slots.

;;--------------------------------------------------------------------------
;;                   FLAVOR MINI-FRAME-MIXIN
;;--------------------------------------------------------------------------


(def$flavor mini-frame-mixin
	()
	(basic-frame-mixin)
  (:documentation "This mixin makes the facilities of mini-frame-processor available,
which is a specialization of basic-frame-processor generating frames which allow
that possible values are specified for the :value property of their slots."))

(def$method (mini-frame-mixin :generate-frame-processor) ()
  "generates an instance of mini-frame-processor."
  (setf frame-processor (make-$instance 'mini-frame-processor
				       :meta-processor self)))

(assign-typefkt 'frame-type 'mini-frame-mixin)
