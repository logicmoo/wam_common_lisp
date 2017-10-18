;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; BASE: 10. ;Package: BABYLON -*-

(in-package "BABYLON")


;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHORS:  Franco di Primio, Eckehard Gross, Juergen Walther



;;;;;;;;; MIXIN FOR POSSIBLE and active VALUES ;;;;;;;;;;;;;;;;;;;;;;;;


(def$flavor normal-frame-mixin
	()
	(mini-frame-mixin)
  (:documentation "specialization of the frame processor mixin for the knowledge base.
generates a different type of frame processor."))

(def$method (normal-frame-mixin :generate-frame-processor) ()
  "generates a frame processor for a knowledge base."
  (setf frame-processor (make-$instance 'normal-frame-processor
				       :meta-processor self)))

(assign-typefkt 'frame-type 'normal-frame-mixin)

