;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   H.W. G U E S G E N


;;--------------------------------------------------------------------------------



(def$flavor active-value-frame-core
	()
	(active-value-mixin frame-core)
  (:documentation "flavor to be used as basic flavor of each frame
instead of basic-frame, if possible values are to be supported."))



(def$flavor normal-frame-processor
	()
	(mini-frame-processor)
  (:documentation "specialization of basic frame processor generating frames
with possible value feature."))


(def$method (normal-frame-processor :after :init) (&rest plist)
  (declare (ignore plist))
  (setf frame-type 'active-value-frame-core))


#-:FMCS(compile-$flavor-$methods normal-frame-processor)

;;; eof

