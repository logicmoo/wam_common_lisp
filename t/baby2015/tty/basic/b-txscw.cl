;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1987   BY
;;           G M D 
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;; DATE:     December 1987
;; AUTHOR:   E. Gross

;; This file depends on: common>*>flavors

;; Contents: a pseudo window for traces and explanations.

;;-------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------

(def$flavor basic-txsc-window
	((stream *default-dialog-stream*))
	()
  :settable-instance-variables
  (:documentation "a pseudo window for traces and explanations.
                  output is written to stream."))


(def$method (basic-txsc-window :convert-sc-window-item) (arguments)
  "internal method.
converts an item for a mouse sensitive scroll window into a string."
  (apply #'concatenate 'string (mapcar #'(lambda (item)
					   (second (member :string item)))
				       arguments)))


(def$method (basic-txsc-window :format) (&rest arguments)
  "writes to stream. arguments might be items for a text scroll window or a format-string
with arguments as required by the format function."
  (terpri stream)
  (cond ((stringp (first arguments))	 
	 (apply #'format stream  (first arguments) (rest arguments))
         (force-output stream))
	(t (format stream "~A" ($send self :convert-sc-window-item arguments)))))


(def$method (basic-txsc-window :clear) ()
  "dummy method."
  t)

(def$method (basic-txsc-window :expose) ()
  "dummy method."
  t)

(def$method (basic-txsc-window :bury) ()
  "dummy method."
  t)