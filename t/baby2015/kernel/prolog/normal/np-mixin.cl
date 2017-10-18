;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   Eckehard Gross

  

(def$flavor normal-prolog-mixin
        ((explanation-window nil)
	 (prolog-ted nil))
	(mini-prolog-mixin)
  :settable-instance-variables
  (:required-instance-variables procs system-trace kb-name)
  (:documentation "A basic prolog processor mixin for the knowledge base flavor."))

 
(def$method (normal-prolog-mixin :generate-prolog-processor) ()
  "generates a prolog-processor binding it to the instance variable prolog-processor."
  (setf prolog-processor (make-$instance 'normal-prolog-processor
					:meta-processor self
					:alternate-meta-processor
					(make-$instance 'kb-stub
						       :meta-processor self))))

(def$method (normal-prolog-mixin :send-explanation-window) (selector &rest args)
  (when explanation-window
    (if (eq selector :format)
	($send explanation-window :expose))
    (lexpr-$send explanation-window selector args)))

;; ************* METHODS FOR GOAL EVALUATION *******************

(assign-typefkt 'prolog-type 'normal-prolog-mixin)


(def$method (normal-prolog-mixin :prolog-why) ()
  "provides context explanations"
  ($send prolog-processor :why))



;; ************** EVALUATION METHODS ****************************

(def$method (normal-prolog-mixin :display-prooftree) ()
  ($send prolog-processor :send-if-handles :display-prolog-tree))

(defun display-prooftree ()
  (and (warn-if-no-prolog)
       (send-current-knowledge-base :display-prooftree)))

