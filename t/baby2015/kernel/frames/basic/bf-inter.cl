;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     uralt
;; AUTHOR:   Franco di Primio, Eckehard Gross

;; This file depends on:  common>*
;;                        frames>basic>frames
;;                        
;; Contents: a frame interpreter


(def$flavor frame-interpreter
	()()
  (:required-instance-variables meta-processor)
  (:documentation "flavor providing access and modification methods for frames."))


(def$method (frame-interpreter :get-value-only)
	   (instance-name slot-name &optional (prop-name :value))
  "basic access method for a property of a slot of an instance."
  ($send (get-instance instance-name) :get-value-only slot-name prop-name))

(def$method (frame-interpreter :get)
	   (instance-name slot-name &optional (prop-name :value))
  "access method for a property of a slot of an instance."  
  ($send (get-instance instance-name) :get slot-name prop-name))


(def$method (frame-interpreter :replace)
	   (instance-name slot-name value &optional (prop-name :value))
  "basic modification method for a property of a slot of an instance." 
  ($send (get-instance instance-name) :replace slot-name value prop-name))

(def$method (frame-interpreter :set)
	   (instance-name slot-name value &optional (prop-name :value))
  "modification method for a property of a slot of an instance."
  ($send (get-instance instance-name) :set slot-name value prop-name))

(def$method (frame-interpreter :put)
	   (instance-name slot-name value &optional (prop-name :value))
  "modification method for a property of a slot of an instance."
  ($send (get-instance instance-name) :put slot-name value prop-name))


(def$method (frame-interpreter :delete-property)
	   (instance-name slot-name prop-name)
 "deletes a property of a slot of an instance."
  ($send (get-instance instance-name) :delete-property slot-name prop-name))

(def$method (frame-interpreter :type) (instance-name &optional a-frame-name)
   "provides the type of an instance or checks whether the instance is of specified type."
  ($send (get-instance instance-name) :type a-frame-name))


(def$method (frame-interpreter :ask)
	   (frame-reference &optional (negation-flag nil))
  "asks the user for the value (of a property) of a slot of an instance."
  (let ((instance-name (first frame-reference))
	(slot-name (second frame-reference))
	(args (rest (rest frame-reference))))
    (if (not slot-name)
	($send (get-instance instance-name) :ask-for-slot-values)
	(if (listp slot-name)
	    ($send (get-instance instance-name)
		  :ask-for-slot-values slot-name)
            ($send (get-instance instance-name)
		  :ask slot-name args negation-flag)))))

(def$method (frame-interpreter :eval-reference)
	    (frame-reference &optional (mode :recall))
  "generic method to get or set a property of a slot of an instance."
  (let ((instance-name (first frame-reference))
	(slot-or-method (second frame-reference))
	(args (rest (rest frame-reference))))
    (cond ((is-user-defined-method slot-or-method)
           (lexpr-$send (get-instance instance-name) slot-or-method 
                        (eval `(list ,@args ,mode))))  ; to evaluate args
          ((null (rest args))
           ($send (get-instance instance-name) :get
                  slot-or-method (or (first args) :value)))
	  (t (let ((normed-args (normalize-args args)))
               (case mode
                 ((:recall :remember)
                  ($send (get-instance instance-name) mode
                         slot-or-method
                         (internal-relation-name (second normed-args))
                         (third normed-args)
                         (first normed-args)))  ; property
                 (:store
                  ($send (get-instance instance-name) mode
                         slot-or-method
                         (third normed-args)
                         (first normed-args)))  ; property
                 (t (baberror (getentry mode-error-fstr frame-io-table)
                              mode))))))))

;;; eof

