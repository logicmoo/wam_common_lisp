;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   H.W. Guesgen 


;;;
;;;     ACTIVE VALUES (combined with ACCESS TO ATTACHED CONSTRAINTS)

;;;     Beyond the facilities in file attachment-access,
;;;
;;;     - this file contains modified access methods for slots
;;;       with active values
;;;
;;;     - to use this new methods include the frame active-value-frame
;;;	into the supers-list of your frames


;;;     Since there is no adequate method combination facility provided
;;;     by the flavor system, the following code is a little bit tricky.
;;;     (Should be revisited when a new BABYLON frame formalism is implemented)

;;-------------------------------------------------------------------------------
;;     LISP stuff
;;-------------------------------------------------------------------------------

(defun is-active-value (value)
  (and (listp value)
       (eql (first value) 'active-value)))


(defun send-to-instance-or-self			
       (self instance-name method-name &rest args)
  (if (eq instance-name 'SELF)
       (lexpr-$send self method-name args)
       (lexpr-$send (get-instance instance-name) method-name args)))

;;-------------------------------------------------------------------------------
;;    FRAME
;;-------------------------------------------------------------------------------

(def$flavor active-value-mixin 
	()
	(poss-val-mixin)
  (:required-instance-variables object-name)
  (:documentation "flavor to be used as basic flavor of each frame
instead of basic-frame, if possible values are to be supported."))


;;-------------------------------------------------------------------------------
;;     :GET stuff
;;-------------------------------------------------------------------------------

(def$method (active-value-mixin :get)
	     (slot-name &optional (prop-name :value))
  ":get method that regards active values and constraint attachments."

  (if (and (is-value prop-name)
	   (eq (flavor-type-of ($send self :get-value-only slot-name))
	       'restricted-slot))
      ($send self :active-value-get
	    ($send ($send self :get-value-only slot-name) :get)
	    slot-name :value)                                     ; constraint attachment
      ($send self :active-value-get
	    (get-value-only (get-slot-plist slot-name) prop-name)
	    slot-name prop-name)))


(def$method (active-value-mixin :active-value-get)
	     (value slot-name prop-name)
  "evaluates an active-value construct recursively."

  (if (is-active-value value)
      ($send self ($send self :get-behavior value)
	    ($send self :active-value-get ($send self :local-value value)
		  slot-name prop-name)
	    value prop-name slot-name)
      value))


(def$method (active-value-mixin :get-behavior)
	     (active-value)
  "selects the :get behavior."

  (if (null (third active-value))
      :default-get-behavior
      (third active-value)))


(def$method (active-value-mixin :default-get-behavior)
	   (old-local-value active-value prop-name slot-name)
  (declare (ignore active-value prop-name slot-name))
  old-local-value)


(def$method (active-value-mixin :local-value)
	     (active-value)
  "selects the local value."

  (second active-value))

;;-------------------------------------------------------------------------------
;;            :SET  AND :PUT   STUFF
;;-------------------------------------------------------------------------------

(def$method (active-value-mixin  :set)      
	   (slot-name new-value &optional (prop-name :value) (test nil))  
  "sets the value of a slot without checking
   whether the value is a possible value for the slot.
   active values are regarded
   furthermore it is checked whether all attached constraints are satisfied
   if the option test is equal to :test"
  
  (cond ((is-value prop-name)	 
	 (cond ((eq (flavor-type-of ($send self :get-value-only slot-name))
		    'restricted-slot)
		($send ($send self :get-value-only slot-name) :put
		      self slot-name
		      ($send self :active-value-set	; handles active value
			    new-value
			    ($send self :get-value-only slot-name)
			    slot-name prop-name)
		      test))
	       (t ($send self :set-value-only slot-name
			($send self :active-value-set	; handles active value
			      new-value
			      ($send self :get-value-only slot-name)
			      slot-name prop-name)
			prop-name))))	
	(t ($send self :set-value-only slot-name
		 ($send self :active-value-set new-value	; handles active value
		       ($send self :get-value-only slot-name prop-name)
		       slot-name prop-name)
		 prop-name))))


(def$method (active-value-mixin  :put-if-satisfied)
	   (slot-name new-value &optional (prop-name :value))
  "sets the value of a slot, if the attached constraints are satisfied."
  ($send self :put slot-name new-value prop-name :test))



(def$method (active-value-mixin :put)      
	   (slot-name new-value &optional (prop-name :value) (test nil))
  "sets thevalue of a slot checking
   whether the value is a possible value for the slot.
   active values are regarded
   furthermore it is checked whether all attached constraints are satisfied
   if the option test is equal to :test."
  (if (is-value prop-name) ;; do constraints check
      (let ((check-result
	      ($send self :check-correct-value slot-name new-value)))
	(if (not (eq '$ABORT$ check-result))
	    ($send self :set
			  slot-name (get-check-result check-result) prop-name test)))
      ($send self :set slot-name new-value prop-name)))


(def$method (active-value-mixin :active-value-set)
	   (new-value old-value slot-name prop-name)  
  "evaluates an active-value construct recursively."
  
  (if (is-active-value old-value)
      `(active-value
	 ,($send self :active-value-set
		($send self ($send self :put-behavior old-value)
		      new-value old-value prop-name slot-name)
		($send self :local-value old-value)
		slot-name prop-name)
	 ,($send self :get-behavior old-value)
	 ,($send self :put-behavior old-value))
      new-value))


(def$method (active-value-mixin :put-behavior)
	     (active-value)
  "selects the :put behavior."

  (if (null (fourth active-value))
      :default-put-behavior
      (fourth active-value)))


(def$method (active-value-mixin :default-put-behavior)
	     (new-local-value active-value prop-name slot-name)
  (declare (ignore active-value prop-name slot-name))
  new-local-value)

;;-------------------------------------------------------------------------------
;;       SOME STANDARD GET BEHAVIORS
;;-------------------------------------------------------------------------------

(def$method (active-value-mixin :first-fetch)
	     (old-local-value active-value prop-name slot-name)
  (declare (ignore active-value))
  ($send self :replace slot-name (eval old-local-value) prop-name))


(def$method (active-value-mixin :get-indirect)
	     (old-local-value active-value prop-name slot-name)
  (declare (ignore active-value prop-name slot-name))
  (let ((instance-name (first old-local-value))
	(slot-name (second old-local-value))
	(prop-name (or (third old-local-value) :value)))
    (send-to-instance-or-self self instance-name :get slot-name prop-name)))


;;-------------------------------------------------------------------------------
;;        SOME STANDARD PUT BEHAVIORS
;;-------------------------------------------------------------------------------

(def$method (active-value-mixin :put-indirect)
	     (new-local-value active-value prop-name slot-name)
  (declare (ignore prop-name slot-name))
  (let ((old-local-value ($send self :local-value active-value)))
    (let ((instance-name (first old-local-value))
	  (slot-name (second old-local-value))
	  (prop-name (or (third old-local-value) :value)))
      (send-to-instance-or-self self instance-name
				:set slot-name new-local-value prop-name)
      old-local-value)))


; does not work with the actual version of active values
;
;(def$method (active-value-mixin :replace-me)
;	     (new-local-value active-value prop-name slot-name)
;  (declare (ignore active-value))
;  ($send self :replace slot-name new-local-value prop-name))


(def$method (active-value-mixin :no-update-permitted)
	     (new-local-value active-value prop-name slot-name)
  (declare (ignore active-value))
  (baberror (getentry no-update-permit-error-fstr frame-io-table)
	 slot-name
	 prop-name
	 object-name
	 new-local-value))

;;-------------------------------------------------------------------------------
;;         CHECK  STUFF
;;-------------------------------------------------------------------------------


(def$method (active-value-mixin :check-init-value) (slot value)
  "internal method. checks whether value is a possible value for slot.
produces an error if the check fails."
  (let ((possible-values ($send self :get slot :possible-values)))
    (cond ((null possible-values))
	  ((not (is-method-of self (get-poss-val-type possible-values)))
	   (baberror (getentry unknown-poss-val-method-fstr frame-io-table)
		  possible-values
		  slot
		  object-name
		  (flavor-type-of self)))
	  ((is-active-value value))
	  (($send self :check-value slot value possible-values))
	  (t (baberror (getentry constraints-violation-fstr frame-io-table)
		     value 
		     (get-poss-val-type possible-values)
		     (get-poss-val-args possible-values)
		     slot
		     object-name
		     (flavor-type-of self))))))


(defun normalize-plist-with-act-vals (plist)
  (cond ((null plist) plist)
	((atom plist) `(:value ,plist))
	((is-multiple-value plist) `(:value ,plist))
	((is-active-value plist) `(:value ,plist))
	((is-value (first plist)) plist)
	(t `(:value . ,plist))))


(def$method (active-value-mixin :init-slot) (slot-name slot-spezification check)
  "initializes a slot with values from slot-spezification."  
  ;;reverse bewirkt, dass :possible-values vor :value initialisiert wird
  ;;allerdings sollten :possible-values per frame definition eingefuehrt werden

  (do ((plist (reverse (normalize-plist-with-act-vals slot-spezification))  
	      (rest (rest plist))))
      ((null plist))
    (cond ((is-active-value (first plist))
	   ($send self :replace slot-name (first plist) (second plist)))
	  (t (if (and check (eq (second plist) :value))
		 ($send self :check-init-value slot-name (first plist)))
	     ($send self :set slot-name (first plist) (second plist))))))
