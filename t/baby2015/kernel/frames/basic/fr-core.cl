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
;;                        
;; Contents: a flavor used as base component of each frame.
;;           it provides methods common to all frames.

;;--------------------------------------------------------------------
;;                   BASE FLAVOR OF FRAME 
;;--------------------------------------------------------------------

(def$flavor frame-core
	(slots
	 object-name)
	()
  :initable-instance-variables
  :gettable-instance-variables
  (:documentation "flavor to be used as base component of each frame.
methods common to all frames are implemented as methods of this flavor."))


(defun get-slots (instance)
  (remove 'slots ;; internal use
	  (remove 'object-name ;; internal use
		  (get-flavor-instance-slots instance))))

(def$method (frame-core :after :init) (&rest plist)
  "stores user defined slots in instance variable slots."
  (declare (ignore plist))
  (setf slots (get-slots self)))

;;; modified to be able to define reset-yourself methods/demons for frames
(def$method (frame-core :reset-yourself) ()
  "resets instance to initial values of the definstance form."
  (eval (get-instance-def object-name)))

(def$method (frame-core :%set-object-name) (x)
  (setf object-name x))

;;--------------------------------------------------------------------
;;                   BASIC ACCESS METHODS 
;;--------------------------------------------------------------------


#+:SABN(defmacro get-slot-plist (slot-name)	;using symbol-value-in-$instance macro
        `(progn (assert (member ,slot-name slots)
                        (,slot-name)
                        "~S is not a slot of instance  ~S ~@
                   the slots are: ~{~S ~}"	;assert does not evaluate this arg
                        ,slot-name object-name slots)
                (symbol-value-in-$instance self ,slot-name)))


#-:SABN(defmacro get-slot-plist (slot-name)	;using $slot macro
        `(progn (assert (member ,slot-name ($slot 'slots))
                        (,slot-name)
                        "~S is not a slot of instance  ~S ~@
                   the slots are: ~{~S ~}" 	;assert does not evaluate this arg
                        ,slot-name ($slot 'object-name) ($slot 'slots))
                ($slot ,slot-name)))

(defun get-value-only (slot-plist prop-name)
  (cond ((is-value prop-name) (first slot-plist))
	((keywordp prop-name)
	 (getf (rest slot-plist) prop-name))))

(defun set-value-only (slot-plist value prop-name)
  (cond ((is-value prop-name) (setf (first slot-plist) value))
	((keywordp prop-name)	 
	 (setf (getf (rest slot-plist) prop-name) value))))

(def$method (frame-core :get-value-only)
	   (slot-name &optional (prop-name :value))
  "basic access method to a property of a slot."
  (get-value-only (get-slot-plist slot-name) prop-name))

(def$method (frame-core :get) 
	   (slot-name &optional (prop-name :value))
  "access method to a property of a slot. intended to be specialized."
  (get-value-only (get-slot-plist slot-name) prop-name))



(def$method (frame-core :get-properties) (slot-name)
  "provides all properties of a slot."
  (do ((plist (rest (get-slot-plist slot-name))
	      (cddr plist))
       (result (list :VALUE)))
      ((null plist) (nreverse result))
    (setf result (cons (first plist) result))))
 

(def$method (frame-core :set-value-only)
	   (slot-name value &optional (prop-name :value))
  "basic modification method for a property of a slot."
  (set-value-only (get-slot-plist slot-name) value prop-name))

(def$method (frame-core :replace)
	   (slot-name value &optional (prop-name :value))
  "basic modification method for a property of a slot."
  (set-value-only (get-slot-plist slot-name) value prop-name))

(def$method (frame-core :set) 
	   (slot-name value &optional (prop-name :value))
  "modification method for a property of a slot. intended to be specialized." 
  (set-value-only (get-slot-plist slot-name) value prop-name))

(def$method (frame-core :put) 
	   (slot-name value &optional (prop-name :value))
  "modification method for a property of a slot. intended to be specialized."
  (set-value-only (get-slot-plist slot-name) value prop-name))


;;;;; sehr problematisch !!!!!!!

(def$method (frame-core :reset-slots) (&optional (prop-name :value))
  "sets all user defined slots to undetermined."
  (mapc #'(lambda (a-slot-name)
	    (let ((slot-plist (get-slot-plist a-slot-name)))
	      (set-value-only slot-plist (undetermined) prop-name)))	      
	slots))

;;--------------------------------------------------------------------


(def$method (frame-core :delete-property)
	   (a-slot-name prop-name)
  "deletes a property of a slot. :value property can't be deleted."
  (if (is-value prop-name)
      (baberror (getentry no-delete-permit-error-fstr frame-io-table)
	     object-name
	     a-slot-name
	     prop-name)
      (remf (rest (get-slot-plist a-slot-name)) prop-name)))


(def$method (frame-core :type) (&optional a-frame-name)
 "provides the type of the receiver or checks whether the receiver is of specified type."
  (if a-frame-name
      (flavor-typep self (get-frame-name-with-check a-frame-name))
      (%get-frame-name (flavor-type-of self))))

;;--------------------------------------------------------------------
;;                   GET-VALUE, PUT-VALUE, $VALUE 
;;--------------------------------------------------------------------

(defmacro GET-VALUE (instance-name slot-name
		     &optional (prop-name :value))
  `($send (get-instance ,instance-name) :get ,slot-name ,prop-name))

(defmacro PUT-VALUE (instance-name slot-name new-value
		     &optional (prop-name :value))
  `($send (get-instance ,instance-name) :put ,slot-name ,new-value ,prop-name))

;; FUER SETF
;; anstatt:
;;  (put-value <instance-name> <slot-name> <new-value> {<prop-name>})
;;  (setf (get-value <instance-name> <slot-name> {<prop-name>}) <new-value>)


(defsetf get-value (object-name slot-name &optional (prop-name :VALUE))
	 (new-value)
  `(put-value ,object-name ,slot-name ,new-value ,prop-name))


(defmacro $VALUE (slot-name &optional (prop-name :value)) 
  ;; das ist zu gebrauchen innerhalb von Behaviors
  `($send self :GET ,slot-name ,prop-name))


(defmacro $SETF-VALUE (slot-name new-value &optional (prop-name :value))
  `($send self :SET ,slot-name ,new-value ,prop-name))


;; FUER SETF

(defsetf $VALUE (slot-name &optional (prop-name :value))
	 (new-value)
  `($send self :SET ,slot-name ,new-value ,prop-name))



;;--------------------------------------------------------------------
;;                   INITIALISATION
;;--------------------------------------------------------------------


(def$method (frame-core :check-your-self) ()
  "dummy method called on initialization.
intended to be specialized by the user."
  t)
  

(defun normalize-plist (plist)
  (cond ((null plist) plist)
	((atom plist) `(:value ,plist))
	((is-multiple-value plist) `(:value ,plist))
	((is-value (first plist)) plist)
	(t `(:value . ,plist))))

(def$method (frame-core :init-slot) (slot-name slot-spezification check)
  (declare (ignore check))
  (do ((plist (normalize-plist slot-spezification)
	      (rest (rest plist))))
      ((null plist))
    ($send self :set slot-name (second plist) (first plist))))

(def$method (frame-core :init-all-slots) (slot-specifications &optional (check nil))
  "initializes all user defined slots using slot-specifications."
  (unless (null slot-specifications)
    ($send self :init-slot
	  (first slot-specifications) (second slot-specifications) check)
    ($send self :init-all-slots (rest (rest slot-specifications)) check)))


(def$method (frame-core :initialize) (with-specification)
  "dummy method. to be used as basic method for user defined daemons." 
  with-specification)

;;-----------------------------------------------------------------------------
;;                       UNPARSING INSTANCES
;;-----------------------------------------------------------------------------

(def$method (frame-core :internal-properties) ()
  nil)

(def$method (frame-core :unparse-slot)
	   (slot-name &optional (all-properties t) internal-properties)
  (let ((header `(,slot-name :value ,($send self :get-value-only slot-name))))
    (setf internal-properties (or internal-properties
				  ($send self :internal-properties)))
    (cond ((null all-properties) header)
	  (t (append header
		     (do ((plist (rest (get-slot-plist slot-name))
				 (cddr plist))
			  (result nil))
			 ((null plist) (reverse result))
		       (unless (member (first plist) internal-properties)
			 (setf result (cons (second plist)
					    (cons (first plist)
						  result))))))))))
 
(def$method (frame-core :unparse-instance)
	   (&optional slot-list (all-properties t) internal-properties)
  (setf internal-properties (or internal-properties
				($send self :internal-properties)))
  (append `(,object-name ,(%get-frame-name (flavor-type-of self)))
	  (mapcar #'(lambda (slot)
		      ($send self :unparse-slot
				    slot all-properties internal-properties))
		  (or slot-list slots))))

;;-----------------------------------------------------------------------------
;;                       ASKING FOR SLOT VALUES 
;;-----------------------------------------------------------------------------


(def$method (frame-core :ask-for-slot-values) (&optional list)
  (do ((rslots (or list slots) (rest rslots)))
      ((null rslots))
    ($send self :ask (first rslots))))

(def$method (frame-core :ask)
	    (slot &optional arg negation-flag (standard-option nil))
  "asks the user for the value (of a property) of a slot."
  (if (and (atom arg)
	   (not (null arg)))
      (if (is-facet arg)
	  (setq  arg (list arg))
	  (baberror (getentry wrong-arg-type-error-fstr frame-io-table)
		 arg object-name slot arg)))
  (if (and (not (null arg))
	   (is-facet (first arg)))
      ;; Dies erlaubt jetzt, beliebige property zu erfragen
      ($send self :ask-for-slot-property
	     slot
	     (first arg)			; a property
	     (rest arg)				; the desired value
	     negation-flag
	     standard-option)
      ($send self :ask-for-slot-value
	     slot arg negation-flag standard-option)))

(defun format-expectations (desired-value negation-flag restrict-method args)
  (concatenate 'string
	       (if restrict-method
		   (format nil
			   (getentry expect-restricted-value-fstr frame-io-table)
			   restrict-method args args)
		   (format nil (getentry expect-no-restricted-value-fstr frame-io-table)))
	       (if desired-value
		   (if negation-flag
		       (format nil 
			       (getentry expected-value-not-str frame-io-table)
			       desired-value)
		       (format nil 
			       (getentry expected-value-str frame-io-table)
			       desired-value))
		   "")))

(def$method (frame-core :ask-for-slot-property)
	    (slot prop-name &optional desired-value negation-flag standard-option)
  "asks the user for the value of a property of a slot."
  (cond ((is-value prop-name)
	 ($send self :ask-for-slot-value
		slot desired-value negation-flag standard-option))
	(t (send-kb :babylon-format
		    (getentry ask-slot-prop-fstr  frame-io-table)
		    prop-name slot object-name)
	   (let ((result (send-kb :babylon-read (list *c-help-key*))))
	     (cond ((eql result *c-help-key*)
		    (send-kb :babylon-format 
			     (format-expectations desired-value negation-flag nil nil))
		    ($send self :ask-for-slot-property
			   slot
			   prop-name
			   desired-value
			   negation-flag
			   standard-option))
		   ((is-help  result) result)
		   (t ($send self :set slot result prop-name)))))))

(defun substitute-o-and-s (object-name slot-name list)
  (sublis `((O . ,object-name) (S . ',slot-name)) list))

;(defun format-translate (slot object-name)
;  (let ((ask-declaration ($send (get-instance object-name) :get slot :ask)))
;    (if ask-declaration ;; (<format-string> . <args>)
;	;; <args> := O "Der Name des Objektes" 
;	;; <args> := S "Der Name des Slots" 
;	(apply #'format nil (substitute-o-and-s object-name slot ask-declaration))
;	(format nil
;		(getentry whats-the-value-of-fstr frame-io-table)
;		object-name slot))))

(def$method (frame-core :prompt-for-value) (slot)
  (let* ((ask-declaration ($send self :get slot :ask))
	 ;; (<format-string> . <args>)
	 ;; <args> := O "Der Name des Objektes"
	 ;; <args> := S "Der Name des Slots"
	 (prompt (substitute-o-and-s object-name slot ask-declaration)))
    (if prompt 
	(send-kb :babylon-format "~?" (first prompt) (eval `(list ,@(rest prompt))))
	(send-kb :babylon-format 
		 (getentry ask-slot-fstr frame-io-table)
		 slot object-name))))

(def$method (frame-core :ask-for-slot-value)   
	   (slot &optional desired-value negation-flag (standard-option nil))
  "asks the user for the value  of <slot>."
  ($send self :prompt-for-value slot)
  (let ((result (send-kb :babylon-read (list *c-help-key*))))
    (cond ((eql result *c-help-key*)
	   (send-kb :babylon-format 
		    (format-expectations desired-value negation-flag nil nil))
	   ($send self :ask-for-slot-value 
		 slot desired-value negation-flag standard-option))
	  ((is-help result) 'help)
	  (t ($send self :set slot result)))))


;;-----------------------------------------------------------------------------
;;                       HANDLING OF SLOT MESSAGES 
;;-----------------------------------------------------------------------------


;; (<- <instance-name> :<method-name> . args)
;; (<- <instance-name> <slot-name> {<facet>} <relation> <expr> {<mode>})
;;
;; <facet> defaults to :value
;;
;; <expr> := atom | frame-reference | behavior-reference | lisp-expression
;;
;; frame-reference := (<obj> <slot> {<facet>} <relation> <expr>)
;;
;; behavior-reference := (<obj> <method-name> . <args>)


(defmacro normalize-args (args)
  `(cond ((is-facet (first ,args))  ,args)
         ((is-path  (first ,args))  ,args)
         (t (cons :value ,args))))


(defmacro check-for-equal-relation (relation)
  `(or (member ,relation '(= is))
       (baberror (getentry expect-relation-fstr frame-io-table) ,relation)))


(defmacro <--  (instance-name slot-or-method &rest args)
  (cond ((is-user-defined-method slot-or-method)
         `($send (get-instance-or-self ,instance-name) ,slot-or-method ,@args))
        ((null (rest args))
         `($send (get-instance-or-self ,instance-name) :get
                 ',slot-or-method ,(or (first args) :value)))
        (t (let* ((normed-args (normalize-args args))
                  (relation (second normed-args))
                  (mode (or (fourth normed-args) :recall)))
             (case mode
               (:recall `($send (get-instance-or-self ,instance-name) ,mode
                                ',slot-or-method
                                ',(internal-relation-name relation)
                                ',(third normed-args)
                                ,(first normed-args)))  ; property
               (:remember 
                (if (check-for-equal-relation relation)
                  `($send (get-instance-or-self ,instance-name) ,mode
                          ',slot-or-method
                          ',(internal-relation-name relation)
                          ',(third normed-args)
                          ,(first normed-args))))    ; property
               (:store
                (if (check-for-equal-relation relation)
                  `($send (get-instance-or-self ,instance-name) ,mode
                          ',slot-or-method
                          ',(third normed-args)
                          ,(first normed-args))))    ; property
               (t (baberror (getentry mode-error-fstr frame-io-table) mode)))))))

(defmacro <- (instance-name slot-or-method &rest args)
  `(<-- ,instance-name ,slot-or-method ,@args))

(defun compute-list-expr (expr self)
  (let ((first-expr (first expr)))
    (cond ((eq first-expr 'SELF)
           (if (is-user-defined-method (second expr))
             (lexpr-$send self (second expr) (eval `(list ,@(cddr expr))))
             (lexpr-$send self :get (rest expr))))
          ((is-instance first-expr)
           (if (is-user-defined-method (second expr))
             (lexpr-$send (get-instance first-expr) (second expr)
                          (eval `(list ,@(cddr expr))))
             (lexpr-$send (get-instance first-expr) :get (rest expr))))
          ((member first-expr  '($E $EVAL))
           (eval (second expr)))	  
          (t expr))))

(def$method (frame-core :slot-message) (slot-name &rest args)
  "generic method to get or set a property of a slot.
args := {<facet>} <relation> <expr> {<mode>}"
  (let* ((normed-args (normalize-args args))
         (prop-name (first normed-args))
         (relation (second normed-args))
         (expr (third normed-args))
         (mode (or (fourth normed-args) :recall)))
    (cond ((null relation)                
           ($send self :get slot-name prop-name)) 
          ((eq mode :recall)
           ($send self :recall slot-name relation expr prop-name))
          ((eq mode :remember)
           (and (check-for-equal-relation relation)
                ($send self :remember slot-name relation expr prop-name)))
          ((eq mode :store)
           (and (check-for-equal-relation relation)
                ($send self :store slot-name expr prop-name)))	  
          (t (baberror (getentry mode-error-fstr frame-io-table) mode)))))

(def$method (frame-core :recall) (slot-name relation expr &optional (prop-name :value))
  "compares a property of a slot with the specified expr using relation."
  (let ((old-value ($send self :get slot-name prop-name))
        (new-value (cond ((atom expr) expr)
                         (t (compute-list-expr expr self))))
        (int-rel-name (internal-relation-name relation)))
    (cond ((is-undetermined old-value)
           (if (and (is-equality-relation int-rel-name)
                    ($send self int-rel-name old-value new-value))
             ;; Dies ist notwendig, falls gerade auf UNDETERMINED
             ;; gecheckt wird
             t
             old-value))
          (t ($send self int-rel-name old-value new-value)))))

(def$method (frame-core :remember) (slot-name relation expr &optional (prop-name :value))
  "sets a property of a slot to the specified expr.
returns nil, if the specified expr is identical to the old value."
  (let ((old-value ($send self :get slot-name prop-name))
        (new-value (cond ((atom expr) expr)
                         (t (compute-list-expr expr self))))
        (int-rel-name (internal-relation-name relation)))
    (cond ((is-undetermined old-value)
           ($send self :put slot-name new-value prop-name))
          (($send self int-rel-name old-value new-value) nil)
          (t ($send self :put slot-name new-value prop-name)))))

(def$method (frame-core :store) (slot-name expr &optional (prop-name :value))
  "sets a property of a slot to the specified expr."
  (let ((new-value (cond ((atom expr) expr)
                         (t (compute-list-expr expr self)))))
    ($send self :put slot-name new-value prop-name)))


;;-----------------------------------------------------------------------------
;;                       RELATIONS FOR SLOT MESSAGES 
;;-----------------------------------------------------------------------------


(defun internal-relation-name (relation)
  (if (keywordp relation)
    relation
    (intern (concatenate 'string (string relation) "-REL") :keyword)))

(defun is-equality-relation (relation-name)
  (member relation-name '(:=-rel :is-rel :/=-rel :one-of-rel :all-of-rel)))

(defmacro DEFINE-RELATION-METHOD ((flavor-name relation-name) lambda-list &body body)
  (let ((internal-rel-name
         (if (keywordp relation-name)
           relation-name
           (intern (concatenate 'string (string relation-name) "-REL") :keyword))))
    `(progn (def$method
              (,flavor-name ,internal-rel-name)
              ,lambda-list . ,body)
            '(,flavor-name ,relation-name))))

;; for the user

(defmacro DEFINE-RELATION-BEHAVIOR ((frame-name relation-name) lambda-list &body body)
  `(define-relation-method (,(get-frame-name-or-signal-error
                              `(,frame-name ,relation-name))
                            ,relation-name)
     ,lambda-list
     . ,body))


;;-----------------------------------------------------------------------------
;;                    STANDARD RELATIONS
;;-----------------------------------------------------------------------------

(DEFINE-RELATION-METHOD (frame-core =) (facet-value expr-value)
  (cond ((IS-MULTIPLE-ANSWER facet-value)
         ; This in case that cfs are not handled
         (member expr-value (rest facet-value)))
        (t (equal facet-value expr-value))))

(DEFINE-RELATION-METHOD (frame-core /=) (facet-value expr-value)
  (cond ((IS-MULTIPLE-ANSWER facet-value)
         ; This in case that cfs are not handled
         (not (member expr-value (rest facet-value))))
        (t (not (equal facet-value expr-value)))))

(DEFINE-RELATION-METHOD (frame-core is) (facet-value expr-value)
  (cond ((IS-MULTIPLE-ANSWER facet-value)
         ; This in case that cfs are not handled
         (member expr-value (rest facet-value)))
        (t (equal facet-value expr-value))))

(DEFINE-RELATION-METHOD (frame-core >=) (facet-value expr-value)
  (>= facet-value expr-value))

(DEFINE-RELATION-METHOD (frame-core >) (facet-value expr-value)
  (> facet-value expr-value))

(DEFINE-RELATION-METHOD (frame-core <=) (facet-value expr-value)
  (<= facet-value expr-value))

(DEFINE-RELATION-METHOD (frame-core <) (facet-value expr-value)
  (< facet-value expr-value))


(defun is-interval-specification (list)
  (and (numberp (first list))
       (numberp (second list))
       (null (rest (rest list)))))

(defun is-in-interval (x interval)
  (or (<= (first interval) x (second interval))
      (>= (first interval) x (second interval))))


(DEFINE-RELATION-METHOD (frame-core between) (facet-value interval)
  (and (numberp facet-value)
       (is-interval-specification interval)
       (is-in-interval facet-value interval)))

(DEFINE-RELATION-METHOD (frame-core one-of) (facet-value expr-value)
  (some #'(lambda (a-value)
            ($send self ':=-rel facet-value a-value))
        expr-value))

(DEFINE-RELATION-METHOD (frame-core all-of) (facet-value expr-value)
  (every #'(lambda (a-value)
             ($send self ':=-rel facet-value a-value))
         expr-value))

;;; eof

