;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     April 1987
;; AUTHORS:  Eckehard Gross

;; This file depends on:  common>*
;;                        frames>basic>*
;;                        
;; Contents: a mixin which allows to specify possible values for the
;;           :value property of a slot.
;;           a possible value specification has the form:
;;
;;           :possible-values := <method-name>     
;;                               | (:interval <number1> <number2>)
;;                               | (:one-of <value1> ... <valuen>)
;;                               | (:some-of <value1> ... <valuen>)
;;
;;           <method-name> := :NUMBER | :STRING | :LIST ...


;;-------------------------------------------------------------------------------
;;                  POSSIBLE VALUE MIXIN
;;-------------------------------------------------------------------------------


(def$flavor poss-val-mixin
	()()
  (:required-instance-variables slots object-name)
  (:documentation "mixin providing means to introduce possible values for slots."))


;;-------------------------------------------------------------------------------
;;              CONSTRUCTOR FOR POSSIBLE VALUES METHODS
;;-------------------------------------------------------------------------------


(defmacro define-possible-values-method
	  ((flavor-name method-name) lambda-list form)
  "macro to create the method which is used to check a new value.
lambda-list := (<value-to-check> <possible-values-args>)."
  `(progn  ;'COMPILE
	  (def$method (,flavor-name ,method-name)
		     ,lambda-list
	    (if (or (is-undetermined ,(first lambda-list))
		    ;; Dies wird automatisch ergaenzt
		    (is-unknown ,(first lambda-list))
		    ;; Dies wird automatisch ergaenzt
		    ,form)
		(list ,(first lambda-list))))
	  '(,flavor-name ,method-name)))

;; Das ist fuer den Benutzer:

(defmacro define-possible-values-behavior
	  ((frame-name method-name) lambda-list form)  
  `(define-possible-values-method
     (,(get-frame-name-or-signal-error `(,frame-name ,method-name)) ,method-name)
     ,lambda-list
     ,form))


;;-------------------------------------------------------------------------------
;;                 STANDARD POSSIBLE VALUES METHODS
;;-------------------------------------------------------------------------------



(define-possible-values-method (poss-val-mixin :interval) (x interval)
  (and (numberp x)
       (is-in-interval x interval)))

(define-possible-values-method (poss-val-mixin :boolean) (x)
  (or (member x '(y yes))
      (member x '(n no))
      (null x)
      (eq x t)))

(define-possible-values-method (poss-val-mixin :number) (x)
  (numberp x))

(define-possible-values-method (poss-val-mixin :string) (x)
  (stringp x))

(define-possible-values-method (poss-val-mixin :list) (x)
  ;; in cl (listp nil) => t
  (listp x))

(define-possible-values-method (poss-val-mixin :symbol) (x)
  (symbolp x))

(define-possible-values-method (poss-val-mixin :one-of) (x list)
  (member x list :test 'equal))

(define-possible-values-method (poss-val-mixin :some-of) (x list)
  (cond ((is-multiple-value x)
	 (every #'(lambda (an-item)
		    (member an-item list :test 'equal))
		(rest x)))
	(t (member x list :test 'equal))))

(define-possible-values-method (poss-val-mixin :any) (x)
  (or x t))

(define-possible-values-method (poss-val-mixin :instance-of) (instance-name list)
  ;; das erste element von list ist ein frame-name, 
  ;; der rest koennten angaben ueber slots-belegungen sein
  ;; (das kann spaeter implementiert werden).
  (and (is-instance instance-name)
       (let ((frame-name (get-frame-name-with-check (first list)))) 
	 (if frame-name 
	     (flavor-typep (get-instance instance-name) frame-name)))))



(DEFINE-POSSIBLE-VALUES-METHOD  (poss-val-mixin :and)
				(x list-of-possible-values-specifications)
  (if (every #'(lambda (a-possible-values-specification)
	     (let ((poss-val-method
		     (get-poss-val-type a-possible-values-specification))
		   (poss-val-args
		     (get-poss-val-args a-possible-values-specification)))
	       (if (null poss-val-args)
		   ($send self poss-val-method x)
		   ($send self poss-val-method x poss-val-args))))
	     list-of-possible-values-specifications)
      (list x)))

(DEFINE-POSSIBLE-VALUES-METHOD  (poss-val-mixin :or)
				(x list-of-possible-values-specifications)
  (if (some #'(lambda (a-possible-values-specification)
		(let ((poss-val-method
			(get-poss-val-type a-possible-values-specification))
		      (poss-val-args
			(get-poss-val-args a-possible-values-specification)))
		  (if (null poss-val-args)
		      ($send self poss-val-method x)
		      ($send self poss-val-method x poss-val-args))))
	    list-of-possible-values-specifications)
      (list x)))

(DEFINE-POSSIBLE-VALUES-METHOD  (poss-val-mixin :not)
				(x list-with-possible-values-specification)
  (let ((poss-val-method
	  (get-poss-val-type (first list-with-possible-values-specification)))
	(poss-val-args
	  (get-poss-val-args (first list-with-possible-values-specification))))
    (unless (if (null poss-val-args)
		($send self poss-val-method x)
		($send self poss-val-method x poss-val-args))
      (list x))))
  

;;-------------------------------------------------------------------------------
;;                 CHECKING THE FORMAT OF POSSIBLE VALUES 
;;-------------------------------------------------------------------------------


(defun get-check-result (check-result) 
  (first check-result))


(defun is-method-of (self possible-values-type)
  (and (symbolp possible-values-type)
       (keywordp  possible-values-type)
       ($send self :operation-handled-p possible-values-type)))


(defun get-poss-val-type (possible-values) 
  (if (atom possible-values)
      possible-values
      (first possible-values)))

(defun get-poss-val-args (possible-values)
  (if (atom possible-values)
      nil
      (rest possible-values)))


(def$method (poss-val-mixin :check-value) (slot value &optional possible-values)
  "checks whether value is a possible value for slot."
  (let* ((poss-vals (or possible-values
			($send self :get slot :possible-values)))
	 (poss-val-method (get-poss-val-type poss-vals))
	 (poss-val-args (get-poss-val-args poss-vals)))
    (cond ((or (null poss-val-method)
	       (eq poss-val-method :any)) `(,value))
	  ;; the result must be a list whose CAR is A-VALUE
	  ;; to remain consistent with the results of possible-values-methods
	  ((null poss-val-args)
	   ($send self poss-val-method value))
	  (t ($send self poss-val-method value poss-val-args)))))

(def$method (poss-val-mixin :check-correct-value) (slot value)
  "checks whether value is a possible value for slot allowing to correct the value."
  (let* ((possible-values
	   ($send self :get slot :possible-values))
	 (check-result
	   ($send self :check-value slot value (or possible-values :any))))
    (cond ((not (null check-result)) check-result)
	  (t (send-kb :babylon-format "~%~?"
		      (getentry constraints-violation-fstr frame-io-table)
		      (list value
			    (get-poss-val-type possible-values)
			    (get-poss-val-args possible-values)
			    slot
			    object-name
			    (flavor-type-of self)))
	     (send-kb :babylon-format
		      (getentry other-value-question-str frame-io-table))
	     (let ((answer (send-kb :babylon-read '(#\y #\n))))
	       (send-kb :babylon-format "~C~%" answer)
	       (cond ((char-equal answer  #\y)               ; char-equal statt eql
	      ; (cond ((eql answer  #\y)
		      (send-kb :babylon-format
			       (getentry new-value-question-fstr frame-io-table)
			       object-name
			       slot)
		      ($send self :check-correct-value
				    slot (send-kb :babylon-read)))
		     (t  '$ABORT$)))))))


(def$method (poss-val-mixin :put)      
	   (slot-name new-value &optional (prop-name :value))
  "sets the value of a slot checking whether the value is a possible value for the slot."
  (if (is-value prop-name) ;; do constraints check
      (let ((check-result
	      ($send self :check-correct-value slot-name new-value)))
	(if (not (eq '$ABORT$ check-result))
	    ($send self :set slot-name (get-check-result check-result) prop-name)))
      ($send self :set slot-name new-value prop-name)))

;;-------------------------------------------------------------------------------
;;          INITIALIZATION WITH  POSSIBLE VALUES METHODS
;;-------------------------------------------------------------------------------


(def$method (poss-val-mixin :check-init-value) (slot value)
  "checks whether the default value is a possible value for slot."
  (let ((possible-values ($send self :get slot :possible-values)))
    (cond ((null possible-values))
	  ((not (is-method-of self (get-poss-val-type possible-values)))
	   (baberror (getentry unknown-poss-val-method-fstr frame-io-table)
		  possible-values
		  slot
		  object-name
		  (flavor-type-of self)))
	  (($send self :check-value slot value possible-values))
	  (t (baberror (getentry constraints-violation-fstr frame-io-table)
		     value 
		     (get-poss-val-type possible-values)
		     (get-poss-val-args possible-values)
		     slot
		     object-name
		     (flavor-type-of self))))))


(def$method (poss-val-mixin :init-slot) (slot-name slot-spezification check)
  "initializes a slot with values from slot-spezification."  
  ;;reverse bewirkt, dass :possible-values vor :value initialisiert wird
  ;;allerdings sollten :possible-values per frame definition eingefuehrt werden
  (do ((plist (reverse (normalize-plist slot-spezification))  
	      (rest (rest plist))))
      ((null plist))
    (if (and check (eq (second plist) :value))
	($send self :check-init-value slot-name (first plist)))
    ($send self :set slot-name (first plist) (second plist))))


(defun explain-answers-choices (answer-explanations)
  (cond ((null answer-explanations) nil)
	((is-simple-list answer-explanations) nil)
	(t (mapcar #'first answer-explanations))))

(def$method (poss-val-mixin :check-format-of-explain-answers) (slot)
  "checks whether all values for which explanations are provided by the
:explain-answers property are possible values."
  (let* ((answer-explanations ($send self :get slot :explain-answers))
	 (choices (explain-answers-choices answer-explanations)))	 
    (cond ((null answer-explanations) t)
	  ((null choices) t)
	  (t (mapc #'(lambda (choice)
		       (if (not ($send self :check-value slot choice))
			   (baberror (getentry explain-answers-spec-error-fstr
					    frame-io-table)
				   (assoc choice answer-explanations :test 'equal)
				   slot
				   object-name
				   (flavor-type-of self)
				   choice)))
		   choices)))))


(def$method (poss-val-mixin :check-your-self) ()
  "checks whether some values of the frame definition are possible values:
1. default values 2. all values for which explanations are provided by the
:explain-answers property."
  (mapc #'(lambda (a-slot-name)
	    (let ((default-value ($send self :get-value-only a-slot-name)))
	      ($send self :check-init-value a-slot-name default-value)))
	slots)
  (mapc #'(lambda (a-slot-name)
	    ($send self :check-format-of-explain-answers a-slot-name))
	slots))


;;-------------------------------------------------------------------------------
;;                 ASKING VOR VALUES
;;-------------------------------------------------------------------------------


(def$method (poss-val-mixin :provide-local-help)
	    (slot &optional window)  
  "displays an explanation from the :explain-answers property.
returns help if no explanation is available."
  (declare (ignore window))
  (let* ((answer-explanations ($send self :get slot :explain-answers))
	 (choices (explain-answers-choices answer-explanations)))
    (if answer-explanations
	(send-kb :babylon-format
		 (getentry explain-answers-fstr frame-io-table)
		 choices choices)
	(send-kb :babylon-format
		 (getentry no-explain-answers-fstr frame-io-table)))
    (send-kb :babylon-format
	     (getentry explain-context-fstr frame-io-table) *help-key* *end-key*)
    (do ((choice (send-kb :babylon-read (list *end-key* *help-key* '#\space))
		 (send-kb :babylon-read (list *end-key* *help-key* '#\space))))
	((eql choice *end-key*) (send-kb :babylon-format "~:C ~%" *end-key*) nil)
      (cond ((eql choice *help-key*)
	     (send-kb :babylon-format "~:C ~%" *help-key*) (return  'help))
	    ((and answer-explanations
		  (eql choice '#\space))
	     (apply #'send-kb :babylon-format 
		    (substitute-o-and-s object-name slot answer-explanations)))
	    ((member choice choices)
	     (let ((explanation
		     (rest (assoc choice answer-explanations :test 'equal))))
	       (apply #'send-kb :babylon-format 
		      (substitute-o-and-s object-name slot explanation)))))
      (send-kb :babylon-format (getentry next-value-fstr frame-io-table) *end-key*))))



(def$method (poss-val-mixin :ask-for-slot-value)  
	   (slot &optional desired-value negation-flag (standard-option nil))
  "asks the user for the value of a slot providing explanations if asked for.
invokes a special method on demand to support the user entering a value."
  ($send self :prompt-for-value slot)
  (let ((result (send-kb :babylon-read (list *help-key* *c-help-key*))))
    (cond ((eql result *c-help-key*)
	   ($send self :ask-guided
		  slot desired-value negation-flag standard-option))
	  ((or (eql result 'help)
	       (eql result *help-key*))
	   (or ($send self :provide-local-help slot)
	       ($send self :ask-for-slot-value
		      slot desired-value negation-flag standard-option)))
	  (t (let ((check-result ($send self :check-value slot result)))
	       (if check-result 
		   ($send self :set slot (get-check-result check-result))
		   ($send self :ask-guided
			  slot desired-value negation-flag standard-option)))))))


(def$method (poss-val-mixin :ask-guided)
	   (slot desired-value negation-flag standard-option)
  "asks the user for the value of a slot.
invokes a special method attached to the :supp-method property
of the possible value type of the slot (or a default method) 
to support the user entering a value."
  (let* ((read-method ($send self :get-read-method slot))
	 ;; read-method is assumed to return a correct value
	 (result ($send self read-method 
			slot desired-value negation-flag standard-option)))
    (cond ((eq result 'help) 'help)
	  (t ($send self :set slot result)))))


(def$method (poss-val-mixin :get-read-method) (slot)
  "fetches the special method to support the user entering a value for slot."
  (let* ((possible-values ($send self :get slot :possible-values))
	 (poss-val-method (get-poss-val-type possible-values)))
    (or (get poss-val-method  :supp-method)
	(get :any :supp-method))))

;;-------------------------------------------------------------------------------

(setf (get :any :supp-method ) :default-read-method)

(def$method (poss-val-mixin :default-read-method)
	    (slot desired-value negation-flag standard-option)
  "default method to support the user entering a value for slot."
  (let ((possible-values ($send self :get slot :possible-values)))
    (send-kb :babylon-format
	     (format-expectations desired-value
				  negation-flag
				  (get-poss-val-type possible-values)
				  (get-poss-val-args possible-values)))
    ($send self :prompt-for-value slot)
    (let ((result (send-kb :babylon-read (list *help-key* *c-help-key*))))
      (cond ((eql result *c-help-key*)
	     ($send self :default-read-method
		    slot desired-value negation-flag standard-option))
	    ((or (eq result 'help)
		 (eql result *help-key*))
	     (or ($send self :provide-local-help slot)
		 ($send self :default-read-method
			slot desired-value negation-flag standard-option)))
	    (t (let ((check-result ($send self :check-value slot result)))
		 (if check-result
		     (get-check-result check-result)		     
		     ($send self :default-read-method
			    slot
			    desired-value
			    negation-flag
			    standard-option))))))))


;;; eof

