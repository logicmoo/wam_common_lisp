;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON  -*-

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
;; Contents: a handler to mantain frames, behaviors and instances



;;--------------------------------------------------------------------
;;                   COMMON FUNCTIONS AND MACROS
;;--------------------------------------------------------------------


(defmacro is-facet (x)
  `(keywordp ,x))

(defmacro is-path (x)
  `(and (consp ,x)    
	(every #'keywordp ,x)))

;; (is-facet :possible-values) ==> T
;; (is-facet 'possible-values)  ==> NIL
;; (is-path nil) ==> NIL

;; BEHAVIOR-Namen (d.h. Benutzer-definierte Methoden fuer die FRAMES)
;; muessen keywords sein !!!!!

(defmacro is-user-defined-method (x)
  `(keywordp ,x))

(defmacro is-value (prop-name)
  `(eq ,prop-name :VALUE))

;;--------------------------------------------------------------------
;; frames and instances are interned in a package assoziated with the kb
;;--------------------------------------------------------------------

(defun %make-object-name (name &optional (kb-pkg nil))
  (intern (symbol-name name) (or kb-pkg (send-kb :pkg))))

(defun %get-object-name (object-name &optional (kb-pkg nil))
  ;; Hier wird NIL zurueckgegeben, falls die Suche erfolglos ist.
  (if (symbolp object-name)
      (find-symbol (symbol-name object-name) (or kb-pkg (send-kb :pkg)))))

;;--------------------------------------------------------------------
;; falls packages nicht zur verfuegung stehen
;; frames und instances werden mit einem mit der *CURRENT-KNOWLEDGE-BASE*
;; assoziierten prefix versehen
;;--------------------------------------------------------------------
;
;(defun %make-object-name (name &optional (kb-prefix nil))
;  (intern (concatenate 'string (or kb-prefix (send-kb :pkg)))
;                               (symbol-name name)))))
;
;(defun %get-object-name (object-name &optional (kb-prefix nil))
;  ;; Hier wird NIL zurueckgegeben, falls die Suche erfolglos ist.
;  (if (symbolp object-name)
;      (find-symbol (concatenate 'string  (or kb-prefix (send-kb :pkg))
;                                         (symbol-name name))))

;;--------------------------------------------------------------------
;;              FLAVOR OF FRAME-PROCESSOR
;;--------------------------------------------------------------------

(def$flavor frame-base
	((instances-list nil)
	 (frames-list nil)
	 (frame-type 'frame-core)
	 (frcheck nil))
	(processor-core) 
  :settable-instance-variables
  (:documentation "frame-base creates frames, behaviors and instances."))


(def$method (frame-base :toggle-frcheck) ()
  (cond ((null frcheck)
	 (setf frcheck t))
	(t (setf frcheck nil))))


;;--------------------------------------------------------------------
;;                      FRAMES 
;;--------------------------------------------------------------------

;; (DEFFRAME <frame-name>
;;      (SUPERS . <list-of-frames>)
;;      (SLOTS  . <list-of-slot-specifications>))
;;
;; <slot-spec> := <slot-name> |
;;                (<slot-name> <default-value> <prop-name-keyword> <value> ...)
;;
;;  <value> := <form>


;;--------------------------------------------------------------------

(defun make-frame-name (frame-name)
  (%make-object-name frame-name))

(defun get-frame-name (frame-name)
  (let ((frame-internal-name (%get-object-name frame-name)))
    (or frame-internal-name
	(baberror (getentry unknown-frame-error-fstr frame-io-table)
	       frame-name
	       (send-kb :kb-name)))))

(defun get-frame-name-with-check (frame-name)
  (let ((frame-internal-name (%get-object-name frame-name)))
    (if (and frame-internal-name
	     (%is-frame frame-internal-name))
	frame-internal-name 
	(baberror (getentry unknown-frame-error-fstr frame-io-table)
	       frame-name
	       (send-kb :kb-name)))))


(defun signal-unknown-frame (frame-name when spez)
  (unless (is-frame frame-name)
    (baberror (getentry unknown-frame-while-defining-fstr frame-io-table)
	      when
	      spez
	      frame-name 
	      (send-kb :kb-name))))


(defmacro frame-definition (frame-internal-name)
  `(get ,frame-internal-name  :FRAME-DEFINITION))

(defun get-frame-def (frame-name)
  ;checks for frame-name
  (frame-definition (get-frame-name-with-check frame-name)))

(defun %get-frame-name (frame-internal-name)
  (second (frame-definition frame-internal-name)))

(defun %is-frame (object-internal-name)
  (not (null (frame-definition object-internal-name))))


(defun is-frame (name)
  ;returns nil if x is not a symbol
  (let ((object-internal-name (%get-object-name name)))
    (if object-internal-name 
	(not (null (frame-definition object-internal-name))))))

;;--------------------------------------------------------------------
;;                      HANDLING SLOTS
;;--------------------------------------------------------------------


(defmacro frame-slots (frame-body)
  `(assoc 'SLOTS ,frame-body))

(defun get-frame-slots (frame-name)
  (rest (frame-slots (cddr (get-frame-def frame-name)))))

(defun compute-slot-names (slot-specs)
  (mapcar #'(lambda (a-slot-spec)
	      (if (atom a-slot-spec)
		  a-slot-spec
		  (first a-slot-spec)))
	  slot-specs))

(defun get-frame-slot-names (frame-name)
  (compute-slot-names (get-frame-slots frame-name)))

;;--------------------------------------------------------------------
;;                      HANDLING SUBCLASSES 
;;--------------------------------------------------------------------


(defmacro frame-subclasses (frame-internal-name)
  `(get ,frame-internal-name :SUBCLASSES))

(defun add-subframe (frame-name super-frame-name)
  (let* ((super-frame-internal-name (get-frame-name-with-check super-frame-name))
	 (subs-so-far (frame-subclasses super-frame-internal-name)))
    (if (not (member frame-name subs-so-far))
	(setf (frame-subclasses super-frame-internal-name)
	      (cons frame-name subs-so-far)))))

(defun install-subframe (frame-name supers)
  (mapcar #'(lambda (a-super)
	      (add-subframe frame-name a-super))
	  supers))


(defun get-subframes (frame-name)
  (frame-subclasses (get-frame-name-with-check frame-name)))

(defun get-all-subframes (frame-name)
  (do ((open (get-subframes frame-name))
       (closed nil))
      ((null open) closed)
    (let ((a-frame-name (pop open)))
      (cond ((member a-frame-name closed) nil)
	    (t (push a-frame-name closed)
	       (setq open (append (get-subframes a-frame-name) open)))))))

;;--------------------------------------------------------------------
;;                      HANDLING SUPERCLASSES 
;;--------------------------------------------------------------------


(defmacro frame-supers (frame-body)
  `(assoc 'SUPERS ,frame-body))

(defun get-supers (frame-name)
  (let ((supers-specification
	  (frame-supers (cddr (get-frame-def frame-name)))))
    (if supers-specification 
	(rest supers-specification))))

(defun get-all-supers (frame-name)
  (do ((open (get-supers frame-name))
       (closed nil))
      ((null open) closed)
    (let ((a-frame-name (pop open)))
      (cond ((member a-frame-name closed) nil)
	    (t (push a-frame-name closed)
	       (setq open (append (get-supers a-frame-name) open)))))))

;;--------------------------------------------------------------------
;;                CHECKING IF FRAME DEFINITION IS CORRECT 
;;--------------------------------------------------------------------


(defmacro frame-options (frame-body)
  `(assoc 'OPTIONS ,frame-body))


(defun make-slot-specification-example ()
  (format nil (getentry slot-spec-example-str frame-io-table)))

(defun make-frame-definition-example ()
  (format nil (getentry frame-spec-example-str frame-io-table)))

(defun make-supers-specification-example ()
  (format nil (getentry supers-spec-example-str frame-io-table)))


(defun check-frame-definition (frame-name body)  
  (if (not (every #'listp body))
      (baberror (getentry frame-spec-error-fstr frame-io-table)
	     frame-name
	     (make-frame-definition-example)))
  (let ((slots (frame-slots body))
	(supers (frame-supers body))
	(options (frame-options body)))
    (if (and supers
	     (not (is-simple-list supers)))
	(baberror (getentry supers-spec-error-fstr frame-io-table)
	       supers
	       frame-name
	       (make-supers-specification-example)))
    (mapc #'(lambda (a-slot-specification)
	      (cond ((symbolp a-slot-specification))
		    ((and (is-true-list a-slot-specification)
			  (symbolp (first a-slot-specification))
			  (evenp (length a-slot-specification))))
		    (t (baberror (getentry slot-spec-error-fstr frame-io-table)
			      a-slot-specification
			      frame-name
			      (make-slot-specification-example)))))
	  (rest slots))
    (if (not (null (remove options (remove slots (remove supers body)))))
	(baberror (getentry frame-spec-error-fstr frame-io-table)
	       frame-name
	       (make-frame-definition-example)))))
 


;;--------------------------------------------------------------------
;;                      THE FRAME CONSTRUCTOR 
;;--------------------------------------------------------------------



(defun compute-slots (slot-specs)
 (mapcar #'(lambda (x)
	      (if (atom x)
		  `(,x (list (undetermined)))
		  `(,(first x) (list . ,(mapcar #'(lambda ($x)
						    (list 'quote $x))
						(cdr x))))))
	  slot-specs))

(defun compute-slots2 (slot-specs)
  (mapcar #'(lambda (x)
	       (if (atom x)
		   `(,x '(,(undetermined)))
		   `(,(first x) ',(cdr x))))
	  slot-specs))


(def$method (frame-base :new-frame-form) (frame-name body)
  "transforms a frame definition into a flavor definition."
  (if frcheck
      (check-frame-definition frame-name body))
  (let ((frame-internal-name (make-frame-name frame-name))
	(frame-internal-supers
	  (mapcar #'get-frame-name (rest (frame-supers body)))))   
    `(progn
       (dolist (a-super ',(rest (frame-supers body)))
	 (signal-unknown-frame a-super "FRAME" ',frame-name))
       (def$frame ,frame-internal-name 
		   ,(compute-slots (rest (frame-slots body)))
	 ,(or frame-internal-supers (list frame-type))
	 :initable-instance-variables
	 ,@(rest (frame-options body)))
       (setf (frame-definition  ',frame-internal-name)
	     '(defframe ,frame-name . ,body))
       (install-subframe ',frame-name ',(rest (frame-supers body)))
       (send-kb :add-to-frames ',frame-name)
       ',frame-name)))


;;--------------------------------------------------------------------
;;                      BEHAVIOR DEFINITION 
;;--------------------------------------------------------------------

;; (DEFBEHAVIOR (<frame-name> {<type>} <selector-keyword>)
;;              <lambda-list>
;;      . <body>)

;; Innerhalb von <body> kann der Zugriff auf slot-werte
;; folgendermassen erfolgen:

;; ($VALUE <slot-name>)  ==> liefert Wert der :VALUE property
;; ($VALUE <slot-name> <prop-name>)  ==> liefert Wert der <prop-name> property

;; Modifikation von slot-werten erfolgt durch den allgemeinen
;; Zuweisungsoperator SETF
;; (s. Lispmachinen-Manual)

;; (setf ($VALUE <slot-name>) <new-value>)
;;    ==> Setzt :VALUE property auf <new-value>

;; (setf ($VALUE <slot-name> <prop-name>) <new-value>)
;; ==> Setzt <prop-name> property auf <new-value>



(defmacro frame-behaviors (frame-internal-name)
  `(get ,frame-internal-name :BEHAVIORS))

(defun get-frame-behaviors (frame-name)
  (frame-behaviors (get-frame-name-with-check frame-name)))


(defun get-frame-behavior-specs (frame-name)
  (mapcar #'car (get-frame-behaviors frame-name)))


(defun add-to-behaviors (frame-name behavior-def)
  (let* ((frame-internal-name (get-frame-name frame-name))
	 (behavior-specification (first behavior-def))
	 (behaviors-so-far (frame-behaviors frame-internal-name))
	 (previous-def (assoc behavior-specification behaviors-so-far :test #'equal)))
    (if previous-def
	(setf (rest previous-def) (rest behavior-def))
	(setf (frame-behaviors frame-internal-name) 
	      `(,@behaviors-so-far ,behavior-def)))))

(defun get-frame-name-or-signal-error
       (behavior-specification)
  (let ((frame-name (%get-object-name (first behavior-specification))))
    (if (or (null frame-name)
	    (not (%is-frame frame-name)))
	(baberror (getentry unknown-frame-for-behavier-error-fstr frame-io-table)
	       behavior-specification
	       (first behavior-specification)
	       (send-kb :kb-name))
	frame-name)))

;;--------------------------------------------------------------------
;;                      BEHAVIOR CONSTRUCTOR 
;;--------------------------------------------------------------------

;;;;;; die substitution von $value eruebgrigt macro-expansion


(def$method (frame-base :new-behavior-form)
	   (behavior-specification lambda-list behavior-body)
  "transforms a behavior definition into a method definition."
  (if (not (listp behavior-specification))
      (baberror (getentry behavior-spec-error-fstr frame-io-table)
	     behavior-specification))
  (if (not (every #'keywordp (rest behavior-specification)))
      (baberror (getentry behavior-spec-error-fstr frame-io-table)
		behavior-specification))
  (let ((frame-name (first behavior-specification)))
    `(progn
       (signal-unknown-frame ',frame-name 
			     "BEHAVIOR"
			     ',behavior-specification)
       (def$behavior (,(get-frame-name frame-name) ,@(rest behavior-specification))
		     ,lambda-list
	 ,@behavior-body)
       (add-to-behaviors ',frame-name 
			 '(,behavior-specification ,lambda-list ,@behavior-body))
       ',behavior-specification)))



;;--------------------------------------------------------------------
;;                      INSTANCES OF FRAMES 
;;--------------------------------------------------------------------

(defun make-instance-name (instance-name)
  (%make-object-name instance-name))

(defun get-instance-name (instance-name)
  (let ((instance-internal-name (%get-object-name instance-name)))
    (or instance-internal-name 
	(baberror (getentry unknown-instance-error-fstr frame-io-table)
	       instance-name
	       (send-kb :kb-name)))))

(defun get-instance-name-with-check (instance-name)
  (let ((instance-internal-name (%get-object-name instance-name)))
    (if (and instance-internal-name 
	     (%is-instance instance-internal-name))
	instance-internal-name 
	(baberror (getentry unknown-instance-error-fstr frame-io-table)
	       instance-name
	       (send-kb :kb-name)))))


(defmacro instance-definition (instance-internal-name)
  `(get ,instance-internal-name :INSTANCE-DEFINITION))

(defun get-instance-def (instance-name)
  (instance-definition (get-instance-name-with-check instance-name)))


(defun %is-instance (object-internal-name)
  (not (null (instance-definition object-internal-name))))

(defun is-instance (name)
  (let ((instance-internal-name (%get-object-name name)))
    (if instance-internal-name 
	(not (null (instance-definition instance-internal-name))))))


;; Das muss ein macro sein, wegen SETF in ADD-INSTANCE-TO-FRAME:

(defmacro frame-instances (frame-internal-name)
  `(get ,frame-internal-name :INSTANCES))

;;; falls frames importiert sind, instances aber nicht
(defun get-instance-list (frame-name)
  (intersection
   (get (get-frame-name-with-check frame-name) :INSTANCES)
   (send-kb :instances)))

(defun get-all-instances (frame-name)
  (do ((frames (cons frame-name (get-all-subframes frame-name))
	       (rest frames))
       (result nil))
      ((null frames) result)
    (setf result (append result (get-instance-list (first frames))))))

(defun add-instance-to-frame (frame-name instance-name)
  (let* ((frame-internal-name (get-frame-name-with-check frame-name))
	 (instances-so-far (frame-instances frame-internal-name)))
    (if (not (member instance-name instances-so-far))
	(setf (frame-instances frame-internal-name)
	      `(,@instances-so-far ,instance-name)))))

;;--------------------------------------------------------------------
;;                      INITIALIZING SLOTS 
;;--------------------------------------------------------------------

;; Actually merging of default and explicit slot specification is only made
;; for instances, not between FRAMES
;; (this is the case in LOOPS).  This may be changed in future.

;; (DEFINSTANCE <instance-name> OF <frame-name>
;;     WITH <slot-name1> = <slot-initialisation1>
;;          ...
;;          <slot-nameN> = <slot-initialisationN>)

;; <slot-initialisation> := <atom>  "<atom> ist init-wert von :VALUE property"
;; <slot-initialisation> := (<value> <prop-name-keyword> <value> ...)


(defun make-definstance-example ()
  (format nil (getentry instance-spec-example-str frame-io-table)))

(defun check-instance-definition (instance-name of frame-name body)
  (if (not (send-kb :kb-name))
      (baberror (getentry no-kb-for-instance-error-fstr frame-io-table)
	     instance-name
	     frame-name))
  (if (not (eq of 'OF))
      (baberror (getentry of-keyword-expect-error-fstr frame-io-table)
	     'of
	     (format nil (getentry definstance-spec-fstr frame-io-table)
		     instance-name of frame-name)))
  (cond ((null body))
	((and (is-true-list body)
	      (eq (first body) 'WITH))
	 (do ((b (rest body) (cdddr b)))
	     ((null b) t)
	   (if (not (and (symbolp (first b))
			 (eq (second b) '=)
			 (or (atom (third b))
			     (is-true-list (third b)))))
	       (baberror (getentry slot-initialization-error-fstr frame-io-table)
		      (first b) 
		      (format nil
			      (getentry definstance-spec-fstr frame-io-table)
			      instance-name of frame-name)
		      (make-definstance-example)))))
	(t (baberror (getentry instance-spec-error-fstr frame-io-table)
		  `(definstance ,instance-name ,of ,frame-name . ,body)
		  (make-definstance-example)))))

;;--------------------------------------------------------------------
;;                      INSTANCE CONSTRUCTOR 
;;--------------------------------------------------------------------


(defun remove-noisy-words (args)
  (remove 'WITH (remove '= args)))

(def$method (frame-base :new-instance)
	   (instance-name frame-name with-specification)
  "generates an instance of frame-name."
  (if frcheck
      (check-instance-definition instance-name 'of frame-name with-specification))
  (let* ((internal-instance-name (make-instance-name instance-name))
	 (instance (make-$instance (get-frame-name-with-check frame-name)
				  :object-name instance-name)))
    (setf (get internal-instance-name :instance) instance)
    (setf (get internal-instance-name :instance-definition)
	  `(definstance ,instance-name of ,frame-name . ,with-specification))
    (if frcheck
	($send instance :check-your-self))
    ($send instance :init-all-slots (remove-noisy-words with-specification) frcheck)
    ($send meta-processor :add-to-instances instance-name)
    (add-instance-to-frame frame-name instance-name)
    ($send instance :initialize with-specification)
    (setf (symbol-value instance-name) instance-name)))


;; create-unnamed-instance wird benoetigt fuer create-instance-of 
;; added: Franco 23.10.

(def$method (frame-base :new-unnamed-instance)
	   (instance-name frame-name &optional with-specification)
  "generates an unnamed instance of frame-name."
  (if frcheck
      (check-instance-definition instance-name 'of frame-name with-specification))
  (let ((instance (make-$instance (get-frame-name-with-check frame-name)
				 :object-name instance-name)))
    (if frcheck
	($send instance :check-your-self))
    ($send instance :init-all-slots (remove-noisy-words with-specification) frcheck)
    ($send instance :initialize with-specification)
    instance))


;;--------------------------------------------------------------------
;;                      GETTING INSTANCES 
;;--------------------------------------------------------------------


(defmacro get-instance (instance-name)
  `(get (get-instance-name ,instance-name) :instance))

;; Das wird gebraucht mit <- . Da wird ein voller Check gemacht.

(defmacro get-instance-with-check (instance-name)
  `(get (get-instance-name-with-check ,instance-name) :instance))

(defmacro $INST (instance-name)
  `(get (get-instance-name-with-check ,instance-name) :instance))

(defmacro get-instance-or-self (instance-name)
  (if (eq instance-name 'SELF)
      `,instance-name
      `(get-instance-with-check ,instance-name)))

;; Das ist anstelle von SETQ zu gebrauchen.
;; Es erlaubt, eine Instanz ueber einen Variablennamen
;; referenzierbar zu machen.


(defmacro set-instance-pointer (variable-name instance-name)
  `(let ((variable-internal-name (make-instance-name ',variable-name))
	 (instance-internal-name (get-instance-name-with-check ,instance-name)))
     (setq ,variable-name ',variable-name)
     (setf (get variable-internal-name :instance)
	   (get instance-internal-name :instance))
     (setf (instance-definition variable-internal-name)
	   (instance-definition instance-internal-name))
     ',variable-name))


;;--------------------------------------------------------------------
;;                      RESETTING  INSTANCES 
;;--------------------------------------------------------------------


(defmacro reset-instance (instance-name)
  `(let ((instance-def (get-instance-def ',instance-name)))
     (if (not instance-def)
	 (send-kb :babylon-format
		  (getentry no-instance-error-msg-fstr frame-io-table)
		  ',instance-name)
	 (eval instance-def))))


(defmacro reset-instances (&rest list-of-instance-names)
  `(progn ,@(mapcar #'(lambda (an-instance-name)
			 `(reset-instance ,an-instance-name))
		    list-of-instance-names)
	  t))


;;-------------------------------------------------------------------------
;;   SIMPLE PRINTING FUNCTIONS FOR FRAMES, BEHAVIORS, INSTANCES 
;;-------------------------------------------------------------------------

(defun print-frame-definition (frame-name &optional (stream *default-dialog-stream*))
  (babpprint (get-frame-def frame-name) stream))



(defun make-behavior-def (behavior-body)
  `(DEFBEHAVIOR . ,behavior-body))

(defun print-frame-behavior (behavior-body &optional (stream *default-dialog-stream*))
  (babpprint (make-behavior-def behavior-body) stream))


(defun print-frames (frame-names &optional (stream *default-dialog-stream*))
  (mapc #'(lambda (a-frame-name)
	     (print-frame-definition a-frame-name stream)
	     (format stream  "~2%")
	     (mapc #'(lambda (a-behavior-def)
		       (print-frame-behavior a-behavior-def stream)
		       (format stream  "~2%"))
		   (frame-behaviors a-frame-name)))
	frame-names)
  T)

(defun print-frame (frame-name &optional (stream *default-dialog-stream*))
  (print-frames (list frame-name) stream))


(defun print-instance-def (instance-name &optional (stream *default-dialog-stream*))
  (babpprint (get-instance-def instance-name) stream))

(defun print-instances (frame-name &optional (stream *default-dialog-stream*))
  (mapc #'(lambda (an-instance-name)
	    (print-instance-def an-instance-name stream)
	    (format stream  "~2%"))
	(get-instance-list frame-name))
  t)


(def$method (frame-base :inspect-frame)
	   (frame-name &optional (stream *default-dialog-stream*))
  "describes frame named <frame-name> on <stream>."  
  (format stream "~%Frame: ~S" frame-name)
  (format stream 
	"~%Slots: ~{~S ~}"
	(compute-slot-names (get-frame-slots frame-name)))
  (format stream 
	"~%Supers: ~{~S  ~}"
	(get-supers frame-name))
  (format stream 
	"~%Behaviors: ~{~{~* ~S ~^ ~S~} ~}"
	(get-frame-behavior-specs frame-name))
  (format stream 
	"~%Instances: ~{~S ~} ~%"
	(get-instance-list frame-name)))

(def$method (frame-base :inspect-instance)
	   (instance-name &optional (stream *default-dialog-stream*))
  "describes instance named <instance-name> on <stream>." 
  (let ((instance (get-instance instance-name)))
    (format stream  "~%Instance: ~S" instance-name)
    (mapc #'(lambda (a-slot)
	      (format stream 
		    "~%Slot: ~S  Value: ~S   Properties: ~{~S ~}"
		    a-slot
		    ($send instance :get a-slot)
		    (rest ($send instance :get-properties a-slot))))
	  ($send instance :slots))
    (format stream  "~%")))