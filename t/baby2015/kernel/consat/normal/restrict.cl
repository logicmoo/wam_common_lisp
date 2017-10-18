;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-


(in-package "BABYLON")


;
;		HILFSFUNKTIONEN FUER RESTRICTION NETS
;



;
;	BESCHREIBUNG DES RESTRICTION-NETZES
;


(def$flavor restriction-definition

	((restrictions nil)
	 (protected nil)
	 (guarded nil))
	()

  :settable-instance-variables
  :initable-instance-variables
  (:documentation "generische Beschreibung des Netzes noetig fuer Neugenerierung.")
  )


(def$method (restriction-definition :store-definition)
	    (new-restrictions new-guarded new-protected)
  
  "	speichert generische Beschreibung des Netzes"
  
  (setf restrictions new-restrictions)
  (setf guarded new-guarded)
  (setf protected new-protected))


(def$method (restriction-definition :print) (name stream)

  "	Ausgabe des Restriction-Net"

  (princ " " stream)
  (terpri stream)
  (babpprint 
    `(defrestriction ,name
       (:guarded-slots . ,guarded)
       (:protected-slots . ,protected)
       (:restrictions . ,restrictions))
        stream)
  (terpri stream))



;
;	FLAVOR RESTRICTION-NET
;


(def$flavor restriction-net
	    
	    ((changed-slots nil)
	     (more-restricted-slots nil))
  (restriction-definition
    constraint-net)
  
  :settable-instance-variables
  :initable-instance-variables
  (:documentation "Constraintnetz auf Slots"))



;
;	MACROS
;

     
(defmacro make-slot-ref (object slot)
  `(list ,object ,slot))


(defmacro get-object-of-slot-ref (slot-ref)
  
  "  	liefert Instanz oder Variable der Slot-referenz"

  `(first ,slot-ref))


(defmacro get-slot-of-slot-ref (slot-ref)
  `(second ,slot-ref))




;
;	TRACER-ANTEILE AN RESTRICTION-NET
;


;
;(def$flavor traced-restriction-net
;
;	;;;	Restriction-Netz mit Trace-Mixin
;
;	()
;	(constraint-trace-mixin
;	 restriction-net)
;  )

;
;(def$method (restriction-net :trace-on)
;	   (c-name) 
;          (declare(ignore c-name))
;  
;  ;;;	erzeugt ein Traced-Restriction-Netz, dass mit dem
;  ;;;	Empfaenger in allen Komponenten (ausser name) uebereinstimmt
;
;  (make-$instance
;    'traced-restriction-net
;    :name c-name
;    :interface interface
;    :net-spec net-spec
;    :agenda agenda
;    :stack stack
;    :restrictions restrictions 
;    :protected protected
;    :guarded guarded
;    :changed-slots changed-slots
;    :more-restricted-slots more-restricted-slots
;    ))
;
;
;(def$method (restriction-net :trace-off)
;	   (c-name)
;          (declare(ignore c-name))
;  self)
;
;
;(def$method (traced-restriction-net :trace-on)
;	   (c-name)
;          (declare(ignore c-name))
;  self)
;
;
;(def$method (traced-restriction-net :trace-off)
;	   (c-name)
;          (declare(ignore c-name))  
;  ;;;	erzeugt ein Restriction-Netz, dass mit dem
;  ;;;	Empfaenger in allen Komponenten (ausser name) uebereinstimmt
;
;  (make-$instance
;    'restriction-net
;    :interface interface
;    :net-spec net-spec
;    :agenda agenda
;    :stack stack
;    :restrictions restrictions 
;    :protected protected
;    :guarded guarded
;    :changed-slots changed-slots
;    :more-restricted-slots more-restricted-slots
;    ))
;
;

;
;	BEWACHUNG VON SLOTS
;


(def$flavor restricted-slot	
  ((value -)
   (restriction-net nil)
   (protected nil)
   (guarded nil))
  ()
  :settable-instance-variables
  :initable-instance-variables
  (:documentation "Flavor fuer Active-Value.
Jeder Slot in einem Restriction-Net erhaelt einen solchen."))



(def$method (restricted-slot :put)
	    (instance slot new-value &optional (test nil))				
  
  ;;;	if the option test isn't equal :test
  ;;;   the specified slot is added to the list of modified slots and
  ;;;   the slot value is set to new-value;
  ;;;
  ;;;   otherwise it is tested if all atteched constraints are satisfied
  ;;;	after assigning new-value to slot;
  ;;;
  ;;;	if an inconsistency is detected the write access is refused
  ;;;
  ;;;	a call of this method yields
  ;;;		- t, if a write access has been performed
  ;;;		- nil, otherwise
  
  (cond ((not (eq test :test))
	 ($send restriction-net
		:update-slot-state
		(make-slot-ref ($send instance :object-name)
			       slot)
		value)
	 (setf value new-value)
	 t)
	(($send restriction-net
		:demon
		(list (make-value-assoc
			(make-slot-ref
			  ($send instance :object-name)
			  slot)
			(slot-value-to-value-spec new-value))))
	 (setf value new-value)
	 t)
	(t nil)))


;(def$method (restricted-slot :put)
;	   (instance slot new-value)				;;;  ?????
;  
;  ;;;	falls ein bewachter Slot vorliegt, wird Aktivierung
;  ;;;	des Netzes ausgeloest,
;  ;;;
;  ;;;	ansonsten wird der Slot in die Liste der geaenderten
;  ;;;   Slots eingefuegt
;  ;;;
;  ;;;	Ergebnis:  T, 	falls Schreibvorgang durchgefuehrt
;  ;;;		   nil,	falls Wert verweigert wird
;  
;  (cond ((not guarded)
;	 ($send restriction-net
;	       :update-slot-state
;	       (make-slot-ref ($send instance :object-name)
;			      slot)
;	       value)
;	 (setf value new-value)
;	 t)
;	(($send restriction-net
;	       :demon
;	       (list (make-value-assoc
;		       (make-slot-ref
;			 ($send instance :object-name)
;			 slot)
;		       (slot-value-to-value-spec new-value))))
;	 (setf value new-value)
;	 t)
;	(t nil)))


(def$method (restricted-slot :try-put) (new-value)				;;;  ?????

  ;;;	fuehrt Schreibvorgang durch, falls Slot nicht geschuetzt ist
  
  (if (not protected)
      (setf value new-value)))



(def$method (restricted-slot :get) ()

  ;;;	liefert Wert des Slot

  value)



;
;	ERZEUGUNG DER BEWACHTEN SLOTS
;


(def$method (restriction-net :make-active-values) (guarded-slots protected-slots)

  ;;;	erzeugt Active-Values fuer alle Slots

  (mapc (function
	  (lambda (slot-ref)
	    ($send self :make-slot-restriction
		  (get-instance
		    (get-object-of-slot-ref slot-ref))
		  (get-slot-of-slot-ref slot-ref))))
	interface)

  (mapc (function
	  (lambda (slot-ref)
	    ($send self :make-guarded-slot
		  (get-instance
		    (get-object-of-slot-ref slot-ref))
		  (get-slot-of-slot-ref slot-ref))))
	guarded-slots)

  (mapc (function
	  (lambda (slot-ref)
	    ($send self :make-protected-slot
		  (get-instance
		    (get-object-of-slot-ref slot-ref))
		  (get-slot-of-slot-ref slot-ref))))
	protected-slots))



(def$method (restriction-net :make-slot-restriction) (instance slot)
  
  ;;; erzeugt aktiven Wert fuer diesen Slot
  
  (if ($send self :correct-restriction-net instance slot)
      
      ($send instance
	    :replace
	    slot
	    (make-$instance
	      'restricted-slot
	      :value ($send instance :get slot)
	      :restriction-net self))
      
      (baberror (getentry restriction-error constraint-io-table))))



(def$method (restriction-net :make-guarded-slot) (instance slot)

  ;;;	markiert den Slot als bewacht

  (if ($send self :correct-restriction-net
	    instance slot)
      ($send ($send instance :get-value-only slot)
	    :set-guarded
	    t)))


(def$method (restriction-net :make-protected-slot) (instance slot)

  ;;;	markiert den Slot als bewacht

  (if ($send self :correct-restriction-net
	    instance slot)
      ($send ($send instance :get-value-only slot)
	    :set-protected
	    t)))


(def$method (restriction-net :correct-restriction-net) (instance slot)
  
  ;;; 	falls der Slot bereits einem (noch existierendem !) Netz
  ;;;   zugeordnet ist und dieses mit demjenigen uebereinstimmt,
  ;;;	an das die Nachricht ge$sendet wurde,
  ;;;	wird t als Ergebnis geliefert;
  ;;;	falls der Slot noch zu keinem Netz gehoert oder dieses nicht mehr
  ;;;   definiert ist, wird ebenfalls t geliefert;

  (let ((value ($send instance :get-value-only slot)))
    (cond ((not (flavor-typep value 'restricted-slot)) t)
	  ((send-kb
	     :is-defined-p ($send value :restriction-net))
	   (eql ($send value :restriction-net) self))
	  (t t))))
  
  ;;;	($send 
  ;;;	  ($send instance :send-if-handles
  ;;;                     :get-value-only slot)
  ;;;	  :restriction-net))

(defun undetermined-slot-value-p (slot-value)
  (member slot-value '(- unknown)))


(defun slot-value-to-value-spec (slot-value)
  
  "	ueberfuehrt slot-value in eine Consat-Wertemenge"
  
  (cond ((undetermined-slot-value-p slot-value)
	 'unconstrained)
	(T (list slot-value))))


;;; eof

