;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-



(in-package "BABYLON")


;
;		ABARBEITUNG VON RESTRICTION NETS
;




;
;	NACHRICHTEN FUER RESTRICTION-NET
;

(defun admissible-net-p (list-of-slots net-spec)
  
  "	liefert t, falls die Wertebelegung der Netzvariablen
  	noch zulaessig ist"
  
  (if (null list-of-slots) t
      (and (admissible-slot-value-p
	     (first list-of-slots)
	     net-spec)
	   (admissible-net-p
	     (rest list-of-slots)
	     net-spec))))


(defun admissible-slot-value-p (slot-ref net-spec)
  
  "	liefert t, falls fuer den Slot gilt
  
  	  	{ sv( slot ) } = cv( slot )"
  
  (equal (slot-value-to-value-spec
	   (get-value-of-referenced-slot
	     slot-ref))
	 (get-var-info-values
	   (assoc slot-ref net-spec :test 'equal))))


(defun further-slot-restriction-p (slot-ref net-spec)
  
  "	liefert T, falls der Slotwert im Vergleich zum Wert im
  	Constraint-Netz eine neue Restriction darstellt, also falls gilt
  
  	{ sv( slot) } -= cv( slot) und
  	sv( slot) -= undetermined "  
  
  (let ((slot-value (get-value-of-referenced-slot
		      slot-ref)))
    
    (and (not (undetermined-slot-value-p slot-value))
	 (not (equal (slot-value-to-value-spec
		       slot-value)
		     (get-var-info-values
		       (assoc slot-ref net-spec :test 'equal)))))))


(defun make-value-ass-of-posted-slots (list-of-slots net-spec)
  
  "	liefert Wertebelegung derjenigen Slots, die seit dem
  	letzten stabilen Zustand staerker eingeschraenkt wurden"
  
  (cond ((null list-of-slots) nil)
	((further-slot-restriction-p
	   (first list-of-slots)
	   net-spec)
	 (cons (make-value-assoc
		 (first list-of-slots)
		 (copy-slot-value (first list-of-slots)))
	       (make-value-ass-of-posted-slots
		 (rest list-of-slots)
		 net-spec)))
	(t (make-value-ass-of-posted-slots
	     (rest list-of-slots)
	     net-spec))))



;
;	ABBILDUNG DER SLOTWERTE AUF CONSAT
;



(defun copy-slot-value (slot-ref)
  
  "	liest einen Slotwert und passt seine Repraesentation an"
  
  (slot-value-to-value-spec
    ($send ($send (get-instance
		  (get-object-of-slot-ref slot-ref))
		:get-value-only 
		(get-slot-of-slot-ref slot-ref))
	  :get)))



(defun copy-possible-values (slot-ref)
  
  "	liest die Possible-Values des Slots und
        transformiert die Repraesentation"
  
  (possible-values-to-value-spec
    ($send (get-instance
	    (get-object-of-slot-ref slot-ref))
	  :get 
	  (get-slot-of-slot-ref slot-ref)
	  :possible-values)))



(defun replace-slot-value (slot-ref value-spec)
  
  "	fuehrt einen Schreibversuch auf den Slot durch,
  	falls value-spec einelementig ist"
  
  (if (and (not (eq value-spec 'unconstrained))
	   (= (length value-spec) 1))
      ($send ($send (get-instance
		    (get-object-of-slot-ref slot-ref))
		  :get-value-only 
		  (get-slot-of-slot-ref slot-ref))
	    :try-put
	    (first value-spec))))



(defun replace-possible-values (slot-ref value-spec)
  
  "	ersetzt possible-values, falls value-spec ungleich
  	unconstrained ist"
  
  (if (not (eq value-spec 'unconstrained))
      ($send (get-instance
	      (get-object-of-slot-ref slot-ref))
	    :put	    
	    (get-slot-of-slot-ref slot-ref)
	    (cons :one-of value-spec)
	    :possible-values)))



(defun possible-values-to-value-spec (possible-values)
  
  "	ueberfuehrt die Possible-Values-Beschreibung in eine
  	Consat-Wertemenge"
  
  (cond ((null possible-values) 'unconstrained)
	((atom possible-values)
	 (case possible-values
	   (:any 'unconstrained)
	   (:symbol 'unconstrained)
	   (:number 'unconstrained)
	   (:list 'unconstrained)
	   (:string 'unconstrained)
	   (:boolean '(t nil))))
	
	(t (case (first possible-values)
	     
	     (:interval 'unconstrained)
	     (:instance-of 'unconstrained)
	     (:not 'unconstrained)
	     
	     (:one-of (rest possible-values))
	     
	     (otherwise 
	       (baberror "keine multiple-values erlaubt"))))))


(defun get-value-of-referenced-slot (slot-ref)

  "   ermittelt Wert des Slots"

  ($send (get-instance
	  (get-object-of-slot-ref slot-ref))
	:get
	(get-slot-of-slot-ref slot-ref)))

;;;

(def$method (restriction-net :test-values) ()
  
  "	ueberprueft die aktuellen Slotwerte auf Konsistenz"
  
  ($send self :get-stable-state)
  ($send self :consistent-p))


(def$method (restriction-net :modify-values) ()
  
  "	ueberprueft die Konsistenz der aktuellen Slotwerte;
  	evtl. erhalten Slots ohne Wert einen Wert zugewiesen"

  ($send self :get-stable-state)
  (cond (($send self :consistent-p)
	 ($send self :replace-values)
	 t)
	(t nil)))


(def$method (restriction-net :test-possible-values) ()

  "	ueberprueft die Possible-Values der Slots auf
  	Konsistenz und aendert die Defaultwerte
  	entsprechend"

  ($send self :filter-possible-values)
  ($send self :consistent-p))


(def$method (restriction-net :modify-possible-values) ()

  "	wie :test-possible-values;
  	zusaetzlich werden die :possible-values aktualisiert"

  ($send self :filter-possible-values)
  ($send self :replace-possible-values)
  ($send self :consistent-p))



(def$method (restriction-net :demon) (value-ass)

  "	ueberfuehrt Netz in einen stabilen Zustand;
  	danach wird Netz versuchsweise mit value-assignment
  	aktiviert;
  	bei Konsistenz wird das Ergebnis der Propagierung beibehalten
  	und Slotwerte angepasst"

  ($send self :get-stable-state)
  ($send self :store-state)
  ($send self :initialize-variables value-ass)
  ($send self :initialize-agenda value-ass)
  
  ($send self :propagate 'local-consistency)

  (cond (($send self :consistent-p)
	 ($send self :replace-values)
	 ($send self :forget-state)
	 t)
	(t ($send self :restore-state)
	   (if ($send self :consistent-p)
	       ($send self :replace-values))
	   nil)))



;
;	ZUSTAND DES NETZES
;



(def$method (restriction-net :filter-possible-values) ()
  
  "	ueberfuehrt das Netz (falls noch nicht geschehen) vom Anfangszustand 
  	in einen Zustand, in dem
  
  	- das Netz gefiltert und die possible-values propagiert wurden
  	- alle Slots in die changed-Liste eingefuegt werden"
  
  (cond ((not (agenda-elem-filtered-p agenda))
	 ($send self :copy-possible-values)
	 ($send self :total-init-queue)
	 (setf (agenda-elem-trace agenda) nil)
	 
	 ($send self :propagate 'local-consistency)
	 
	 ($send self :freeze-state)
	 ($send self :init-slot-state)
	 
	 (setf (agenda-elem-filtered-p agenda) t))))



(def$method (restriction-net :get-stable-state) ()
  
  "	ueberfuehrt das Netz in einem Zustand, in dem
  
  	- alle inzwischen durchgefuehrten Slotwertaenderungen propagiert
  	  werden und das Netz somit wieder mit den Slotwerten uebereinstimmt"
  
  ($send self :filter-possible-values)
  
  (cond ((admissible-net-p changed-slots net-spec)
	 (let ((value-ass (make-value-ass-of-posted-slots
			    more-restricted-slots
			    net-spec)))
	   ($send self :initialize-variables value-ass)
	   ($send self :initialize-agenda value-ass)))
	
	(t ($send self :get-initiale-state)
	   ($send self :copy-values)
	   ($send self :total-init-queue)))
  
  ($send self :propagate 'local-consistency)
  ($send self :reset-slot-state))



;
;	PROTOKOLLIERUNG DER GEAENDERTEN SLOTS
;


(def$method (restriction-net :init-slot-state) ()

  "	initialisiert die Slot-Listen, um die
  	Herstellung eines ersten stabilen Zustands
  	zu ermoeglichen"

  (setf changed-slots interface)
  (setf more-restricted-slots interface))


(def$method (restriction-net :reset-slot-state) ()

  "	loescht alle protokollierten Schreibvorgaenge"

  (setf changed-slots nil)
  (setf more-restricted-slots nil))


(def$method (restriction-net :update-slot-state) (slot-ref old-value)
  
  "	protokolliert Schreibvorgang auf Slot"
  
  (cond ((or (member slot-ref changed-slots :test 'equal)
	     (member slot-ref more-restricted-slots :test 'equal)))
	
	((undetermined-slot-value-p old-value)
	 (setf more-restricted-slots
	       (cons slot-ref more-restricted-slots)))
	(t
	 (setf changed-slots
	       (cons slot-ref changed-slots)))))


;
;	ABBILDUNG DER SLOTS AUF NETZVARIABLEN
;



(def$method (restriction-net :copy-values) ()
  
  "	kopiere die Werte aller Slots auf die Netzvariablen"
  
  (mapc (function
	  (lambda (info-assoc)
	    (add-var-info-values
	      info-assoc
	      (copy-slot-value
		(get-net-var info-assoc)))))
	net-spec))

   
(def$method (restriction-net :copy-possible-values) ()

  "	kopiere :possible-values auf Netzvariablen"

  (mapc (function
	  (lambda (info-assoc)
	    (add-var-info-values
	      info-assoc
	      (copy-possible-values
		(get-net-var info-assoc)))))
	net-spec))


(def$method (restriction-net :replace-values) ()
  
  ;;;	fuehrt fuer jede Netzvariable mit einelementiger
  ;;;   Wertemenge einen Schreibversuch auf die Slots durch
  
  (mapc (function
	  (lambda (info-assoc)
	    (replace-slot-value
	      (get-net-var info-assoc)
	      (get-var-info-values info-assoc))))
	net-spec))


(def$method (restriction-net :replace-possible-values) ()
  
  "	ersetzt die :possible-values-Komponente der Slots
  	durch die Wertebelgung der entsprechenden
  	Netzvariablen"
  
  (mapc (function
	  (lambda (info-assoc)
	    (replace-possible-values
	      (get-net-var info-assoc)
	      (get-var-info-values info-assoc))))
	net-spec))




;;; eof

