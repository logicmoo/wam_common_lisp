;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-


(in-package "BABYLON")

;
;	KONSTRUKTOR FUER RESTRICTIONS
;



(defmacro defrestriction (name &rest expr)

  `(send-kb
     :new&delete-restriction
     ',name
     ',(get-guarded-slots expr)
     ',(get-protected-slots expr)
     ',(get-restrictions expr)))


;
;	HILFSFUNKTIONEN
;


(defun get-guarded-slots (expr)

  (let ((entry (assoc :guarded-slots expr :test 'equal)))
    (if (null entry) nil
	(cdr entry))))


(defun get-protected-slots (expr)

  (let ((entry (assoc :protected-slots expr :test 'equal)))
    (if (null entry) nil
	(cdr entry))))


(defun get-restrictions (expr)

  (let ((entry (assoc :restrictions expr :test 'equal)))
    (if (null entry)
	(baberror (getentry no-restrictions constraint-io-table)
		expr)
	(cdr entry))))


(defmacro get-inst-assignment (generic-restriction)

  `(cdr ,generic-restriction))


(defmacro inst-assignment-p (inst-ass)

  " 	ueberprueft, ob der Anfang der Liste inst-ass eine
  	Belegung einer variablen darstellt"

  `(and (> (length ,inst-ass) 2)
	(atom (first ,inst-ass))
	(eq (second ,inst-ass) '=)
	(not (eq (first ,inst-ass) :if))))


(defmacro get-var-of-inst-ass (inst-ass)

  `(first ,inst-ass))


(defmacro get-set-of-instances (inst-ass)

  `(third ,inst-ass))


(defmacro next-inst-assignment (inst-ass)

  `(rest (rest (rest ,inst-ass))))


(defun get-instance-condition (expr)

  (cond ((null (rest expr)) T)
	((eq (first expr) :if)
	 (second expr))
	(t (get-instance-condition (rest expr)))))


(defun get-uninstantiated-restriction (restriction)

  (car (last restriction)))


(defun get-slot-refs (expr)
  
  (cond ((generic-expr-p expr)
         (get-slot-refs (get-inst-assignment expr)))
        ((inst-assignment-p expr)
         (get-slot-refs (next-inst-assignment expr)))
        ((null expr)
         (baberror (getentry slot-description-error constraint-io-table)))
        (T expr)))



;
;	INSTANTIIERUNG VON RESTRICTIONS
;


(defun instantiate-restrictions (list-of-restriction)
  
  "	ermittelt Menge aller Restrictions, die durch
  	list-of-restrictions beschrieben werden"
  
  (if (null list-of-restriction) nil
      (union-sets
	(inst-restriction
	  (first list-of-restriction))
	(instantiate-restrictions
	  (rest list-of-restriction)))))


(defun inst-restriction (restriction)

  "	ermittelt Menge aller Restrictions, die durch
  	list-of-restrictions beschrieben werden"
  
  (if (generic-expr-p restriction)
      (inst-generic-restriction restriction)
      (inst-simple-restriction restriction)))


(defun generic-expr-p (restriction)
  (eq (first restriction) :for-all))


(defun inst-simple-restriction (restriction)
  (list restriction))


(defun inst-generic-restriction (restriction)
  
  "	fuer alle Wertekombinationen der Variablen wird
  	die eingeschlossene uninstantiierte Restriction
  	instantiiert, falls die Variablen die angegebene
  	Bedingung erfuellen"
  
  (mapcar (function
	    (lambda (simple-alist)
	      (inst-uninstantiated-restriction
		(get-uninstantiated-restriction
		  restriction)
		simple-alist)))
	  (get-instance-combinations
	    restriction)))


(defun inst-uninstantiated-restriction (restriction simple-alist)
  
  "	ersetzt die Variablen in restriction durch die Instanz,
  	die ihnen simple-alist zuweist"
  
  (make-c-expr
    (get-constr-name restriction)
    (inst-slot-ref-list (get-parameters restriction)
			simple-alist)))


(defun inst-slot-ref-list (slot-ref-list simple-alist)
  
  "	instantiiert alle Slots in slot-ref-list"
  
  (mapcar (function
	    (lambda (slot-ref)
	      (inst-slot-ref slot-ref simple-alist)))
	  slot-ref-list))


(defun inst-slot-ref (slot-ref simple-alist)
  
  "	falls die erste Komponente von slot-ref in simple-alist
  	auftritt, wird sie durch die entsprechende Instanz ersetzt"
  
  (let ((instance-assoc (assoc (get-object-of-slot-ref
				 slot-ref)
			       simple-alist :test 'equal)))
    (if (null instance-assoc)
	slot-ref
	(make-slot-ref
	  (get-simple-value instance-assoc)
	  (get-slot-of-slot-ref slot-ref)))))


;
;	ERMITTLE INSTANZENKOMBINATIONEN
;


(defun get-instance-combinations (restriction)
  
  "	liefert die Menge von Assoziationslisten,
  	fuer die die eingeschlossene restriction
  	instantiiert werden soll"
  
  (select-instance-combinations
    (split-variable-alist
      (purge-instance-alist
	(make-$instance-alist
	  (get-inst-assignment restriction))
	(get-parameters
	  (get-uninstantiated-restriction restriction))))
    (get-instance-condition restriction)))


(defun make-$instance-alist (inst-ass)

  "	baut eine Assoziationsliste fuer die variablen
  	des for-all-Konstrukts"
  
  (if (not (inst-assignment-p inst-ass)) nil
      (cons (make-value-assoc
	      (get-var-of-inst-ass inst-ass)
	      (determine-set-of-instances
		(get-set-of-instances inst-ass)))
	    (make-$instance-alist
	      (next-inst-assignment inst-ass)))))


(defun purge-instance-alist (alist slot-ref-list)
  
  "	entfernt alle Variablen aus alist, die nicht in
  	slot-ref-list auftreten"
  
  (cond ((null alist) nil)
	((occurs-in-restriction (get-var (first alist))
				slot-ref-list)
	 (cons (first alist)
	       (purge-instance-alist (rest alist)
				     slot-ref-list)))
	(T
	 (purge-instance-alist (rest alist)
			       slot-ref-list))))


(defun occurs-in-restriction (variable slot-ref-list)
  
  "	ueberprueft, ob die Variable auch tatsaechlich in
  	slot-ref-list auftritt"
  
  (and slot-ref-list
       (or (equal variable
		  (get-object-of-slot-ref
		    (first slot-ref-list)))
	   (occurs-in-restriction variable
				  (rest slot-ref-list)))))


(defun select-instance-combinations (list-of-alists condition)
  
  "	waehlt all diejenigen alists aus, die die Bedingung
  	erfuellen"
  
  (cond ((null list-of-alists) nil)
	((evaluate-condition condition
			     (first list-of-alists))
	 (cons (first list-of-alists)
	       (select-instance-combinations
		 (rest list-of-alists)
		 condition)))
	(T (select-instance-combinations
	     (rest list-of-alists)
	     condition))))


(defun determine-set-of-instances (expr)

  "	ermittelt eine Menge von Instanzen"

  (case (first expr)
    (:instance-of
     (get-all-instances (second expr)))
    (:one-of
     (cdr expr))))


;
;	Slot-Mengen instantiieren
;


(defun determine-slots (set-of-slots all-slots)

  "	instantiiert alle Slot-Referenzen in set-of-slots"

  (case (car set-of-slots)
    (:none nil)
    (:all all-slots)
    (otherwise
     (instantiate-slots set-of-slots))))


(defun instantiate-slots (set-of-slots)
  
  (if (null set-of-slots) nil
      (union-sets
	(if (generic-expr-p (first set-of-slots))
	    (inst-slot-set (first set-of-slots))
	    (list (first set-of-slots)))
	(instantiate-slots (rest set-of-slots)))))


(defun inst-slot-set (slot-description)
  
  "	instantiiert die eingeschlossenen Slot-Referenzen"
  
  (remove-duplicates
    (mapcan (function
	      (lambda (simple-alist)
		(inst-slot-ref-list
		  (get-slot-refs slot-description)
		  simple-alist)))
	    (split-variable-alist
	      (make-$instance-alist
		(get-inst-assignment slot-description))))))


;
;	Constraintausdruecke auf Slots: DEFINITION
;




(def$flavor restriction-base
	((restriction-nets nil))
	()
  
  :settable-instance-variables
  :initable-instance-variables)



;
;	ZUGRIFF AUF RESTRICTION NETS
;


(def$method (restriction-base :get-restrictions) (name)

  "	Zugriff auf Restriction-Net ueber Name"

  (let ((c-assoc (assoc name restriction-nets :test 'equal)))
    (if c-assoc
	(get-object-of-c-assoc c-assoc))))


(def$method (restriction-base :put-restrictions) (name net)

  "	Eintrag eines neuen Restriction-Nets"

  (setf restriction-nets
	(cons (make-constraint-assoc
		name net)
	      restriction-nets)))


(def$method (restriction-base :is-defined-p) (r-net)

  "	testet, ob das Restriction-Net r-net definiert, d.h. in der
        restriction-base eingetragen ist"

  (rassoc r-net restriction-nets))


(def$method (restriction-base :delete-restrictions) (name)

  "	Loeschen eines Restriction-net"
  
  (let ((c-assoc (assoc name restriction-nets :test 'equal)))
    (if c-assoc
	(setf restriction-nets
	      (remove c-assoc restriction-nets :test 'equal)))))


;
;	DEFINITION EINES NETZES
;


(def$method (restriction-base :new-restriction)
	    (name guarded-slots protected-slots restrictions)
  
  "	Definition eines neuen Restriction-Netzes"
  
  (if (or ($send self :get name)
	  ($send self :get-restrictions name))
      nil
      (let ((new-net
	      (make-$instance 'restriction-net)))
	
	($send self :put-restrictions
	       name new-net)
	
	($send new-net :store-definition
	       restrictions
	       guarded-slots
	       protected-slots)
	
	($send new-net :set-net-spec
	       (create-net-spec
		 (instantiate-restrictions restrictions)))
	
	($send new-net :set-interface
	       ($send new-net :net-variables))
	
	($send new-net :make-active-values
	       (determine-slots guarded-slots
				($send new-net :net-variables))
	       (determine-slots protected-slots
				($send new-net :net-variables))))))



(def$method (restriction-base :redefine) (name)
  
  (let ((r-net ($send self :get-restrictions name)))
    (if (null r-net) nil
	($send r-net :redefine-one))))


(def$method (restriction-base :redefine-all) ()
  
  "	erzeugt alle Restriction-Netze neu"
  
  (mapc (function
	  (lambda (constraint-assoc)
	    ($send (get-object-of-c-assoc
		    constraint-assoc)
		  :redefine-one)))
	restriction-nets))


(def$method (restriction-net :redefine-one) ()
  
  "	Erzeugen eines neuen Netzes mit Hilfe der generischen
  	Beschreibung
  	(noetig bei Aenderung von Wissensbasiskomponenten, 
  	die in der Definition des Netzes benutzt werden"
  
  ($send self :set-net-spec
	(create-net-spec
	  (instantiate-restrictions
	    ($send self :restrictions))))
  
  ($send self :set-interface
	($send self :net-variables))
  
  ($send self :make-active-values
	(determine-slots
	  ($send self :guarded)
	  ($send self :net-variables))
	(determine-slots
	  ($send self :protected)
	  ($send self :net-variables)))
  
  ($send self :set-agenda
	(make-agenda-elem))
  ($send self :set-stack nil))


(def$method (restriction-base :new&delete-restriction)
	    (name guarded-slots protected-slots restrictions)
  
  ($send self :delete-restrictions name)
  ($send self :new-restriction
	 name
	 guarded-slots
	 protected-slots
	 restrictions))



;;; eof

