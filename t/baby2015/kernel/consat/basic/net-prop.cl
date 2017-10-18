;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-


(in-package "BABYLON")

;
;		Constraint Propagierung
;

;  3.6. 1987  Anpassung an GENERA 7; R. Lopatta
; 

;
; 	PROPAGIERUNG
;


(def$method (constraint-net :activate)
	   (multiple-value-ass &optional
			       (init-option 'initialize)
			       (consistency-level 'local-consistency)
			       (number-of-results nil))
  
  
  ;;; Eingabe: 	multiple Wertebelegung der Interfacevariablen,
  ;;;		Option: Variablen und Trace mit Defaultwert
  ;;;			initialisieren
  ;;;		Option: nach Propagierung Konsistenztest
  ;;;		 	durchfuehren 
  ;;;           Option: Anzahl der global konsistenten Ergebnisse
  ;;;
  ;;; Ausgabe:  multiple Wertebelegung nach Propagierung,
  ;;;           falls consistency-level = local-consistency,
  ;;;           Liste mit global konsistenten Wertebelegungen,
  ;;;           falls consistency-level = global-consistency

  (catch 'error
    ($send self :initialize multiple-value-ass init-option)
    ($send self :propagate consistency-level)
    ($send self :result consistency-level number-of-results)))


    
(def$method (constraint-net :initialize) (multiple-value-ass init-option)
  
  ;;; initialisiert das Netz 
  ;;;
  ;;; falls init-option = 'initialize werden die Variablen mit der
  ;;; Defaultwertmenge belegt und der Default-Trace uebernommen
  
  ($send self :store-state)
  ($send self :filter)
  (if (eq init-option 'initialize)
      ($send self :get-initiale-state))
  ($send self :initialize-variables multiple-value-ass)
  ($send self :initialize-agenda multiple-value-ass))



(def$method (constraint-net :result) (consistency-level number-of-results)
  
  ;;; fuehrt evtl. einen Konsistenztest durch
  ;;;
  ;;; Ergebnis: Belegung der Interface-Variablen
  
  (prog1
    (case consistency-level
      (global-consistency
       ($send self :consistent-p number-of-results))
      (local-consistency
       ($send self :interface-assignment))
      (global-consistency-if-single-valued
       ($send self :test-consistency-if-single-valued)))
    ($send self :restore-state)))



(def$method (constraint-net :filter) ()
  
  ;;; fuehrt Vorpropagierung durch:
  ;;; alle Constraint-Ausdruecke werden mindestens einmal
  ;;; aktiviert
  
  (cond ((not (agenda-elem-filtered-p agenda))
	 
	 (setf (agenda-elem-filtered-p agenda) t)	
	 ($send self :total-init-queue)
	 ($send self :propagate 'local-consistency)	 
	 ($send self :freeze-state))))



(def$method (constraint-net :propagate) (consistency-level)
  
  ;;; fuehrt lokale Propagierung durch
  ;;; bis Agenda leer ist
  
  (do ()
      ((null (agenda-elem-queue agenda))
       ())
    
    (let* ((constraint-expr (first (agenda-elem-queue agenda)))
	   (new-value-ass ($send (get-constraint
				  (get-constr-name constraint-expr))
				:evaluate-expression
				constraint-expr
				net-spec
				consistency-level)))
      
      ($send self :update-variables
	    new-value-ass)	  
      ($send self :update-agenda
	    constraint-expr
	    new-value-ass))))



(def$method (constraint-net :evaluate-expression)
	    (constraint-expr global-net-spec consistency-level)
  
  ;;; fuehrt Umsetzung globaler in lokale Variablen durch
  ;;; und umgekehrt
  
  (local-to-global-subst
    constraint-expr
    ($send self
	   :activate
	   (global-to-local-subst
	     constraint-expr
	     global-net-spec)
	   'initialize
	   (adapt-consistency-level consistency-level))))


(defun adapt-consistency-level (consistency-level)
  
  ;;;	bei Test auf globale Konsistenz wird solange wie
  ;;;	moeglich lokales Propagieren ausgefuehrt;
  ;;;	nur bei eindeutiger Belegung der Interfacevariablen
  ;;;	wird in dem zu aktivierenden teilnetz ein
  ;;;	globaler Konsistenztest durchgefuehrt
  
  (if (eq consistency-level 'global-consistency)
      'global-consistency-if-single-valued
      consistency-level))


;
;	KONSISTENZTEST DURCHFUEHREN
;


(def$method (constraint-net :consistent-p)
	    (&optional (number-of-results nil))
  
  ;;; ueberprueft die globale konsistenz des netzwerks
  ;;; mittels backtracking + lokaler propagierung
  
  (case (state-of-net-spec net-spec)
    (inconsistent nil)
    (unconstrained
     (list ($send self :interface-assignment)))   ;;; ???
    (single-valued
     (list ($send self :interface-assignment)))
    (multiple-valued
     (let ((info-assoc (select-multiple-valued-variable
			 net-spec)))
       ($send self :test-choices
	      (get-net-var info-assoc)
	      (get-var-info-values info-assoc)
	      number-of-results)))))


(def$method (constraint-net :test-choices)
	    (variable value-set number-of-results)
  
  ;;; fuer alle Werte w aus value-set wird das aktuelle
  ;;; Constraint-Netz aktiviert mit (variable = w)
  ;;; bis number-of-results Belegungen gefunden sind
  ;;;
  ;;; tritt dies nicht ein, werden die gefundenen Belegungen
  ;;; zurueckgeliefert (nil, falls das Netz inkonsistent ist)
  
  (cond ((null value-set) nil)                ;;; Inkonsistenz
	((and (not (null number-of-results))
	      (<= number-of-results 0)) nil)  ;;; hinreichend viele Belegungen gefunden
	
	(t (let ((list-of-value-ass ($send self :activate
					   (list (make-value-assoc
						   variable
						   (list (first value-set))))
					   'continue
					   'global-consistency
					   number-of-results)))
	     (append list-of-value-ass
		     ($send self :test-choices
			    variable
			    (rest value-set)
			    (compute-new-number-of-results
			      number-of-results
			      list-of-value-ass)))))))


(defun compute-new-number-of-results (number-of-results list-of-value-ass)
  
  ;;; berechnet die Anzahl der Belegungen, die noch
  ;;; ermittelt werden muessen
  
  (declare (list list-of-value-ass))
  (if (null number-of-results) nil
      (- number-of-results (the fixnum (length list-of-value-ass)))))


(def$method (constraint-net :test-consistency-if-single-valued) ()
  
  ;;;	falls alle Interface-Variablen einen eindeutigen Wert besitzen,
  ;;;	wird zusaetzlich ein konsistenztest durchgefuehrt
  
  (if (eq (state-of-value-ass
	    ($send self :interface-assignment))
	  'single-valued)
      ($send self :adapt-to-local-consistency ($send self :consistent-p 1))
      ($send self :interface-assignment)))


(def$method (constraint-net :adapt-to-local-consistency) (list-of-value-ass)

  ;;; gleicht list-of-value-ass dem Ergebnis an, das
  ;;; bei einer lokalen Propagierung entstehen wuerde

  (if (null list-of-value-ass)
      (empty-alist interface)
      (first list-of-value-ass)))


;
;	OPERATIONEN AUF NETZ-VARIABLEN
;


(def$method (constraint-net :initialize-variables) (multiple-value-ass)
  
  ;;; ergaenzt die Wertebelegung jeder Interface-Variablen
  ;;; in multiple-value-ass um die Wertemenge, die ihr
  ;;; multiple-value-ass zuordnet
  
  (update-net-value-ass multiple-value-ass
			net-spec))


(def$method (constraint-net :update-variables) (multiple-value-ass)

  ;;; aktualisiert die Wertebelegung der Netzvariablen durch
  ;;; die Wertebelegung von multiple-value-ass

  (modify-net-value-ass multiple-value-ass
			net-spec))


(def$method (constraint-net :interface-assignment) ()
  
  ;;; liefert die Wertebelegung der Interface-Variablen
  
  (mapcar (function
	    (lambda (interface-var)
	      (make-value-assoc
		interface-var
		(get-var-info-values
		  (assoc interface-var net-spec
			 :test 'equal)))))
	  interface))


(def$method (constraint-net :net-variables) ()
  
  ;;;	liefert eine Liste aller Netzvariablen
  
  (mapcar (function
	    (lambda (info-assoc)
	      (get-net-var info-assoc)))
	  net-spec))


;
;	AGENDA-OPERATIONEN
;


(def$method (constraint-net :initialize-agenda) (multiple-value-ass)
  
  ;;; alle Constraint-Ausdruecke mit Variablen, denen eine Menge ungleich
  ;;; 'unconstrained zugeordnet wurde, werden in die queue gefuegt
  
  (setf (agenda-elem-queue agenda)
	(select-relevant-constraints
	  net-spec multiple-value-ass)))


(def$method (constraint-net :total-init-queue) ()

  ;;; initialisiert die Queue mit allen Constraint-Ausdruecken

  (setf (agenda-elem-queue agenda)
	(select-all-constraints net-spec)))


(def$method (constraint-net :update-agenda) (constraint-expr multiple-value-ass)
  
  ;;;  - fuege  (constraint-expr multiple-value-ass)  auf den Trace
  ;;;
  ;;;  - Bilde die Menge aller Constraint-Ausdruecke, in denen globale
  ;;;    Variablen aus multiple-value-ass vorkommen, deren Wertemenge
  ;;;    ungleich 'unconstrained ist
  ;;;
  ;;;  - Aktualisiere die queue mit Hilfe dieser Menge
  
  (setf (agenda-elem-trace agenda)
	(cons (make-trace-elem
		constraint-expr
		multiple-value-ass)
	      (agenda-elem-trace agenda)))
  
  (setf (agenda-elem-queue agenda)
	(update-queue
	  (rest (agenda-elem-queue agenda))
	  (remove-duplicates
	    (select-relevant-constraints
	      net-spec
	      multiple-value-ass)
	    :test #'equal)
	  (agenda-elem-trace agenda)
	  multiple-value-ass)))


(defun update-queue (old-queue list-of-constr-expr
		     trace new-value-ass)

  ;;; Fuer jeden constraint-Ausdruck in list-of-constr-expr Tue:
  ;;;
  ;;;   - falls er bereits aktiviert wurde, pruefe,
  ;;;		ob es in new-value-ass eine Variable gibt,
  ;;;		die in dem Constraint-Ausdruck auftritt und
  ;;;		jetzt eine kleinere Wertemenge als damals besitzt
  ;;;
  ;;;   - falls diese Bedingung gilt oder der Constraint-Ausdruck
  ;;;	  noch nicht aktiviert wurde,
  ;;;	  fuege ihn in die Queue (falls noch nicht vorhanden)

  (cond ((null list-of-constr-expr)
	 old-queue)
	((and (trace-test
		(assoc (first list-of-constr-expr) trace
		       :test 'equal)
		new-value-ass)
	      (not (member (first list-of-constr-expr)
			   old-queue
			   :test 'equal)))
	 (cons (first list-of-constr-expr)
	       (update-queue old-queue
			     (rest list-of-constr-expr)
			     trace
			     new-value-ass)))
	(t (update-queue old-queue
			 (rest list-of-constr-expr)
			 trace
			 new-value-ass))))


(defun trace-test (associated-trace-element new-value-ass)
  
  (if (null associated-trace-element) t
      (some-new-restrictions-p
	new-value-ass
	(get-trace-value-ass
	  associated-trace-element))))


;
;	STACK-OPERATIONEN
;


(def$method (constraint-net :store-state) ()
  
  ;;; rettet die Variablenbelegung und die Agenda auf den Stack;
  
  (setf stack
	(cons (make-stack-elem
		:values (mapcar
			 (function
			   (lambda (info-assoc)
			     (make-value-assoc
			       (get-net-var info-assoc)
			       (get-var-info-values info-assoc))))
			 net-spec)
		
		:queue (agenda-elem-queue agenda)
		:trace (agenda-elem-trace agenda))
	      stack)))


(def$method (constraint-net :restore-state) ()
  
  ;;; stellt Zustand gemaess des obersten Stackelements wieder her
  ;;; und entfernt das oberste Stackelement vom Stack
  
  (cond ((null stack)
	 (baberror (getentry restore-error constraint-io-table)))
	(t (modify-net-value-ass (stack-elem-values (first stack))
				 net-spec)
	   (setf (agenda-elem-queue agenda)
		 (stack-elem-queue (first stack)))
	   (setf (agenda-elem-trace agenda)
		 (stack-elem-trace (first stack)))
	   ($send self :set-stack
		  (rest stack)))))


(def$method (constraint-net :forget-state) ()

  ;;; loescht das oberste Stackelement:
  ;;; dieser Zustand wird also vergessen

  (setf stack (rest stack)))


(def$method (constraint-net :freeze-state) ()

  ;;; "friert" die aktuelle Variablenbelegung und den
  ;;; aktuellen Trace dauerhaft ein

  (mapc (function freeze-var-info-values)
	net-spec)
  
  (setf (agenda-elem-init-trace agenda)
        (agenda-elem-trace agenda)))


(def$method (constraint-net :get-initiale-state) ()

  ;;; initialisiert die Variablen und den trace mit den
  ;;; entsprechenden Defaultwerten

  (mapc (function init-var-info-values)
	net-spec)

  (setf (agenda-elem-trace agenda)
	(agenda-elem-init-trace agenda)))


(def$method (constraint-net :reset-state) ()

  ;;; setzt die Variablen-Defaultwerte auf 'unconstrained
  ;;; und init-trace auf nil

  (mapc (function reset-var-info-values)
	net-spec)

  (setf (agenda-elem-init-trace agenda)
	nil)

  (setf (agenda-elem-filtered-p agenda)
	nil))

;;; eof



