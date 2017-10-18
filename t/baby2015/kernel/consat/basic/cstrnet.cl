;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-


(in-package "BABYLON")


;
; 		Constraint-Netz
;

;  3.6. 1987  Anpassung an GENERA 7; R. Lopatta
; 

;
;	CONSTRAINT-NETZ
;


;	variable infos:		Angaben zu einer globalen Variablen
;
;	Structure mit folgenden Komponenten
;		constraints:	list( <constraint-expression> )
;				{constraints, in denen diese Variable auftritt}
;		values:		<value specification>
;		init-values:	<value specification>

;	agenda-element:
;
;	Structure mit folgenden Komponenten
;		queue:		list( <constraint-expression> )
;		trace:		list( <trace-element> )
;		init-trace:	list( <trace-element> )
;		filtered-p:	T | nil
;				{wurde Vorpropagierung bereits durchgefuehrt ?}
;
;

(defstruct (agenda-elem)
  (queue nil)
  (trace nil)
  (init-trace nil)
  (filtered-p nil))


;	stack-element:
;
;	Structure mit folgenden Komponenten
;		values:	        list( <value association> )
;		queue:		list( <constraint-expression> )
;		trace:		list( <trace-element> )

(defstruct (stack-elem)
  (values nil)
  (queue nil)
  (trace nil))


;	Constraint-Netz:
;
;	Flavor mit folgenden Variablen
;		interface:	list( <interface variable> )
;		net-spec: 	list( <info association> )
;				{Liste der globalen Variablen
;				 mit weiteren Angaben}
;		agenda:		list( <agenda-element> )
;		stack:		list( <stack-element> )
;		

(def$flavor constraint-net

	((interface nil)
	 (net-spec nil)
	 (agenda (make-agenda-elem))
	 (stack nil))
	()
  
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables
  )


(def$method (constraint-net :print) (name stream)
  
  ;;;	Ausgabe des Constraint-Netzes
  
  (princ " " stream)
  (terpri stream)
  (babpprint
    `(defconstraint ,name
       (:type compound)
       (:interface . ,interface)
       (:constraint-expressions . ,(select-all-constraints net-spec)))
    stream)
  (terpri stream))




;
;	NETZSPEZIFIKATION ERMITTELN
;


(defun create-net-spec (c-expressions)
  
  (create-var-info-alist
    (determine-net-variables c-expressions)
    c-expressions))


(defun create-var-info-alist (net-vars c-expressions)
  
  ;;; erzeugt eine Assoziationsliste, die fuer jede
  ;;; Netzvariable die noetigen Angaben enthaelt
  
  (mapcar (function
	    (lambda (net-var)
	      (make-info-assoc
		net-var
		(make-var-info
		  :constraints (get-associated-constraints
				c-expressions
				net-var)))))
	  net-vars))


(defun get-associated-constraints (c-expressions net-var)

  ;;; ermittelt eine Liste der extended-constraints,
  ;;; in denen net-var als globale Variable vorkommt

  (cond ((null c-expressions) nil)
	((member
	   net-var
	   (get-parameters (first c-expressions))
	   :test 'equal)
	 (cons (first c-expressions)
	       (get-associated-constraints
		 (rest c-expressions)
		 net-var)))
	(t (get-associated-constraints
		 (rest c-expressions)
		 net-var))))


(defun determine-net-variables (c-expressions)
  
  ;;; ermittelt mit Hilfe der Variablenassoziationen der
  ;;; constraint-expressions die Variablen des Netzes
  
  (if (null c-expressions)
      nil
      (union-sets
	(remove-duplicates
	  (get-parameters (first c-expressions))
	  :test #'equal)
	(determine-net-variables
	  (rest c-expressions)))))


;
;	NETZSPEZIFIKATION AKTUALISIEREN
;


(defun update-net-value-ass (new-value-ass net-spec)
  
  ;;; ergaenzt die Wertemengen der Variablen in variables
  ;;; um die neuen Wertemengen aus new-value-ass
  ;;; (Bildung der Schnittmenge)
  
  (mapc (function
	  (lambda (new-value-assoc)
	    (add-var-info-values
	      (assoc (get-var new-value-assoc)
		     net-spec
		     :test 'equal)
	      (get-value-spec new-value-assoc))))
	new-value-ass))


(defun modify-net-value-ass (new-value-ass net-spec)

  ;;; die Variablen-Werte-Assoziationen in new-value-ass ersetzen
  ;;; entsprechende Eintraege in net-spec
  ;;; (die var-info-values-Komponenten werden durch
  ;;;  SEITENEFFEKTE geaendert)

  (mapc (function
	  (lambda (new-value-assoc)
	    (replace-var-info-values
	      (assoc (get-var new-value-assoc)
		     net-spec
		     :test 'equal)
	      (get-value-spec new-value-assoc))))
	new-value-ass))


;
;	CONSTRAINT-AUSDRUECKE AUSWAEHLEN
;


(defun select-all-constraints (net-spec)
  
  ;;; bestimmt die Menge aller Constraint-Ausdruecke,
  
  (cond ((null net-spec) nil)
	(t (union-sets
	     (get-var-info-constraints (first net-spec))
	     (select-all-constraints (rest net-spec))))))


(defun select-relevant-constraints (net-spec value-ass)
  
  ;;; bestimmt die Menge aller Constraint-Ausdruecke,
  ;;; in denen eine globale Variable auftritt,
  ;;; der durch value-ass eine Wertemenge ungleich 'unconstrained
  ;;; zugeordnet wird
  
  (cond ((null value-ass) nil)
	((eq (get-value-spec (first value-ass))
	     'unconstrained)
	 (select-relevant-constraints net-spec
				      (rest value-ass)))
	(t (union-sets
	     (get-var-info-constraints
	       (assoc (get-var (first value-ass))
		      net-spec
		      :test 'equal))
	     (select-relevant-constraints net-spec
					  (rest value-ass))))))


;
;	FUNKTIONEN FUER CHOICE-POINT
;


(defun state-of-net-spec (net-spec &optional (state 'single-valued))
  
  ;;; ueberprueft zustand der variablenbelegung
  
  (if (null net-spec) state
      (let ((value-spec (get-var-info-values
                         (first net-spec))))
        (declare (list value-spec))
        (cond ((null value-spec) 'inconsistent)
              ((eq value-spec 'unconstrained)
               (state-of-net-spec (rest net-spec)
                                  'unconstrained))
              ((= (length value-spec) 1)
               (state-of-net-spec (rest net-spec) state))
              (t (state-of-net-spec (rest net-spec)
                                    (if (eq state 'unconstrained)
                                      'unconstrained
                                      'multiple-valued)))))))


(defun state-of-value-ass (value-ass &optional (state 'single-valued))
  
  ;;; ueberprueft Zustand der variablenbelegung
  
  (if (null value-ass) state
      (let ((value-spec (get-value-spec (first value-ass))))
        (declare (list value-spec))
        (cond
         ((null value-spec) 'inconsistent)
         ((eq value-spec 'unconstrained)
          (state-of-value-ass (rest value-ass)
                              'unconstrained))
         ((= (length value-spec) 1)
          (state-of-value-ass (rest value-ass)
                              state))
         (t (state-of-value-ass (rest value-ass)
                                'multiple-valued))))))


(defun select-multiple-valued-variable (net-spec &optional candidate)
  
  ;;; liefert eine Netzvariable (mit Zusatzangaben)
  ;;; mit der kleinsten mehrelementigen Wertemenge
  
  (if (null net-spec) candidate
      (let ((new-length (length (the list (get-var-info-values (first net-spec))))))
        (select-multiple-valued-variable
         (rest net-spec)
         (if (or (<= new-length 1)
                 (and candidate
                      (<= (length (the list (get-var-info-values candidate)))
                          new-length)))	      
           candidate
           (first net-spec))))))


(defun consistent-value-ass-p (value-ass)

  ;;; liefert nil, falls eine Variable existiert,
  ;;; die mit der leeren Liste markiert ist;
  ;;;
  ;;; ansonsten wird value-ass oder T zurueckgegeben

  (cond ((null value-ass) T)
	((null (get-value-spec (car value-ass))) nil)
	((consistent-value-ass-p (cdr value-ass))
	 value-ass)
	(T nil)))
