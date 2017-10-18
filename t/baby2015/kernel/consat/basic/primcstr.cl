;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-


(in-package "BABYLON")


;
;		Evaluierung primitiver Constraints
;
;
;  3.6. 1987  Anpassung an GENERA 7; R. Lopatta
; 

;
;	PRIMITIVES CONSTRAINT
;
;
;	Flavor mit folgenden Variablen 
;		interface:  	list( <variables> )
;		relation:	<relation>
;		condition:	<lisp-expr>
;

;
;	Consat-SCHNITTSTELLE
;


(defun evaluate-funcall (expression &optional (simple-value-ass nil))
  
  "Zugriff von consat auf andere Prozessoren"
  
  (send-kb :eval
	   (substitute-constraint-variables
	     expression
	     simple-value-ass)
	   :recall
	   'constraint-processor))


(defun evaluate-condition (expression simple-value-ass)
  
  "Zugriff von consat auf andere Prozessoren"
  
  (or (eq expression 't)
      (send-kb :eval
	       (substitute-constraint-variables
		 expression
		 simple-value-ass)
	       :recall
	       'constraint-processor)))


(defun substitute-constraint-variables (expr simple-value-ass)

  "ersetzt in expression alle Symbole, die in simple-value-ass auftreten,
   durch quote und den Wert, den ihnen simple-value-ass zuweist"

  (cond ((null expr) nil)
	((atom expr)
	 (substitute-if-possible
	   expr (assoc expr simple-value-ass)))
	(t
	 (cons (substitute-constraint-variables
		   (car expr)
		   simple-value-ass)
		 (substitute-constraint-variables
		   (cdr expr)
		   simple-value-ass)))))


(defun substitute-if-possible (symbol value-assoc)
  (if (null value-assoc)
      symbol
      `(quote ,(get-simple-value value-assoc))))



;
;  	BEHANDLUNG DER PARAMETER
;


(defun global-to-local-subst (c-expr net-spec)
  
  ;;; ermittelt Wertebelegung fuer Variablen des Constraints von c-expr
  
  (make-local-value-ass (get-parameters c-expr)
			($send (get-constraint
				(get-constr-name c-expr))
			      :interface)
			net-spec))


(defun local-to-global-subst (c-expr local-value-ass)
  
  ;;; ersetzt in local-value-ass die lokalen durch die in c-expr
  ;;; zugeordneten Variablen
  
  (make-determined-value-ass
    (remove-duplicates (get-parameters c-expr)
		       :test #'equal)
    (make-global-value-ass (get-parameters c-expr)
			   ($send (get-constraint
				   (get-constr-name c-expr))
				 :interface)
			   local-value-ass)))


(defun make-local-value-ass (global-vars local-vars net-spec)
  
  ;;; die i-te Variable in global-vars sei mit der i-ten Variablen
  ;;; in local-vars assoziiert;
  ;;; jede lokale Variable erhaelt die Wertemenge, die der entsprechenden
  ;;; globalen Variablen in net-spec zugeordnet ist
  
  (cond ((null global-vars)
	 (if (null local-vars) nil
	     (baberror (getentry length-error constraint-io-table)
		     global-vars local-vars)))
	((null local-vars)
	 (baberror (getentry length-error constraint-io-table)
		 global-vars local-vars))
	
	(t (cons (make-value-assoc
		   (first local-vars)
		   (get-var-info-values
		     (assoc (first global-vars) net-spec
			    :test 'equal)))
		 (make-local-value-ass
		   (rest global-vars)
		   (rest local-vars)
		   net-spec)))))


(defun make-global-value-ass (global-vars local-vars local-value-ass)
  
  ;;; die i-te Variable in global-vars sei mit der i-ten Variablen
  ;;; in local-vars assoziiert;
  ;;; jede globale Variable erhaelt die Wertemenge, die der entsprechenden
  ;;; lokalen Variablen in local-value-ass zugeordnet ist
  
  (cond ((null global-vars)
	 (if (null local-vars) nil
	     (baberror (getentry length-error constraint-io-table)
		     global-vars local-vars)))
	((null local-vars)
	 (baberror (getentry length-error constraint-io-table)
		 global-vars local-vars))
	
	(t (let ((val-assoc (assoc (first local-vars) local-value-ass
				   :test 'equal)))
	     (if (null val-assoc)
		 (baberror (getentry unknown-variable constraint-io-table)
			 (first local-vars)
			 local-value-ass)
		 (cons (make-value-assoc
			 (first global-vars)
			 (get-value-spec val-assoc))
		       (make-global-value-ass
			 (rest global-vars)
			 (rest local-vars)
			 local-value-ass)))))))


(defun make-determined-value-ass (variables value-ass)

  ;;; erzeugt eine eindeutige Assoziationsliste :
  ;;; jede Variable in variables erhaelt als Wert die Schnittmenge
  ;;; ihrer Werte in value-ass

  (mapcar (function
	    (lambda (variable)
	      (make-value-assoc
		variable
		(intersect-associated-value-specs
		  variable
		  value-ass))))
	  variables))


(defun intersect-associated-value-specs (variable value-ass)
  
  (cond ((null value-ass) 'unconstrained)
	((equal variable (get-var (first value-ass)))
	 (extended-intersection
	   (get-value-spec (first value-ass))
	   (intersect-associated-value-specs
	     variable
	     (rest value-ass))))
	(t (intersect-associated-value-specs
	     variable
	     (rest value-ass)))))

;;;


(def$flavor constraint

	((interface nil)
	 (relation nil)
	 (condition t)
	 (compiled-condition-flag nil))
	()
  
  :gettable-instance-variables
  :settable-instance-variables
;  :initable-instance-variables
  )


(def$method (constraint :print) (name stream)
  
  ;;;	Ausgabe des Constraints
  
  (princ " " stream)
  (terpri stream)
  (babpprint
    `(defconstraint ,name
       (:type primitive)
       (:interface . ,interface)
       (:relation . ,relation)
       (:condition ,(if compiled-condition-flag
			:or condition)))
    stream)
  (terpri stream))


;
;	EVALUATION
;


(def$method (constraint :activate) (new-value-ass
				     &optional
				     init-option
				     (consistency-level 'local-consistency)
				     (number-of-results nil))
  
  ;;; Eingabe:	Zuweisung von Wertemengen an die Constraint-Variablen
  ;;;
  ;;; Ausgabe:  neue Wertemengenzuweisung,
  ;;;           falls consistency-level = local-consistency;
  ;;;           Liste von Zuweisungen, die den Variablen einzelne Werte
  ;;;           zuordnen, falls consistency-level = global-consistency
  ;;;
  ;;; falls fuer eine Wertebelegung die activation-Bedingung
  ;;; nicht erfuellt ist, wird die alte Wertebelegung als Ergebnis
  ;;; zurueckgeliefert (bei local-consistency) bzw. die Liste aller
  ;;; moeglichen Einzelwertzuweisungen (bei global-consistency)
  ;;;
  ;;; der Parameter init-option ist ohne Bedeutung, muss wegen
  ;;; des Compilers jedoch mindestens einmal benutzt werden:
  
  init-option
  
  (catch 'error
    (let* ((multiple-value-ass (adjust-value-ass
				 interface
				 new-value-ass))
	   (list-of-value-ass (split-variable-alist
				multiple-value-ass)))
      
      (case consistency-level
	
	((local-consistency global-consistency-if-single-valued)	 
	 (if (activation-p condition list-of-value-ass)
	     (combine-variable-alists
	       (multiple-evaluation relation interface list-of-value-ass)
	       interface)
	     multiple-value-ass))
	
	(global-consistency
	  (select-some-value-ass
	    (if (activation-p condition list-of-value-ass)
		(multiple-evaluation relation interface
				     list-of-value-ass)
		(mapcar #'convert-simple-to-multiple
			list-of-value-ass))
	    number-of-results))))))
	  

(def$method (constraint :evaluate-expression)
  (constraint-expr global-net-spec &rest ignore)
  
  ;;; fuehrt Umsetzung globaler in lokale Variablen durch
  ;;; und umgekehrt
  (declare (ignore ignore))
  (local-to-global-subst
   
   constraint-expr
   ($send self
          :activate
          (global-to-local-subst
           constraint-expr
           global-net-spec))))


(defun multiple-evaluation (relation variables list-of-value-ass)
  
  ;;; falls keine einwertige Variablenbelegung existiert,
  ;;; wird die leere Liste geliefert;
  ;;;
  ;;; ansonsten wird fuer jede Wertebelegung die Relation
  ;;; evaluiert

  (if (null list-of-value-ass)
      nil
      (append
	(evaluate-relation relation
			   variables
			   (first list-of-value-ass))
	(multiple-evaluation relation
			     variables
			     (rest list-of-value-ass)))))


(defun activation-p (condition list-of-value-ass)

  ;;; ueberprueft die Bedingung fuer jede Wertebelegung in
  ;;; list-of-value-ass und bildet die Konjunktion der
  ;;; Ergebnisse

  (or (null list-of-value-ass)
      (and (evaluate-condition condition
			       (first list-of-value-ass))
	   (activation-p condition
			 (rest list-of-value-ass)))))


(defun evaluate-relation (relation variables simple-val-ass)

  ;;; Eingabe:	eine Liste von Relationenelemente,
  ;;;		eine Liste der Variablen,
  ;;;		eine Wertzuweisung
  ;;;
  ;;; Ausgabe:	neue (multiple) Wertezuweisung, in der im Vergleich zu
  ;;;		simple-val-ass uneingeschraenkte Variablen einen oder
  ;;;		mehrere neue Werte erhalten haben

  (if (null relation)
      nil
      (cons-if-not-nil
	(evaluate-relation-element (first relation)
				   variables
				   simple-val-ass)
	(evaluate-relation (rest relation)
			   variables
			   simple-val-ass))))


(defun evaluate-relation-element (rel-element variables simple-val-ass)
  
  ;;; Eingabe:	ein Relationenelement,
  ;;;		eine Liste der Variablen,
  ;;;		eine Wertzuweisung
  ;;;
  ;;; Ausgabe:	nil, falls simple-val-ass inkonsistent ist,
  ;;;		oder neue (multiple) Wertezuweisung, sonst
  
  (declare (list rel-element variables))
  (case (get-keyword rel-element)
    (:tuple (evaluate-tupel (get-tupel rel-element)
			    variables
			    simple-val-ass))
    (:pattern (if (or (= (length rel-element) 2)
		      (evaluate-condition (get-condition rel-element)
					  simple-val-ass))
		  (evaluate-pattern (get-expressions rel-element)
				     variables
				     simple-val-ass)
		  nil))))


(defun evaluate-tupel (tupel variables simple-val-ass
		       &optional (new-val-ass nil))

  ;;; Eingabe:	eine Liste von Konstanten,
  ;;;		eine Liste von Variablen und eine Wertzuweisung an diese Variablen
  ;;;
  ;;; Ausgabe:  nil, falls einer der Werte aus tupel nicht mit dem Wert der
  ;;;		    entsprechenden Variable in simple-val-ass vertraeglich ist
  ;;;		neue Wertzuweisung, sonst

  (cond ((null tupel) new-val-ass)
        ((compatible-value-p (first tupel)
			     (first variables)
			     simple-val-ass)
	 (evaluate-tupel (rest tupel)
			 (rest variables)
			 simple-val-ass
			 (cons (new-association
				 (first variables)
				 (first tupel))
			       new-val-ass)))
	(t nil)))


(defun evaluate-pattern (pattern variables simple-val-ass
			 &optional (new-val-ass nil))

  ;;; Eingabe:	eine Liste von Lisp-Ausdruecken,
  ;;;		eine Liste von Variablen und eine Wertzuweisung an diese Variablen
  ;;;
  ;;; Ausgabe:	es werden nacheinander alle Lisp-Ausdruecke in der Umgebung
  ;;;		simple-val-ass evaluiert;
  ;;;		falls dabei ein Wert ermittelt wird, der mit der Wertebelegung
  ;;;		der entsprechenden Variablen in simple-val-ass nicht
  ;;;		uebereinstimmt, wird mit nil abgebrochen;
  ;;;		ansonsten wird eine neue Wertebelgung ermittelt

  (if (null pattern)
      (reverse new-val-ass)
      (let ((new-value (evaluate-funcall (first pattern)
			 simple-val-ass)))
	(if (compatible-value-p new-value
				(first variables)
				simple-val-ass)
	    (evaluate-pattern (rest pattern)
			      (rest variables)
			      simple-val-ass
			      (cons (new-association
				      (first variables)
				      new-value)
				    new-val-ass))
	    nil))))


;
;	TEST
;


(defun constrained-p (&rest variables)

  ;;; Eingabe:	Liste von Variablen der aktuellen Lisp-Umgebung
  ;;;
  ;;; Ausgabe:	T, falls keine der Variablen den Wert 'unconstrained
  ;;;		besitzt

  (cond ((null variables))
	((eq (car variables)
	     'unconstrained)
	 nil)
	(t (apply (function constrained-p)
		  (cdr variables)))))


(defun unconstrained-p (&rest variables)

  ;;; Eingabe:	Liste von Variablen der aktuellen Lisp-Umgebung
  ;;;
  ;;; Ausgabe:	T, falls keine der Variablen den Wert 'constrained
  ;;;		besitzt

  (cond ((null variables))
	((not (eq (car variables)
		  'unconstrained))
	 nil)
	(t (apply (function unconstrained-p)
		  (cdr variables)))))

;
; 	OPERATIONEN FUER INFO ASSOCIATION
;


;	info association:	
;
;		( <variable> . <variable info> )
;

(defmacro make-info-assoc (var var-info)
  `(cons ,var ,var-info))

(defmacro get-net-var (info-assoc)
  `(car ,info-assoc))

(defmacro get-var-info (info-assoc)
  `(cdr ,info-assoc))


(defun get-var-info-constraints (info-assoc)

  ;;; ermittelt die Constraint-Ausdruecke, die der Variablen in
  ;;; info-assoc zugeordnet sind
  
  (if (null info-assoc)
      nil
      (var-info-constraints
	(get-var-info info-assoc))))


(defun get-var-info-values (info-assoc)

  ;;; ermittelt die Wertemenge, die der Variablen in info-assoc
  ;;; zugeordnet sind
  
  (if (null info-assoc)
      (baberror (getentry net-spec-access constraint-io-table))
      (var-info-values
	(get-var-info info-assoc))))


(defun add-var-info-values (info-assoc value-spec)

  ;;; ergaenzt die Wertemenge der Variablen in info-assoc
  ;;; durch value-spec (Schnittbildung)

  (if info-assoc
      (replace-var-info-values
	info-assoc
	(extended-intersection
	  (var-info-values
	    (get-var-info info-assoc))
	  value-spec))))


(defun replace-var-info-values (info-assoc value-spec)

  ;;; ersetzt die Wertemenge der Variablen in info-assoc
  ;;; durch value-spec (Seiteneffekt !)

  (if info-assoc
      (setf (var-info-values
	      (get-var-info info-assoc))
	    value-spec)))


(defun freeze-var-info-values (info-assoc)

  ;;; speichert die aktuelle Wertemenge als
  ;;; Init-Value

  (if info-assoc
      (setf (var-info-init-values
	      (get-var-info info-assoc))
	    (var-info-values
	      (get-var-info info-assoc)))))


(defun init-var-info-values (info-assoc)

  ;;; initialisiert die Values-Komponente mit dem
  ;;; Default-Wert

  (if info-assoc
      (setf (var-info-values
	      (get-var-info info-assoc))
	    (var-info-init-values
	      (get-var-info info-assoc)))))


(defun reset-var-info-values (info-assoc)
  
  ;;; setzt Defaultwert auf 'unconstrained
  
  (if info-assoc
      (setf (var-info-init-values
	      (get-var-info info-assoc))
	    'unconstrained)))


;;; eof

