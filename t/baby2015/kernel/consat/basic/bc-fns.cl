;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

; 	MAKROS FUER ASSOZIATIONSLISTEN
;


;
;	value association:	
;
;		( <variable> . <value-specification> )
;
;       mit  	<value-specification> ::= list( <value> ) | unconstrained
;


(defmacro make-value-assoc (var value-spec)
  `(cons ,var ,value-spec))

(defmacro get-var (value-assoc)
  `(car ,value-assoc))

(defmacro get-value-spec (value-assoc)
  `(cdr ,value-assoc))


;
;	simple value association:
;
;		( <variable> <one-or-no-value> )
;
;	mit <one-or-nor-values> ::= <value> | unconstrained

(defmacro make-simple-val-assoc (var value)
  `(list ,var ,value))

(defmacro get-simple-value (simple-value-assoc)
  `(second ,simple-value-assoc))




(defstruct (var-info)
  (constraints nil)
  (values 'unconstrained)
  (init-values 'unconstrained))




;
;	variable association:	
;
;		( <local variable> . <global variable> )
;

(defmacro make-var-assoc (local global)
  `(cons ,local ,global))

(defmacro get-local-var (var-assoc)
  `(car ,var-assoc))

(defmacro get-global-var (var-assoc)
  `(cdr ,var-assoc))


;
;	constraint-expression:
;
;		( <constraint-name> . list( <global variable> ) )
;

(defmacro make-c-expr (c-name var-alist)
  `(cons ,c-name ,var-alist))

(defmacro get-constr-name (c-expr)
  `(car ,c-expr))

(defmacro get-parameters (c-expr)
  `(cdr ,c-expr))


;
;	trace-element:  	Ergebnis einer Constraint-Aktivierung
;
;		( <constraint-expression>  list( <value association> ) )
;

(defmacro make-trace-elem (c-expr value-alist)
  `(list ,c-expr ,value-alist))

(defmacro get-trace-constr (trace-elem)
  `(first ,trace-elem))

(defmacro get-trace-value-ass (trace-elem)
  `(second ,trace-elem))



;
;	constraint-association:		Zuordnung Name zu Constraint
;
;		( <constraint-name> . <beliebiges Constraint> )
;

(defmacro make-constraint-assoc (name constraint)
  `(cons ,name ,constraint))

(defmacro get-name-of-c-assoc (constraint-assoc)
  `(car ,constraint-assoc))

(defmacro get-object-of-c-assoc (constraint-assoc)
  `(cdr ,constraint-assoc))



;
;	LISTEN MIT WERT-ZUWEISUNGEN
;
;
;	Die folgenden Funktionen fuehren Operationen auf
;	Assoziationslisten durch, die Wertzuweisungen an
;	Variablen beschreiben.
;
;	Dabei treten folgende Typen von Listen auf:
;
;	<simple-value-assignment>	::= list( ( <variable> <one-or-no-value> ) )
;	<one-or-no-value>		::= unconstrained | <value> 
;	<multiple-value-assignment>	::= list( <variable> . <value-specification> )
;	<value-specification>		::= unconstrained | list( <value> )
;	


(defun select-some-value-ass (list-of-value-ass number-of-results
			      &optional (new-list-of-value-ass nil))
  
  ;;; falls number-of-results nicht nil ist, werden die ersten
  ;;; number-of-results verschiedenen Wertebelegungen aus
  ;;; list-of-value-ass zurueckgeliefert
  
  (cond ((null list-of-value-ass)
	 (reverse new-list-of-value-ass))
	((enough-results number-of-results)
	 (reverse new-list-of-value-ass))
	((member (first list-of-value-ass)
		 new-list-of-value-ass
		 :test 'equal)
	 (select-some-value-ass (rest list-of-value-ass)
				number-of-results
				new-list-of-value-ass))
	(t (select-some-value-ass (rest list-of-value-ass)
				  (decr-number-of-results number-of-results)
				  (cons (first list-of-value-ass)
					new-list-of-value-ass)))))


(defun enough-results (number-of-results)
  (and (not (null number-of-results))
       (<= number-of-results 0)))


(defun decr-number-of-results (number-of-results)
  (if (null number-of-results)
      nil
      (1- number-of-results)))


(defun convert-simple-to-multiple (simple-val-ass)

  ;;; erzeugt aus einer simplen Wertzuweisung eine multiple,
  ;;; in dem (var unconstrained) durch (var . unconstrained) ersetzt wird

  (mapcar (function
	    (lambda (simple-val-assoc)
	      (if (eq (get-simple-value simple-val-assoc)
		      'unconstrained)
		  (make-value-assoc
		    (get-var simple-val-assoc)
		    'unconstrained)
		  simple-val-assoc)))
	  simple-val-ass))


(defun empty-alist (variables)

  ;;; erzeugt eine Wertzuweisung fuer die angegebenen Variablen;
  ;;; jede Variable in variables erhaelt als wert die leere Menge

  (mapcar (function
	    (lambda (var)
	      (make-value-assoc var nil)))
	  variables))


(defun adjust-value-ass (variables value-ass)

  ;;; ermittelt eine Wertebelegung der Variablen in variables;
  ;;; falls value-ass der Variablen v eine Wertemenge zuordnet,
  ;;; wird diese als Wert fuer v gewaehlt;
  ;;; ansonsten wird mit unconstrained vorbesetzt

  (if (null variables)
      nil
      (cons (make-value-assoc
	      (first variables)
	      (let ((value-assoc (assoc (first variables)
					value-ass
					:test 'equal)))
		(if (null value-assoc)
		    'unconstrained
		    (get-value-spec value-assoc))))
	    (adjust-value-ass
	      (rest variables)
	      value-ass))))


(defun some-new-restrictions-p (val-ass1 val-ass2)
  
  ;;; t, falls es eine Variable gibt, deren Wertemenge in val-ass2 durch
  ;;;    die Wertemenge in val-ass1 staerker eingeschraenkt wird
  
  (if (null val-ass1) nil
      (let ((val-assoc2 (assoc (get-var (first val-ass1))
			       val-ass2
			       :test 'equal)))
	(if (and val-assoc2
		 (more-constrained-p
		   (get-value-spec (first val-ass1))
		   (get-value-spec val-assoc2)))
	    t
	    (some-new-restrictions-p
		(rest val-ass1)
		val-ass2)))))


;
;	VEREINIGUNG VON LISTEN MIT WERTZUWEISUNGEN
;


(defun combine-variable-alists (list-of-val-ass variables)

  ;;; Eingabe:	eine Liste multipler Wertzuweisungen
  ;;;
  ;;; Ausgabe:  eine multiple Wertzuweisung, die die Vereinigung
  ;;;		der eingegebenen Wertzuweisungen dastellt

  (if (null list-of-val-ass)
      (empty-alist variables)
      (combine-two-alists
	(first list-of-val-ass)
	(combine-variable-alists
	  (rest list-of-val-ass)
	  variables))))


(defun combine-two-alists (val-ass1 val-ass2)

  ;;; Eingabe: 	Zwei multiple Wertzuweisungen val-ass1, val-ass2
  ;;;
  ;;; Ausgabe: 	Neues multiple-value-assignment, die eine Vereinigung
  ;;;		von val-ass1 und val-ass2 darstellt;
  ;;;		dabei werden Wertemengen doppelt auftretender Variablen
  ;;; 		zu einer vereinigt
  
  (cond ((null val-ass1) val-ass2)
	((null val-ass2) val-ass1)
	(t (mapcar (function
		     (lambda (variable)
		       (make-value-assoc
			 variable
			 (combine-values
			   (get-value-spec (assoc variable val-ass1
						  :test 'equal))
			   (get-value-spec (assoc variable val-ass2
						  :test 'equal))))))
		   (union-sets (mapcar (function
					 (lambda (val-assoc)
					   (get-var val-assoc)))
				       val-ass1)
			       (mapcar (function
					 (lambda (val-assoc)
					   (get-var val-assoc)))
				       val-ass2))))))


(defun combine-values (value-set1 value-set2)

  ;;; vereinigt zwei Wertemengen, wobei zu beachten ist,
  ;;; dass unconstrained eine allgemeine Obermenge darstellt

  (if (or (eq value-set1 'unconstrained)
	  (eq value-set2 'unconstrained))
      'unconstrained
      (union-sets value-set1 value-set2)))


(defun cons-if-not-nil (x y)
  (if (null x) y
      (cons x y)))


;
;	KREUZPRODUKT BILDEN
;


(defun split-variable-alist (val-ass)

  ;;; Eingabe:	multiple-value-assignment val-ass
  ;;;
  ;;; Ausgabe:	bildet die Menge aller eindeutigen (bis auf unconstrained)
  ;;;		Wertezuordnungen gemaess val-ass
  ;;;		(es wird im Prinzip das Kreuzprodukt der einzelnen
  ;;;		 Wertemengen gebildet);
  ;;;		das Ergebnis ist eine Liste von value-assignments
  ;;;		

  (if (null val-ass)
      (list nil)
      (split-and-put-association
	(first val-ass)
	(split-variable-alist
	  (rest val-ass)))))


(defun split-and-put-association (value-assoc list-of-val-ass)

  ;;; Eingabe:	Wert/Variable-Assoziation (v . (w1 w2 ... wn))
  ;;;		Liste von value-assignments (l1 l2 ... lm),
  ;;;		in denen v nicht vorkommt
  ;;;
  ;;; Ausgabe:	Liste von value-assignments
  ;;;
  ;;;		(((v.w1).l1) ((v.w1).l2) ... ((v.w1).lm)
  ;;;		 ...
  ;;;		 ((v.wn).l1) ((v.wn).l2) ... ((v.wn).lm))

  (cond ((eq (get-value-spec value-assoc)
	     'unconstrained)
	 (mapcar (function
		   (lambda (simple-val-ass)
		     (cons (make-simple-val-assoc
			     (get-var value-assoc)
			     'unconstrained)
			   simple-val-ass)))
		 list-of-val-ass))
	
	((member 'unconstrained
		 (get-value-spec value-assoc))
	 (baberror (getentry wrong-unconstrained constraint-io-table)
		 value-assoc))

	(t (mapcan (function
		     (lambda (value)
		       (mapcar (function
				 (lambda (simple-val-ass)
				   (cons (make-simple-val-assoc
					   (get-var value-assoc)
					   value)
					 simple-val-ass)))
			       list-of-val-ass)))
		   (get-value-spec value-assoc)))))



;
;	BEHANDLUNG VON WERTEMENGEN
;


(defun more-constrained-p (value-spec1 value-spec2)

  ;;; t, falls value-spec2 durch value-spec1 staerker eingeschraenkt wird

  (cond ((eq value-spec1 'unconstrained) nil)
	((eq value-spec2 'unconstrained) t)
	((not (difference-empty-p value-spec2 value-spec1)))))


(defun new-association (variable value)

  ;;; erzeugt neue Variable-Wert-Assoziation unter besonderer
  ;;; Beachtung von 'unconstrained

  (make-value-assoc
    variable
    (if (eq value 'unconstrained)
	value
	(list value))))

				
(defun compatible-value-p (value variable simple-val-ass)
  
  ;;; t, falls value Wert von variable in val-ass ist
  
  (let ((ass-value (get-simple-value
		     (assoc variable simple-val-ass
			    :test 'equal))))
    (or (equal ass-value 'unconstrained)
	(equal ass-value value))))



;
;	MENGEN
;


;(defun remove-duplicates (liste)
;
;  ;;; entfernt alle Duplikate
;  
;  (cond ((null liste) nil)
;	((member (car liste) (cdr liste) :test 'equal)
;	 (remove-duplicates (cdr liste)))
;	(t (cons (car liste)
;		 (remove-duplicates (cdr liste))))))


(defun union-sets (set1 set2)

  ;;; vereinigt zwei Mengen mit beliebigen Elementen
  ;;; (Zetalisp-union benutzt nur eq zum Elementvergleich)

  (union set1 set2 :test 'equal))


(defun intersect-sets (set1 set2)

  ;;; bildet die Schnittmenge zweier Mengen mit
  ;;; beliebigen Elementen (intersection benutzt dagegen eq !)

  (intersection set1 set2 :test 'equal))


(defun difference-empty-p (set1 set2)

  ;;; berechnet set1 \ set2 = 0

  (null (set-difference set1 set2 :test 'equal)))


;
;	MENGENOPERATIONEN UNTER BERUECKSICHTIGUNG VON UNCONSTRAINED
;


(defun extended-intersection (value-spec1 value-spec2)

  ;;; bildet die Schnittmenge von value-spec1 und value-spec2
  ;;; unter Beruecksichtigung von 'unconstrained

  (cond ((eq value-spec1 'unconstrained) value-spec2)
	((eq value-spec2 'unconstrained) value-spec1)
	(t (intersect-sets value-spec1 value-spec2))))


(defun extended-member (value value-spec)

  (or (eq value-spec 'unconstrained)
      (member value value-spec :test 'equal)))


;
;	macros fuer Relationen-Element
;


(defmacro get-keyword (rel-elem)
  `(first ,rel-elem))

(defmacro get-tupel (rel-elem)
  `(second ,rel-elem))

(defmacro get-expressions (rel-elem)
  `(second ,rel-elem))

(defmacro get-condition (rel-elem)
  `(fourth ,rel-elem))


(defun send-constraint-processor (selector &rest args)
  "sendet Nachricht an aktuellen Constraint-Processor"
  (lexpr-$send (send-kb :constraint-processor) selector args))

(defun get-constraint (constraint-name)
  " ermittelt das zugehoerige primitive oder
      zusammengesetzte Constraint 
      liefert Fehlermeldung, falls Constraint nicht definiert ist"
  (let ((constraint (send-constraint-processor :get constraint-name)))
    (if (null constraint)
      (baberror (getentry unknown-constraint constraint-io-table)
		constraint-name)
      constraint)))


;;; eof

