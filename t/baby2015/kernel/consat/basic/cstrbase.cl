;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-


(in-package "BABYLON")


;
;		Verwaltung von Constraints
;

;  3.6. 1987  Anpassung an GENERA 7; R. Lopatta
; 
     
(def$flavor constraint-base
	((constraints nil)
	 (constraint-nets nil))
	()
  
  :settable-instance-variables
  :initable-instance-variables)


;
;      	ZUGRIFF AUF CONSTRAINTS   
;


(def$method (constraint-base :get) (c-name)
  
  " ermittelt das primitive oder zusammengesetzte Constraint mit
      dem angegebenen Namen
      (Beachte: ein Netz und ein primitives Constraint duerfen nicht
       		den gleichen Namen besitzen)"
  
  (let ((primitive-c-assoc (assoc c-name constraints))
	(compound-c-assoc (assoc c-name constraint-nets)))
    
    (cond ((get-object-of-c-assoc primitive-c-assoc))
	  ((get-object-of-c-assoc compound-c-assoc))
	  (t nil))))


(def$method (constraint-base :put-primitive) (c-name c-primitive)

    " traegt ein neues primitives Constraint ein"

  ($send self
	:set-constraints
	(cons (make-constraint-assoc
		c-name
		c-primitive)
	      constraints)))


(def$method (constraint-base :delete) (c-name)

    " Loeschen des angegebenen Constraints"

  (let ((primitive-c-assoc (assoc c-name constraints))
	(compound-c-assoc (assoc c-name constraint-nets)))
    
    (cond (primitive-c-assoc
	   ($send self :set-constraints
		 (remove primitive-c-assoc
			 constraints)))
	  (compound-c-assoc
	   ($send self :set-constraint-nets
		 (remove compound-c-assoc
			 constraint-nets))))))


(def$method (constraint-base :put-compound) (c-name c-net)

    " traegt ein neues Constraintnetz ein"

  ($send self
	:set-constraint-nets
	(cons (make-constraint-assoc
		c-name
		c-net)
	      constraint-nets)))


(defun print-constraint-list (constraint-list stream)

  "  	druckt alle Constraints in der Liste in wiedereinlesbarer
  	Form nach stream"

  (mapc (function
	  (lambda (c-assoc)
	    ($send (get-object-of-c-assoc c-assoc)
		  :print
		  (get-name-of-c-assoc c-assoc)
		  stream)))
	constraint-list))


;
;       AKTIVIERUNG VON CONSTRAINTS
;



(def$method (constraint-base :satisfied-p) (expression)
  
    " T, falls die Wertebelegung konsistent ist;
      NIL, sonst
     
      der Konsistenzlevel und die Anzahl der
      geforderten Ergebnisse werden beruecksichtigt"
  
  (catch 'error
    ($send self :activate-and-adapt-result
	  (get-constraint (get-constr-name expression))
	  (eval-value-ass (get-external-value-ass expression))
	  (determine-consistency-level expression)
	  (determine-number-of-results expression)
	  'boolean)))


(def$method (constraint-base :satisfy) (expression)

  " aktiviert den in expression angegebenen Constraint, wobei
    die Argumente in die interne Darstellung ueberfuehrt werden"

  (catch 'error
    ($send self :activate-and-adapt-result
	  (get-constraint (get-constr-name expression))
	  (eval-value-ass (get-external-value-ass expression))
	  (determine-consistency-level expression)
	  (determine-number-of-results expression)
	  'value-assignment)))


(def$method (constraint-base :activate-and-adapt-result)
	    (constraint multiple-value-ass consistency-level
			number-of-results result-type)
  
  " aktiviert den Constraint und passt das Ergebnis
      dem geforderten Typ an"
  
  (case result-type
    (value-assignment ($send constraint
			     :activate
			     multiple-value-ass
			     'initialize
			     consistency-level
			     number-of-results))
    (boolean (value-assignment-to-boolean-value
	       ($send constraint
		      :activate
		      multiple-value-ass
		      'initialize
		      consistency-level
		      number-of-results)
	       consistency-level
	       number-of-results))))


(defun value-assignment-to-boolean-value
       (value-assignment consistency-level number-of-results)
  
  " macht aus dem Ergebnis einer Constraint-Aktivierung
    einen boolschen Wert"

  (declare (list value-assignment))
  (case consistency-level
    (local-consistency (if (consistent-value-ass-p value-assignment)
                         t
                         nil))
    (global-consistency (if (null number-of-results)
                          (if (null value-assignment)
                            nil
                            t)
                          (if (= (length value-assignment)
                                 number-of-results)
                            t
                            nil)))))


(defun get-external-value-ass (expression)
  (rest (member ':with expression)))


(defun eval-value-ass (externel-value-ass)
  
  " berechnet die Wertebelegung der Constraint-Variablen
    aus der externen Darstellung der Wertebelegung"
  
  (if (null externel-value-ass) nil
      (cons (eval-first-value-ass externel-value-ass)
	    (eval-value-ass (rest (rest (rest  externel-value-ass)))))))


(defun eval-first-value-ass (external-value-ass)
  
  " berechnet die Wertebelegung der ersten Constraint-Variablen"
  
  (if (eq (second external-value-ass) '=)
      (make-value-assoc (first external-value-ass)
			(convert-to-consat-value (third external-value-ass)
						 'eval))
      (baberror (getentry invalid-external-value-ass constraint-io-table))))


(defun convert-to-consat-value
       (expression &optional (mode 'no-eval))
  
  " ueberfuehrt expression in eine Consat-Wertemenge"
  
  (cond ((eq expression 'unconstrained) 'unconstrained)
	((eq expression '-) 'unconstrained)
	((atom expression) (list expression))
	((eq (first expression) :one-of)
	 (rest expression))
	(t
	 (case mode
	   (eval
	    (value-spec-test (evaluate-funcall expression)))
	   (no-eval
	    (value-spec-test expression))))))


(defun value-spec-test (expression)
  
   " falls expression keine Consat-Wertemenge
     ist, erfolgt Fehlermeldung"
  
  (if (is-value-spec expression)
      expression
      (baberror (getentry value-spec-error
			constraint-io-table)
	      expression)))


(defun is-value-spec (expr)
  
  " ueberprueft, ob Consat-Wertemenge vorliegt
    (laesst u.a. keine Dotted-Pairs zu)"
  
  (or (eq expr 'unconstrained)
      (null expr)
      (and (listp expr)
	   (is-value-spec (rest expr)))))


(defun determine-consistency-level (expression)
  
  (cond ((or (null (rest expression))		; Default-Fall
             (eq (second expression) ':with))
	 'local-consistency)	
	((and (eq (second expression) ':locally)  ; lokale Konsistenz
	      (or (null (rest (rest expression)))
		  (eq (third expression) ':with)))
	 'local-consistency)
	
	((and (eq (second expression) ':globally)  ; globale Konsistenz
	      (or (null (rest (rest expression)))
		  (null (rest (rest (rest expression))))
		  (eq (third expression) ':with)
		  (eq (fourth expression) ':with)))
	 'global-consistency)
	
	(t (baberror (getentry invalid-consistency-level
			     constraint-io-table)))))


(defun determine-number-of-results (expression)
  
  (if (and (eq (second expression) :globally)
	   (not (null (rest (rest expression))))
	   (not (eq (third expression) :with)))
      (evaluate-funcall (third expression))))



;
;	DEFINITION VON CONSTRAINTS
;


(def$method (constraint-base :new&delete) (c-type c-name c-variables c-body
						  &optional (c-condition t))
  
  " Ueberschreiben der alten Definition"
  
  ($send self :delete c-name)
  ($send self
	 (case c-type
	   (primitive :new-primitive)
	   (compound  :new-compound))	
	 c-name
	 c-variables
	 c-body
	 c-condition))


(def$method (constraint-base :new-primitive) (c-name c-variables c-relation
						     &optional (c-condition t))
  
  " Definition eines primitiven Constraints"
  
  (catch 'error
    (if ($send self :get c-name)
	nil
	(let ((new-constraint
		(make-$instance 'constraint)))
	  ($send self
		:put-primitive
		c-name new-constraint)
	  ($send new-constraint
		:set-interface
		c-variables)
	  ($send new-constraint
		:set-relation
		c-relation)
	  ($send new-constraint
		:set-condition
		(compile-condition c-condition c-relation))
	  ($send new-constraint
		:set-compiled-condition-flag
		(abbreviated-condition c-condition))
	  t))))


(def$method (constraint-base :new-compound) (c-name interface c-expressions
                                                    &rest ignore)
  
  " Definition eines Constraint-Netz"
  (declare (ignore ignore))
  (catch 'error
    (if ($send self :get c-name)
      nil
      (let ((new-net (make-$instance 'constraint-net)))
        ($send self :put-compound c-name new-net)
        ($send new-net :set-interface interface)
        ($send new-net :set-net-spec (create-net-spec c-expressions))
        t))))


;
;	KONSTRUKTOR
;

  
  ;;; benutzerfreundliches Defconstraint
  ;;;
  ;;; Syntax:
  ;;; 	<constraint-def>	::= <primitve constraint> |
  ;;;			            <compound constraint>
  ;;;   <primitive constraint>  ::= (DEFCONSTRAINT <def-name> . list( p-def-elem> ) )
  ;;;   <compound constraint>	::= (DEFCONSTRAINT <def-name> . list( c-def-elem> ) )
  ;;;
  ;;; 	<p-def-elem>		::= (:RELATION . <relation> ) |
  ;;;				    (:INTERFACE .  list( <variable> ) ) |
  ;;;				    (:CONDITION <activation-condition> ) | <def-elem>
  ;;;   <c-def-elem>		::= (:CONSTRAINT-EXPRESSIONS .
  ;;;					list( <constraint-expressions> ) ) |
  ;;;				    (:INTERFACE . list( <variable> )
  ;;;   <def-elem>		::= (:TYPE <constraint-typ> ) 
  ;;;   <def-name>		::= symbol
  

(defmacro defconstraint (def-name &rest def-body)
  (catch 'error
    (case (get-def-typ def-body)
      (primitive
       `(send-kb
	  :new&delete
	  'primitive
	  ',def-name
	  ',(get-def-interface def-body)
	  ',(get-def-relation def-body)
	  ',(get-def-condition def-body)))
      (compound
       `(send-kb
	  :new&delete
	  'compound
	  ',def-name
	  ',(get-def-interface def-body)
	  ',(get-def-expressions def-body))))))



;
;	FUNKTIONEN FUER CONSTRAINT-DEFINITION
;

(defun get-def-typ (def-body)
  
  " sucht in def-body einen Ausdruck der Form (:type <constraint-typ>)
 
   	mit  <constraint-typ> ::= primitive | compound"
  
  (let ((typ-pair (assoc ':type def-body)))
    (cond ((null typ-pair)
	   (baberror (getentry no-type constraint-io-table)))
	  ((not (member (second typ-pair)
		      '(compound primitive)))
	   (baberror (getentry wrong-type constraint-io-table)
		   (second typ-pair)))
	  (t (second typ-pair)))))




(defun get-def-interface (def-body)

  " sucht in def-body einen Ausdruck der Form
   (:interface <variable-list>)"

  (let ((var-pair (assoc ':interface def-body)))
    (if (null var-pair)
	(baberror (getentry no-interface constraint-io-table))
	(cdr var-pair))))


(defun get-def-relation (def-body)

  " sucht in def-body einen Ausdruck der Form (:relation <relation>)
    und ueberprueft die Syntax von <relation>"

  (let ((rel-pair (assoc ':relation def-body)))
    (cond ((null rel-pair)
	   (baberror (getentry no-relation constraint-io-table)))
	  ((not (parse-relation (cdr rel-pair)))
	   (baberror (getentry wrong-relation constraint-io-table))
	   (cdr rel-pair))
	  (t (cdr rel-pair)))))


(defun get-def-condition (def-body)
  
  " sucht in def-body einen Ausdruck der Form
    (:condition <activation-condition>)"
  
  (let ((cond-pair (assoc ':condition def-body)))
    (cond ((null cond-pair) t)
          ((/= (length cond-pair) 2)
           (baberror (getentry wrong-condition constraint-io-table)
                     cond-pair))
          (t (second cond-pair)))))


(defun get-def-expressions (def-body)
  
  " sucht in def-body einen Ausdruck der Form
    (:constraint-expressions . list( <constraint-expression> )"
  
  (let ((expr-pair (assoc ':constraint-expressions def-body)))
    (if (null expr-pair)
      (baberror (getentry no-expressions constraint-io-table))
      (cdr expr-pair))))


(defun compile-condition (condition relation)

  " falls :OR als condition angegeben ist, wird die
    Disjunktion der :IF's genommen"

  (if (abbreviated-condition condition)
      (cons 'or (select-local-conditions relation))
      condition))


(defun abbreviated-condition (condition)

  " T, falls condition gleich :OR ist"

  (if (eql condition :or) t))


(defun select-local-conditions (relation)

  " selektiert die lokalen Bedingungen (IF's)"

  (cond ((null relation) nil)
	((has-condition-p (first relation))
         (cons (get-local-condition (first relation))
	       (select-local-conditions (rest relation))))
	(t (select-local-conditions (rest relation)))))


(defun has-condition-p (relation-element)
  (member :if relation-element))


(defun get-local-condition (relation-element)
  (second (member :if relation-element)))


;
;	SYNTAXTEST FUER <RELATION>
;
;
;
;
;	Eine Constraint-Relation ist folgendermassen aufgebaut:
;
;	<relation> 		::= list( <relation-element> )
;	<relation-element>	::= <tuple> | <patttern>
;	<tuple>			::= ( :tuple list( <value> ) )
;	<pattern> 		::= ( :pattern list( <lisp-expr> ) ) |
;				    ( :pattern list( <lisp-expr> )
;				      :if <lisp-expr> )


(defun parse-relation (relation)
  (cond ((null relation) t)
	((atom relation) nil)
	((and (parse-rel-elem (first relation))
	      (parse-relation (rest relation))))))


(defun parse-rel-elem (rel-elem)
  (if (atom rel-elem)
      nil
      (case  (get-keyword rel-elem)
	(:tuple (and (= (length rel-elem) 2)
		     (is-liste (get-tupel rel-elem))))
	(:pattern (and (if (= (length rel-elem) 4)
			   (eq (third rel-elem) ':if)
			   (= (length rel-elem) 2))
		       (is-liste (get-expressions
				   rel-elem))))
	(otherwise nil))))


(defun is-liste (liste)
  (cond ((null liste) t)
	((atom liste) nil)
	((is-liste (cdr liste)))))


;;; eof

