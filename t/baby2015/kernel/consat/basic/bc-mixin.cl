;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-


(in-package "BABYLON")

;
;	CONSTRAINT-MIXIN
;

(defun choose-special-constraint (constraint-liste)  
  
  "bittet den Benutzer, eines der Constraints in Constraint-Liste auszuwaehlen."
  
  (constraint-input-test
    (send-kb :choose-from-menu
	     (append
	       (getentry do-nothing-items constraint-io-table)
	       (mapcar (function
			 (lambda (constraint-assoc)
			   (list (get-name-of-c-assoc constraint-assoc)
				 :value constraint-assoc)))
		       constraint-liste))
	     (getentry choose-name constraint-io-table))))


(defun constraint-input-test (expr)
  
  "Abbruch, falls expr gleich nil ist"
  
  (if (null expr)
      (throw 'no-select nil)
      expr))


(defun transform-constraint-type (constraint-type)

  (case constraint-type
    (primitive :constraints)
    (compound :constraint-nets)))


(defun read-expr-from-window (text)
  (let ((input (send-kb :prompt-for-input text)))
    (declare (simple-string input))
    (if (= (length input) 0)
      (read-expr-from-window text)
      (read-from-string input))))


;;;

(def$flavor basic-constraint-mixin
	(constraint-processor)
	(constraint-base)
  
  :settable-instance-variables
  (:required-instance-variables procs kb-name)
  (:documentation "Anteil des Constraint-Systems am Metaprozessor")
  )  


(def$method (basic-constraint-mixin :after :init) (&rest plist)
  
  "wird nach dem Erzeugen eines Metaprozessors aufgerufen:
   erzeugt einen zugehoerigen Constraint-Prozessor
   und kettet ihn an den Metaprozessor"
  
  (declare (ignore plist))
  ($send self :generate-constraint-processor)
  (setf procs (cons constraint-processor procs)))


(def$method (basic-constraint-mixin :generate-constraint-processor) ()
	   
  "erzeugt einen Constraint-Prozessor"

  (setf constraint-processor
	(make-$instance 'basic-constraint-processor
			:meta-processor self)))


(def$method (basic-constraint-mixin :set-up-constraint-cmds) ()
  (let ((table (get 'cmd-table ($send self :language))))
    (when (and table ($send self :operation-handled-p :add-operations))
      ($send self :add-sub-operations
	     :top (gethash 'consat table)
	     :consat (gethash 'consat-commands table)))))



(def$method (basic-constraint-mixin :after :new&delete) (&rest ignore)

  "runterreichen der definierten Constraints"

  (declare (ignore ignore))
  ($send constraint-processor :set-constraints constraints)
  ($send constraint-processor :set-constraint-nets constraint-nets))

;
;	FESTLEGUNG DER KONSTRUKTE
;
;
;
;	- Constraint Aktivierung
;
;		(activate <constraint-name> ) |
;		(activate <constraint-name> with
;			<local-var 1> = <value-spec 1>
;				...
;			<local-var n> = <value-spec n> )
;
;	- Konsistenztest
;
;		(consistent-p <constraint-name>) |
;		(consistent-p <constraint-name> with
;			<local-var 1> = <value-spec 1>
;				...
;			<local-var n> = <value-spec n>)


(defmacro constraint-type (expression)
  
   "ermittelt den Typ von expression und fuehrt einen Syntaxtest durch"
  
  `(if (atom ,expression) nil
       (case (car ,expression)
	 (satisfy
	  'satisfy-request)
	 (satisfied-p
	  'satisfied-p-request)
	 (t nil))))


(assign-typefkt 'constraint-type 'basic-constraint-mixin)


(defrequest satisfy-request
	    :lisp :eval-satisfy
	    :recall :eval-satisfy)


(defrequest satisfied-p-request
	    :lisp    	:eval-satisfied-p
	    :recall	:eval-satisfied-p
	    :prolog	:eval-satisfied-p)


(def$method (basic-constraint-mixin :eval-satisfy) (expression &rest ignore)
  
  (declare (ignore ignore))
  ($send constraint-processor
	 :satisfy
	 (remove-request-key expression)))


(def$method (basic-constraint-mixin :eval-satisfied-p) (expression &rest ignore)
  
  (declare (ignore ignore))
  ($send constraint-processor
	:satisfied-p
	(remove-request-key expression)))


(defun remove-request-key (expression)
  (rest expression))


;
;	LISP-SCHNITTSTELLE
;


(defmacro satisfy (&rest expression)
  
  "ermoeglicht Verwendung von Satisfy-Konstrukten in Lisp-Ausdruecken"
  
  `(send-kb :eval
	    '(satisfy . ,expression)
	    :lisp
	    'lisp-processor))


(defmacro satisfied-p (&rest expression)
  
  "ermoeglicht Verwendung von Satisfied-p-Konstrukten in Lisp-Ausdruecken"
  
  `(send-kb :eval
	    '(satisfied-p . ,expression)
	    :lisp
	    'lisp-processor))

;
;	EXTERNE SYNTAX
;


(defun external-value-ass-p (expression)

  "ueberprueft die Syntax der externen Darstellung einer Variablenbelegung"

  (cond ((null expression) t)
	((and (rest (rest expression))
	      (atom (first expression))
	      (eq (second expression) '=)
	      (external-value-ass-p (rest (rest (rest expression))))))
	(t nil)))


(defun get-list-of-choices (expression value-ass)

  " Eingabe:  eine Zuordnung von Prolog-Variablen zu
  	      lokalen Constraint-Variablen,
  	      eine Wertebelegung der lokalen Variablen
  
    Ausgabe:  eine Liste von Wertemengen , so dass gilt:
  	      das i-te Element ist die Wertemenge der mit dem
  	      i-ten Prolog-Term assoziierten Variable"

  (cond ((null expression) nil)
	((and (rest (rest expression))
	      (eq (second expression) '=))
	 (let ((value-assoc (assoc (third expression)
				   value-ass)))
	   (if (null value-assoc)
	       (baberror (getentry wrong-prolog-term
				 constraint-io-table)
		       (third expression)
		       value-ass)
	       (cons (get-value-spec value-assoc)
		     (get-list-of-choices
		       (rest (rest (rest expression)))
		       value-ass)))))
	(t (baberror (getentry candidate-expr-error
			     constraint-io-table)
		   expression))))


;
;	CONSTRAINT ANZEIGEN
;


(defun display-constraint (&rest ignore)
  (declare (ignore ignore))
  (catch 'no-select
    (send-kb :display)))


(def$method (basic-constraint-mixin :display) ()
  
  "ermoeglicht die Auswahl eines Constraints, das daraufhin ausgegeben wird"

  (let ((c-assoc ($send self :choose-constraint)))

    ($send (get-object-of-c-assoc c-assoc)
	  :print
	  (get-name-of-c-assoc c-assoc)
	  ($send self :dialog-stream))))

;
; 	CONSTRAINT LESEN
;


;;;	der folgende Programmteil zur Eingabe von Constraints ist
;;;	sehr unkomfortabel und faengt kaum Eingabefehler ab
;;;
;;;	VORSICHT bei BENUTZUNG !!!!

(defun read-constraint (&rest ignore)
  (declare (ignore ignore))
  (catch 'no-select
    (send-kb :read)))


(defun choose-element-type ()
  (send-kb :choose-from-menu
    (getentry rel-elem-items constraint-io-table)
    (getentry choose-rel-elem constraint-io-table)))


(defun choose-relation ()
  (let ((elem-type (choose-element-type)))
    (case elem-type
      (tuple
       (cons (list ':tuple
		   (read-expr-from-window
                    (getentry choose-tuple constraint-io-table)))
	     (choose-relation)))
      (pattern
       (cons (list ':pattern
		   (read-expr-from-window
                    (getentry choose-pattern constraint-io-table)))
	     (choose-relation)))
      (conditional-pattern
       (cons (list ':pattern
		   (read-expr-from-window
                    (getentry choose-pattern constraint-io-table))
		   'if
		   (read-expr-from-window
                    (getentry choose-pattern-condition
                              constraint-io-table)))
	     (choose-relation)))
      (exit nil))))


(def$method (basic-constraint-mixin :read) ()
  
  "liest die Komponenten einer Constraint-Definition aus Pop-Up-Menus"
  
  (let ((c-type ($send self :choose-from-menu
		       (getentry choose-type-items constraint-io-table)
		       (getentry choose-type constraint-io-table))))    
    (if (member c-type '(primitive compound))
	($send self
	      :new&delete
	      c-type
	      (read-expr-from-window
		(getentry choose-name constraint-io-table))
	      (read-expr-from-window
		(case c-type
		  (primitive (getentry choose-variables
				       constraint-io-table))
		  (compound (getentry choose-interface
				      constraint-io-table))))
	      
	      (case c-type
		(primitive (choose-relation))
		(compound (read-expr-from-window
			    (getentry choose-expressions
				      constraint-io-table))))			  
	      (read-expr-from-window
		(getentry choose-condition constraint-io-table)
		)))))


;	CONSTRAINT AKTIVIEREN
;


(defun satisfy-constraint-locally ()
  (catch 'no-select
    (send-kb :activate-interactive
	     'local-consistency)))


(defun satisfy-constraint-globally ()
  (catch 'no-select
    (send-kb :activate-interactive
	     'global-consistency)))


(def$method (basic-constraint-mixin  :activate-interactive) (consistency-level)
  
  "ermoeglicht die Aktivierung eines Constraints"
  
  (let* ((c-assoc ($send self :choose-constraint))
	 (number-of-results (if (eq consistency-level
				    'global-consistency)
				(choose-number-of-results)))
	 (value-ass (choose-value-assignment
		      ($send (get-object-of-c-assoc c-assoc)
			     :interface))))
    
    ($send self :print-enter
	  (get-name-of-c-assoc c-assoc)
	  value-ass
	  consistency-level
	  number-of-results
	  ($send self :dialog-stream))
    ($send self :print-exit
	  (get-name-of-c-assoc c-assoc)
	  ($send  (get-object-of-c-assoc c-assoc)
		  :activate
		  value-ass
		  'initialize
		  consistency-level
		  number-of-results)
	  consistency-level
	  ($send self :dialog-stream))))


(defun choose-value-assignment (interface)

  "liest eine Wertebelegung der Interface-Variablen schrittweise ein"

  (mapcar (function
	    (lambda (variable)
	      (make-value-assoc
		variable
		(choose-value-spec variable))))
	  interface))


(defun choose-value-spec (variable)

  "liest eine Wertemenge fuer diese Variable ein"

  (convert-to-consat-value
    (read-expr-from-window (format nil "~S" variable))
    'no-eval))


(defun choose-number-of-results ()

  " liest die Anzahl der global konsistenten
    Loesungen ein, die berechnet werden sollen"

  (do ((input (read-expr-from-window
		(getentry choose-number-of-results
			  constraint-io-table))
	      (read-expr-from-window
		(getentry choose-number-of-results
			  constraint-io-table))))
      ((or (null input)
	   (numberp input)) input)))


(def$method (basic-constraint-mixin :print-enter)
	    (c-name value-ass consistency-level number-of-results
		    &optional (stream nil))
  
  (terpri stream)
  (princ "SATISFY  " stream)
  (princ c-name stream)
  (princ (case consistency-level
	   (local-consistency "  :LOCALLY  ")
	   (global-consistency "  :GLOBALLY  "))
	 stream)
  (if (not (null number-of-results))
      (princ number-of-results stream))
  (princ "  :WITH    " stream)
  (print-value-ass value-ass stream))


(def$method (basic-constraint-mixin :print-exit)
	    (c-name one-or-list-of-value-ass consistency-level
		    &optional (stream nil))
  
  (declare (ignore c-name))
  (terpri stream)
  (cond ((eq consistency-level 'local-consistency)
	 (print-result one-or-list-of-value-ass stream))
	((null one-or-list-of-value-ass)
	 (format stream "--> ~A"
		 (getentry no-solutions constraint-io-table))
	 (terpri stream))
	(t (mapc #'(lambda (value-ass)
		     (print-result value-ass stream))
		 one-or-list-of-value-ass))))


(defun print-result (value-ass stream)

  (princ "-->  " stream)
  (print-value-ass value-ass stream)
  (terpri stream))


(defun print-value-ass (value-ass stream)

  (mapc (function
	  (lambda (value-assoc)
	    (princ (get-var value-assoc) stream)
	    (princ " = " stream)
	    (princ (get-value-spec value-assoc) stream)
	    (princ "   " stream)))
	value-ass))


;
;	AUSWAHL VON CONSTRAINTS
;
;
;
;	Beachte:  Falls der Benutzer keine Wahl trifft (z.B. das
;		  Menu verlaesst oder "do nothing" anklickt,
;		  erfolgt ein Abbruch zu Label  'no-select
;


(def$method (basic-constraint-mixin :choose-constraint) ()
  
  "bittet den Beutzer, ein Constraint unter den in
   der aktuellen Wissensbasis definierten auszuwaehlen"
  
  (choose-special-constraint
    ($send self :choose-c-type)))


(def$method (basic-constraint-mixin :choose-c-type) ()
  
  "bittet den Benutzer, einen Constraint-Typ auszuwaehlen  
   Ergebnis:  Selektor fuer Constraint-Liste"
  
  (constraint-input-test
    ($send
      self
      :send-if-handles
      (constraint-input-test
	(transform-constraint-type
	  ($send self
		:choose-from-menu
		(append
		  (getentry do-nothing-items constraint-io-table)
		  (getentry choose-type-items constraint-io-table))
		(getentry choose-type constraint-io-table)))))))



;;; eof

