;;;  -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base:10 -*-

(in-package "BABYLON")

;;;     TABELLE FUER CONSTRAINT-MELDUNGEN (english)
;;;

 

(defbabylon-table constraint-io-table english :size 30)


;
;	HAUPTKOMMANDOMENU
;


(defbabylon-entry main-constraint-item constraint-io-table english
	  '("Constraint Operations"
	    :funcall constraint-operations
	    #+:LISPM :font #+:LISPM fonts:hl12b
	    :documentation "a menue of constraint operations"))


(defbabylon-entry read-constraint-item constraint-io-table english
	  '((" "
	     :no-select nil)
	    ("Define Constraint"
	     :funcall read-constraint
	     :documentation "defines a new constraint")
	    ("Display Constraint"
	     :funcall display-constraint
	     :documentation "displays description of a defined constraint")
	    ("Satisfy Constraint Locally"
	     :funcall satisfy-constraint-locally
	     :documentation "computes the maximal locally consistent solution")
	    ("Satisfy Constraint Globally"
	     :funcall satisfy-constraint-globally
	     :documentation "computes globally consistent solutions")
	    ("Trace Mode"
	     :funcall trace-constraints
	     :documentation "choose trace mode")
	    (" "
	     :no-select nil)))


(defbabylon-entry exit constraint-io-table english
	  '("exit"
	    :value nil))


(defbabylon-entry number-of-primitives constraint-io-table english
	  "- Number of PRIMITIVE CONSTRAINTS:    ~A")

(defbabylon-entry number-of-nets constraint-io-table english
	  "- Number of CONSTRAINT NETS:          ~A")

(defbabylon-entry number-of-restrictions constraint-io-table english
	  "- Number of RESTRICITON NETS:         ~A")

;
;	EINTRAEGE FUER INTERAKTIVEN MODUS
;


(defbabylon-entry do-nothing-items constraint-io-table english
	  '(("do nothing"
	     :value nil
	     :documentation "abort command")
	    (" "
	     :no-select nil)))


(defbabylon-entry choose-name constraint-io-table english
	  "enter constraint name")


(defbabylon-entry choose-type constraint-io-table english
	  "choose type")

(defbabylon-entry choose-type-items constraint-io-table english
	  '(("primitive"
	     :value primitive
	     :documentation "choose primitive constraint")
	    ("compound"
	     :value compound
	     :documentation "choose compound constraint")))


(defbabylon-entry choose-trace-modes constraint-io-table english
	  "set trace mode")

(defbabylon-entry trace-on-item constraint-io-table english
	  '((trace-on "Trace On")))

(defbabylon-entry choose-variables constraint-io-table english
	  "enter list of variables")

(defbabylon-entry choose-interface constraint-io-table english
	  "enter list of interface variables")

(defbabylon-entry choose-relation constraint-io-table english
	  "enter constraint relation")

(defbabylon-entry choose-expressions constraint-io-table english
	  "enter list of constraint expressions")

(defbabylon-entry choose-condition constraint-io-table english
	  "enter activation condition")

(defbabylon-entry rel-elem-items constraint-io-table english
	  '(("tuple"
	     :value tuple
	     :documentation "enter a tuple of values")
	    ("pattern"
	     :value pattern
	     :documentation "enter a list of lisp expressions")
	    ("conditional-pattern"
	     :value conditional-pattern
	     :documentation "enter a pattern and a condition")
	    ("exit"
	     :value exit
	     :documentation "relation is complete")))

(defbabylon-entry choose-rel-elem constraint-io-table english
	  "choose type")

(defbabylon-entry choose-tuple constraint-io-table english
	  "enter a list of values")

(defbabylon-entry choose-pattern constraint-io-table english
	  "enter a list of expressions")

(defbabylon-entry choose-pattern-condition constraint-io-table english
	  "enter a conditional expression")

(defbabylon-entry read-value-ass constraint-io-table english
	  "enter variable assignment")

(defbabylon-entry choose-number-of-results constraint-io-table english
	  "enter number of solutions (or NIL for all solutions)")

(defbabylon-entry no-solutions constraint-io-table english
	  "NO GLOBALLY CONSISTENT SOLUTIONS")

(defbabylon-entry fail constraint-io-table english
	  "  FAIL:   no more values for variable  ~A")



;
;	FEHLERMELDUNGEN
;

(defbabylon-entry restriction-error constraint-io-table english
  "slot reference and restriction net are incompatible")

(defbabylon-entry net-spec-access constraint-io-table english
  "CONSAT SYSTEM ERROR: wrong net-specification access")

(defbabylon-entry no-name constraint-io-table english
  "no constraint name specified")

(defbabylon-entry no-type constraint-io-table english
  "no constraint type specified")

(defbabylon-entry wrong-type constraint-io-table english
  "~A is not a constraint type")

(defbabylon-entry no-variables constraint-io-table english
  "no variables specified")

(defbabylon-entry no-interface constraint-io-table english
  "no interface-variables specified")

(defbabylon-entry no-relation constraint-io-table english
  "no constraint-relation specified")

(defbabylon-entry wrong-relation constraint-io-table english
  "syntax error in ~A")

(defbabylon-entry wrong-condition constraint-io-table english
  "error in length of ~A")

(defbabylon-entry no-expressions constraint-io-table english
  "no constraint-expressions specified")

(defbabylon-entry wrong-unconstrained constraint-io-table english
  "wrong occurence of unconstrained in ~A")

(defbabylon-entry restore-error constraint-io-table english
  "CONSAT SYSTEM ERROR: cannot restore state while stack is empty")

(defbabylon-entry length-error constraint-io-table english
  "CONSAT SYSTEM ERROR: ~A and ~A differ in length")

(defbabylon-entry unknown-variable constraint-io-table english
  "CONSAT SYSTEM ERROR: variable ~A doesn't occur in ~A")

(defbabylon-entry unknown-constraint constraint-io-table english
  "~A is not a name of a defined constraint")

(defbabylon-entry wrong-prolog-term constraint-io-table english
  "error in candidate expression: ~A doesn't occurs in ~A")

(defbabylon-entry candidate-expr-error constraint-io-table english
  "syntax error in ~A")

(defbabylon-entry no-restrictions constraint-io-table english
	  "no restrictions specified in ~A")

(defbabylon-entry slot-description-error constraint-io-table english
	  "no slot references specified")

(defbabylon-entry value-spec-error constraint-io-table english
	  "~A is not a consat value specification")


;     HIER HOEREN ULI'S MELDUNGEN AUF UND HWG'S BEGINNEN


(defbabylon-entry invalid-external-value-ass constraint-io-table english
	  "syntax error in value assignment")


(defbabylon-entry invalid-consistency-level constraint-io-table english
	  "invalid consistency level")



(defbabylon-entry mark-explain-item  constraint-io-table english
    '(nil "constraints marked by # are traced"))

(defbabylon-entry toggle-trace-modes constraint-io-table english
    "select constraints to toggle trace mode")

