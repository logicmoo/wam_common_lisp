;;;  -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base:10 -*-

(in-package "BABYLON")

;;;     TABELLE FUER CONSTRAINT-MELDUNGEN (deutsch)   D. FUCHS 
;;;

 

(defbabylon-table constraint-io-table german :size 30)


;
;	HAUPTKOMMANDOMENU
;


(defbabylon-entry main-constraint-item constraint-io-table german
	  '("Constraint-Operationen"
	    :funcall constraint-operations
	    #+:LISPM :font #+:LISPM fonts:hl12b
	    :documentation "Ein Menue der Constraint-Operationen"))


(defbabylon-entry read-constraint-item constraint-io-table german
	  '((" "
	     :no-select nil)
	    ("Constraint-Definition"
	     :funcall read-constraint
	     :documentation "definiere ein neues Constraint")
	    ("Constraint-Beschreibung"
	     :funcall display-constraint
	     :documentation "zeigt die Beschreibung eines definierten Constraints an")
	    ("Lokale Constraint-Erfuellbarkeit"
	     :funcall satisfy-constraint-locally
	     :documentation "ermittelt die maximale, lokale, konsistente Loesung")
	    ("Globale Constraint-Erfuellbarkeit"
	     :funcall satisfy-constraint-globally
	     :documentation "ermittelt die globale, konsistente Loesung")
	    ("Trace-Modus"
	     :funcall trace-constraints
	     :documentation "einschalten des Trace-Modus")
	    (" "
	     :no-select nil)))


(defbabylon-entry exit constraint-io-table german
	  '("Beende"
	    :value nil))


(defbabylon-entry number-of-primitives constraint-io-table german
	  "- Anzahl der PRIMITIVEN CONSTRAINTS:    ~A")

(defbabylon-entry number-of-nets constraint-io-table german
	  "- Anzahl der CONSTRAINT-NETZE:          ~A")

(defbabylon-entry number-of-nets constraint-io-table german
	  "- Anzahl der RESTRICTION-NETZE:         ~A")

;
;	EINTRAEGE FUER INTERAKTIVEN MODUS
;


(defbabylon-entry do-nothing-items constraint-io-table german
	  '(("Ignoriere"
	     :value nil
	     :documentation "Abbruch")
	    (" "
	     :no-select nil)))


(defbabylon-entry choose-name constraint-io-table german
	  "Eingabe eines Constraint-Namen")


(defbabylon-entry choose-type constraint-io-table german
	  "Wahl des Typs")

(defbabylon-entry choose-type-items constraint-io-table german
	  '(("einfach"
	     :value primitive
	     :documentation "Wahl eines einfachen Constraints")
	    ("zusammengesetzt"
	     :value compound
	     :documentation "Wahl eines zusammengesetzten Constraints")))


(defbabylon-entry choose-trace-modes constraint-io-table german
	  "Setze Trace-Modus")

(defbabylon-entry trace-on-item constraint-io-table german
	  '((trace-on "Trace-Modus eingeschaltet")))

(defbabylon-entry choose-variables constraint-io-table german
	  "Eingabe der Variablenliste")

(defbabylon-entry choose-interface constraint-io-table german
	  "Eingabe der Liste von Interface-Variablen")

(defbabylon-entry choose-relation constraint-io-table german
	  "Eingabe der Constraint-Relation")

(defbabylon-entry choose-expressions constraint-io-table german
	  "Eingabe der Liste von Constraint-Ausdruecken")

(defbabylon-entry choose-condition constraint-io-table german
	  "Eingabe der Aktivierungsbedingung")

(defbabylon-entry rel-elem-items constraint-io-table german
	  '(("tuple"
	     :value tuple
	     :documentation "Eingabe der Tupel")
	    ("pattern"
	     :value pattern
	     :documentation "Eingabe der Liste von Lisp-Ausdruecken")
	    ("conditional-pattern"
	     :value conditional-pattern
	     :documentation "Eingabe eines 'pattern' und einer 'condition'")
	    ("Beende"
	     :value exit
	     :documentation "Die Relation ist vollstaendig")))

(defbabylon-entry choose-rel-elem constraint-io-table german
	  "Wahl des Typs")

(defbabylon-entry choose-tuple constraint-io-table german
	  "Eingabe einer Liste von Werten")

(defbabylon-entry choose-pattern constraint-io-table german
	  "Eingabe einer Liste von Ausdruecken")

(defbabylon-entry choose-pattern-condition constraint-io-table german
	  "Eingabe eines bedingten Ausdrucks")

(defbabylon-entry read-value-ass constraint-io-table german
	  "Eingabe einer Wertzuweisung an eine Variable")

(defbabylon-entry choose-number-of-results constraint-io-table german
	  "Angabe der Anzahl der Loesungen (ansonsten Null fuer alle Loesungen)")

(defbabylon-entry no-solutions constraint-io-table german
	  "KEINE GLOBALEN, KONSISTENTEN LOESUNGEN")

(defbabylon-entry Misserfolg constraint-io-table german
	  "  MISSERFOLG:   keine weiteren Variablenwerte  ~A")



;
;	FEHLERMELDUNGEN
;

(defbabylon-entry restriction-error constraint-io-table german
  "die Slot-Referenzen und das eingeschraenkte Netz stimmen nicht ueberein")

(defbabylon-entry net-spec-access constraint-io-table german
  "CONSAT SYSTEM FEHLER: Zugriff mittels falscher Netz-Spezifikation")

(defbabylon-entry no-name constraint-io-table german
  "fehlender Constraint-Name")

(defbabylon-entry no-type constraint-io-table german
  "fehlender Constraint-Typ")

(defbabylon-entry wrong-type constraint-io-table german
  "~A ist kein Constraint-Typ")

(defbabylon-entry no-variables constraint-io-table german
  "Variablen-Spezifikation fehlt")

(defbabylon-entry no-interface constraint-io-table german
  "Spezifikation der Interface-Variablen fehlt")

(defbabylon-entry no-relation constraint-io-table german
  "Spezifikation der Constraint-Relation fehlt")

(defbabylon-entry wrong-relation constraint-io-table german
  "Syntaxfehler in ~A")

(defbabylon-entry wrong-condition constraint-io-table german
  "falsche Wortlaenge bei ~A")

(defbabylon-entry no-expressions constraint-io-table german
  "Spezifikation der 'constraint-expressions' fehlt")

(defbabylon-entry wrong-unconstrained constraint-io-table german
  "falsches Auftreten von 'unconstrained' in  ~A")

(defbabylon-entry restore-error constraint-io-table german
  "CONSAT SYSTEM FEHLER :speichern des Zustands auf leeren Stack nicht erlaubt")

(defbabylon-entry length-error constraint-io-table german
  "CONSAT SYSTEM FEHLER: ~A und ~A sind von unterschiedlicher Laenge")

(defbabylon-entry unknown-variable constraint-io-table german
  "CONSAT SYSTEM FEHLER: Variable ~A nicht in ~A enthalten")

(defbabylon-entry unknown-constraint constraint-io-table german
  "~A ist kein Name eines definierten Constraints")

(defbabylon-entry wrong-prolog-term constraint-io-table german
  "Fehler im 'candidate'-Ausdruck : ~A gehoert nicht zu ~A")

(defbabylon-entry candidate-expr-error constraint-io-table german
  "Syntaxfehler in ~A")

(defbabylon-entry no-restrictions constraint-io-table german
	  "keine Einschraenkungen vorgesehen in~A")

(defbabylon-entry slot-description-error constraint-io-table german
	  "keine Slot-Referenzen vorgesehen")

(defbabylon-entry value-spec-error constraint-io-table german
	  "bei ~A handelt es sich um keine Consat-Wertspezifikation")


;     HIER HOEREN ULI'S MELDUNGEN AUF UND HWG'S BEGINNEN


(defbabylon-entry invalid-external-value-ass constraint-io-table german
	  "Syntaxfehler in der Wertzuweisung")


(defbabylon-entry invalid-consistency-level constraint-io-table german
	  "ungueltiger 'consistency level'")


(defbabylon-entry mark-explain-item  constraint-io-table german 
    '(nil "Mit # markierte Constraints werden protokolliert"))


(defbabylon-entry toggle-trace-modes constraint-io-table german
    "Waehle Constraints deren Trace-Modus wechseln soll")

