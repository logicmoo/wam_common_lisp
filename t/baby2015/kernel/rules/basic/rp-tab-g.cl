;;; -*- Mode: Lisp; Syntax:  Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   J. W A L T H E R


;;
;;This is the German version of all the strings and menu-item-lists of 
;;the rule processor. 



(defbabylon-table rule-io-table german :size 200)


;;; **************** syntax ****************

(defbabylon-entry rule-set-name-error-fstr rule-io-table german
  "~S: Nicht zulaessiger Bezeichner fuer ein Regelpaket der Wissensbasis ~S.~@
   Der Bezeichner muss ein Symbol sein.")

(defbabylon-entry rule-set-name-error-spot-fstr rule-io-table german
  "in der Regelmenge ~S der Wissensbasis ~S")

(defbabylon-entry rule-lhs-example-str rule-io-table german
  "(<JUNKTOR> <praemisse-1> ... <praemisse-N>)")

(defbabylon-entry rule-rhs-example-str rule-io-table german
  "(<AKTIONS-TYP> <aktion-1> ... <aktion-N>)")

(defbabylon-entry rule-example-fstr rule-io-table german
  "~%(<REGEL-BEZEICHNER>~@
   ~4@T~A~@
   ~4@T~A")

(defbabylon-entry rule-syntax-error-fstr rule-io-table german
  "Falsche Syntax in Regel ~S ~A. Das korrekte Regelformat ist: ~A")

(defbabylon-entry rule-error-description-fstr rule-io-table german
  "~%>>Falsche Syntax in Regel ~S ~A.")

(defbabylon-entry rule-correct-description-fstr rule-io-table german
  "Das korrekte Regelformat ist: ~A")

(defbabylon-entry rule-variables-error-fstr rule-io-table german
  "~S: ~@
   Falsche Variablen-Spezifikation ~A. ~@
   Die Variablen-Spezifikation muss eine Liste von mit ~@
   Unterstrich beginnenden Symbolen sein.")


;;; **************** rule statistics ****************


(defbabylon-entry rule-statistics-header-str rule-io-table german
  "~2%;; ************** R E G E L N ************~2%")

(defbabylon-entry rule-sets-header-fstr rule-io-table german
  "~%- Anzahl der REGELMENGEN: ~38T~D")

(defbabylon-entry rule-set-header-fstr rule-io-table german
  "~%  - Anzahl der Regeln in Regelmenge~%    ~S ~38T~D")

(defbabylon-entry rule-statistics-trailer-str rule-io-table german
  "~%- Gesamtzahl der Regeln: ~38T~D")


;;; **************** tracing ****************


(defbabylon-entry active-str rule-io-table german
  "Aktive")

(defbabylon-entry inactive-str rule-io-table german
  "Inaktive")

(defbabylon-entry is-needed-to-show-fstr rule-io-table german
  " ~A wird gebraucht zum ~S: ")

(defbabylon-entry is-hypothesis-fstr rule-io-table german
  " ~A ist eine Hypothese ")

(defbabylon-entry attempt-to-verify-str rule-io-table german
  " Versuche zu verifizieren ")

(defbabylon-entry click-why-str rule-io-table german
  "Klicken Sie hier um zu erfahren warum.")

(defbabylon-entry rule-str rule-io-table german
  " -> Regel ")

(defbabylon-entry click-show-str rule-io-table german
  "Klicken Sie hier um diese Regel zu zeigen.")

(defbabylon-entry conclude-str rule-io-table german
  " schliesst ")

(defbabylon-entry execute-str rule-io-table german
  " fuehrt aus ")

(defbabylon-entry sharp-sign-str rule-io-table german
  " ## ")

(defbabylon-entry try-rule-str rule-io-table german
  " -> Versuche Regel")

(defbabylon-entry test-hypothesis-fstr rule-io-table german
  " ->> Hypothese = ~S.")


;;; **************** term matching ****************

(defbabylon-entry a-rule-set-fstr rule-io-table german
  " ~S Menge")

(defbabylon-entry which-rule-set-to-inspect-str rule-io-table german
  " Welche Regelmenge soll inspiziert werden ?")

(defbabylon-entry match-choose-item-list rule-io-table german
  '(("                       " :no-select t)
    (" Finde alle Terme      " :value equal
     :documentation "Finde alle Terme in der Regelmenge")
    (" Vergleiche 1. Element " :value filter-first
     :documentation "Finde alle ersten Elemente der Terme einer Regelmenge")
    (" Vergleiche 2. Element " :value filter-second
     :documentation "Finde alle zweiten Elemente der Terme einer Regelmenge")
    (" Vergleiche 1. und 2.  " :value filter-first-and-second
     :documentation "Finde alle ersten und zweiten Elemente einer Regelmenge")
    ("                       " :no-select t)
;    (" SELEKTIERE REGELMENGE " :value choose-rule-set
;     :documentation "Waehle eine neue Regelmenge aus")
    ("                       " :no-select t)
    (" E N D E               " :value do-nothing
     :documentation "Verlasse das Regelinspizieren")))

(defbabylon-entry match-choose-menu-str rule-io-table german
  " Nach welchem Muster moechten Sie die Terme selektieren ?")

(defbabylon-entry match-choose-element-first-str rule-io-table german
  " Waehlen Sie erstes Element fuer den Vergleich: ")

(defbabylon-entry match-choose-element-second-str rule-io-table german
  " Waehlen Sie zweites Element fuer den Vergleich: ")

(defbabylon-entry match-choose-slots-str rule-io-table german
  " Waehlen Sie einen der Slots: ")

(defbabylon-entry match-choose-term-str rule-io-table german
  " Es gibt ~S Terme in den ~S Regeln.     
 ~% Welche moechten Sie inspizieren ?~%     ")

(defbabylon-entry used-as-condition-str  rule-io-table german
  "------ Benutzt als Praemisse in: ------")

(defbabylon-entry used-as-action-str rule-io-table german
  "------ Benutzt als Aktion in:    ------")

(defbabylon-entry list-rules-for-term-fstr rule-io-table german
  " Untenstehend finden Sie die Regeln, die den Term~@
    ~@T~S benutzen.~@
    ~@TWaehlen Sie die anzuzeigenden Regeln:                     ")

;;; **************** add rule ****************


(defbabylon-entry add-to-which-rule-set-str rule-io-table german
  " Zu welcher Regelmenge soll die Regel hinzugefuegt werden ? ")

(defbabylon-entry add-rule-prompt-fstr rule-io-table german
  " Soll die Regel ~S jetzt zur Regelmenge ~S hinzugefuegt werden? ")

;;; **************** delete rule ****************


(defbabylon-entry in-which-rule-set-to-delete-str rule-io-table german
  " In welcher Regelmenge ist die zu loeschende Regel ?")

(defbabylon-entry which-rule-to-delete-fstr rule-io-table german
  " Welche Regel aus ~S loeschen ? ")

(defbabylon-entry rule-fstr rule-io-table german
  " REGEL ~S")

(defbabylon-entry in-rhs-str rule-io-table german
  " => in RHS von:")

(defbabylon-entry in-lhs-str rule-io-table german
  " => in LHS von:")

(defbabylon-entry prev-rule-fstr rule-io-table german
  " VORIGE REGEL: ~S")

(defbabylon-entry wrong-display-choice-fstr rule-io-table german
  "~%Ungueltige Wahl aus :DISPLAY-RULE ~A")

;;; **************** print rule ****************

(defbabylon-entry in-which-rule-set-to-print-str rule-io-table german
  " In welche Regelmenge ist die zu zeigende Regel ?")

(defbabylon-entry which-rule-to-print-fstr rule-io-table german
  " Welche Regel aus ~S soll gezeigt werden ? ")

;;; **************** edit rule ****************

(defbabylon-entry in-which-rule-set-to-edit-str rule-io-table german
  " In welche Regelmenge ist die zu editierende Regel ?")

(defbabylon-entry which-rule-to-edit-fstr rule-io-table german
  " Welche Regel aus ~S soll editiert werden ? ")


(defbabylon-entry which-rule-set-to-edit-str rule-io-table german
  " Welche Regelmenge soll editiert werden ? ")

;;; **************** prolog stuff ****************

(defbabylon-entry prolog-goal-spec-str rule-io-table german
  "<PROLOG GOAL>")

(defbabylon-entry input-spec-fstr rule-io-table german
  "<INPUT ~S>")

(defbabylon-entry input-spec-str rule-io-table german
  "<INPUT USER>")

;;; **************** disply rule tree ****************


(defbabylon-entry which-rule-set-to-display-str rule-io-table german
  " Welche Regelmenge soll untersucht werden ? ")

(defbabylon-entry match-choose-header-str rule-io-table german
  " Nach welcher Vergleichsart soll untersucht werden ?")

(defbabylon-entry match-choose-first-str rule-io-table german
  " Waehle erstes Element fuer den Vergleich: ")

(defbabylon-entry match-choose-second-str rule-io-table german
  " Waehle zweites Element fuer den Vergleich: ")

(defbabylon-entry choose-one-slot-str rule-io-table german
  " Waehle einen der Slots: ")

(defbabylon-entry choose-terms-fstr rule-io-table german
  " Es gibt ~S Terme in der ~S Regelmenge.     
 ~% Welche wollen Sie untersuchen ?~%     ")


;;; **************** general ****************

(defbabylon-entry exit-label-str rule-io-table german
  " E X I T ")

(defbabylon-entry confirm-str rule-io-table german
  " Bestaetige: ")

(defbabylon-entry do-nothing-str rule-io-table german
  " Nichts ")

(defbabylon-entry yes-str rule-io-table german
  " Ja ")

(defbabylon-entry no-str rule-io-table german
  " Nein ")

(defbabylon-entry rule-sort-function-arg-error-fstr rule-io-table german
  "==>> ~S : falsche Anzahl Parameter in RULE-SORT-FUNCTION ~S~@
   ~A")

(defbabylon-entry rule-sort-function-undef-error-fstr rule-io-table german
  "==>> ~S : RULE-SORT-FUNCTION~@
   ~A nicht definiert")

(defbabylon-entry rule-sort-function-error-fstr rule-io-table german
  "==>> ~S : falsche RULE-SORT-FUNCTION~@
   ~A")

(defbabylon-entry hypotheses-spec-number-error-fstr rule-io-table german
  "==>> ~S : falsche Angabe fuer die Anzahl der zu verifizierenden Hypothesen~@
   ~9@Tin :TEST-HYPOTHESES ~S fuer Wissensbasis ~S.")

(defbabylon-entry hypotheses-spec-list-error-fstr rule-io-table german
  "==>> ~S: falsche Angabe der zu verifizierenden Hypothesen~@
   ~9@Tin :TEST-HYPOTHESES fuer Wissensbasis ~S.")

(defbabylon-entry hypotheses-spec-number-error-obtain-fstr rule-io-table german
  "==>> ~S : falsche Angabe fuer die Anzahl der zu verifizierenden Hypothesen~@
   ~9@Tin :OBTAIN ~S fuer Wissensbasis ~S.")

(defbabylon-entry method-property-error-fstr rule-io-table german
  "~S besitzt keinen Regelprozessor laut Eigenschaftsliste von ~S.")

(defbabylon-entry bindings-spec-error-fstr rule-io-table german
  "~S~@
   Falsche Variablenspezifikation fuer~@
   ~S in Regelmenge ~S.")

(defbabylon-entry rule-set-not-found-error-fstr rule-io-table german
  "~%==>> Die Regelmenge ~S existiert nicht.")

(defbabylon-entry rule-does-not-exist-error-fstr rule-io-table german
  "~S ~S nicht definiert.")

(defbabylon-entry ask-user-wrong-answer-error-str rule-io-table german
  "Falsche Antwort fuer rule-interpreter :ask-user.")

(defbabylon-entry justifications-type-error-str rule-io-table german
  "~%unbekannter Typ von justificans ~S")

(defbabylon-entry fact-type-error-fstr rule-io-table german
  "~%~S: Falscher Fakttyp.")

(defbabylon-entry justifications-missing-error-fstr rule-io-table german
  "~S ist nicht begruendbar.")

(defbabylon-entry said-to-be-true-fstr rule-io-table german
  "Laut Eingabe ist ~S wahr.")

(defbabylon-entry said-to-be-false-fstr rule-io-table german
  "Laut Eingabe ist ~S falsch.")

(defbabylon-entry said-to-be-unknown-fstr rule-io-table german
  "Laut Eingabe ist ~S unbekannt.")

(defbabylon-entry how-description-fstr rule-io-table german
  "~S wurde ~Sd mittels Regel ~S ~S.")

(defbabylon-entry not-provable-fstr rule-io-table german
  "~S konnte nicht mittels Regelmenge ~S geschlossen werden.")

(defbabylon-entry evaluation-msg-fstr rule-io-table german
  " Ich arbeite jetzt an Regel ~S ~S.")

(defbabylon-entry already-established-msg-str rule-io-table german
  " Es ist bereits bekannt:")

(defbabylon-entry false-true-msg-fstr rule-io-table german
  " Wenn ~S ~:[false,~;true,~] ist,")

(defbabylon-entry is-false-msg-fstr rule-io-table german
  " ~S ~S ist falsch")

(defbabylon-entry is-true-msg-fstr rule-io-table german
  " ~S ~S ist wahr")

(defbabylon-entry then-msg-fstr rule-io-table german
  " kann ich ~S ")

(defbabylon-entry since-msg-fstr rule-io-table german
  " Da ~S")

(defbabylon-entry i-have-to-msg-fstr rule-io-table german
  " muss ich ~S ")

(defbabylon-entry results-header-msg-str rule-io-table german
  " Die Ergebnisse sind :")

(defbabylon-entry no-positive-results-msg-str rule-io-table german
  " Es gibt keine positiven Ergebnisse !")

(defbabylon-entry rule-set-fstr rule-io-table german
  " ~S Regelmenge")

(defbabylon-entry to-display-question-str rule-io-table german
  " Soll die gerade erstellte Regel gezeigt werden ? ")

(defbabylon-entry delete-item-fstr rule-io-table german
  " Loesche ~S")

(defbabylon-entry display-item-fstr rule-io-table german
  " Zeige ~S")

(defbabylon-entry edit-item-fstr rule-io-table german
  " Editiere ~S")

(defbabylon-entry type-fstr rule-io-table german
  " ~S Typ")

(defbabylon-entry no-point-str  rule-io-table german
  " Nein. ")

(defbabylon-entry how-str  rule-io-table german
  " Warum positiv ? ")

(defbabylon-entry how-str-mouse-doc  rule-io-table german
  "Gib eine Begruendung fuer einen gueltigen Fakt")

(defbabylon-entry how-all-str   rule-io-table german
  " Warum alle ? ")

(defbabylon-entry how-all-str-mouse-doc  rule-io-table german
  "Gib eine Begruendung fuer einen untersuchten Fakt")

(defbabylon-entry how-ultimately-str rule-io-table german
  " Warum letztlich ? ")

(defbabylon-entry how-ultimately-str-mouse-doc rule-io-table german
  "Gib eine Ableitung fuer einen gueltigen Fakt")

(defbabylon-entry why-not-str  rule-io-table german
  " Warum nicht ? ")

(defbabylon-entry why-not-str-mouse-doc rule-io-table german
  "Gib eine Ableitung fuer einen ungueltigen Fakt")

(defbabylon-entry print-rule-item-str rule-io-table german
  " Zeige Regel ")

(defbabylon-entry lisp-item-str  rule-io-table german
  " LISP ")

(defbabylon-entry other-questions-str rule-io-table german
  "  Noch Fragen ? ")

(defbabylon-entry no-display-available-str rule-io-table german
  " Die Display Methode ist in dieser Konfiguration nicht verfuegbar ")

(defbabylon-entry print-rule-term-header-str rule-io-table german
  " Zeige die Regeln fuer welchen Term ? ")


(defbabylon-entry rule-ted-str rule-io-table german
  "Regel-Baumeditor")


(defbabylon-entry how-which-fact-str rule-io-table german
  "Wie wurde welcher Fakt erhalten ? ")

(defbabylon-entry meta-rule-reference-trace-fstr rule-io-table german
  " META -> REGEL-PROZESSOR ~S  ~S")

;;;;;;;;;;; added e.gross 

(defbabylon-entry hypotheses-verified-fstr rule-io-table german
 "~&~%Verifizierte Hypothesen: ~{~%   ~S~} ~%")

(defbabylon-entry no-hypothesis-verified-fstr rule-io-table german
 "~&~%Keine Hypothese konnte verifiziert werden. ~%")

(defbabylon-entry  true-facts-fstr rule-io-table german
  "~&~%Folgende Aussagen sind wahr: ~{~%   ~S~} ~%")

(defbabylon-entry  no-true-facts-fstr rule-io-table german 
  "~&~%Wahre Aussagen waren nicht zu ermitteln. ~%")



(defbabylon-entry suspend-item rule-io-table  german
  '("- Suspend Menu -" :value suspend
    #+:lispm :font #+:lispm fonts:cptfontb
    ))


(defbabylon-entry exit-menu-item rule-io-table  german
  '("-- Ende Auswahl --" :value exit
    #+:lispm :font #+:lispm fonts:cptfontb
    ))


(defbabylon-entry type-end-to-continue-str rule-io-table  german
  " [Druecke ~:C zur Fortsetzung] ")


(defbabylon-entry select-rule-set-str rule-io-table  german
  " Waehle Regelmenge aus ")


(defbabylon-entry which-rule-fstr rule-io-table german
  " Liste welche Regel aus ~S ?")


;;;----------------------------------------------------------------
;;; added for 2.1
;;;----------------------------------------------------------------


(defbabylon-entry mark-menu-title rule-io-table german 
  " Markiere Regeln der Regelmenge ")

(defbabylon-entry mark-menu-items rule-io-table german 
  '(("Zeige alle" :value show-all)
    ("Verberge alle" :value hide-all)
    ("Wechsel Markierung" :value toggle
     :documentation "Waehle Regeln aus, deren Auftreten im Trace sich aendern soll")
    ("Exit" :value exit)))

(defbabylon-entry toggle-mark-menu-title rule-io-table german 
   " Wechsel Markierung fuer Regeln aus ")		  

(defbabylon-entry toggle-mark-menu-item rule-io-table german 
   '(nil "Mit # markierte Regeln koennen im Trace auftreten" nil))

(defbabylon-entry ask-user-str rule-io-table german 
  "fragt Benutzer nach")

(defbabylon-entry no-entry-str rule-io-table german 
  "Kein Eintrag qualifizierte sich.")


(defbabylon-entry option-menu-title rule-io-table german 
 "Waehle Rule Trace Optionen")

(defbabylon-entry option-menu-items rule-io-table german 
  '(("- Exit -" :value exit)
    ("Direkter Trace"
     :value (:set-rule-trace-mode direct)
     :documentation "Zeige Trace sofort an.")
    ("Hintergrund Trace"
     :value (:set-rule-trace-mode back)
     :documentation "Speichere Trace ohne Anzeige.")
    ("Kombinierter Trace"
     :value (:set-rule-trace-mode comb)
     :documentation "Speichere Trace bei sofortiger Anzeige.")
    ("  " :no-select t)
    ("Markiere alle Regeln" :value :show-all
     :documentation "Alle Regeln saemtlicher Regelmengen erscheinen im Trace.")
    ("Selektiere Regeln" :value (:select-rules-for-tracing t)
     :documentation "Waehle Regeln aus, die im Trace erscheinen sollen.")
    ("Aendere Auswahl" :value :select-rules-for-tracing 
     :documentation "Aendere Auswahl der Regeln, die im Trace erscheinen sollen.")
    ("  " :no-select t)   
    ("Vollstaendiger Trace"
     :value (:set-rule-trace-displayer :display-rule-trace)
     :documentation "Zeige bei Regel Trace anzeigen den vollstaendigen Trace.")
    ("Anwenden von Regeln"
     :value (:set-rule-trace-displayer :display-rules-used) 
     :documentation "Zeige bei Regel Trace anzeigen, wann Regeln angewandt wurden.")
    ("Pruefen von Regeln"
     :value (:set-rule-trace-displayer :display-rules-tried)
     :documentation "Zeige bei Regel Trace anzeigen, wann Regeln geprueft wurden.")
    ("-----------------------" :no-select t)
    ("Regel Trace anzeigen" :value :call-rule-trace-displayer
     :documentation "Gespeicherten Trace gemaess eingestellter Methode anzeigen.")))

;;; eof

