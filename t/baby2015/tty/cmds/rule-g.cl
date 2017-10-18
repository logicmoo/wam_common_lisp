;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;------------------------------------------------------------------------
;;          deutsche kommandos fuer basic-rule-mixin
;;------------------------------------------------------------------------



(defbabylon-entry rule cmd-table german
  '(" Regel-Operationen " :value (:open-menu :rule) 
    :documentation "Ein Menue von Regel-Operationen."))

(defbabylon-entry rule2 cmd-table german
  '(" Regel-Operationen " :value (:toggle-menu :rule :top) 
    :documentation "Ein Menue von Regel-Operationen."))

(defbabylon-entry rule-commands cmd-table german
  `(("Liste Regeln" :value :list-rules
     :documentation "Liste ausgewaehlte Regeln.")))
 

;;------------------------------------------------------------------------
;;          deutsche kommandos fuer mini-rule-mixin
;;------------------------------------------------------------------------

;;; mini-rule-mixin kommandos ersetzt fuer 2.1

(defbabylon-entry rule-trace-maincommands cmd-table german
  '(("Regel Trace umschalten"
     :value :toggle-rule-trace
     :documentation "Regel Trace ein- bzw. ausschalten.")
    ("Regel Trace anzeigen"
     :value :call-rule-trace-displayer
     :documentation "Gespeicherten Trace gemaess eingestellter Methode anzeigen.")))

(defbabylon-entry rule-trace cmd-table german 
    '("Regel Trace Optionen"
     :value (:open-menu :rule-trace)))


(defbabylon-entry rule-trace-commands cmd-table german
  '(("Direkter Trace"
     :value (:set-rule-trace-mode direct)
     :documentation "Zeige Trace sofort an.")
    ("Hintergrund Trace"
     :value (:set-rule-trace-mode back)
     :documentation "Speichere Trace ohne Anzeige.")
    ("Kombinierter Trace"
     :value (:set-rule-trace-mode comb)
     :documentation "Speichere Trace bei sofortiger Anzeige.")
    ("  " :no-select t)
    ("Markiere alle Regeln" :value (:mark-all show)
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
    ("Regel Trace anzeigen"
     :value :call-rule-trace-displayer
     :documentation "Gespeicherten Trace gemaess eingestellter Methode anzeigen.")))

 
;;------------------------------------------------------------------------
;;          deutsche kommandos fuer normal-rule-mixin
;;------------------------------------------------------------------------


(defbabylon-entry rule-develop-commands cmd-table german
  '(("  " :no-select t)
    ("Erklaere Ergebnisse" :value :explain-results
      :documentation "Erklaere die Ergebnisse")
    ("Zeige Regel"  :value :print-rule
     :documentation "Zeige eine Regel.")
    ("Inspiziere Regelterme"  :value :inspect-terms
     :documentation
     "Inspiziere Bedingungen und Aktionen von Regeln.")))

;; eof
