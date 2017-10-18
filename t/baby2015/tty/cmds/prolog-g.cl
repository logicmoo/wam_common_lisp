;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Base: 10. ;Package: BABYLON -*-

(in-package "BABYLON")

;;------------------------------------------------------------------------
;;          deutsche kommandos fuer basic-prolog-mixin
;;------------------------------------------------------------------------


(defbabylon-entry prolog cmd-table german 
  '(" Klausel-Operationen " :value (:open-menu :prolog) 
    :documentation "Ein Menue mit Prolog-Operationen."))

(defbabylon-entry prolog-commands cmd-table german 
  '((" Beantworte Anfrage " :value :prove-display
     :documentation "Stelle Anfrage und gib erste Antwort")
    (" Naechste Antwort " :value  (:prove-display *)
     :documentation "Gib naechste Antwort")
    (" Stelle Format ein " :value :select-format 
     :documentation "Stelle Format ein zum Anzeigen der Ergebnisse")
    (" Erneute Anzeige " :value (:display-result nil redisplay)
     :documentation "Zeige letztes Ergebnis erneut an")
    (" " :no-select t)
    (" Waehle Klauselmengen " :value :select-load-axioms 
     :documentation "waehle die aktuellen klauselmengen aus ")
    (" Liste Klauseln " :value :list-axioms 
     :documentation "liste klauseln auf aus den bekannten klauselmengen")
    (" Zeige Status " :value :show-status 
     :documentation "Zeige eingestellte Optionen an")))


;;------------------------------------------------------------------------
;;          deutsche kommandos fuer mini-prolog-mixin
;;------------------------------------------------------------------------


(defbabylon-entry prolog-toggle-command cmd-table german
  '((" " :no-select t)
    ("Prolog Trace umschalten"
     :value :toggle-prolog-trace
     :documentation "Prolog Trace ein- bzw. ausschalten.")))


(defbabylon-entry prolog-trace cmd-table  german 
  '(" Prolog Trace Optionen " :value (:open-menu :prolog-trace)))


(defbabylon-entry prolog-trace-commands cmd-table german 
  '((" Erweiterter Modus " :value (:set-prolog-trace-mode full)
     :documentation "Zeige alle Klauseln, fuer die Unifikation versucht wird")
    (" Normaler Modus " :value (:set-prolog-trace-mode normal)
     :documentation "Zeige keine Klauseln")
    (" " :no-select t)
    (" Trace alles " :value (:trace-preds all)
     :documentation "Trace alle Praedikate")   
    (" Selektiere Praedikate " :value (:select-for-trace t)
     :documentation "Waehle Praedikate aus, die im Trace erscheinen sollen.")
    (" Aendere Auswahl " :value :select-for-trace
     :documentation "Aendere Auswahl der Praedikate, die im Trace erscheinen sollen.")
    (" " :no-select t)
    (" Zeige Einstellung " :value :show-trace-status
     :documentation "Zeige ausgewaehlte Optionen an")))


;; eof
