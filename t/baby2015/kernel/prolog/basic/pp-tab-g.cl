;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base:10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   Eckehard Gross


;;; generate hash-table ;;;;;;;;;;;;;;;;;;

(defbabylon-table prolog-io-table german :size 100)

;;;;;;;; entries for prolog-mixin ;;;;;;


(defbabylon-entry type-end-to-continue-str prolog-io-table german
  "[Druecke ~:C zur Fortsetzung] ")

(defbabylon-entry rel-name-fstr prolog-io-table german  "~S-CLAUSES")

(defbabylon-entry  sys-trace-fstr prolog-io-table german
  " META -> PROLOG: ~S  ~S")

(defbabylon-entry no-current-kb-fstr prolog-io-table german 
  "~%Keine momentane Wissensbasis")

(defbabylon-entry no-prolog-fstr prolog-io-table german 
  "Momentane Wissensbasis enthaelt kein Prolog")

(defbabylon-entry no-develop-entry prolog-io-table german
  '(" Keine Entwicklungsumgebung verfuegbar " :no-select t))

;;;;;;;; entries for ax-basic ;;;;;;;;;;;

(defbabylon-entry clause-syntax-error-fstr prolog-io-table german
  "~S falsche Syntax fuer Klauseln.")

(defbabylon-entry unknown-axset-fstr prolog-io-table german
  "~%Unbekannte Klauselmenge: ~S")

(defbabylon-entry  clauses-header-fstr prolog-io-table german
  "~% --- Klauseln fuer das Praedikat ~S aus ~S --- ~% ")

(defbabylon-entry no-ax-fstr prolog-io-table german "~%Keine Klauselmengen.")

(defbabylon-entry list-ax-fstr prolog-io-table german "~%Klauselmengen: ~{~S   ~}")

(defbabylon-entry unknown-mode-fstr prolog-io-table german "~%Unbekannter Modus ~S")

(defbabylon-entry relations-fstr prolog-io-table german 
  "~2%;; ********* K L A U S E L N ************~%")

(defbabylon-entry number-of-relations-fstr prolog-io-table german 
  "~%- Anzahl der KLAUSELN: ~38T~d")


;;;;;;;; entries for ax-sc ;;;;;;;;;;;;;;;;;

(defbabylon-entry a-list-str prolog-io-table german "eine Liste")

(defbabylon-entry goals-prompt-fstr prolog-io-table german "~&~%Anfrage = ")

(defbabylon-entry explain-goal-format-fstr prolog-io-table german
  "~%Format: <Goal> oder (<Goal1> ... <GoalN>) ~@
	   ~8@Tmit Goal = (<Praed> <Arg1> ... <ArgN>) oder <Variable>")

(defbabylon-entry result-fstr prolog-io-table german "~%Ergebnis: ~S")

(defbabylon-entry no-fstr prolog-io-table german "~%Nein")

(defbabylon-entry status-fstr prolog-io-table german "~%Status: ~S")

(defbabylon-entry yes-fstr prolog-io-table german "~%Ja")

(defbabylon-entry status-str prolog-io-table german "Status")

(defbabylon-entry wrong-format-fstr prolog-io-table german
  "~S: falsches Format.")

;;;;;;;; entries for syspred ;;;;;;;;;

(defbabylon-entry abort-fstr prolog-io-table german
  "~%Syntaxfehler fuer Goal: ~S")

(defbabylon-entry not-instant-fstr prolog-io-table german
  "~%Goal: ~S nicht instantiiert")

(defbabylon-entry illegal-pred-fstr prolog-io-table german
  "~%Praedikat des Goals: ~S unzulaessig")

(defbabylon-entry not-eval-fstr prolog-io-table german
  "~%Form: ~S nicht zu evaluieren")

(defbabylon-entry wrong-argument-fstr prolog-io-table german
  "~%~S falsches Argument fuer ~S.")

(defbabylon-entry illegal-clause-fstr prolog-io-table german
  "~%~S unzulaessiges Argument fuer eine Klausel.")


;;;;;;;;;; entries from syspreds-trace ;;;;;;;;

(defbabylon-entry first-proof-fstr prolog-io-table german
  "~A. . . . . erster Beweis von ~S . . . . .")

(defbabylon-entry next-proof-fstr prolog-io-table german
  "~A. . . . . naechster Beweis von ~S . . . . .")

(defbabylon-entry top-cut-fail-fstr prolog-io-table german
  "~A- - - - cut-fail auf oberster Stufe - - - ")

(defbabylon-entry normal-try-fstr prolog-io-table german
  "~A=> try: ~S")

(defbabylon-entry normal-retry-fstr prolog-io-table german
  "~A=> retry: ~S")

(defbabylon-entry normal-succ-fstr prolog-io-table german
  "~A<= succ: ~S")

(defbabylon-entry normal-fail-fstr prolog-io-table german
  "~A<= fail: ~S")

(defbabylon-entry cut-fail-fstr prolog-io-table german
  "~A<= cut-fail: ~S")

(defbabylon-entry top-cut-fstr prolog-io-table german
  "~A....... cut auf oberster Stufe ........")

(defbabylon-entry cut-fstr prolog-io-table german
  "~A..........cut............")

(defbabylon-entry forced-fail-fstr prolog-io-table german
  "~A- - - - - erzwungenes fail - - - - - - ")

(defbabylon-entry succ-lisp-fstr prolog-io-table german
  "~A+ + erfolgreicher Lisp-Aufruf: ~S")

(defbabylon-entry fail-lisp-fstr prolog-io-table german
  "~A- - fehlgeschlagener Lisp-Aufruf: ~S")

(defbabylon-entry succ-is-fstr prolog-io-table german
  "~A+ + erfolgreiches is: ~S == ~S = ~S")

(defbabylon-entry fail-is-fstr prolog-io-table german
  "~A- - fehlgeschlagenes is: ~S =//= ~S = ~S")

(defbabylon-entry repeat-fstr prolog-io-table german
  "~A- - - - ~S. Wiederholung - - - - - - ")

(defbabylon-entry succ-equal-fstr prolog-io-table german
  "~A+ + erfolgreiches  = : ~S und ~S unifizierbar")

(defbabylon-entry fail-equal-fstr prolog-io-table german
  "~A- - fehlgeschlagenes  = : ~S und ~S nicht unifizierbar")

(defbabylon-entry succ-noequal-fstr prolog-io-table german
  "~A+ + erfolgreiches  \= : ~S und ~S nicht unifizierbar")

(defbabylon-entry fail-noequal-fstr prolog-io-table german
  "~A- - fehlgeschlagenes  \= : ~S und ~S unifizierbar")

(defbabylon-entry succ-read-fstr prolog-io-table german
  "~A+ + erfolgreiches  read: ~S == ~S")

(defbabylon-entry fail-read-fstr prolog-io-table german
  "~A- - fehlgeschlagenes  read: ~S =//= ~S")

(defbabylon-entry write-fstr prolog-io-table german
  "~A+ + geschrieben: ~S")

(defbabylon-entry format-fstr prolog-io-table german
  "~A+ + geschrieben im Format ~S : ~{~S ~}")

(defbabylon-entry succ-type-fstr prolog-io-table german
  "~A+ + erfolgreicher Typ-Check: ~S ist vom Typ ~S")

(defbabylon-entry fail-type-fstr prolog-io-table german
  "~A- - fehlgeschlagener Typ-Check: ~S ist nicht vom Typ ~S")

(defbabylon-entry assert-fstr prolog-io-table german
  "~A+ + hinzugefuegt zu ~S: ~S")

(defbabylon-entry remove-fstr prolog-io-table german
  "~A+ + entfernt aus ~S: ~S")

(defbabylon-entry pred-remove-fstr prolog-io-table german
  "~A+ + Praedikat ~S entfernt aus  ~S")

(defbabylon-entry pred-not-def-fstr prolog-io-table german
  "~A+ + Praedikat ~S war nicht definiert")

;;;;;;;;;;;; entries for prolog-interpreter ;;;;;;

(defbabylon-entry cr-wrong-status-fstr prolog-io-table german
  "~%~S flascher Status fuer ~S")

(defbabylon-entry wrong-status prolog-io-table german
  "~S flascher Status fuer ~S")

(defbabylon-entry wrong-mode prolog-io-table german
  "~S: flascher Modus fuer :PROVE.")

(defbabylon-entry next-solution-str prolog-io-table german
  " Naechste Antwort ")

(defbabylon-entry no-explanation-entry prolog-io-table german
  '(" Keine Erklaerung verfuegbar " :no-select t))


;;;;;;;;;;  entries for trace-mixin ;;;;;;;;;;;;;;;

;;; ersetzt fuer 2.1

;;;;;;;;;;;; entries for axdevelop-mixin ;;;;;;


(defbabylon-entry no-axioms-item prolog-io-table german
  '(no-axioms  " Keine Klauselmengen " (t)))

(defbabylon-entry choose-axioms-str prolog-io-table german
  " Waehle Klauselmengen aus: ")

(defbabylon-entry clause-prompt-fstr prolog-io-table german
  "KLAUSEL (oder ~:C) = ")

(defbabylon-entry clause-help-fstr prolog-io-table german
  "~&Moegliche Eingaben 1. = (<praed> <arg1> ... <argN>) ~@
   ~19@T2. = ((<praed> <arg1> ... <argN>) <- (<praed1> <arg1> ... <argN>) ...)~%")

(defbabylon-entry incorrect-syntax-fstr prolog-io-table german
  "~&==>> Unzulaessige Syntax: ~S")

(defbabylon-entry answer-prompt-fstr prolog-io-table german
  "~S Antwort (oder am Ende RETURN) = ")

(defbabylon-entry axset-prompt-fstr prolog-io-table german
  "~%Name der Klauselmenge = ")

(defbabylon-entry overwrite?-fstr prolog-io-table german
  "~%~S Existiert bereits. Soll ueberschreiben werden? ~
                  (gib J, N oder zum Abbruch ~:C ein): ")

(defbabylon-entry nr-clause-prompt-fstr prolog-io-table german
  "~S KLAUSEL (oder ~:C) = ")

(defbabylon-entry suspend-item prolog-io-table german
  '("- SUSPEND MENUE -" :value suspend
    #+:lispm :font #+:lispm fonts:cptfontb
    ))

(defbabylon-entry exit-listing-item prolog-io-table german
  '("- EXIT LISTING -" :value exit
    #+:lispm :font #+:lispm fonts:cptfontb
    ))

(defbabylon-entry select-axset-str prolog-io-table german
  " Waehle Klauselmenge aus ")

(defbabylon-entry all-item prolog-io-table german
  '("--- ALLE ---" :value all
    #+:lispm :font #+:lispm fonts:cptfontb
    ))

(defbabylon-entry exit-menu-item prolog-io-table german
  '("-- EXIT MENUE --" :value exit
    #+:lispm :font #+:lispm fonts:cptfontb
    ))

(defbabylon-entry which-pred-fstr prolog-io-table german
  " Welches Praedikat aus ~S ?")

(defbabylon-entry is-metavar-fstr prolog-io-table german
  " ~S ist eine Metavariable ")

(defbabylon-entry is-varpred-fstr prolog-io-table german
  " ~S hat Variable zum Praedikat ")

(defbabylon-entry is-system-pred-fstr prolog-io-table german
  " ~S ist ein Systempraedikat ")

(defbabylon-entry syntax-error-fstr prolog-io-table german
  " Syntaxfehler ")

(defbabylon-entry no-clauses-for-pred-fstr prolog-io-table german
  "Keine Klauseln fuer Praedikat ~S")

(defbabylon-entry back-to-fstr prolog-io-table german
  " ZURUECK zu ~S ")

(defbabylon-entry exit-inspect-item prolog-io-table german
  '("-- EXIT INSPECT --" :value exit
    #+:lispm :font #+:lispm fonts:cptfontb
    ))

(defbabylon-entry exit-list-item prolog-io-table german
  '("-- EXIT LIST --" :value exit
    #+:lispm :font #+:lispm fonts:cptfontb
    ))

(defbabylon-entry edit-clauses-str prolog-io-table german
  " Editiere Klauseln der Klauselmenge: ")

(defbabylon-entry edit-which-axset-str prolog-io-table german
  " Editiere Klauselmenge: ")

(defbabylon-entry file-prompt-fstr prolog-io-table german
  "~%Geben Sie bitte den Namen der Datei an ~@
   zum Retten der ausgewaehlten Klauselmengen ~@
   (Default: ~A): ")

(defbabylon-entry nothing-saved-fstr prolog-io-table german
  "Nichts gerettet~%")

(defbabylon-entry save-confirm-fstr prolog-io-table german
  "Rette die Klauselmengen: ~{~S ~} auf Datei: ~A ~@
                            Bitte bestaetigen (Yes oder No): ")

(defbabylon-entry done-fstr prolog-io-table german
  "Fertig~%")

(defbabylon-entry true?-fstr prolog-io-table german
  "~%Ist folgendes wahr: ~S ? ")

(defbabylon-entry ax-manipulation-str prolog-io-table german
  " Klausel-Manipulation: ")

;;;;;;;;; entries for explain-mixin ;;;;;;;;

(defbabylon-entry subgoals-fstr prolog-io-table german
  "~%~%~3TSubgoal: ~S")

(defbabylon-entry topgoal-reached-fstr prolog-io-table german
  "~%~3TTopgoal erreicht")

(defbabylon-entry why-item-list prolog-io-table german
  '(Why-Goal Why-Path Exit))

(defbabylon-entry further-explanation-str prolog-io-table german
  " Weitere Erklaerungen: ")

(defbabylon-entry needed-to-prove-fstr prolog-io-table german
  "~3Tbenoetigt zum Beweis von: ~S")

(defbabylon-entry by-clause-fstr prolog-io-table german 
  "~3Tmittels Klausel: ~S ~@[~S~] ~{~%~16T~S~}")

(defbabylon-entry clauses-for-pred prolog-io-table german
  "Klauseln fuer Praedikat ~S")

(defbabylon-entry fictive-top-goal-str prolog-io-table german
  " fiktives Topgoal ")

(defbabylon-entry built-in-goal-str prolog-io-table german
  "Ist ein Systempraedikat")

(defbabylon-entry goal-fstr prolog-io-table german
  "  Goal: ~S ")

(defbabylon-entry clause-used-for-proof-fstr prolog-io-table german
  "Zum Beweis von Goal: ~S benutzte Klausel")

(defbabylon-entry no-clauses-fstr prolog-io-table german
  " Keine Klauseln fuer Goal: ~S ")


(defbabylon-entry prolog-ted-str prolog-io-table german
  "Prolog-Baumeditor")

(defbabylon-entry clauses-str prolog-io-table german
  "Klauseln")

(defbabylon-entry clauses-doc-str prolog-io-table german
  "Zeige alle Klauseln.")

(defbabylon-entry clause-used-str prolog-io-table german
  "Benutzte Klausel")

(defbabylon-entry clause-used-doc-str prolog-io-table german
  "Zeige benutzte Klausel.")

(defbabylon-entry prolog-prooftree-fstr prolog-io-table german
  "PROLOG-BEWEISBAUM~D")

(defbabylon-entry explain-mixin-item-list prolog-io-table german
  '((" Erklaere Ergebnis " :FUNCALL display-prooftree 
     :documentation "Zeige Beweisbaum an")))

;;;;;; entries from prolog-processor ;;;;;;;


(defbabylon-entry choose-format-str prolog-io-table german
  " Waehle Format zum Anzeigen der Ergebnisse: ")

(defbabylon-entry format-item-list prolog-io-table german
  '(("Anfrage mit Ersetzungen" :value form)
    ("Alle Variablen" :value vars)
    ("Alle gebundenen Variablen" :value bound)
    ("Keine Ausgabe" :value no)))

(defbabylon-entry continue-str prolog-io-table german
 " Weiter mit: ")

(defbabylon-entry return-to-prolog-fstr prolog-io-table german
  "Zurueck zu PROLOG")


;;;--------------------------------------------------------------------------------
;;; new entries for 2.1
;;;--------------------------------------------------------------------------------


(defbabylon-entry if-toggled-fst prolog-io-table german 
  "~%Falls der Prolog Trace eingeschaltet wird:")

(defbabylon-entry trace-for-preds-fstr prolog-io-table german
  "~%~S Trace fuer die Praedikate:")

(defbabylon-entry none-fstr prolog-io-table german
  "  KEINE ")

(defbabylon-entry all-fstr prolog-io-table german
  "  ALLE ")


(defbabylon-entry trace-menu-title prolog-io-table german 
  "Trace Praedikate der Klauselmenge ")


(defbabylon-entry trace-menu-items prolog-io-table german 
  '(("Trace alles"    :value trace-all)
    ("Trace nichts"   :value trace-none)
    ("Wechsel Einstellung" :value toggle
     :documentation "Waehle Praedikate aus, deren Auftreten im Trace sich aendern soll")
    ("Exit" :value exit)))

(defbabylon-entry toggle-trace-menu-title prolog-io-table german 
   "Wechsel Einstellung fuer Praedikate aus ")

(defbabylon-entry toggle-trace-menu-item prolog-io-table german 
   '(nil "Mit # markierte Praedikate treten im Trace auf" nil))


(defbabylon-entry option-menu-title prolog-io-table german 
   "Waehle Prolog Trace Optionen")

(defbabylon-entry option-menu-items prolog-io-table german 
  '(("- Exit -" :value exit)
    ("Erweiterter Modus" :value (:set-prolog-trace-mode full)
     :documentation "Zeige alle Klauseln, fuer die Unifikation versucht wird")
    ("Normaler Modus" :value (:set-prolog-trace-mode normal)
     :documentation "Zeige keine Klauseln")
    (" " :no-select t)
    ("Trace alles" :value (:trace-preds all)
     :documentation "Trace alle Praedikate")   
    ("Selektiere Praedikate" :value (:select-for-trace t)
     :documentation "Waehle Praedikate aus, die im Trace erscheinen sollen.")
    ("Aendere Auswahl" :value :select-for-trace
     :documentation "Aendere Auswahl der Praedikate, die im Trace erscheinen sollen.")
    (" " :no-select t)
    ("Zeige Einstellung" :value :show-trace-status
     :documentation "Zeige ausgewaehlte Optionen an")))


;;; eof

