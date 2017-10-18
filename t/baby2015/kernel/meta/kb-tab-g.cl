;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   Eckehard Gross

;; All the string stuff for files in directory IO and KBSYSTEM.
;; Meaning of suffices:
;; -str         Simple String
;; -fstr        Format String
;; -item        Single Menu Item
;; -item-list   Item Lists for Menus


(defbabylon-table babylon-io-table german)


(defbabylon-entry type-end-to-confirm-str babylon-io-table german
  "[Druecke ~:C zur Bestaetigung] ")

(defbabylon-entry type-end-to-continue-str babylon-io-table german
  "[Druecke ~:C zur Fortsetzung] ")


(defbabylon-entry select-to-confirm-str babylon-io-table german 
  "[Selektiere zur Bestaetigung] ")

(defbabylon-entry select-to-continue-str babylon-io-table german 
  "[Selektiere zur Fortsetzung] ")


(defbabylon-entry confirm-str babylon-io-table german
  " Bestaetige: ")

(defbabylon-entry quit-item babylon-io-table german
  '(" Quittiere:   x " :value :ok))

(defbabylon-entry do-nothing-str babylon-io-table german
  " Do Nothing ")

(defbabylon-entry none-kb-current-str babylon-io-table german
  " Keine WB aktuell ")

(defbabylon-entry none-kb-known-str babylon-io-table german
  " Keine WB bekannt ")

(defbabylon-entry current-kb-fstr babylon-io-table german
  "Aktuelle WB: ~S")

(defbabylon-entry choose-kb-str babylon-io-table german
   " Waehle eine WB ")

(defbabylon-entry kill-kb-fstr babylon-io-table german
    " Kill ~S ")

(defbabylon-entry run-loop-str babylon-io-table german
    " Gib Nachrichten via Lisp Listener ein")
					
(defbabylon-entry file-name-with-default-fstr babylon-io-table  german
  "Gib Dateinamen an [Standard ~A]: ")


(defbabylon-entry print-info-about-fstr babylon-io-table  german
  " Liste Informationen ueber ~S")


(defbabylon-entry reset-kb-fstr babylon-io-table  german
  " Setze ~S zurueck")

(defbabylon-entry start-fstr babylon-io-table  german
  " Starte ~S")

(defbabylon-entry starting-kb-fstr babylon-io-table  german
   "~2%~10T******  STARTING ~S  ****** ~2%")



(defbabylon-entry state-of-kb-fstr babylon-io-table  german
  "~2%****** Status der Wissensbasis ~S ****** ~%")

(defbabylon-entry source-file-fstr babylon-io-table  german
  "~%- Quelldatei(en): ~38T~{~A  ~}")



(defbabylon-entry enter-name-of-file-for-kb-fstr babylon-io-table  german
  "Geben Sie einen Dateinamen an, um ~S zu retten [STANDARD = ~A] ")


(defbabylon-entry kb-declaration-fstr babylon-io-table  german
  "~2%;; ************* WISSENSBASIS  DEKLARATION ***********~2%")

(defbabylon-entry instructions-fstr babylon-io-table  german
  "~2%;; ********* I N S T R U K T I O N E N ************~2%")


(defbabylon-entry save-fstr babylon-io-table german
  " Rette ~S")

(defbabylon-entry there-is-no-current-kb-str babylon-io-table  german
  "Es gibt keine aktuelle Wissensbasis.")


(defbabylon-entry unknown-eval-type-error-fstr babylon-io-table german
  "~%Typ der Anfrage ~S ist unbekannt")

(defbabylon-entry unknown-eval-mode-error-fstr babylon-io-table german
  "~%~S ist unzulaessiger Modus fuer ~S: ~S")

(defbabylon-entry meta-proc-trace-fstr babylon-io-table german
  " META <- ~S: ~S ~S")


(defbabylon-entry trace-on-fstr babylon-io-table german
  " ~A Trace ON ")

(defbabylon-entry trace-off-fstr babylon-io-table german
  " ~A Trace OFF ")


(defbabylon-entry meta-help-item-list babylon-io-table german
  '((" Warum ? " why)
    (" Unterbrechung " interrupt)
    (" LISP " lisp)))

(defbabylon-entry help-str babylon-io-table german
  "  H I L F E  ")

(defbabylon-entry possible-answers babylon-io-table german
  '((ja . yes)
    (j . yes)
    (nein . no)
    (n . no)
    (unbekannt . unknown)
    (u . unknown)
    (? . help)
    (h . help)
    (hilfe . help)   
    (anfordern . prompt)
    (p . prompt)
    ))


(defbabylon-entry star-str babylon-io-table german
  (make-string-of-length 71 "*"))

(defbabylon-entry no-select-str babylon-io-table german 
  "Keine Auswahl")

(defbabylon-entry mult-choose-header-str babylon-io-table german 
  "Waehle mehrere Eintraege aus")

(defbabylon-entry illegal-choice-fstr babylon-io-table german 
  "Unzulaessige Auswahl")

(defbabylon-entry unknown-operation-fstr babylon-io-table german 
  "~S unbekannte Operation fuer ~S")

(defbabylon-entry restart-kb-fstr babylon-io-table german
  "Restart WB ~S")

(defbabylon-entry notify-on-select-fstr babylon-io-table german 
  "~&===> Aktuelle WB: ~S~%~A")


(defbabylon-entry use-kb-fstr babylon-io-table german 
  "Werde die Wissensbasis ~S benutzen, die nicht aktuell ist. In Ordnung? ")

(defbabylon-entry unwanted-kb-fstr babylon-io-table german 
  "~S unerwuenscht")


(defbabylon-entry kb-does-not-exist-fstr babylon-io-table german
  "Die Wissensbasis ~S existiert noch nicht." )


(defbabylon-entry kb-exists-fstr babylon-io-table german
  "Eine Wissensbasis ~S vom Typ ~S existiert bereits.~%Soll sie verwendet werden? ")

(defbabylon-entry enter-file-fstr babylon-io-table  german 
  "Geben Sie einen Dateinamen ein: ")


(defbabylon-entry kb-of-wrong-type-str babylon-io-table german 
  "Aktuelle WB vom falschen Typ")

