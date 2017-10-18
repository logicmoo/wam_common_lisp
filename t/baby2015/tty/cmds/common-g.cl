;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Base: 10. ;Package: BABYLON -*-

(in-package "BABYLON")
   
;;------------------------------------------------------------------------
;;      deutsche kommandos
;;------------------------------------------------------------------------

(defbabylon-table cmd-table german :size 100)

(defbabylon-entry system-commands cmd-table german	  
  '((" Globale Operationen " :no-select t
     #+:LISPM :font #+:LISPM fonts:cptfontb)
    (" Status " :value :system-status)
    (" Selektiere die aktuelle WB " :value :select-current-kb
     :documentation "Selektiere die aktuelle Wissensbasis.")
    (" Selektiere eine WB " :value :select-any-kb
     :documentation "Selektiere irgendeine der geladenen Wissensbasen.")
    (" Lade eine Datei " :value :load-any-file 
     :documentation "Lade eine Datei.")
    (" Loesche eine WB " :value :kill-any-kb
    :documentation "Loesche irgendeine der geladenen Wissensbasen.")))

(defbabylon-entry exit-entry cmd-table german 
  '(" Exit " :value :exit))

(defbabylon-entry suspend-entry cmd-table german 
  '(" Suspend " :value :suspend))

(defbabylon-entry explore-any-command cmd-table german 
  '(" Erkunde eine WB " :value :explore-any-kb
    :documentation "Erkunde irgendeine der geladenen Wissensbasen."))

(defbabylon-entry dummy-entry cmd-table german 
  '( "" :no-select t))

(defbabylon-entry submenu-entry cmd-table german 
  `(("------------------------" :no-select t
     #+:LISPM :font #+:LISPM fonts:cptfontb)
    (" Submenu " :value :close-menu
     :documentation "Schliesse das momentane Untermenue."
     #+:LISPM :font #+:LISPM fonts:cptfontb)
    ("------------------------" :no-select t
     #+:LISPM :font #+:LISPM fonts:cptfontb)))

 
(defbabylon-entry kb-commands cmd-table german 
  '((" WB Operationen " :no-select t
     #+:LISPM :font #+:LISPM fonts:cptfontb)
    (" Starte aktuelle WB " :value :start-kb-confirmed
     :documentation "Starte die Aktuelle Wissensbasis.")
    (" Setze aktuelle WB zurueck " :value :reset-kb-confirmed
     :documentation "Setze die Aktuelle Wissensbasis zurueck.")
    (" Schalte System Trace um " :value :toggle-system-trace
     :documentation "Schalte System Trace ein oder aus.")
    (" " :no-select t)
    (" Beschreibe aktuelle WB " :value :describe-kb
     :documentation "Liste Informationen ueber die Aktuelle Wissensbasis.")))

(defbabylon-entry explore-current-command cmd-table german 
  '(" Erkunde aktuelle WB " :value :explore-kb
    :documentation "Erkunde die Aktuelle Wissensbasis."))

;;------------------------------------------------------------------------
