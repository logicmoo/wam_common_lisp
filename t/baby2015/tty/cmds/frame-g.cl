;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Base: 10. ;Package: BABYLON -*-

(in-package "BABYLON")

;;------------------------------------------------------------------------
;;          deutsche kommandos fuer micro-frame-mixin
;;------------------------------------------------------------------------


(defbabylon-entry frame cmd-table german
  '(" Objekt-Operationen " :value (:open-menu :frame) 
    :documentation "Ein Menue von Objekt-Operationen."))

(defbabylon-entry frame2 cmd-table german
  '(" Objekt-Operationen " :value (:toggle-menu :frame :top) 
    :documentation "Ein Menue von Objekt-Operationen."))

(defbabylon-entry frame-commands cmd-table german 
  '(("Inspiziere Frames" :value :inspect-frames 
     :documentation "Inspiziere einen Frame.")
    ("Inspiziere Instanzen" :value :inspect-instances 
     :documentation "Inspiziere eine Instanz.")))

;; eof
