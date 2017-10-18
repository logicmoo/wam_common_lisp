;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Base: 10. ;Package: BABYLON -*-

(in-package "BABYLON")

;;------------------------------------------------------------------------
;;          englische kommandos fuer micro-frame-mixin
;;------------------------------------------------------------------------




(defbabylon-entry frame cmd-table english
  '(" Object Operations " :value (:open-menu :frame)
    :documentation "A menu of object operations."))

(defbabylon-entry frame2 cmd-table english
  '(" Object Operations " :value (:toggle-menu :frame :top)
    :documentation "A menu of object operations."))

(defbabylon-entry frame-commands cmd-table english
  `(("Inspect Frames" :value :inspect-frames
     :documentation "Inspect a frame.")
    ("Inspect Instances" :value  :inspect-instances
     :documentation "Inspect an instance.")))

;; eof
