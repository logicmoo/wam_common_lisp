;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Base: 10. ;Package: BABYLON -*-

(in-package "BABYLON")

;;------------------------------------------------------------------------
;;          englische kommandos
;;------------------------------------------------------------------------



(defbabylon-table cmd-table english :size 100)

   
(defbabylon-entry system-commands cmd-table english
  '((" Global Operations " :no-select t
     #+:LISPM :font #+:LISPM fonts:cptfontb)
    (" Status " :value :system-status)
    (" Select Current KB " :value :select-current-kb
     :documentation "Select the Current Knowledge Base.")
    (" Select any KB " :value :select-any-kb
    :documentation "Select a Knowledge Base.")
    (" Load any File " :value :load-any-file
    :documentation "Load a File.")
    (" Kill any KB " :value :kill-any-kb
    :documentation "Kill a Knowledge Base.")))


(defbabylon-entry exit-entry cmd-table english
  '(" Exit " :value :exit))

(defbabylon-entry suspend-entry cmd-table english
  '(" Suspend " :value :suspend))

(defbabylon-entry explore-any-command cmd-table english
  '(" Explore any KB " :value :explore-any-kb
    :documentation "Explore a Knowledge"))

(defbabylon-entry dummy-entry cmd-table english
  '( "" :no-select t))

(defbabylon-entry submenu-entry cmd-table english
  `(("------------------------" :no-select t
     #+:LISPM :font #+:LISPM fonts:cptfontb)
    (" Submenu " :value :close-menu
     :documentation "Closes the current submenu"
     #+:LISPM :font #+:LISPM fonts:cptfontb)
    ("------------------------" :no-select t
     #+:LISPM :font #+:LISPM fonts:cptfontb)))

(defbabylon-entry kb-commands cmd-table english 
  '((" KB Operations " :no-select t
     #+:LISPM :font #+:LISPM fonts:cptfontb)
    (" Start Current KB " :value :start-kb-confirmed
     :documentation "Starts the Current Knowledge Base.")
    (" Reset Current KB " :value :reset-kb-confirmed
     :documentation "Resets the Current Knowledge Base.")
    (" Toggle System Trace " :value :toggle-system-trace
     :documentation "Toggle System Trace.")
    (" " :no-select t)
    (" Describe Current KB " :value :describe-kb
     :documentation "Print some information about this KB.")))


(defbabylon-entry explore-current-command cmd-table english
  '(" Explore Current KB " :value :explore-kb
    :documentation "Explore the Current Knowledge Base."))

;;------------------------------------------------------------------------

