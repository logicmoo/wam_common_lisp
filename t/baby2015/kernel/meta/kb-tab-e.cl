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


(defbabylon-table babylon-io-table english)


(defbabylon-entry type-end-to-confirm-str babylon-io-table english
  "[Type ~:C to confirm] ")

(defbabylon-entry type-end-to-continue-str babylon-io-table english
  "[Type ~:C to continue] ")

(defbabylon-entry select-to-confirm-str babylon-io-table english
  "[Select to confirm] ")

(defbabylon-entry select-to-continue-str babylon-io-table english
  "[Select to continue] ")

(defbabylon-entry confirm-str babylon-io-table english
  " Confirm: ")

(defbabylon-entry quit-item babylon-io-table english
  '(" Quit:   x " :value :ok))

(defbabylon-entry do-nothing-str babylon-io-table english
  " Do Nothing ")

(defbabylon-entry none-kb-current-str babylon-io-table english
  " None KB current ")

(defbabylon-entry none-kb-known-str babylon-io-table english
  " None KB known ")

(defbabylon-entry current-kb-fstr babylon-io-table english
  "Current KB: ~S")

(defbabylon-entry choose-kb-str babylon-io-table english
   " Choose a KB ")

(defbabylon-entry kill-kb-fstr babylon-io-table english
    " Kill ~S ")

(defbabylon-entry run-loop-str babylon-io-table english
    " Enter Messages via Lisp Listener")

(defbabylon-entry file-name-with-default-fstr babylon-io-table  english
  "Enter Filename (Default ~A):")


(defbabylon-entry print-info-about-fstr babylon-io-table  english
  " Print informations about ~S")


(defbabylon-entry reset-kb-fstr babylon-io-table  english
  " Reset ~S")

(defbabylon-entry start-fstr babylon-io-table  english
  " Start ~S")

(defbabylon-entry starting-kb-fstr babylon-io-table  english
   "~2%~10T******  STARTING ~S  ****** ~2%")



(defbabylon-entry state-of-kb-fstr babylon-io-table  english
  "~2%****** State of Knowledge Base ~S ****** ~%")

(defbabylon-entry source-file-fstr babylon-io-table  english
  "~%- Source File(s): ~38T~{~A  ~}")



(defbabylon-entry enter-name-of-file-for-kb-fstr babylon-io-table  english
  "Give the name of the file where to save ~S [DEFAULT = ~A] ")


(defbabylon-entry kb-declaration-fstr babylon-io-table  english
  "~2%;; ************* KNOWLEDGE BASE  DECLARATION ***********~2%")

(defbabylon-entry instructions-fstr babylon-io-table  english
  "~2%;; ********* I N S T R U C T I O N S ************~2%")


(defbabylon-entry save-fstr babylon-io-table english
  " Save ~S")

(defbabylon-entry there-is-no-current-kb-str babylon-io-table  english
  "There is no current knowledge base.")


(defbabylon-entry unknown-eval-type-error-fstr babylon-io-table english
  "~%request ~S of unknown type")

(defbabylon-entry unknown-eval-mode-error-fstr babylon-io-table english
  "~%~S wrong mode for ~S: ~S")

(defbabylon-entry meta-proc-trace-fstr babylon-io-table english
  " META <- ~S: ~S ~S")


(defbabylon-entry trace-on-fstr babylon-io-table english
  " ~A Trace ON ")

(defbabylon-entry trace-off-fstr babylon-io-table english
  " ~A Trace OFF ")


(defbabylon-entry meta-help-item-list babylon-io-table english
  '((" Why ? " why)
    (" Interrupt " interrupt)
    (" LISP " lisp)))

(defbabylon-entry help-str babylon-io-table english
  "  H E L P  ")

(defbabylon-entry possible-answers babylon-io-table english
  '((yes . yes)
    (y . yes)
    (no . no)
    (n . no)
    (unknown . unknown)
    (u . unknown)
    (? . help)
    (h . help)
    (prompt . prompt)
    (p . prompt)
    ))


(defbabylon-entry star-str babylon-io-table english 
  (make-string 71 :initial-element #\*))

(defbabylon-entry no-select-str babylon-io-table english
  "no selection")

(defbabylon-entry mult-choose-header-str babylon-io-table english   
  "Select Several Items")

(defbabylon-entry illegal-choice-fstr babylon-io-table english
  "illegal choice")

(defbabylon-entry unknown-operation-fstr babylon-io-table english
  "~S Unknown Operation for ~S")

(defbabylon-entry restart-kb-fstr babylon-io-table english 
  "Restart KB ~S")

(defbabylon-entry notify-on-select-fstr babylon-io-table english
  "~&===> Current KB: ~S~%~A")


(defbabylon-entry use-kb-fstr babylon-io-table english
  "I will use ~S which is not current. OK? ")

(defbabylon-entry unwanted-kb-fstr babylon-io-table english
  "~S unwanted")


(defbabylon-entry kb-exists-fstr babylon-io-table english
  "A knowledge base ~S of type ~S already exists.~%Do you want to use it? ")


(defbabylon-entry kb-does-not-exist-fstr babylon-io-table english
  "The knowledge base ~S does not yet exists." )


(defbabylon-entry enter-file-fstr babylon-io-table  english
  "Enter Filename: ")


(defbabylon-entry kb-of-wrong-type-str babylon-io-table english
  "Current KB of wrong type")

