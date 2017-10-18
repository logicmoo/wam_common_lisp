;;; -*- Mode: Lisp; Base:10; Syntax: Common-Lisp; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   J. W A L T H E R


;;
;; This is the German version of all the strings and menu-item-lists of 
;; the free-text processor. 



(defbabylon-table free-text-io-table german)


;;; **************** syntax ****************


(defbabylon-entry is-it-true-question-fstr free-text-io-table german
  "~5TTrifft es zu: ~{~S ~}")

(defbabylon-entry choose-one-of-str free-text-io-table german
  " Waehle einen Eintrag :")

(defbabylon-entry yes-str free-text-io-table german
  "Ja  ")

(defbabylon-entry no-str free-text-io-table german
  "Nein")

(defbabylon-entry expected-answer-yes-str free-text-io-table german
  '("Erwartet wird : Ja   " :no-select t))

(defbabylon-entry expected-answer-no-str free-text-io-table german
  '("Erwartet wird : Nein " :no-select t))


(defbabylon-entry prompt-str free-text-io-table german 
  "Anfordern ")

(defbabylon-entry ask-item-list free-text-io-table german 
  `((" ja " :value yes)
    (" nein " :value no)
    (""    :no-select t)
    (" -- Help -- " :value help)))

(defbabylon-entry incorrect-answer-str free-text-io-table german 
"~&~10TUnzulaessige Antwort: ~S~@
   ~10T====>> Bitte die Maus beachten !")


(defbabylon-entry meta-free-text-trace-fstr free-text-io-table german
  " META -> FREE-TEXT ~S  ~S")
