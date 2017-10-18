;;; -*- Mode: Lisp; Base:10; Syntax: Common-Lisp; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   J. W A L T H E R



;;This is the English version of all the strings and menu-item-lists of 
;;the free-text processor. 



(defbabylon-table free-text-io-table english)


;;; **************** syntax ****************


(defbabylon-entry is-it-true-question-fstr free-text-io-table english
  "~5TIs this true: ~{~S ~}")

(defbabylon-entry choose-one-of-str free-text-io-table english
  " Choose one of :")

(defbabylon-entry yes-str free-text-io-table english
  "Yes")

(defbabylon-entry no-str free-text-io-table english
  "No ")

(defbabylon-entry expected-answer-yes-str free-text-io-table english
  '("Expected answer: Yes" :no-select t))

(defbabylon-entry expected-answer-no-str free-text-io-table english
  '("Expected answer: No " :no-select t))

(defbabylon-entry prompt-item free-text-io-table english
  `(" Prompt " :value prompt))

(defbabylon-entry ask-item-list free-text-io-table english
  `((" Yes " :value yes)
    (" No " :value no)
    (""    :no-select t)
    (" -- Help -- " :value help)))

(defbabylon-entry incorrect-answer-str free-text-io-table english
"~&~10TIncorrect answer: ~S~@
   ~10T====>> Look at the mouse, please ! ")

(defbabylon-entry meta-free-text-trace-fstr free-text-io-table english
  " META -> FREE-TEXT ~S  ~S")

