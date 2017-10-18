;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10  -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther
;;           basic-rule-add-on

(bab-require 'basic-rule-add-on)

#+:(and :CCL (not :MCL)) (cc-load "mac^tools>normal-rule-add-on")
#+:MCL (cc-load "mac^tools>normal-rule-add-on-clos")

(bab-provide 'normal-rule-add-on)

;;; eof

