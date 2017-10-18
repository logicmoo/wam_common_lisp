;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10  -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther
;;           basic-rule-add-on

(bab-require 'add-on-base)

#+:(and :CCL (not :MCL)) (cc-load "mac^tools>basic-rule-add-on")
#+:MCL (cc-load "mac^tools>basic-rule-add-on-clos")

(bab-provide 'basic-rule-add-on)

;;; eof

