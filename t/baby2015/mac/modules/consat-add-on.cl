;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10  -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther
;;           display constraint

#+(and :CCL (not :MCL)) (cc-load "mac^tools>consat-add-on")
#+:MCL (cc-load "mac^tools>consat-add-on-clos")

(bab-provide 'consat-add-on)

;;; eof

