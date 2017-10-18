;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;; Babylon site customs File for the Macintosh


(setf *help-key*          #\?)
(setf *c-help-key*        #\escape)
(setf *end-key*           #\return)
(setf *item-width*        50)
(setf *max-menu-entries*  20)


;;; use Load Axset from the Prolog menu, otherwise you do not have an
;;; editor buffer and you can not use goto from the Explore Axset dialog

;(when (flavorp 'basic-prolog-mixin)
;  (load "babylon;samples:axsets:set-ax"))

;;; eof

