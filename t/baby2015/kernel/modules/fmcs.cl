;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;; mcs (old flavor oriented version)

;;; META CLASS SYSTEM
;;; copy this file into one of your modules folders
;;; _______________________________________________________________________


(cc-load "fmcs^mcs-core")
(cc-load "fmcs^mcs-root")
(cc-load "fmcs^mcs-meth")
(cc-load "fmcs^mcs-util")
(cc-load "fmcs^mcs-map")    ; mapping babylon flavor primitives to fmcs

(pushnew :FMCS *features*)

;;; mcs does not allow slot access by name
;;; you must use slot-value 

;;; eof

