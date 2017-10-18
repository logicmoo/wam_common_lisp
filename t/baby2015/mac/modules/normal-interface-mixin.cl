;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;; normal-interface-mixin

#+(and :CCL (not :MCL))
(progn
  (cc-load "mac^io>normal>menu") 
  (cc-load "mac^io>normal>window") 
  (cc-load "mac^io>normal>interface")
  (cc-load "mac^io>normal>menubar"))

#+:MCL
(progn
  (cc-load "mac^io>normal>menu-clos") 
  #+:CCL-3(cc-load "mac^io>normal>fred-window-comp")
  (cc-load "mac^io>normal>window-clos") 
  (cc-load "mac^io>normal>interface-clos")
  (cc-load "mac^io>normal>menubar-clos"))

;;; eof

 
