;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

; these parts belong to the KADS - Model but due to technical reasons it must be loaded
; in this directory


(DEF-CLAUSE-SET functional-relationships     ; the properties of a relation is exclusive

  ;;; *** transitivity of a 2 argmented relation ***

  ((trans _relation _argx _argy) <- (_relation _argx _argy))

  ((trans _relation _argx _argz)
   <- 
   (_relation _argx _argy)
   (trans _relation _argy _argz))

  ((trans _relation _argx _argy _more) <- (_relation _argx _argy _more))

  ((trans _relation _argx _argz _more)
   <- 
   (_relation _argx _argy _more)
   (trans _relation _argy _argz _more))


  ;;; *** symetry of a relation ***
  
  ((sym _relation _argx _argy) <- (_relation _argx _argy))
  ((sym _relation _argx _argy) <- (_relation _argy _argx))

  ((sym _relation _argx _argy _more) <- (_relation _argx _argy _more))
  ((sym _relation _argx _argy _more) <- (_relation _argy _argx _more)))

;;; eof

