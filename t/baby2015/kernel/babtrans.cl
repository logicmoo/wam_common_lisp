;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1988    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHORS:  J.Walther, E. Gross
;; DATE:     April 1994, June 1988


;; complete the following list in your load file
;; by setting "babhome^" z.B.
;; (defbabylon-translation "babhome^" "babylon>" "babhome^bin>")
;; and adding names for interface-modules

;;------------------------------------------------------------------------
;;          logical pathnames
;;------------------------------------------------------------------------

(defbabylon-translation "kernel^"  "babhome^kernel>")

(defbabylon-translation "common^"  "kernel^common>")
(defbabylon-translation "meta^"    "kernel^meta>")

(defbabylon-translation "freetext^" "kernel^freetext>")
(defbabylon-translation "frames^"   "kernel^frames>")
(defbabylon-translation "consat^"   "kernel^consat>")
(defbabylon-translation "rules^"    "kernel^rules>")
(defbabylon-translation "prolog^"   "kernel^prolog>")

(defbabylon-translation "modules^" "kernel^modules>")
(defbabylon-translation "patches^" "kernel^patches>")

(defbabylon-translation "samples^" "babhome^samples>")
(defbabylon-translation "configs^" "samples^configs>")
(defbabylon-translation "axsets^"  "samples^axsets>")
(defbabylon-translation "kbs^"     "samples^kbs>")

;;------------------------------------------------------------------------
;;           short names for modules
;;------------------------------------------------------------------------

(defbabylon-translation "free-text-mixin" "freetext")

(defbabylon-translation "basic-frame-mixin"  "b-frame")
(defbabylon-translation "mini-frame-mixin"   "m-frame")
(defbabylon-translation "normal-frame-mixin" "n-frame")

(defbabylon-translation "basic-constraint-mixin"  "b-consat")
(defbabylon-translation "mini-constraint-mixin"   "m-consat")
(defbabylon-translation "normal-constraint-mixin" "n-consat")

(defbabylon-translation "basic-rule-mixin"  "b-rule")
(defbabylon-translation "mini-rule-mixin"   "m-rule")
(defbabylon-translation "normal-rule-mixin" "n-rule")

(defbabylon-translation "basic-prolog-mixin"  "b-prolog")
(defbabylon-translation "mini-prolog-mixin"   "m-prolog")
(defbabylon-translation "normal-prolog-mixin" "n-prolog")

;;;  eof
