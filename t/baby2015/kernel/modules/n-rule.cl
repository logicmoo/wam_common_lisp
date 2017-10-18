;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;;           Copyright   1988, 1987, 1986, 1985 and 1984    BY
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;;  AUTHOR:  J. Walther

;;;  system module: normal-rule-mixin : n-rule

(bab-require 'mini-rule-mixin)

(cc-load "rules^normal>nr-expl")
(cc-load "rules^normal>nr-devel")
(cc-load "rules^normal>nr-proc")
(cc-load "rules^normal>nr-mixin")

(bab-provide 'normal-rule-mixin)
 
#+(and :CCL (not :MCL)) (cc-load "mac^tools>normal-rule-add-on")
#+:MCL (cc-load "mac^tools>normal-rule-add-on-clos")

;;; eof

