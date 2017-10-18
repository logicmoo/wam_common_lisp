;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;;           Copyright   1988, 1987, 1986, 1985 and 1984    BY
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;;  AUTHOR:  J. Walther

;;;  system module: basic-rule-mixin : b-rule

(cc-load "rules^basic>rp-tab-e")
(cc-load "rules^basic>rp-tab-g")

(cc-load "rules^basic>rules")
(cc-load "rules^basic>data")
(cc-load "rules^basic>br-inter")
(cc-load "rules^basic>br-proc")
(cc-load "rules^basic>br-mixin")

(bab-provide 'basic-rule-mixin)

#+(and :CCL (not :MCL)) (cc-load "mac^tools>basic-rule-add-on")
#+:MCL (cc-load "mac^tools>basic-rule-add-on-clos")

;;; eof

