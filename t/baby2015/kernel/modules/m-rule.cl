;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;;           Copyright   1988, 1987, 1986, 1985 and 1984    BY
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;;  AUTHOR:  J. Walther

;;;  system module: mini-rule-mixin : m-rule

(bab-require 'basic-rule-mixin)

(cc-load "rules^mini>mr-trace")
(cc-load "rules^mini>mr-proc")
(cc-load "rules^mini>mr-mixin")

(bab-provide 'mini-rule-mixin)

;;; eof

