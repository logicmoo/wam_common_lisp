;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;;           Copyright   1988, 1987, 1986, 1985 and 1984    BY
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;;  AUTHOR:  J. Walther

;;;  system module: mini-prolog-mixin : m-prolog

(bab-require 'basic-prolog-mixin)

(cc-load "prolog^mini>mp-preds")
(cc-load "prolog^mini>mp-trace")
(cc-load "prolog^mini>mp-proc")
(cc-load "prolog^mini>mp-mixin")

(bab-provide 'mini-prolog-mixin)

;;; eof

