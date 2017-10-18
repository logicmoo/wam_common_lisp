;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;;           Copyright   1988, 1987, 1986, 1985 and 1984    BY
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;;  AUTHOR:  J. Walther

;;;  system module: mini-frame-mixin : m-frame

(bab-require 'basic-frame-mixin)

(cc-load "frames^mini>pos-vals")
(cc-load "frames^mini>ask-supp")
(cc-load "frames^mini>mf-proc")
(cc-load "frames^mini>mf-mixin")

(bab-provide 'mini-frame-mixin)

;;; eof

