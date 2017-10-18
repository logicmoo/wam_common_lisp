;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;;           Copyright   1988, 1987, 1986, 1985 and 1984    BY
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;;  AUTHOR:  J. Walther

;;;  system module: mini-constraint-mixin : m-consat

(bab-require 'basic-constraint-mixin)

(cc-load "consat^mini>mc-trace")
(cc-load "consat^mini>mc-proc")
(cc-load "consat^mini>mc-mixin")

(bab-provide 'mini-constraint-mixin)

;;; eof

