;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;;           Copyright   1988, 1987, 1986, 1985 and 1984    BY
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;;  AUTHOR:  J. Walther

;;;  system module: normal-prolog-mixin : n-prolog

(bab-require 'mini-prolog-mixin)

(cc-load "prolog^normal>np-devel")
(cc-load "prolog^normal>np-expl")
(cc-load "prolog^normal>np-proc")
(cc-load "prolog^normal>np-mixin")

(bab-provide 'normal-prolog-mixin)

;;; eof
