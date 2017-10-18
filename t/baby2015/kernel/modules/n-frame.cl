;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;;           Copyright   1988, 1987, 1986, 1985 and 1984    BY
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;;  AUTHOR:  J. Walther

;;;  system module: normal-frame-mixin : n-frame

(bab-require 'mini-frame-mixin)

(cc-load "frames^normal>act-vals")
(cc-load "frames^normal>nf-proc")
(cc-load "frames^normal>nf-mixin")

(bab-provide 'normal-frame-mixin)

;;; eof

