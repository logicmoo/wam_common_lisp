;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;;           Copyright   1989 1988
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;;  AUTHOR:  E. Gross

;;;  tty module: basic-interface-mixin : b-interf

(cc-load "tty^basic>tty-menu")
(cc-load "tty^basic>t-dialog")
(cc-load "tty^basic>b-txscw")
(cc-load "tty^basic>b-mixin")

(bab-provide 'basic-interface-mixin)