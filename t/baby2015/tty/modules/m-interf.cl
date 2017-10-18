;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;;           Copyright   1989 1988
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;;  AUTHOR:  E. Gross

;;;  tty module: mini-interface-mixin : m-interf 

(bab-require 'basic-interface-mixin)

(cc-load "tty^mini>mitem")
(cc-load "tty^mini>mloop")
(cc-load "tty^mini>m-mixin")
(cc-load "tty^mini>sys-core")
(cc-load "tty^mini>mbabylon")

(cc-load "tty^cmds>common-e")
(cc-load "tty^cmds>frame-e")
(cc-load "tty^cmds>consat-e")
(cc-load "tty^cmds>rule-e")
(cc-load "tty^cmds>prolog-e")

(cc-load "tty^cmds>common-g")
(cc-load "tty^cmds>frame-g")
(cc-load "tty^cmds>consat-g")
(cc-load "tty^cmds>rule-g")
(cc-load "tty^cmds>prolog-g")

(bab-provide 'mini-interface-mixin)

;;; eof

