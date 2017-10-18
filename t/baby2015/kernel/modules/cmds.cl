;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;;           Copyright   1988, 1987, 1986, 1985 and 1984    BY
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;;  AUTHOR:  J. Walther

;;;  system module: cmds : cmds

(cc-load "io^cmds>common-e")
(cc-load "io^cmds>frame-e")
(cc-load "io^cmds>consat-e")
(cc-load "io^cmds>rule-e")
(cc-load "io^cmds>prolog-e")

(cc-load "io^cmds>common-g")
(cc-load "io^cmds>frame-g")
(cc-load "io^cmds>consat-g")
(cc-load "io^cmds>rule-g")
(cc-load "io^cmds>prolog-g")

(bab-provide 'cmds)

;;; eof

