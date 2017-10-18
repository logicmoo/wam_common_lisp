;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;;           Copyright   1988, 1987, 1986, 1985 and 1984    BY
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;;  AUTHOR:  J. Walther

;;;  system module: basic-prolog-mixin : b-prolog

(cc-load "prolog^basic>pp-tab-e")
(cc-load "prolog^basic>pp-tab-g")

(cc-load "prolog^basic>axioms")
(cc-load "prolog^basic>ax-sc")
(cc-load "prolog^basic>bp-inter")
(cc-load "prolog^basic>bp-preds")
(cc-load "prolog^basic>bp-proc")
(cc-load "prolog^basic>bp-mixin")

(bab-provide 'basic-prolog-mixin)

#+(and :CCL (not :MCL)) (cc-load "mac^tools>basic-prolog-add-on")
#+:MCL (cc-load "mac^tools>basic-prolog-add-on-clos")

;;; eof

