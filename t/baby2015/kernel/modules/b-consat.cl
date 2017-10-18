;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;;           Copyright   1988, 1987, 1986, 1985 and 1984    BY
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;;  AUTHOR:  J. Walther

;;;  system module: basic-constraint-mixin : b-consat

(bab-require 'common)

(cc-load "consat^basic>cp-tab-e")
(cc-load "consat^basic>cp-tab-g")
(cc-load "consat^basic>bc-fns")
(cc-load "consat^basic>primcstr")
(cc-load "consat^basic>cstrnet")
(cc-load "consat^basic>net-prop")
(cc-load "consat^basic>cstrbase")
(cc-load "consat^basic>bc-proc")
(cc-load "consat^basic>bc-mixin")

(bab-provide 'basic-constraint-mixin)
 
#+(and :CCL (not :MCL)) (cc-load "mac^tools>consat-add-on")
#+:MCL (cc-load "mac^tools>consat-add-on-clos")

;;; eof

