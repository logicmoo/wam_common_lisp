;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;;           Copyright   1988, 1987, 1986, 1985 and 1984    BY
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;;  AUTHOR:  J. Walther

;;;  system module: basic-frame-mixin : b-frame

(cc-load "frames^basic>fp-tab-e")
(cc-load "frames^basic>fp-tab-g")

(cc-load "frames^basic>frames")
(cc-load "frames^basic>fr-core")
(cc-load "frames^basic>bf-inter")
(cc-load "frames^basic>bf-proc")
(cc-load "frames^basic>bf-mixin")

(bab-provide 'basic-frame-mixin)

#+(and :CCL (not :MCL)) (cc-load "mac^tools>frame-add-on")
#+:MCL (cc-load "mac^tools>frame-add-on-clos")

#+(or :CCL-1.3 :MCL)(when (y-or-n-p "~&Load graphic frame browser?")
                      #+:CCL-1.3(cc-load "mac^tools>babgrapher")
                      #+:MCL(cc-load "mac^tools>babgrapher-clos"))

;;; eof

