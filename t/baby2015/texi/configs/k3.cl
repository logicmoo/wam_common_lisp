;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")



(unless (find-translation "texi^" 'source)
  (defbabylon-translation "texi^" "babhome^texi>"))

(pushnew "texi^configs>" *babylon-module-search-path* :test #'string-equal)

(bab-require 'k1)

(cc-load "texi^configs>k3dummyc")
(cc-load "texi^configs>k3c")
(cc-load "texi^configs>k3-misc")
(cc-load "texi^configs>k3-print")
(bab-provide 'k3-misc)
(bab-provide 'k3-print)

(cc-load "texi^k3>k3-kb") ; :recompile nil)

(bab-provide 'k3)

;;; eof
