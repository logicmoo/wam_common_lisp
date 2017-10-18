;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

(pushnew "babhome^model-k>configs>" *babylon-module-search-path* :test #'string-equal)

(defbabylon-translation "dummy-interface-mixin"  "d-interf")
(defbabylon-translation "import-export-mixin"  "imexport")

(bab-provide 'textrans)

;;; eof




