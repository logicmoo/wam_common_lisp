;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")



(unless (find-translation "texi^" 'source)
  (defbabylon-translation "texi^" "babhome^texi>"))

(pushnew "texi^configs>" *babylon-module-search-path* :test #'string-equal)

(defbabylon-translation "dummy-interface-mixin"  "d-interf")
(defbabylon-translation "import-export-mixin"  "imexport")

(bab-provide 'textrans)

;; patch
(defmacro define-possible-values-behavior
	  ((frame-name method-name) lambda-list form)  
  `(progn 
     (signal-unknown-frame ',frame-name 
			     "POSSIBLE VALUES BEHAVIOR"
			     '(,frame-name ,method-name))
     (define-possible-values-method
       (,(%get-object-name frame-name) ,method-name)
     ,lambda-list
     ,form)))

(cc-load "texi^configs>k1dummyc")
(cc-load "texi^configs>imexport")
(cc-load "texi^configs>d-interf")
(cc-load "texi^configs>k1c")

(unless (find-package :ks)
  (make-package :ks))

(cc-load "texi^k1>unit-kb") ; :recompile nil)

(if (y-or-n-p "load KB Task&Agendas ?")
  (cc-load "texi^k1>tasks" ; :recompile nil
           ))

(if (y-or-n-p "load KB top-down-refine ?")
  (cc-load "texi^k1>topdown" ; :recompile nil
           ))

(bab-provide 'k1)

;;; eof

