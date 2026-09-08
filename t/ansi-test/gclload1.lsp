#+ecl (si::package-lock (find-package "COMMON-LISP") nil)
#+(and ecl (not ecl-bytecmp)) (require :cmp)
#+(and ecl (not ecl-bytecmp)) (setq c:*suppress-compiler-messages*
                                    '(or c:compiler-warning
                                         c:compiler-note
                                         style-warning))
#+:armedbear (require 'pprint)
#+cmu
(progn
  (setq ext:*gc-verbose* nil)
  ;; Set *default-pathname-defaults* to include the full path to this
  ;; file.  This is needed for the tests so that they'll be loaded
  ;; correctly from the subdirectories.
  (setf *default-pathname-defaults*
	(make-pathname :name nil :type nil :defaults *load-truename*)))

#+gcl (setq compiler:*suppress-compiler-notes* t
            compiler:*suppress-compiler-warnings* t
            compiler:*compile-verbose* nil
            compiler:*compile-print* nil)

#+lispworks
(progn
  ;; This is required for test make-array.28 in ./arrays/make-array.lsp,
  ;; because it generates a string to read a form which is 
  ;; (1- (min 10000 array-rank-limit)) nested.  The stack is full of pairs
  ;; of SYSTEM::READ-LIST and  SYSTEM::READ-MAYBE-NOTHING calling each other. 
  ;; 
  (unless (> (hcl:current-stack-length) 150000)
    (let ((amount (or #+lispworks-32bit 500
                      #+lispworks-64bit 3000)))
      (when amount
        (hcl:extend-current-stack amount))))

  ;; The tests efectively assume the default character element type is character
  (lw:set-default-character-element-type 'character)

  (pushnew :utf-8 sys:*specific-valid-file-encodings*)

  (setq system:*file-length-error-p* :error)
  (setq system:*right-paren-whitespace* nil)

  )

#+clisp (setq custom::*warn-on-floating-point-contagion* nil)

;;; Configure logical pathnames
(defvar *aux-dir*
  (merge-pathnames "auxiliary/"
                   (make-pathname
                    :directory
                    (pathname-directory *load-truename*))))

(let (*load-verbose* *load-print* *compile-verbose* *compile-print*)
  (load "compile-and-load.lsp"))

(let (*load-verbose* *load-print* *compile-verbose* *compile-print*)
  (load "rt-package.lsp")
  (compile-and-load "rt.lsp")
  (load "cl-test-package.lsp")
  (in-package :cl-test)
  (compile-and-load* "ansi-aux-macros.lsp")
  (handler-bind
   #-sbcl ()
   #+sbcl ((sb-ext:code-deletion-note #'muffle-warning))
   (load "universe.lsp"))
  (compile-and-load* "random-aux.lsp")
  (compile-and-load* "ansi-aux.lsp")
  
  (load "cl-symbol-names.lsp")
  (load "notes.lsp"))

(setq *compile-verbose* nil
      *compile-print* nil
      *load-verbose* nil)

