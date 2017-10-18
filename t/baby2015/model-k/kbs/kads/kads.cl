;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;; revised Juergen Walther 17. 6.94

;;; prepare for loading

(unless (find-translation "model-k^" 'source)
  (defbabylon-translation "model-k^" "babhome^model-k>")
  (pushnew "model-k^configs>" *babylon-module-search-path* :test #'string-equal)
  'model-k)

#-:MCL
(progn
  (defvar *babylon-kbs* nil)
  (setf *babylon-kbs* "model-k^kbs>kads>")
  
  (defun include-kb-file (file &key (bufferp t))
    (declare (ignore bufferp))
    (cc-load (concatenate 'string *babylon-kbs* file) :recompile *recompile*))
  )

;;; file for loading the KADS knowledge base

(if (y-or-n-p "Dummy Interface Configuration for KADS? ")
  (def-kb-instance KADS KADSD)
  (def-kb-instance KADS KADSC))

(setf *kads-editor-buffer-p* 
      (y-or-n-p "Editor buffers for KADS? "))

(send-fp :set-frcheck t)

(include-kb-file "basics"  :bufferp *kads-editor-buffer-p*)
(include-kb-file "errors"  :bufferp *kads-editor-buffer-p*)
(include-kb-file "mixins"  :bufferp *kads-editor-buffer-p*)
(include-kb-file "domain"  :bufferp *kads-editor-buffer-p*)
(include-kb-file "infere"  :bufferp *kads-editor-buffer-p*)
(include-kb-file "task"    :bufferp *kads-editor-buffer-p*)
(include-kb-file "tracing" :bufferp *kads-editor-buffer-p*)

(send-kb :export-kb)

;;; eof

