;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;; Definition des floor-plan layers
;;; revised Juergen Walther 17. 6.94
;;; Ralf Schukey

;;; prepare for loading

(unless (find-translation "model-k^" 'source)
  (defbabylon-translation "model-k^" "babhome^model-k>")
  (pushnew "model-k^configs>" *babylon-module-search-path* :test #'string-equal)
  'model-k)

#-:MCL 
(defvar *babylon-kbs* "model-k^kbs>floor>")

(eval-when (eval load compile)
  (unless (member 'KADS *known-knowledge-bases*)
    (let ((working-directory *babylon-kbs*))
      (setf *babylon-kbs* (transform-pathstring "model-k^kbs>kads>kads" 'source))
      (cc-load "model-k^kbs>kads>kads")
      (setf *babylon-kbs* working-directory))))

;;;

(setf *floor-editor-buffer-p* 
      (y-or-n-p "Editor buffers for FLOOR? "))

(DEF-KB-INSTANCE floor floorc :pkg :KADS)

;; the kb uses the definitions of KADS knowledge base

(send-kb :import-kb KADS)
(send-fp :set-frcheck t)

(include-kb-file "clauses"           :bufferp *floor-editor-buffer-p*)
(include-kb-file "basics"            :bufferp *floor-editor-buffer-p*)
(include-kb-file "enumeras"          :bufferp *floor-editor-buffer-p*)
(include-kb-file "rooms"             :bufferp *floor-editor-buffer-p*)
(include-kb-file "employee"          :bufferp *floor-editor-buffer-p*)
(include-kb-file "relation"          :bufferp *floor-editor-buffer-p*)
(include-kb-file "conditio"          :bufferp *floor-editor-buffer-p*)
(include-kb-file "occupancy"         :bufferp *floor-editor-buffer-p*)
(include-kb-file "requires"          :bufferp *floor-editor-buffer-p*)
(include-kb-file "mclasses"          :bufferp *floor-editor-buffer-p*)
(include-kb-file "ksources"          :bufferp *floor-editor-buffer-p*)
(include-kb-file "tasks"             :bufferp *floor-editor-buffer-p*)

;;; (initialize-task-layer)

(instructions 
 (initialize-task-layer)
 (<- plan-office :activate T))

;;; eof