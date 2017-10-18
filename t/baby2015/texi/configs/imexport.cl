;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;
;; mixin for BABYLON if you want to use kbs of different packages
;;

(def$flavor import-export-mixin
  ()
  ())
 
;;; --- import part -----------------------------------------

(def$method (import-export-mixin :frame-import) (kb)
  (when ($send kb :operation-handled-p :frame-processor)
    (use-package ($send kb :pkg) ($send self :pkg))
    (dolist (a-frame ($send kb :frames))
      ($send self :add-to-frames a-frame))
    (dolist (an-instance ($send kb :instances))
      ($send self :add-to-instances an-instance))))


(def$method (import-export-mixin :constraint-import) (kb)
  (let ((cstr-proc ($send kb :send-if-handles :constraint-processor)))
    (when cstr-proc
      (let ((cstrs (append ($send self :constraints)
                          ($send kb :constraints)))
            (cstr-nets (append ($send self :constraint-nets)
                               ($send kb :constraint-nets))))
        ($send self :set-constraints cstrs)
        ($send self :set-constraint-nets cstr-nets)
        ($send cstr-proc :set-constraints cstrs)
        ($send cstr-proc :set-constraint-nets cstr-nets)))))

(def$method (import-export-mixin :rule-import) (kb)
  (when ($send kb :operation-handled-p :rule-processor)    
    (dolist (a-rule-set ($send kb :rules))
      ($send self :add-to-rules a-rule-set))))


(def$method (import-export-mixin :import-kb) (kb)
  (if ($send self :operation-handled-p :loaded)            ; import file references
    (dolist (file ($send kb :send-if-handles :file-name))
      ($send self :loaded file)))
  (if ($send self :operation-handled-p :frame-processor)
      ($send self :frame-import kb))
  (if ($send self :operation-handled-p :constraint-processor)
      ($send self :constraint-import kb))
  (if ($send self :operation-handled-p :rule-processor)
      ($send self :rule-import kb)))

(defun import-kb (kb)
  (send-kb :import kb))


;;; --- export part -----------------------------------------


(def$method (import-export-mixin :export-kb) ()
  (when ($send self :operation-handled-p :frame-processor)
    (let ((pkg ($send self :pkg))
          (frames ($send self :frames))
          (instances ($send self :instances)))
      (export (mapcar #'(lambda (symb)
                          (find-symbol (string symb) pkg))
                      (append frames instances)) 
              pkg))))


(defun export-objects ()
  (send-kb :export-kb))


;;; eof

