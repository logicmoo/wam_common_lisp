;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")
;;
;; mixin for BABYLON if you want to use kbs of different packages
;;

(def$flavor import-export-mixin
  ()
  ()
  (:required-instance-variables constraint-processor 
                                constraints 
                                constraint-nets)
  (:required-flavors basic-frame-mixin 
                     basic-constraint-mixin 
                     basic-rule-mixin))

;;; --- import part -----------------------------------------

(def$method (import-export-mixin :frame-import) (kb)
  (when ($send kb :operation-handled-p :frame-processor)
    (use-package ($send kb :pkg) ($send self :pkg))
    (dolist (a-frame ($send kb :frames))
      ($send self :add-to-frames a-frame))
    (dolist (an-instance ($send kb :instances))
      ($send self :add-to-instances an-instance))))

(def$method (import-export-mixin :constraint-import) (kb)
  (when ($send kb :operation-handled-p :constraint-processor)    
    (setf constraints (append constraints ($send kb :constraints)))
    (setf  constraint-nets (append constraint-nets ($send kb :constraint-nets)))
    ($send constraint-processor :set-constraints constraints)
    ($send constraint-processor :set-constraint-nets constraint-nets)))

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
  (let ((pkg ($send self :pkg))
        (frames ($send self :frames))
        (instances ($send self :instances)))
  (export (mapcar #'(lambda (symb)
                      (find-symbol (string symb) pkg))
                  (append frames instances)) 
          pkg)))


(defun export-objects ()
  (send-kb :export-kb))


;;; eof

