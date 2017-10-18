;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User; Base: 10  -*-

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther
;;           dialogs
;;  cascaded sequence dialog item class

(defobject *c-sequence-dialog-item* *sequence-dialog-item*)

(proclaim '(object-variable (*c-sequence-dialog-item* dependents update-function)))

(defobfun (exist *c-sequence-dialog-item*) (init-list)
  (have 'dependents (getf init-list :dependents '()))
  (have 'update-function (getf init-list :update-function #'(lambda () '())))
  (usual-exist 
   (init-list-default init-list
                      :dialog-item-size #@(166 96)
                      :cell-size #@(151 11)
                      :table-hscrollp nil)))

(defobfun (broadcast *c-sequence-dialog-item*) (request)
  (dolist (dependent dependents)
    (ask (symbol-value dependent) (funcall (symbol-function request)))))

(defobfun (deselect *c-sequence-dialog-item*) ()
  (dolist (cell (selected-cells))
    (cell-deselect (point-h cell) (point-v cell))))

(defobfun (reset-table-sequence *c-sequence-dialog-item*) 
          (&optional (table-sequence '()))
  (deselect)
  (broadcast 'reset-table-sequence)
  (set-table-sequence table-sequence))

(defobfun (update-table-sequence *c-sequence-dialog-item*) ()
  (reset-table-sequence (funcall update-function)))


(defobject *bury-dialog* *dialog*)

(defobfun (window-close *bury-dialog*) (&optional real)
  (if real (usual-window-close)
      (window-hide)))

;;; -----------------------------------------------------------------------------

(defobject *string-sequence-dialog* *sequence-dialog-item*)

(defun split-lines (string)
  (with-input-from-string (stream string)
    (do ((m-line (read-line stream nil nil) (read-line stream nil nil))
         (lines))
        ((null m-line) (nreverse lines))
      (push m-line lines))))
             
(defobfun (set-dialog-item-text *string-sequence-dialog*) (string)
  (dolist (cell (selected-cells))                     ; deselect selected cells
    (cell-deselect (point-h cell) (point-v cell)))
  (set-table-sequence (split-lines string)))

(defobfun (get-dialog-item-text *string-sequence-dialog*) ()
  (format nil "~{~A~%~}" (table-sequence)))

;;; -----------------------------------------------------------------------------


(defun babylon-edit (construct)
  "Positions in one of the Editor Buffers of current knowledge base to definition construct"
  (let  ((construct-string (format nil "~S" construct)))
    (dolist (kb-file (send-kb :file-name))
      (let ((fred-buffer (is-known-window kb-file *kb-window*)))
        (if fred-buffer
          (let* ((definitions (ask fred-buffer (ccl::list-definitions)))
                 (position (assoc construct-string definitions :test #'string-equal)))
            (if position
              (ask fred-buffer (ccl::go-to-def position) (window-select)))))))))

(defun babylon-edit-rule (rule-set-name rule-name)
  (let  ((construct-string (format nil "~S" rule-set-name)))
    (dolist (kb-file (send-kb :file-name))
      (let ((fred-buffer (is-known-window kb-file *kb-window*)))
        (if fred-buffer
          (let* ((definitions (ask fred-buffer (ccl::list-definitions)))
                 (position (assoc construct-string definitions :test #'string-equal)))
            (if position
              (ask fred-buffer 
                (ccl::go-to-def position)
                (ccl::wsearch (concatenate 'string "(" (string rule-name)))
                (collapse-selection nil)
                (window-select)
                (window-update)))))))))

(defun babylon-edit-prolog (construct)
  "Positions in one of the Editor Buffers of current knowledge base to definition construct"
  (let  ((construct-string (format nil "~S" construct)))
    (dolist (kb-file (send-kb :file-name))
      (let ((fred-buffer (or (get construct '%editor-buffer)
                             (is-known-window kb-file *kb-window*))))
        (if fred-buffer
          (let* ((definitions (ask fred-buffer (ccl::list-definitions)))
                 (position (assoc construct-string definitions :test #'string-equal)))
            (if position
              (ask fred-buffer (ccl::go-to-def position) (window-select)))))))))

(defun babylon-edit-prolog-clause (axset-name predicate-name)
  (let  ((construct-string (format nil "~S" axset-name)))
    (dolist (kb-file (send-kb :file-name))
      (let ((fred-buffer (or (get axset-name '%editor-buffer)
                             (is-known-window kb-file *kb-window*))))
        (if fred-buffer
          (let* ((definitions (ask fred-buffer (ccl::list-definitions)))
                 (position (assoc construct-string definitions :test #'string-equal)))
            (if position
              (ask fred-buffer 
                (ccl::go-to-def position)
                (ccl::wsearch (concatenate 'string "((" (string predicate-name)))
                (collapse-selection nil)
                (window-select)
                (window-update)))))))))

;;; eof

