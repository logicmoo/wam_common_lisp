;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10  -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther
;;           dialogs
;;           some special dialog item classes (CLOS version)

(defmethod deselect ((self sequence-dialog-item))
  (if (selected-cells self)
    (dolist (cell (selected-cells self))
      (cell-deselect self (point-h cell) (point-v cell)))))

(defmethod selection ((self sequence-dialog-item))
  (and (selected-cells self)
       (cell-contents self (car (selected-cells self)))))

(defclass c-sequence-dialog-item (sequence-dialog-item)
	  ((dependents :initarg :dependents :initform '() 
                       :accessor dependents)
	   (update-function :initarg :update-function :initform #'(lambda () '())
                            :accessor update-function))
  (:default-initargs
    :view-size #@(166 96)
    :cell-size #@(151 11)
    :view-font '("Monaco" 9)
    :table-hscrollp nil))

(defmethod broadcast ((self c-sequence-dialog-item) request)
  (dolist (dependent (dependents self))
    (funcall (symbol-function request) (symbol-value dependent))))

(defmethod reset-table-sequence ((self c-sequence-dialog-item) 
                                 &optional (table-sequence '()))
  (deselect self)
  (broadcast self 'reset-table-sequence)
  (set-table-sequence self table-sequence))

(defmethod update-table-sequence ((self c-sequence-dialog-item))
  (reset-table-sequence self (funcall (update-function self))))

;;; -----------------------------------------------------------------------------

(defclass bury-dialog (dialog)
  ((mode :initform :bury)))

(defmethod window-close ((self bury-dialog))
  (if (eq (slot-value self 'mode) :bury)
    (window-hide self)
    (call-next-method)))

(defmethod window-deallocate ((self bury-dialog))
  (setf (slot-value self 'mode) :close)
  (window-close self))

;;; -----------------------------------------------------------------------------

(defclass string-sequence-dialog (sequence-dialog-item)()
  (:default-initargs
    :view-font '("Monaco" 9)))

(defun split-lines (string)
  (with-input-from-string (stream string)
    (do ((m-line (read-line stream nil nil) (read-line stream nil nil))
         (lines))
        ((null m-line) (nreverse lines))
      (push m-line lines))))
             
(defmethod set-dialog-item-text ((self string-sequence-dialog) string)
  (deselect self)
  (set-table-sequence self (split-lines string)))

(defmethod get-dialog-item-text ((self string-sequence-dialog))
  (format nil "~{~A~%~}" (table-sequence self)))

;;; -----------------------------------------------------------------------------


(defun babylon-edit (construct)
  "Positions in one of the Editor Buffers of current knowledge base to definition construct"
  (let  ((construct-string (format nil "~S" construct)))
    (dolist (kb-file (send-kb :file-name))
      (let ((fred-buffer (is-known-window kb-file 'kb-window)))
        (if fred-buffer
          (let* ((definitions (ccl::list-definitions fred-buffer))
                 (position (assoc construct-string definitions :test #'string-equal)))
            (when position
              #-:CCL-3(ccl::go-to-def fred-buffer position)
              #+:CCL-3(ccl::window-scroll fred-buffer (cdr position))
              (window-select fred-buffer))))))))

(defun babylon-edit-rule (rule-set-name rule-name)
  (let  ((construct-string (format nil "~S" rule-set-name)))
    (dolist (kb-file (send-kb :file-name))
      (let ((fred-buffer (is-known-window kb-file 'kb-window)))
        (if fred-buffer
          (let* ((definitions (ccl::list-definitions fred-buffer))
                 (position (assoc construct-string definitions :test #'string-equal)))
            (when position
              #-:CCL-3(ccl::go-to-def fred-buffer position)
              #+:CCL-3(ccl::window-scroll fred-buffer (cdr position))
              (ccl::window-search fred-buffer (concatenate 'string "(" (string rule-name)))
              (collapse-selection fred-buffer nil)
              (window-select fred-buffer)
              (fred-update fred-buffer))))))))

(defun babylon-edit-prolog (construct)
  "Positions in one of the Editor Buffers of current knowledge base to definition construct"
  (let  ((construct-string (format nil "~S" construct)))
    (dolist (kb-file (send-kb :file-name))
      (let ((fred-buffer (or (get construct '%editor-buffer)
                             (is-known-window kb-file 'kb-window))))
        (if fred-buffer
          (let* ((definitions (ccl::list-definitions fred-buffer))
                 (position (assoc construct-string definitions :test #'string-equal)))
            (when position
              #-:CCL-3(ccl::go-to-def fred-buffer position)
              #+:CCL-3(ccl::window-scroll fred-buffer (cdr position))
              (window-select fred-buffer))))))))

(defun babylon-edit-prolog-clause (axset-name predicate-name)
  (let  ((construct-string (format nil "~S" axset-name)))
    (dolist (kb-file (send-kb :file-name))
      (let ((fred-buffer (or (get axset-name '%editor-buffer)
                             (is-known-window kb-file 'kb-window))))
        (if fred-buffer
          (let* ((definitions (ccl::list-definitions fred-buffer))
                 (position (assoc construct-string definitions :test #'string-equal)))
            (when position
              #-:CCL-3(ccl::go-to-def fred-buffer position)
              #+:CCL-3(ccl::window-scroll fred-buffer (cdr position))
              (ccl::window-search fred-buffer (concatenate 'string "((" (string predicate-name)))
              (collapse-selection fred-buffer nil)
              (window-select fred-buffer)
              (fred-update fred-buffer))))))))

;;; eof



