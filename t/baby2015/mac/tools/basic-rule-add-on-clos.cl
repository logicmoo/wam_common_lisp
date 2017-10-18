;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10  -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;  BABYLON  exploring rules (the CLOS version)

(eval-when (eval compile load)
  (bab-require 'add-on-base)
  (bab-require 'basic-rule-mixin))

(defvar Rule-Display-Item nil)
(defvar Rule-Item-Header nil)
(defvar Rule-Edit-Buttom nil)
(defvar Rule-Item-List nil)
(defvar Rule-Set-Item-Header nil)
(defvar Rule-Set-Edit-Buttom nil)
(defvar Rule-Set-Item-List nil)
(defvar Rule-Set-Menu nil)

(defclass rs-sequence-dialog-item (c-sequence-dialog-item) ())

(defmethod reset-table-sequence ((self rs-sequence-dialog-item) 
                                 &optional (table-sequence '()))
  (deselect self)
  (set-dialog-item-text Rule-Display-Item "")
  (set-table-sequence self table-sequence))

;;; -----------------------------------------------------------------

(defun allocate-rule-set-menu ()
  (setf Rule-Display-Item
        (make-instance 'string-sequence-dialog
                       :table-sequence nil
                       :table-hscrollp nil
                       :view-font '("Monaco" 9)
                       :cell-size #@(487 12)
                       :view-size #@(502 183)
                       :view-position #@(2 125)))
  
  (setf Rule-Item-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "Rule"
                       :view-font '("Chicago" 12)
                       :view-position #@(300 6)))
  
  (setf Rule-Edit-Buttom
        (make-instance 'button-dialog-item
                       :dialog-item-text "GoTo"
                       :dialog-item-enabled-p nil
                       :view-position #@(258 6)
                       :view-size #@(36 15)
                       :view-font '("Chicago" 12)
                       :dialog-item-action
                       #'(lambda (item) 
                           (declare (ignore item))
                           (babylon-edit-rule *CRS* *CR*))))
  
  (setf Rule-Item-List
        (make-instance 'rs-sequence-dialog-item
                       :update-function
                       #'(lambda () 
                           (dialog-item-disable Rule-Edit-Buttom)
                           (let ((fl (filter-list (send-rule :get-rule-names *CRS*) 
                                                  *CRNF*)))
                             (if *CRNS* (sort fl #'string-lessp) fl)))
                       :dialog-item-action
                       #'(lambda (item) 
                           (if (selected-cells item)
                             (let ((rule (selection item)))
                               (dialog-item-enable Rule-Edit-Buttom)
                               (setf *CR* rule)
                               (set-dialog-item-text 
                                Rule-Display-Item
                                (write-to-string (send-rule :get-rule *CRS* rule) 
                                                 :pretty t :escape t)))))
                       :cell-size #@(235 12)
                       :view-size #@(250 96)
                       :view-position #@(254 27)))
  
  (setf Rule-Set-Edit-Buttom
        (make-instance 'button-dialog-item
                       :dialog-item-text "GoTo"
                       :view-font '("Chicago" 12)
                       :dialog-item-enabled-p nil
                       :view-position #@(8 6)
                       :view-size #@(36 15)               
                       :dialog-item-action
                       #'(lambda (item)
                           (declare (ignore item))
                           (babylon-edit *CRS*))))
  
  (setf Rule-Set-Item-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "Rule Set"
                       :view-font '("Chicago" 12)
                       :view-position #@(50 6)))
  
  (setf Rule-Set-Item-List
        (make-instance 'c-sequence-dialog-item
                       :dependents '(Rule-Item-List)
                       :update-function
                       #'(lambda () 
                           (dialog-item-disable rule-set-edit-buttom)
                           (dialog-item-disable rule-edit-buttom)
                           (let ((fl (filter-list (send-rule :get-rule-set-names) 
                                                  *CRSF*)))
                             (if *CRSS* (sort fl #'string-lessp) fl)))
                       :dialog-item-action
                       #'(lambda (item) 
                           (if (selected-cells item)
                             (let ((rule-set (selection item)))
                               (setf *CRS* rule-set)
                               (broadcast item 'update-table-sequence)
                               (dialog-item-enable Rule-Set-Edit-Buttom))))
                       :view-size #@(250 96)
                       :cell-size #@(235 12)
                       :view-position #@(2 27)))
  
  (setf Rule-Set-Menu
        (make-instance 'bury-dialog
                       :window-type :document
                       :window-title "Rule Exploration Menu"
                       :window-show nil
                       :view-size #@(506 310)
                       :view-position #@(2 40)
                       :view-font '("Monaco" 9)
                       :view-subviews 
                       (list Rule-Set-Edit-Buttom Rule-Set-Item-Header 
                             Rule-Edit-Buttom Rule-Item-Header 
                             Rule-Set-Item-List Rule-Item-List 
                             Rule-Display-Item))))

;;; for dumplisp -----------------------------------------------------------

(allocate-rule-set-menu)

(defun deallocate-rule-set-menu ()
  (window-deallocate rule-set-menu))

(progn
  (push (symbol-function 'deallocate-rule-set-menu) 
        *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'allocate-rule-set-menu)))))

;;; -------------------------------------------------------------------------

;;  BABYLON  exploring results (explain-results)

(defvar Facts-Explain-Item nil)
(defvar True-Facts-Item-Header nil)
(defvar True-Facts-Item-List nil)
(defvar Unprovable-Facts-Item-Header nil)
(defvar Unprovable-Facts-Item-List nil)
(defvar True-Facts-Menu nil)

(defclass tf-dialog (bury-dialog) ())

(defmethod adjust ((self tf-dialog))
  (set-dialog-item-text Facts-Explain-Item "")
  (deselect true-facts-item-list)
  (set-table-sequence true-facts-item-list (send-rule :get-true-facts))
  (deselect unprovable-facts-item-list)
  (set-table-sequence unprovable-facts-item-list 
                      (send-rule :get-unprovable-facts)))
 
(defmethod window-select ((self tf-dialog))
  (adjust self)
  (call-next-method))

(defun allocate-true-facts-menu ()
  (setf Facts-Explain-Item
        (make-instance 'static-text-dialog-item
                       :dialog-item-text ""
                       :view-font '("Monaco" 9)
                       :view-size #@(500 102)
                       :view-position #@(2 116)))
  
  (setf True-Facts-Item-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "Proved"
                       :view-font '("Monaco" 9)
                       :view-position #@(4 1)))
  
  (setf True-Facts-Item-List
        (make-instance 'sequence-dialog-item
                       :dialog-item-action
                       #'(lambda (item)
                           (when (selected-cells item)
                             (deselect unprovable-facts-item-list)
                             (set-dialog-item-text 
                              Facts-Explain-Item
                              (format nil "~{~A~%~}" 
                                      (send-rule :translate-status-into-string 
                                                 (selection item))))))
                       :view-size #@(250 96)
                       :cell-size #@(235 12)
                       :view-position #@(2 14)
                       :table-hscrollp nil))
  
  (setf Unprovable-Facts-Item-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "Unproved"
                       :view-font '("Monaco" 9)
                       :view-position #@(256 1)))
  
  (setf Unprovable-Facts-Item-List
        (make-instance 'sequence-dialog-item
                       :dialog-item-action
                       #'(lambda (item)
                           (declare (ignore item))
                           (ed-beep)
                           (message-dialog "Not yet implemented! TED needed"))
                       :view-size #@(250 96)
                       :cell-size #@(235 12)
                       :view-position #@(254 14)
                       :table-hscrollp nil))
  
  (setf True-Facts-Menu
        (make-instance 'tf-dialog
                       :window-type :document
                       :view-size #@(506 220)
                       :view-position #@(2 40)
                       :window-show nil
                       :window-title "Facts Exploration Menu"
                       :view-font '("Monaco" 9)
                       :view-subviews 
                       (list 
                        True-Facts-Item-Header True-Facts-Item-List
                        Unprovable-Facts-Item-Header Unprovable-Facts-Item-List
                        Facts-Explain-Item))))

;;; for dumplisp -----------------------------------------------------------

(allocate-True-Facts-Menu)

(defun deallocate-True-Facts-Menu ()
  (window-deallocate True-Facts-Menu))

(progn
  (push (symbol-function 'deallocate-True-Facts-Menu) 
        *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'allocate-True-Facts-Menu)))))

;;; -------------------------------------------------------------------------

(def$method (basic-rule-mixin :explore-facts) ()
  (window-select True-Facts-Menu))

(def$method (basic-rule-mixin :explore-rules) ()
  (window-select Rule-Set-Menu))

;;; -------------------------------------------------------------------------

(def$method (basic-rule-mixin :after :refresh-yourself) ()
  (setf *CRS* nil *CR* nil)
  (update-table-sequence Rule-Set-Item-List)
  (adjust True-Facts-Menu))

(def$method (basic-rule-mixin :after :reset-kb) ()
  (adjust True-Facts-Menu))

(def$method (basic-rule-mixin :after :add-to-rules) (a-rule-set)
  (if (equal a-rule-set *CRS*) 
    (update-table-sequence Rule-Item-List)
    (update-table-sequence Rule-Set-Item-List)))

;;; -------------------------------------------------------------------------

(def$method (basic-rule-mixin :after :deselect-kb) ()
  (window-hide True-Facts-Menu)
  (window-hide Rule-Set-Menu))

;;; -------------------------------------------------------------------------

;;; eof

