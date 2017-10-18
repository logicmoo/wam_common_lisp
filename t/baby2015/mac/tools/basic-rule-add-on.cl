;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User; Base: 10  -*-

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;  BABYLON  exploring rules

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

(defobject *rs-sequence-dialog-item* *c-sequence-dialog-item*)

(defobfun (reset-table-sequence *rs-sequence-dialog-item*) 
          (&optional (table-sequence '()))
  (deselect)
  (ask Rule-Display-Item (set-dialog-item-text ""))
  (set-table-sequence table-sequence))

;;; -----------------------------------------------------------------

(defun allocate-rule-set-menu ()
  (setf Rule-Display-Item
        (oneof *string-sequence-dialog*
               :table-sequence nil
               :table-hscrollp nil
               :dialog-item-font '("Monaco" 9)
               :cell-size #@(490 12)
               :dialog-item-size #@(502 183)
               :dialog-item-position #@(2 125)))
  
  (setf Rule-Item-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "Rule"
               :dialog-item-font '("Chicago" 12)
               :dialog-item-position #@(300 6)))

  (setf Rule-Edit-Buttom
        (oneof *button-dialog-item*
               :dialog-item-text "GoTo"
               :dialog-item-enabled-p nil
               :dialog-item-position #@(258 6)
               :dialog-item-size #@(36 15)
               :dialog-item-font '("Chicago" 12)
               :dialog-item-action
               #'(lambda () 
                   (babylon-edit-rule
                    (ask Rule-Set-Item-List (cell-contents (car (selected-cells))))
                    (ask Rule-Item-List (cell-contents (car (selected-cells))))))
               ))

  (setf Rule-Item-List
        (oneof *rs-sequence-dialog-item*
               :update-function
               #'(lambda () 
                   (ask Rule-Edit-Buttom (dialog-item-disable))
                   (let ((fl (filter-list (send-rule :get-rule-names *CRS*) *CRNF*)))
                     (if *CRNS* (sort fl #'string-lessp) fl)))
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (funcall #'(lambda (rule-name)
                                 (ask Rule-Edit-Buttom (dialog-item-enable))
                                 (ask Rule-Display-Item
                                   (set-dialog-item-text 
                                    (write-to-string 
                                     (send-rule :get-rule *CRS* rule-name) 
                                     :pretty t 
                                     :escape t))))
                             (cell-contents (car (selected-cells))))
                    (usual-dialog-item-action))))
               :dialog-item-size #@(250 96)
               :cell-size #@(250 12)
               :dialog-item-position #@(254 27)))
  
  (setf Rule-Set-Edit-Buttom
        (oneof *button-dialog-item*
               :dialog-item-text "GoTo"
               :dialog-item-font '("Chicago" 12)
               :dialog-item-enabled-p nil
               :dialog-item-position #@(8 6)
               :dialog-item-size #@(36 15)               
               :dialog-item-action
               #'(lambda () 
                   (babylon-edit
                    (ask Rule-Set-Item-List 
                      (cell-contents (car (selected-cells))))))
               ))

  (setf Rule-Set-Item-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "Rule Set"
               :dialog-item-font '("Chicago" 12)
               :dialog-item-position #@(50 6)))
 
  (setf Rule-Set-Item-List
        (oneof *c-sequence-dialog-item*
               :dependents '(Rule-Item-List)
               :update-function
               #'(lambda () 
                   (ask rule-set-edit-buttom (dialog-item-disable))
                   (ask rule-edit-buttom (dialog-item-disable))
                   (let ((fl (filter-list (send-rule :get-rule-set-names) *CRSF*)))
                     (if *CRSS* (sort fl #'string-lessp) fl)))
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (funcall #'(lambda (rule-set)
                                 (setf *CRS* rule-set)
                                 (ask Rule-Set-Edit-Buttom (dialog-item-enable))
                                 (broadcast 'update-table-sequence))
                             (cell-contents (car (selected-cells))))
                    (usual-dialog-item-action))))
               :dialog-item-size #@(250 96)
               :cell-size #@(250 12)
               :dialog-item-position #@(2 27)))
  
  (setf Rule-Set-Menu
        (oneof *bury-dialog* ;*dialog*
               :window-type :document
               :window-size #@(506 310)
               :window-position #@(2 40)
               :window-show nil
               :window-title "Rule Exploration Menu"
               :window-font '("Monaco" 9)
               :dialog-items (list Rule-Set-Edit-Buttom Rule-Set-Item-Header 
                                   Rule-Edit-Buttom Rule-Item-Header 
                                   Rule-Set-Item-List Rule-Item-List 
                                   Rule-Display-Item))))

;;; for dumplisp -----------------------------------------------------------

(allocate-rule-set-menu)

(defun deallocate-rule-set-menu ()
  (ask rule-set-menu (window-close t)))

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

(defobject *tf-dialog* *bury-dialog*)

(defobfun (adjust *tf-dialog*) ()
  (ask Facts-Explain-Item (set-dialog-item-text ""))
  (ask true-facts-item-list
    (dolist (cell (selected-cells))
      (cell-deselect (point-h cell) (point-v cell)))
    (set-table-sequence (send-rule :get-true-facts)))
  (ask unprovable-facts-item-list
    (dolist (cell (selected-cells))
      (cell-deselect (point-h cell) (point-v cell)))
    (set-table-sequence (send-rule :get-unprovable-facts))))
 
(defobfun (window-select *tf-dialog*) ()
  (adjust)
  (usual-window-select))

(defun allocate-true-facts-menu ()
  (setf Facts-Explain-Item
        (oneof *static-text-dialog-item*
               :dialog-item-text ""
               :dialog-item-size #@(500 102)
               :dialog-item-position #@(2 116)))
  
  (setf True-Facts-Item-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "True Facts"
               :dialog-item-position #@(4 2)))
  
  (setf True-Facts-Item-List
        (oneof *sequence-dialog-item*
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (funcall #'(lambda (fact)
                                 (ask unprovable-facts-item-list
                                   (dolist (cell (selected-cells))
                                     (cell-deselect (point-h cell) (point-v cell))))
                                 (ask Facts-Explain-Item 
                                   (set-dialog-item-text
                                    (format nil "~{~A~%~}" 
                                            (send-rule :translate-status-into-string fact)))))
                             (cell-contents (car (selected-cells))))
                    (usual-dialog-item-action))))
               :dialog-item-size #@(250 96)
               :cell-size #@(246 12)
               :dialog-item-position #@(2 14)
               :table-hscrollp nil))
  
  (setf Unprovable-Facts-Item-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "Unprovable Facts"
               :dialog-item-position #@(256 2)))
  
  (setf Unprovable-Facts-Item-List
        (oneof *sequence-dialog-item*
               :dialog-item-action
               '(progn 
                  (ed-beep)
                  (message-dialog "Not yet implemented! TED needed"))
               :dialog-item-size #@(250 96)
               :cell-size #@(246 12)
               :dialog-item-position #@(254 14)
               :table-hscrollp nil))
  
  (setf True-Facts-Menu
        (oneof *tf-dialog*
               :window-type :document
               :window-size #@(506 220)
               :window-position #@(2 40)
               :window-show nil
               :window-title "Facts Exploration Menu"
               :window-font '("Monaco" 9)
               :dialog-items (list 
                              True-Facts-Item-Header True-Facts-Item-List
                              Unprovable-Facts-Item-Header Unprovable-Facts-Item-List
                              Facts-Explain-Item)))) 

;;; for dumplisp -----------------------------------------------------------

(allocate-True-Facts-Menu)

(defun deallocate-True-Facts-Menu ()
  (ask True-Facts-Menu (window-close t)))

(progn
  (push (symbol-function 'deallocate-True-Facts-Menu) 
        *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'allocate-True-Facts-Menu)))))

;;; -------------------------------------------------------------------------

(def$method (basic-rule-mixin :explore-facts) ()
  (ask True-Facts-Menu (window-select)))

(def$method (basic-rule-mixin :explore-rules) ()
  (ask Rule-Set-Menu (window-select)))

;;; -------------------------------------------------------------------------

(def$method (basic-rule-mixin :after :refresh-yourself) ()
  (setf *CRS* nil)
  (ask Rule-Set-Item-List (update-table-sequence))
  (ask True-Facts-Menu (adjust)))

(def$method (basic-rule-mixin :after :reset-kb) ()
  (ask True-Facts-Menu (adjust)))

(def$method (basic-rule-mixin :after :add-to-rules) (a-rule-set)
  (if (equal a-rule-set *CRS*) 
    (ask Rule-Item-List (update-table-sequence))
    (ask Rule-Set-Item-List (update-table-sequence))))

;;; -------------------------------------------------------------------------

(def$method (basic-rule-mixin :after :deselect-kb) ()
  (ask True-Facts-Menu (window-hide))
  (ask Rule-Set-Menu (window-hide)))

;;; -------------------------------------------------------------------------

;;; eof

