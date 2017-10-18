;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User; Base: 10  -*-

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;  BABYLON  exploring rule terms

(eval-when (eval compile load)
  (bab-require 'add-on-base)
  (bab-require 'normal-rule-mixin))

(defvar Rule-Terms-Rule-Display-Item nil)
(defvar Rule-Terms-Condition-Header nil)
(defvar Rule-Terms-Condition-Item-List nil)
(defvar Rule-Terms-Action-Header nil)
(defvar Rule-Terms-Action-Item-List nil)
(defvar Rule-Terms-Second-Header nil)
(defvar Rule-Terms-Second-Item-List nil)
(defvar Rule-Terms-First-Header nil)
(defvar Rule-Terms-First-Item-List nil)
(defvar Rule-Terms-Rule-Set-Header nil)
(defvar Rule-Terms-Rule-Set-Item-List nil)
(defvar Rule-Terms-All-Button nil)
(defvar Rule-Terms-First-Button nil)
(defvar Rule-Terms-Second-Button nil)
(defvar Rule-Terms-Menu nil)
(defvar Rule-Terms-Rule-Set-Edit-Buttom nil)
(defvar Rule-Terms-Rule-Edit-Buttom nil)

(defun allocate-rule-terms-menu ()
  (setf Rule-Terms-Rule-Display-Item
        (oneof *string-sequence-dialog*
               :table-sequence nil
               :table-hscrollp nil
               :dialog-item-font '("Monaco" 9)
               :cell-size #@(490 12)
               :dialog-item-size #@(502 70)
               :dialog-item-position #@(2 228)))
  
  (setf Rule-Terms-Rule-Edit-Buttom
        (oneof *button-dialog-item*
               :dialog-item-text "GoTo"
               :dialog-item-enabled-p nil
               :dialog-item-position #@(130 126)
               :dialog-item-size #@(36 15)
               :dialog-item-font '("Chicago" 12)
               :dialog-item-action
               #'(lambda () 
                   (babylon-edit-rule
                    (ask Rule-Terms-Rule-Set-Item-List
                      (cell-contents (car (selected-cells))))
                    (or
                     (and (ask Rule-Terms-Condition-Item-List (table-sequence))
                          (ask Rule-Terms-Condition-Item-List (selected-cells))
                          (ask Rule-Terms-Condition-Item-List (cell-contents (car (selected-cells)))))
                     (and (ask Rule-Terms-Action-Item-List (table-sequence))
                          (ask Rule-Terms-Action-Item-List (selected-cells))
                          (ask Rule-Terms-Action-Item-List (cell-contents (car (selected-cells))))))))
               ))

  (setf Rule-Terms-Condition-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "used as condition in rule"
               :dialog-item-position #@(172 114)))
  
  (setf Rule-Terms-Condition-Item-List
        (oneof *c-sequence-dialog-item*
               :update-function
               #'(lambda ()
                   (ask Rule-Terms-Rule-Edit-Buttom (dialog-item-disable))
                   (mapcar #'rule-set-name
                           (send-rule :inif *RTCFT* 
                                      (send-rule :get-rule-set *RTRS*))))
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (ask Rule-Terms-Rule-Display-Item (set-dialog-item-text ""))
                    (funcall #'(lambda (rule)
                                 (ask Rule-Terms-Rule-Edit-Buttom (dialog-item-enable))
                                 (ask Rule-Terms-Action-Item-List (deselect))
                                 (ask Rule-Terms-Rule-Display-Item
                                   (set-dialog-item-text 
                                    (write-to-string 
                                     (send-rule :get-rule *RTRS* rule) 
                                     :pretty t 
                                     :escape t))))
                             (cell-contents (car (selected-cells))))
                    (usual-dialog-item-action))))
               :dialog-item-position #@(170 126)
               :dialog-item-font '("Monaco" 9)))
  
  (setf Rule-Terms-Action-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "used as action in rule"
               :dialog-item-position #@(340 114)))
  
  (setf Rule-Terms-Action-Item-List
        (oneof *c-sequence-dialog-item*
               :update-function 
               #'(lambda () 
                   (ask Rule-Terms-Rule-Edit-Buttom (dialog-item-disable))
                   (mapcar #'rule-set-name 
                           (send-rule :inthen *RTCFT* 
                                      (send-rule :get-rule-set *RTRS*))))
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (ask Rule-Terms-Rule-Display-Item (set-dialog-item-text ""))
                    (funcall #'(lambda (rule)
                                 (ask Rule-Terms-Rule-Edit-Buttom (dialog-item-enable))
                                 (ask Rule-Terms-Condition-Item-List (deselect))
                                 (ask Rule-Terms-Rule-Display-Item
                                   (set-dialog-item-text 
                                    (write-to-string 
                                     (send-rule :get-rule *RTRS* rule) 
                                     :pretty t 
                                     :escape t))))
                             (cell-contents (car (selected-cells))))
                    (usual-dialog-item-action))))
               :dialog-item-position #@(338 126)
               :dialog-item-font '("Monaco" 9)))
  
  (setf Rule-Terms-Second-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "Term containing Element"
               :dialog-item-position #@(340 2)))
  
  (setf Rule-Terms-Second-Item-List
        (oneof *c-sequence-dialog-item*
               :dependents '(Rule-Terms-Condition-Item-List
                             Rule-Terms-Action-Item-List)
               :update-function 
               #'(lambda () 
                   (case *RTO*
                     (first-and-second '(has is))
                     (otherwise '())))
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (ask Rule-Terms-Rule-Display-Item (set-dialog-item-text ""))
                    (funcall #'(lambda (term)
                                 (setf *RTCFT* term)
                                 (broadcast 'update-table-sequence))
                             (cell-contents (car (selected-cells))))
                    (usual-dialog-item-action))))
               :dialog-item-position #@(338 14)
               :dialog-item-font '("Monaco" 9)))
  
  (setf Rule-Terms-First-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "Element or Term"
               :dialog-item-position #@(172 2)))
  
  (setf Rule-Terms-First-Item-List
        (oneof *c-sequence-dialog-item*
               :dependents '(Rule-Terms-Condition-Item-List
                             Rule-Terms-Action-Item-List)
               :update-function 
               #'(lambda ()
                   (ask Rule-Terms-Rule-Edit-Buttom (dialog-item-disable))
                   (ask Rule-Terms-Second-Item-List (reset-table-sequence))
                   (ask Rule-Terms-Rule-Display-Item (set-dialog-item-text ""))
                   (case *RTO*
                     (all *RTALL*)
                     (first (collect-term-components *RTALL* 'first))
                     (second (collect-term-components *RTALL* 'second))
                     (first-and-second nil)
                     (otherwise (message-dialog 
                                 (format nil "Unknown Option ~S" *RTO*)))))
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (ask Rule-Terms-Rule-Edit-Buttom (dialog-item-disable))
                    (ask Rule-Terms-Rule-Display-Item (set-dialog-item-text ""))
                    (funcall #'(lambda (term)
                                 (case *RTO*
                                   (all (setf *RTCFT* term))
                                   (first (ask Rule-Terms-Second-Item-List 
                                             (reset-table-sequence 
                                              (filter-first `(,term nil) *RTALL*))))
                                   (second (ask Rule-Terms-Second-Item-List 
                                              (reset-table-sequence 
                                               (filter-second `(nil ,term) *RTALL*))))
                                   (first-and-second nil))
                                 (if (eq *RTO* 'all)
                                   (broadcast 'update-table-sequence)))
                             (cell-contents (car (selected-cells))))
                    (usual-dialog-item-action))))
               :dialog-item-position #@(170 14)
               :dialog-item-font '("Monaco" 9)))
  
  (setf Rule-Terms-Rule-Set-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "Select Rule Set"
               :dialog-item-position #@(4 2)))
  
  (setf Rule-Terms-Rule-Set-Item-List
        (oneof *c-sequence-dialog-item*
               :dependents '(Rule-Terms-First-Item-List
                             Rule-Terms-Second-Item-List)
               :update-function 
               #'(lambda () 
                   (ask Rule-Terms-Rule-Set-Edit-Buttom (dialog-item-disable))
                   (ask Rule-Terms-Rule-Edit-Buttom (dialog-item-disable))
                   (let ((fl (filter-list (send-rule :get-rule-set-names) *CRSF*)))
                     (if *CRSS* (sort fl #'string-lessp) fl)))
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (ask Rule-Terms-Rule-Set-Edit-Buttom (dialog-item-enable))
                    (ask Rule-Terms-Rule-Display-Item (set-dialog-item-text ""))
                    (funcall #'(lambda (RuleSet)
                                 (setf *RTRS* RuleSet)
                                 (setf *RTALL* (send-rule :used-terms 
                                                          (send-rule :get-rule-set RuleSet)))
                                 (broadcast 'update-table-sequence))
                             (cell-contents (car (selected-cells))))
                    (usual-dialog-item-action))))
               :dialog-item-position #@(2 14)
               :dialog-item-font '("Monaco" 9)))
  
  (setf Rule-Terms-Rule-Set-Edit-Buttom
        (oneof *button-dialog-item*
               :dialog-item-text "GoTo"
               :dialog-item-font '("Chicago" 12)
               :dialog-item-enabled-p nil
               :dialog-item-position #@(4 114)
               :dialog-item-size #@(36 15)               
               :dialog-item-action
               #'(lambda () 
                   (babylon-edit
                    (ask Rule-Terms-Rule-Set-Item-List 
                      (cell-contents (car (selected-cells))))))))
 
 (setf Rule-Terms-All-Button
        (oneof *radio-button-dialog-item*
               :dialog-item-text "All Terms"
               :dialog-item-position #@(20 146)
               :dialog-item-font '("Chicago" 12)
               :dialog-item-action '(progn 
                                      (setq *RTO* 'all)
                                      (ask Rule-Terms-First-Item-List 
                                        (update-table-sequence))
                                      (usual-dialog-item-action))
               :radio-button-cluster '*RTO*
               :radio-button-pushed-p t))
  
  (setf Rule-Terms-First-Button
        (oneof *radio-button-dialog-item*
               :dialog-item-text "First Element"
               :dialog-item-position #@(20 166)
               :dialog-item-font '("Chicago" 12)
               :dialog-item-action '(progn 
                                      (setq *RTO* 'first)
                                      (ask Rule-Terms-First-Item-List 
                                        (update-table-sequence))
                                      (usual-dialog-item-action))
               :radio-button-cluster '*RTO*))
  
  (setf Rule-Terms-Second-Button
        (oneof *radio-button-dialog-item*
               :dialog-item-text "Second Element"
               :dialog-item-position #@(20 186)
               :dialog-item-font '("Chicago" 12)
               :dialog-item-action '(progn 
                                      (setq *RTO* 'second)
                                      (ask Rule-Terms-First-Item-List 
                                        (update-table-sequence))
                                      (usual-dialog-item-action))
               :radio-button-cluster '*RTO*))
  
  (setf Rule-Terms-Menu
        (oneof *bury-dialog*
               :window-type :document
               :window-size #@(506 300)
               :window-position #@(2 40)
               :window-show nil
               :window-title "Rule Terms Exploration Menu"
               :window-font '("Monaco" 9)
               :default-button nil
               :dialog-items (list 
                              Rule-Terms-Rule-Set-Edit-Buttom
                              Rule-Terms-Rule-Edit-Buttom
                              Rule-Terms-Rule-Display-Item
                              Rule-Terms-Condition-Header
                              Rule-Terms-Condition-Item-List
                              Rule-Terms-Action-Header
                              Rule-Terms-Action-Item-List
                              Rule-Terms-Second-Header
                              Rule-Terms-Second-Item-List
                              Rule-Terms-First-Header
                              Rule-Terms-First-Item-List
                              Rule-Terms-Rule-Set-Header
                              Rule-Terms-Rule-Set-Item-List
                              Rule-Terms-All-Button
                              Rule-Terms-First-Button
                              Rule-Terms-Second-Button))))

;;; for dumplisp -----------------------------------------------------------

(allocate-Rule-Terms-Menu)

(defun deallocate-Rule-Terms-Menu ()
  (ask Rule-Terms-Menu (window-close t)))

(progn
  (push (symbol-function 'deallocate-Rule-Terms-Menu) 
        *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'allocate-Rule-Terms-Menu)))))

;;; -------------------------------------------------------------------------

(def$method (normal-rule-mixin :explore-rule-terms) ()
  (ask Rule-Terms-Menu (window-select)))

;;; -------------------------------------------------------------------------

(def$method (normal-rule-mixin :after :refresh-yourself) ()
  (setf *CRS* nil)
  (ask Rule-Terms-Rule-Set-Item-List (update-table-sequence))
  (ask Rule-Terms-Rule-Display-Item (set-dialog-item-text "")))

(def$method (basic-rule-mixin :after :add-to-rules) (a-rule-set)
  (if (equal a-rule-set *RTRS*) 
    (ask Rule-Terms-Rule-Set-Item-List (update-table-sequence))))

;;; -------------------------------------------------------------------------

(def$method (normal-rule-mixin :after :deselect-kb) ()
  (ask Rule-Terms-Menu (window-hide)))

;;; eof

