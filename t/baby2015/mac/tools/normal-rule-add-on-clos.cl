;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10  -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;  BABYLON  exploring rule terms (the CLOS version)

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
        (make-instance 'string-sequence-dialog
                       :table-sequence nil
                       :table-hscrollp nil
                       :view-font '("Monaco" 9)
                       :cell-size #@(487 12)
                       :view-size #@(502 70)
                       :view-position #@(2 228)))
  
  (setf Rule-Terms-Rule-Edit-Buttom
        (make-instance 'button-dialog-item
                       :dialog-item-text "GoTo"
                       :dialog-item-enabled-p nil
                       :view-position #@(130 126)
                       :view-size #@(36 15)
                       :view-font '("Chicago" 12)
                       :dialog-item-action
                       #'(lambda (item)
                           (declare (ignore item))
                           (babylon-edit-rule
                            (selection Rule-Terms-Rule-Set-Item-List)
                            (or (selection Rule-Terms-Condition-Item-List)
                                (selection Rule-Terms-Action-Item-List))))))
  
  (setf Rule-Terms-Condition-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "used as condition in rule"
                       :view-font '("Monaco" 9)
                       :view-position #@(172 114)))
  
  (setf Rule-Terms-Condition-Item-List
        (make-instance 'c-sequence-dialog-item
                       :update-function
                       #'(lambda ()
                           (dialog-item-disable Rule-Terms-Rule-Edit-Buttom)
                           (mapcar #'rule-set-name
                                   (send-rule :inif *RTCFT* 
                                              (send-rule :get-rule-set *RTRS*))))
                       :dialog-item-action
                       #'(lambda (item)
                           (when (selected-cells item)
                             (set-dialog-item-text Rule-Terms-Rule-Display-Item "")
                             (dialog-item-enable Rule-Terms-Rule-Edit-Buttom)
                             (deselect Rule-Terms-Action-Item-List)
                             (set-dialog-item-text Rule-Terms-Rule-Display-Item
                                                   (write-to-string 
                                                    (send-rule :get-rule *RTRS* (selection item)) 
                                                    :pretty t 
                                                    :escape t))))
                       :view-position #@(170 126)
                       :view-font '("Monaco" 9)))
  
  (setf Rule-Terms-Action-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "used as action in rule"
                       :view-font '("Monaco" 9)
                       :view-position #@(340 114)))
  
  (setf Rule-Terms-Action-Item-List
        (make-instance 'c-sequence-dialog-item
                       :update-function 
                       #'(lambda () 
                           (dialog-item-disable Rule-Terms-Rule-Edit-Buttom)
                           (mapcar #'rule-set-name 
                                   (send-rule :inthen *RTCFT* 
                                              (send-rule :get-rule-set *RTRS*))))
                       :dialog-item-action
                       #'(lambda (item)
                           (when (selected-cells item)
                             (set-dialog-item-text Rule-Terms-Rule-Display-Item "")
                             (dialog-item-enable Rule-Terms-Rule-Edit-Buttom)
                             (deselect Rule-Terms-Condition-Item-List)
                             (set-dialog-item-text Rule-Terms-Rule-Display-Item 
                                                   (write-to-string 
                                                    (send-rule :get-rule *RTRS* (selection item)) 
                                                    :pretty t 
                                                    :escape t))))
                       :view-position #@(338 126)
                       :view-font '("Monaco" 9)))
  
  (setf Rule-Terms-Second-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "Term containing Element"
                       :view-font '("Monaco" 9)
                       :view-position #@(340 2)))
  
  (setf Rule-Terms-Second-Item-List
        (make-instance 'c-sequence-dialog-item
                       :dependents '(Rule-Terms-Condition-Item-List
                                     Rule-Terms-Action-Item-List)
                       :update-function 
                       #'(lambda () 
                           (case *RTO*
                             (first-and-second '(has is))
                             (otherwise '())))
                       :dialog-item-action
                       #'(lambda (item)
                           (when (selected-cells item)
                             (set-dialog-item-text Rule-Terms-Rule-Display-Item "")
                             (setf *RTCFT* (selection item))
                             (broadcast item 'update-table-sequence)))
                       :view-position #@(338 14)
                       :view-font '("Monaco" 9)))
  
  (setf Rule-Terms-First-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "Element or Term"
                       :view-font '("Monaco" 9)
                       :view-position #@(172 2)))
  
  (setf Rule-Terms-First-Item-List
        (make-instance 'c-sequence-dialog-item
                       :dependents '(Rule-Terms-Condition-Item-List
                                     Rule-Terms-Action-Item-List)
                       :update-function 
                       #'(lambda ()
                           (dialog-item-disable Rule-Terms-Rule-Edit-Buttom)
                           (reset-table-sequence Rule-Terms-Second-Item-List)
                           (set-dialog-item-text Rule-Terms-Rule-Display-Item "")
                           (case *RTO*
                             (all *RTALL*)
                             (first (collect-term-components *RTALL* 'first))
                             (second (collect-term-components *RTALL* 'second))
                             (first-and-second nil)
                             (otherwise (message-dialog 
                                         (format nil "Unknown Option ~S" *RTO*)))))
                       :dialog-item-action
                       #'(lambda (item)
                           (when (selected-cells item)
                             (dialog-item-disable Rule-Terms-Rule-Edit-Buttom)
                             (set-dialog-item-text Rule-Terms-Rule-Display-Item "")
                             (let ((term (selection item)))
                               (case *RTO*
                                 (all (setf *RTCFT* term))
                                 (first (reset-table-sequence Rule-Terms-Second-Item-List
                                                              (filter-first `(,term nil) *RTALL*)))
                                 (second (reset-table-sequence Rule-Terms-Second-Item-List
                                                               (filter-second `(nil ,term) *RTALL*)))
                                 (first-and-second nil))
                               (if (eq *RTO* 'all)
                                 (broadcast item 'update-table-sequence)))))
                       :view-position #@(170 14)
                       :view-font '("Monaco" 9)))
  
  (setf Rule-Terms-Rule-Set-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "Select Rule Set"
                       :view-font '("Monaco" 9)
                       :view-position #@(4 2)))
  
  (setf Rule-Terms-Rule-Set-Item-List
        (make-instance 'c-sequence-dialog-item
                       :dependents '(Rule-Terms-First-Item-List
                                     Rule-Terms-Second-Item-List)
                       :update-function 
                       #'(lambda () 
                           (dialog-item-disable Rule-Terms-Rule-Set-Edit-Buttom)
                           (dialog-item-disable Rule-Terms-Rule-Edit-Buttom)
                           (let ((fl (filter-list (send-rule :get-rule-set-names) *CRSF*)))
                             (if *CRSS* (sort fl #'string-lessp) fl)))
                       :dialog-item-action
                       #'(lambda (item)
                           (when (selected-cells item)
                             (dialog-item-enable Rule-Terms-Rule-Set-Edit-Buttom)
                             (set-dialog-item-text Rule-Terms-Rule-Display-Item "")
                             (let ((RuleSet (selection item)))
                               (setf *RTRS* RuleSet)
                               (setf *RTALL* (send-rule :used-terms 
                                                        (send-rule :get-rule-set RuleSet)))
                               (broadcast item 'update-table-sequence))))
                       :view-position #@(2 14)
                       :view-font '("Monaco" 9)))
  
  (setf Rule-Terms-Rule-Set-Edit-Buttom
        (make-instance 'button-dialog-item
                       :dialog-item-text "GoTo"
                       :view-font '("Chicago" 12)
                       :dialog-item-enabled-p nil
                       :view-position #@(4 114)
                       :view-size #@(36 15)               
                       :dialog-item-action
                       #'(lambda (item) 
                           (declare (ignore item))
                           (babylon-edit
                            (selection Rule-Terms-Rule-Set-Item-List)))))
  
  (setf Rule-Terms-All-Button
        (make-instance 'radio-button-dialog-item
                       :dialog-item-text "All Terms"
                       :view-position #@(20 146)
                       :view-font '("Chicago" 12)
                       :dialog-item-action 
                       #'(lambda (item) 
                           (declare (ignore item))
                           (setq *RTO* 'all)
                           (update-table-sequence Rule-Terms-First-Item-List))
                       :radio-button-cluster '*RTO*
                       :radio-button-pushed-p t))
  
  (setf Rule-Terms-First-Button
        (make-instance 'radio-button-dialog-item
                       :dialog-item-text "First Element"
                       :view-position #@(20 166)
                       :view-font '("Chicago" 12)
                       :dialog-item-action 
                       #'(lambda (item) 
                           (declare (ignore item))
                           (setq *RTO* 'first)
                           (update-table-sequence Rule-Terms-First-Item-List))
                       :radio-button-cluster '*RTO*))
  
  (setf Rule-Terms-Second-Button
        (make-instance 'radio-button-dialog-item
                       :dialog-item-text "Second Element"
                       :view-position #@(20 186)
                       :view-font '("Chicago" 12)
                       :dialog-item-action 
                       #'(lambda (item) 
                           (declare (ignore item))
                           (setq *RTO* 'second)
                           (update-table-sequence Rule-Terms-First-Item-List)) 
                       :radio-button-cluster '*RTO*))
  
  (setf Rule-Terms-Menu
        (make-instance 'bury-dialog
                       :window-type :document
                       :view-size #@(506 300)
                       :view-position #@(2 40)
                       :window-show nil
                       :window-title "Rule Terms Exploration Menu"
                       :view-font '("Monaco" 9)
                       :view-subviews
                       (list 
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
  (window-deallocate Rule-Terms-Menu))

(progn
  (push (symbol-function 'deallocate-Rule-Terms-Menu) 
        *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'allocate-Rule-Terms-Menu)))))

;;; -------------------------------------------------------------------------

(def$method (normal-rule-mixin :explore-rule-terms) ()
  (window-select Rule-Terms-Menu))

;;; -------------------------------------------------------------------------

(def$method (normal-rule-mixin :after :refresh-yourself) ()
  (setf *CRS* nil)
  (update-table-sequence Rule-Terms-Rule-Set-Item-List)
  (set-dialog-item-text Rule-Terms-Rule-Display-Item ""))

(def$method (basic-rule-mixin :after :add-to-rules) (a-rule-set)
  (if (equal a-rule-set *RTRS*) 
    (update-table-sequence Rule-Terms-Rule-Set-Item-List)))

;;; -------------------------------------------------------------------------

(def$method (normal-rule-mixin :after :deselect-kb) ()
  (window-hide Rule-Terms-Menu))

;;; eof

