;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10  -*-

(in-package "BABYLON")

;;           Copyright   1991    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;  BABYLON  exploring prolog axsets (CLOS version)

(eval-when (eval compile load)
  (bab-require 'add-on-base)
  (bab-require 'basic-prolog-mixin))

(defvar predicate-display-item nil)
(defvar predicate-item-Header nil)
(defvar predicate-Edit-Buttom nil)
(defvar predicate-item-List nil)
(defvar axset-Item-Header nil)
(defvar axset-Edit-Buttom nil)
(defvar axset-item-list nil)
(defvar Explore-Axsets-Dialog nil)

(defvar *CAS* nil)

(defclass ps-sequence-dialog-item (c-sequence-dialog-item) ())

(defmethod reset-table-sequence ((self ps-sequence-dialog-item) 
                                 &optional (table-sequence '()))
  (deselect self)
  (set-dialog-item-text predicate-Display-Item"")
  (set-table-sequence self table-sequence))


;;-----------------------------------------------------------------

(defun allocate-Explore-Axsets-Dialog ()
  (setf predicate-display-item
        (make-instance 'string-sequence-dialog
                       :table-sequence nil
                       :table-hscrollp nil
                       :view-font '("Monaco" 9)
                       :cell-size #@(490 12)
                       :view-size #@(502 183)
                       :view-position #@(2 125)))
  
  (setf predicate-item-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "Predicate"
                       :view-font '("Chicago" 12)
                       :view-position #@(300 6)))
  
  (setf predicate-Edit-Buttom
        (make-instance 'button-dialog-item
                       :dialog-item-text "GoTo"
                       :dialog-item-enabled-p nil
                       :view-position #@(258 6)
                       :view-size #@(36 15)
                       :view-font '("Chicago" 12)
                       :dialog-item-action
                       #'(lambda (item) 
                           (declare (ignore item))
                           (babylon-edit-prolog-clause
                            (selection axset-item-list)
                            (selection predicate-item-list)))))
  
  (setf predicate-item-List
        (make-instance 'ps-sequence-dialog-item
                       :update-function
                       #'(lambda ()
                           (set-dialog-item-text predicate-display-item "")
                           (dialog-item-disable  predicate-edit-buttom)
                           (get-predicates *CAS*))
                       :dialog-item-action
                       #'(lambda (item)
                           (when (selected-cells item)
                             (set-dialog-item-text predicate-display-item
                                                   (print-pred *CAS* (selection item) "" nil))
                             (dialog-item-enable predicate-edit-buttom)))
                       :view-size #@(250 96)
                       :cell-size #@(235 12)
                       :view-position #@(254 27)))
  
  (setf axset-Edit-Buttom
        (make-instance 'button-dialog-item
                       :dialog-item-text "GoTo"
                       :view-font '("Chicago" 12)
                       :dialog-item-enabled-p nil
                       :view-position #@(8 6)
                       :view-size #@(36 15)               
                       :dialog-item-action
                       #'(lambda (item) 
                           (declare (ignore item))
                           (babylon-edit-prolog
                            (selection axset-item-list)))))
  
  (setf axset-Item-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "AxiomSet"
                       :view-font '("Chicago" 12)
                       :view-position #@(50 6)))
  
  (setf axset-item-list
        (make-instance 'c-sequence-dialog-item
                       :dependents '(predicate-item-List)
                       :update-function
                       #'(lambda () 
                           (dialog-item-disable axset-edit-buttom)
                           (dialog-item-disable predicate-edit-buttom)
                           (set-dialog-item-text predicate-display-item "")
                           (send-prolog :axioms))
                       :dialog-item-action
                       #'(lambda (item)
                           (when (selected-cells item)
                             (setf *CAS* (selection item))
                             (broadcast item 'update-table-sequence)
                             (dialog-item-enable axset-edit-buttom)))
                       :view-size #@(250 96)
                       :cell-size #@(235 12)
                       :view-position #@(2 27)))
  
  (setf Explore-Axsets-Dialog
        (make-instance 'bury-dialog
                       :window-type :document
                       :view-size #@(506 310)
                       :view-position #@(2 40)
                       :window-show nil
                       :window-title "AxiomSet Exploration Menu"
                       :view-font '("Monaco" 9)
                       :view-subviews (list axset-edit-buttom axset-item-header 
                                           predicate-edit-buttom predicate-item-header 
                                           axset-item-list predicate-item-list 
                                           predicate-display-item))))

;;; for dumplisp -----------------------------------------------------------

(allocate-explore-axsets-dialog)

(defun deallocate-explore-axsets-dialog ()
  (window-deallocate explore-axsets-dialog))

(progn
  (push (symbol-function 'deallocate-Explore-Axsets-Dialog) 
        *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'allocate-Explore-Axsets-Dialog)))))

;;; -------------------------------------------------------------------------

(def$method (basic-prolog-mixin :explore-axset) ()
  (window-select Explore-Axsets-Dialog))

;;; -------------------------------------------------------------------------

(def$method (basic-prolog-mixin :after :refresh-yourself) ()
  (setf *CAS* nil)
  (update-table-sequence axset-item-list))

(def$method (basic-prolog-processor :after :set-axioms) (axsets)
  (declare (ignore axsets))
  (let ((*current-knowledge-base* meta-processor)) ; proc may not yet be current
    (setf *CAS* nil)
    (update-table-sequence axset-Item-List)))

;;; -------------------------------------------------------------------------

(def$method (basic-prolog-mixin :after :deselect-kb) ()
  (window-hide Explore-Axsets-Dialog))

;;; -------------------------------------------------------------------------

;;; eof

