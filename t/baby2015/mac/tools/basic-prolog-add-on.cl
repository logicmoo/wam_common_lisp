;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User; Base: 10  -*-

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;  BABYLON  exploring prolog axsets

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

(defobject *ps-sequence-dialog-item* *c-sequence-dialog-item*)

(defobfun (reset-table-sequence *ps-sequence-dialog-item*) 
          (&optional (table-sequence '()))
  (deselect)
  (ask predicate-Display-Item (set-dialog-item-text ""))
  (set-table-sequence table-sequence))


;;-----------------------------------------------------------------

(defun allocate-Explore-Axsets-Dialog ()
  (setf predicate-display-item
        (oneof *string-sequence-dialog*
               :table-sequence nil
               :table-hscrollp nil
               :dialog-item-font '("Monaco" 9)
               :cell-size #@(490 12)
               :dialog-item-size #@(502 183)
               :dialog-item-position #@(2 125)))
  
  (setf predicate-item-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "Predicate"
               :dialog-item-font '("Chicago" 12)
               :dialog-item-position #@(300 6)))

  (setf predicate-Edit-Buttom
        (oneof *button-dialog-item*
               :dialog-item-text "GoTo"
               :dialog-item-enabled-p nil
               :dialog-item-position #@(258 6)
               :dialog-item-size #@(36 15)
               :dialog-item-font '("Chicago" 12)
               :dialog-item-action
               #'(lambda () 
                   (babylon-edit-prolog-clause
                    (ask axset-item-list (cell-contents (car (selected-cells))))
                    (ask predicate-item-list (cell-contents (car (selected-cells))))))
               ))

  (setf predicate-item-List
        (oneof *ps-sequence-dialog-item*
               :update-function
               #'(lambda ()
                   (ask predicate-display-item (set-dialog-item-text ""))
                   (ask predicate-edit-buttom (dialog-item-disable))
                   (get-predicates *CAS*))
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (funcall #'(lambda (predicate)
                                 (ask predicate-edit-buttom (dialog-item-enable))
                                 (ask predicate-display-item
                                   (set-dialog-item-text 
                                    (print-pred *CAS* predicate "" nil))))
                             (cell-contents (car (selected-cells))))
                    (usual-dialog-item-action))))
               :dialog-item-size #@(250 96)
               :cell-size #@(246 12)
               :dialog-item-position #@(254 27)))
 
  (setf axset-Edit-Buttom
        (oneof *button-dialog-item*
               :dialog-item-text "GoTo"
               :dialog-item-font '("Chicago" 12)
               :dialog-item-enabled-p nil
               :dialog-item-position #@(8 6)
               :dialog-item-size #@(36 15)               
               :dialog-item-action
               #'(lambda () 
                   (babylon-edit-prolog
                    (ask axset-item-list 
                      (cell-contents (car (selected-cells))))))
               ))

  (setf axset-Item-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "AxiomSet"
               :dialog-item-font '("Chicago" 12)
               :dialog-item-position #@(50 6)))
 
  (setf axset-item-list
        (oneof *c-sequence-dialog-item*
               :dependents '(predicate-item-List)
               :update-function
               #'(lambda () 
                   (ask axset-edit-buttom (dialog-item-disable))
                   (ask predicate-edit-buttom (dialog-item-disable))
                   (ask predicate-display-item (set-dialog-item-text ""))
                   (send-prolog :axioms))
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (funcall #'(lambda (axset)
                                 (setf *CAS* axset)
                                 (ask axset-edit-buttom (dialog-item-enable))
                                 (broadcast 'update-table-sequence))
                             (cell-contents (car (selected-cells))))
                    (usual-dialog-item-action))))
               :dialog-item-size #@(250 96)
               :cell-size #@(246 12)
               :dialog-item-position #@(2 27)))
  
  (setf Explore-Axsets-Dialog
        (oneof *bury-dialog* ;*dialog*
               :window-type :document
               :window-size #@(506 310)
               :window-position #@(2 40)
               :window-show nil
               :window-title "AxiomSet Exploration Menu"
               :window-font '("Monaco" 9)
               :dialog-items (list axset-edit-buttom axset-item-header 
                                   predicate-edit-buttom predicate-item-header 
                                   axset-item-list predicate-item-list 
                                   predicate-display-item))))

;;; for dumplisp -----------------------------------------------------------

(allocate-explore-axsets-dialog)

(defun deallocate-explore-axsets-dialog ()
  (ask explore-axsets-dialog (window-close t)))

(progn
  (push (symbol-function 'deallocate-Explore-Axsets-Dialog) 
        *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'allocate-Explore-Axsets-Dialog)))))

;;; -------------------------------------------------------------------------

(def$method (basic-prolog-mixin :explore-axset) ()
  (ask Explore-Axsets-Dialog (window-select)))

;;; -------------------------------------------------------------------------

(def$method (basic-prolog-mixin :after :refresh-yourself) ()
  (setf *CAS* nil)
  (ask axset-item-list (update-table-sequence)))

(def$method (basic-prolog-processor :after :set-axioms) (axsets)
  (let ((*current-knowledge-base* meta-processor)) ; proc may not yet be current
    (setf *CAS* nil)
    (ask axset-Item-List (update-table-sequence))))

;;; -------------------------------------------------------------------------

(def$method (basic-prolog-mixin :after :deselect-kb) ()
  (ask Explore-Axsets-Dialog (window-hide)))

;;; -------------------------------------------------------------------------

;;; eof

