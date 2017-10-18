;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10  -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;  BABYLON  Exploring Constraints

(eval-when (eval compile load)
  (bab-require 'add-on-base)
  (bab-require 'basic-constraint-mixin))


(defvar *ctype* t)      ; t = constraint, nil = restriction
(defvar *compound* nil)   ;current constraint type

(defvar Consat-Display-Item nil)
(defvar Consat-Item-Header nil)
(defvar Consat-Item-List nil)
(defvar Consat-Restriction-Button nil)
(defvar Consat-Constraint-Button nil)
(defvar Consat-Primitive-Button nil)
(defvar Consat-Compound-Button nil)
(defvar Consat-Explore-Menu nil)
(defvar Consat-Edit-Button nil)
(defvar Consat-Copy-Button nil)

(defobject *my-alist-dialog-item* *alist-dialog-item*)

(defobfun (reset-table-sequence *my-alist-dialog-item*) 
          (&optional (table-sequence '()))
  (dolist (cell (selected-cells))
    (cell-deselect (point-h cell) (point-v cell)))
  (ask Consat-Copy-Button (dialog-item-disable))
  (ask Consat-Display-Item (set-dialog-item-text ""))
  (set-table-sequence table-sequence))

(defun allocate-consat-explore-menu ()
  (setf Consat-Display-Item
        (oneof *string-sequence-dialog*
               :table-sequence nil
               :table-hscrollp nil
               :dialog-item-font '("Monaco" 9)
               :cell-size #@(490 12)
               :dialog-item-size #@(502 183)
               :dialog-item-position #@(2 115)))
  
  (setf Consat-Item-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "Constraints"
               :dialog-item-position #@(256 2)))
  
  (setf Consat-Item-List
        (oneof *my-alist-dialog-item*
               :table-sequence '(("" . ""))
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (funcall #'(lambda (term)                                 
                                 (with-output-to-string (cstr)
                                   ($send (get-object-of-c-assoc term)
                                          :print
                                          (get-name-of-c-assoc term)
                                          cstr)
                                   (ask Consat-Edit-Button (dialog-item-enable))
                                   (ask Consat-Copy-Button (dialog-item-enable))
                                   (ask consat-display-item 
                                     (set-dialog-item-text
                                      (get-output-stream-string cstr)
                                      ))))
                             (full-cell-contents (car (selected-cells))))
                    (usual-dialog-item-action))))
               :dialog-item-size #@(250 96)
               :cell-size #@(246 12)
               :dialog-item-position #@(254 14)
               :dialog-item-font '("Monaco" 9)
               :table-hscrollp nil))
  
  (setf Consat-Edit-Button
        (oneof *button-dialog-item*
               :dialog-item-text "GoTo"
               :dialog-item-font '("Chicago" 12)
               :dialog-item-enabled-p nil
               :dialog-item-position #@(204 14)
               :dialog-item-action
               #'(lambda () 
                   (babylon-edit
                    (ask Consat-Item-List (cell-contents (car (selected-cells))))))
               ))
  
  (setf Consat-Copy-Button
        (oneof *button-dialog-item*
               :dialog-item-text "Copy"
               :dialog-item-font '("Chicago" 12)
               :dialog-item-enabled-p nil
               :dialog-item-position #@(204 34)
               :dialog-item-action
               #'(lambda () 
                   (add-to-killed-strings
                    (cons
                     (ask Consat-Display-Item (get-dialog-item-text))
                     (vector 1 4 2304 256 0 0 18)))                  ;;; ???
                   (ask *top-listener* 
                     (set-mini-buffer "Copied to the kill ring. Use control-y to yank."))
                   (ask Consat-Copy-Button (dialog-item-disable)))
               ))
  
  
  (setf Consat-Restriction-Button
        (oneof *radio-button-dialog-item*
               :dialog-item-text "Restrictions"
               :dialog-item-position #@(20 20)
               :dialog-item-font '("Chicago" 12)
               :dialog-item-action 
               '(progn 
                  (setf *ctype* nil)
                  (ask Consat-Edit-Button (dialog-item-disable))
                  (ask Consat-Primitive-Button (dialog-item-disable))
                  (ask Consat-Compound-Button (dialog-item-disable))
                  (ask Consat-Item-List  
                    (reset-table-sequence (send-kb :send-if-handles :restriction-nets)))
                  (usual-dialog-item-action))
               :radio-button-cluster '*RTOT*
               :radio-button-pushed-p (not *ctype*)))
  
  (setf Consat-Constraint-Button
        (oneof *radio-button-dialog-item*
               :dialog-item-text "Constraints"
               :dialog-item-position #@(20 40)
               :dialog-item-font '("Chicago" 12)
               :dialog-item-action 
               '(progn 
                  (setf *ctype* t)
                  (ask Consat-Primitive-Button (dialog-item-enable))
                  (ask Consat-Compound-Button (dialog-item-enable))
                  (ask Consat-Edit-Button (dialog-item-disable))
                  (ask Consat-Item-List  
                    (if *compound*
                      (reset-table-sequence (send-kb :constraint-nets))
                      (reset-table-sequence (send-kb :constraints))))
                  (usual-dialog-item-action))
               :radio-button-cluster '*RTOT*
               :radio-button-pushed-p *ctype*))
  
  (setf Consat-Primitive-Button
        (oneof *radio-button-dialog-item*
               :dialog-item-text "Primitive Constraints"
               :dialog-item-position #@(40 66)
               :dialog-item-font '("Chicago" 12)
               :dialog-item-action 
               '(progn 
                  (setf *compound* nil)
                  (ask Consat-Edit-Button (dialog-item-disable))
                  (ask Consat-Item-List  
                    (reset-table-sequence (send-kb :constraints)))
                  (usual-dialog-item-action))
               :radio-button-cluster '*RTO*
               :radio-button-pushed-p (not *compound*)))
  
  (setf Consat-Compound-Button
        (oneof *radio-button-dialog-item*
               :dialog-item-text "Compound  Constraints"
               :dialog-item-position #@(40 86)
               :dialog-item-font '("Chicago" 12)
               :dialog-item-action 
               '(progn 
                  (setf *compound* t)
                  (ask Consat-Edit-Button (dialog-item-disable))
                  (ask Consat-Item-List
                    (reset-table-sequence (send-kb :constraint-nets)))
                  (usual-dialog-item-action))
               :radio-button-cluster '*RTO*
               :radio-button-pushed-p *compound*))
  
  (setf Consat-Explore-Menu
        (oneof *bury-dialog*
               :window-type :document
               :window-size #@(506 300)
               :window-position #@(2 40)
               :window-show nil
               :window-title "Constraint Exploration Menu"
               :window-font '("Monaco" 9)
               :default-button nil
               :dialog-items (list Consat-Restriction-Button Consat-Constraint-Button
                                   Consat-Edit-Button Consat-Copy-Button
                                   Consat-Primitive-Button Consat-Compound-Button
                                   Consat-Item-Header Consat-Item-List 
                                   Consat-Display-Item))))


;;; for dumplisp -----------------------------------------------------------

(allocate-Consat-Explore-Menu)

(defun deallocate-Consat-Explore-Menu ()
  (ask Consat-Explore-Menu (window-close t)))

(progn
  (push (symbol-function 'deallocate-Consat-Explore-Menu) *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'allocate-Consat-Explore-Menu)))))

;;; -------------------------------------------------------------------------

(def$method (basic-constraint-mixin :explore-constraint) ()
  (ask Consat-Explore-Menu (window-select)))

;;; -------------------------------------------------------------------------

(def$method (basic-constraint-mixin :after :refresh-yourself) ()
  (ask Consat-Item-List
    (reset-table-sequence 
     (if *compound* 
       ($send self :constraint-nets)
       ($send self :constraints)))))

(def$method (basic-constraint-mixin :after :new&delete) (c-type c-name c-variables c-body
                                                                &optional (c-condition t))
  "this redefines the normal after after method!"
  (declare (ignore c-name c-variables c-body c-condition))
  ($send constraint-processor :set-constraints constraints)            ; old after demon
  ($send constraint-processor :set-constraint-nets constraint-nets)    ; old after demon
  (if (if  *compound*
        (eq c-type 'compound)
        (eq c-type 'primitive))
    (ask Consat-Item-List
      (reset-table-sequence
       (if *compound*
         ($send self :constraint-nets)
         ($send self :constraints))))))

;;; -------------------------------------------------------------------------

(def$method (basic-constraint-mixin :after :deselect-kb) ()
  (ask Consat-Explore-Menu (window-hide)))

;;; eof

