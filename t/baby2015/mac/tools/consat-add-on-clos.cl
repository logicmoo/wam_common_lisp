;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10  -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;  BABYLON  Exploring Constraints (CLOS version)

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
(defvar *Consat-Explore-Menu* nil)
(defvar Consat-Edit-Button nil)
(defvar Consat-Copy-Button nil)

(defclass my-alist-dialog-item (sequence-dialog-item) ())

(defmethod cell-contents ((self my-alist-dialog-item) h &optional v)
  (declare (ignore h v))
  (car (call-next-method)))

(defmethod full-cell-contents ((self my-alist-dialog-item) cell)
  (elt (table-sequence self) (cell-to-index self cell)))

(defmethod value-cell-contents ((self my-alist-dialog-item) cell)
  (cdr (full-cell-contents self cell)))

(defmethod reset-table-sequence ((self my-alist-dialog-item) 
                                 &optional (table-sequence '()))
  (dolist (cell (selected-cells self))
    (cell-deselect self (point-h cell) (point-v cell)))
  (dialog-item-disable Consat-Copy-Button)
  (set-dialog-item-text Consat-Display-Item"")
  (set-table-sequence self table-sequence))

(defun allocate-consat-explore-menu ()
  (setf Consat-Display-Item
        (make-instance 'string-sequence-dialog
                       :table-sequence nil
                       :table-hscrollp nil
                       :view-font '("Monaco" 9)
                       :cell-size #@(490 12)
                       :view-size #@(502 183)
                       :view-position #@(2 115)))
  
  (setf Consat-Item-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "Constraints"
                       :view-font '("Monaco" 9)
                       :view-position #@(256 2)))
  
  (setf Consat-Item-List
        (make-instance 'my-alist-dialog-item
                       :table-sequence '(("" . ""))
                       :dialog-item-action
                       #'(lambda (item)
                           (when (selected-cells item)
                             (let ((term (full-cell-contents item (car (selected-cells item)))))
                               (with-output-to-string (cstr)
                                 ($send (get-object-of-c-assoc term)
                                        :print
                                        (get-name-of-c-assoc term)
                                        cstr)
                                 (dialog-item-enable Consat-Edit-Button)
                                 (dialog-item-enable Consat-Copy-Button)
                                 (set-dialog-item-text consat-display-item
                                                       (get-output-stream-string cstr))))))
                       :view-size #@(250 96)
                       :cell-size #@(246 12)
                       :view-position #@(254 14)
                       :view-font '("Monaco" 9)
                       :table-hscrollp nil))
  
  (setf Consat-Edit-Button
        (make-instance 'button-dialog-item
                       :dialog-item-text "GoTo"
                       :view-font '("Chicago" 12)
                       :dialog-item-enabled-p nil
                       :view-position #@(190 14)
                       :dialog-item-action
                       #'(lambda (item)
                           (declare (ignore item))
                           (babylon-edit (selection Consat-Item-List)))))
  
  (setf Consat-Copy-Button
        (make-instance 'button-dialog-item
          :dialog-item-text "Copy"
          :view-font '("Chicago" 12)
          :dialog-item-enabled-p nil
          :view-position #@(190 34)
          :dialog-item-action
          #'(lambda (item)
              (declare (ignore item))
              (add-to-killed-strings
               (cons
                (get-dialog-item-text Consat-Display-Item)
                nil))
              ;  (vector 1 4 2304 256 0 0 18)))
              #-:CCL-3(set-mini-buffer *top-listener*
                                      "Copied to the kill ring. Use control-y to yank.")
              (dialog-item-disable Consat-Copy-Button))))
  
  
  (setf Consat-Restriction-Button
        (make-instance 'radio-button-dialog-item
                       :dialog-item-text "Restrictions"
                       :view-position #@(20 20)
                       :view-font '("Chicago" 12)
                       :dialog-item-action 
                       #'(lambda (item)
                           (declare (ignore item))
                           (setf *ctype* nil)
                           (dialog-item-disable Consat-Edit-Button)
                           (dialog-item-disable Consat-Primitive-Button)
                           (dialog-item-disable Consat-Compound-Button)
                           (reset-table-sequence Consat-Item-List 
                                                 (send-kb :send-if-handles :restriction-nets)))
                       :radio-button-cluster '*RTOT*
                       :radio-button-pushed-p (not *ctype*)))
  
  (setf Consat-Constraint-Button
        (make-instance 'radio-button-dialog-item
                       :dialog-item-text "Constraints"
                       :view-position #@(20 40)
                       :view-font '("Chicago" 12)
                       :dialog-item-action 
                       #'(lambda (item)
                           (declare (ignore item))
                           (setf *ctype* t)
                           (dialog-item-enable Consat-Primitive-Button)
                           (dialog-item-enable Consat-Compound-Button)
                           (dialog-item-disable Consat-Edit-Button)
                           (reset-table-sequence Consat-Item-List 
                                                 (if *compound* 
                                                   (send-kb :constraint-nets)
                                                   (send-kb :constraints))))
                       :radio-button-cluster '*RTOT*
                       :radio-button-pushed-p *ctype*))
  
  (setf Consat-Primitive-Button
        (make-instance 'radio-button-dialog-item
                       :dialog-item-text "Primitive Constraints"
                       :view-position #@(40 66)
                       :view-font '("Chicago" 12)
                       :dialog-item-action 
                       #'(lambda (item)
                           (declare (ignore item))
                           (setf *compound* nil)
                           (dialog-item-disable Consat-Edit-Button)
                           (reset-table-sequence Consat-Item-List (send-kb :constraints)))
                       :radio-button-cluster '*RTO*
                       :radio-button-pushed-p (not *compound*)))
  
  (setf Consat-Compound-Button
        (make-instance 'radio-button-dialog-item
                       :dialog-item-text "Compound  Constraints"
                       :view-position #@(40 86)
                       :view-font '("Chicago" 12)
                       :dialog-item-action 
                       #'(lambda (item)
                           (declare (ignore item))
                           (setf *compound* t)
                           (dialog-item-disable Consat-Edit-Button)
                           (reset-table-sequence Consat-Item-List (send-kb :constraint-nets)))
                       :radio-button-cluster '*RTO*
                       :radio-button-pushed-p *compound*))
  
  (setf *Consat-Explore-Menu*
        (make-instance 'bury-dialog
                       :window-type :document
                       :view-size #@(506 300)
                       :view-position #@(2 40)
                       :window-show nil
                       :window-title "Constraint Exploration Menu"
                       :view-font '("Monaco" 9)
                       :view-subviews (list Consat-Restriction-Button Consat-Constraint-Button
                                            Consat-Edit-Button Consat-Copy-Button
                                            Consat-Primitive-Button Consat-Compound-Button
                                            Consat-Item-Header Consat-Item-List 
                                            Consat-Display-Item))))


;;; for dumplisp -----------------------------------------------------------

(allocate-Consat-Explore-Menu)

(defun deallocate-Consat-Explore-Menu ()
  (window-deallocate *Consat-Explore-Menu*))

(progn
  (push (symbol-function 'deallocate-Consat-Explore-Menu) *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'allocate-Consat-Explore-Menu)))))

;;; -------------------------------------------------------------------------

(def$method (basic-constraint-mixin :explore-constraint) ()
  (window-select *Consat-Explore-Menu*))

;;; -------------------------------------------------------------------------

(def$method (basic-constraint-mixin :after :refresh-yourself) ()
  (reset-table-sequence Consat-Item-List
                        (if *compound* 
                          ($send self :constraint-nets)
                          ($send self :constraints))))

(def$method (basic-constraint-mixin :after :new&delete) (c-type c-name c-variables c-body
                                                                &optional (c-condition t))
  "this redefines the normal after after method!"
  (declare (ignore c-name c-variables c-body c-condition))
  ($send constraint-processor :set-constraints constraints)            ; old after demon
  ($send constraint-processor :set-constraint-nets constraint-nets)    ; old after demon
  (if (if  *compound*
        (eq c-type 'compound)
        (eq c-type 'primitive))
    (reset-table-sequence Consat-Item-List
                          (if *compound*
                            ($send self :constraint-nets)
                            ($send self :constraints)))))

;;; -------------------------------------------------------------------------

(def$method (basic-constraint-mixin :after :deselect-kb) ()
  (window-hide *Consat-Explore-Menu*))

;;; eof

