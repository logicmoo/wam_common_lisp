;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10  -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG
 
;;  AUTHOR:  Juergen Walther

;;  BABYLON  exploring frames

(eval-when (eval compile load)
  (bab-require 'add-on-base)
  (bab-require 'basic-frame-mixin))

(defvar Frame-Display-Item nil)
(defvar Frame-Property-Header nil)
(defvar Frame-Property-Item-List nil)
(defvar Frame-Slot-Header nil)
(defvar Frame-Slot-Item-List nil)
(defvar Frame-Instance-Header nil)
(defvar Frame-Instance-Item-List nil)
(defvar Frame-Behavior-Header nil)
(defvar Frame-Behavior-Item-List nil)
(defvar Frame-Supers-Header nil)
(defvar Frame-Supers-Item-List nil)
(defvar Frame-Frame-Header nil)
(defvar Frame-Frame-Item-List nil)
(defvar Frame-Frame-Menu nil)
(defvar Frame-Edit-Buttom nil)
(defvar Instance-Edit-Buttom nil)
(defvar Behavior-Edit-Buttom nil)
(defvar Behavior-Trace-Buttom nil)

(defun get-description-string ()
  (let* ((method-spec
          (find-if #'(lambda (spec)
                       (equal (car spec) (append (list *CF*) *CB*)))
                   (getf (symbol-plist 
                          (%get-object-name *CF* (send-kb :pkg)))
                         :behaviors)))
         (parms (second method-spec))
         (docu (third method-spec)))
    (format nil "~S~%~A" parms (if (stringp docu) docu ""))))

(defun new-current-frame (frame)
  (setf *CF* frame)
  ; frame in frame-frame-item-list selectieren
  (let* ((new-cell (ask Frame-Frame-Item-List 
                    (index-to-cell (position frame (table-sequence)))))
         (sel-cell (ask Frame-Frame-Item-List 
                    (car (selected-cells))))
         (h-nc (point-h new-cell))
         (v-nc (point-v new-cell)))
    (ask Frame-Frame-Item-List 
      (cell-deselect (point-h sel-cell) (point-v sel-cell))
      (cell-select h-nc v-nc)
      (scroll-to-cell h-nc v-nc)))
  (ask Frame-Frame-Item-List (broadcast 'update-table-sequence)))

(defun %trace-method (trace)
  (let ((class (symbol-value (%make-object-name *cf*)))
        (selector (if (rest *cb*)
                    (second *cb*)
                    (first *cb*))))
    (if trace
      (mcs-trace class selector)
      (mcs-untrace class selector))))

(defun %is-traced-method ()
  (mcs-is-traced (symbol-value (%make-object-name *cf*)) 
                 (if (rest *cb*)
                   (second *cb*)
                   (first *cb*))))

(defun allocate-frame-frame-menu ()
  (setf Frame-Display-Item
        (oneof *string-sequence-dialog*
               :table-sequence nil
               :table-hscrollp nil
               :dialog-item-font '("Monaco" 9)
               :cell-size #@(490 12)
               :dialog-item-size #@(502 73)
               :dialog-item-position #@(2 225)))
  
  (setf Frame-Property-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "Properties"
               :dialog-item-position #@(340 114)))
  
  (setf Frame-Property-Item-List
        (oneof *c-sequence-dialog-item*
               :update-function 
               #'(lambda ()
                   (progn 
                     (ask Frame-Display-Item (set-dialog-item-text ""))
                     (if *CI* ($send (get-instance *CI*) :get-properties *CS*))))
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (funcall #'(lambda (property)
                                 (setf *CP* property)
                                 (ask Frame-Behavior-Item-List (deselect))
                                 (ask Behavior-Edit-Buttom (dialog-item-disable))
                                 (ask Behavior-Trace-Buttom (dialog-item-disable))
                                 (let ((object ($send (get-instance *CI*) :get-value-only *CS* *CP*)))
                                   (ask Frame-Display-Item 
                                     (set-dialog-item-text
                                      (if (flavor-instancep object)
                                        (with-output-to-string (*default-dialog-stream*) 
                                          ($send object :describe-short))
                                        (format nil "~S" object))))))
                             (cell-contents (car (selected-cells))))
                    (usual-dialog-item-action))))
               :dialog-item-position #@(338 126)))
  
  (setf Frame-Slot-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "Slots"
               :dialog-item-position #@(340 2)))
  
  (setf Frame-Slot-Item-List
        (oneof *c-sequence-dialog-item*
               :dependents '(Frame-Property-Item-List)
               :update-function
               #'(lambda () 
                   (let ((il (if *CSlotAll* (<- *CI* :slots)
                                 (get-frame-slot-names *CF*))))
                     (if *CSlotSort* (sort (copy-list il) #'string-lessp) il)))
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (funcall #'(lambda (slot)
                                 (setf *CS* slot)
                                 (broadcast 'update-table-sequence))
                             (cell-contents (car (selected-cells))))
                    (usual-dialog-item-action))))
               :dialog-item-position #@(338 14)))
  
  (setf Frame-Instance-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "Instances"
               :dialog-item-position #@(172 2)))
  
  (setf Instance-Edit-Buttom
        (oneof *button-dialog-item*
               :dialog-item-text ">"
               :dialog-item-enabled-p nil
               :dialog-item-position #@(240 1)
               :dialog-item-action
               #'(lambda () 
                   (babylon-edit
                    (ask Frame-Instance-Item-List 
                      (cell-contents (car (selected-cells))))))))
  
  (setf Frame-Instance-Item-List
        (oneof *c-sequence-dialog-item*
               :dependents '(Frame-Slot-Item-List)
               :update-function 
               #'(lambda ()
                   (progn
                     (ask Instance-Edit-Buttom (dialog-item-disable))
                     (let ((il (filter-list (if *CIA* 
                                              (get-all-instances *CF*)
                                              (get-instance-list *CF*))
                                            *CIF*)))
                       (if *CIS* (sort il #'string-lessp) il))))
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (funcall #'(lambda (instance)
                                 (setf *CI* instance)
                                 (broadcast 'update-table-sequence))
                             (cell-contents (car (selected-cells))))
                    (ask Instance-Edit-Buttom (dialog-item-enable))
                    (usual-dialog-item-action))))
               :dialog-item-position #@(170 14)))
  
  (setf Frame-Behavior-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "Behaviors"
               :dialog-item-position #@(172 114)))
  
  (setf Behavior-Edit-Buttom
        (oneof *button-dialog-item*
               :dialog-item-text ">"
               :dialog-item-enabled-p nil
               :dialog-item-position #@(240 113)
               :dialog-item-action
               #'(lambda () 
                   (babylon-edit
                    `(,(ask Frame-Frame-Item-List 
                         (cell-contents (car (selected-cells))))
                      . ,(ask Frame-Behavior-Item-List 
                           (cell-contents (car (selected-cells)))))))))
  
  (setf Behavior-Trace-Buttom
        (oneof *button-dialog-item*
               :dialog-item-text " "
               :dialog-item-enabled-p nil
               :dialog-item-position #@(270 113)
               :dialog-item-action
               #'(lambda ()
                   (let ((traced (%is-traced-method)))
                     (%trace-method (not traced))
                     (ask Behavior-Trace-Buttom (set-dialog-item-text (if traced "t" "u")))))))
  
  (setf Frame-Behavior-Item-List
        (oneof *c-sequence-dialog-item*
               :update-function
               #'(lambda ()
                   (progn
                     (ask Frame-Display-Item (set-dialog-item-text ""))
                     (ask Behavior-Trace-Buttom  
                       (set-dialog-item-text " ") (dialog-item-disable))
                     (ask Behavior-Edit-Buttom (dialog-item-disable))
                     (mapcar #'(lambda (spec)
                                 (rest spec))
                             (get-frame-behavior-specs *CF*))))
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (funcall #'(lambda (behavior)
                                 (setf *CB* behavior))
                             (cell-contents (car (selected-cells))))
                    (ask Behavior-Edit-Buttom (dialog-item-enable))
                    (ask Behavior-Trace-Buttom 
                      (set-dialog-item-text (if (%is-traced-method) "u" "t"))
                      (dialog-item-enable))
                    (ask Frame-Property-Item-List (deselect))
                    (ask Frame-Display-Item (set-dialog-item-text (get-description-string)))                    
                    (usual-dialog-item-action))))
               :dialog-item-position #@(170 126)))
  
  (setf Frame-Supers-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "Super Frames"
               :dialog-item-position #@(4 114)))
  
  (setf Frame-Supers-Item-List
        (oneof *c-sequence-dialog-item*
               :update-function 
               #'(lambda () 
                   (let ((sl (filter-list (if *CSA* 
                                            (get-all-supers *CF*)
                                            (get-supers *CF*))
                                          *CSF*)))
                     (if *CSS* (sort sl #'string-lessp) sl)))
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (funcall #'(lambda (Super)
                                 (new-current-frame Super))
                             (cell-contents (car (selected-cells))))
                    (usual-dialog-item-action))))
               :dialog-item-position #@(2 126)))
  
  (setf Frame-Frame-Header
        (oneof *static-text-dialog-item*
               :dialog-item-text "Frames"
               :dialog-item-position #@(4 2)))
  
  (setf Frame-Edit-Buttom
        (oneof *button-dialog-item*
               :dialog-item-text ">"
               :dialog-item-enabled-p nil
               :dialog-item-position #@(50 1)
               :dialog-item-action
               #'(lambda () 
                   (babylon-edit
                    (ask Frame-Frame-Item-List 
                      (cell-contents (car (selected-cells))))))
               ))
  
  (setf Frame-Frame-Item-List
        (oneof *c-sequence-dialog-item*
               :dependents '(Frame-Instance-Item-List
                             Frame-Behavior-Item-List
                             Frame-Supers-Item-List)
               :update-function 
               #'(lambda ()
                   (progn
                     (ask Frame-Edit-Buttom (dialog-item-disable))
                     (ask Instance-Edit-Buttom (dialog-item-disable))
                     (ask Behavior-Edit-Buttom (dialog-item-disable))
                     (let ((fl (filter-list (send-kb :send-if-handles :frames) *CFF*)))
                       (if *CFS* (sort fl #'string-lessp) fl))))
               :dialog-item-action
               (nfunction
                dialog-item-action
                (lambda ()
                  (when (selected-cells)
                    (funcall #'(lambda (Frame)
                                 (setf *CF* Frame)
                                 (broadcast 'update-table-sequence))
                             (cell-contents (car (selected-cells))))
                    (ask Frame-Edit-Buttom (dialog-item-enable))
                   (usual-dialog-item-action))))
               :dialog-item-position #@(2 14)))
  
  (setf Frame-Frame-Menu
        (oneof *bury-dialog*
               :window-title "Frame Exploration Menu"
               :window-type :document
               :window-size #@(506 300)
               :window-position #@(2 40)
               :window-show nil
               :window-font '("Monaco" 9)
               :default-button nil
               :dialog-items 
               (list Frame-Frame-Header Frame-Edit-Buttom 
                     Frame-Instance-Header Instance-Edit-Buttom 
                     Frame-Slot-Header 
                     Frame-Frame-Item-List Frame-Instance-Item-List Frame-Slot-Item-List
                     Frame-Supers-Header 
                     Frame-Behavior-Header Behavior-Edit-Buttom Behavior-Trace-Buttom
                     Frame-Property-Header 
                     Frame-Supers-Item-List Frame-Behavior-Item-List Frame-Property-Item-List
                     Frame-Display-Item))))
 
(allocate-frame-frame-menu)

(defun deallocate-frame-frame-menu ()
  (ask frame-frame-menu (window-close t)))

(progn
  (push (symbol-function 'deallocate-frame-frame-menu) *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'allocate-frame-frame-menu)))))
  
;;; -------------------------------------------------------------------------

(def$method (basic-frame-mixin :explore-objects) ()
  (ask Frame-Frame-Menu (window-select)))

;;; -------------------------------------------------------------------------

(defun reset-frame-explore-dialog ()
  (setf *CF* nil)
  (ask Frame-Frame-Item-List (update-table-sequence))
  (ask Frame-Display-Item (set-dialog-item-text "")))

(def$method (basic-frame-mixin :after :refresh-yourself) ()
  (reset-frame-explore-dialog))

(def$method (basic-frame-mixin :after :add-to-frames) (a-frame-name)
  (declare (ignore a-frame-name))
  (reset-frame-explore-dialog))

(def$method (basic-frame-mixin :after :add-to-instances) (an-instance-name)
  (if (eq *CF* (<- an-instance-name :type)) 
    (reset-frame-explore-dialog)))

(def$method (frame-base :after :new-behavior-form) 
            (behavior-specification lambda-list behavior-body)
  (declare (ignore lambda-list behavior-body))
  (if (eq *CF* (first behavior-specification))
    (reset-frame-explore-dialog)))

;;; -------------------------------------------------------------------------

(def$method (basic-frame-mixin :after :deselect-kb) ()
  (ask Frame-Frame-Menu (window-hide)))

;;; eof

