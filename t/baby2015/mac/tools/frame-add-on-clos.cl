;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10  -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG
 
;;  AUTHOR:  Juergen Walther

;;  BABYLON  exploring frames (CLOS version)

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
                          (%get-object-name *CF* ($send *current-knowledge-base* :pkg)))
                         :behaviors)))
         (parms (second method-spec))
         (docu (third method-spec)))
    (format nil "~S~%~A" parms (if (stringp docu) docu ""))))

(defun new-current-frame (frame)
  (setf *CF* frame)
  ; frame in frame-frame-item-list selectieren
  (let* ((new-cell (index-to-cell Frame-Frame-Item-List 
                                  (position frame (table-sequence Frame-Frame-Item-List))))
         (sel-cell (car (selected-cells Frame-Frame-Item-List)))
         (h-nc (point-h new-cell))
         (v-nc (point-v new-cell)))
    (cell-deselect Frame-Frame-Item-List (point-h sel-cell) (point-v sel-cell))
    (cell-select Frame-Frame-Item-List h-nc v-nc)
    (scroll-to-cell Frame-Frame-Item-List h-nc v-nc)
    (broadcast Frame-Frame-Item-List 'update-table-sequence)))

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
        (make-instance 'string-sequence-dialog
                       :table-sequence nil
                       :table-hscrollp nil
                       :view-font '("Monaco" 9)
                       :cell-size #@(490 12)
                       :view-size #@(502 73)
                       :view-position #@(2 225)))
  
  (setf Frame-Property-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "Properties"
                       :view-font '("Monaco" 9)
                       :view-position #@(340 114)))
  
  (setf Frame-Property-Item-List
        (make-instance 'c-sequence-dialog-item
                       :view-position #@(338 126)
                       :update-function 
                       #'(lambda ()
                           (progn 
                             (set-dialog-item-text Frame-Display-Item "")
                             (if *CI* ($send (get-instance *CI*) :get-properties *CS*))))
                       :dialog-item-action
                       #'(lambda (item)
                           (when (selected-cells item)
                             (setf *CP* (selection item))  ; (car (selected-cells item)))
                             (deselect Frame-Behavior-Item-List)
                             (dialog-item-disable Behavior-Edit-Buttom)
                             (dialog-item-disable Behavior-Trace-Buttom)
                             (let ((object ($send (get-instance *CI*) :get-value-only *CS* *CP*)))
                               (set-dialog-item-text Frame-Display-Item 
                                (if (flavor-instancep object)
                                  (with-output-to-string (*default-dialog-stream*) 
                                    ($send object :describe-short))
                                  (format nil "~S" object))))))))
  
  (setf Frame-Slot-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "Slots"
                       :view-font '("Monaco" 9)
                       :view-position #@(340 2)))
  
  (setf Frame-Slot-Item-List
        (make-instance 'c-sequence-dialog-item
                       :view-position #@(338 14)
                       :dependents '(Frame-Property-Item-List)
                       :update-function
                       #'(lambda () 
                           (let ((il (if *CSlotAll* (<- *CI* :slots)
                                         (get-frame-slot-names *CF*))))
                             (if *CSlotSort* (sort (copy-list il) #'string-lessp) il)))
                       :dialog-item-action
                       #'(lambda (item)
                           (when (selected-cells item)
                             (setf *CS* (selection item))
                             (broadcast item 'update-table-sequence)))))
  
  (setf Frame-Instance-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "Instances"
                       :view-font '("Monaco" 9)
                       :view-position #@(172 2)))
  
  (setf Instance-Edit-Buttom
        (make-instance 'button-dialog-item
                       :dialog-item-text ">"
                       :view-font '("Monaco" 9)
                       :dialog-item-enabled-p nil
                       :view-position #@(240 1)
                       :view-size #@(14 11)
                       :dialog-item-action
                       #'(lambda (item) 
                           (declare (ignore item))
                           (babylon-edit
                            (selection Frame-Instance-Item-List)))))
  
  (setf Frame-Instance-Item-List
        (make-instance 'c-sequence-dialog-item
                       :view-position #@(170 14)
                       :dependents '(Frame-Slot-Item-List)
                       :update-function 
                       #'(lambda ()
                           (progn
                             (dialog-item-disable Instance-Edit-Buttom)
                             (let ((il (filter-list (if *CIA* 
                                                      (get-all-instances *CF*)
                                                      (get-instance-list *CF*))
                                                    *CIF*)))
                               (if *CIS* (sort il #'string-lessp) il))))
                       :dialog-item-action
                       #'(lambda (item)
                           (when (selected-cells item)
                             (setf *CI* (selection item))
                             (dialog-item-enable Instance-Edit-Buttom)
                             (broadcast item 'update-table-sequence)))))
  
  
  (setf Frame-Behavior-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "Behaviors"
                       :view-font '("Monaco" 9)
                       :view-position #@(172 114)))
  
  (setf Behavior-Edit-Buttom
        (make-instance 'button-dialog-item
                       :dialog-item-text ">"
                       :view-font '("Monaco" 9)
                       :dialog-item-enabled-p nil
                       :view-position #@(240 113)
                       :view-size #@(14 11)
                       :dialog-item-action
                       #'(lambda (item) 
                           (declare (ignore item))
                           (babylon-edit
                            `(,(selection Frame-Frame-Item-List)
                              . ,(selection Frame-Behavior-Item-List))))))
  
  (setf Behavior-Trace-Buttom
        (make-instance 'button-dialog-item
                       :dialog-item-text " "
                       :view-font '("Monaco" 9)
                       :dialog-item-enabled-p nil
                       :view-position #@(270 113)
                       :view-size #@(14 11)
                       :dialog-item-action
                       #'(lambda (item)
                           (declare (ignore item))
                           (let ((traced (%is-traced-method)))
                             (%trace-method (not traced))
                             (set-dialog-item-text Behavior-Trace-Buttom(if traced "t" "u"))))))
  
  (setf Frame-Behavior-Item-List
        (make-instance 'c-sequence-dialog-item
                       :view-position #@(170 126)
                       :update-function
                       #'(lambda ()
                           (progn
                             (set-dialog-item-text Frame-Display-Item "")
                             (set-dialog-item-text Behavior-Trace-Buttom" ") 
                             (dialog-item-disable Behavior-Trace-Buttom)
                             (dialog-item-disable Behavior-Edit-Buttom)
                             (mapcar #'(lambda (spec)
                                         (rest spec))
                                     (get-frame-behavior-specs *CF*))))
                       :dialog-item-action
                       #'(lambda (item)
                           (when (selected-cells item)
                             (setf *CB* (selection item))
                             (dialog-item-enable Behavior-Edit-Buttom)
                             (set-dialog-item-text Behavior-Trace-Buttom (if (%is-traced-method) "u" "t"))
                             (dialog-item-enable Behavior-Trace-Buttom)
                             (deselect Frame-Property-Item-List)
                             (set-dialog-item-text Frame-Display-Item (get-description-string))))))
  
  (setf Frame-Supers-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "Super Frames"
                       :view-font '("Monaco" 9)
                       :view-position #@(4 114)))
  
  (setf Frame-Supers-Item-List
        (make-instance 'c-sequence-dialog-item
                       :view-position #@(2 126)
                       :update-function 
                       #'(lambda () 
                           (let ((sl (filter-list (if *CSA* 
                                                    (get-all-supers *CF*)
                                                    (get-supers *CF*))
                                                  *CSF*)))
                             (if *CSS* (sort sl #'string-lessp) sl)))
                       :dialog-item-action
                       #'(lambda (item)
                           (if (selected-cells item)
                             (new-current-frame (selection item))))))
  
  (setf Frame-Frame-Header
        (make-instance 'static-text-dialog-item
                       :dialog-item-text "Frames"
                       :view-font '("Monaco" 9)
                       :view-position #@(4 2)))
  
  (setf Frame-Edit-Buttom
        (make-instance 'button-dialog-item
                       :dialog-item-text ">"
                       :view-font '("Monaco" 9)
                       :dialog-item-enabled-p nil
                       :view-position #@(50 1)
                       :view-size #@(14 11)
                       :dialog-item-action
                       #'(lambda (item) 
                           (declare (ignore item))
                           (babylon-edit
                            (selection Frame-Frame-Item-List)))))
  
  (setf Frame-Frame-Item-List
        (make-instance 'c-sequence-dialog-item
                       :view-position #@(2 14)
                       :dependents '(Frame-Instance-Item-List
                                     Frame-Behavior-Item-List
                                     Frame-Supers-Item-List)
                       :update-function 
                       #'(lambda ()
                           (progn
                             (dialog-item-disable Frame-Edit-Buttom)
                             (dialog-item-disable Instance-Edit-Buttom)
                             (dialog-item-disable Behavior-Edit-Buttom)
                             (let ((fl (filter-list ($send *current-knowledge-base* :send-if-handles :frames) *CFF*)))
                               (if *CFS* (sort fl #'string-lessp) fl))))
                       :dialog-item-action
                       #'(lambda (item)
                           (when (selected-cells item)
                             (setf *CF* (selection item))
                             (dialog-item-enable Frame-Edit-Buttom)
                             (broadcast item 'update-table-sequence)))))
  
  
  (setf Frame-Frame-Menu
        (make-instance 'bury-dialog
                       :window-title "Frame Exploration Dialog"
                       :window-type :document
                       :view-size #@(506 300)
                       :view-position #@(2 40)
                       :window-show nil
                       :view-font '("Monaco" 9)
                       :view-subviews
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
  (window-deallocate frame-frame-menu))

(progn
  (push (symbol-function 'deallocate-frame-frame-menu) *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'allocate-frame-frame-menu)))))
  
;;; -------------------------------------------------------------------------

(def$method (basic-frame-mixin :explore-objects) ()
  (window-select Frame-Frame-Menu))

;;; -------------------------------------------------------------------------

(defun reset-frame-explore-dialog ()
  (setf *CF* nil)
  (update-table-sequence Frame-Frame-Item-List)
  (set-dialog-item-text Frame-Display-Item ""))

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
  (window-hide Frame-Frame-Menu))

;;; eof

