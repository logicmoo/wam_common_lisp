;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright  1988  BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;  Options dialog for the explore commands (CLOS version)

(defvar *CRS* nil)   ;curent rule set
(defvar *CRSF* "")   ;filter
(defvar *CRSS* nil)  ;sort

(defvar *CR* nil)    ; current rule
(defvar *CRNF* "")   ;filter for rule names
(defvar *CRNS* nil)  ;sort

(defvar *RTO* 'all)       ; filter option
(defvar *RTRS* NIL)       ; current rule set
(defvar *RTCFT* NIL)      ; current first term
(defvar *RTCST* NIL)      ; current second term
(defvar *RTALL* NIL)      ; all terms of current rule set

(defvar *CF* nil)   ;current frame
(defvar *CFF* "")   ;filter
(defvar *CFS* nil)    ;sort?

(defvar *CI* nil)   ;current instance
(defvar *CIA* nil)  ;all?
(defvar *CIF* "")   ;filter
(defvar *CIS* nil)    ;sort?

(defvar *CS* nil)   ;current supers
(defvar *CSA* nil)  ;all?
(defvar *CSF* "")   ;filter
(defvar *CSS* nil)    ;sort?

(defvar *CS* nil)   ;current slot
(defvar *CSlotSort* nil)  ;sort?
(defvar *CSlotAll* nil)   ;all?

(defvar *CP* nil)   ;current property
(defvar *CB* nil)   ;current behavior

(defvar dummy nil)

(defclass my-check-box-dialog-item (check-box-dialog-item)
  ((associated-variable :initarg :associated-variable
                        :initform 'dummy
                        :accessor associated-variable)))

(defmethod manifest ((item my-check-box-dialog-item))
  (setf (symbol-value (associated-variable item)) 
        (check-box-checked-p item)))

(defclass my-editable-text-dialog-item (editable-text-dialog-item)
  ((associated-variable :initarg :associated-variable
                        :initform 'dummy
                        :accessor associated-variable)))

(defmethod manifest ((item my-editable-text-dialog-item))
  (setf (symbol-value (associated-variable item)) 
        (string-upcase (dialog-item-text item))))

(defclass configuration-dialog (dialog)
  ()
  (:default-initargs 
    :window-type :double-edge-box
    :window-show nil
    :view-size #@(480 230)
    :view-position (make-point (ash (- *screen-width* 480) -1) 60)))

(defmethod initialize-instance ((self configuration-dialog) &rest initargs)
  (apply #'call-next-method self initargs)
  (add-subviews self
                (make-instance 'static-text-dialog-item    ; frames
                               :view-position #@(10 10)
                               :dialog-item-text "Frames:")
                (make-instance 'my-editable-text-dialog-item
                               :dialog-item-text *CFF*
                               :associated-variable '*CFF*
                               :view-position #@(90 10)
                               :view-size #@(150 16))
                (make-instance 'my-check-box-dialog-item
                               :view-position #@(250 10)
                               :dialog-item-text "Sort"
                               :associated-variable '*CFS*
                               :check-box-checked-p *CFS*)
                
                (make-instance 'static-text-dialog-item     ; supers
                               :view-position #@(10 40)
                               :dialog-item-text "Supers:")
                (make-instance 'my-check-box-dialog-item
                               :view-position #@(250 40)
                               :dialog-item-text "Sort"
                               :associated-variable '*CSS*
                               :check-box-checked-p *CSS*)
                (make-instance 'my-check-box-dialog-item
                               :view-position #@(350 40)
                               :dialog-item-text "All"
                               :associated-variable '*CSA*
                               :check-box-checked-p *CSA*)
                
                (make-instance 'static-text-dialog-item    ; instances
                               :view-position #@(10 70)
                               :dialog-item-text "Instances:")
                (make-instance 'my-editable-text-dialog-item
                               :view-position #@(90 70)
                               :dialog-item-text *CIF*
                               :associated-variable '*CIF*
                               :view-size #@(150 16))
                (make-instance 'my-check-box-dialog-item
                               :view-position #@(250 70)
                               :dialog-item-text "Sort"
                               :associated-variable '*CIS*
                               :check-box-checked-p *CIS*)
                (make-instance 'my-check-box-dialog-item
                               :view-position #@(350 70)
                               :dialog-item-text "All"
                               :associated-variable '*CIA*
                               :check-box-checked-p *CIA*)
                
                (make-instance 'static-text-dialog-item     ; slots
                               :view-position #@(10 100)
                               :dialog-item-text "Slots:")
                (make-instance 'my-check-box-dialog-item
                               :view-position #@(250 100)
                               :dialog-item-text "Sort"
                               :associated-variable '*CSlotSort*
                               :check-box-checked-p *CSlotSort*)
                (make-instance 'my-check-box-dialog-item
                               :view-position #@(350 100)
                               :dialog-item-text "All"
                               :associated-variable '*CSlotAll*
                               :check-box-checked-p *CSlotAll*)
                
                (make-instance 'static-text-dialog-item    ; rule sets
                               :view-position #@(10 130)
                               :dialog-item-text "Rule Sets:")
                (make-instance 'my-editable-text-dialog-item
                               :view-position #@(90 130)
                               :dialog-item-text *CRSF*
                               :associated-variable '*CRSF*
                               :view-size #@(150 16))
                (make-instance 'my-check-box-dialog-item
                               :view-position #@(250 130)
                               :dialog-item-text "Sort"
                               :associated-variable '*CRSS*
                               :check-box-checked-p *CRSS*)
                
                (make-instance 'static-text-dialog-item      ; rules
                               :view-position #@(10 160)
                               :dialog-item-text "Rules:")
                (make-instance 'my-editable-text-dialog-item
                               :view-position #@(90 160)
                               :dialog-item-text *CRNF*
                               :associated-variable '*CRNF*
                               :view-size #@(150 16))
                (make-instance 'my-check-box-dialog-item
                               :view-position #@(250 160)
                               :dialog-item-text "Sort"
                               :associated-variable '*CRNS*
                               :check-box-checked-p *CRNS*)

                (make-instance 'static-text-dialog-item 
                               :view-position #@(90 180)
                               :dialog-item-text "filter pattern:"
                               :view-font '("Monaco" 9))
                
                (make-instance 'button-dialog-item
                               :dialog-item-text "Do It"
                               :dialog-item-action 
                               #'(lambda (self)
                                   (dolist (item (dialog-items 
                                                  (view-container self) 'my-check-box-dialog-item))
                                     (manifest item))
                                   (dolist (item (dialog-items 
                                                  (view-container self) 'my-editable-text-dialog-item))
                                     (manifest item))
                                   (if *current-knowledge-base*
                                     (send-kb :refresh-yourself))
                                   (return-from-modal-dialog t))
                               :view-position #@(160 200))
                (make-instance 'button-dialog-item
                               :dialog-item-text "Abort"
                               :dialog-item-action 
                               #'(lambda (self)
                                   (declare (ignore self))
                                   (return-from-modal-dialog nil))
                               :view-position #@(260 200))

                (make-instance 'static-text-dialog-item     ; legende
                               :view-position #@(320 130)
                               :dialog-item-text "within pattern:"
                               :view-font '("Monaco" 9))
                (make-instance 'static-text-dialog-item
                               :view-position #@(322 150)
                               :dialog-item-text "? = one character"
                               :view-font '("Monaco" 9))
                (make-instance 'static-text-dialog-item
                               :view-position #@(322 160)
                               :dialog-item-text "* = one or more character"
                               :view-font '("Monaco" 9)))
  (window-select self))


(defvar Explore-Options-Command
  (make-instance 'menu-item
                 :menu-item-title "Explore Opts..." 
                 :menu-item-action 
                 #'(lambda ()
                     (modal-dialog (apply #'make-instance 'configuration-dialog '())))))

(add-menu-items Babylon-Menu Explore-Options-Command)

;;; eof



