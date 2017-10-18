;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright  1988  BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;  Options dialog for the explore commands

(defvar *CRS* nil)   ;curent rule set
(defvar *CRSF* "")   ;filter
(defvar *CRSS* nil)  ;sort

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
(defvar *CSlotSort* t)  ;sort?
(defvar *CSlotAll* t)   ;all?

(defvar *CP* nil)   ;current property
(defvar *CB* nil)   ;current behavior

(defvar dummy nil)


(defobject *my-check-box-dialog-item* *check-box-dialog-item*)

(proclaim '(object-variable (*my-check-box-dialog-item* associated-variable)))

(defobfun (exist *my-check-box-dialog-item*) (init-list)
  (have 'associated-variable (getf init-list :associated-variable 'dummy))
  (usual-exist 
   (init-list-default init-list)))

(defobfun (manifest *my-check-box-dialog-item*) ()
  (setf (symbol-value associated-variable) (check-box-checked-p)))


(defobject *my-editable-text-dialog-item* *editable-text-dialog-item*)

(proclaim '(object-variable (*my-editable-text-dialog-item* associated-variable)))

(defobfun (exist *my-editable-text-dialog-item*) (init-list)
  (have 'associated-variable (getf init-list :associated-variable 'dummy))
  (usual-exist 
   (init-list-default init-list)))

(defobfun (manifest *my-editable-text-dialog-item*) ()
  (setf (symbol-value associated-variable) (string-upcase (dialog-item-text))))


(defobject *configuration-dialog* *dialog*)

(proclaim '(object-variable (*configuration-dialog* my-dialog)))

(defobfun (exist *configuration-dialog*) (init-list)
  (let ((item-list 
         (list 
          (oneof *static-text-dialog-item*             ; frames
                 :dialog-item-position #@(10 10)
                 :dialog-item-text "Frames:")
          (oneof *my-editable-text-dialog-item*
                 :dialog-item-text *CFF*
                 :associated-variable '*CFF*
                 :dialog-item-position #@(90 10)
                 :dialog-item-size #@(150 16))
          (oneof *my-check-box-dialog-item*
                 :dialog-item-position #@(250 10)
                 :dialog-item-text "Sort"
                 :associated-variable '*CFS*
                 :check-box-checked-p *CFS*)
          
          (oneof *static-text-dialog-item*             ; supers
                 :dialog-item-position #@(10 40)
                 :dialog-item-text "Supers:")
          (oneof *my-check-box-dialog-item*
                 :dialog-item-position #@(250 40)
                 :dialog-item-text "Sort"
                 :associated-variable '*CSS*
                 :check-box-checked-p *CSS*)
          (oneof *my-check-box-dialog-item*
                 :dialog-item-position #@(350 40)
                 :dialog-item-text "All"
                 :associated-variable '*CSA*
                 :check-box-checked-p *CSA*)

          (oneof *static-text-dialog-item*             ; instances
                 :dialog-item-position #@(10 70)
                 :dialog-item-text "Instances:")
          (oneof *my-editable-text-dialog-item*
                 :dialog-item-position #@(90 70)
                 :dialog-item-text *CIF*
                 :associated-variable '*CIF*
                 :dialog-item-size #@(150 16))
          (oneof *my-check-box-dialog-item*
                 :dialog-item-position #@(250 70)
                 :dialog-item-text "Sort"
                 :associated-variable '*CIS*
                 :check-box-checked-p *CIS*)
          (oneof *my-check-box-dialog-item*
                 :dialog-item-position #@(350 70)
                 :dialog-item-text "All"
                 :associated-variable '*CIA*
                 :check-box-checked-p *CIA*)

          (oneof *static-text-dialog-item*               ; slots
                 :dialog-item-position #@(10 100)
                 :dialog-item-text "Slots:")
          (oneof *my-check-box-dialog-item*
                 :dialog-item-position #@(250 100)
                 :dialog-item-text "Sort"
                 :associated-variable '*CSlotSort*
                 :check-box-checked-p *CSlotSort*)
          (oneof *my-check-box-dialog-item*
                 :dialog-item-position #@(350 100)
                 :dialog-item-text "All"
                 :associated-variable '*CSlotAll*
                 :check-box-checked-p *CSlotAll*)

          (oneof *static-text-dialog-item*              ; rule sets
                 :dialog-item-position #@(10 130)
                 :dialog-item-text "Rule Sets:")
          (oneof *my-editable-text-dialog-item*
                 :dialog-item-position #@(90 130)
                 :dialog-item-text *CRSF*
                 :associated-variable '*CRSF*
                 :dialog-item-size #@(150 16))
          (oneof *my-check-box-dialog-item*
                 :dialog-item-position #@(250 130)
                 :dialog-item-text "Sort"
                 :associated-variable '*CRSS*
                 :check-box-checked-p *CRSS*)

          (oneof *static-text-dialog-item*              ; rules
                 :dialog-item-position #@(10 160)
                 :dialog-item-text "Rules:")
          (oneof *my-editable-text-dialog-item*
                 :dialog-item-position #@(90 160)
                 :dialog-item-text *CRNF*
                 :associated-variable '*CRNF*
                 :dialog-item-size #@(150 16))
          (oneof *my-check-box-dialog-item*
                 :dialog-item-position #@(250 160)
                 :dialog-item-text "Sort"
                 :associated-variable '*CRNS*
                 :check-box-checked-p *CRNS*)

          (oneof *button-dialog-item*
                 :dialog-item-text "Do It"
                 :dialog-item-action 
                 (nfunction
                  dialog-item-action
                  (lambda ()
                    (dolist (item (ask my-dialog (dialog-items *my-check-box-dialog-item*)))
                      (ask item (manifest)))
                    (dolist (item (ask my-dialog (dialog-items *my-editable-text-dialog-item*)))
                      (ask item (manifest)))
                    (send-kb :refresh-yourself)         ; modified 3.1.89 J.W.
                    (return-from-modal-dialog t)))
                 :dialog-item-position #@(160 200))
          (oneof *button-dialog-item*
                 :dialog-item-text "Abort"
                 :dialog-item-action 
                 (nfunction
                  dialog-item-action
                  (lambda ()
                    (return-from-modal-dialog nil)))
                 :dialog-item-position #@(260 200))
        )))
    (usual-exist
     (init-list-default init-list
                        :window-type :double-edge-box
                        :window-size #@(480 230)
                        :window-position (make-point (ash (- *screen-width* 480) -1) 60)
                        :dialog-items item-list))))

(defvar Explore-Options-Command 
  (oneof *menu-item*
         :menu-item-title "Explore Opts..." 
         :menu-item-action 
         '(modal-dialog (apply #'oneof *configuration-dialog* '()))))

(defobfun (menu-item-update Explore-Options-Command) ()
  (if (and *current-knowledge-base* 
           (or (flavor-typep *current-knowledge-base* 'basic-frame-mixin)
               (flavor-typep *current-knowledge-base* 'basic-rule-mixin)
               (flavor-typep *current-knowledge-base* 'basic-constraint-mixin)))
    (menu-item-enable)
    (menu-item-disable)))

(ask Babylon-Menu (add-menu-items Explore-Options-Command))

;;; eof

