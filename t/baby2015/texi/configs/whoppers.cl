;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;; changes (all included in 2.2)

#|
;;; correct defconstraint<e> error from kernel>consat>basic>cstrnet
(def$method (constraint-net :print) (name stream)
  
  ;;;	Ausgabe des Constraint-Netzes
  
  (princ " " stream)
  (terpri stream)
  (babpprint
    `(defconstraint ,name
       (:type compound)
       (:interface . ,interface)
       (:constraint-expressions . ,(select-all-constraints net-spec)))
    stream)
  (terpri stream))

;;; redefines CALL-NEXT-METHOD from mcs/mcs-core
(defmacro CALL-NEXT-METHOD (&rest changed-args)
  (if changed-args
    `(call-next-method-fn self class-env inst-env mcs%caller mcs%next-methods 
                          ',changed-args)
    `(call-next-method-fn self class-env inst-env mcs%caller mcs%next-methods mcs%args)
    ))

;;; addition to mcs/mcs-util
(defmethod (standard-object :describe-short) ()
  (format t "an object of class ~S with instance variable values:~%~S" 
          (get-class-slot 'name)
          (rest (mapcar #'(lambda (ivar)
                      `(,ivar ,(slot-value self ivar)))
                  (get-class-slot 'all-slots)))))

;;; additions to mcs/mcs-map
(defun flavor-instancep (object)
  (typep object 'mcsobject))

(defmacro defwhopper ((flavor-name operation) arglist &body body)
  `(def$method (,flavor-name :around ,operation) (,@arglist)
     ,@body))

(defmacro continue-whopper (&rest changed-args)
  (if changed-args
    `(call-next-method-fn self class-env inst-env mcs%caller mcs%next-methods 
                          (list ,@changed-args))
    `(call-next-method-fn self class-env inst-env mcs%caller mcs%next-methods 
                          mcs%args)
    ))


#|

;;; a whopper (better :around method combination) allows you:
;;; 1. to modify parameters before passing them
;;; 2. to modify the result of the whole method passing event
;;; this can't be done using demon method combination

(def$flavor foo
  ((x 1)
   (y 2))
  ()
  :settable-instance-variables)

(def$method (foo :test) (arg)
  (print arg))

(defwhopper (foo :test) (arg)
  (let ((uuu (continue-whopper (1+ arg))))                  ;pass modified parameter
    (format nil "Result from primary + 1 is ~S" (1+ uuu)))) ;modify result

(setf i (make-$instance 'foo))

($send i :test 4)

|#

;;; redefinition for mac/io>normal>interface

(def$method (normal-interface-mixin :allocate-kb-windows) ()
  ($send self :set-up-windows)
  (dolist (pathname file-name)
    ;8.6.89 to avoid duplicate editor buffers
    (unless (is-known-window pathname *kb-window*) 
      (eval `(make-$instance 'mac-window 
                             :mw-filename ',pathname
                             :mw-title ,(file-namestring pathname)))))) 


;;; redefinition for mac/tools>frame-add-on

(defun allocate-frame-frame-menu ()
  (setf Frame-Display-Item
        (oneof *static-text-dialog-item*
               :dialog-item-text ""
               :dialog-item-size #@(500 70)
               :dialog-item-position #@(2 228)))
  
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
                                 (let ((object ($send (get-instance *CI*) :get-value-only *CS* *CP*)))
                                   (ask Frame-Display-Item 
                                     (set-dialog-item-text
                                      (if (instancep object)
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
                     (if *CSlotSort* (sort il #'string-lessp) il)))
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
  
  (setf Frame-Behavior-Item-List
        (oneof *c-sequence-dialog-item*
               :update-function
               #'(lambda ()
                   (progn
                     (ask Frame-Display-Item (set-dialog-item-text ""))
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
               (list Frame-Frame-Header Frame-Edit-Buttom Frame-Instance-Header Instance-Edit-Buttom Frame-Slot-Header 
                     Frame-Frame-Item-List Frame-Instance-Item-List Frame-Slot-Item-List
                     Frame-Supers-Header Frame-Behavior-Header Behavior-Edit-Buttom Frame-Property-Header 
                     Frame-Supers-Item-List Frame-Behavior-Item-List Frame-Property-Item-List
                     Frame-Display-Item))))


|#


;;; eof

