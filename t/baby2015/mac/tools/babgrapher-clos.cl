;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright  1988  BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;  Frame Grapher (CLOS version)

;;;; babylon 

(eval-when (eval compile load)
  (bab-require 'basic-frame-mixin)
  (cc-load "mac^tools>grapher-clos")
  (use-package "GRAPHER"))

(defvar *frame-options-dialog* nil)
(defvar frame-inheritance-graph-window nil)
(defvar frame-options-dialog nil)

(defvar frame-inheritance-graph-window-title 
  "Frame Inheritance Graph {douple click or option double click}")
(defvar frame-inheritance-graph-window-position #@(15 48))
(defvar frame-inheritance-graph-window-size #@(490 235))

(defclass frame-node (node) 
  ((my-object  :initarg :object :accessor my-object :initform (find-class 't))
   (my-parents  :initarg :parents :accessor node-parents :initform nil)
   (my-children :accessor node-children)))

(defmethod initialize-instance ((self frame-node) &key)
  (call-next-method)
  (setf (node-children self) (mapcar #'(lambda (object)
                                         (make-instance 'frame-node
                                           :object object
                                           :parents (list self)))
                                     (get-subframes (my-object self)))))



#-:CCL-3(eval-when (eval compile load)
          
          (defmacro with-clip-rect-intersect (rect &rest body)
            (let ((old (gensym))
                  (new (gensym)))
              `(let ((,old (#_NewRgn))
                     (,new (#_NewRgn)))
                 (#_getclip ,old)
                 (#_rectrgn ,new ,rect)
                 (#_SectRgn ,old ,new ,new)
                 (#_SetClip ,new)
                 (unwind-protect
                   (progn ,@body)
                   (#_SetClip ,old)
                   (#_DisposeRgn ,old)
                   (#_DisposeRgn ,new)))))
          
          ) ;end eval-when

(defmethod node-draw ((self frame-node))
  (when (call-next-method)
    (let* ((topleft (node-position self))
           (left (point-h topleft))
           (bottomright (add-points topleft (node-size self)))
           (bottom (point-v bottomright)))
      (rlet ((r :rect
                :topleft topleft
                :bottomright bottomright))
        (#_eraserect r)
        (#_framerect r)
        (#_moveto (+ left 3) (- bottom 5))
        (#_insetrect :ptr r :long #@(2 2))
        (without-interrupts
         (with-clip-rect-intersect r
           (with-pstrs ((str (object-name-string self)))
             (#_drawstring str))))))))

(defmethod object-name-string ((self frame-node))
  (string (my-object self)))

(defmethod node-click-event-handler ((self frame-node) where)
  (declare (ignore where))
  (when (double-click-p)
    (if (option-key-p) 
      (handle-grapher-option-double-click self)
      (if (control-key-p)
        (handle-grapher-control-double-click self)
        (handle-grapher-double-click self)))))

(defmethod handle-grapher-double-click ((self frame-node))
  (babylon-edit (my-object self)))

(defmethod handle-grapher-control-double-click ((self frame-node))
  (ed-beep))

(defmethod handle-grapher-option-double-click ((self frame-node))
  (set-new-frame *frame-options-dialog* (string (my-object self))))

;    (inspect (my-object self)))

(defmethod node-size ((self frame-node))
  (make-point (+ 8. (string-width (object-name-string self))) 16.))


;;; -----------------------------------------------------------------------------------
;;; build a virtual frame root node
;;; -----------------------------------------------------------------------------------

(defun built-frame-root ()
  (let ((sym (%make-object-name 'frame)))
    (setf (get sym :frame-definition) '(defframe frame))
    (setf (get sym :subclasses) (remove-if #'get-supers 
                                           (send-kb :send-if-handles :frames)))
    (setf (get sym :instances) '())))

;;; -----------------------------------------------------------------------------------
;;; the frame-grapher-window class
;;; -----------------------------------------------------------------------------------


(defclass frame-grapher-window (grapher-window)
  ())

(defmethod window-close ((self frame-grapher-window))
  (call-next-method)
  (setf frame-inheritance-graph-window nil))

(defmethod window-deallocate ((self frame-grapher-window))
  (window-close self))

(defmethod set-window-position ((self frame-grapher-window) h &optional v)
  (call-next-method)
  (setf frame-inheritance-graph-window-position (make-point h v)))

(defmethod set-window-size ((self frame-grapher-window) h &optional v)
  (call-next-method)
  (setf frame-inheritance-graph-window-size (make-point h v)))

(defmethod option-double-click-event-handler ((self frame-grapher-window) frame-symbol where)
  (declare (ignore where))
  (set-new-frame frame-options-dialog frame-symbol)
  (window-select frame-options-dialog))


;;; -----------------------------------------------------------------------------------
;;; menu item for grapher
;;; -----------------------------------------------------------------------------------

(defun built-frame-graph ()
  (let ((root (send-kb :choose-from-menu 
                       (append '(*root*) (send-kb :frames)) 
                       "Select Root Frame")))
    (when (eq root '*root*)
      (built-frame-root)
      (setf root 'frame))
    (setf frame-inheritance-graph-window
          (make-instance 'frame-grapher-window
            :ROOT-NODE (make-instance 'frame-node :OBJECT root)
            :WINDOW-TITLE frame-inheritance-graph-window-title
            :VIEW-POSITION frame-inheritance-graph-window-position
            :VIEW-SIZE frame-inheritance-graph-window-size))))

(defvar Frame-Graph-Command 
  (make-instance 'menu-item
    :menu-item-title "Frame Inheritance Graph" 
    :menu-item-action 
    #'(lambda ()
        (if frame-inheritance-graph-window 
          (window-select frame-inheritance-graph-window)
          (built-frame-graph)))))

(set-menu-item-update-function  
 Frame-Graph-Command
 #'(lambda (self)
     (if (and *current-knowledge-base*
              (flavor-typep *current-knowledge-base* 'basic-frame-mixin))
       (menu-item-enable self)
       (menu-item-disable self))))

(add-menu-items frame-menu Frame-Graph-Command)


;;; -----------------------------------------------------------------------------------
;;; *frame-options-dialog* class
;;; -----------------------------------------------------------------------------------

(defclass frame-options-dialog (DIALOG) 
  ((mode :initform :bury)))

(defmethod window-close ((self frame-options-dialog))
  (if (eq (slot-value self 'mode) :bury)
    (window-hide self)
    (call-next-method)))

(defmethod window-deallocate ((self frame-options-dialog))
  (setf (slot-value self 'mode) :close)
  (window-close self))

(defmethod set-new-frame ((self frame-options-dialog) frame-name)
  (let ((display (view-named 'display self)))
    (deselect display)
    (set-table-sequence display '())
    (set-dialog-item-text (view-named 'frame self) frame-name)
    (map-subviews self #'radio-button-unpush 'radio-button-dialog-item)
    (window-select self)))

(defmethod execute-display-selection ((self frame-options-dialog) selection)
  (let ((frame (view-named 'frame self)))
    (if (radio-button-pushed-p (view-named 'b-1 self))                ; local slots
      (babylon-edit (find-symbol (dialog-item-text frame)))
      (if (radio-button-pushed-p (view-named 'b-2 self))              ; all slots
        (babylon-edit (first selection))
        (if (radio-button-pushed-p (view-named 'b-3 self))            ; local behaviors
          (babylon-edit (cons (find-symbol (dialog-item-text frame))
                              selection))
          (if (radio-button-pushed-p (view-named 'b-4 self))          ; all behaviors
            (babylon-edit selection)
            (if (or (radio-button-pushed-p (view-named 'b-5 self))    ; direct instances
                    (radio-button-pushed-p (view-named 'b-6 self)))   ; all instances
              (babylon-edit selection)
              (ed-beep))))))))

(defun allocate-frame-options-dialog ()
  (setf *frame-options-dialog*
        (make-instance 'frame-options-dialog
          :window-type :document
          :window-title "Frame Explore"
          :window-show nil
          :view-position #@(15 48)
          :view-size #@(269 235)
          :view-subviews
          (LIST 
           (MAKE-DIALOG-ITEM 'RADIO-BUTTON-DIALOG-ITEM
                             #@(10 26)
                             #@(118 16)
                             "Local Slots"
                             #'(LAMBDA (item) (declare (ignore item))
                                (LOCAL-SLOTS *frame-options-dialog*))
                             :view-nick-name 'b-1)
           (MAKE-DIALOG-ITEM 'RADIO-BUTTON-DIALOG-ITEM
                             #@(144 26)
                             #@(72 16)
                             "All Slots"
                             #'(LAMBDA (item) (declare (ignore item))
                                (ALL-SLOTS *frame-options-dialog*))
                             :view-nick-name 'b-2)
           (MAKE-DIALOG-ITEM 'RADIO-BUTTON-DIALOG-ITEM
                             #@(10 42)
                             #@(123 16)
                             "Local Behaviors"
                             #'(LAMBDA (item) (declare (ignore item))
                                (LOCAL-BEHAVIORS *frame-options-dialog*))
                             :view-nick-name 'b-3)
           (MAKE-DIALOG-ITEM 'RADIO-BUTTON-DIALOG-ITEM
                             #@(144 42)
                             #@(102 16)
                             "All Behaviors"
                             #'(LAMBDA (item) (declare (ignore item))
                                (ALL-BEHAVIORS *frame-options-dialog*))
                             :view-nick-name 'b-4)
           (MAKE-DIALOG-ITEM 'RADIO-BUTTON-DIALOG-ITEM
                             #@(10 58)
                             #@(126 16)
                             "Direct Instances"
                             #'(LAMBDA (item) (declare (ignore item))
                                (direct-instances *frame-options-dialog*))
                             :view-nick-name 'b-5)
           (MAKE-DIALOG-ITEM 'RADIO-BUTTON-DIALOG-ITEM
                             #@(144 58)
                             #@(102 16)
                             "All Instances"
                             #'(LAMBDA (item) (declare (ignore item))
                                (all-instances *frame-options-dialog*))
                             :view-nick-name 'b-6)
           (MAKE-DIALOG-ITEM 'SEQUENCE-DIALOG-ITEM
                             #@(5 79)
                             #@(259 151)
                             "List"
                             #'(lambda (item)
                                 (when (selected-cells item)
                                   (execute-display-selection *frame-options-dialog*
                                                              (selection item))))
                             :CELL-SIZE #@(244 12)
                             :view-font '("Monaco" 9)
                             :TABLE-HSCROLLP NIL
                             :TABLE-VSCROLLP T
                             :TABLE-SEQUENCE '()
                             :view-nick-name 'display)
           (MAKE-DIALOG-ITEM 'STATIC-TEXT-DIALOG-ITEM #@(11 5) #@(56 16) "Frame:" NIL)
           (MAKE-DIALOG-ITEM 'STATIC-TEXT-DIALOG-ITEM
                             #@(65 5)
                             #@(192 16)
                             (string 'urlaubs-antrag-frame)
                             NIL
                             :VIEW-NICK-NAME 'FRAME)))))

;;; -----------------------------------------------------------------------------------
;;; slots
;;; -----------------------------------------------------------------------------------

(defmethod local-slots ((self frame-options-dialog))
  (let ((il (get-frame-slot-names (find-symbol (dialog-item-text (view-named 'frame self)))))
        (display (view-named 'display self)))
    (deselect display)
    (set-table-sequence display
                        (if *CSlotSort* (sort il #'string-lessp) il))))

(defun get-all-slots (frame)
  (let ((l (mapcar #'(lambda (slot)
                       (list frame slot))
                   (get-frame-slot-names frame)))
        (supers (get-all-supers frame)))
    (dolist (super supers (remove-duplicates l :test #'(lambda (spec1 spec2)
                                                         (equal (cdr spec1)
                                                                (cdr spec2)))))
      (setq l (nconc l (mapcar #'(lambda (slot)
                                   (list super slot))
                               (get-frame-slot-names super)))))
    (if *CSlotSort* 
      (sort l #'(lambda (e1 e2)
                  (string-lessp (second e1)
                                (second e2))))
      l)))

(defmethod all-slots ((self frame-options-dialog))
  (let ((display (view-named 'display self)))
    (deselect display)
    (set-table-sequence display
                        (get-all-slots
                         (find-symbol (dialog-item-text (view-named 'frame self)))))))


;;; -----------------------------------------------------------------------------------
;;; behaviors
;;; -----------------------------------------------------------------------------------

(defmethod local-behaviors ((self frame-options-dialog))
  (let ((display (view-named 'display self)))
    (deselect display)
    (set-table-sequence 
     display
     (mapcar #'(lambda (spec)
                 (rest spec))
             (get-frame-behavior-specs
              (find-symbol (dialog-item-text (view-named 'frame self))))))))

(defun get-all-frame-behavior-specs (frame)
  (let ((l (get-frame-behavior-specs frame))
        (supers (get-all-supers frame)))
    (dolist (super supers (remove-duplicates l :test #'(lambda (spec1 spec2)
                                                         (equal (rest spec1)
                                                                (rest spec2)))))
      (setq l (nconc l (get-frame-behavior-specs super))))))

(defmethod all-behaviors ((self frame-options-dialog))
  (let ((display (view-named 'display self)))
    (deselect display)
    (set-table-sequence display
                        (get-all-frame-behavior-specs
                         (find-symbol (dialog-item-text (view-named 'frame self)))))))


;;; -----------------------------------------------------------------------------------
;;; instances
;;; -----------------------------------------------------------------------------------

(defun filtered-instance-list (frame all)
  (let ((il (filter-list (if all 
                           (get-all-instances frame)
                           (get-instance-list frame))
                         *CIF*)))
    (if *CIS* (sort il #'string-lessp) il)))

(defmethod direct-instances ((self frame-options-dialog))
  (let ((display (view-named 'display self)))
    (deselect display)
    (set-table-sequence display
                        (filtered-instance-list 
                         (find-symbol (dialog-item-text (view-named 'frame self)))
                         nil))))

(defmethod all-instances ((self frame-options-dialog))
  (let ((display (view-named 'display self)))
    (deselect display)
    (set-table-sequence display
                        (filtered-instance-list
                         (find-symbol (dialog-item-text (view-named 'frame self)))
                         t))))

;;; -----------------------------------------------------------------------------------
;;; window / dialog allocation / deallocation
;;; -----------------------------------------------------------------------------------


(allocate-frame-options-dialog)

(defun deallocate-frame-options-dialog ()
  (window-deallocate *frame-options-dialog*))

(progn
  (push (symbol-function 'deallocate-frame-options-dialog) *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'allocate-frame-options-dialog))))
  t)

;;; -----------------------------------------------------------------------------------
;;; knowledge base switching support
;;; -----------------------------------------------------------------------------------

(def$method (basic-frame-mixin :after :deselect-kb) ()
  (window-hide Frame-Frame-Menu)
  (if frame-inheritance-graph-window (window-hide frame-inheritance-graph-window))
  (window-hide *frame-options-dialog*)
  (setf frame-inheritance-graph-window nil))



;;; eof

