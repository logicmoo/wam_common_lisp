;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;Grapher
;;
;;Copyright © 1989, Apple Computer, Inc
;;
;;  fuer babylon frame inheritance grapher ausgenutzt: J.Walther
;;
;;  This file implements the base functionality for nodes and grapher-windows
;;  In order to use it, specific types of nodes must be defined.  The file
;;  list-nodes is an example.  Nodes should follow the node protocol by
;;  defining the the following functions:
;;    node-children --  returns a list of the node's children nodes
;;    node-parent   --  returns a list of the node's parent nodes
;;    node-draw     --  does the work of drawing a node.  usual-node-draw
;;                      should be called.
;;    node-size     --  returns a point: the size of the node.  Default: #@(150 20)
;;
;;  The redrawing could be sped up by caching the rectangles
;;  for all the nodes and lines in a quad-tree.  This would, however,
;;  consume a lot more space for a graph.
;;


;(in-package :grapher :use '(:lisp :ccl))

(eval-when (eval compile load)
  (require 'records)
  (require 'traps)
  (require 'scrolling-windows "ccl;examples:scrolling-windows"))



;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; variables
;;

(defparameter *last-y* 0)
(defparameter *x-spacing* 20)
(defparameter *y-spacing* 10)

(defparameter *grapher-window-size* *fred-window-size*)

(defparameter *display-font* '("geneva" 9))

;;;;;;;;;;;;;;;
;;
;;  some utilities
;;

(defun point-max (a b)
  (make-point (max (point-h a) (point-h b))
              (max (point-v a) (point-v b))))

(defun point-min (a b)
  (make-point (min (point-h a) (point-h b))
              (min (point-v a) (point-v b))))

(defun halve-point (point)
  (make-point (truncate (point-h point) 2)
              (truncate (point-v point) 2)))


;;;;;;;;;;;;;;;;;;;
;;;
;;; node objects
;;;

(defobject *node*)

(defobfun (exist *node*) (init-list)
  (have 'node-position-iv #@(0 0))
  (have 'node-size-iv #@(150 20))    ;a default value
  (have 'node-center-iv nil)
  (usual-exist init-list))

(defobfun (node-position *node*) ()
  (objvar node-position-iv))

(defobfun (set-node-position *node*) (h &optional v)
  (setf (objvar node-position-iv) (make-point h v)
        (objvar node-center-iv) nil))

(defobfun (node-size *node*) ()
  (objvar node-size-iv))

(defobfun (set-node-size *node*) (h &optional v)
  (setf (objvar node-size-iv) (make-point h v)
        (objvar node-center-iv) nil))

(defobfun (node-center *node*) ()
  (or (objvar node-center-iv)
      (setf (objvar node-center-iv)
            (add-points (node-position)
                        (halve-point (node-size))))))

(defobfun (node-field-size *node*) (limit)
  (setq limit (point-max limit
                         (add-points (node-position)
                                     (node-size))))
  (dolist (child (node-children) limit)
    (setq limit (ask child (node-field-size limit)))))

(defobfun (node-click-event-handler *node*) (where)
  (declare (ignore where)))

(defun layout (root-node)
  (graph-init root-node)
  (ask root-node
    (set-node-position (make-point *x-spacing*
                                   (point-v (node-position)))))
  (setq *last-y* 0)
  (layout-y root-node)
  (leaf-funcall #'layout-x root-node))

(defun graph-init (node)
  "Zeros the coordinates of a node and all of its subnodes"
  (ask node
    (set-node-position #@(0 0))
    (setf (objvar node-center-iv) nil)
    (mapc #'graph-init (node-children))))

(defun layout-y (node)
  (ask node
    (when (zerop (point-v (node-position)))
      (let ((children (node-children)))
        (if (dolist (child children)
              (if (zerop (ask child (point-v (node-position))))
                (return t)))
          (progn
            (mapc #'layout-y children)
            (set-node-position
             (make-point (point-h (node-position))
                         (ceiling 
                          (reduce #'(lambda (a b) 
                                      (+ a (ask b (point-v (node-position)))))
                                  children 
                                  :initial-value 0)
                          (length children)))))
          (set-node-position
           (make-point (point-h (node-position))
                       (setf *last-y* (+ *y-spacing* 
                                         *last-y* 
                                         (point-v (node-size)))))))))))

(defun layout-x (node &aux parents)
  (ask node
    (let* ((pos (node-position)))
      (when (and (zerop (point-h pos))
                 (setq parents (node-parents)))
        (dolist (parent parents)
          (layout-x parent))
        (set-node-position
         (make-point (+ *x-spacing*
                        (apply #'max (mapcar #'(lambda (node) 
                                                 (ask node
                                                   (point-h
                                                    (add-points (node-position)
                                                                (node-size)))))
                                             parents)))
                     (point-v pos)))))))

(defun leaf-funcall (fn node &aux (children (ask node (node-children))))
  "Calls fn on all the leaves of the graph starting at node"
  (if children
    (dolist (child children)
      (leaf-funcall fn child))
    (funcall fn node)))

(defobfun (node-draw-links *node*) (&aux (children (node-children)))
  (when children
    (let* ((center (node-center)))
      (dolist (child children)
        (ask child
          (let ((child-center (node-center)))
            (_MoveTo :long center)
            (_LineTo :long child-center)))))))

(defobfun (node-draw *node*) ()
  (let* ((children (node-children))
         (vis? (node-visible-p))
         (draw-links? (and (or vis? (node-on-right-p))
                           (some #'(lambda (kid)
                                     (ask kid (node-on-left-p)))
                                 children)))
         (do-kids? (or draw-links? (some #'(lambda (kid)
                                             (ask kid (node-on-right-p)))
                                         children))))
    (when draw-links?
      (node-draw-links))
    (when do-kids?
      (dolist (child children)
        (ask child (node-draw))))
    vis?))

(defobfun (node-on-right-p *node*) ()
  (< (point-h (node-center))
     (rref (ccl::%getport) :grafport.portrect.right)))
 
(defobfun (node-on-left-p *node*) ()
  (> (point-h (node-center)) 
     (rref (ccl::%getport) :grafport.portrect.left)))

(defobfun (node-visible-p *node*) ()
  (let ((pos (node-position))
        (grafrect (rref (ccl::%getport) :grafport.portrect)))
    (rlet ((noderect :rect
                     :topleft pos
                     :bottomright (add-points pos (node-size))))
      (logbitp 8 (_SectRect :ptr grafrect :ptr noderect :ptr noderect :word)))))


(defun find-node-containing-point (node point &aux ret)
  (ask node
    (let* ((pos (node-position)))
      (rlet ((r :rect 
                :topleft pos
                :bottomright (add-points pos (node-size))))
        (if (logbitp 8 (_PtInRect :long point :ptr r :word))
          node
          (dolist (child (node-children))
            (if (setq ret (find-node-containing-point child point))
              (return ret))))))))


;;;;;;;;;;;;;;;;;;;;;;
;;
;; grapher window
;;

(defobject *grapher-window* ccl::*scrolling-window*)

(defobfun (exist *grapher-window*) (init-list)
  (let* ((rn (getf init-list :root-node))) 
    (unless rn (error "A root-node must be specified"))
    (have 'root-node rn)
    (layout rn)
    (let ((field-size (add-points (make-point *x-spacing* *y-spacing*)
                                  (ask rn (node-field-size 0)))))
      (without-interrupts
       (usual-exist (init-list-default 
                     init-list
                     :window-font *display-font*
                     :window-size (point-min field-size *grapher-window-size*)
                     :window-title "Untitled Grapher"
                     :window-type :document-with-zoom
                     :field-size field-size))))
    (set-window-font :patcopy)))

(defobfun (view-draw-contents *grapher-window*) ()
  (usual-view-draw-contents)
  (with-focused-view (objvar ccl::my-scroller)
    (ask (objvar root-node)
      (node-draw))))

(defobfun (window-click-event-handler *grapher-window*) (where)
  (let* ((other-where (convert-coordinates where (self) (objvar ccl::my-scroller)))
         (dialog-item (point-to-dialog-item where))
         (node (or dialog-item
                   (find-node-containing-point (objvar root-node) other-where))))
    (cond (dialog-item (usual-window-click-event-handler where))
          (node (ask node (node-click-event-handler other-where))))))


(eval-when (eval compile load)

(defmacro with-clip-rect-intersect (rect &rest body)
    (let ((old (gensym))
          (new (gensym)))
      `(let ((,old (_NewRgn :ptr))
             (,new (_NewRgn :ptr)))
         (_getclip :ptr ,old)
         (_rectrgn :ptr ,new :ptr ,rect)
         (_SectRgn :ptr ,old :ptr ,new :ptr ,new)
         (_SetClip :ptr ,new)
         (unwind-protect
           (progn ,@body)
           (_SetClip :ptr ,old)
           (_DisposRgn :ptr ,old)
           (_DisposRgn :ptr ,new)))))

) ;end eval-when

;;; -----------------------------------------------------------------------------------
;;; babylon frame inheritance graph
;;; -----------------------------------------------------------------------------------

(eval-when (eval compile load)
  (bab-require 'add-on-base)
  (bab-require 'basic-frame-mixin))

;;; -----------------------------------------------------------------------------------
;;; *frame-node* class
;;; -----------------------------------------------------------------------------------

(defobject *frame-node* *node*)

(defobfun (exist *frame-node*) (init-list)
  (have 'my-object (getf init-list :object 'frame))
  (have 'my-parents (getf init-list :parents nil))
  (have 'my-children (let* ((me (list (self))))
                       (mapcar #'(lambda (object)
                                   (oneof *frame-node*
                                          :object object
                                          :parents me))
                               (get-subframes (ask (self) (objvar my-object))))))
  (usual-exist init-list))

(defobfun (node-children *frame-node*) ()
  (objvar my-children))

(defobfun (node-parents *frame-node*) ()
  (objvar my-parents))

(defobfun (node-draw *frame-node*) ()
  (when (usual-node-draw)
    (let* ((topleft (node-position))
           (left (point-h topleft))
           (bottomright (add-points topleft (node-size)))
           (bottom (point-v bottomright)))
      (rlet ((r :rect
                :topleft topleft
                :bottomright bottomright))
        (_eraserect :ptr r)
        (_framerect :ptr r)
        (_moveto :word (+ left 3) :word (- bottom 5))
        (_insetrect :ptr r :long #@(2 2))
        (without-interrupts
         (with-clip-rect-intersect r
           (with-pstrs ((str (object-name-string)))
             (_drawstring :ptr str))))))))

(defobfun (object-name-string *frame-node*) ()
  (string (objvar my-object)))

(defobfun (node-size  *frame-node*) ()
  (make-point (+ 10 (string-width (object-name-string) *display-font*))
              20))

(defobfun (node-click-event-handler *frame-node*) (where)
  (when (double-click-p)
    (if (option-key-p) 
      (ask (self) (option-double-click-event-handler where))
      (babylon-edit (objvar my-object)))))

(defobfun (option-double-click-event-handler *frame-node*) (where)
  (let ((frame (objvar my-object)))
    (ask frame-inheritance-graph-window
      (option-double-click-event-handler frame where))))

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
;;; the *frame-grapher-window* class
;;; -----------------------------------------------------------------------------------

(defvar frame-inheritance-graph-window nil)
(defvar frame-options-dialog nil)

(defvar frame-inheritance-graph-window-title 
  "Frame Inheritance Graph {douple click or option double click}")
(defvar frame-inheritance-graph-window-position #@(15 48))
(defvar frame-inheritance-graph-window-size #@(490 235))

(defobject *frame-grapher-window* *grapher-window*)

(defobfun (window-close *frame-grapher-window*) ()
  (ask frame-options-dialog (window-hide))
  (usual-window-close)
  (setf frame-inheritance-graph-window nil))

(defobfun (set-window-position *frame-grapher-window*) (h &optional v)
  (usual-set-window-position h v)
  (setf frame-inheritance-graph-window-position (make-point h v)))

(defobfun (set-window-size *frame-grapher-window*) (h &optional v)
  (usual-set-window-size h v)
  (setf frame-inheritance-graph-window-size (make-point h v)))

(defobfun (option-double-click-event-handler *frame-grapher-window*) (frame-symbol where)
  (declare (ignore where))
  (ask frame-options-dialog 
    (unless (string-equal (string frame-symbol) 
                          (ask-named-item 'frame (dialog-item-text)))
      (ask-named-item 'frame 
        (set-dialog-item-text (string frame-symbol)))
      (ask (objvar display)
        (dolist (cell (selected-cells))
          (cell-deselect (point-h cell) (point-v cell)))
        (set-table-sequence '()))
      (dolist (button '(b-1 b-2 b-3 b-4 b-5 b-6)) 
        (ask (symbol-value button) (radio-button-unpush))))
    (window-select)))

;;; -----------------------------------------------------------------------------------
;;; menu item for grapher
;;; -----------------------------------------------------------------------------------

(defun built-frame-graph ()
  (let ((root (send-kb :choose-from-menu (append '(*root*) (send-kb :frames)) 
                       "Select Root Frame")))
    (when (eq root '*root*)
      (built-frame-root)
      (setf root 'frame))
    (setf frame-inheritance-graph-window
          (oneof *frame-grapher-window*
                 :ROOT-NODE (oneof *frame-node* :OBJECT root)
                 :WINDOW-TITLE frame-inheritance-graph-window-title
                 :WINDOW-POSITION frame-inheritance-graph-window-position
                 :WINDOW-SIZE frame-inheritance-graph-window-size))))
              
(setf Frame-Graph-Command 
  (oneof *menu-item*
         :menu-item-title "Frame Inheritance Graph" 
         :menu-item-action 
         '(if frame-inheritance-graph-window 
            (ask frame-inheritance-graph-window (window-select))
            (built-frame-graph))))

(defobfun (menu-item-update Frame-Graph-Command) ()
  (if (and *current-knowledge-base* 
           ($send *current-knowledge-base* :operation-handled-p :frames))
    (menu-item-enable)
    (menu-item-disable)))

(ask frame-menu (add-menu-items Frame-Graph-Command))

;;; -----------------------------------------------------------------------------------
;;; *frame-options-dialog* class
;;; -----------------------------------------------------------------------------------

(defobject *frame-options-dialog* *DIALOG*)

(defun allocate-frame-options-dialog ()
       (setf frame-options-dialog
             (ONEOF *frame-options-dialog*
                    :WINDOW-TYPE :DOCUMENT
                    :window-title "Frame Explore"
                    :window-show nil
                    :WINDOW-POSITION #@(15 48)
                    :WINDOW-SIZE #@(269 235)
                    :DIALOG-ITEMS
                    (LIST 
                     (have 'b-1
                           (MAKE-DIALOG-ITEM *RADIO-BUTTON-DIALOG-ITEM*
                                             #@(10 26)
                                             #@(118 16)
                                             "Local Slots"
                                             (NFUNCTION DIALOG-ITEM-ACTION
                                                        (LAMBDA NIL
                                                          (PROGN
                                                            (ASK MY-DIALOG 
                                                              (DESELECT)
                                                              (LOCAL-SLOTS))
                                                            (USUAL-DIALOG-ITEM-ACTION))))))
                     (have 'b-2
                           (MAKE-DIALOG-ITEM *RADIO-BUTTON-DIALOG-ITEM*
                                             #@(144 26)
                                             #@(72 16)
                                             "All Slots"
                                             (NFUNCTION DIALOG-ITEM-ACTION
                                                        (LAMBDA NIL
                                                          (PROGN
                                                            (ASK MY-DIALOG 
                                                              (DESELECT)
                                                              (ALL-SLOTS))
                                                            (USUAL-DIALOG-ITEM-ACTION))))))
                     (have 'b-3
                           (MAKE-DIALOG-ITEM *RADIO-BUTTON-DIALOG-ITEM*
                                             #@(10 42)
                                             #@(123 16)
                                             "Local Behaviors"
                                             (NFUNCTION DIALOG-ITEM-ACTION
                                                        (LAMBDA NIL
                                                          (PROGN
                                                            (ASK MY-DIALOG 
                                                              (DESELECT)
                                                              (LOCAL-BEHAVIORS))
                                                            (USUAL-DIALOG-ITEM-ACTION))))))
                     (have 'b-4
                           (MAKE-DIALOG-ITEM *RADIO-BUTTON-DIALOG-ITEM*
                                             #@(144 42)
                                             #@(102 16)
                                             "All Behaviors"
                                             (NFUNCTION DIALOG-ITEM-ACTION
                                                        (LAMBDA NIL
                                                          (PROGN
                                                            (ASK MY-DIALOG 
                                                              (DESELECT)
                                                              (ALL-BEHAVIORS))
                                                            (USUAL-DIALOG-ITEM-ACTION))))))
                     (have 'b-5
                           (MAKE-DIALOG-ITEM *RADIO-BUTTON-DIALOG-ITEM*
                                             #@(10 58)
                                             #@(126 16)
                                             "Direct Instances"
                                             (NFUNCTION DIALOG-ITEM-ACTION
                                                        (LAMBDA NIL
                                                          (PROGN
                                                            (ASK MY-DIALOG 
                                                              (DESELECT)
                                                              (DIRECT-INSTANCES))
                                                            (USUAL-DIALOG-ITEM-ACTION))))))
                     (have 'b-6
                           (MAKE-DIALOG-ITEM *RADIO-BUTTON-DIALOG-ITEM*
                                             #@(144 58)
                                             #@(102 16)
                                             "All Instances"
                                             (NFUNCTION DIALOG-ITEM-ACTION
                                                        (LAMBDA NIL
                                                          (PROGN
                                                            (ASK MY-DIALOG 
                                                              (DESELECT)
                                                              (ALL-INSTANCES))
                                                            (USUAL-DIALOG-ITEM-ACTION))))))
                     (have 'display
                           (MAKE-DIALOG-ITEM *SEQUENCE-DIALOG-ITEM*
                                             #@(5 79)
                                             #@(259 151)
                                             "List"
                                             (nfunction
                                              dialog-item-action
                                              (lambda ()
                                                (when (selected-cells)
                                                  (funcall #'(lambda (selection)
                                                               (ask my-dialog (execute-display-selection selection)))
                                                           (cell-contents (car (selected-cells))))
                                                  (usual-dialog-item-action))))
                                             :CELL-SIZE #@(250 12)
                                             :dialog-item-font '("Monaco" 9)
                                             :TABLE-HSCROLLP NIL
                                             :TABLE-VSCROLLP T
                                             :TABLE-SEQUENCE '()))
                     (MAKE-DIALOG-ITEM *STATIC-TEXT-DIALOG-ITEM* #@(11 5) #@(56 16) "Frame:" NIL)
                     (have 'frame
                           (MAKE-DIALOG-ITEM *STATIC-TEXT-DIALOG-ITEM*
                                             #@(65 5)
                                             #@(192 16)
                                             (string 'urlaubs-antrag-frame)
                                             NIL
                                             :DIALOG-ITEM-NICK-NAME 'FRAME))))))

(defobfun (window-close *frame-options-dialog*) (&optional real)
  (if real (usual-window-close)
      (window-hide)))

(defobfun (deselect *frame-options-dialog*) ()
  (ask (objvar display) 
    (dolist (cell (selected-cells))
      (cell-deselect (point-h cell) (point-v cell)))))

(defobfun (execute-display-selection *frame-options-dialog*) (selection)
  (if (ask b-1 (radio-button-pushed-p))                ; local slots
    (babylon-edit (find-symbol (ask frame (dialog-item-text))))
    (if (ask b-2 (radio-button-pushed-p))              ; all slots
      (babylon-edit (first selection))
      (if (ask b-3 (radio-button-pushed-p))            ; local behaviors
        (babylon-edit (cons (find-symbol (ask frame (dialog-item-text)))
                            selection))
        (if (ask b-4 (radio-button-pushed-p))          ; all behaviors
          (babylon-edit selection)
          (if (or (ask b-5 (radio-button-pushed-p))    ; direct instances
                  (ask b-6 (radio-button-pushed-p)))   ; all instances
            (babylon-edit selection)
            (ed-beep)))))))

;;; -----------------------------------------------------------------------------------
;;; slots
;;; -----------------------------------------------------------------------------------

(defobfun (local-slots *frame-options-dialog*) ()
  (let ((il (get-frame-slot-names (find-symbol 
                                   (ask (objvar frame) (dialog-item-text))))))
    (ask (objvar display)
      (set-table-sequence (if *CSlotSort* (sort il #'string-lessp) il)))))

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

(defobfun (all-slots *frame-options-dialog*) ()
  (ask (objvar display)
    (set-table-sequence 
     (get-all-slots
      (find-symbol (ask (objvar frame) (dialog-item-text)))))))


;;; -----------------------------------------------------------------------------------
;;; behaviors
;;; -----------------------------------------------------------------------------------

(defobfun (local-behaviors *frame-options-dialog*) ()
  (ask (objvar display)
    (set-table-sequence 
     (mapcar #'(lambda (spec)
                 (rest spec))
             (get-frame-behavior-specs
              (find-symbol (ask (objvar frame) (dialog-item-text))))))))

(defun get-all-frame-behavior-specs (frame)
  (let ((l (get-frame-behavior-specs frame))
        (supers (get-all-supers frame)))
    (dolist (super supers (remove-duplicates l :test #'(lambda (spec1 spec2)
                                                         (equal (rest spec1)
                                                                (rest spec2)))))
      (setq l (nconc l (get-frame-behavior-specs super))))))

(defobfun (all-behaviors *frame-options-dialog*) ()
  (ask (objvar display)
    (set-table-sequence 
     (get-all-frame-behavior-specs
      (find-symbol (ask (objvar frame) (dialog-item-text)))))))


;;; -----------------------------------------------------------------------------------
;;; instances
;;; -----------------------------------------------------------------------------------

(defun filtered-instance-list (frame all)
  (let ((il (filter-list (if all 
                           (get-all-instances frame)
                           (get-instance-list frame))
                         *CIF*)))
    (if *CIS* (sort il #'string-lessp) il)))

(defobfun (direct-instances *frame-options-dialog*) ()
  (ask (objvar display)
    (set-table-sequence 
     (filtered-instance-list 
      (find-symbol (ask (objvar frame) (dialog-item-text)))
      nil))))

(defobfun (all-instances *frame-options-dialog*) ()
  (ask (objvar display)
    (set-table-sequence 
     (filtered-instance-list
      (find-symbol (ask (objvar frame) (dialog-item-text)))
      t))))

;;; -----------------------------------------------------------------------------------
;;; window / dialog allocation / deallocation
;;; -----------------------------------------------------------------------------------


(allocate-frame-options-dialog)

(defun deallocate-frame-options-dialog ()
  (ask frame-options-dialog (window-close t)))

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
  (ask Frame-Frame-Menu (window-hide))
  (if frame-inheritance-graph-window
    (ask frame-inheritance-graph-window (window-close)))
  (if frame-options-dialog
    (ask frame-options-dialog (window-hide))))


;;; eof