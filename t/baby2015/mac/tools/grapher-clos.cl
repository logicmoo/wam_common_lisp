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

(defpackage :grapher 
  (:use :common-lisp :ccl)
  (:export "NODE" "GRAPHER-WINDOW"
           "NODE-CHILDREN" "NODE-PARENTS" "NODE-DRAW" "NODE-SIZE" "NODE-POSITION"
           "NODE-CLICK-EVENT-HANDLER"))

(in-package :grapher)



(require :quickdraw)
(require :scrolling-windows)



;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; variables
;;

(defparameter *last-y* 0)
(defparameter *x-spacing* 30)
(defparameter *y-spacing* 10)

(defparameter *grapher-window-size* *window-default-size*)

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

(defclass node () 
  ((node-position-iv :reader node-position  :initform #@(0 0))
   (node-size-iv  :reader node-size  :initform #@(150 150))
   (node-center-iv  :reader node-center  :initform nil))
   )

(defmethod set-node-position ((self node) h &optional v)
  (setf (slot-value self 'node-position-iv) (make-point h v)
        (slot-value self 'node-center-iv) nil))

(defmethod set-node-size ((self node) h &optional v)
    (setf (slot-value self 'node-size-iv) (make-point h v)
        (slot-value self  'node-center-iv) nil))

(defmethod node-center ((self node))
  (or (slot-value self 'node-center-iv)
      (setf (slot-value self 'node-center-iv)
            (add-points (node-position self)
                        (halve-point (node-size self))))))



(defmethod node-field-size ((self node) limit)
  (setq limit (point-max limit
                         (add-points (node-position self)
                                     (node-size self))))
  (dolist (child (node-children self) limit)
    (setq limit (node-field-size child limit))))

(defmethod node-click-event-handler ((self node) where)
  (declare (ignore where)))

(defun layout (root-node)
  (graph-init root-node)
  (set-node-position root-node (make-point *x-spacing*
                                           (point-v (node-position root-node))))
  (setq *last-y* 0)
  (layout-y root-node)
  (leaf-funcall #'layout-x root-node))

(defun graph-init (node)
  "Zeros the coordinates of a node and all of its subnodes"
    (set-node-position node #@(0 0))
    (setf (slot-value node 'node-center-iv) nil)
    (mapc #'graph-init (node-children node)))

(defun layout-y (node)
 
    (when (zerop (point-v (node-position node)))
      (let ((children (node-children node)))
        (if (dolist (child children)
              (if (zerop (point-v (node-position child)))
                (return t)))
          (progn
            (mapc #'layout-y children)
            (set-node-position node
             (make-point (point-h (node-position node))
                         (ceiling 
                          (reduce #'(lambda (a b) (+ a (point-v (node-position b))))
                                  children 
                                  :initial-value 0)
                          (length children)))))
          (set-node-position node
           (make-point (point-h (node-position node))
                       (setf *last-y* (+ *y-spacing* *last-y* (point-v (node-size node))))))))))

(defun layout-x (node &aux parents)
      (let* ((pos (node-position node)))
        (when (and (zerop (point-h pos))
                   (setq parents (node-parents node)))
          (dolist (parent parents)
            (layout-x parent))
          (set-node-position node
           (make-point (+ *x-spacing*
                          (apply #'max (mapcar #'(lambda (node)
                                                        (point-h
                                                         (add-points (node-position node)
                                                                     (node-size node))))
                                               parents)))
                       (point-v pos))))))

(defun leaf-funcall (fn node &aux (children (node-children node)))
  "Calls fn on all the leaves of the graph starting at node"
  (if children
    (dolist (child children)
      (leaf-funcall fn child))
    (funcall fn node)))

(defmethod node-draw-links ((self node) &aux (children (node-children self)))
  (when children
    (let* ((center (node-center self)))
      (dolist (child children)
          (let ((child-center (node-center child)))
            (#_MoveTo :long center)
            (#_LineTo :long child-center))))))

(defmethod node-draw ((self node))
  (let* ((children (node-children self))
         (vis? (node-visible-p self))
         (draw-links? (and (or vis? (node-on-right-p self))
                           (some #'(lambda (kid)
                                    (node-on-left-p kid))
                                 children)))
         (do-kids? (or draw-links? (some #'(lambda (kid)
                                             (node-on-right-p kid))
                                         children))))
    (when draw-links?
      (node-draw-links self))
    (when do-kids?
      (dolist (child children)
       (node-draw child)))
    vis?))

(defmethod node-on-right-p ((self node))
  (< (point-h (node-center self))
     (rref (ccl::%getport) :grafport.portrect.right)))
 
(defmethod node-on-left-p ((self node))
  (> (point-h (node-center self)) (rref (ccl::%getport) :grafport.portrect.left)))

(defmethod node-visible-p ((self node))
  (let ((pos (node-position self))
        (grafrect (rref (ccl::%getport) :grafport.portrect)))
    (rlet ((noderect :rect
                     :topleft pos
                     :bottomright (add-points pos (node-size self))))
      (#_SectRect grafrect noderect noderect))))


(defun find-node-containing-point (node point &aux ret)
    (let* ((pos (node-position node)))
      (rlet ((r :rect 
                :topleft pos
                :bottomright (add-points pos (node-size node))))
        (if (#_PtInRect point r)
          node
          (dolist (child (node-children node))
            (if (setq ret (find-node-containing-point child point))
              (return ret)))))))


;;;;;;;;;;;;;;;;;;;;;;
;;
;; grapher window
;;

(defclass grapher-window (ccl::scrolling-window) ((root-node :initarg :root-node)))

(defmethod initialize-instance ((self grapher-window) 
                                &key (window-title "Untitled Grapher")
                                (view-font '("monaco" 9 :plain)) root-node)
  
  (unless root-node (error "A root-node must be specified"))
  (setf (slot-value self 'root-node) root-node)
  (multiple-value-bind (ff ms) (font-codes view-font)
    (with-font-codes ff ms              ; make string-width work right.
      (layout root-node)))
  (let ((field-size (add-points (make-point *x-spacing* *y-spacing*)
                                (node-field-size root-node 0))))
    (without-interrupts
    (call-next-method
            self
            :view-font view-font
            :view-size (point-min field-size *grapher-window-size*)
            :window-title window-title
            :window-type :document-with-zoom
            :field-size field-size
            )))
  (set-view-font self :patcopy))

(defmethod view-draw-contents ((self grapher-window))
  (call-next-method)
  (with-focused-view (slot-value self  'ccl::my-scroller)
    (node-draw (slot-value self 'root-node))))

(defmethod view-click-event-handler ((self grapher-window) where)
  (let* ((other-where (convert-coordinates where
                                           self
                                           (slot-value self 'ccl::my-scroller)))
         (dialog-item nil) ;(point-to-dialog-item where))
         (node (or dialog-item
                   (find-node-containing-point (slot-value self 'root-node)
                                               other-where))))
    (cond ;(dialog-item (call-next-method))
          ;(usual-window-click-event-handler where))
          (node
           (node-click-event-handler node other-where))
          (t (call-next-method)))))



(provide :grapher)

;;; eof

