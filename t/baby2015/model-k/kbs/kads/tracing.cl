;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                 ;;;
;;;  tracing-functions for model-k and office-plan  ;;;
;;;                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; simple tracing for office-plan

(defvar *trace* NIL)

#|

(declaim (special office-plan))

(defun op ()
  (setq *trace* (<- office-plan :activate))
  (init_expl)
  'traced)

|#

;;; pretty-print for the model-k produced trace

#|
(defun trace-print (trace &optional mode)
  (declare (ignore mode))
  (terpri)
  (write-string "TASK : ")
  (prin1 (car trace))(terpri)
  (ks-print (cdr trace)))
|#

(defun ks-print (trace)
  (if (null trace)
    nil
    (if (atom (car trace))
       (let ((input-mc (cadr trace))
             (output-mc (caddr trace)))
         (terpri)(write-string "  KS : ")
         (prin1 (car trace))
         (terpri)(write-line "  with metaclasses ...")
         (print-mcs input-mc "  --> ")
         (print-mcs output-mc "  <-- ")
        (ks-print (cdddr trace)))
       (progn
         (trace-print (car trace))
         (ks-print (cdr trace))))))

(defun print-mcs (mc sign)
  (mapc #'(lambda (x)
            (if (and (atom x) x)
              (prog1
                (write-string sign)(prin1 x))
              (prin1 x))
            (terpri))
        mc))


;;; window-supported representation of the trace (more structured!)

#+:CCL-1.3
(progn
  
  (load "babylon;Library:QuickDraw")
  
  (setq *t-window* (kindof *window*))
  
  (defobfun (view-draw-contents *t-window*) ()
    (usual-view-draw-contents)
    (erase-rect 0 (view-size))
    (move-to 10 29)
    (if (eq trace-type :mc)
      (write-string (m-list2 window) (self))
      (progn 
        (set-window-font :bold)
        (write (car t-k-list2) :stream (self))
        (set-window-font :plain)
        (mapcar #'(lambda (obj)
                    (write-char #\Space (self))
                    (write obj :stream (self)))
                (cdr t-k-list2)))))
  
  (defobfun (window-key-event-handler *t-window*) (char)
    (cond
     ((eq trace-type :task)
      (cond
       ((eq char #\BackArrow)
        (scroll-word :left))
       ((eq char #\ForwardArrow)
        (scroll-word :right))
       ((eq char #\DownArrow)
        (if (member (car t-k-list2) my-trace-part)
          (let* ((local-start (member (car t-k-list2) my-trace-part))
                 (local-trace (list (first local-start)
                                    (second local-start)
                                    (third local-start)))
                 (wtop (point-v (view-position)))
                 (wlen (point-v (view-size))))
            (setq subwindows (append
                              (open-ks-windows local-trace (+ wtop wlen 22))
                              subwindows)))
          (let ((local-trace (get-task (car t-k-list2) my-trace-part))
                (wtop (point-v (view-position)))
                (wlen (point-v (view-size))))
            (setq subwindows (cons
                              (open-task-window local-trace (+ wtop wlen 22))
                              subwindows)))))
       ((eq char #\UpArrow)
        (window-close))))
     ((eq trace-type :ks)
      (cond
       ((eq char #\BackArrow)
        (scroll-word :left))
       ((eq char #\ForwardArrow)
        (scroll-word :right))
       ((eq char #\DownArrow)
        (let* ((local-start (member (car t-k-list2) my-trace-part))
               (local-trace (list (first local-start)
                                  (second local-start)))
               (wtop (point-v (view-position)))
               (wlen (point-v (view-size))))
          (setq subwindows (cons
                            (open-mc-window local-trace (+ wtop wlen 22))
                            subwindows))))
       ((eq char #\UpArrow)
        (window-close))))
     ((eq trace-type :mc)
      (cond
       ((eq char #\BackArrow)
        (scroll-char :left))
       ((eq char #\ForwardArrow)
        (scroll-char :right))
       ((eq char #\UpArrow)
        (window-close))))))
  
  (defobfun (exist *t-window*) (init-list)
    (have 't-k-list1     (getf init-list :t-k-list1 'nil))
    (have 't-k-list2     (getf init-list :t-k-list2 'nil))
    (have 'm-list1       (getf init-list :m-list1 ""))
    (have 'm-list2       (getf init-list :m-list2 ""))
    (have 'my-trace-part (getf init-list :my-trace-part 'nil))
    (have 'trace-type    (getf init-list :trace-type :task))
    (have 'subwindows    (getf init-list :subwindows 'nil))
    (usual-exist init-list))
  
  (defobfun (scroll-word *t-window*) (dir)
    (if (eq dir :left)
      (if t-k-list1
        (setq (t-k-list2 window) (cons (car (last t-k-list1)) t-k-list2)
              t-k-list1 (reverse (cdr (reverse t-k-list1)))))
      (if (cdr t-k-list2)
        (setq t-k-list1 (append t-k-list1 (list (car t-k-list2)))
              (t-k-list2 window) (cdr t-k-list2))))
    (view-draw-contents))
  
  (defobfun (scroll-char *t-window*) (dir)
    (if (eq dir :left)
      (if (> (length m-list1) 0)
        (setq (m-list2 window) (concatenate 'string
                                            (subseq (m-list1 window) (- (length m-list1) 5))
                                            m-list2)
              (m-list1 window) (subseq (m-list1 window) 0 (- (length m-list1) 5))))
      (if (> (length m-list2) 50)
        (setq (m-list1 window) (concatenate 'string
                                            m-list1
                                            (subseq (m-list2 window) 0 5))
              (m-list2 window) (subseq (m-list2 window) 5))))
    (view-draw-contents))
  
  (defobfun (window-close *t-window*) ()
    (mapcar #'(lambda (win)
                (ask win (window-close)))
            subwindows)
    (usual-window-close))
  
  (defun get-task (task-name trace)
    (car
     (member-if #'(lambda (elt)
                    (eq (car elt) task-name))
                trace)))
  
  (defun show-trace (trace)
    (open-task-window trace (+ *menubar-bottom* 2)))
  
  (defun open-task-window (trace window-top)
    (oneof *t-window*
           :window-title (concatenate
                          'string "Task : "
                          (symbol-name (car trace)))
           :view-position (make-point 5 window-top)
           :view-size (make-point (- *screen-width* 50) 61)
           :window-font '(12 :srcCopy)
           :window-type :tool
           :t-k-list2 (task-elt-list (cdr trace))
           :my-trace-part (cdr trace)
           :trace-type :task))
  
  (defun task-elt-list (t-list)
    (cond
     ((null t-list) nil)
     ((atom (car t-list))
      (cons (car t-list)
            (task-elt-list (cdddr t-list))))
     (t
      (cons (caar t-list)
            (task-elt-list (cdr t-list))))))
  
  (defun open-ks-windows (trace window-top)
    (list
     (oneof *t-window*
            :window-title (concatenate
                           'string "Input for KS : "
                           (symbol-name (car trace)))
            :view-position (make-point 5 window-top)
            :view-size (make-point (/ (- *screen-width* 54) 2) 61)
            :window-font '(12 :srcCopy)
            :window-type :tool
            :t-k-list2 (ks-elt-list (cadr trace))
            :my-trace-part (cadr trace)
            :trace-type :ks)
     (oneof *t-window*
            :window-title (concatenate
                           'string "Output for KS : "
                           (symbol-name (car trace)))
            :view-position (make-point (/ (- *screen-width* 36) 2) window-top)
            :view-size (make-point (/ (- *screen-width* 54) 2) 61)
            :window-font '(12 :srcCopy)
            :window-type :tool
            :t-k-list2 (ks-elt-list (caddr trace))
            :my-trace-part (caddr trace)
            :trace-type :ks)))
  
  (defun ks-elt-list (trace)
    (if (null trace)
      nil
      (cons
       (car trace)
       (ks-elt-list (cddr trace)))))
  
  (defun open-mc-window (trace window-top)
    (oneof *t-window*
           :window-title (concatenate
                          'string "Metaclass : "
                          (symbol-name (car trace)))
           :view-position (make-point 5 window-top)
           :view-size (make-point (- *screen-width* 50) 61)
           :window-font '(12 :srcCopy)
           :window-type :tool
           :m-list2 (write-to-string (cdr trace))
           :trace-type :mc))
  
  )


#+:MCL
(progn
  
  (require 'QuickDraw)
  
  (defclass t-window (window)
    ((t-k-list1 :accessor t-k-list1 :initarg :t-k-list1 :initform '())
     (t-k-list2 :accessor t-k-list2 :initarg :t-k-list2 :initform '())
     (m-list1 :accessor m-list1 :initarg :m-list1 :initform "")
     (m-list2 :accessor m-list2 :initarg :m-list2 :initform "")
     (my-trace-part :accessor my-trace-part :initarg :my-trace-part :initform '())
     (trace-type :accessor trace-type :initarg :trace-type :initform :task)
     (subwindows :accessor subwindows :initarg :subwindows :initform '())
     ))
  
  (defmethod view-draw-contents :after ((window t-window))
    (erase-rect window 0 (view-size window))
    (move-to 10 29)
    (if (eq (trace-type window) :mc)
      (write-string (m-list2 window) window)
      (progn 
        (set-view-font window :bold)
        (write (car (t-k-list2 window)) :stream window)
        (set-view-font window :plain)
        (mapcar #'(lambda (obj)
                    (write-char #\Space window)
                    (write obj :stream window))
                (cdr (t-k-list2 window))))))
  
  (defmethod view-key-event-handler ((window t-window) char)
    (cond
     ((eq (trace-type window) :task)
      (cond
       ((eq char #\BackArrow)
        (scroll-word :left))
       ((eq char #\ForwardArrow)
        (scroll-word :right))
       ((eq char #\DownArrow)
        (if (member (car (t-k-list2 window)) (my-trace-part window))
          (let* ((local-start (member (car (t-k-list2 window)) (my-trace-part window)))
                 (local-trace (list (first local-start)
                                    (second local-start)
                                    (third local-start)))
                 (wtop (point-v (view-position window)))
                 (wlen (point-v (view-size window))))
            (setf (subwindows window) (append
                                       (open-ks-windows local-trace (+ wtop wlen 22))
                                       (subwindows window))))
          (let ((local-trace (get-task (car (t-k-list2 window)) (my-trace-part window)))
                (wtop (point-v (view-position window)))
                (wlen (point-v (view-size window))))
            (setf (subwindows window) (cons
                                       (open-task-window local-trace (+ wtop wlen 22))
                                       (subwindows window))))))
       ((eq char #\UpArrow)
        (window-close window))))
     ((eq (trace-type window) :ks)
      (cond
       ((eq char #\BackArrow)
        (scroll-word window :left))
       ((eq char #\ForwardArrow)
        (scroll-word window :right))
       ((eq char #\DownArrow)
        (let* ((local-start (member (car (t-k-list2 window)) (my-trace-part window)))
               (local-trace (list (first local-start)
                                  (second local-start)))
               (wtop (point-v (view-position window)))
               (wlen (point-v (view-size window))))
          (setf (subwindows window) (cons
                                     (open-mc-window local-trace (+ wtop wlen 22))
                                     (subwindows window)))))
       ((eq char #\UpArrow)
        (window-close window))))
     ((eq (trace-type window) :mc)
      (cond
       ((eq char #\BackArrow)
        (scroll-char window :left))
       ((eq char #\ForwardArrow)
        (scroll-char window :right))
       ((eq char #\UpArrow)
        (window-close window))))))
  
  (defmethod scroll-word ((window t-window) dir)
    (if (eq dir :left)
      (when (t-k-list1 window)
        (setf (t-k-list2 window) (cons (car (last (t-k-list1 window))) (t-k-list2 window)))
        (setf (t-k-list1 window) (reverse (cdr (reverse (t-k-list1 window))))))
      (when (cdr (t-k-list2 window))
        (setf (t-k-list1 window) (append (t-k-list1 window) (list (car (t-k-list2 window)))))
        (setf (t-k-list2 window) (cdr (t-k-list2 window)))))
    (view-draw-contents window))
  
  (defmethod scroll-char ((window t-window) dir)
    (if (eq dir :left)
      (when (> (length (m-list1 window)) 0)
        (setf (m-list2 window) 
              (concatenate 'string
                           (subseq (m-list1 window) (- (length (m-list1 window)) 5))
                           (m-list2 window)))
        (setf (m-list1 window) (subseq (m-list1 window) 0 (- (length (m-list1 window)) 5))))
      (when (> (length (m-list2 window)) 50)
        (setf (m-list1 window) 
              (concatenate 'string (m-list1 window) (subseq (m-list2 window) 0 5)))
        (setf (m-list2 window) (subseq (m-list2 window) 5))))
    (view-draw-contents window))
  
  (defmethod window-close :before ((window t-window))
    (mapcar #'(lambda (win)
                (window-close win))
            (subwindows window)))
  
  (defun get-task (task-name trace)
    (car
     (member-if #'(lambda (elt)
                    (eq (car elt) task-name))
                trace)))
  
  (defun task-elt-list (t-list)
    (cond
     ((null t-list) nil)
     ((atom (car t-list))
      (cons (car t-list)
            (task-elt-list (cdddr t-list))))
     (t
      (cons (caar t-list)
            (task-elt-list (cdr t-list))))))
  
  (defun open-task-window (trace window-top)
    (make-instance 't-window
      :window-title (concatenate 'string "Task : " (symbol-name (car trace)))
      :view-position (make-point 5 window-top)
      :view-size (make-point (- *screen-width* 50) 61)
      :window-font '(12 :srcCopy)
      :window-type :tool
      :t-k-list2 (task-elt-list (cdr trace))
      :my-trace-part (cdr trace)
      :trace-type :task))
  
  (defun show-trace (trace)
    (open-task-window trace (+ *menubar-bottom* 2)))
  
  (defun ks-elt-list (trace)
    (if (null trace)
      nil
      (cons
       (car trace)
       (ks-elt-list (cddr trace)))))
  
  (defun open-ks-windows (trace window-top)
    (list
     (make-instance 't-window
       :window-title (concatenate
                      'string "Input for KS : "
                      (symbol-name (car trace)))
       :view-position (make-point 5 window-top)
       :view-size (make-point (/ (- *screen-width* 54) 2) 61)
       :window-font '(12 :srcCopy)
       :window-type :tool
       :t-k-list2 (ks-elt-list (cadr trace))
       :my-trace-part (cadr trace)
       :trace-type :ks)
     (make-instance 't-window
       :window-title (concatenate
                      'string "Output for KS : "
                      (symbol-name (car trace)))
       :view-position (make-point (/ (- *screen-width* 36) 2) window-top)
       :view-size (make-point (/ (- *screen-width* 54) 2) 61)
       :window-font '(12 :srcCopy)
       :window-type :tool
       :t-k-list2 (ks-elt-list (caddr trace))
       :my-trace-part (caddr trace)
       :trace-type :ks)))
  
  (defun open-mc-window (trace window-top)
    (make-instance 't-window
      :window-title (concatenate
                     'string "Metaclass : "
                     (symbol-name (car trace)))
      :view-position (make-point 5 window-top)
      :view-size (make-point (- *screen-width* 50) 61)
      :window-font '(12 :srcCopy)
      :window-type :tool
      :m-list2 (write-to-string (cdr trace))
      :trace-type :mc))
  
  )

;;; eof

