;;;; -*- Mode: Lisp; Package: Capi; Syntax: Common-Lisp -*-
;;;; Code for Paradigms of AI Programming
;;;; Copyright (c) 1996 Peter Norvig

(import 'capi:define-interface "CL-USER")

(defun paip-tutor ()
  "Create and display a window for running the self-tutoring code 
  examples for the book 'Paradigms of AI Programming'."
  (let ((*package* (find-package "CL-USER")))
    (capi:display (make-instance 'paip-tutor))))
 
(define-interface paip-tutor (capi:interface)
  ((chapter :initform nil :accessor tutor-chapter)
   (examples :initform nil :accessor tutor-examples)
   (page :initform 1 :accessor tutor-page))
  (:panes
   (do-c capi:push-button :text "Do Chapter" :callback 'do-chapter)
   (chapter-popup capi:option-pane :selection-callback 'set-chapter
		  :items *chapters* :width '(:character 60))
   (do-e capi:push-button :text "Do Example" :callback 'do-one-example)
   (example-popup capi:option-pane :selection-callback 'set-example
		  :items (chapter-examples (first *chapters*))
		  :print-function 'print-example-to-string
		  :width '(:character 60))
   (listener capi:listener-pane))
  (:layouts (main capi:column-layout '(row1 row2 listener))
	    (row1 capi:row-layout '(do-c chapter-popup))
	    (row2 capi:row-layout '(do-e example-popup)))
  ;;(:menus (help-menu "Help" ("Basic Help" "More Help")))
  ;;(:menu-bar help-menu)
  (:default-initargs :best-width 500 :best-height 600))


(defmethod initialize-instance :after ((interface paip-tutor) &rest args)
  (declare (ignore (args)))
  (set-chapter (first *chapters*) interface nil))

;;;; The Six Methods

(defmethod set-chapter (chapter (interface paip-tutor) &optional (start t))
  (when (chapter-p chapter)
    (format (output-stream interface) "~2&CHAPTER ~A~2%" chapter)
    (setf (tutor-chapter interface) chapter)
    (setf (tutor-examples interface) (chapter-examples chapter))
    (capi::set-collection-items (pane interface 'example-popup)
	  (chapter-examples chapter) t)
    (update-title interface)
    (when start (start-examples interface))))

(defmethod set-page (page interface)
  (setf (tutor-page interface) page)
  (update-title interface))

(defmethod set-example (example (interface paip-tutor))
  "Jump to a particular example."
  (start-examples 
   interface 
   (member example (chapter-examples (tutor-chapter interface)))))

;; DISPLAY-EXAMPLE and DISPLAY-SECTION use the default methods

(defmethod output-stream ((interface paip-tutor))
  (let ((listener (pane interface 'listener)))
    (if (slot-boundp listener 'stream)
        (slot-value listener 'stream))))

;;;; Other Stuff 

(defun pane (interface pane-name) 
  (slot-value interface pane-name))

(defmethod update-title ((interface paip-tutor))
  (setf (capi::interface-title interface)
	(format nil "PAIP Tutor: Page ~D, Chapter ~D."
		(tutor-page interface) (tutor-chapter interface))))

(defun do-chapter (chapter interface)
  "Do the whole chapter, without pausing."
  (when (tutor-examples interface)
    (start-examples interface)
    (do-chapter chapter interface)))

(defun do-one-example (ignore interface)
  (declare (ignore ignore))
  (when (tutor-examples interface)
    (do-example (pop (tutor-examples interface)) interface)
    (start-examples interface)))
  
(defun start-examples (interface &optional (examples
					    (tutor-examples interface)))
  "Get the junk out of the way; stop when you hit a real example."
  (setf (tutor-examples interface) 
	(do-documentation-examples examples interface))
  (when (tutor-examples interface)
    (setf (capi::choice-selection (pane 'example-popup interface))
          (- (length (chapter-examples (tutor-chapter interface)))
             (max 1 (length (tutor-examples interface))))))
  (update-title interface))

(defun print-example-to-string (example)
  (truncate-sequence
   (cond ((and (consp example) (eq (first example) ':section))
          (format nil "Section ~A" (second example)))
         ((consp example)
          (prin1-to-string (first example)))
         (t example))
   60))

(defun truncate-sequence (sequence n)
  (subseq sequence 0 (min n (length sequence))))

;;(setf (choice-selection (pane 'section-button interface)) 0)


;;; QUESTIONS

;; How do I get the menu bar from a Listener into my interface?
;; How do I change the items in a popup?
;; Is there something to call to make a popup redisplay?
;; Can I change the prompt?