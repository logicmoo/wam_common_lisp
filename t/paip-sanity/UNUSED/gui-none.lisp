;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig
;;;     File: .lisp; Date: /95

;;;; What to do when there's no Graphical User Interface

(defun paip-tutor ()
  (format t "What chapter would you like to work on?~%")
  (format t "Type a chapter number, or ALL or QUIT.~%")
  (format t "Chapter? ")
  (let ((chapter (read)))
    (unless (eq chapter 'quit)
      (do-examples chapter)
      (paip-tutor))))


