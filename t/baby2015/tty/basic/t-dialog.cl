;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1987   BY
;;           G M D 
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;; DATE:     August 1987
;; AUTHOR:   E. Gross



(def$flavor tty-dialog-mixin
	((dialog-stream *default-dialog-stream*))
	(tty-menu-mixin)
  :settable-instance-variables
  (:required-instance-variables language)
  (:documentation "a mixin which handels features for the dialog window.
                   further more there are methods
                   which are used by the processors for i/o."))


;;----------------------------------------------------------------------------
;;          methods to be used by the processors
;;----------------------------------------------------------------------------


(def$method (tty-dialog-mixin :babylon-format) (fstr &rest args)
 "writes formatted output to dialog-stream. fstr corresponds to the
control-string of the format function."
 
  (apply #'format dialog-stream fstr args)
  (force-output dialog-stream))


(def$method (tty-dialog-mixin :format) (fstr &rest args)
  "writes formatted output to dialog-stream.
fstr corresponds to the control-string of the format function." 

  (apply #'format dialog-stream fstr args)
  (force-output dialog-stream))



;;-----------------------------------------------------------------------------


#+(or :LISPM :MCL)
(def$method (tty-dialog-mixin :babylon-read) (&optional special-keys)
  "reads in a character or a lisp form from dialog-stream.
only those characters occurring  in the list special-keys are read."
  
  (let ((char (peek-char nil dialog-stream)))
    (cond ((member char special-keys :test 'char-equal)
           (clear-input dialog-stream)
           char)
          (t (prog1 (read dialog-stream)
               (clear-input dialog-stream))))))

#+(and :CCL (not :MCL))
(def$method (tty-dialog-mixin :babylon-read) (&optional (key-list NIL))
  (ask *top-listener*
       (loop
         (multiple-value-bind (message modifiers)
                              (get-next-key-event 
                               (format nil "enter ~{~C or ~}type in" key-list))
           (let ((key (code-char (event-keystroke message modifiers))))
             (if (not (null key))
               (if (member key key-list :test 'char-equal) 
                 (return key)
                 (progn
                   (ask *top-listener*
                        (ed-insert-char key)
                        (window-update))
                   (return (read-from-string (read-line)))))))))))

#+:CCL
(def$method (tty-dialog-mixin :after :babylon-read) (&optional (key-list NIL))
  #+:MCL (set-mini-buffer *top-listener* " Busy")     
  #-:MCL(ask *top-listener* (set-mini-buffer " Busy")))


#-(or :CCL :LISPM)
(def$method (tty-dialog-mixin :babylon-read) (&optional special-keys)
  "reads in a character or a lisp form from dialog-stream.
only those characters occurring  in the list special-keys are read."
  
  (let ((char (read-char dialog-stream)))
    (cond ((member char special-keys :test 'char-equal)
           (clear-input dialog-stream)
           char)
          (t (prog2 (unread-char char dialog-stream)
                    (read dialog-stream)
                    (clear-input dialog-stream))))))


;;-----------------------------------------------------------------------------


(def$method (tty-dialog-mixin :notify) (&rest comments)
  "writes comments to dialog-stream.
comments are printed without slashification."

  (format dialog-stream "~%~A ~A " *bab-prompt* (first comments))
  (if (rest comments)
      (dolist (string (rest comments))
	(format dialog-stream "~%     ~A " string)))
  (terpri dialog-stream)
  (force-output dialog-stream))


(def$method (tty-dialog-mixin :confirm) (&rest comments)
  "writes comments to dialog-stream and waits for input.
comments are printed without slashification.
returns :yes if *end-key* is entered and nil otherwise."
  
  (lexpr-$send self :notify comments)
  (format dialog-stream "~:[~;~%~]     ~@? "
	  (rest comments)
	  (getentry type-end-to-confirm-str babylon-io-table)
	  *end-key*)
  (force-output dialog-stream)
  (let ((char (read-char dialog-stream)))
    (if (eql char *end-key*)
      :yes)))




(def$method (tty-dialog-mixin :type-end-to-continue) (string)
  "writes string to dialog-stream and waits till *end-key* is entered.
returns :ok."
  
  (format dialog-stream "~%~A ~@? " *bab-prompt* string *end-key*)
  (force-output dialog-stream)
  (do ((char (read-char dialog-stream)
	     (read-char dialog-stream)))
      ((eql char *end-key*) :ok)))


(def$method (tty-dialog-mixin :prompt-for-input) (comment)
  "reads in a line from dialog-stream and returns the line as string.
comment is used as prompt."
  (format dialog-stream "~%~A ~A " *bab-prompt* comment)
  (force-output dialog-stream)
  (prog1
    (read-line dialog-stream)
    (terpri dialog-stream)))


;;; eof

