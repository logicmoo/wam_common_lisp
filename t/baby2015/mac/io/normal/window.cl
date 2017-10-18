;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: User -*-

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;; windows


(defobject *kb-window* *fred-window*)

#|
(defobfun (to-be-remembered *kb-window*) ()
  `(:mw-title ,(window-title)
              :mw-position ,(window-position)
              :mw-size ,(window-size)
              :mw-font ',(window-font)
              :mw-filename ,(window-filename)))
|#

(defun is-known-window (file type)
  (let ((file-name (namestring file)))
    (dolist (window (windows type) nil)
      (if (string-equal file-name (ask window (window-filename)))
        (return window)))))

(def$flavor mac-window
  ((mw-title "Untitled")
   (mw-filename nil)
   (mw-position  *fred-window-position*)
   (mw-size  *fred-window-size*)
   (mw-show-p t)
   (mw-font '("monaco" 9))
   (mw-layer 0)
   (mw-scratch-p nil)
   (stream nil))
  ()
  :settable-instance-variables
  (:documentation "This is the basic flavor for all text scroll output windows.")
  )

(def$method (mac-window :after :init) (&rest plist)
  "Generate the window using features of the Mac Allegro Common Lisp."
  (setf stream (oneof *kb-window* 
                      :filename mw-filename
                      :window-show mw-show-p
                      :window-title mw-title
                      :window-position mw-position
                      :window-size mw-size
                      :window-font mw-font
                      :close-box-p nil
                      :scratch-p mw-scratch-p)))

;  (ask stream (set-window-filename (namestring mw-filename)))

(defobfun (visible-lines *window*) ()
  (floor (/ (- (point-v (window-size)) 17)
            (multiple-value-bind (ascent descent ignore leading) 
                                 (font-info (window-font))
              (+ ascent descent leading)))))

(defobfun (put-last-line-in-window *window*) ()
  (set-mark (window-start-mark) 
            (buffer-line-start (window-buffer) 
                               (window-cursor-mark) 
                               (- 2 (visible-lines)))))

(def$method (mac-window :format) (&rest args)
  "Output to scroll window using fstring and args like using format function
or an item list intended for mouse sensitive scroll windows."
  (ask stream (ccl::ed-end-of-buffer)(ed-insert-char #\Return))
 ; (terpri stream)
  (if (stringp (first args))
    (apply #'format stream (first args) (rest args))
    (format stream "~A" (apply #'string-append 
                               (mapcar #'(lambda (item)
                                           (second (member :string item)))
                                       args))))
  (ask stream (put-last-line-in-window) (window-update)))

(def$method (mac-window :expose) ()
  (ask stream (window-select)))

(def$method (mac-window :bury) ()
  (ask stream (set-window-layer (1+ (window-layer)))))

(def$method (mac-window :clear) ()
  (ask stream (select-all) (ccl::ed-rubout-char)))

(def$method (mac-window :kill) ()
  (ask stream (if (ownp 'wptr) (window-close))))   ; don't close if already closed

;;; eof

