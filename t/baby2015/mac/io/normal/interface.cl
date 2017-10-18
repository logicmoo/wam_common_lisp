;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: User -*-

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;
;  normal-interface-mixin for Macintosh Coral Allegro Common Lisp
;


(defun include-kb-file (file &key (bufferp t))
  (let ((true-name (merge-pathnames file (pathname-directory *babylon-kbs*))))
    (if bufferp
      (let ((source (or (probe-file (make-pathname 
                                     :directory (pathname-directory true-name)
                                     :name (pathname-name true-name)
                                     :type "lisp"))
                        (choose-file-dialog))))
        (unless (is-known-window source *kb-window*)
          (let ((new-window (make-$instance 'mac-window
                                            :mw-filename source 
                                            :mw-show-p nil
                                            :mw-scratch-p nil)))
            (ask ($send new-window :stream)
              (set-window-title (file-namestring source))
              (window-select))))
        ($send *current-knowledge-base* :loaded source)))
    (load true-name)))

(defun use-kb (kb)
  (let ((ckb *current-knowledge-base*)  ; remember current knowledge base
        (ckbdir *babylon-kbs*))         ; remember current kbs directory
    (do ()
        ((member kb *known-knowledge-bases*)
         (progn
           (setf *babylon-kbs* ckbdir)           ; restore directory
           ($send ckb :make-yourself-current)    ; restore kb
           (send-kb :import-kb kb)
           (format nil "~S imported" kb)))
      (if (not (member kb *known-knowledge-bases*))
        (cerror (format nil "Load ~S before proceeding!" kb)
                "Unknown Knowledge Base ~S" kb)))))

(def$flavor normal-interface-mixin 
  ((dialog-stream *default-dialog-stream*)
   (file-name nil))                   ; a list of file names from kb parts
  (mac-menu-mixin)
  :settable-instance-variables
  (:required-instance-variables language kb-name))

(def$method (normal-interface-mixin :after :init) (&rest ignore)
  ($send self :set-up-windows))

(def$method (normal-interface-mixin :set-up-windows) ()
    (if ($send self :operation-handled-p :explanation-window)
      ($send self :send-if-handles :set-explanation-window 
             (make-$instance 'mac-window
                             :mw-scratch-p t
                             :mw-title (format nil "~@(~S~) Explanation Window" kb-name))))
    (let ((trace-window 
         (make-$instance 'mac-window
                         :mw-scratch-p t
                         :mw-title (format nil "~@(~S~) Trace Window" kb-name))))
    ($send self :send-if-handles :set-system-trace-window trace-window)
    ($send self :send-if-handles :set-consat-trace-window trace-window)
    ($send self :send-if-handles :set-rule-trace-window trace-window)
    ($send self :send-if-handles :set-prolog-trace-window trace-window)))

(def$method (normal-interface-mixin :deallocate-kb-windows) ()
  (dolist (f file-name)
    (let ((buffer (is-known-window f *kb-window*)))
      (if buffer (ask buffer (window-close)))))
  ($send self :send-if-handles :send-explanation-window :kill)
  ($send self :send-if-handles :send-system-trace-window :kill))

(def$method (normal-interface-mixin :allocate-kb-windows) ()
  ($send self :set-up-windows)
  (dolist (pathname file-name)
    ;8.6.89 to avoid duplicate editor buffers
    (unless (is-known-window pathname *kb-window*) 
      (eval `(make-$instance 'mac-window 
                             :mw-filename ',pathname
                             :mw-title ,(file-namestring pathname)))))) 

(def$method (normal-interface-mixin :loaded) (file)
  (unless (member file file-name :test #'equal)
    (setf file-name (append file-name (list file))))
  t) 

#+FMCS(def$method (normal-interface-mixin :after :toggle-frcheck) ()
       (setf *redefine-warnings* ($send frame-proocessor :frcheck))) 

(def$method (normal-interface-mixin :after :make-yourself-current) ()
  (if (flavor-typep self 'basic-frame-mixin)
    (ask frame-menu (menu-enable))
    (ask frame-menu (menu-disable)))
  (if (flavor-typep self 'basic-constraint-mixin)
    (ask consat-menu (menu-enable))
    (ask consat-menu (menu-disable)))
  (if (flavor-typep self 'basic-rule-mixin)
    (ask rule-menu (menu-enable))
    (ask rule-menu (menu-disable)))
  (if (flavor-typep self 'basic-prolog-mixin)
    (ask prolog-menu (menu-enable))
    (ask prolog-menu (menu-disable)))
  (ask babylon-menu (set-menu-title (string-capitalize kb-name))))

(def$method (normal-interface-mixin :after :make-yourself-unknown) ()
  ($send self :deselect-kb)
  ($send self :send-if-handles :send-explanation-window :kill)
  ($send self :send-if-handles :send-system-trace-window :kill)
  (dolist (file file-name)
    (let ((kb-window (is-known-window file *fred-window*)))
      (if kb-window (ask kb-window (window-close))))))

(def$method (normal-interface-mixin :after :reset-kb) ()
  ($send self :clear))

(def$method (normal-interface-mixin :refresh-yourself) ()
  "this is the stub for daemons to refresh dialogs"
  t)

(def$method (normal-interface-mixin :after :select-kb) ()
  "this is the stub for daemons to refresh dialogs"
  ($send self :refresh-yourself)
  (let ((kb-window (is-known-window (first (last file-name)) *fred-window*)))
    (if kb-window (ask kb-window (window-select)))))

#|
(def$method (normal-interface-mixin :before :explain-results) ()
  ($send self :send-if-handles :send-explanation-window :expose))

(def$method (normal-interface-mixin :after :explain-results) ()
  ($send self :send-if-handles :send-explanation-window :bury))
|#

(def$method (normal-interface-mixin :send-dialog-window) (selector &rest args)
  (lexpr-$send self selector args))

(def$method (normal-interface-mixin :format) (fstr &rest args)
  (ask *top-listener* (window-select))     ; dialog-stream hat keine window operationen
  (apply #'format dialog-stream fstr args))

(def$method (normal-interface-mixin :babylon-format) (fstr &rest args)
  (ask *top-listener* (window-select))
  (apply #'format dialog-stream fstr args))


#+:CCL-1.3
(def$method (normal-interface-mixin :babylon-read) (&optional (key-list NIL))
  (let* ((m-line (string-left-trim '(#\ ) (read-line dialog-stream)))
         (char (if (> (length m-line) 0) (aref m-line 0) *c-help-key*)))
    (cond ((member char key-list :test 'char-equal) char)
          (t (read-from-string m-line nil)))))

#-:CCL-1.3
(def$method (normal-interface-mixin :babylon-read) (&optional (key-list NIL))
  (ask *top-listener*
    (loop
      (multiple-value-bind (message modifiers)
                           (get-next-key-event 
                            (format nil "enter ~{~C or ~}type in" key-list))
        (let ((key (code-char (event-keystroke message modifiers))))
          (unless (null key)
            (if (member key key-list :test 'char-equal) 
              (return key)
              (progn
                (ask *top-listener* (ed-insert-char key) (window-update))
                (return (read-from-string (read-line) nil))))))))))

(def$method (normal-interface-mixin :after :babylon-read) (&optional (key-list NIL))
  (ask *top-listener* (set-mini-buffer " Busy")))

(def$method (normal-interface-mixin :clear) ()
  ($send self :send-if-handles :send-explanation-window :clear)
  ($send self :send-if-handles :send-system-trace-window :clear))

(def$method (normal-interface-mixin :run-loop) ()
  ($send self :format
         (getentry notify-on-select-fstr babylon-io-table)
         kb-name
         (get-prompt-string))
  :exit)

;;; *********** i o operations ***********

(defun dialog-dimensions (&rest messages)
  (let ((msg (format nil "~{~A~%~}" messages)))
    (values msg
            (make-point 
             (max 318 
                  (min (- *screen-width* 60)
                       (+ 50 (* 11 (apply #'max (mapcar #'length messages))))))
             (max 145 
                  (min (- *screen-height* 20)
                       (+ 100  (* 16 (count #\Newline msg)))))))))

(def$method (normal-interface-mixin :notify) (&rest messages)
  (multiple-value-bind (msg size) (apply #'dialog-dimensions messages)
    (message-dialog msg :size size)))

(def$method (normal-interface-mixin :confirm) (&rest messages)
  (multiple-value-bind (msg size) (apply #'dialog-dimensions messages)
    (y-or-n-dialog msg :size size)))

(def$method (normal-interface-mixin :type-end-to-continue) (string)
  (format dialog-stream "~%~A ~@? " *bab-prompt* string *end-key*)
  (do ((char (read-char dialog-stream)
             (read-char dialog-stream)))
      ((eql char *end-key*) :ok)))

(def$method (normal-interface-mixin :prompt-for-input) (message)
  (get-string-from-user message))

;;; -------------------------------------------------------------------

(defun deallocate-kb-windows ()
  (dolist (kb *known-knowledge-bases*)
    ($send (symbol-value kb) :send-if-handles :deallocate-kb-windows)))

(defun allocate-kb-windows ()
  (dolist (kb *known-knowledge-bases*)
    ($send (symbol-value kb) :send-if-handles :allocate-kb-windows)))

(progn
  (push (symbol-function 'deallocate-kb-windows) 
        *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'allocate-kb-windows)))))

(defun refresh-kb-windows ()
  (if *current-knowledge-base*
    ($send *current-knowledge-base* :send-if-handles :refresh-yourself)))

(defun save-kb-image ()
  (when (and (or *current-knowledge-base*
                 (not (y-or-n-dialog 
                       "There is no Knowledge Base!
Save an Image anyway?" :yes-text "No" :no-text "Yes" :cancel-text nil)))
             (y-or-n-dialog "Are you sure to save an Image?" :cancel-text nil))
    (let ((compressp (not (y-or-n-dialog "Compress the Image?" 
                                         :yes-text "No" 
                                         :no-text "Yes"
                                         :cancel-text nil)))
          (file (choose-new-file-dialog 
                 :directory (make-pathname 
                             :directory (pathname-directory *babylon-kbs*) 
                             :name "foo"))))
      (unless (eq (first (last *restore-lisp-functions*))
                  (symbol-function 'refresh-kb-windows))
        (setf *restore-lisp-functions*
              (append *restore-lisp-functions*
                      (list (symbol-function 'refresh-kb-windows)))))
      (dumplisp file :compress compressp))))
      
      
      
;;; eof

