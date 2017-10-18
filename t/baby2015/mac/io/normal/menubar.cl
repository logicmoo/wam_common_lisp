;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10  -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;  BABYLON Commands and Menus


(defvar Babylon-Copyright 
  (oneof *menu-item*
         :menu-item-title "About Babylon ..."
         :menu-item-action
         '(message-dialog 
           (format nil "
Babylon Version ~A for the Macintosh 
using 
~A ~A

Copyright © 1992 by
  GMD / FIT - KI
  Postfach 1316
  D-5205 Sankt Augustin 1
  Tel.:  (02241) 14-2661
  email: juergen@gmdzi.gmd.de
" *babylon-version* (lisp-implementation-type) (lisp-implementation-version))
           :position (make-point (ash (- *screen-width* 340) -1) 60) 
           :size #@(360 230))))

(ask *apple-menu* 
  (apply #'remove-menu-items (menu-items))
  (add-menu-items Babylon-Copyright #-:CCL-1.3 (oneof *menu-item* :menu-item-title "-")))

(defvar Babylon-Start-Command 
  (oneof *menu-item*
         :menu-item-title "Start" 
         ; :command-key #\S
         :menu-item-action 
         '(eval-enqueue 
           '(progn
              (ask *top-listener* (window-select))
              ($send *current-knowledge-base* :start-kb-confirmed)))
         :disabled t))

(defobfun (menu-item-update Babylon-Start-Command) ()
  (if *current-knowledge-base* 
    (menu-item-enable)
    (menu-item-disable)))

(defvar Babylon-Describe-Command 
  (oneof *menu-item*
         :menu-item-title "Describe" 
         :command-key #\D
         :menu-item-action '($send *current-knowledge-base* :kb-inform)
         :disabled t))

(defobfun (menu-item-update Babylon-Describe-Command) ()
  (if *current-knowledge-base* 
    (menu-item-enable)
    (menu-item-disable)))

(defvar Babylon-Trace-Command 
  (oneof *menu-item*
         :menu-item-title "Trace" 
         :menu-item-action 
         '($send *current-knowledge-base* :toggle-system-trace)
         :disabled t))

(defobfun (menu-item-update Babylon-Trace-Command) ()
  (if *current-knowledge-base*
    (progn (menu-item-enable)
           (if ($send *current-knowledge-base* :system-trace)
             (set-menu-item-check-mark t)
             (set-menu-item-check-mark nil)))
    (menu-item-disable)))


#|
(defvar Babylon-Reset-Command 
  (oneof *menu-item*
         :menu-item-title "Reset" 
         :command-key #\R
         :menu-item-action 
         '(eval-enqueue 
           '(if (y-or-n-dialog (format nil "Are you sure to reset Knowledge Base~%~A"
                                       (send-kb :kb-name)))
              (with-cursor *watch-cursor*
                ($send *Current-Knowledge-Base* :reset-kb))))
         :disabled t))
|#

(defvar Babylon-Reset-Command 
  (oneof *menu-item*
         :menu-item-title "Reset" 
         :command-key #\R
         :menu-item-action 
         '(eval-enqueue 
           '($send *current-knowledge-base* :reset-kb-confirmed))
         :disabled t))

(defobfun (menu-item-update Babylon-Reset-Command) ()
  (if *current-knowledge-base* 
    (menu-item-enable)
    (menu-item-disable)))

(defvar Babylon-Kill-Command 
  (oneof *menu-item*
         :menu-item-title "Kill" 
         :menu-item-action 
         '(eval-enqueue 
           '(if (y-or-n-dialog (format nil "Are you sure to kill Knowledge Base~%~A"
                                       (send-kb :kb-name)))
              (with-cursor *watch-cursor*
                ($send *current-knowledge-base* :kill-kb)
                (gc)
                (unless *current-knowledge-base*
                  (progn (ask frame-menu (menu-disable))
                         (ask rule-menu (menu-disable))
                         (ask prolog-menu (menu-disable))
                         (ask consat-menu (menu-disable))
                         (ask babylon-menu (set-menu-title "Babylon")))))))
         :disabled nil))

(defobfun (menu-item-update Babylon-Kill-Command) ()
  (if *current-knowledge-base* 
    (menu-item-enable)
    (menu-item-disable)))

(defun module= (dir1 dir2)
  (string= dir1 
           (expand-logical-namestring  
            (directory-namestring (transform-pathstring dir2 'source)))))

(defun register-new-kbs-dir (file)
  (let* ((dir (pathname-directory file))
         (end (1- (length dir)))
         (configs (concatenate 'string 
                               (subseq dir 
                                       0 
                                       (1+ (position #\: dir :end end :from-end t)))
                               "configs:")))
    (if (probe-file configs)
      (or (member configs *babylon-module-search-path* :test #'module=)
          (push configs *babylon-module-search-path*))
      (message-dialog (format nil "Directory ~S missing! You may loose!" configs)))
    (setf *babylon-kbs* file)))

(defvar Babylon-Load-Command 
  (oneof *menu-item*
         :menu-item-title "Load-KB" 
         :menu-item-action 
         '(eval-enqueue 
           '(let* ((file (register-new-kbs-dir
                          (choose-file-dialog :mac-file-type '(:TEXT :FASL)
                                              :directory (if (probe-file *babylon-kbs*)
                                                           *babylon-kbs*
                                                           (user-homedir-pathname)))))
                   (source-file (if (string-equal (pathname-type file) "fasl")
                                  (or (probe-file (make-pathname 
                                                   :directory (pathname-directory file)
                                                   :name (pathname-name file)
                                                   :type "lisp"))
                                      (choose-file-dialog :mac-file-type :TEXT))
                                  file))
                   (window (is-known-window source-file *kb-window*)))
              (if window
                (ask window (window-select))
                (let ((new-window (make-$instance 'mac-window
                                                  :mw-filename source-file 
                                                  :mw-show-p nil
                                                  :mw-scratch-p nil)))
                  (load file :verbose t :print t) 
                  ($send *current-knowledge-base* :loaded source-file)
                  ($send *current-knowledge-base* :select-kb)
                  (ask ($send new-window :stream)
                    (set-window-title (file-namestring source-file))
                    ; (format nil "~@(~S~) Knowledge Base" 
                    ;         ($send *current-knowledge-base* :kb-name)))
                    (window-select))))))))

(defvar Babylon-Select-Command 
  (oneof *menu-item*
         :menu-item-title "Select-KB"
         :disabled t
         :menu-item-action 
         '($send 
           (if (> (length *known-knowledge-bases*) 1)
             (symbol-value (pop-up :item-list *known-knowledge-bases*
                                   :dispatch-function #'(lambda (kb) kb)))
             (symbol-value (car *known-knowledge-bases*)))
           :select-kb)))

(defobfun (menu-item-update Babylon-Select-Command) ()
  (if *current-knowledge-base* 
    (menu-item-enable)
    (menu-item-disable)))


#|       
(defvar Babylon-Help-Command 
  (oneof *menu-item*
         :menu-item-title "Help" 
         :menu-item-action 
         '(let* ((pathname (choose-file-dialog :directory "babylon-help;"))
                 (filename (file-namestring pathname))
                 (window (is-known-window pathname *fred-window*))
                 (new-window (or window
                                 (oneof *fred-window* 
                                        :filename pathname 
                                        :window-title filename))))
            (ask new-window (window-select)))))
|#

(defvar Babylon-Menu 
  (oneof *menu* 
         :menu-title "Babylon"
         :menu-items (list 
                      Babylon-Start-Command 
                      Babylon-Describe-Command 
                      Babylon-Trace-Command 
                      Babylon-Reset-Command  
                      Babylon-Kill-Command  
                      (oneof *menu-item* :menu-item-title "-")
                      Babylon-Load-Command 
                      Babylon-Select-Command
                      (oneof *menu-item* :menu-item-title "-"))))


;;; FRAME Commands and Menu


(defvar Frame-Check-Command 
  (oneof *menu-item*
         :menu-item-title "Check" 
         :menu-item-action 
         '($send *current-knowledge-base* :toggle-frcheck)))

(defobfun (menu-item-update Frame-Check-Command) ()
  (if (and *current-knowledge-base* 
           ($send *current-knowledge-base* 
                  :operation-handled-p :toggle-frcheck))
    (progn (menu-item-enable)
           (if ($send ($send *current-knowledge-base* :frame-processor) :frcheck)
             (set-menu-item-check-mark t)
             (set-menu-item-check-mark nil)))
    (menu-item-disable)))

(defvar Frame-Explore-Command 
  (oneof *menu-item*
         :menu-item-title "Explore Objects" 
         :menu-item-action 
         '($send *current-knowledge-base* :explore-objects)))

(defobfun (menu-item-update Frame-Explore-Command) ()
  (if (and *current-knowledge-base* 
           ($send *current-knowledge-base* 
                  :operation-handled-p :explore-objects))
    (menu-item-enable)
    (menu-item-disable)))


(defvar Frame-Menu 
  (oneof *menu* 
         :menu-title "Frame"
         :menu-items (list Frame-Check-Command 
                           Frame-Explore-Command)))

(ask Frame-Menu (menu-disable))

;;; RULE Commands and Menu


(defvar Rule-Trace-Command 
  (oneof *menu-item*
         :menu-item-title "Trace" 
         :menu-item-action 
         '($send *current-knowledge-base* :toggle-rule-trace)
         :disabled t))

(defobfun (menu-item-update Rule-Trace-Command) ()
  (if (and *current-knowledge-base* 
           ($send *current-knowledge-base* 
                  :operation-handled-p :toggle-rule-trace))
    (progn (menu-item-enable)
           (if ($send (send-kb :rule-processor) :rule-trace)
             (set-menu-item-check-mark t)
             (set-menu-item-check-mark nil)))
    (menu-item-disable)))

(defvar Rule-Trace-Options-Command 
  (oneof *menu-item*
         :menu-item-title "Set Trace Options" 
         :menu-item-action 
         '($send *current-knowledge-base* :set-rule-trace-options)
         :disabled t))

(defobfun (menu-item-update Rule-Trace-Options-Command) ()
  (if (and *current-knowledge-base*
           ($send *current-knowledge-base* 
                  :operation-handled-p :set-rule-trace-options)) 
    (menu-item-enable)
    (menu-item-disable)))

(defvar Rule-Explore-Command 
  (oneof *menu-item*
         :menu-item-title "Explore Rules" 
         :menu-item-action '(send-kb :explore-rules)))

(defobfun (menu-item-update Rule-Explore-Command) ()
  (if (and *current-knowledge-base* 
           (flavor-typep *current-knowledge-base* 'basic-rule-mixin))
    (menu-item-enable)
    (menu-item-disable)))

(defvar Rule-Terms-Explore-Command 
  (oneof *menu-item*
         :menu-item-title "Explore Rule Terms" 
         :menu-item-action '(send-kb :explore-rule-terms)))

(defobfun (menu-item-update Rule-Terms-Explore-Command) ()
  (if (and *current-knowledge-base* 
           (flavor-typep *current-knowledge-base* 'normal-rule-mixin))
    (menu-item-enable)
    (menu-item-disable)))

(defvar Facts-Explore-Command 
  (oneof *menu-item*
         :menu-item-title "Explore Facts" 
         :menu-item-action '(send-kb :explore-facts)))

(defobfun (menu-item-update Facts-Explore-Command) ()
  (if (and *current-knowledge-base* 
           (flavor-typep *current-knowledge-base* 'basic-rule-mixin))
    (menu-item-enable)
    (menu-item-disable)))

(defvar Rule-Hypotheses-Command 
  (oneof *menu-item*
         :menu-item-title "Hypotheses" 
         :menu-item-action 
         '($send *current-knowledge-base* :print-hypotheses-verified)))

(defobfun (menu-item-update Rule-Hypotheses-Command) ()
  (if (and *current-knowledge-base* 
           ($send *current-knowledge-base* 
                  :operation-handled-p :print-hypotheses-verified))
    (menu-item-enable)
    (menu-item-disable)))

(defvar Rule-Explain-Command 
  (oneof *menu-item*
         :menu-item-title "Explain" 
         :menu-item-action 
         '($send *current-knowledge-base* :explain-results)))

(defobfun (menu-item-update Rule-Explain-Command) ()
  (if (and *current-knowledge-base* 
           ($send *current-knowledge-base* :operation-handled-p :explain-results))
    (menu-item-enable)
    (menu-item-disable)))

(defvar Rule-Menu 
  (oneof *menu* 
         :menu-title "Rule"
         :menu-items (list
                      Rule-Trace-Command 
                      Rule-Trace-Options-Command
                      Rule-Explore-Command
                      Rule-Terms-Explore-Command 
                      Facts-Explore-Command 
                      Rule-Hypotheses-Command
                      Rule-Explain-Command)))

(ask Rule-Menu (menu-disable))

;;; PROLOG Command and Menu


(defvar Prolog-Trace-Command 
  (oneof *menu-item*
         :menu-item-title "Trace" 
         :menu-item-action 
         '($send *current-knowledge-base* :toggle-prolog-trace)
         :disabled t))

(defobfun (menu-item-update Prolog-Trace-Command) ()
  (if (and *current-knowledge-base*
           ($send *current-knowledge-base* 
                  :operation-handled-p :prolog-trace-window)) 
    (progn (menu-item-enable)
           (if ($send (send-kb :prolog-processor) :prolog-trace)
             (set-menu-item-check-mark t)
             (set-menu-item-check-mark nil)))
    (menu-item-disable)))

(defvar Prolog-Trace-Options-Command 
  (oneof *menu-item*
         :menu-item-title "Set Trace Options" 
         :menu-item-action 
         '($send *current-knowledge-base* :set-prolog-trace-options)
         :disabled t))

(defobfun (menu-item-update Prolog-Trace-Options-Command) ()
  (if (and *current-knowledge-base*
           ($send *current-knowledge-base* 
                  :operation-handled-p :set-prolog-trace-options)) 
    (menu-item-enable)
    (menu-item-disable)))

(defvar Prolog-Load-Command 
  (oneof *menu-item*
         :menu-item-title "Load Axset" 
         :menu-item-action 
         '(let* ((file (setf *babylon-axs* 
                             (choose-file-dialog :mac-file-type :TEXT
                                                 :directory (if (probe-file *babylon-axs*)
                                                              *babylon-axs*
                                                              (user-homedir-pathname)))))
                 (window (is-known-window file *kb-window*)))
            (if window
              (ask window (window-select))
              (let ((new-window (make-$instance 'mac-window
                                                :mw-filename file 
                                                :mw-show-p nil
                                                :mw-scratch-p nil)))
                (load file :verbose t :print t)
                (setf (get (first *axiom-sets*) '%editor-buffer) ($send new-window :stream))
                (ask ($send new-window :stream)
                  (set-window-title (file-namestring file))
                  ; (format nil "~@(~S~) Axiom Set" (first *axiom-sets*)))
                  (window-select)))))))

(defvar Prolog-Display-Command 
  (oneof *menu-item*
         :menu-item-title "Explore Axset" 
         :menu-item-action 
         '($send *current-knowledge-base* :explore-axset)))

(defvar Prolog-Select-Command 
  (oneof *menu-item*
         :menu-item-title "Select Axset" 
         :menu-item-action 
         '($send *current-knowledge-base* :select-load-axioms)
         :disabled t))

(defobfun (menu-item-update Prolog-Select-Command) ()
  (if (and *current-knowledge-base* 
           ($send *current-knowledge-base* 
                  :operation-handled-p :select-load-axioms))
    (menu-item-enable)
    (menu-item-disable)))

(defvar Prolog-Prove-Command 
  (oneof *menu-item*
         :menu-item-title "Prove" 
         :menu-item-action 
         '(eval-enqueue 
           '(progn
              (ask (find-window "Listener") (window-select))
              ($send *current-knowledge-base* :prove-display)))
         :disabled t))


(defobfun (menu-item-update Prolog-Prove-Command) ()
  (if (and *current-knowledge-base* 
           ($send *current-knowledge-base* 
                  :operation-handled-p :prove-display))
    (menu-item-enable)
    (menu-item-disable)))


(defvar Prolog-Next-Command 
  (oneof *menu-item*
         :menu-item-title "Next" 
         :command-key #\; 
         :menu-item-action 
         '(eval-enqueue 
           '($send *current-knowledge-base* :prove-display '*))
         :disabled t))

(defobfun (menu-item-update Prolog-Next-Command) ()
  (if (and *current-knowledge-base* 
           ($send *current-knowledge-base* 
                  :operation-handled-p :prove-display))
    (menu-item-enable)
    (menu-item-disable)))

(defvar Prolog-Menu 
  (oneof *menu* 
         :menu-title "Prolog"
         :menu-items (list Prolog-Trace-Command
                           Prolog-Trace-Options-Command
                           Prolog-Display-Command 
                           Prolog-Load-Command 
                           Prolog-Select-Command
                           Prolog-Prove-Command
                           Prolog-Next-Command)))

(ask Prolog-Menu (menu-disable))

;;; CONSAT Commands and Menu


(defvar Consat-Trace-Command 
  (oneof *menu-item*
         :menu-item-title "Trace" 
         :menu-item-action 
         '(trace-constraints)))

(defobfun (menu-item-update Consat-Trace-Command) ()
  (let ((cp (and *current-knowledge-base* 
                 (flavor-typep *current-knowledge-base* 'mini-constraint-mixin)
                 ($send *current-knowledge-base* :send-if-handles :constraint-processor))))
    (if cp
      (progn (menu-item-enable)
             (if ($send (send-kb :constraint-processor) :send-if-handles :trace)
               (set-menu-item-check-mark t)
               (set-menu-item-check-mark nil)))
      (progn (menu-item-disable)
             (set-menu-item-check-mark nil)))))

(defvar Consat-Define-Command 
  (oneof *menu-item*
         :menu-item-title "Define Constraint" 
         :menu-item-action 
         '(read-constraint)))

(defobfun (menu-item-update Consat-Define-Command) ()
  (if (and *current-knowledge-base* 
           (flavor-typep *current-knowledge-base* 'basic-constraint-mixin))
    (menu-item-enable)
    (menu-item-disable)))

(defvar Consat-Explore-Command 
  (oneof *menu-item*
         :menu-item-title "Explore Constraints" 
         :menu-item-action '(send-kb :explore-constraint)))

(defobfun (menu-item-update Consat-Explore-Command) ()
  (if (and *current-knowledge-base* 
           (flavor-typep *current-knowledge-base* 'basic-constraint-mixin))
    (menu-item-enable)
    (menu-item-disable)))

(defvar Consat-Locally-Command 
  (oneof *menu-item*
         :menu-item-title "Satisfy Locally" 
         :menu-item-action 
         '(satisfy-constraint-locally)))

(defobfun (menu-item-update Consat-Locally-Command) ()
  (if (and *current-knowledge-base* 
           (flavor-typep *current-knowledge-base* 'basic-constraint-mixin))
    (menu-item-enable)
    (menu-item-disable)))

(defvar Consat-Globally-Command 
  (oneof *menu-item*
         :menu-item-title "Satisfy Globally" 
         :menu-item-action 
         '(satisfy-constraint-globally)))

(defobfun (menu-item-update Consat-Globally-Command) ()
  (if (and *current-knowledge-base* 
           (flavor-typep *current-knowledge-base* 'basic-constraint-mixin))
    (menu-item-enable)
    (menu-item-disable)))

(defvar Consat-Menu 
  (oneof *menu* 
         :menu-title "Consat"
         :menu-items (list Consat-Trace-Command
                           Consat-Explore-Command
                           Consat-Define-Command
                           Consat-Locally-Command
                           Consat-Globally-Command)))

(ask Consat-Menu (menu-disable))

;;; the menus are installed in the make file

;;; eof

