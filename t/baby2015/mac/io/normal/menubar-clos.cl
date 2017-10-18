;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10  -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;  BABYLON Commands and Menus (CLOS version)


(defvar Babylon-Copyright 
  (make-instance  'menu-item
                  :menu-item-title "About Babylon ..."
                  :menu-item-action
                  #'(lambda ()
                      (message-dialog 
                       (format nil "
Babylon Version ~A for the Macintosh 
using 
~A ~A

Copyright © 1994 by
  GMD / FIT - KI
  Postfach 1316
  D-53731 Sankt Augustin
  Tel.:  (02241) 14-2661
  email: juergen.walther@gmd.de
" *babylon-version* (lisp-implementation-type) (lisp-implementation-version))
                       :position (make-point (ash (- *screen-width* 340) -1) 60) 
                       :size #@(360 230)))))

(progn
  (apply #'remove-menu-items *apple-menu* (menu-items *apple-menu*))
  (add-menu-items *apple-menu* Babylon-Copyright))

(defvar Babylon-Start-Command 
  (make-instance 'menu-item
                 :menu-item-title "Start" 
                 :menu-item-action
                 #'(lambda ()
                     (eval-enqueue 
                      '(progn
                         (window-select *top-listener*)
                         ($send *current-knowledge-base* :start-kb-confirmed))))
                 :disabled t))

(defun enable-if-kb-exists (menu-item)
  (if *current-knowledge-base* 
    (menu-item-enable menu-item)
    (menu-item-disable menu-item)))

(set-menu-item-update-function Babylon-Start-Command #'enable-if-kb-exists)

(defvar Babylon-Describe-Command 
  (make-instance 'menu-item
                 :menu-item-title "Describe" 
                 ;:command-key #\D
                 :menu-item-action 
                 #'(lambda () ($send *current-knowledge-base* :kb-inform))
                 :disabled t))

(set-menu-item-update-function Babylon-Describe-Command #'enable-if-kb-exists)

(defvar Babylon-Trace-Command 
  (make-instance 'menu-item
                 :menu-item-title "Trace" 
                 :menu-item-action 
                 #'(lambda () ($send *current-knowledge-base* :toggle-system-trace))
                 :disabled t))

(set-menu-item-update-function  
 Babylon-Trace-Command
 #'(lambda (self)
     (if *current-knowledge-base*
       (progn (menu-item-enable self)
              (if ($send *current-knowledge-base* :system-trace)
                (set-menu-item-check-mark self t)
                (set-menu-item-check-mark self nil)))
       (menu-item-disable self))))


(defvar Babylon-Reset-Command 
  (make-instance 'menu-item
                 :menu-item-title "Reset" 
                 ;:command-key #\R
                 :menu-item-action 
                 #'(lambda () 
                     (eval-enqueue 
                      '($send *current-knowledge-base* :reset-kb-confirmed)))
                 :disabled t))

(set-menu-item-update-function Babylon-Reset-Command #'enable-if-kb-exists)

(defvar Babylon-Kill-Command 
  (make-instance 'menu-item
    :menu-item-title "Kill" 
    :menu-item-action 
    #'(lambda ()
        (eval-enqueue 
         '(if (y-or-n-dialog (format nil "Are you sure to kill Knowledge Base~%~A"
                                     ($send *current-knowledge-base* :kb-name)))
            (progn
              ($send *current-knowledge-base* :kill-kb)
              (unless *current-knowledge-base*
                (menu-disable frame-menu)
                (menu-disable rule-menu)
                (menu-disable prolog-menu)
                (menu-disable consat-menu)
                (set-menu-title babylon-menu "Babylon"))))))
    :disabled nil))

(set-menu-item-update-function Babylon-Kill-Command #'enable-if-kb-exists)

(defun module= (dir1 dir2)
  (string= dir1 (namestring (transform-pathstring dir2 'source))))

(defun register-new-kbs-dir (file)
  (let* ((dir (pathname-directory file))
         (configs (namestring 
                   (make-pathname :directory (append (butlast dir) (list "configs"))))))
    (if (probe-file configs)
      (or (member configs *babylon-module-search-path* :test #'module=)
          (push configs *babylon-module-search-path*))
      (message-dialog (format nil "Directory ~S missing! You may loose!" configs)))
    (setf *babylon-kbs* file)))

(defvar Babylon-Load-Command 
  (make-instance 'menu-item
                 :menu-item-title "Load-KB" 
                 :menu-item-action 
                 #'(lambda ()
                     (eval-enqueue 
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
                              (window (is-known-window source-file 'kb-window)))
                         (if window
                           (window-select window)
                           (let ((new (make-$instance 'mac-window
                                                      :mw-filename source-file 
                                                      :mw-title (file-namestring source-file)
                                                      :mw-show-p nil
                                                      :mw-scratch-p nil)))
                             (window-select ($send new :stream))
                             (with-compilation-unit () (load file :verbose t :print t))
                             ($send *current-knowledge-base* :loaded source-file)
                             ($send *current-knowledge-base* :select-kb))))))))

(defvar Babylon-Select-Command 
  (make-instance 'menu-item
                 :menu-item-title "Select-KB"
                 :disabled t
                 :menu-item-action 
                 #'(lambda ()
                     ($send (symbol-value (select-single *known-knowledge-bases*)) :select-kb))))

(set-menu-item-update-function 
 Babylon-Select-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* (> (length *known-knowledge-bases*) 1))
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Babylon-Menu 
  (make-instance 'menu
                 :menu-title "Babylon"
                 :menu-items (list 
                              Babylon-Start-Command 
                              Babylon-Describe-Command 
                              Babylon-Trace-Command 
                              Babylon-Reset-Command  
                              Babylon-Kill-Command  
                              (make-instance 'menu-item :menu-item-title "-")
                              Babylon-Load-Command 
                              Babylon-Select-Command
                              (make-instance 'menu-item :menu-item-title "-"))))


;;; FRAME Commands and Menu


(defvar Frame-Check-Command 
  (make-instance 'menu-item
                 :menu-item-title "Check" 
                 :menu-item-action
                 #'(lambda ()
                     ($send *current-knowledge-base* :toggle-frcheck))))

(set-menu-item-update-function 
 Frame-Check-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* 
              ($send *current-knowledge-base* :operation-handled-p :toggle-frcheck))
       (progn (menu-item-enable self)
              (if ($send (send-kb :frame-processor) :frcheck)
                (set-menu-item-check-mark self t)
                (set-menu-item-check-mark self nil)))
       (menu-item-disable self))))

(defvar Frame-Explore-Command 
  (make-instance 'menu-item
                 :menu-item-title "Explore Objects" 
                 :menu-item-action 
                 #'(lambda ()
                     ($send *current-knowledge-base* :explore-objects))))

(set-menu-item-update-function 
 Frame-Explore-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* 
              ($send *current-knowledge-base* :operation-handled-p :explore-objects))
       (menu-item-enable self)
       (menu-item-disable self))))


(defvar Frame-Menu 
  (make-instance 'menu
                 :menu-title "Frame"
                 :menu-items (list Frame-Check-Command 
                                   Frame-Explore-Command)))

(menu-disable Frame-Menu)

;;; RULE Commands and Menu


(defvar Rule-Trace-Command 
  (make-instance 'menu-item
                 :menu-item-title "Trace" 
                 :menu-item-action 
                 #'(lambda ()
                     ($send *current-knowledge-base* :toggle-rule-trace))
                 :disabled t))

(set-menu-item-update-function 
 Rule-Trace-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* 
              ($send *current-knowledge-base* :operation-handled-p :toggle-rule-trace))
       (progn (menu-item-enable self)
              (if ($send ($send *current-knowledge-base* :rule-processor) :rule-trace)
                (set-menu-item-check-mark self t)
                (set-menu-item-check-mark self nil)))
       (menu-item-disable self))))

(defvar Rule-Trace-Options-Command 
  (make-instance 'menu-item
                 :menu-item-title "Set Trace Options" 
                 :menu-item-action 
                 #'(lambda ()
                     ($send *current-knowledge-base* :set-rule-trace-options))
                 :disabled t))

(set-menu-item-update-function 
 Rule-Trace-Options-Command
 #'(lambda (self)
     (if (and *current-knowledge-base*
              ($send *current-knowledge-base* :operation-handled-p :set-rule-trace-options)) 
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Rule-Explore-Command 
  (make-instance 'menu-item
                 :menu-item-title "Explore Rules" 
                 :menu-item-action          
                 #'(lambda ()
                     ($send *current-knowledge-base* :explore-rules))))

(set-menu-item-update-function 
 Rule-Explore-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* 
              ($send *current-knowledge-base* :operation-handled-p :explore-rules))
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Rule-Terms-Explore-Command 
  (make-instance 'menu-item
                 :menu-item-title "Explore Rule Terms" 
                 :menu-item-action          
                 #'(lambda ()
                     ($send *current-knowledge-base* :explore-rule-terms))))

(set-menu-item-update-function 
 Rule-Terms-Explore-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* 
              ($send *current-knowledge-base* :operation-handled-p :explore-rule-terms))
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Facts-Explore-Command 
  (make-instance 'menu-item
                 :menu-item-title "Explore Facts" 
                 :menu-item-action 
                 #'(lambda ()
                     ($send *current-knowledge-base* :explore-facts))))

(set-menu-item-update-function 
 Facts-Explore-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* 
              ($send *current-knowledge-base* :operation-handled-p :explore-facts))
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Rule-Hypotheses-Command 
  (make-instance 'menu-item
                 :menu-item-title "Hypotheses" 
                 :menu-item-action 
                 #'(lambda ()
                     ($send *current-knowledge-base* :print-hypotheses-verified))))

(set-menu-item-update-function 
 Rule-Hypotheses-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* 
              ($send *current-knowledge-base* 
                     :operation-handled-p :print-hypotheses-verified))
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Rule-Explain-Command 
  (make-instance 'menu-item
                 :menu-item-title "Explain" 
                 :menu-item-action 
                 #'(lambda ()
                     ($send *current-knowledge-base* :explain-results))))

(set-menu-item-update-function 
 Rule-Explain-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* 
              ($send *current-knowledge-base* :operation-handled-p :explain-results))
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Rule-Menu 
  (make-instance 'menu
                 :menu-title "Rule"
                 :menu-items (list
                              Rule-Trace-Command 
                              Rule-Trace-Options-Command
                              Rule-Explore-Command
                              Rule-Terms-Explore-Command 
                              Facts-Explore-Command 
                              Rule-Hypotheses-Command
                              Rule-Explain-Command)))

(menu-disable Rule-Menu)

;;; PROLOG Command and Menu


(defvar Prolog-Trace-Command 
  (make-instance 'menu-item
                 :menu-item-title "Trace" 
                 :menu-item-action 
                 #'(lambda ()
                     ($send *current-knowledge-base* :toggle-prolog-trace))
                 :disabled t))

(set-menu-item-update-function 
 Prolog-Trace-Command
 #'(lambda (self)
     (if (and *current-knowledge-base*
              ($send *current-knowledge-base* :operation-handled-p :prolog-trace-window)) 
       (progn (menu-item-enable self)
              (if ($send ($send *current-knowledge-base* :prolog-processor) :prolog-trace)
                (set-menu-item-check-mark self t)
                (set-menu-item-check-mark self nil)))
       (menu-item-disable self))))

(defvar Prolog-Trace-Options-Command 
  (make-instance 'menu-item
                 :menu-item-title "Set Trace Options" 
                 :menu-item-action 
                 #'(lambda ()
                     ($send *current-knowledge-base* :set-prolog-trace-options))
                 :disabled t))

(set-menu-item-update-function 
 Prolog-Trace-Options-Command
 #'(lambda (self)
     (if (and *current-knowledge-base*
              ($send *current-knowledge-base* 
                     :operation-handled-p :set-prolog-trace-options)) 
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Prolog-Load-Command 
  (make-instance 'menu-item
                 :menu-item-title "Load Axset" 
                 :menu-item-action 
                 #'(lambda ()
                     (eval-enqueue
                      '(let* ((file (setf *babylon-axs* 
                                          (choose-file-dialog :mac-file-type :TEXT
                                                              :directory (if (probe-file *babylon-axs*)
                                                                           *babylon-axs*
                                                                           (user-homedir-pathname)))))
                              (window (is-known-window file 'kb-window)))
                         (if window
                           (window-select window)
                           (let ((new-window (make-$instance 'mac-window
                                                             :mw-filename file 
                                                             :mw-show-p nil
                                                             :mw-scratch-p nil)))
                             (load file :verbose t :print t)
                             (setf (get (first *axiom-sets*) '%editor-buffer) ($send new-window :stream))
                             (set-window-title ($send new-window :stream) (file-namestring file))
                             (window-select ($send new-window :stream)))))))))

(set-menu-item-update-function 
 Prolog-Load-Command
 #'(lambda (self)
     (if (and *current-knowledge-base*
              ($send *current-knowledge-base* 
                     :operation-handled-p :prolog-processor)) 
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Prolog-Display-Command 
  (make-instance 'menu-item
                 :menu-item-title "Explore Axset" 
                 :menu-item-action 
                 #'(lambda ()
                    ($send *current-knowledge-base* :explore-axset))))

(set-menu-item-update-function 
 Prolog-Display-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* 
              ($send *current-knowledge-base* 
                     :operation-handled-p :explore-axset))
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Prolog-Select-Command 
  (make-instance 'menu-item
                 :menu-item-title "Select Axset" 
                 :menu-item-action 
                 #'(lambda ()
                     ($send *current-knowledge-base* :select-load-axioms))
                 :disabled t))

(set-menu-item-update-function 
 Prolog-Select-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* 
              ($send *current-knowledge-base* 
                     :operation-handled-p :select-load-axioms))
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Prolog-Prove-Command 
  (make-instance 'menu-item
                 :menu-item-title "Prove" 
                 :menu-item-action 
                 #'(lambda ()
                     (eval-enqueue 
                      '(progn
                         (window-select (find-window "Listener"))
                         ($send *current-knowledge-base* :prove-display)))
                     :disabled t)))

(set-menu-item-update-function 
 Prolog-Prove-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* 
              ($send *current-knowledge-base* 
                     :operation-handled-p :prove-display))
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Prolog-Next-Command 
  (make-instance 'menu-item
                 :menu-item-title "Next" 
                 :command-key #\+ 
                 :menu-item-action 
                 #'(lambda ()
                     (eval-enqueue 
                      '($send *current-knowledge-base* :prove-display '*)))
                 :disabled t))

(set-menu-item-update-function 
 Prolog-Next-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* 
              ($send *current-knowledge-base* 
                     :operation-handled-p :prove-display))
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Prolog-Menu 
  (make-instance 'menu
                 :menu-title "Prolog"
                 :menu-items (list Prolog-Trace-Command
                                   Prolog-Trace-Options-Command
                                   Prolog-Display-Command 
                                   Prolog-Load-Command 
                                   Prolog-Select-Command
                                   Prolog-Prove-Command
                                   Prolog-Next-Command)))

(menu-disable Prolog-Menu)

;;; CONSAT Commands and Menu


(defvar Consat-Trace-Command 
  (make-instance 'menu-item
                 :menu-item-title "Trace" 
                 :menu-item-action 
                 #'(lambda ()
                     (eval-enqueue '(trace-constraints)))))

(set-menu-item-update-function 
 Consat-Trace-Command
 #'(lambda (self)
     (let ((cp (and *current-knowledge-base* 
                    (flavor-typep *current-knowledge-base* 'mini-constraint-mixin)
                    ($send *current-knowledge-base* :send-if-handles :constraint-processor))))
       (if cp
         (progn (menu-item-enable self)
                (if ($send ($send *current-knowledge-base* :constraint-processor) :send-if-handles :trace)
                  (set-menu-item-check-mark self t)
                  (set-menu-item-check-mark self nil)))
         (progn (menu-item-disable self)
                (set-menu-item-check-mark self nil))))))

(defvar Consat-Define-Command 
  (make-instance 'menu-item
                 :menu-item-title "Define Constraint" 
                 :menu-item-action 
                 #'(lambda ()
                     (eval-enqueue '(read-constraint)))))

(set-menu-item-update-function 
 Consat-Define-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* 
              (flavor-typep *current-knowledge-base* 'basic-constraint-mixin))
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Consat-Explore-Command 
  (make-instance 'menu-item
                 :menu-item-title "Explore Constraints" 
                 :menu-item-action 
                 #'(lambda () 
                     (eval-enqueue 
                      '($send *current-knowledge-base* :explore-constraint)))))


(set-menu-item-update-function 
 Consat-Explore-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* 
              ($send *current-knowledge-base* :operation-handled-p :explore-constraint))
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Consat-Locally-Command 
  (make-instance 'menu-item
                 :menu-item-title "Satisfy Locally" 
                 :menu-item-action 
                 #'(lambda ()
                     (eval-enqueue '(satisfy-constraint-locally)))))

(set-menu-item-update-function 
 Consat-Locally-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* 
              (flavor-typep *current-knowledge-base* 'basic-constraint-mixin))
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Consat-Globally-Command 
  (make-instance 'menu-item
                 :menu-item-title "Satisfy Globally" 
                 :menu-item-action 
                 #'(lambda ()
                     (eval-enqueue '(satisfy-constraint-globally)))))

(set-menu-item-update-function 
 Consat-Globally-Command
 #'(lambda (self)
     (if (and *current-knowledge-base* 
              (flavor-typep *current-knowledge-base* 'basic-constraint-mixin))
       (menu-item-enable self)
       (menu-item-disable self))))

(defvar Consat-Menu 
  (make-instance 'menu
                 :menu-title "Consat"
                 :menu-items (list Consat-Trace-Command
                                   Consat-Explore-Command
                                   Consat-Define-Command
                                   Consat-Locally-Command
                                   Consat-Globally-Command)))

(menu-disable Consat-Menu)

;;; the menus are installed in the make file

;;; eof

