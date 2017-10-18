;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Packagw: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;  FILE:    babylon^make-mcl.cl

;;           This is the make file for making a Babylon Image
;;           on an Apple Macintosh using Apple Macintosh Common Lisp 2.0.1 or better

;;           use file babylon^mac-make-CCL.cl 
;;           if you have Macintosh Allegro Common Lisp

(unless (member :MCL *features*)
  (progn (message-dialog "Sorry! This is the load file for Macintosh Common Lisp 2.0 or better")
         (quit)))

(defvar *ccl-dir* (logical-pathname-translations "ccl"))

(let ((dir (concatenate 
            'string
            (namestring
             (make-pathname :directory 
                            (pathname-directory *loading-file-source-file*)))
            "**:*.*")))
  (setf (logical-pathname-translations "babylon")
        `(("**;*.*" ,dir))))


;;; *********************************************************************************
(setf *recompile* t)               ; nil = load only 
;;; *********************************************************************************


(setq *load-verbose* t           ; default nil
      *verbose-eval-selection* t ; default nil
      *warn-if-redefine* t       ; default t
      *break-on-warnings* nil    ; default nil
      *break-on-errors* t        ; default t
      *backtrace-on-break* nil   ; default nil
      *fast-eval* nil            ; default nil
      *compile-definitions* t
      *fasl-compiler-warnings* t
      *paste-with-styles* nil
      *print-case* :upcase
      *print-array* nil
      *emacs-mode* nil
      ccl::*line-width* 80
      *save-fred-window-positions* nil)

(defun development-options ()
  (setq *record-source-file* t 
        *save-doc-strings* t 
        *save-definitions* t 
        *save-local-symbols* t
        *fasl-save-local-symbols* t))
      
(defun runtime-options ()
  (setq *record-source-file* t
        *save-doc-strings* nil 
        *save-definitions* nil 
        *save-local-symbols* nil
        *fasl-save-local-symbols* nil))

(defvar *fasl-opts* nil)

(progn
  (setf *fasl-opts* (y-or-n-p "Use development options for compiling files?"))
  (if *fasl-opts*
    (development-options)
    (runtime-options)))

;; adapt to different screen sizes for MAC SE or II in your init file

(defvar  *fred-window-position* *window-default-position*)
(defvar  *fred-window-size* *window-default-size*)

#|
(defun babylon-default-sizes ()
  (setf *fred-window-position*
        (make-point 4. (+ *menubar-bottom* 4))
        
        *fred-window-size*
        (make-point (- *screen-width* 80) 
                    (floor (- *screen-height* *menubar-bottom* 8.) 2.))
        
        *LISTENER-WINDOW-POSITION*
        (make-point 4. (+ 24. 
                          (point-v *fred-window-position*)
                          (point-v *fred-window-size*)))
        
        *LISTENER-WINDOW-SIZE*
        (make-point (point-h *fred-window-size*)
                    (- (point-v *fred-window-size*) 24))))
|#

(defun babylon-default-sizes () T)  ;;; do that in your customizations

(babylon-default-sizes)
  
(format t "~&;;; Loading Babylon ...~%")
  
(progn
  (set-view-position *top-listener* *listener-window-position*)
  (set-view-size *top-listener* *listener-window-size*))
  

;;; 

(defvar *babylon-kbs*  "babylon:samples;kbs;")
(defvar *babylon-axs*  "babylon:samples;axsets;")

(defvar *bab-host* "")

(setf *.LISP-PATHNAME* (make-pathname :type "cl"))

(load "babylon:kernel;require")
(load "babylon:kernel;babtrans")
  
(defun make-local-pathname (bab-path host type)
  (declare (ignore host))
  (let ((true-type (case type
                     (source "cl")
                     #-:MCL(bin    "cfasl") 
                     #+:MCL(bin    "mfasl")
                     (t      (string-downcase type)))))
    (merge-pathnames (substitute #\; #\> bab-path)
                     (concatenate 'string "foo." true-type))))
  
(setf *trans-path-fkt*  #'make-local-pathname)
  
(defbabylon-translation "babhome^" "babylon:")
(defbabylon-translation "kernel^"  "babhome^kernel>")
(defbabylon-translation "modules^" "babhome^kernel>modules>")
(defbabylon-translation "configs^" "babhome^samples>configs>")
(defbabylon-translation "fmcs^"    "babhome^fmcs>")
(defbabylon-translation "mac^"     "babhome^mac>")

(defbabylon-translation "tty^"     "babhome^tty>")
(defbabylon-translation "basic-interface-mixin" "b-interf")
(defbabylon-translation "mini-interface-mixin"  "m-interf")
(defbabylon-translation "mini-babylon" "m-interf")
  
(setf *babylon-module-search-path*
      '("mac^modules>" "modules^" "configs^"))  ;;; not on mac "tty^modules>" 
  
(cc-load "mac^extens")
  
(bab-require 'fmcs)     ; meta class system (flavor oriented)
(bab-require 'common)
(bab-require 'meta)

;;; alternatively

(bab-require 'normal-interface-mixin)
 
;;; or
;;; (bab-require 'mini-interface-mixin)
;;; (def$flavor normal-interface-mixin () (mini-interface-mixin))


;;; -----------------------------------------------------------------------
 

(defvar *BABYLON-MENUBAR-INSTALLED* nil)
(defvar *BABYLON-MENUBAR* nil) 
(defvar *ALLEGRO-MENUBAR* nil)
(defvar *BOTH-MENUBAR* nil) 

(defvar toggle-menubar-item
  (make-instance 'menu-item
    :menu-item-title "Toggle Menubar"
    :command-key #\T
    :menu-item-action
    #'(lambda () 
        (if (null *babylon-menubar-installed*) 
          (set-menubar *babylon-menubar*)
          (set-menubar *allegro-menubar*))
        (setf *babylon-menubar-installed* 
              (not *babylon-menubar-installed*)))))

(let ((*windows* (find-menu "Windows")))
  (declare (special babylon-menu frame-menu rule-menu prolog-menu consat-menu))
  (setf *allegro-menubar* (append (menubar) (list babylon-menu)))
  (setf *babylon-menubar* 
        (list *apple-menu* *file-menu* *edit-menu* *windows* 
              babylon-menu frame-menu rule-menu prolog-menu consat-menu))
  (setf *both-menubar* 
        (append *default-menubar* 
                (list babylon-menu frame-menu rule-menu prolog-menu consat-menu))))
  

(defun babylon-default-menubar ()
  (if (> *screen-width* 512)
    (set-menubar *both-menubar*)
    (progn (unless (find-menu-item babylon-menu "Toggle Menubar")
             (add-menu-items babylon-menu toggle-menubar-item))
           (set-menubar *babylon-menubar*)))
  (setf *babylon-menubar-installed* t))

(defun babylon-hallo ()
  (format t ";Welcome to Babylon Release ~A on the Macintosh!~%" *babylon-version*))

(defun babylon-initialize-image ()
  (setf (logical-pathname-translations "babylon")
        `(("**;*.*" ,(full-pathname "home:**;*.*"))))
  (setf (logical-pathname-translations "ccl") *ccl-dir*) ;;; what the hell
  (set-choose-file-default-directory "babylon:")
  (babylon-hallo)
  (if (or (probe-file "babylon:patches.cl")      ; loading patches
          (probe-file "babylon:patches.fasl"))             
    (load "babylon:patches"))
  (if (or (probe-file "babylon:bab-init.cl") ; and  your babylon init file
          (probe-file "babylon:bab-init.fasl"))              
    (load "babylon:bab-init")))

;(load "babylon:mac;compiler-warnings.cl")

(defvar *babylon-configure-dialog* ())
    
(defun deallocate-babylon-configure-dialog ()
  (window-close *babylon-configure-dialog*))
    
(defun make-configuration (dialog)
  (let ((file (catch :cancel 
                (choose-new-file-dialog :directory "babylon:Babylon"))))
    (if (not (eql :cancel file))
      (return-from-modal-dialog
       (progn
         (if (radio-button-pushed-p (view-named 'development dialog))
           (eval-enqueue '(eval `(development-options)))
           (eval-enqueue '(eval `(runtime-options))))
         (if (check-box-checked-p (view-named 'free-text dialog))
           (eval-enqueue '(bab-require 'free-text-mixin)))
         (if (not (radio-button-pushed-p (view-named 'fm dialog)))
           (if (radio-button-pushed-p (view-named 'nfm dialog))
             (eval-enqueue '(bab-require 'normal-frame-mixin))
             (if (radio-button-pushed-p (view-named 'mfm dialog))
               (eval-enqueue '(bab-require 'mini-frame-mixin))
               (eval-enqueue '(bab-require 'basic-frame-mixin)))))
         (if (not (radio-button-pushed-p (view-named 'rm dialog)))
           (if (radio-button-pushed-p (view-named 'nrm dialog))
             (eval-enqueue '(bab-require 'normal-rule-mixin))
             (if (radio-button-pushed-p (view-named 'mrm dialog))
               (eval-enqueue '(bab-require 'mini-rule-mixin))
               (eval-enqueue '(bab-require 'basic-rule-mixin)))))
         (if (not (radio-button-pushed-p (view-named 'pm dialog)))
           (if (radio-button-pushed-p (view-named 'npm dialog))
             (eval-enqueue '(bab-require 'normal-prolog-mixin))
             (if (radio-button-pushed-p (view-named 'mpm dialog))
               (eval-enqueue '(bab-require 'mini-prolog-mixin))
               (eval-enqueue '(bab-require 'basic-prolog-mixin)))))
         (if (not (radio-button-pushed-p (view-named 'cm dialog)))
           (if (radio-button-pushed-p (view-named 'ncm dialog))
             (eval-enqueue '(bab-require 'normal-constraint-mixin))
             (if (radio-button-pushed-p (view-named 'mcm dialog))
               (eval-enqueue '(bab-require 'mini-constraint-mixin))
               (eval-enqueue '(bab-require 'basic-constraint-mixin)))))
         (eval-enqueue `(progn 
                          (cc-load "mac^customs")
                          (remove-menu-items 
                           babylon-menu 
                           (find-menu-item babylon-menu "Configure Image"))
                          (menu-item-enable babylon-load-command)
                          (loop 
                            (if (y-or-n-p "Is your AntiVirus software temporarily disabled?") 
                              (return)))
                          (save-application ',file :creator '|????|)
                          )))))))

#|
      :application-class (find-class *app-class-name*)
      :error-handler error-handler
      :toplevel-function (and (neq toplevel 'toplevel-function)
                              toplevel)
      :size (list (* pref-size 1024) (* min-size  1024))
      :resources (get-app-resources res-file sig)
      :clear-clos-caches (check-box-checked-p (view-named 'caches d))
      :excise-compiler  (check-box-checked-p (view-named 'excise d))                                              
      :creator sig)))

|#

(defun allocate-babylon-configure-dialog ()
  (setf *babylon-configure-dialog*
        (make-instance 'dialog
          :window-title "Configure"
          :view-position 
          (make-point (ash (- *screen-width* 420) -1)
                      *menubar-bottom*)
          :view-size #@(420 285)
          :window-type :double-edge-box
          :window-show nil
          :view-subviews
          (list
           
           (make-instance 'static-text-dialog-item
             :dialog-item-text "Include"
             :view-position #@(8 8)
             :view-size #@(53 16))
           
           (make-instance 'check-box-dialog-item
             :dialog-item-text "free-text-mixin"
             :view-position #@(210 5)
             :view-size #@(155 15)
             :check-box-checked-p t
             :view-nick-name 'free-text)
           
           (make-instance 'static-text-dialog-item
             :dialog-item-text "Frame Interpreter"
             :view-position #@(30 25)
             :view-size #@(155 15))
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "basic-frame-mixin"
             :view-position #@(210 25)
             :view-size #@(155 15)
             :radio-button-pushed-p nil
             :radio-button-cluster '0
             :view-nick-name 'bfm)
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "no"
             :view-position #@(50 40)
             :view-size #@(100 15)
             :radio-button-pushed-p nil
             :radio-button-cluster '0
             :view-nick-name 'fm)
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "mini-frame-mixin"
             :view-position #@(210 40)
             :view-size #@(155 15)
             :radio-button-pushed-p nil
             :radio-button-cluster '0
             :view-nick-name 'mfm)
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "normal-frame-mixin"
             :view-position #@(210 55)
             :view-size #@(155 15)
             :radio-button-pushed-p t
             :radio-button-cluster '0
             :view-nick-name 'nfm)
           
           (make-instance 'static-text-dialog-item
             :dialog-item-text "Rule Interpreter"
             :view-position #@(30 70)
             :view-size #@(155 15))
           
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "basic-rule-mixin"
             :view-position #@(210 70)
             :view-size #@(155 15)
             :radio-button-pushed-p nil
             :radio-button-cluster '1
             :view-nick-name 'brm)
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "no"
             :view-position #@(50 85)
             :view-size #@(100 15)
             :radio-button-pushed-p nil
             :radio-button-cluster '1
             :view-nick-name 'rm)
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "mini-rule-mixin"
             :view-position #@(210 85)
             :view-size #@(155 15)
             :radio-button-pushed-p nil
             :radio-button-cluster '1
             :view-nick-name 'mrm)
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "normal-rule-mixin"
             :view-position #@(210 100)
             :view-size #@(155 15)
             :radio-button-pushed-p t
             :radio-button-cluster '1
             :view-nick-name 'nrm)
           
           (make-instance 'static-text-dialog-item
             :dialog-item-text "Prolog Interpreter"
             :view-position #@(30 125)
             :view-size #@(155 15))
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "basic-prolog-mixin"
             :view-position #@(210 125)
             :view-size #@(155 15)
             :radio-button-pushed-p nil
             :radio-button-cluster '3
             :view-nick-name 'bpm)
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "no"
             :view-position #@(50 140)
             :view-size #@(100 15)
             :radio-button-pushed-p nil
             :radio-button-cluster '3
             :view-nick-name 'pm)
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "mini-prolog-mixin"
             :view-position #@(210 140)
             :view-size #@(155 15)
             :radio-button-pushed-p nil
             :radio-button-cluster '3
             :view-nick-name 'mpm)
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "normal-prolog-mixin"
             :view-position #@(210 155)
             :view-size #@(155 15)
             :radio-button-pushed-p t
             :radio-button-cluster '3
             :view-nick-name 'npm)
           
           (make-instance 'static-text-dialog-item
             :dialog-item-text "Constraint Interpreter"
             :view-position #@(30 180)
             :view-size #@(155 15))
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "basic-constraint-mixin"
             :view-position #@(210 180)
             :view-size #@(185 15)
             :radio-button-pushed-p nil
             :radio-button-cluster '4
             :view-nick-name 'bcm)
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "no"
             :view-position #@(50 195)
             :view-size #@(100 15)
             :radio-button-pushed-p nil
             :radio-button-cluster '4
             :view-nick-name 'cm)
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "mini-constraint-mixin"
             :view-position #@(210 195)
             :view-size #@(185 15)
             :radio-button-pushed-p nil
             :radio-button-cluster '4
             :view-nick-name 'mcm)
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "normal-constraint-mixin"
             :view-position #@(210 210)
             :view-size #@(185 15)
             :radio-button-pushed-p t
             :radio-button-cluster '4
             :view-nick-name 'ncm)
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "Development Version"
             :view-position #@(11 230)
             :radio-button-pushed-p *fasl-opts*
             :radio-button-cluster '5
             :view-nick-name 'development)
           
           (make-instance 'radio-button-dialog-item
             :dialog-item-text "Run Time Version"
             :view-position #@(210 230)
             :radio-button-pushed-p (not *fasl-opts*)
             :radio-button-cluster '5
             :view-nick-name 'runtime)
           
           (make-instance 'static-text-dialog-item
             :dialog-item-text "Create Image:"
             :view-position #@(11 250)
             :view-size #@(102 18))
           
           (make-instance 'button-dialog-item
             :dialog-item-text "Save As ..."
             :view-position #@(245 250)
             :view-size #@(85 20)
             :view-nick-name 'save
             :dialog-item-action
             #'(lambda (item)
                 (make-configuration (view-container item))))
           
           (make-instance 'button-dialog-item
             :dialog-item-text "Cancel"
             :view-position #@(340 246)
             :view-size #@(71 27)
             :dialog-item-action 
             #'(lambda (item)
                 (declare (ignore item))
                 (return-from-modal-dialog :cancel)))))))
    

(progn
  (push (symbol-function 'deallocate-babylon-configure-dialog) 
        *save-exit-functions*)
  (setf *restore-lisp-functions*
        (append *restore-lisp-functions*
                (list (symbol-function 'babylon-default-sizes)
                      (symbol-function 'babylon-default-menubar)
                      (symbol-function 'babylon-initialize-image))))
  (babylon-default-menubar)
  (allocate-babylon-configure-dialog)
  (if (eql :cancel (catch :cancel
                     (modal-dialog *babylon-configure-dialog* nil)))
    (progn (menu-item-disable babylon-load-command)
           (add-menu-items babylon-menu
                           (make-instance 'menu-item
                             :menu-item-title "Configure Image"
                             :menu-item-action 
                             #'(lambda () 
                                 (modal-dialog *babylon-configure-dialog* nil)))))))


;;; (set-menubar *default-menubar*)

;;; we use a special top level read loop (compiler-warning-muffler) for the generated image 
;;; to shut up compiler-warnings s. file "babylon:mac;compiler-warnings.cl"
;;; we do not do this any longer

;;; eof

