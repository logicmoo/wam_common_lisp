;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")


;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;
;  dummy-interface-mixin for Macintosh Common Lisp
;
;  a specialized normal-interface-mixin, that use the dialog-stream for all windows

(def$flavor dummy-window
  ((stream nil))
  ()
  (:settable-instance-variables))

(def$method (dummy-window :format) (fstr &rest args)
  (terpri stream)
  (apply #'format stream fstr args))

(def$method (dummy-window :clear) ()
  t)
 
(def$method (dummy-window :kill) ()
  t)
 
(def$method (dummy-window :expose) ()
  t)

(def$flavor dummy-interface-mixin 
  () 
  (normal-interface-mixin)
  (:required-instance-variables dialog-stream))

;;; we only need to redefine the :set-up-windows method

(def$method (dummy-interface-mixin :set-up-windows) ()
  (let ((window (make-$instance 'dummy-window :stream dialog-stream)))
    ($send self :send-if-handles :set-explanation-window window)
    ($send self :send-if-handles :set-system-trace-window window)
    ($send self :send-if-handles :set-consat-trace-window window)
    ($send self :send-if-handles :set-rule-trace-window window)
    ($send self :send-if-handles :set-prolog-trace-window window)))

;;; eof

