;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1987   BY
;;           G M D 
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;; DATE:     August 1987
;; AUTHOR:   E. Gross

;; CONTENTS: a basic interface mixin.

;;-------------------------------------------------------------------------------


(def$flavor basic-interface-mixin
	(#+:CCL(file-name nil)
	 )
	(tty-dialog-mixin)
  :settable-instance-variables
  (:required-instance-variables language)
  (:documentation "interface mixin providing the protocol
                   required by the mini versions of the standard processors."))


(def$method (basic-interface-mixin :after :init) (&rest plist)
  "triggers initialization of window slots."
  (declare (ignore plist))
  ($send self :set-up-windows))


(def$method (basic-interface-mixin :set-up-windows) ()
  "generates a stream as pseudo window and assigns it to the standard window slots:
system-trace-window, rule-trace-window ,prolog-trace-window, explanation-window
which are available in the current configuration."
  
  (let ((stw (make-$instance 'basic-txsc-window :stream dialog-stream)))
    ($send self :send-if-handles :set-system-trace-window stw)
    ($send self :send-if-handles :set-consat-trace-window stw)
    ($send self :send-if-handles :set-rule-trace-window stw)
    ($send self :send-if-handles :set-prolog-trace-window stw)
    ($send self :send-if-handles :set-explanation-window stw)))

#+:CCL(def$method (basic-interface-mixin :loaded) (file)
        (unless (member file file-name :test #'equal)
          (setf file-name (append file-name (list file))))
        t) 

;;; eof

