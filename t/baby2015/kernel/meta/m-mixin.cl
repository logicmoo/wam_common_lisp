;;; -*- Mode: LISP; Package: BABYLON; Syntax: Common-Lisp; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHORS:  Franco di Primio, Eckehard Gross, Juergen Walther



(def$flavor meta-processor-core
	((system-trace nil)
	 (system-trace-window nil)
	 (active-proc))
	()
  :settable-instance-variables
  (:documentation "This is the meta-processor-core flavor.
It provides all the facilities to handle communication of special
processors to be mixed into one knowledge base."))

(def$method (meta-processor-core :after :init) (&rest plist)
  (declare (ignore plist))
  (setf active-proc self))

(def$method (meta-processor-core :send-system-trace-window)
	    (selector &rest args)
  (lexpr-$send system-trace-window selector args))
				
;;--------------------------------------------------------------------------
;;                    EVALUATION OF REQUESTS
;;--------------------------------------------------------------------------

(def$method (meta-processor-core :eval) (expression mode processor &rest args)
  "Evaluate an expression in mode mode for processor processor."
  (declare (ignore processor))
  (let* ((type ($send self :get-type expression))
	 (method (get type mode)))
    (cond ((null type)
	   (baberror (getentry unknown-eval-type-error-fstr babylon-io-table)
		     expression))
	  ((null method)
	   (baberror (getentry unknown-eval-mode-error-fstr babylon-io-table)
		     mode type expression))
	  (t (lexpr-$send self method expression mode args)))))

(def$method (meta-processor-core :before :eval)
	    (expression mode processor &rest args)
  "Trace :eval messages."
  (declare (ignore args))
  (when system-trace
    ($send self :send-system-trace-window :format
	   (getentry meta-proc-trace-fstr babylon-io-table)
	   processor mode expression)))

;;--------------------------------------------------------------------------

(def$method (meta-processor-core :return-nil) (expression mode &rest args)
  "Always returns nil."
  (declare (ignore expression mode args))
  nil)

;;--------------------------------------------------------------------------

(def$method (meta-processor-core :help) ()
  'why)

;;--------------------------------------------------------------------------
 
(def$method (meta-processor-core :toggle-system-trace) ()
  "Toggles system trace mode."
  (setf system-trace (if system-trace nil t)))

(defun toggle-system-trace ()
  "Toggles system trace mode."
  (if (is-activated-kb)
      (send-kb :toggle-system-trace)))

(def$method (meta-processor-core :trace-status) ()
  (if system-trace
      (format nil (getentry trace-on-fstr  babylon-io-table) "System")
      (format nil (getentry trace-off-fstr babylon-io-table) "System")))